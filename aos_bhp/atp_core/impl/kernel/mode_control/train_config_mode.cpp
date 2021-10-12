/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the TrainConfigMode class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-21    arastogi    Created
* 2016-10-17    arastogi    Set ATP reset to false when config is received.
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "train_config_mode.hpp"
#include "abstract_loco_io.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_position.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_tracks.hpp"
#include "abstract_targets.hpp"
#include "abstract_tims.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_tic.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_mode_control_event_ids.hpp"
#include <vfw_string.h>

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    TrainConfigMode::TrainConfigMode() : AbstractMode(),
      modeState(trainConfigStart),
      isNewConfig(true),
      isTsetupAcceptedByDriverInReReg(false),
      noMovementInConf(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdSBinConfig,
        ATC::NoSB, 0x0U, "No movement allowed in Train Config Mode")),
      unKnownStateInConf(ATC::Event::createEBReqEvent(atpModeControlId, ATC::CoreContainer, eventIdUnknownStateInConfig,
        ATC::NoEB, DMICom::unknownStateInConfig, "Unknown State in Train Config Mode"))
    {
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void TrainConfigMode::resetMode()
    {
      modeState = trainConfigStart;
      isNewConfig = false;
      isTsetupAcceptedByDriverInReReg = false;
      AbstractMode::resetMode();

      //resetting static variable here
      qSetupFromTSetup = TrainSetupRegistration;
      isValidQSetupReceived = false;

      const Pos::PosAccuracyState posAccuracy =
        Pos::AbstractPosition::corePtr()->getAccuracyState();
      // Do not change travel direction if position known
      if (posAccuracy != Pos::PosKnown)
      {
        //Reset the travel direction
        DS::AbstractTargets::corePtr()->setSupposedTravelDir(DirNone);
      }
      char_t buffer[maxModeStateNameLength];
      getModeStateString(modeState, &buffer[0]);

      AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
      AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void TrainConfigMode::handleMode(CommonDataForModes &commonData)
    {
      TrainConfigModeState oldModeState = modeState;

      // Check if the Unregistration message is received from TCC.
      if (!handleUnRegMessage())
      {
        handleCurrentDrivDirection(commonData);

        /* If train setup aborted in another mode like registration and Balise Search
         Moving to configuration mode*/
        if ((commonData.trainSetupAborted))
        {
          commonData.trainSetupAborted = false;
          commonData.abortSetupReason = AbortedByAos;
          modeState = trainConfigRejectedOrAborted;
        }
        else
        {
          handleAbortSetup(commonData);
        }

        handleConfigAndTSetupRejected();

        // Raise standstill event to restrict movement
        ATC::AbstractEventHandler::corePtr()->reportEvent(noMovementInConf,
          __FILE__, __LINE__);

        runModeFunction(commonData);

        //If Mode state has changed
        if (oldModeState != modeState)
        {
          char_t buffer[maxModeStateNameLength];
          getModeStateString(modeState, &buffer[0]);

          AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
          AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
        }

        //Setting common data
        commonData.isNewConfig = isNewConfig;
      }
    }

    /******************************************************************************
    * runModeFunction
    ******************************************************************************/
    void TrainConfigMode::runModeFunction(CommonDataForModes &commonData)
    {
      // Run the function corresponding to the modeState.
      switch (modeState)
      {
        case trainConfigStart:
          runTrainConfigStart(commonData);
          break;

        case trainConfigWaitSetupFrmATP:
          runTrainConfigWaitSetUpFrmATP();
          break;

        case trainConfigWaitSetupFrmTCC:
          runTrainConfigWaitSetupFrmTCC(commonData);
          break;

        case trainConfigWaitNewConfigDMI:
          runTrainConfigWaitNewConfigDMI(commonData);
          break;

        case trainConfigConfirmNewConfigFrmTCC:
          runTrainConfigConfirmNewConfigFrmTCC();
          break;

        case trainConfigConfirmReRegFrmDMI:
          runTrainConfigConfirmReRegFrmDMI();
          break;

        case trainConfigWaitTIC:
          runTrainConfigWaitTIC(commonData);
          break;

        case trainConfigTICvsTS:
          runTrainConfigTICvsTS();
          break;

        case trainConfigSendStartUpForNewConfig:
          runTrainConfigSendStartUpForNewConfig();
          break;

        case trainConfigSendReRegDataToDMI:
          runTrainConfigSendReRegDataToDMI();
          break;

        case trainConfigSendStartUpForReReg:
          runTrainConfigSendStartUpForReReg();
          break;

        case trainConfigTSetupRejected:
          runTrainConfigTSetupRejected();
          break;

        case trainConfigTSetupAccepted:
          runTrainConfigTSetupAccepted();
          break;

        case trainConfigWaitForAcceptAutomatic:
          runTrainConfigWaitForAcceptAutomatic();
          break;

        case trainConfigFinishOK:
          runTrainConfigFinishOK();
          break;

        case trainConfigFinishNOK:
          runTrainConfigFinishNOK();
          break;

        case trainConfigRejectedOrAborted:
          runTrainConfigRejectedOrAborted(commonData);
          break;

        case trainConfigWaitForDepartureTest:
          runTrainConfigWaitForDepartureTest();
          break;

        default:
          //Safety Halt
          ATC::AbstractEventHandler::corePtr()->reportEvent(unKnownStateInConf,
            __FILE__, __LINE__);
          break;
      }
    }

    /******************************************************************************
    * getModeName
    ******************************************************************************/
    ATPMode TrainConfigMode::getModeId()
    {
      return ATPModeConfiguration;
    }

    /******************************************************************************
    * runTrainConfigStart
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigStart(CommonDataForModes &commonData)
    {
      // Clearing MA Timeout and Train Idling 
      manageMATimeout(commonData);
      manageTrainIdling(commonData);
      //To deactivate the Abort Setup on DMI
      commonData.isAbortSetupActive = false;

      //delete any targets
      DS::AbstractTargets::corePtr()->removeAll();

      // Reset the TIC-component in order to receive new configuration.
      TG::AbstractTIC::corePtr()->reset();

      InitiateConfigReason initiateConfig;
      const bool isSleepingSignalActive = IO::AbstractLocoIO::corePtr()->getSleepingSignal();
      const bool isInitiateConfigReceived = Kernel::AbstractMessageHandler::corePtr()->getInitiateConfig(initiateConfig);
      const bool isDriverLoggedIn = (DriverLoginSeq::driverLoggedIn == AbstractModeControl::corePtr()->getDriverLoginSeqState());
      
      if (isInitiateConfigReceived && isDriverLoggedIn && (!isSleepingSignalActive))
      {
        //If the initiate config is known
        if (ConfigKnownByTCC == initiateConfig)
        {
          isNewConfig = false;
          //AU-FT3 fix
          commonData.isATPReset = false;
          modeState = trainConfigWaitSetupFrmTCC;
        }
        else if (ConfigUnknownByTCC == initiateConfig)
        {//If the initiate config is unknown
          isNewConfig = true;
          commonData.isATPReset = false;
          modeState = trainConfigWaitSetupFrmATP;
        }
        else
        {
          /*Safety Halt*/
          ATC::AbstractEventHandler::corePtr()->reportEvent(unKnownStateInConf,
            __FILE__, __LINE__);
        }
      }
    }

    /******************************************************************************
    * runTrainConfigWaitSetUpFrmATP
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigWaitSetUpFrmATP()
    {
      // If TIC is available  -> Send request to fetch configuration and switch to trainConfigWaitTIC.
      if (TG::AbstractTIC::corePtr()->getTICAvailable())
      {
        TG::AbstractTIC::corePtr()->requestConfig();
        modeState = trainConfigWaitTIC;
      }
      else
      {
        modeState = trainConfigWaitNewConfigDMI;
      }
    }

    /******************************************************************************
    * runTrainConfigWaitSetupFrmTCC
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigWaitSetupFrmTCC(CommonDataForModes &commonData)
    {
      // Indicate to Message Handler to send the reconfiguration sequence to DMI.
      const bool posUnknown = (Pos::AbstractPosition::corePtr()->getAccuracyState() == Pos::PosUnknown);
      isValidQSetupReceived = Kernel::AbstractMessageHandler::corePtr()->getQSetup(qSetupFromTSetup);

      //Condition to accept train setup in case of configuration known to TCC
      const bool isValidStateToAcceptTSetup = ((isValidQSetupReceived)
        && ((TrainSetupReposition == qSetupFromTSetup) || (TrainSetupReregistration == qSetupFromTSetup))
        && (posUnknown));

      //Condition to move to safe brake to stop mode
      const bool moveToSBTSMode = ((isValidQSetupReceived)
        && ((qSetupFromTSetup == TrainSetupRegistration)
          || (qSetupFromTSetup == TrainSetupReconfiguration)));

      if (isValidStateToAcceptTSetup)
      {
        modeState = trainConfigSendReRegDataToDMI;
        commonData.isATPReset = false;
        commonData.isAbortSetupActive = true;
      }
      else if (moveToSBTSMode)
      {
        modeState = trainConfigFinishNOK;
      }
      else
      {
        //Do nothing
      }
    }

    /******************************************************************************
    * runTrainConfigWaitNewConfigDMI
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigWaitNewConfigDMI(CommonDataForModes &commonData)
    {
      // Abort button visibility on DMI
      commonData.isAbortSetupActive = true;
      DS::TrainSetup tSetup;
      //  TrainSetupReason qSetupFromTSetupTemp;
      bool isTSConfirmByDMI = DMICom::AbstractDMIHandler::corePtr()->getDMINewTrainSetUpConfirmed();
      //If Tsetup is entered from DMI and temporary setup  from DMI is valid
      if ((DS::AbstractTSetup::corePtr()->getPreliminaryTrainSetup(tSetup)) &&
        (isTSConfirmByDMI))
      {
        modeState = trainConfigSendStartUpForNewConfig;
      }
    }

    /******************************************************************************
    * runTrainConfigSendStartUpForNewConfig
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigSendStartUpForNewConfig()
    {
      if (AbstractMessageHandler::corePtr()->isStartUpMessageSent())
      {
        modeState = trainConfigConfirmNewConfigFrmTCC;
      }
    }

    /******************************************************************************
    * runTrainConfigSendStartUpForReReg
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigSendStartUpForReReg()
    {

      if (AbstractMessageHandler::corePtr()->isStartUpMessageSent())
      {
        // Rejected from DMI?
        if (isTsetupAcceptedByDriverInReReg)
        {
          modeState = trainConfigTSetupAccepted;
          //Rest the flag  
          isTsetupAcceptedByDriverInReReg = false;
        }
        else
        {
          modeState = trainConfigStart;
        }
      }
    }

    /******************************************************************************
    * runTrainConfigConfirmNewConfigFrmTCC
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigConfirmNewConfigFrmTCC()
    {

      // Unregistration message received.
      if (AbstractMessageHandler::corePtr()->isTrainSetupRejectedByAOS())
      {
        modeState = trainConfigTSetupRejected;
      }
      // Valid train setup received
      else if (DS::AbstractTSetup::corePtr()->getTrainSetup() != static_cast<const DS::TrainSetup*>(NULL))
      {
        if (isNewConfig)
        {
          //Fetching Q_SETUP in case of Registration only, since in Re-Registration Q_SETUP is already received
          isValidQSetupReceived = Kernel::AbstractMessageHandler::corePtr()->getQSetup(qSetupFromTSetup);
        }
        modeState = trainConfigTSetupAccepted;
      }
      else
      {
        // Do Nothing
      }
    }

    /******************************************************************************
    * runTrainConfigSendReRegDataToDMI
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigSendReRegDataToDMI()
    {
      //Intentionally change mode after 1 cycle
      modeState = trainConfigConfirmReRegFrmDMI;
    }

    /******************************************************************************
    * runTrainConfigWaitReConfigDMI
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigConfirmReRegFrmDMI()
    {
      // Wait for Accept/Reject from the DMI reconfiguration.
      DS::TrainSetup tSetup;
      bool retValue = true;
      DS::VehicleSetup    tVehicleSetup;
      DMICom::Confirmation dmiConfirmation = DMICom::AbstractDMIHandler::corePtr()->getConfirmation();
      bool isTSConfirmByDMI = (dmiConfirmation != DMICom::DMIATPConfirmationUndefined);

      // Tsetup is accepted from DMI and temporary setup is valid?
      if (DS::AbstractTSetup::corePtr()->getPreliminaryTrainSetup(tSetup) &&
        (isTSConfirmByDMI))
      {
        //train set up
        if (DS::AbstractTSetup::corePtr()->getPreliminaryTrainSetup(tSetup))
        {
          //copy the prel setup to permanent setup
          retValue = DS::AbstractTSetup::corePtr()->setTrainSetup(tSetup);

          //car setup
          for (uint16_t i = 0U; ((i < tSetup.vehicleCount) && retValue); ++i)
          {
            if (DS::AbstractTSetup::corePtr()->getPreliminaryVehicleSetup(i, tVehicleSetup))
            {
              retValue = DS::AbstractTSetup::corePtr()->setVehicleSetup(i, tVehicleSetup);
            }
          }
        }
        if (retValue)
        {
          if (dmiConfirmation == DMICom::DMIATPConfirmationOK)
          {
            isTsetupAcceptedByDriverInReReg = true;
          }

          modeState = trainConfigSendStartUpForReReg;
        }

        //TODO do we need to raise error here
      }
      else
      {
        // Rejected from DMI?
        if (dmiConfirmation == DMICom::DMIATPConfirmationNOK)
        {
          modeState = trainConfigStart;
        }
      }
    }

    /******************************************************************************
    * runTrainConfigWaitTIC
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigWaitTIC(CommonDataForModes &commonData)
    {
      TG::TICConfigStatus ticStatus = TG::AbstractTIC::corePtr()->getStatus();

      // Abort button visibility on DMI
      commonData.isAbortSetupActive = true;

      // Configuration was successfully received via TIC
      if (TG::TICConfigStatusCompleted == ticStatus)
      {
        modeState = trainConfigWaitForAcceptAutomatic;
        isNewConfig = true;
      }

      // Configuration not completed as TIC timeout happened. Change state to Config rejected
      else if (TG::TICConfigStatusError == ticStatus)
      {
        modeState = trainConfigRejectedOrAborted;
        isNewConfig = true;
      }
      else
      {
        // Just wait if Pending/Progress
      }
    }

    /******************************************************************************
    * runTrainConfigTICvsTS
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigTICvsTS()
    {
      // TODO: Probably remove, but wait for further requirements
    }

    /******************************************************************************
    * runTrainConfigTSetupRejected
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigTSetupRejected()
    {
      //Display message on DMI
      if (isNewConfig)
      {
        modeState = trainConfigRejectedOrAborted;
      }
      else
      {
        modeState = trainConfigFinishNOK;
      }
    }

    /******************************************************************************
    * runTrainConfigTSetupAccepted
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigTSetupAccepted()
    {
      modeState = trainConfigFinishOK;
    }

    /******************************************************************************
  * runtrainConfigWaitForAcceptAutomatic
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigWaitForAcceptAutomatic()
    {
      // Get the DMI Button Status...
      const DMICom::DMIButtonStatus dmiButtonStatus = DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus();

      if ((dmiButtonStatus == DMICom::DMIButtonAcceptAutomaticConfig) ||
        (dmiButtonStatus == DMICom::DMIButtonConfirmChangeOfTrainLoaded))
      {
        //Fetching the LOCO Train Direction from the DMI
        uint8_t locoVsTrainDirData;

        if (DMICom::AbstractDMIHandler::corePtr()->getLocoVsTrainDirData() == DMICom::DMIATPcarsConnectedAtAEnd)
        {
          locoVsTrainDirData = trainLocoOrientation;
        }
        else
        {
          locoVsTrainDirData = 0x0U;
        }

        //Set the Loco Orientation in Preliminary TSetup
        bool operationSuccedded = DS::AbstractTSetup::corePtr()->setOrientationinPreliminaryTSetup(locoVsTrainDirData);

        if (operationSuccedded)
        {
          trace->write(ATC::briefTrace, "Orientation is set in the preliminary train setup");
        }
        modeState = trainConfigSendStartUpForNewConfig;
      }
    }

    /******************************************************************************
    * runTrainConfigFinishOK
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigFinishOK()
    {
      Pos::PosAccuracyState posAcc =
        Pos::AbstractPosition::corePtr()->getAccuracyState();

      // Set the next mode depending on the position accuracy state.
      if (posAcc == Pos::PosKnown)
      {
        setNextMode(ATPModeNormal);

        //remove tracks not under the train
        if (TG::AbstractTIMS::corePtr()->isRearPositionValid())
        {
          if (DS::AbstractTracks::corePtr()->removeNotCovered(
            TG::AbstractTIMS::corePtr()->getSafePositionToDeleteTrack(),
            Pos::AbstractPosition::corePtr()->getSafeLeadingPosOdo()))
          {
            // No tracks or balises to remove.
          }
        }

        //delete targets
        DS::AbstractTargets::corePtr()->removeAll();
      }
      else
      {
        setNextMode(ATPModeRegistration);
      }
    }

    /******************************************************************************
    * runTrainConfigFinishNOK
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigFinishNOK()
    {
      //Next mode after config fails is SafeBrakeToStop
      setNextMode(ATPModeSafeBrakeToStop);
    }

    /******************************************************************************
    * runTrainConfigRejected
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigRejectedOrAborted(CommonDataForModes &commonData)
    {
      //Disable the Abort button on DMI
      commonData.isAbortSetupActive = false;

      // Remove the train setup when configuration is rejected
      DS::AbstractTSetup::corePtr()->removeTrainSetup();
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    TrainConfigModeState TrainConfigMode::getModeState() const
    {
      return modeState;
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool TrainConfigMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void TrainConfigMode::getModeStateString(const TrainConfigModeState state, char_t* const buffer) const
    {
      switch (state)
      {
        case trainConfigStart:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigStart", maxModeStateNameLength));
          break;

        case trainConfigWaitSetupFrmATP:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigWaitSetupFrmATP", maxModeStateNameLength));
          break;

        case trainConfigWaitSetupFrmTCC:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigWaitSetupFrmTCC", maxModeStateNameLength));
          break;

        case trainConfigWaitNewConfigDMI:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigWaitNewConfigDMI", maxModeStateNameLength));
          break;

        case trainConfigConfirmReRegFrmDMI:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigConfirmReRegFrmDMI", maxModeStateNameLength));
          break;

        case trainConfigSendReRegDataToDMI:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigSendReRegDataToDMI", maxModeStateNameLength));
          break;

        case trainConfigWaitTIC:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigWaitTIC", maxModeStateNameLength));
          break;

        case trainConfigTICvsTS:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigTICvsTS", maxModeStateNameLength));
          break;

        case trainConfigSendStartUpForNewConfig:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigSendStartUpForNewConfig", maxModeStateNameLength));
          break;

        case trainConfigSendStartUpForReReg:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigSendStartUpForReReg", maxModeStateNameLength));
          break;

        case trainConfigConfirmNewConfigFrmTCC:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigConfirmNewConfigFrmTCC", maxModeStateNameLength));
          break;

        case trainConfigTSetupRejected:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigTSetupRejected", maxModeStateNameLength));
          break;

        case trainConfigTSetupAccepted:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigTSetupAccepted", maxModeStateNameLength));
          break;

        case trainConfigWaitForAcceptAutomatic:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigWaitForAcceptAutomatic", maxModeStateNameLength));
          break;

        case trainConfigFinishOK:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigFinishOK", maxModeStateNameLength));
          break;

        case trainConfigFinishNOK:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigFinishNOK", maxModeStateNameLength));
          break;

        case trainConfigRejectedOrAborted:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigRejectedOrAborted", maxModeStateNameLength));
          break;

        case trainConfigWaitForDepartureTest:
          static_cast<void>(vfw_strlcpy(buffer, "trainConfigWaitForDepartureTest", maxModeStateNameLength));
          break;

        default:
          static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
          break;
      }
    }

    /******************************************************************************
    * manageIdling
    ******************************************************************************/
    void TrainConfigMode::manageTrainIdling(CommonDataForModes &commonData)
    {
      commonData.idling = false;
    }

    /******************************************************************************
    * manageMATimeoutState
    ******************************************************************************/
    void TrainConfigMode::manageMATimeout(CommonDataForModes &commonData)
    {
      commonData.maTimeOut = false;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void TrainConfigMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&isNewConfig));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&isTsetupAcceptedByDriverInReReg));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&modeState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&noMovementInConf));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&unKnownStateInConf));
    }

    /******************************************************************************
    * handleConfigAndTSetupRejected
    ******************************************************************************/
    void TrainConfigMode::handleConfigAndTSetupRejected()
    {
    
      // Update the Sub-state as manual Configuration state when Reject Configuration message is received
      RejectConfigInfo rejectReason;
      const bool rejectConfiguration = AbstractMessageHandler::corePtr()->getRejectConfigurationInfo(rejectReason);
      const bool trainSetupRejected = AbstractMessageHandler::corePtr()->isTrainSetupRejectedByAOS();

      if (rejectConfiguration || trainSetupRejected)
      {
        modeState = trainConfigRejectedOrAborted;
      }

     
    }

    /******************************************************************************
    * runTrainConfigWaitForDepartureTest
    ******************************************************************************/
    void TrainConfigMode::runTrainConfigWaitForDepartureTest()
    {
      //Nothing to do here
      // State handled in Adaptation
    }

    /******************************************************************************
    * handleAbortSetup
    ******************************************************************************/
    void TrainConfigMode::handleAbortSetup(CommonDataForModes &commonData)
    {
      if (DMICom::DMIButtonAbortSetup == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus())
      {
        if ((trainConfigWaitTIC == modeState) || (trainConfigWaitNewConfigDMI == modeState)
           || (trainConfigWaitForAcceptAutomatic == modeState))
        {
          //In case of aborting the assembling of train setup 
          //AbortSetup message will be not sent to TCC
          commonData.trainSetupAborted = false;
          modeState = trainConfigRejectedOrAborted;
        }
        else
        {
          commonData.trainSetupAborted = true;
          commonData.abortSetupReason = AbortedByDriver;
        }
      }
    }
  }
}
