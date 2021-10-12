/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the TrainRegistrationMode class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-05-10    skothiya    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_config.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_mode.hpp"
#include "primary_target.hpp"
#include "abstract_targets.hpp"
#include "abstract_odometry.hpp"
#include "abstract_message_handler.hpp"
#include "atc_math.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_tsetup.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
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
    TrainRegistrationMode::TrainRegistrationMode() : AbstractMode(),
      // creating different set of objects for different type of events
      noMovementinREG(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer,
        eventIdSBinReg, ATC::NoSB, 0x0U, "No movement allowed in Train Reg Mode")),
      incorrectQsetupAndPosCombination(ATC::Event::createSafeBrakeSBEvent(atpModeControlId, ATC::CoreContainer,
        eventIdIncorrectQsetupAndPositionCombination, ATC::NoSB, DMICom::modReRegInvalidQSetup, "Incorrect Qsetup and Position Combination")),
      inValidMAReceivedInReRegistration(ATC::Event::createSafeBrakeSBEvent(atpModeControlId, ATC::CoreContainer,
        eventIdInValidMAReceivedInReRegistration, ATC::NoSB, DMICom::modReRefInvalidMA, "Invalid Movement Authority in Re-registration")),
      emergencyAlertInRegistrationMode(ATC::Event::createSafeBrakeSBEvent(atpModeControlId, ATC::CoreContainer, eventIdEmergencyAlertInRegistrationMode,
        ATC::NoSB, DMICom::modEmergencyAlertRegistrationMode, "Emergency Alert in Registration Mode"))
    {
      modeState = trainRegistrationStart;
      dmiTravelDir = DirUndefined;
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void TrainRegistrationMode::resetMode()
    {
      modeState = trainRegistrationStart;
      dmiTravelDir = DirUndefined;
      maxSearchDistanceInReReg = 0U;
      AbstractMode::resetMode();

      char_t buffer[maxModeStateNameLength];
      getModeStateString(modeState, &buffer[0]);

      AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
      AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void TrainRegistrationMode::handleMode(CommonDataForModes &commonData)
    {
      TrainRegistrationModeState oldModeState = modeState;

      //Check if the setup is aborted and switch the mode state
      handleAbortSetup(commonData);

      handleCurrentDrivDirection(commonData);

      // Check if Unregistration message is received from TCC
      if (!handleUnRegMessage())
      {
        //raise standstill event to restrict movement
        ATC::AbstractEventHandler::corePtr()->reportEvent(noMovementinREG,
          __FILE__, __LINE__);

        //run the function corresponding to the modeState.
        switch (modeState)
        {
          case trainRegistrationStart:
            runTrainRegistrationStart(commonData);
            break;

          case trainRegistrationSendTrackDirToDMI:
            runTrainRegistrationSendTrackDirToDMI();
            break;

          case trainRegistrationWaitTrackDir:
            runTrainRegistrationWaitTrackDir(commonData);
            break;

          case trainRegistrationSetupBSTarget:
            runTrainRegistrationSetupBSTarget();
            break;

          case trainRegistrationWaitReRegMA:
            runTrainRegistrationWaitReRegMA(commonData);
            break;

          case trainRegistrationFinish:
            runTrainRegistrationFinish();
            break;

          case trainRegistrationRePosition:
            runTrainRegistrationRePos();
            break;

          case trainRegistrationWaitForApprxMesg:
            runTrainRegistrationWaitForApproxMesg(commonData);
            break;

          default:
            break;
        }

        //If Mode state has changed
        if (oldModeState != modeState)
        {
          char_t buffer[maxModeStateNameLength];
          getModeStateString(modeState, &buffer[0]);

          AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
          AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
        }

        // Check if Emergency Alert is raised in Registration mode
        if ((AbstractModeControl::corePtr()->getEmergencyAlertSeqState() != emergencyAlertInactive))
        {
          //Raise a Safe Brake To Stop 
          ATC::AbstractEventHandler::corePtr()->reportEvent(emergencyAlertInRegistrationMode,
            __FILE__, __LINE__);
        }

      }

    }

    /******************************************************************************
    * getModeName
    ******************************************************************************/
    ATPMode TrainRegistrationMode::getModeId()
    {
      return ATPModeRegistration;
    }

    /******************************************************************************
    * runTrainRegistrationStart
    ******************************************************************************/
    void TrainRegistrationMode::runTrainRegistrationStart(CommonDataForModes &commonData)
    {
      //Clearing MA Timeout and Train Idling 
      manageMATimeout(commonData);
      manageTrainIdling(commonData);
      Pos::PosAccuracyState aosCurrentPos = Pos::AbstractPosition::corePtr()->getAccuracyState();

      if (isValidQSetupReceived)
      {
        //change modeState based on Q_setup
        switch (qSetupFromTSetup)
        {
          case TrainSetupRegistration: /*new configuration*/
          {
            if (Pos::PosUnknown == aosCurrentPos)
            {
              DriverLoginState atpDriverLoginState = AbstractModeControl::corePtr()->getDriverLoginSeqState();
              if (atpDriverLoginState == DriverLoginSeq::DriverLoginSeq::driverLoggedIn)
              {
                modeState = trainRegistrationSendTrackDirToDMI;
              }
            }
            else
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(incorrectQsetupAndPosCombination,
                __FILE__, __LINE__);
            }
            break;
          }

          case TrainSetupReregistration: /* Reregistration */
          {
            if (Pos::PosUnknown == aosCurrentPos)
            {
              modeState = trainRegistrationWaitReRegMA;
              // check if MA is received by calling the next state run function.
              runTrainRegistrationWaitReRegMA(commonData);
            }
            else
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(incorrectQsetupAndPosCombination,
                __FILE__, __LINE__);
            }
            break;
          }

          case TrainSetupReconfiguration: /* Reconfiguration */
          {
            if (Pos::PosKnown == aosCurrentPos)
            {
              //Enter the Normal mode
              setNextMode(ATPModeNormal);
            }
            else
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(incorrectQsetupAndPosCombination,
                __FILE__, __LINE__);
            }
            break;
          }

          case TrainSetupReposition: /*ATP Reposition*/
          {

            if (Pos::PosUnknown == aosCurrentPos)
            {
              modeState = trainRegistrationWaitForApprxMesg;
            }
            else
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(incorrectQsetupAndPosCombination,
                __FILE__, __LINE__);
            }
            break;
          }

          default:
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(incorrectQsetupAndPosCombination,
              __FILE__, __LINE__);
            break;
          }
        }
      }
    }

    /******************************************************************************
    * runTrainRegistrationWaitReRegMA
    ******************************************************************************/
    void TrainRegistrationMode::runTrainRegistrationWaitReRegMA(CommonDataForModes &commonData)
    {
      MAHead mHead;
      bool isPartlyMA = AbstractMessageHandler::corePtr()->getPartlyMaReceived();
      bool validMAReceived = AbstractMessageHandler::corePtr()->isValidMAReceived();
      uint32_t tempVal = 0U;
      bool validMaxSearchDistance = (AbstractMessageHandler::corePtr()->getMaxSearchDistInReReg(tempVal));

      if (validMAReceived && (!isPartlyMA))
      {
        if ((AbstractMessageHandler::corePtr()->getMAHead(mHead))
          && (mHead.routeType == RtReRegistration) && validMaxSearchDistance )
        {
          //To assign the valid Max Search distance during Rereg
           maxSearchDistanceInReReg = tempVal;
          //To deactivate the Abort Setup on DMI
          commonData.isAbortSetupActive = false;
          modeState = trainRegistrationFinish;
        }
        else
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(inValidMAReceivedInReRegistration,
            __FILE__, __LINE__);
        }
      }

    }

    /******************************************************************************
    * runTrainRegistrationSendTrackDirToDMI
    ******************************************************************************/
    void TrainRegistrationMode::runTrainRegistrationSendTrackDirToDMI()
    {
        DriverLoginState atpDriverLoginState = AbstractModeControl::corePtr()->getDriverLoginSeqState();

      if (atpDriverLoginState == DriverLoginSeq::DriverLoginSeq::driverLoggedIn)
      {
          // Wait until Driver logged in again
          modeState = trainRegistrationWaitTrackDir;
      }
    }

    /******************************************************************************
    * runTrainRegistrationWaitTrackDir
    ******************************************************************************/
    void TrainRegistrationMode::runTrainRegistrationWaitTrackDir(CommonDataForModes &commonData)
    {
      //Wait for the train orientation to be entered from DMI
      DMICom::TrainVsTrackDirection dir =
        DMICom::AbstractDMIHandler::corePtr()->getTrainVsTrackDirection();
      //Driver Login State
      DriverLoginState atpDriverLoginState = AbstractModeControl::corePtr()->getDriverLoginSeqState();

      if (dir != DMICom::DMIATPTrainVsTrackDirectionUndefined)
      {
        modeState = trainRegistrationSetupBSTarget;

        switch (dir)
        {
          case DMICom::DMIATPTrainVsTrackDirectionForwardTrackTravelForward:
            dmiTravelDir = DirForward;
            commonData.odoDirectionNewReg = OdoPositive;
            break;
          case DMICom::DMIATPTrainVsTrackDirectionForwardTrackTravelReverse:
            dmiTravelDir = DirReverse;
            commonData.odoDirectionNewReg = OdoPositive;
            break;
          case DMICom::DMIATPTrainVsTrackDirectionReverseTrackTravelForward:
            dmiTravelDir = DirForward;
            commonData.odoDirectionNewReg = OdoNegative;
            break;
          case DMICom::DMIATPTrainVsTrackDirectionReverseTrackTravelReverse:
            dmiTravelDir = DirReverse;
            commonData.odoDirectionNewReg = OdoNegative;
            break;
          case DMICom::DMIATPTrainVsTrackDirectionUndefined:
            // Fall through
          default:
            AbstractModeControl::corePtr()->getTrace()->write(2U, "Invalid/Undefined TrainVsTrackDirection!");
            break;
        }
      }
      else if (atpDriverLoginState != DriverLoginSeq::DriverLoginSeq::driverLoggedIn)
      {
          modeState = trainRegistrationStart;
      }
      else
      {
          //Please Lint
      }
    }

    /******************************************************************************
    * runTrainRegistrationSetupBSTarget
    ******************************************************************************/
    void TrainRegistrationMode::runTrainRegistrationSetupBSTarget()
    {
      //If train setup is valid
      const bool isValidTrainSetup = DS::AbstractTSetup::corePtr()->isTrainSetupValid();

      if (isValidTrainSetup)
      {
        //set track and position of target as 0. No tracks available now
        TrackAndPos targetPos;
        targetPos.track = 0U;
        targetPos.position = 0U;

        OdoPosition targetOdo = Pos::AbstractOdometry::corePtr()->getOdoPosition();

        OdoPosition baliseSearchDistanceCm =
          static_cast<OdoPosition>(AbstractConfig::corePtr()->getBalSearchDistance());

        //increment/decrement odometer reading based on the travel direction.
        if (dmiTravelDir == DirForward)
        {
          targetOdo += baliseSearchDistanceCm;
        }
        else if (dmiTravelDir == DirReverse)
        {
          targetOdo -= baliseSearchDistanceCm;
        }
        else
        {
          //error
        }

        // set the Travel direction based on the Tsetup
        DS::AbstractTargets::corePtr()->setSupposedTravelDir(dmiTravelDir);

        //Create primary target object.
        DS::PrimaryTarget ptarget(0U, //No MA id for virtual target
          static_cast<uint8_t>(RtNormal), // Normal route
          maMarginBaliseSearch, // MA Margin
          0U, // No Timeout
          targetPos, // Target Position
          dmiTravelDir, // travel direction
          targetOdo); // odometer of target

        //set the targets ceiling speed to the balise search speed.
        DS::AbstractTargets::corePtr()->setCurCeilingSpeed(
          static_cast<uint32_t>(AbstractConfig::corePtr()->getBalSearchSpeed()));

        //Add the target
        DS::AbstractTargets::corePtr()->addTarget(ptarget);
        modeState = trainRegistrationFinish;
      }
    }

    /******************************************************************************
    * runTrainRegistrationFinish
    ******************************************************************************/
    void TrainRegistrationMode::runTrainRegistrationFinish()
    {
      setNextMode(ATPModeBaliseSearch);
    }

    /******************************************************************************
    * runTrainRegistrationRePos
    ******************************************************************************/
    void TrainRegistrationMode::runTrainRegistrationRePos()
    {
      MAHead mHead;
      if (AbstractMessageHandler::corePtr()->getMAHead(mHead)
        && (RtStaffResponsible == mHead.routeType))
      {
        setNextMode(ATPModeStaffResponsible);
      }
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    TrainRegistrationModeState TrainRegistrationMode::getModeState() const
    {
      return modeState;
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool TrainRegistrationMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void TrainRegistrationMode::getModeStateString(const TrainRegistrationModeState state, char_t* const buffer) const
    {
      switch (state)
      {
        case trainRegistrationStart:
          static_cast<void>(vfw_strlcpy(buffer, "trainRegistrationStart", maxModeStateNameLength));
          break;

        case trainRegistrationWaitTrackDir:
          static_cast<void>(vfw_strlcpy(buffer, "trainRegistrationWaitTrackDir", maxModeStateNameLength));
          break;

        case trainRegistrationSetupBSTarget:
          static_cast<void>(vfw_strlcpy(buffer, "trainRegistrationSetupBSTarget", maxModeStateNameLength));
          break;

        case trainRegistrationSendTrackDirToDMI:
          static_cast<void>(vfw_strlcpy(buffer, "trainRegistrationSendTrackDirToDMI", maxModeStateNameLength));
          break;

        case trainRegistrationWaitReRegMA:
          static_cast<void>(vfw_strlcpy(buffer, "trainRegistrationWaitReRegMA", maxModeStateNameLength));
          break;

        case trainRegistrationFinish:
          static_cast<void>(vfw_strlcpy(buffer, "trainRegistrationFinish", maxModeStateNameLength));
          break;

        case trainRegistrationRePosition:
          static_cast<void>(vfw_strlcpy(buffer, "trainRegistrationRePosition", maxModeStateNameLength));
          break;

        case trainRegistrationWaitForApprxMesg:
          static_cast<void>(vfw_strlcpy(buffer, "trainRegistrationWaitForApprxMesg", maxModeStateNameLength));
          break;

        default:
          static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
          break;
      }
    }

    /******************************************************************************
    * manageIdling
    ******************************************************************************/
    void TrainRegistrationMode::manageTrainIdling(CommonDataForModes &commonData)
    {
      commonData.idling = false;
    }


    /******************************************************************************
    * manageMATimeoutState
    ******************************************************************************/
    void TrainRegistrationMode::manageMATimeout(CommonDataForModes &commonData)
    {
      commonData.maTimeOut = false;
    }

    /******************************************************************************
    * isValidQRouteType
    ******************************************************************************/
    bool TrainRegistrationMode::isValidQRouteType(const RouteType routeType) const 
    {
      bool retFlag = false;
      switch (routeType)
      {
        case RtStaffResponsible:
          if (trainRegistrationRePosition == modeState) 
          {
              retFlag = true;
          }
          break;

        case RtReRegistration:
          if (trainRegistrationWaitReRegMA == modeState)
          {
            retFlag = true;
          }
          break;
        case RtShuntingRoute:
        case RtNormal:
        case RtSplit:
        case RtJoin:
        case RtUndefined:
        {
          retFlag = false;
          break;
        }
        default:
        {
          retFlag = false;
          AbstractModeControl::corePtr()->getTrace()->write(2U, "Invalid Q Route Type !");
          break;
        }
      }
      return retFlag;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void TrainRegistrationMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&modeState));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<TravelDir>(&dmiTravelDir));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&noMovementinREG));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&inValidTsetupInREG));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&incorrectQsetupAndPosCombination));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&inValidMAReceivedInReRegistration));
    }

    /******************************************************************************
    * runTrainRegistrationWaitForApproxMesg
    ******************************************************************************/
    void TrainRegistrationMode::runTrainRegistrationWaitForApproxMesg(CommonDataForModes &commonData)
    {
      if (AbstractMessageHandler::corePtr()->getApproximatePosition())
      {
        commonData.isAbortSetupActive = false;
        modeState = trainRegistrationRePosition;
      }
      else if (AbstractMessageHandler::corePtr()->isApproximatePositionMsgRejected())
      {
        //Abort the train setup
        commonData.trainSetupAborted = true;
        commonData.abortSetupReason = AbortedByAos;
        setNextMode(ATPModeConfiguration);
      }
      else
      {
        //Do nothing to avoid lint warning
      }
    }
  }
}
