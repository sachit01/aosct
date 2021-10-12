/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each DMi messageType (AOS->DMI) has an associated creator class inherited from AbstractDMIMessageOut.
* This file implements the creator for the ATPModesAndStatus message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-13    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
* 2017-01-13    saprasad    Fixed Linux warning for PPC Target for collectData fun
* 2017-04-11    skothiya    Updated for the implementation of cabin handling and authorization

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_btm_handler.hpp"
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_atp_modes_and_states.hpp"
#include "abstract_radio_handler.hpp"
#include "abstract_loco_io.hpp"
#include "abstract_config.hpp"
#include "abstract_loco_io.hpp"
#include "abstract_odometry.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_vehicle_com.hpp"
#include "abstract_tims.hpp"
#include "abstract_brake.hpp"
#include "abstract_tsetup.hpp"
#include "staff_responsible_mode.hpp"
#include "abstract_targets.hpp"
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
  namespace DMICom
  {
    /******************************************************************************
    * DmiData::DmiData
    ******************************************************************************/
    DMIMessageOutATPModesAndStatus::DmiData::DmiData() :
      dmiATPMode(DMIATPModeUndefined),
      dmiATPStates(DMIATPStatusUndefined),
      atpModeState(UndefinedModeSubState),
      driverVerState(DMIATPDriverVerificationStateUndefined),
      dmiIntegrityRelatedData(0U),
      trainStatus(0U),
      additionalStatusBits1(0U),
      additionalStatusBits2(0U),
      dmiATOMode(DMIATOModeUndefined),
      dmiWantedATOSwitchPos(DMIWantedATOSwitchPosUndefined),
      dmiConfirmModeChange(DMIConfirmModeChangeUndefined),
      dmiVisibilityControl(0U),
      brakeTestStatus(0U),
      remainingTimeToMandatoryBrakeTest(0U),
      remainingTimeToMandatoryRoutineTest(0U),
      additionalAllowedToInfo(0U),
      additionalConfirmInfo(0U),
      adaptationTrainStatus(0U),
      additionalConfirmInfo2(0U),
      PlatformStatus(0U)
    {
    }

    /******************************************************************************
    * DmiData::invalidate
    ******************************************************************************/
    void DMIMessageOutATPModesAndStatus::DmiData::invalidate()
    {
      // Clear all the data
      dmiATPMode = DMIATPModeUndefined;
      dmiATPStates = DMIATPStatusUndefined;
      atpModeState = UndefinedModeSubState;
      driverVerState = DMIATPDriverVerificationStateUndefined;
      dmiIntegrityRelatedData = 0U;
      trainStatus = 0U;
      additionalStatusBits1 = 0U;
      additionalStatusBits2 = 0U;
      dmiATOMode = DMIATOModeUndefined;
      dmiWantedATOSwitchPos = DMIWantedATOSwitchPosUndefined;
      dmiConfirmModeChange = DMIConfirmModeChangeUndefined;
      dmiVisibilityControl = 0U;
      brakeTestStatus = 0U;
      remainingTimeToMandatoryBrakeTest = 0U;
      remainingTimeToMandatoryRoutineTest = 0U;
      additionalAllowedToInfo = 0U;
      additionalConfirmInfo = 0U;
      adaptationTrainStatus = 0U;
      additionalConfirmInfo2 = 0U;
      PlatformStatus = 0U;
    }

    /******************************************************************************
    * DmiData::operator !=
    ******************************************************************************/
    bool DMIMessageOutATPModesAndStatus::DmiData::operator !=(const DmiData& that) const
    {
      return ((dmiATPMode != that.dmiATPMode) ||
        (dmiATPStates != that.dmiATPStates) ||
        (atpModeState != that.atpModeState) ||
        (driverVerState != that.driverVerState) ||
        (dmiIntegrityRelatedData != that.dmiIntegrityRelatedData) ||
        (trainStatus != that.trainStatus) ||
        (additionalStatusBits1 != that.additionalStatusBits1) ||
        (additionalStatusBits2 != that.additionalStatusBits2) ||
        (dmiATOMode != that.dmiATOMode) ||
        (dmiWantedATOSwitchPos != that.dmiWantedATOSwitchPos) ||
        (dmiConfirmModeChange != that.dmiConfirmModeChange) ||
        (dmiVisibilityControl != that.dmiVisibilityControl) ||
        (brakeTestStatus != that.brakeTestStatus) ||
        (remainingTimeToMandatoryBrakeTest != that.remainingTimeToMandatoryBrakeTest) ||
        (remainingTimeToMandatoryRoutineTest != that.remainingTimeToMandatoryRoutineTest));
    }

    /******************************************************************************
    * DMIMessageOutATPModesAndStatus constructor
    ******************************************************************************/
    DMIMessageOutATPModesAndStatus::DMIMessageOutATPModesAndStatus() : AbstractDMIMessageOut(MTypeATPModeAndState)
    {
    }

    /******************************************************************************
    * DMIMessageOutATPModesAndStatus::collectData
    ******************************************************************************/
    void DMIMessageOutATPModesAndStatus::collectData()
    {
      bool trainNameChangeAllowedInMode = false;

      ATPMode atpMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      switch (atpMode)
      {
        case ATPModeUndefined:
        {
          dmiData.dmiATPMode = DMIATPModeUndefined;
          break;
        }
        case ATPModePowerUp:
        {
          dmiData.dmiATPMode = DMIATPModePowerUp;
          break;
        }
        case ATPModeConfiguration:
        {
          dmiData.dmiATPMode = DMIATPModeTrainConfiguration;
          break;
        }
        case ATPModeRegistration:
        {
          dmiData.dmiATPMode = DMIATPModeTrainRegistration;
          break;
        }
        case ATPModeBaliseSearch:
        {
          dmiData.dmiATPMode = DMIATPModeBaliseSearchMode;
          break;
        }
        case ATPModeNormal:
        {
          dmiData.dmiATPMode = DMIATPModeNormal;
          trainNameChangeAllowedInMode = true;
          break;
        }
        case ATPModeShunting:
        {
          dmiData.dmiATPMode = DMIATPModeShunting;
          break;
        }
        case ATPModeLocation:
        {
          dmiData.dmiATPMode = DMIATPModeLocation;
          trainNameChangeAllowedInMode = true;
          break;
        }
        case ATPModeYard:
        {
          dmiData.dmiATPMode = DMIATPModeYardMode;
          break;
        }
        case ATPModeUnregistered:
        {
          dmiData.dmiATPMode = DMIATPModeUnregistered;
          break;
        }
        case ATPModePoweringDown:
        {
          dmiData.dmiATPMode = DMIATPModePowerDown;
          break;
        }
        case ATPModeSafetyHalt:
        {
          dmiData.dmiATPMode = DMIATPModeSafetyHalt;
          break;
        }
        case ATPModeSleeping:
        {
          dmiData.dmiATPMode = DMIATPModeSleeping;
          break;
        }
        case ATPModeStaffResponsible:
        {
          dmiData.dmiATPMode = DMIATPModeStaffResponsible;
          trainNameChangeAllowedInMode = true;
          break;
        }
        case ATPModeShuntingRoute:
        {
          dmiData.dmiATPMode = DMIATPModeShuntingRoute;
          break;
        }
        case ATPModePossession:
        {
          dmiData.dmiATPMode = DMIATPModePossession;
          break;
        }
        case ATPModeSplit:
        {
          dmiData.dmiATPMode = DMIATPModeSplit;
          break;
        }
        case ATPModeJoin:
        {
          dmiData.dmiATPMode = DMIATPModeJoin;
          break;
        }
        case ATPModeSafeBrakeToStop:
        {
          dmiData.dmiATPMode = DMIATPModeSafeBrakeToStop;
          break;
        }
        case ATPModesCount:
        {
          dmiData.dmiATPMode = DMIATPModeUndefined;
          break;
        }
        default:
        {
          dmiData.dmiATPMode = DMIATPModeUndefined;
          break;
        }
      }

      // Set the ATPModeSubstate 
      dmiData.atpModeState = getATPModeSubState(atpMode);

      // Set the dmiData.driverVerState variable
      const Kernel::DriverLoginState atpDriverLoginState = Kernel::AbstractModeControl::corePtr()->getDriverLoginSeqState();
      dmiData.driverVerState = getDMIATPDriverVerState(atpDriverLoginState);
      //In sleeping mode driver is logedout but on DMI logout screen will be seen when NCU signal will be deactivated
      const Kernel::SleepingModeState sleepingModeState = Kernel::AbstractModeControl::corePtr()->getSleepingModeState();
      if ((ATPModeSleeping == atpMode) && (Kernel::SleepingMode::sleepingWaitSleepingSigDeactiv == sleepingModeState))
      {
        dmiData.driverVerState = DMIATPDriverVerificationStateNoActionState;
      }
      // Set the DMI ATP State
      dmiData.dmiATPStates = getDMIATPState(atpMode, atpDriverLoginState);

      // Set the dmiData.dmiIntegrityRelatedData

      // TODO: ATP should prevent driver to Login until protocol version is verified
      // Set bit for Radio Available in dmiData.dmiIntegrityRelatedData
      const bool isConnected = RadioCom::AbstractRadioHandler::corePtr()->getConnected();

      if (isConnected)
      {
        dmiData.dmiIntegrityRelatedData |= radioAvailable;
      }

      // Set bit for TIMS Available
      if (TG::AbstractTIMS::corePtr()->getTimsAvailable())
      {
        dmiData.dmiIntegrityRelatedData |= timsAvailable;
      }

      // Set bit for TIMS OK
      if (TG::AbstractTIMS::corePtr()->getTimsStatus() == TG::TIMSStatusIntact)
      {
        dmiData.dmiIntegrityRelatedData |= timsOK;
      }

      // Set bit for Stop Train in dmiData.dmiIntegrityRelatedData
      if (Kernel::AbstractModeControl::corePtr()->getStopTrainActive())
      {
        dmiData.dmiIntegrityRelatedData |= stopTrainRequest;
      }

      // Set bit for allowed to enter Yard mode
      if (Kernel::AbstractModeControl::corePtr()->isAllowedToEnterYardMode())
      {
        dmiData.dmiVisibilityControl |= allowedToEnterYardMode;
      }

      // Set bit for allowed to enter possession mode
      if (Kernel::AbstractModeControl::corePtr()->isAllowedToEnterPosMode())
      {
        dmiData.dmiVisibilityControl |= allowedToEnterPossessionMode;
      }

      // Set bit for allowed to enter shunting mode
      if (Kernel::AbstractModeControl::corePtr()->isAllowedToEnterShuntMode())
      {
        dmiData.dmiVisibilityControl |= allowedToEnterShuntingMode;
      }

      // Set bit to activate Login button if train is stand still, TCC is connected, ATPOK is activated and protocol version is matched
      if (Kernel::AbstractModeControl::corePtr()->isAllowedToLogin())
      {
        dmiData.dmiVisibilityControl |= enableLogin;
      }

      // Set bit for allowed to enter Configuration mode
      if ((Kernel::AbstractModeControl::corePtr()->isAllowedToEnterConfigMode()))
      {
        dmiData.dmiVisibilityControl |= allowedToEnterConfigMode;
      }

      const ATOMode atoMode = Kernel::AbstractModeControl::corePtr()->getATOMode();
      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      // Set Bit for Allowed to Change Train Name, when Driver is logged in. 
      if (isStandStill && trainNameChangeAllowedInMode && (ATOModeManual == atoMode) &&
        (atpDriverLoginState == Kernel::DriverLoginSeq::DriverLoginSeq::driverLoggedIn))
      {
        dmiData.dmiVisibilityControl |= allowedToChangeTrainName;
      }

      const bool allowedToAbort = Kernel::AbstractModeControl::corePtr()->isAllowedToAbortSetup();
      if ((DMIATPDriverVerificationStateLoggedOn == dmiData.driverVerState)
           && allowedToAbort && isStandStill)
      {
        dmiData.dmiVisibilityControl |= allowedToAbortSetup;
      }

      // Set Bit for Allowed to Logout when train is standstill and driver is already logged in
      //Also in Powering Down OR Safety Halt modes login is not allowed
      if (isStandStill && (DMIATPDriverVerificationStateLoggedOn == dmiData.driverVerState)
        && (!((ATPModePoweringDown == atpMode) || (ATPModeSafetyHalt == atpMode))))
      {
        dmiData.dmiVisibilityControl |= allowedToLogout;
      }

      // Status of BrakeTest (last or in progress)
      dmiData.brakeTestStatus = static_cast<uint8_t>(Supv::AbstractBrake::corePtr()->getBrakeTestStatus());

      // Remaining time until next mandatory Brake Test (driver shall be notified)
      dmiData.remainingTimeToMandatoryBrakeTest = Kernel::AbstractModeControl::corePtr()->getRemainingTimeToMandatoryBrakeTest();

      // Remaining time until next mandatory Routine Test (driver shall be notified)
      dmiData.remainingTimeToMandatoryRoutineTest = IO::AbstractBTMHandler::corePtr()->getRemainingTimeToMandatoryRoutineTest();

      // Check if any mode-change needs to be confirmed.
      dmiData.dmiConfirmModeChange = getConfirmModeChange(dmiData.dmiATPMode);


      // Set the dmiData.trainStatus
      // Set bit for MA Timeout in dmiData.trainStatus
      if (Kernel::AbstractModeControl::corePtr()->getMATimeOut())
      {
        dmiData.trainStatus |= trainStatusMaTimeOut;
      }

      // Set bit for Train Idling in dmiData.trainStatus
      if (Kernel::AbstractModeControl::corePtr()->getIdleState())
      {
        dmiData.trainStatus |= trainStatusTrainIdling;
      }

      // Bit regarding Safety halt shall also be set not only in safety halt mode but also in powering down mode,
      // if ATP is being powering down after safety halt event has occurred
      if (ATC::AbstractEventHandler::corePtr()->isModeChangeToSafetyHalt())
      {
        // Bit for Safety Halt 
        dmiData.trainStatus |= trainStatusSafetyHalt;
      }

      const Kernel::EmergencyAlertState currentEmergencyState = Kernel::AbstractModeControl::corePtr()->getEmergencyAlertSeqState();

      // Set bit for Emergency Alert and Emergency Alert Active in dmiData.trainStatus
      if (currentEmergencyState != Kernel::emergencyAlertInactive)
      {

        // Bit for Emergency Alert Active
        dmiData.trainStatus |= trainStatusEmergencyAlertActive;
      }

      const bool timsBroken = TG::TIMSStatusBroken == TG::AbstractTIMS::corePtr()->getTimsStatus();
      if (timsBroken)
      {
        // Set bit for train Integrity Broken
        dmiData.trainStatus |= trainStatusTIMSIntegrityBroken;
      }

      //Free Rolling
      if (Kernel::AbstractModeControl::corePtr()->getFreeRolling())
      {
        //Set bit for Free Rolling
        dmiData.trainStatus |= trainStatusFreeRolling;
      }

      // bit for Train integrity Granted by Driver
      if (TG::AbstractTIMS::corePtr()->getTimsSupervision() == TG::TIMSInhibited)
      {
        dmiData.trainStatus |= trainStatusIntegrityInhibitedByDriver;
      }

      // Set the dmiData.dmiATOMode
      dmiData.dmiATOMode = DMIATOModeManual;

      // Set the dmiData.dmiWantedATOSwitchPos
      dmiData.dmiWantedATOSwitchPos = DMIWantedATOSwitchPosUndefined;

      // Set the atoEnable
      if (AbstractConfig::corePtr()->getAtoEnable())
      {
        dmiData.additionalStatusBits1 |= additionalStatusAtoEnabled;
      }

      // Set the bit for Driving forward in additional status
      if (Kernel::AbstractModeControl::corePtr()->getCurrentDrivingDirection() == DirForward)
      {
        dmiData.additionalStatusBits1 |= additionalStatusDrivingForward;
      }

      // Set the bit for Standstill required in additional status except in Powering Down OR Safety Halt modes
      if (!((ATPModePoweringDown == atpMode) || (ATPModeSafetyHalt == atpMode)))
      {
        if (ATC::AbstractEventHandler::corePtr()->isStandstillEventActive())
        {
          // Set the bit for Standstill in additional status
          dmiData.additionalStatusBits1 |= additionalStatusStandStillEventActive;
        }
      }

      // Set the bit for Brake Test Possible in additional status
      if (Kernel::AbstractModeControl::corePtr()->getBrakeTestPossible())
      {
          dmiData.additionalStatusBits1 |= additionalStatusBrakeTestPossible;
          if (Kernel::AbstractModeControl::corePtr()->getBrakeTestMandatory())
          {
            // Set the bit for Brake Test Mandatory in additional status
            dmiData.additionalStatusBits1 |= additionalStatusBrakeTestMandatory;
          }
      }

      // Set the bit for Brake Test Notification in additional status
      if (Kernel::AbstractModeControl::corePtr()->getBrakeTestNotification())
      {
        dmiData.additionalStatusBits1 |= additionalStatusBrakeTestNotification;
      }

      // Set the atoLCSCommStatus
      if (TG::AbstractVehicleCom::corePtr()->connectedVehicleComm())
      {
        dmiData.additionalStatusBits2 |= additionalLcsCommunicationStatus;
      }

      const Kernel::LocationModeState locationModeState = Kernel::AbstractModeControl::corePtr()->getLocationModeState();
      const bool freeRollingInUnloadLocation = Kernel::AbstractModeControl::corePtr()->getFreeRolling()
        && (atpMode == ATPModeLocation) && (locationModeState == Kernel::LocationMode::locationUnloadLocation);

      if ((atpMode != ATPModeSleeping) && (!freeRollingInUnloadLocation) &&
        (atpDriverLoginState == Kernel::DriverLoginSeq::DriverLoginSeq::driverLoggedIn))
      {
        // Set the bit for Routine Test Possible in additional status 2
        if (IO::AbstractBTMHandler::corePtr()->getIsRoutineTestPossible())
        {
          dmiData.additionalStatusBits2 |= additionalStatusRoutineTestPossible;
        }

        // Set the bit for Routine Test Mandatory in additional status 2
        if (IO::AbstractBTMHandler::corePtr()->getIsRoutineTestMandatory())
        {
          dmiData.additionalStatusBits2 |= additionalStatusRoutineTestMandatory;
        }

        // Set the bit for Routine Test Needed in additional status 2
        if (IO::AbstractBTMHandler::corePtr()->getIsRoutineTestNeeded())
        {
          dmiData.additionalStatusBits2 |= additionalStatusRoutineTestNeeded;
        }
      }

      //Check valid train setup is available
      const bool validTrainSetup = DS::AbstractTSetup::corePtr()->isTrainSetupValid();
      const bool isTargetListEmpty = DS::AbstractTargets::corePtr()->isMATargetListEmpty();
      if (isStandStill &&  validTrainSetup && isTargetListEmpty)
      {
        //Set the bit Allowed to Show Train Composition
        dmiData.additionalAllowedToInfo |= allowedToShowTrainComp;
      }

      //Is allowed to show handling done button
      if (Kernel::AbstractModeControl::corePtr()->isAllowedToDisplayHandlingDone())
      {
        //Set the bit Allowed to Show Handling done button
        dmiData.additionalAllowedToInfo |= allowedToShowHandlingDone;
      }

      //Is allowed to show Request Cancel Registration Area
      const bool isDriverLoggedIn = (Kernel::AbstractModeControl::corePtr()->getDriverLoginSeqState() == Kernel::DriverLoginSeq::driverLoggedIn);

      const bool isAtpModeCorrectForCancelRegArea = ((atpMode == ATPModePowerUp) || (atpMode == ATPModeYard) || (atpMode == ATPModeConfiguration)
        || (atpMode == ATPModeRegistration) || (atpMode == ATPModeBaliseSearch));

      const bool isRegAreaMsgSendToCentralTCC = Kernel::AbstractMessageHandler::corePtr()->getRegistrationAreaMessageSentToCentralTCC();

      if (isDriverLoggedIn && isAtpModeCorrectForCancelRegArea && isRegAreaMsgSendToCentralTCC)
      {
        //Set the bit Allowed to Show Request Cancel Registration Area
        dmiData.additionalAllowedToInfo |= allowedToRequestCancelRegistrationArea;
      }

      //Is allowed to show Free rolling button 
      if (Kernel::AbstractModeControl::corePtr()->isAllowedToDisplayFreeRolling())
      {
        dmiData.additionalAllowedToInfo |= allowedToShowFreeRollButton;
      }

      // Always ask for A/B in core
      dmiData.additionalAllowedToInfo |= allowedToSelectCarsOnAOrBSide;

      const bool timsOverriddenDriver = TG::TIMSInhibited == TG::AbstractTIMS::corePtr()->getTimsSupervision();
      const bool timsConfirmed = TG::AbstractTIMS::corePtr()->getTimsConfirmed();

      if (((timsBroken) && ((ATOModeManual == atoMode) || (ATOModeSupervisedAutomatic == atoMode)) &&
        (isStandStill)) && (!timsOverriddenDriver))
      {
        //Set Allowed to Inhibit train integrity
        dmiData.additionalAllowedToInfo |= allowedToInhibitTrainIntegritySupv;
      }

      if (timsOverriddenDriver && timsConfirmed)
      {
        //Set Allowed to resume the train integrity supervision
        dmiData.additionalAllowedToInfo |= allowedToResumeTrainIntegritySupv;
      }

      //Is allowed to confirm clear free rolling
      if (Kernel::AbstractModeControl::corePtr()->isAllowedToDisplayConfirmFreeRolling())
      {
        dmiData.additionalConfirmInfo |= allowedToShowConfFreeRollButton;
      }

      if (Kernel::DriverLoginSeq::driverLoggedIn == Kernel::AbstractModeControl::corePtr()->getDriverLoginSeqState())
      {

        // Confirm the MA acceptance in Staff Responsible mode
        const Kernel::StaffResponsibleModeState staffResponsibleconfigState = Kernel::AbstractModeControl::corePtr()->getStaffResponsibleModeState();
        if ((ATPModeStaffResponsible == atpMode) && (Kernel::StaffResponsibleMode::staffResponsibleConfirmMAScratch == staffResponsibleconfigState))
        {
          dmiData.additionalConfirmInfo |= activateStaffResponsibleMAConfirm;
        }

        // Confirm the MA acceptance in Join mode
        const Kernel::JoinModeState joinModeconfigState = Kernel::AbstractModeControl::corePtr()->getJoinModeState();
        if ((ATPModeJoin == atpMode) && (Kernel::JoinMode::joinConfirmMAScratch == joinModeconfigState))
        {
          dmiData.additionalConfirmInfo |= activateJoinMAConfirmation;
        }

        // Confirm the MA acceptance in Shunting route mode
        const Kernel::TrainShuntingRouteModeState shuntingRouteconfigState = Kernel::AbstractModeControl::corePtr()->getShuntingRouteModeState();
        if ((ATPModeShuntingRoute == atpMode) && (Kernel::ShuntingRouteMode::shuntingRouteConfirmMAScratch == shuntingRouteconfigState))
        {
          dmiData.additionalConfirmInfo |= activateShuntingrouteMAConfirmation;
        }
      }
      // Confirmation of manual integrity report
      if (TG::AbstractTIMS::corePtr()->isManualConfirmationNeeded())
      {
        dmiData.additionalConfirmInfo |= confirmManualIntegrityConfirmation;
      }

      // Tachometer 1 is failed.
      if (Pos::AbstractOdometry::corePtr()->getTachometer1Failure())
      {
        dmiData.PlatformStatus |= platformStatusTachometer1Failure;
      }

      // Tachometer 2 is failed.
      if (Pos::AbstractOdometry::corePtr()->getTachometer2Failure())
      {
        dmiData.PlatformStatus |= platformStatusTachometer2Failure;
      }

      // Doppler is failed.
      if (Pos::AbstractOdometry::corePtr()->getDopplerFailure())
      {
        dmiData.PlatformStatus |= platformStatusDopplerFailure;
      }

      //check if driver has confirmed the tachometer1 failure
      if (!AbstractDMIHandler::corePtr()->getDriverTacho1FailureConfirmation())
      {
        if ((AbstractDMIHandler::corePtr()->getOdometerTacho1Failure()))
        {
          dmiData.additionalConfirmInfo2 |= confirmTachometer1Failure;
        }
      }


      //check if driver has confirmed the tachometer2 failure
      if (!AbstractDMIHandler::corePtr()->getDriverTacho2FailureConfirmation())
      {
        if ((AbstractDMIHandler::corePtr()->getOdometerTacho2Failure()))
        {
          dmiData.additionalConfirmInfo2 |= confirmTachometer2Failure;
        }
      }


      //check if driver has confirmed the doppler failure
      if (!AbstractDMIHandler::corePtr()->getDriverDopplerFailureConfirmation())
      {
        if ((AbstractDMIHandler::corePtr()->getOdometerDopplerFailure()))
        {
          dmiData.additionalConfirmInfo2 |= confirmDopplerFailure;
        }
      }

      dmiDataProcessState = DMIDataAvailable;
    }
    /******************************************************************************
    * DMIMessageOutATPModesAndStatus::validate
    ******************************************************************************/
    bool DMIMessageOutATPModesAndStatus::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        if ((AbstractDMIHandler::corePtr()->getCycleCount() % sendCycleATPModeAndState) == delayATPModeAndState)
        {
          if (AbstractDMIHandler::corePtr()->getDMICompatibilityVersionAccepted())
          {
            if (assembleDMIMessageData())
            {
              trace->write(ATC::briefTrace, "DMI Handler: DMI Message validated :ATP Modes And States");
              dmiDataProcessState = DMIDataValidated;
            }
          }
        }
      }
      return(DMIDataValidated == dmiDataProcessState);
    }

    /******************************************************************************
    * DMIMessageOutATPModesAndStatus::invalidate
    ******************************************************************************/
    void DMIMessageOutATPModesAndStatus::invalidate()
    {
      // Clear all the data
      dmiData.invalidate();

      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * DMIMessageOutATPModesAndStatus::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutATPModesAndStatus::assembleDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));

      // Header Type
      messageData.headerType = dmiHeaderTypeUnAckMsg;
      // Message Number
      messageData.msgNumber = unacknowledgedMessageNumber;

      // Get DMIMessageType 
      messageData.dmiData.msgType = static_cast<uint8_t>(messageType);

      // Assemble data and write in network order
      vfwPutU8(&buffer, static_cast<uint8_t>(dmiData.dmiATPMode));
      vfwPutU8(&buffer, static_cast<uint8_t>(dmiData.dmiATPStates));
      vfwPutU8(&buffer, static_cast<uint8_t>(dmiData.atpModeState));
      vfwPutU8(&buffer, static_cast<uint8_t>(dmiData.driverVerState));
      vfwPutU8(&buffer, dmiData.dmiIntegrityRelatedData);
      vfwPutU32(&buffer, dmiData.trainStatus);
      vfwPutU8(&buffer, static_cast<uint8_t>(dmiData.dmiATOMode));
      vfwPutU8(&buffer, static_cast<uint8_t>(dmiData.dmiWantedATOSwitchPos));
      vfwPutU8(&buffer, dmiData.additionalStatusBits1);
      vfwPutU8(&buffer, dmiData.additionalStatusBits2);
      vfwPutU8(&buffer, static_cast<uint8_t>(dmiData.dmiConfirmModeChange));
      vfwPutU8(&buffer, dmiData.dmiVisibilityControl);
      vfwPutU8(&buffer, dmiData.brakeTestStatus);
      vfwPutU16(&buffer, dmiData.remainingTimeToMandatoryBrakeTest);
      vfwPutU16(&buffer, dmiData.remainingTimeToMandatoryRoutineTest);
      vfwPutU8(&buffer, dmiData.additionalAllowedToInfo);
      vfwPutU8(&buffer, dmiData.additionalConfirmInfo);
      vfwPutU8(&buffer, dmiData.adaptationTrainStatus);
      vfwPutU8(&buffer, dmiData.additionalConfirmInfo2);
      vfwPutU8(&buffer, dmiData.PlatformStatus);
      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      // Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * DMIMessageOutATPModesAndStatus::logToRU
    ******************************************************************************/
    void DMIMessageOutATPModesAndStatus::logToRU() const
    {
      static bool firstMessageLogged = false;
      static DmiData lastDmiData;

      if ((!firstMessageLogged) || (dmiData != lastDmiData))
      {
        firstMessageLogged = true;
        lastDmiData = dmiData;

        AbstractDMIMessageOut::logToRU();
      }
    }

    /******************************************************************************
    * DMIMessageOutATPModesAndStatus::getDMIATPState
    ******************************************************************************/
    DMIATPStates DMIMessageOutATPModesAndStatus::getDMIATPState(ATPMode const atpmode,
      Kernel::DriverLoginState const atpDriverLoginState) const
    {
      DMIATPStates retValue = DMIATPStatusActive;

      switch (atpmode)
      {
        case ATPModePowerUp:
        {
          retValue = DMIATPStatusActivationInitiation;
          break;
        }
        case ATPModeConfiguration:
        case ATPModeRegistration:
        case ATPModeBaliseSearch:
        {
          if (atpDriverLoginState == Kernel::DriverLoginSeq::driverLoggedIn)
          {
            retValue = DMIATPStatusActive;
          }
          else
          {
            retValue = DMIATPStatusActivationInitiation;
          }

          break;
        }

        case ATPModeSafetyHalt:
          retValue = DMIATPStatusFatalFailureState;
          break;

        case ATPModeNormal:
        case ATPModeShunting:
        case ATPModeLocation:
        case ATPModeYard:
        case ATPModeUnregistered:
        case ATPModePoweringDown:
        case ATPModeSleeping:
        case ATPModeStaffResponsible:
        case ATPModeShuntingRoute:
        case ATPModePossession:
        case ATPModeSplit:
        case ATPModeJoin:
        case ATPModeSafeBrakeToStop:
          break;
        case ATPModeUndefined:
        case ATPModesCount:
        default:
          ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
          break;
      }

      return retValue;
    }

    /******************************************************************************
    * DMIMessageOutATPModesAndStatus::getDMIATPDriverVerState
    ******************************************************************************/
    DriverVerificationState DMIMessageOutATPModesAndStatus::getDMIATPDriverVerState(
      Kernel::DriverLoginState const atpDriverLoginState) const
    {
      DriverVerificationState retValue = DMIATPDriverVerificationStateUndefined;

      if (atpDriverLoginState == Kernel::DriverLoginSeq::driverLoginVerification)
      {
        retValue = DMIATPDriverVerificationStateVerificationState;
      }
      else if (atpDriverLoginState == Kernel::DriverLoginSeq::driverLoginFailed)
      {
        retValue = DMIATPDriverVerificationStateRedoInputState;
      }
      else if (atpDriverLoginState == Kernel::DriverLoginSeq::driverLoggedIn)
      {
        retValue = DMIATPDriverVerificationStateLoggedOn;
      }
      else if (atpDriverLoginState == Kernel::DriverLoginSeq::driverLoggedOut)
      {
        retValue = DMIATPDriverVerificationStateInputState;
      }
      else
      {
        retValue = DMIATPDriverVerificationStateNoActionState;
      }

      return retValue;
    }


    /******************************************************************************
    * DMIMessageOutATPModesAndStatus::getDMIATPconfigModeSubstate
    ******************************************************************************/
    ATPModeSubState DMIMessageOutATPModesAndStatus::getATPModeSubState(
      ATPMode const atpMode) const
    {
      ATPModeSubState retValue = UndefinedModeSubState;

      if (atpMode == ATPModeConfiguration)
      {
        // configModeSubstate is Data2 in message (“Undefined”=0, “Manual”=1, “Reconfigure”=2)
        switch (Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState())
        {
          case (Kernel::TrainConfigMode::trainConfigStart):
          case (Kernel::TrainConfigMode::trainConfigWaitSetupFrmATP):
          case (Kernel::TrainConfigMode::trainConfigWaitTIC):
          {
            retValue = UndefinedModeSubState;
            break;
          }

          case (Kernel::TrainConfigMode::trainConfigWaitNewConfigDMI):
          {
            retValue = ATPModeSubStateConfigManualOrPowerUpSelectMode;
            break;
          }

          case (Kernel::TrainConfigMode::trainConfigWaitSetupFrmTCC):
            break;

          case (Kernel::TrainConfigMode::trainConfigConfirmReRegFrmDMI):
          {
            retValue = ATPModeSubStateConfigReReg;
            break;
          }

          case (Kernel::TrainConfigMode::trainConfigSendReRegDataToDMI):
          {
            retValue = ATPModeSubStateConfigReReg;
            break;
          }
          case (Kernel::TrainConfigMode::trainConfigWaitForAcceptAutomatic):
          {
            retValue = ATPModeSubStateAutoConfig;
            break;
          }
          case (Kernel::TrainConfigMode::trainConfigRejectedOrAborted):
          {
            retValue = ATPModeSubStateConfigReStarted;
            break;
          }
          default:
            retValue = UndefinedModeSubState;
            break;
        }
      }
      else if (atpMode == ATPModePowerUp)
      {
        if (Kernel::AbstractModeControl::corePtr()->getDriverLoginSeqState() == Kernel::DriverLoginSeq::driverLoggedIn)
        {
          retValue = ATPModeSubStateConfigManualOrPowerUpSelectMode;
        }
        else
        {
          retValue = UndefinedModeSubState;
        }
      }
      else
      {
        retValue = UndefinedModeSubState;
      }

      return retValue;
    }


    /******************************************************************************
    * DMIMessageOutATPModesAndStatus::getConfirmModeChange
    ******************************************************************************/
    DMIConfirmModeChange DMIMessageOutATPModesAndStatus::getConfirmModeChange(const DMIATPMode mode) const
    {
      DMIConfirmModeChange modeChange = DMIConfirmModeChangeUndefined;

      const Kernel::ModeRequestSeqState curYardModeSubModeState
          = Kernel::AbstractModeControl::corePtr()->getYardModReqSeqState();
      const Kernel::DriverLoginState curDriverLoginState
        = Kernel::AbstractModeControl::corePtr()->getDriverLoginSeqState();

      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

      bool  isreadytoEnterYard = Kernel::YardModeRequestSeq::YardDmiAckManualConfirm == curYardModeSubModeState;

      switch (mode)
      {
        case DMIATPModeShuntingRoute:
        {
          if (Kernel::DriverLoginSeq::driverLoggedIn == curDriverLoginState)
          {
            const Kernel::TrainShuntingRouteModeState curShuntingRouteSubModeState
              = Kernel::AbstractModeControl::corePtr()->getShuntingRouteModeState();

            // Need to confirm changed to ShuntingRoute?
            if (Kernel::ShuntingRouteMode::shuntingRouteWaitConfirmFromDMI == curShuntingRouteSubModeState)
            {
              modeChange = DMIConfirmModeChangeToShuntingRoute;
            }
            else if (isreadytoEnterYard)
            {
              modeChange = DMIConfirmModeChangeToYard;
            }
            else
            {
              //To Please lint
            }
          }
          break;
        }

        case DMIATPModeYardMode:
        {
          const Kernel::YardModeState curYardSubModeState
            = Kernel::AbstractModeControl::corePtr()->getYardModeState();
          
          if (Kernel::DriverLoginSeq::driverLoggedIn == curDriverLoginState)
          {
            // Need to confirm before changing to Sleep?
            if ((Kernel::YardMode::yardWaitSleepConfirmDMI == curYardSubModeState) && isStandStill)
            {
              modeChange = DMIConfirmModeChangeToSleep;
            }
          } 
          // Driver to confirm mode change to yard mode as TCC not connected
          else if (Kernel::DriverLoginSeq::driverLoggedOut == curDriverLoginState)
          {
            if (isreadytoEnterYard)
            {
              modeChange = DMIConfirmModeChangeToYard;
            }
          }
          else
          {
            modeChange = DMIConfirmModeChangeUndefined;
          }
          break;
        }

        case DMIATPModeStaffResponsible:
        {
          if (Kernel::DriverLoginSeq::driverLoggedIn == curDriverLoginState)
          {
            const Kernel::StaffResponsibleModeState curStaffRespSubModeState
              = Kernel::AbstractModeControl::corePtr()->getStaffResponsibleModeState();

            // Need to confirm changed to StaffResponsible?
            if (Kernel::StaffResponsibleMode::staffResponsibleWaitConfirmDMI == curStaffRespSubModeState)
            {
              modeChange = DMIConfirmModeChangeToStaffResponsible;
            }
            else if (isreadytoEnterYard)
            {
              modeChange = DMIConfirmModeChangeToYard;
            }
            else
            {
              modeChange = DMIConfirmModeChangeUndefined;
            }
          }
          break;
        }

        case DMIATPModeJoin:
        {
          if (Kernel::DriverLoginSeq::driverLoggedIn == curDriverLoginState)
          {
            const Kernel::JoinModeState curJoinSubModeState
              = Kernel::AbstractModeControl::corePtr()->getJoinModeState();

            // Need to confirm changed to Join?
            if (Kernel::JoinMode::joinWaitJoinConfirmDMI == curJoinSubModeState)
            {
              modeChange = DMIConfirmModeChangeToJoin;
            }
            // Need to confirm before changing to Sleep?
            else if ((Kernel::JoinMode::joinWaitSleepConfirmDMI == curJoinSubModeState) && isStandStill)
            {
              modeChange = DMIConfirmModeChangeToSleep;
            }
            else if (isreadytoEnterYard)
            {
              modeChange = DMIConfirmModeChangeToYard;
            }
            else
            {
              modeChange = DMIConfirmModeChangeUndefined;
            }
          }
          break;
        }

        case DMIATPModeSplit:
        {
          if (Kernel::DriverLoginSeq::driverLoggedIn == curDriverLoginState)
          {
            const Kernel::SplitModeState curSplitSubModeState
              = Kernel::AbstractModeControl::corePtr()->getSplitModeState();

            // Need to confirm changed to Split?
            if (Kernel::SplitMode::splitWaitConfirmDMI == curSplitSubModeState)
            {
              modeChange = DMIConfirmModeChangeToSplit;
            }
            else if (isreadytoEnterYard)
            {
              modeChange = DMIConfirmModeChangeToYard;
            }
            else
            {
              modeChange = DMIConfirmModeChangeUndefined;
            }
          }
          break;
        }

        case DMIATPModePowerUp:
        {
          const Kernel::PowerUpModeState curPowerUpSubModeState
            = Kernel::AbstractModeControl::corePtr()->getPowerUpModeState();

            if (Kernel::PowerUpMode::powerUpWaitSleepConfirmDMI == curPowerUpSubModeState)
            {
              if (Kernel::DriverLoginSeq::driverLoggedIn == curDriverLoginState)
              {
                modeChange = DMIConfirmModeChangeToSleep;
              }
            }
            else if (isreadytoEnterYard)
            {
              modeChange = DMIConfirmModeChangeToYard;
            }
            else
            {
              modeChange = DMIConfirmModeChangeUndefined;
            }

         break;
        }

        case DMIATPModeShunting:
        {
          if (Kernel::DriverLoginSeq::driverLoggedIn == curDriverLoginState)
          {
            const Kernel::ShuntingModeState curShuntingSubModeState
                = Kernel::AbstractModeControl::corePtr()->getShuntingModeState();
            // Need to confirm before changing to Sleep?
            if ((Kernel::ShuntingMode::shuntingWaitSleepConfirmDMI == curShuntingSubModeState) && isStandStill)
            {
              modeChange = DMIConfirmModeChangeToSleep;
            }
            else if (isreadytoEnterYard)
            {
              modeChange = DMIConfirmModeChangeToYard;
            }
            else
            {
              modeChange = DMIConfirmModeChangeUndefined;
            }
          }
          break;
        }
        case DMIATPModeSleeping:
        case DMIATPModeLocation:
        case DMIATPModePossession:
        case DMIATPModeUnregistered:
        case DMIATPModeTrainConfiguration:
        case DMIATPModeTrainRegistration:
        case DMIATPModeBaliseSearchMode:
        case DMIATPModeNormal:
        case DMIATPModeSafeBrakeToStop:
        {
            if (isreadytoEnterYard)
            {
                modeChange = DMIConfirmModeChangeToYard;
            }
            break;
        }
 
        case DMIATPModePowerDown:
        case DMIATPModeSafetyHalt:
        case DMIATPModeUndefined:
        default:
          break;
      }

      return modeChange;
    }
  }
}

