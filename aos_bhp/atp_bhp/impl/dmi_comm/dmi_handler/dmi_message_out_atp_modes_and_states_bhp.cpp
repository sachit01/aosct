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
* 2018-17-11    akushwah    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "dmi_message_out_atp_modes_and_states_bhp.hpp"
#include "targets.hpp"
#include "message_handler.hpp"
#include "mode_control.hpp"
#include "train_config_mode_bhp.hpp"
#include "vehicle_com.hpp"
#include "config.hpp"
#include "abstract_tsetup.hpp"

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
    * DMIMessageOutATPModesAndStatus constructor
    ******************************************************************************/
    DMIMessageOutATPModesAndStatusBHP::DMIMessageOutATPModesAndStatusBHP() : DMIMessageOutATPModesAndStatus()
    {
      dmiData.invalidate();
    }

    /******************************************************************************
    * DMIMessageOutATPModesAndStatus::collectData
    ******************************************************************************/
    void DMIMessageOutATPModesAndStatusBHP::collectData()
    {
      const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      //Set bit for Radio Channel Enable
      if ((Kernel::MessageHandler::instance().getRadioChannelEnable())
        && (ATPModePoweringDown != currentMode))
      {
        dmiData.additionalAllowedToInfo |= radioChannelEnable;
      }

      //Call the Collect data for Core part
      DMIMessageOutATPModesAndStatus::collectData();

      const bool lastCarBrkTestInProgress = getLastCarBrkTestInProgress();

      if (Kernel::ModeControl::instance().isManualAbortConfirmationNeeded())
      {
        if ((Kernel::ModeControl::instance().getLastCarBrakePressureTestAborted()))
        {
          if (lastCarBrkTestInProgress)
          {
            dmiData.additionalConfirmInfo |= confirmAbortLastCarBrakeTestInProgress;
          }
        }
      }

      if (lastCarBrkTestInProgress)
      {
        dmiData.additionalStatusBits2 |= additionalLastCarBrkTestinProgress;
      }

      const Kernel::TrainConfigModeState configState = Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();
      if ((ATPModeConfiguration == currentMode)
        && (Kernel::TrainConfigModeBHP::trainConfigWaitForDepartureTest == configState))
      {
        dmiData.additionalConfirmInfo |= activateDepartureTestConfirmation;
      }

      if (Kernel::ModeControl::instance().getConfirmTrainLoadedStateNeeded()
        && (ATPModeConfiguration == currentMode)
        && ((Kernel::TrainConfigMode::trainConfigWaitForAcceptAutomatic == configState) ||
        (Kernel::TrainConfigMode::trainConfigWaitNewConfigDMI == configState)))
      {
        dmiData.additionalConfirmInfo |= confirmTrainLoadedStatusChange;
      }

      if (lastCarBrkTestInProgress)
      {
        // Disable brake-test request from driver while Last Car Brake Test in progress because the two brake-tests may interfere 
        dmiData.additionalStatusBits1 &= ~additionalStatusBrakeTestPossible;
        dmiData.additionalStatusBits1 &= ~additionalStatusBrakeTestMandatory;
      }

      // The AOS shall present the current load status used by AOS to the Driver
      if (DS::AbstractTSetup::corePtr()->getTrainLoadStatus() == TrainIsLoaded)
      {
        // Set bit for additional status 1 when the train is loaded 
        dmiData.additionalStatusBits1 |= additionalTrainStatusTrainLoaded;
      }

      uint16_t approachingSpeedLevelCrossing = 0U;
      if (DS::Targets::instance().getApproachingLevelCrossing(approachingSpeedLevelCrossing) && (0U != approachingSpeedLevelCrossing))
      {
        // Set bit for additional status 1, In Approach area for level crossing
        dmiData.additionalStatusBits1 |= additionalStatusInApproachAreaForLevelCrossing;
      }

      // Comm status is set in the core, but adaptation overrides this depending on LocoType
      const bool isEMDLoco = (EMD == static_cast<LocoTypeAdap>(AbstractConfig::corePtr()->getLocoType())) ? true : false;
      if (!isEMDLoco)
      {
        // Set the atoLCSCommStatus to always OK (in order to hide the indication on the DMI), when LocoType is not EMD
        dmiData.additionalStatusBits2 |= additionalLcsCommunicationStatus;
      }

      // Set current brake system in use (M_BRAKE_SYSTEM)
      const bool isAtpModeInBrakesystem = ((ATPModeConfiguration == currentMode) || (ATPModeRegistration == currentMode) || (ATPModeBaliseSearch == currentMode)
        || (ATPModeNormal == currentMode) || (ATPModeLocation == currentMode) || (ATPModeStaffResponsible == currentMode) || (ATPModeShuntingRoute == currentMode)
        || (ATPModeSplit == currentMode) || (ATPModeJoin == currentMode) || (ATPModeSafeBrakeToStop == currentMode));

      if (isAtpModeInBrakesystem)
      {
        collectBrakeSystemInUse();
      }
      else
      {
        // do nothing
      }


    }

    /******************************************************************************
    * DMIMessageOutATPModesAndStatus::getLastCarBrkTestInProgress
    ******************************************************************************/
    bool DMIMessageOutATPModesAndStatusBHP::getLastCarBrkTestInProgress() const
    {
      bool testInProgress = false;
      const DS::TrainSetup* trainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

      // do not access trainSetup if NULL
      if (trainSetup != NULL)
      {
        if (trainSetup->timsSupNeeded)
        {
          // It is not enough to check only for the ConfigModeState, because train-configuration will pass these states also 
          // for other vehicle types than EMD and for other brakeSystems than PB
          // when actually Last Car brake-test is not in progress
          const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
          const Kernel::TrainConfigModeState configState = Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();
          const bool configModeStateLastCarBrakeTestInProgress = ((Kernel::TrainConfigModeBHP::trainConfigPreDepartureBrakeTest == configState) ||
            (Kernel::TrainConfigModeBHP::trainConfigCheckValidBrkPrFeedBack == configState));
          const LocoTypeAdap locoType = static_cast<LocoTypeAdap>(Config::instance().getLocoType());
          const BrakeSystemType brakeSystem = DS::AbstractTSetup::corePtr()->getBrakeSystemInUse();

          if ((ATPModeConfiguration == currentMode) &&
            configModeStateLastCarBrakeTestInProgress &&
            (locoType == EMD) &&
            (brakeSystem == BrakeSystemType1))
          {
            testInProgress = true;
          }
        }
      }
      return testInProgress;
    }
    /******************************************************************************
    * DMIMessageOutATPModesAndStatus::collectBrakeSystemInUse
    ******************************************************************************/
    void  DMIMessageOutATPModesAndStatusBHP::collectBrakeSystemInUse()
    {
      BrakeSystemType brakeSystem = TG::VehicleCom::instance().getBrakeSystem();
      if (DS::AbstractTSetup::corePtr()->isTrainSetupValid())
      {
        brakeSystem = DS::AbstractTSetup::corePtr()->getBrakeSystemInUse();
      }
      else
      {
        brakeSystem = TG::VehicleCom::instance().getBrakeSystem();
      }
      switch (brakeSystem)
      {
      case BrakeSystemTypeUndefined:
        break;

      case BrakeSystemType1:
        dmiData.additionalStatusBits2 |= additionalStatusPneumaticBrakeSystem;
        break;

      case BrakeSystemType2:
        dmiData.additionalStatusBits2 |= additionalStatusECPBBrakeSystem;
        break;

      case BrakeSystemType3:
        dmiData.additionalStatusBits2 |= additionalStatusBrakeSystem3;
        break;

      default:
        break;
      }

    }
  }
}

