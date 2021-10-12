/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the TrainConfigModeBHP class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-05-10    skothiyal    Created
* 2019-02-22    ramyashree   updated according to requirements
*
*******************************************************************************/
#include "atc_math.hpp"
#include "train_config_mode_bhp.hpp"
#include "dmi_handler.hpp"
#include "tims.hpp"
#include "config.hpp"
#include "tsetup.hpp"
#include "mode_control_event_ids.hpp"
#include "dmi_bhp_event_codes.hpp"
#include "atp_types.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "brake.hpp"
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
    TrainConfigModeBHP::TrainConfigModeBHP() : TrainConfigMode(),
      abortLastCarBrakeTestButtonPressed(false),
      startTimeToConfirmAbortOfLastCarBrakeTest(0),
      trainLoadedStatusRequested(TrainIsLoaded),
      confirmTrainLoadedStateNeeded(false),
      lastCarBrakeTestPending(true),
      confirmTrainLoadedWaitCycles(0U),
      waitForBrakePressureTimer(0U),
      waitingFeedbackFromOBRD(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer,
        eventIdBrakeToCheckOBRDFeedback, DMICom::checkingLastCarBrakePressure,
        "Brake applied, waiting for last car brake pressure to drop")),
      brakePressureDidDrop(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer,
        eventIdLastCarBrakePressurePassed, DMICom::lastCarBrakePressurePassed,
        "Last car brake pressure dropped as expected")),
      brakePressureDidNotDrop(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer,
        eventIdLastCarBrakePressureNotValid, DMICom::lastCarBrakePressureFailed,
        "Timeout while waiting for last car brake pressure to drop")),
      abortLastCarBrakeTestConfirmed(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer,
        eventIdLastCarBrakePressureTestAborted, DMICom::lastCarBrakePressureTestAborted,
        "Last car brake pressure test is aborted by driver")),
      lastCarBrakeTestIsPending(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer,
        eventIdLastCarBrakePressureTestInProgress, DMICom::lastCarBrakePressureIsPending,
        "Last car brake presssure test is pending"))
    {
    }


    /******************************************************************************
    * runTrainConfigTSetupAccepted
    ******************************************************************************/
    void TrainConfigModeBHP::runTrainConfigTSetupAccepted()
    {
      modeState = trainConfigWaitForDepartureTest;
    }

    /******************************************************************************
    * runTrainConfigWaitForDepartureTest
    ******************************************************************************/
    void TrainConfigModeBHP::runTrainConfigWaitForDepartureTest()
    {
      const bool isDepartureTestConfirm
        = (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonConfirmDeparture);

      if (isDepartureTestConfirm)
      {
        modeState = trainConfigFinishOK;
      }
    }

    /******************************************************************************
    * runTrainConfigWaitForAcceptAutomatic (Automatic configuration)
    ******************************************************************************/
    void TrainConfigModeBHP::runTrainConfigWaitForAcceptAutomatic()
    {
      if (!handleTrainLoadedConfirmation())
      {
        // Confirm not in progress, core behavior
        TrainConfigMode::runTrainConfigWaitForAcceptAutomatic();
      }
    }

    /******************************************************************************
    * runTrainConfigWaitNewConfigDMI (Manual configuration)
    ******************************************************************************/
    void TrainConfigModeBHP::runTrainConfigWaitNewConfigDMI(CommonDataForModes &commonData)
    {
      if (!handleTrainLoadedConfirmation())
      {
        // Confirm not in progress, core behavior
        TrainConfigMode::runTrainConfigWaitNewConfigDMI(commonData);
      }
    }

    /******************************************************************************
    * handleTrainLoadedConfirmation (Same for Manual / Automatic configuration
    ******************************************************************************/
    bool TrainConfigModeBHP::handleTrainLoadedConfirmation()
    {
      bool confirmationInProgress = false;
      TrainLoaded trainLoadedStatusRequestedByDriver;
      // DMI sent TrainLoaded message?
      if (DMICom::DMIHandler::instance().getTrainLoadedStatusRequestedByDriver(trainLoadedStatusRequestedByDriver))
      {
        // Save requested LoadedStatus until confirmed and saved
        trainLoadedStatusRequested = trainLoadedStatusRequestedByDriver;
        // Check if Train Loaded state same as already is stored in TSetup
        if (DS::TSetup::instance().getTrainLoadStatus() == trainLoadedStatusRequested)
        { // Confirm not needed
          confirmTrainLoadedStateNeeded = false;
        }
        else
        { // Confirm needed
          confirmTrainLoadedStateNeeded = true;
          confirmTrainLoadedWaitCycles = 0U;
        }
      }
      if (confirmTrainLoadedStateNeeded)
      {
        confirmationInProgress = true;
        // Wait for Confirm, Cancel or Timeout
        confirmTrainLoadedWaitCycles++;
        if (DMICom::DMIHandler::instance().getDMIButtonStatus() == DMICom::DMIButtonConfirmChangeOfTrainLoaded)
        { // Train Loaded state confirmed
          // Store Train Loaded state 
          DS::TSetup::instance().setTrainLoadStatus(trainLoadedStatusRequested);
          modeState = trainConfigSendStartUpForNewConfig;
          confirmTrainLoadedStateNeeded = false;
          confirmationInProgress = false;
        }
        else if ((DMICom::DMIHandler::instance().getDMIButtonStatus() == DMICom::DMIButtonCancelChangeOfTrainLoaded)  ||
                 (confirmTrainLoadedWaitCycles > confirmTrainLoadWaitCyclesMax))
        { // Confirm Train Loaded state canceled
          confirmTrainLoadedStateNeeded = false;
        }
        else
        {
          // Wait for confirmation
        }
      }
      else
      {   // No confirmation to handle
        confirmationInProgress = false;
      }
      return confirmationInProgress;
    }

    /******************************************************************************
    * handleAbortBrakeTestActions
    ******************************************************************************/
    void TrainConfigModeBHP::handleAbortBrakeTestActions()
    {
      // To Cancel the confirmation handshake if times out
      const int64_t timeNow = vfwGetReferenceTime();
      if ((timeNow - startTimeToConfirmAbortOfLastCarBrakeTest) > maxConfirmationTime)
      {
        startTimeToConfirmAbortOfLastCarBrakeTest = 0;
      }
      const DMICom::DMIButtonStatus abortStatus = DMICom::DMIHandler::instance().getDMIButtonStatus();

      switch (abortStatus)
      {
      case DMICom::DMIButtonAbortLastCarBrakeTest:
        abortLastCarBrakeTestButtonPressed = true;
        startTimeToConfirmAbortOfLastCarBrakeTest = vfwGetReferenceTime();
        break;
      case DMICom::DMIButtonConfirmAbortLastCarBrakeTest:
        abortLastCarBrakeTestButtonPressed = false;
        startTimeToConfirmAbortOfLastCarBrakeTest = 0;
        ATC::AbstractEventHandler::corePtr()->reportEvent(abortLastCarBrakeTestConfirmed, __FILE__, __LINE__);
        modeState = trainConfigTSetupAccepted;
        break;
      case DMICom::DMIButtonCancelAbortLastCarBrakeTest:
        startTimeToConfirmAbortOfLastCarBrakeTest = 0;
        abortLastCarBrakeTestButtonPressed = true;
        break;
        //To Please Lint
      case DMICom::DMIButtonUndefined:
      case DMICom::DMIButtonBrakeRelease:
      case DMICom::DMIButtonTIMSInhibit:
      case DMICom::DMIButtonTIMSResume:
      case DMICom::DMIButtonTrainConfig:
      case DMICom::DMIButtonSpare5:
      case DMICom::DMIButtonHandlingDone:
      case DMICom::DMIButtonEnterYardMode:
      case DMICom::DMIButtonManualTrainIntegrity:
      case DMICom::DMIButtonConfirmManualTrainIntegrity:
      case DMICom::DMIButtonCancelManualTrainIntegrity:
      case DMICom::DMIButtonRetryConfig:
      case DMICom::DMIButtonAbortSetup:
      case DMICom::DMIButtonLogoutDriver:
      case DMICom::DMIButtonEnterPossessionMode:
      case DMICom::DMIButtonShuntingMode:
      case DMICom::DMIButtonSpare16:
      case DMICom::DMIButtonStartBrakeTest:
      case DMICom::DMIButtonAbortBrakeTest:
      case DMICom::DMIButtonStartBtmTest:
      case DMICom::DMIButtonConfirmYard:
      case DMICom::DMIButtonConfirmShuntingRoute:
      case DMICom::DMIButtonConfirmStaffResponsible:
      case DMICom::DMIButtonConfirmJoin:
      case DMICom::DMIButtonConfirmSleep:
      case DMICom::DMIButtonConfirmSplit:
      case DMICom::DMIButtonConfirmDeparture:
      case DMICom::DMIButtonAcceptAutomaticConfig:
      case DMICom::DMIButtonRequestFreeRolling:
      case DMICom::DMIButtonConfirmFreeRollingCleared:
      case DMICom::DMIButtonConfirmStaffResponsibleMA:
      case DMICom::DMIButtonConfirmShuntingRouteMA:
      case DMICom::DMIButtonConfirmJoinMA:
      case DMICom::DMIButtonCancelRegistrationArea:
      case DMICom::DMIButtonConfirmChangeOfTrainLoaded:
      case DMICom::DMIButtonCancelChangeOfTrainLoaded:
      case DMICom::DMIButtonConfirmTachometer1Failure:
      case DMICom::DMIButtonConfirmTachometer2Failure:
      case DMICom::DMIButtonConfirmDopplerFailure:
      case DMICom::DMIButtonMaxCount:
        // Do nothing
        break;
      }
    }

    /******************************************************************************
    * checkBrakePressureLocoBelowPsb
    ******************************************************************************/
    bool TrainConfigModeBHP::checkBrakePressureLocoBelowPsb() const
    {
      uint16_t bpValue = 0U;

      bool brakePressureOk = Supv::Brake::instance().getBrakePressureFb(bpValue);

      if (brakePressureOk)
      {
        if (bpValue >= Config::instance().getOBRDBrakeTestPressure())
        {
          brakePressureOk = false;
        }
      }

      return brakePressureOk;
    }

    /******************************************************************************
    * getConfirmTrainLoadedStateNeeded
    ******************************************************************************/
    bool TrainConfigModeBHP::getConfirmTrainLoadedStateNeeded() const
    {
      return confirmTrainLoadedStateNeeded;
    }

    /******************************************************************************
    * getLastCarBrakePressureTestAborted
    ******************************************************************************/
    bool TrainConfigModeBHP::getLastCarBrakePressureTestAborted() const
    {
      return abortLastCarBrakeTestButtonPressed;
    }

    /******************************************************************************
    * isManualAbortConfirmationNeeded
    ******************************************************************************/
    bool TrainConfigModeBHP::isManualAbortConfirmationNeeded() const
    {
      return startTimeToConfirmAbortOfLastCarBrakeTest != 0;
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void TrainConfigModeBHP::resetMode()
    {
      confirmTrainLoadedStateNeeded = false;
      lastCarBrakeTestPending = true;
      TrainConfigMode::resetMode();
    }

    /******************************************************************************
    runModeFunction
    ******************************************************************************/
    void TrainConfigModeBHP::runModeFunction(CommonDataForModes &commonData)
    {
      // Run the function corresponding to the modeState.
      switch (modeState)
      {
      case trainConfigPreDepartureBrakeTest:
        runTrainConfigPreDepartureBrakeTest();
        handleAbortBrakeTestActions();
        break;

      case trainConfigCheckValidBrkPrFeedBack:
        handleAbortBrakeTestActions();
        runTrainConfigCheckValidBrkPrFeedBack(commonData);
        break;

      default:
        TrainConfigMode::runModeFunction(commonData);
      }
    }

    /******************************************************************************
    * runTrainConfigConfirmNewConfigFrmTCC
    ******************************************************************************/
    void TrainConfigModeBHP::runTrainConfigConfirmNewConfigFrmTCC()
    {
      TrainConfigMode::runTrainConfigConfirmNewConfigFrmTCC();

      if (modeState == trainConfigTSetupAccepted)
      {
        startTimeToConfirmAbortOfLastCarBrakeTest = 0;
        modeState = trainConfigPreDepartureBrakeTest;

      }
    }

    /******************************************************************************
    * runTrainConfigPreDepartureBrakeTest
    ******************************************************************************/
    void TrainConfigModeBHP::runTrainConfigPreDepartureBrakeTest()
    {
      const LocoTypeAdap locoType = static_cast<LocoTypeAdap>(Config::instance().getLocoType());
      const BrakeSystemType brakeSystem = DS::AbstractTSetup::corePtr()->getBrakeSystemInUse();
      const DS::TrainSetup* trainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

      bool pressureValid = false;
      bool pressureDropped = false;
      TG::TIMS::instance().checkLastCarBPDrop(pressureValid, pressureDropped);

      if ((locoType == EMD)
        && (brakeSystem == BrakeSystemType1)
        && (trainSetup != NULL)
        && trainSetup->timsSupNeeded)
      {
        if (pressureValid && (!pressureDropped))
        {
          modeState = trainConfigCheckValidBrkPrFeedBack; // this will apply SB
          waitForBrakePressureTimer = 0U;
          ATC::AbstractEventHandler::corePtr()->reportEvent(waitingFeedbackFromOBRD, __FILE__, __LINE__);
        }
        else if (lastCarBrakeTestPending)  // Inform the Driver that last Car Brake Pressure is pending.
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(lastCarBrakeTestIsPending, __FILE__, __LINE__);
          lastCarBrakeTestPending = false;
        }
        else
        {
          //Do Nothing
        }
      }
      else
      {
        modeState = trainConfigTSetupAccepted;
      }
    }

    /******************************************************************************
    * runTrainConfigCheckValidBrkPrFeedBack
    ******************************************************************************/
    void TrainConfigModeBHP::runTrainConfigCheckValidBrkPrFeedBack(CommonDataForModes& commonData)
    {
      if (checkBrakePressureLocoBelowPsb() || (waitForBrakePressureTimer > 0U))
      {
        waitForBrakePressureTimer++;

        const uint32_t brakePrTimeout = getBrakePressureDropTimeout();
        const uint32_t obrdTimeout = static_cast<uint32_t>(Config::instance().getOBRDReportTimeout());
        const uint32_t brakePrTimeoutCycleCount = ((brakePrTimeout + obrdTimeout) * ATC::secToMSec) / ATC::cycleCntToMsec;

        if (waitForBrakePressureTimer <= brakePrTimeoutCycleCount)
        {
          bool pressureValid = false;
          bool pressureDropped = false;
          TG::TIMS::instance().checkLastCarBPDrop(pressureValid, pressureDropped);

          if (pressureValid && pressureDropped)
          {
            modeState = trainConfigTSetupAccepted;
            ATC::AbstractEventHandler::corePtr()->reportEvent(brakePressureDidDrop, __FILE__, __LINE__);
            trace->write(ATC::briefTrace, "Last car brake pressure dropped within expected time");
            waitForBrakePressureTimer = 0U; //Reset the Cycle Count
            abortLastCarBrakeTestButtonPressed = false;
            lastCarBrakeTestPending = true;
          }
        }
        else
        {
          // last car brake pressure did not drop within the expected time
          // inform driver about the error
          ATC::AbstractEventHandler::corePtr()->reportEvent(brakePressureDidNotDrop, __FILE__, __LINE__);
          trace->write(ATC::briefTrace, "Last car brake pressure did not drop within expected time");
          // send abort message to tcc
          commonData.trainSetupAborted = true;
          commonData.abortSetupReason = AbortedByAos;
          modeState = trainConfigRejectedOrAborted;
          waitForBrakePressureTimer = 0U; //Reset the Cycle Count
          lastCarBrakeTestPending = true;
        }
      }
    }

    /******************************************************************************
    * getBrakePressureDropTimeout
    ******************************************************************************/
    uint32_t TrainConfigModeBHP::getBrakePressureDropTimeout() const
    {
      uint32_t trainLength = DS::TSetup::instance().getMaxConsecutiveCarLength();
      // Time in seconds
      uint32_t propagationTime = ATC::ATCMath::instance().unsignDivRoundUp(trainLength,
        Config::instance().getSbPrPropagationSpeed(), __FILE__, __LINE__);

      return propagationTime;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void TrainConfigModeBHP::getModeStateString(const TrainConfigModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case trainConfigPreDepartureBrakeTest:
        static_cast<void>(vfw_strlcpy(buffer, "trainConfigPreDepartureBrakeTest", maxModeStateNameLength));
        break;

      case trainConfigCheckValidBrkPrFeedBack:
        static_cast<void>(vfw_strlcpy(buffer, "trainConfigCheckValidBrkPrFeedBack", maxModeStateNameLength));
        break;

      default:
        TrainConfigMode::getModeStateString(state, buffer);
      }
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void TrainConfigModeBHP::initCrossCompare() const
    {

      TrainConfigMode::initCrossCompare();

      // Add cross-compares for BHP adaptation config mode
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TrainLoaded>(&trainLoadedStatusRequested));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&confirmTrainLoadedStateNeeded));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&confirmTrainLoadedWaitCycles));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&waitForBrakePressureTimer));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&abortLastCarBrakeTestButtonPressed));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&waitingFeedbackFromOBRD));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&brakePressureDidDrop));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&brakePressureDidNotDrop));
    }
  }
}
