/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file implements the abstract (core) brake component class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-19    arastogi    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-04-21    lantback    Implemented corePtr()
* 2016-04-22    lantback    Added component type
* 2016-07-18    saprasad    Implemented all core function of SB & EB Brake
* 2016-07-27    saprasad    Implemented testing function for SB/EB,Fixed review comments
*                           Fixes Lint Error.
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-19    arastogi    Updated the interface to get speed from odometry
* 2016-10-12    arastogi    Added receiving the release signal from DMI
*                           The default values for EB is EB applied.
* 2017-10-12    nsyed       Added Brake Test functionality.
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "abstract_atp_application.hpp"
#include "abstract_brake.hpp"
#include "abstract_config.hpp"
#include "abstract_loco_io.hpp"
#include "abstract_odometry.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_targets.hpp"
#include "abstract_mode_control.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_brake_event_ids.hpp"
#include "atc_math.hpp"
#include "atc_types.hpp"

#include <vfw_checkpoints.h>

#ifndef __GNUG__
#include "time.h"
extern "C" int64_t vfwGetReferenceTime(void);
extern "C" void vfwGetTimeOfDay(struct timespec * const timespec_p, struct tm * const tm_p);
#else
#include <vfw_time.h>
#endif

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
  namespace Supv
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractBrake::AbstractBrake() : ATC::ProcComponent(atpBrakeId, "Brake", "BK"),
      // creating different set of objects for different type of events
      brakeTestInProgress(ATC::Event::createStandstillEvent(atpBrakeId, ATC::CoreContainer, eventIdBrakeTestInProgress,
        ATC::NoSB, DMICom::brakeTestInProgress, "Brake Test is in Progress")),
      startingBrakeTest(ATC::Event::createStandstillEvent(atpBrakeId, ATC::CoreContainer, eventIdStartingBrakeTest,
        ATC::NoSB, 0x0U, "Starting Brake Test")),
      ebBadReadingErrAcceleration(ATC::Event::createSafetyHaltEvent(atpBrakeId, ATC::CoreContainer, eventIdReportEventEBSuprvision,
        ATC::NoEB, DMICom::brakeEbSupv, "Safety Halt:EB supervision fault. Brake test mandatory after PowerUp.")),
      ebWhenInsufficientSBDeceleration(ATC::Event::createEBReqEvent(atpBrakeId, ATC::CoreContainer, eventIdReportEventbReqSB,
        ATC::NoEB, DMICom::ebEventInSBSupv, "Emergency Break Event during Service break supervision")),
      brakeRelayFeedbackErr(ATC::Event::createSafetyHaltEvent(atpBrakeId, ATC::CoreContainer, eventIdReportEventEBRelayFb,
        ATC::NoEB, DMICom::brakeEbRelaySupv, "Safety Halt:EB relay feedback fault. Brake test mandatory after PowerUp.")),
      brakeTestExecTimeout(ATC::Event::createLogEvent(atpBrakeId, ATC::CoreContainer, eventIdBrakeTestExecTimeout,
        DMICom::brakeTestTimedOut, "Brake Test Execution Timed out")),
      brakeTestFailed(ATC::Event::createStandstillEvent(atpBrakeId, ATC::CoreContainer, eventIdBrakeTestFailed,
        ATC::NoSB, DMICom::brakeTestFailed, "Brake Test failed, perform new test.")),
      brakeTestAborted(ATC::Event::createStandstillEvent(atpBrakeId, ATC::CoreContainer, eventIdBrakeTestAborted,
        ATC::NoSB, DMICom::brakeTestAborted, "Brake Test Aborted, perform new test.")),
      brakeTestFailedEbInternal(ATC::Event::createLogEvent(atpBrakeId, ATC::CoreContainer, eventIdBrakeTestFailedEbInternal,
        DMICom::internalEbFbFailure, "Brake Test failed, due to EB Internal Health Supervision failure.")),
      cabinDeactivated(ATC::Event::createLogEvent(atpBrakeId, ATC::CoreContainer, eventIdCabinDeactivated,
        DMICom::cabinDeactivated, "Cabin De-active, Brake Test Aborted.")),
      additionalBrakeOrders(ATC::Event::createLogEvent(atpBrakeId, ATC::CoreContainer, eventIdAdditionalBrakeOrders,
        DMICom::additionalBrakeOrders, "Brake Orders other than from Brake Test, Brake Test Aborted.")),
      brakeTestAbortedByDriver(ATC::Event::createLogEvent(atpBrakeId, ATC::CoreContainer, eventIdBrakeTestAbortedByDriver,
        DMICom::brakeTestAbortedByDriver, "Brake Test Aborted by Driver.")),
      emergencyAlertActive(ATC::Event::createLogEvent(atpBrakeId, ATC::CoreContainer, eventIdEmergencyAlertActive,
        DMICom::emergencyAlertActive, "Brake Test Aborted by Emergency Stop Active Signal")),
      brakeTestSuccessful(ATC::Event::createLogEvent(atpBrakeId, ATC::CoreContainer, eventIdBrakeTestSuccessful,
        DMICom::brakeTestSuccessful, "Brake Test Successful")),
      unableToStartBrakeTest(ATC::Event::createStandstillEvent(atpBrakeId, ATC::CoreContainer, eventIdUnableToStartBrakeTest,
        ATC::NoSB, DMICom::unableToStartBrakeTest, "Unable to start Brake Test")),
      initDone(false),
      vitalDriverTestState(TestIdle),
      vitalDriverTestCycleCounter(0U),
      sbEscalatedToEB(false)
      {
      if (coreBrakeInstancePtr != 0)
      {
        // Error to event handler
        ATC::aosHalt(__FILE__, __LINE__, "Brake constructor already instantiated");
      }

      // Setup single instance pointer for core access
      coreBrakeInstancePtr = this;
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractBrake::init(void)
    {
      if (!initDone)
      {
        // Get Max Brake Test Exec. time as multiples of ATP cycles
        const uint32_t brakeTestExecTime = static_cast<uint32_t>(AbstractConfig::corePtr()->getBrakeTestExecTime()) * 1000U;
        maxBrakeTestExecCycle = ATC::ATCMath::instance().unsignDiv(brakeTestExecTime, Kernel::AbstractATPApplication::atpAppCycleTime,
          __FILE__, __LINE__);

        // Initialize with default value
        ebApplied = false;
        sbApplied = false;

        ebReleaseEnable = false;
        sbReleaseEnable = false;
        eb1TestApplied = false;
        eb2TestApplied = false;
        feedbackWaitCycle = 0U;
        currentBrakeTestCycle = 0U;
        currentSBReleaser = ATC::NoSB;
        currentEBReleaser = ATC::NoEB;
        timeSbV0AddInZoneD = 0;
        timeEbV0AddInZoneD = 0;
        cntLowRetSamplesInRowSB = 0U;
        cntLowRetSamplesInOneSB = 0U;
        cntLowRetSamplesInRowEB = 0U;
        cntLowRetSamplesInOneEB = 0U;
        bSlipSideDetected = false;
        tebStartTime = 0;
        brakeTestStarted = false;
        brakeTestStatus = BrakeTestStatusIdle;
        brakeTestSequenceStep = BrakeTestSequenceStepInit;
        
        //Prepares any values for cross-compare  
        initCrossCompare();

        initDone = true;
      }

      return initDone;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void AbstractBrake::run(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "BK_run");

      processServiceBrake();
      processEmergencyBrake();
      processTco();
      processEbCutOut();

      if (Kernel::AbstractModeControl::corePtr()->getBrakeTestRequested() ||  (brakeTestStatus == BrakeTestStatusInProgress))
      {
        // Perform Brake Test
        performBrakeTest();
      }
    }

    /******************************************************************************
    * processServiceBrake
    ******************************************************************************/
    void AbstractBrake::processServiceBrake(void)
    {
      checkAndApplySBrakeReq();
      if (sbApplied)
      {
        performSBrakeReleaseControl();
        if (0U != (Pos::AbstractOdometry::corePtr()->getSpeed()))
        {
          performSBrakeDecControl();
        }
      }
    }

    /******************************************************************************
    * processEmergencyBrake
    ******************************************************************************/
    void AbstractBrake::processEmergencyBrake(void)
    {
      checkAndApplyEBrakeReq();
      if (ebApplied)
      {
        performEBrakeReleaseControl();
        if (0U != (Pos::AbstractOdometry::corePtr()->getSpeed()))
        {
          performEBrakeDecControl();
        }
      }
    }

    /******************************************************************************
    * checkAndApplySBrakeReq
    ******************************************************************************/
    void AbstractBrake::checkAndApplySBrakeReq(void)
    {
      bool sbAvailStatus = AbstractConfig::corePtr()->getSbAvailable();
      bool isBrakeInhibit = Kernel::AbstractModeControl::corePtr()->getInhibitAllBrakes();

      if (ATC::AbstractEventHandler::corePtr()->isApplySb())
      {
        //if break is not applied then apply the break
        if ((!sbApplied) && (sbAvailStatus) && (!isBrakeInhibit))
        {
          // tSBdec(ms) :Time between service brake order and start of the supervision of an applied service brake.
          const uint32_t timeDecSBApp = (DS::AbstractTSetup::corePtr()->getServiceBrakeResponseTime()) * 100U;
          const int64_t tsbStartTime = vfwGetReferenceTime(); //Starting time when SB is applied

          sbApplied = true;
          
          //Start the SB deceleration supervision at this time.
          tsbDecMaxTime = tsbStartTime + static_cast<int64_t>(timeDecSBApp);
        }
      }
      else if ((sbAvailStatus) && (ebApplied) && (!isBrakeInhibit))
      {
        sbApplied = true;
      }
      else
      {
        //Do nothing
      }

      //Check if SB is already applied and brake inhibit is active
      if ((isBrakeInhibit) && (sbApplied))
      {
        //Set SB applied to false
        sbApplied = false;
      }
    }

    /******************************************************************************
    * performSBrakeReleaseControl
    ******************************************************************************/
    void AbstractBrake::performSBrakeReleaseControl(void)
    {
      //Check if event requested SB in last cycle
      bool isApplySb = ATC::AbstractEventHandler::corePtr()->isApplySb();

      //Check if the SB release request status is enabled or not
      bool isSBRelReqActive = isSBReleaseReqActive();
      
      if (sbReleaseEnable && isSBRelReqActive && (!ebApplied))
      {
        resetSBParams();
      }

      //Service brake release condition 
      if (sbApplied && (!ebApplied) && (!isApplySb))
      {
        sbReleaseEnable = true;
      }
      else
      {
        sbReleaseEnable = false;
      }
    }

    /******************************************************************************
    * isSBReleaseReqActive
    ******************************************************************************/
    bool AbstractBrake::isSBReleaseReqActive(void) 
    {
      bool brakeReleaseReqStatus = false;

      //Fetch the current ATO mode. Current implementation return ATOManual by default.
      ATOMode currATOMode = Kernel::AbstractModeControl::corePtr()->getATOMode();

      //Fetch required SB releaser from Event handler.
      ATC::SBReleaser reqSBReleaser = ATC::AbstractEventHandler::corePtr()->getSbReleaser();

      //Fetch the current SB releaser
      currentSBReleaser = getcurrentSBReleaser();

      //For ATO mode Manual, SB release request should be accepted only by the driver
      if (ATOModeManual == currATOMode)
      {
        reqSBReleaser = ATC::DriverSB;
      }

      //Check if the current SB releaser is more than what is required.
      if (currentSBReleaser >= reqSBReleaser)
      {
        brakeReleaseReqStatus = true;
      }

      return brakeReleaseReqStatus;
    }

    /******************************************************************************
    * resetSBParams
    ******************************************************************************/
    void AbstractBrake::resetSBParams(void)
    {
      //reset all values and counters
      sbApplied = false;
      currentSBReleaser = ATC::NoSB;
      timeSbV0AddInZoneD = 0;
      sbReleaseEnable = false;
      cntLowRetSamplesInRowSB = 0U;
      cntLowRetSamplesInOneSB = 0U;
    }

    /******************************************************************************
    * performSBrakeDecControl
    ******************************************************************************/
    void AbstractBrake::performSBrakeDecControl(void)
    {
      int64_t currentTimeStamp = vfwGetReferenceTime();
      bool requestEmergencyBreak = false;
      int64_t timeStartOfZoneDForSB = 0;

      // If NOT true: We still in Zone A
      if (currentTimeStamp > tsbDecMaxTime)
      {
        //Zone B start
        const int16_t actualDeacceleration = static_cast<int16_t>(Pos::AbstractOdometry::corePtr()->getAcceleration() * percentageMultiplier);

        const uint32_t currentSpeed = static_cast<uint32_t>(Pos::AbstractOdometry::corePtr()->getSpeed());

        //rSb(cm/s2) :Train service brake deceleration as per current breakeability.
        const uint16_t rSb = static_cast<uint16_t>(DS::AbstractTSetup::corePtr()->getBrakeability(currentSpeed));
        // vSBdec(cm/s) :Upper speed boundary value used during supervision of an applied service brake.
        const uint32_t vSbDec = AbstractConfig::corePtr()->getUpperSpeedSbDec();
        //vSBv0(cm / s) :Lower speed boundary value used during supervision of an applied service brake.
        const uint32_t vSbv0 = AbstractConfig::corePtr()->getLowerSpeedSbDec();
        
        if (vSbDec <= currentSpeed)
        {
          // Zone B actualDeacceleration should atleast be greater than configurable deceleration limit percentage of expected
          // deceleration(gradient adjusted).

          const int16_t addGradToConstAccVal = addGradToConstAcc(rSb);
          if (actualDeacceleration < ((static_cast<int16_t>(AbstractConfig::corePtr()->getSbExpectedDecLimit()) * addGradToConstAccVal)))
          {
            // Number of bad reading
            cntLowRetSamplesInRowSB++;
            cntLowRetSamplesInOneSB++;
          }
          else
          {
            cntLowRetSamplesInRowSB = 0U;
          }
        }
        else if (vSbv0 <= currentSpeed)//Zone C
        {
          //Actual deceleration should atleast be greater than configurable deceleration limit percentage of expected deceleration.
          const int16_t expDecValue = getExpectedDeceleration(rSb, vSbDec);
          if (actualDeacceleration < (static_cast<int16_t>(AbstractConfig::corePtr()->getSbExpectedDecLimit()) * expDecValue))
          {
            // Number of bad reading
            cntLowRetSamplesInRowSB++;
            cntLowRetSamplesInOneSB++;
          }
          else
          {
            cntLowRetSamplesInRowSB = 0U;
          }
        }
        else//Zone D
        {
          if (0 == timeSbV0AddInZoneD)
          {
            // tSbv0(ms) :Time measured from detection of va < vSbv0. The supervision of an applied service
            // brake requires zero speed within this time.
            const int64_t tSbv0 = static_cast<int64_t>(AbstractConfig::corePtr()->getTimeSbv0());

            timeStartOfZoneDForSB = currentTimeStamp;
            timeSbV0AddInZoneD = timeStartOfZoneDForSB + tSbv0;
          }
          else
          {
            //if within tSbV0 velocity is not 0 then apply emergency break
            if ((timeSbV0AddInZoneD <= currentTimeStamp) && (trainStopVelocity != currentSpeed))
            {
              requestEmergencyBreak = true;
              trace.write(ATC::briefTrace, "Time in zone d deceleration supervision exceeded");
            }
          }
        }

        // Maximum Number of bad readings in a row allowed when present acceleration is lesser than expected SB deceleration.
        const uint16_t maxLowDecInRow = AbstractConfig::corePtr()->getMaxLowDecInRowSB();
        // Maximum Number of bad readings accumulative allowed when present acceleration is lesser than expected SB deceleration.
        const uint16_t maxLowDecAccum = AbstractConfig::corePtr()->getMaxLowDecAccumSB();

        //If number of bad reading crossed threshold limit
        if ((maxLowDecAccum < cntLowRetSamplesInOneSB)
          || (maxLowDecInRow < cntLowRetSamplesInRowSB))
        {
          requestEmergencyBreak = true;
          trace.write(ATC::briefTrace, "incorrect samples for deceleration after SB exceeded!");
        }
      }

      if (requestEmergencyBreak)
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(ebWhenInsufficientSBDeceleration, __FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * addGradToConstAcc
    ******************************************************************************/
    int16_t AbstractBrake::addGradToConstAcc(const uint16_t accValue) const
    {
      int32_t targetGradient = DS::AbstractTargets::corePtr()->getCurGradient();
      int16_t addGradToAcc = static_cast<int16_t>(accValue);

      if (targetGradient < 0)
      {
        // accValue is the deceleration value while, curGradient is +ve for uphill.
        // In this case of Downhill(-ve gradient) this value should be added to reduce actual breakability.
        addGradToAcc += static_cast<int16_t>(targetGradient);
      }
      else
      {
        // No action
      }
      return addGradToAcc;
    }

    /******************************************************************************
    * Calculate getExpectedDeacceleration
    ******************************************************************************/
    int16_t AbstractBrake::getExpectedDeceleration(const uint16_t accValue, const uint32_t upperSpeedDec) const
    {
      int16_t curGradient = static_cast<int16_t>(DS::AbstractTargets::corePtr()->getCurGradient());
      uint16_t currentSpeed = Pos::AbstractOdometry::corePtr()->getSpeed();
      int32_t tempValue = static_cast<int32_t>(accValue)*static_cast<int32_t> (currentSpeed);
      int32_t vSbDec = static_cast<int32_t>(upperSpeedDec);
      int16_t calculatedDeacceleration = static_cast<int16_t>((tempValue) / vSbDec);

      if (curGradient < 0)
      {
        calculatedDeacceleration += curGradient;
      }
      return calculatedDeacceleration;
    }

    /******************************************************************************
    * checkAndApplyEBrakeReq
    ******************************************************************************/
    void AbstractBrake::checkAndApplyEBrakeReq(void)
    {  
      // Check if brake inhibit status is set or not
      bool isBrakeInhibit = Kernel::AbstractModeControl::corePtr()->getInhibitAllBrakes();

      // Check if SB is configured not to be used
      bool sbAvailStatus = AbstractConfig::corePtr()->getSbAvailable();

      // Check if any event requesting service brake application is active
      bool isApplySb = ATC::AbstractEventHandler::corePtr()->isApplySb();

      // Check if any event requesting emergency brake application is active
      bool isApplyEb = ATC::AbstractEventHandler::corePtr()->isApplyEb();

      // escalate SB to EB if valid train setup not present and if any event has requested service brake
      sbEscalatedToEB = (!DS::AbstractTSetup::corePtr()->isTrainSetupValid()) && isApplySb;

      if (((isApplyEb) || (isApplySb && (!sbAvailStatus)) || sbEscalatedToEB ) && (!isBrakeInhibit))
      {
        //change the authority if its higher
        if (ATC::AbstractEventHandler::corePtr()->getEbReleaser() > currentEBReleaser)
        {
          currentEBReleaser = ATC::AbstractEventHandler::corePtr()->getEbReleaser();
        }

        //if break is not already applied, then apply the break 
        if (!ebApplied)
        {
          // tEBdec(ms) :Time between an emergency brake order and start of the supervision of an applied emergency brake.          
          const uint32_t timeDecEBApp = (DS::AbstractTSetup::corePtr()->getEmergencyBrakeResponseTime()) * 100U;

          ebApplied = true;

          //if EB is applied and SB is not applied
          if ((!sbApplied) && (sbAvailStatus))
          {
            //Apply SB and write to log. This might be an error.
            sbApplied = true;
            writeToLog(ATC::BriefLog, "EB Applied before SB Application !!!", __FILE__, __LINE__);
          }
          
          //If SB need to escalate to EB as no valid train setup exist
          if (sbEscalatedToEB)
          {
            writeToLog(ATC::BriefLog, "EB applied instead of SB as no valid train setup exist!!", __FILE__, __LINE__);
          }

          if ((!sbAvailStatus) && (isApplySb))
          {
            writeToLog(ATC::BriefLog, "EB applied instead of SB since SB is not available", __FILE__, __LINE__);
          }

          tebStartTime = vfwGetReferenceTime();
          tebDecMaxTime = tebStartTime + static_cast<int64_t>(timeDecEBApp);
          tebEBRDecMaxTime = tebStartTime + static_cast<int64_t>(AbstractConfig::corePtr()->getInternalRelaysTimeout());
        }
        else
        {
          int64_t currentTimeStamp = vfwGetReferenceTime();
          bool resultEB1;
          bool resultEB2;
          bool resFunEB1;
          bool resFunEB2;
          resFunEB1 = IO::AbstractLocoIO::corePtr()->getCoreDigitalOutputValue(IO::AbstractLocoIO::EmerBrake1, &resultEB1);
          resFunEB2 = IO::AbstractLocoIO::corePtr()->getCoreDigitalOutputValue(IO::AbstractLocoIO::EmerBrake2, &resultEB2);
          
          if (resFunEB1 && resFunEB2)
          {
            // EB1 and EB2 are active low, and should both be set to 0. Issue an error if any of these is '1'.
            if (((resultEB1 || resultEB2)) && (currentTimeStamp > tebEBRDecMaxTime))
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(brakeRelayFeedbackErr, __FILE__, __LINE__);
              // Brake Test becomes mandatory after recovering from Safety Halt
              if (!AbstractConfig::corePtr()->setBrakeTestReason(brakeTestReasonBrakeRelayFault))
              {
                trace.write(ATC::briefTrace, "Failed to store the Brake Test Reason in NVS!");
                writeToLog(ATC::BriefLog, "Failed to store the Brake Test Reason in NVS!", __FILE__, __LINE__);
              }
            }
          }
          else
          {
            trace.write(ATC::briefTrace, "Could not read EmergencyBrake I/O:s.");
          }
          int64_t timeSinceEbApplied = currentTimeStamp - tebStartTime;
          checkEbFeedback(timeSinceEbApplied);
        }
      }
      else
      {
        //do nothing
      }

      //Check if EB is already applied and brake inhibit is active
      if ((isBrakeInhibit) && (ebApplied))
      {
        //Set EB applied to false
        ebApplied = false;
      }
    }

    /******************************************************************************
    * performEBrakeReleaseControl
    ******************************************************************************/
    void AbstractBrake::performEBrakeReleaseControl(void)
    { 
      //Check if EB is applied
      const bool isApplyEb = ATC::AbstractEventHandler::corePtr()->isApplyEb();

      //Check if the train is at standstill
      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

      //Check if EB release request status is enabled or not
      const bool isEBRelReqActive = isEBReleaseReqActive();

      // Check if brake inhibit status is set or not
      const bool isBrakeInhibit = Kernel::AbstractModeControl::corePtr()->getInhibitAllBrakes();

      // Check if SB is configured not to be used
      const bool sbAvailStatus = AbstractConfig::corePtr()->getSbAvailable();

      // Check if any event requesting service brake application is active
      const bool isApplySb = ATC::AbstractEventHandler::corePtr()->isApplySb();

      if (ebReleaseEnable && isEBRelReqActive)
      {
        cntLowRetSamplesInOneEB = 0U;//cntLowRetSamplesInOneEB reset to zero 
        ebApplied = false;
        resetEBParams();
      }

      //Emergency Brake release condition
      if (sbEscalatedToEB )
      {
        ebReleaseEnable = sbReleaseEnable;
      }
      else
      {
        const bool isEBTriggerNeeded = (isApplySb && (!sbAvailStatus));
        if (((ebApplied) && (!isApplyEb) && (!isEBTriggerNeeded) && (isStandStill)) || (isBrakeInhibit))
        {
          ebReleaseEnable = true;
        }
        else
        {
          ebReleaseEnable = false;
        }
      }
    }

    /******************************************************************************
    * isEBReleaseReqActive
    ******************************************************************************/
    bool AbstractBrake::isEBReleaseReqActive() 
    {
      bool checkBrakeRelReqstatus = false;

      // Fetch required EB releaser from Event handler. This should be case Ideally. Till the ATO is implemented,
      // we will have EB release as Driver.
      //ATC::EBReleaser reqEBReleaser = ATC::AbstractEventHandler::corePtr()->getEbReleaser();

      //Set the Eb releaser to Driver as EB can only be released by Driver.
      ATC::EBReleaser reqEBReleaser = ATC::DriverEB;

      //Fetch value for current EB releaser
      currentEBReleaser = getCurrentEBReleaser();  
      
      //Check if the current EB releaser is set to required releaser
      if (currentEBReleaser >= reqEBReleaser)
      {
        checkBrakeRelReqstatus = true;
      }     
      return checkBrakeRelReqstatus;
    }

    /******************************************************************************
    * resetEBParams
    ******************************************************************************/
    void AbstractBrake::resetEBParams(void)
    {
      //reset all values
      currentEBReleaser = ATC::NoEB;
      timeEbV0AddInZoneD = 0;
      cntLowRetSamplesInOneSB = 0U;
      ebReleaseEnable = false;
      tebDecMaxTime = 0;
      tebEBRDecMaxTime = 0;
      cntLowRetSamplesInRowEB = 0U;
      cntLowRetSamplesInOneEB = 0U;
    }

    /******************************************************************************
    * performEBrakeReleaseControl
    ******************************************************************************/
    void AbstractBrake::performEBrakeDecControl(void)
    {
      int64_t currentTimeStamp = vfwGetReferenceTime();
      uint16_t currentSpeed = Pos::AbstractOdometry::corePtr()->getSpeed();
      bool bGenerateFatalEvent = false;
      int64_t stopMinSlipSlideTime = 0;
      int64_t timeStartOfZoneDForEB = 0;
      bool bSlipe = Pos::AbstractOdometry::corePtr()->isSlipping();
      bool bSlide = Pos::AbstractOdometry::corePtr()->isSliding();

      //check if train slip-slide start
      if ((bSlipe || bSlide) && (!(bSlipSideDetected)))
      {
        bSlipSideDetected = true;
        tInhibitTime = currentTimeStamp + static_cast<int64_t>(tMaxSlipSide);
      }

      //check if train slip-slide stopped
      if (((!bSlipe) && (!bSlide)) && bSlipSideDetected)
      {
        stopMinSlipSlideTime = currentTimeStamp + static_cast<int64_t>(tMinSlipSide);
        bSlipSideDetected = false;
        tInhibitTime = (tInhibitTime < stopMinSlipSlideTime) ? (tInhibitTime) : (stopMinSlipSlideTime);
      }

      if (currentTimeStamp > tInhibitTime)
      {
        if (currentTimeStamp > tebDecMaxTime)
        {
          //Zone B start
          const int16_t actualDeacceleration = static_cast<int16_t>(Pos::AbstractOdometry::corePtr()->getAcceleration() * percentageMultiplier);

          const uint32_t speed = static_cast<uint32_t>(Pos::AbstractOdometry::corePtr()->getSpeed());

          //Reb(cm/s2) :Train Emergency brake deceleration from manufacturer data.
          const uint16_t rEb = static_cast<uint16_t>(DS::AbstractTSetup::corePtr()->getBrakeability(speed));

          //Vebdec(cm/s) :Upper speed boundary value used during supervision of an applied emergency brake.
          const uint16_t vEbDec = AbstractConfig::corePtr()->getUpperSpeedEbDec();
          //Vebv0(cm/s) :Lower speed boundary value used during supervision of an applied emergency brake.
          const uint16_t vEbv0 = AbstractConfig::corePtr()->getLowerSpeedEbDec();

          if (vEbDec <= currentSpeed)
          {
            //Zone B
            const int16_t addGradToConstAccVal = (addGradToConstAcc(rEb));
            if (actualDeacceleration < (static_cast<int16_t>(AbstractConfig::corePtr()->getEbExpectedDecLimit()) * addGradToConstAccVal))
            {
              cntLowRetSamplesInRowEB++;
              cntLowRetSamplesInOneEB++;
            }
            else
            {
              cntLowRetSamplesInRowEB = 0U;
            }
          }
          else if (vEbv0 <= currentSpeed)//Zone C
          {
            const int16_t expDecValue = getExpectedDeceleration(rEb, vEbDec);
            if (actualDeacceleration < (static_cast<int16_t>(AbstractConfig::corePtr()->getEbExpectedDecLimit()) * expDecValue))
            {
              cntLowRetSamplesInRowEB++;
              cntLowRetSamplesInOneEB++;
            }
            else
            {
              cntLowRetSamplesInRowEB = 0U;
            }
          }
          else//Zone D
          {
            if (0 == timeEbV0AddInZoneD)
            {
              //tEbv0(ms) :Time measured from detection of va < vEBv0. The supervision of an applied emergency brake
              // requires zero speed within this time.
              const uint16_t tEbv0 = AbstractConfig::corePtr()->getTimeEbv0();

              timeStartOfZoneDForEB = currentTimeStamp;
              timeEbV0AddInZoneD = timeStartOfZoneDForEB + static_cast<int64_t>(tEbv0);
            }
            else
            {
              //if within Tebv0 velocity is not 0 then make it bGenerateFatalEvent true
              if ((timeEbV0AddInZoneD <= currentTimeStamp) && (trainStopVelocity != currentSpeed))
              {
                bGenerateFatalEvent = true;
              }
            }
          }

          // Maximum Number of bad readings in a row allowed when present acceleration is lesser than expected SB deceleration.
          const uint16_t maxLowDecInRow = AbstractConfig::corePtr()->getMaxLowDecInRowEB();
          // Maximum Number of bad readings accumulative allowed when present acceleration is lesser than expected SB deceleration.
          const uint16_t maxLowDecAccum = AbstractConfig::corePtr()->getMaxLowDecAccumEB();

          if ((maxLowDecInRow < cntLowRetSamplesInRowEB)
            || (maxLowDecAccum < cntLowRetSamplesInOneEB))
          {
            bGenerateFatalEvent = true;
          }
        }

      }

      if (bGenerateFatalEvent)
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(ebBadReadingErrAcceleration, __FILE__, __LINE__);
        // Brake Test becomes mandatory after recovering from Safety Halt
        if (!AbstractConfig::corePtr()->setBrakeTestReason(brakeTestReasonEbOrderFault))
        {
          trace.write(ATC::briefTrace, "Failed to fulfil EB order!");
          writeToLog(ATC::BriefLog, "Failed to fulfil EB order!", __FILE__, __LINE__);
        }
      }
    }

    /******************************************************************************
    * verifyInternalEbRelays
    ******************************************************************************/
    bool AbstractBrake::verifyInternalEbRelays() const
    {
      bool internalEB1RelayOk = true;
      bool internalEB2RelayOk = true;

      bool readEb1Value;
      bool readEb2Value;
      bool value1Found;
      bool value2Found;

      value1Found = IO::AbstractLocoIO::corePtr()->getCoreDigitalOutputValue(IO::AbstractLocoIO::EmerBrake1, &readEb1Value);
      value2Found = IO::AbstractLocoIO::corePtr()->getCoreDigitalOutputValue(IO::AbstractLocoIO::EmerBrake2, &readEb2Value);

      //EB values are active Low
      readEb1Value = !readEb1Value;
      readEb2Value = !readEb2Value;

      if (value1Found && value2Found)
      {
        if (((!readEb1Value) && eb1TestApplied) || (readEb1Value && (!eb1TestApplied)))
        {
          internalEB1RelayOk = false;
        }
        if (((!readEb2Value) && eb2TestApplied) || (readEb2Value && (!eb2TestApplied)))
        {
          internalEB2RelayOk = false;
        }
      }
      else
      {
        //Unable to read the EB Relays!!
        internalEB1RelayOk = false;
        internalEB2RelayOk = false;
        trace.write(ATC::briefTrace, "Could not read internal EB relays!!");
      }
      return (internalEB1RelayOk && internalEB2RelayOk);
    }

    /******************************************************************************
    * getMaxInternalRelaysFbWaitCycle
    ******************************************************************************/
    uint32_t AbstractBrake::getMaxInternalRelaysFbWaitCycle() const
    {
      uint32_t maxInternalRelaysWaitCycle = ATC::ATCMath::instance().unsignDiv(static_cast<uint32_t>(
        AbstractConfig::corePtr()->getInternalRelaysTimeout()), Kernel::AbstractATPApplication::atpAppCycleTime, __FILE__, __LINE__);
      
      return maxInternalRelaysWaitCycle;
    }

    /******************************************************************************
    * verifyBrakeTestFeedback
    ******************************************************************************/
    bool AbstractBrake::verifyBrakeTestFeedback()
    {
      bool internalEBRelayOk = verifyInternalEbRelays();

      // All feedback ok -> Reset 'timer'
      if (internalEBRelayOk)
      {
        feedbackWaitCycle = 0U;
      }
      // Check if Max time has exceeded to get the expected EB feedback -> Brake Test Failed!
      else if (feedbackWaitCycle > getMaxInternalRelaysFbWaitCycle())
      {
        if (brakeTestSequenceStep == BrakeTestSequenceStepInit)
        {
          // Unable to start Brake Test!
          ATC::AbstractEventHandler::corePtr()->reportEvent(unableToStartBrakeTest, __FILE__, __LINE__);
        }
        else
        {
          // Brake Test failed due to internal EB brake relay 
          ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestFailedEbInternal, __FILE__, __LINE__);
        }
       
        trace.write(ATC::briefTrace, "feedbackWaitCycle=", feedbackWaitCycle);
        trace.write(ATC::briefTrace, "maxEbWaitCycle=", getMaxInternalRelaysFbWaitCycle());

        // Issue standstill
        ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestFailed, __FILE__, __LINE__);
        
        brakeTestStatus = BrakeTestStatusFail;
        resetBrakeTestStateMachine();
      }
      else
      {
        // Wait some more to get the expected Feedback
        feedbackWaitCycle++;
      }
      return (internalEBRelayOk);
    }

    /******************************************************************************
    * abortBrakeTest
    ******************************************************************************/
    bool AbstractBrake::abortBrakeTest(void)
    {
      bool brakeTestAbort = true;
      bool isEbApplied = getEbApplied();
      bool isSbApplied = getSbApplied();

      bool abortByDriver = (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonAbortBrakeTest) ? true : false;
      bool additionalBrakeEvents = (isEbApplied || isSbApplied);
      CabActiveStatus cabStatus = ATP::Kernel::AbstractModeControl::corePtr()->getActiveCab();

      // Read the LocoIo signal for Emergency Stop Active
      bool emergencyStopActive = IO::AbstractLocoIO::corePtr()->getEmergencyStopActiveAlert();

      // Create DMI events if Brake Test is about to be aborted
      if (abortByDriver)
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestAbortedByDriver, __FILE__, __LINE__);
      }
      else if (emergencyStopActive)
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(emergencyAlertActive, __FILE__, __LINE__);
      }
      else if (additionalBrakeEvents)
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(additionalBrakeOrders, __FILE__, __LINE__);
      }
      else if ((CabAActive != cabStatus) && (CabBActive != cabStatus))
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(cabinDeactivated, __FILE__, __LINE__);
      }
      else
      {
        brakeTestAbort = false;
      }

      return brakeTestAbort;
    }

    /******************************************************************************
    * resetBrakeTestStateMachine
    ******************************************************************************/
    void AbstractBrake::resetBrakeTestStateMachine()
    {
      currentBrakeTestCycle = 0U; 
      feedbackWaitCycle = 0U;
      brakeTestSequenceStep = BrakeTestSequenceStepInit;
      brakeTestStarted = false;
      vitalDriverTestState = TestIdle;
      vitalDriverTestCycleCounter = 0U;

      // If deactivated, activate it again...
      IO::AbstractLocoIO::corePtr()->setVitalDriverIsActiveOrder(true);

      // Release EB orders triggered by Brake Test Sequence
      eb1TestApplied = false;
      eb2TestApplied = false;
    }


    /******************************************************************************
    * processBrakeStateMachine
    ******************************************************************************/
    bool AbstractBrake::processBrakeTestStateMachine()
    {
      // Brake-test orders EB1, EB2
      const BrakeTestOrderStatus brakeTestOrderStatusArray[numberOfBrakeTestSteps] =
      { { false, false },    // Step 1
        { true,  false },    // Step 2
        { false, false },    // Step 3
        { false, true  },    // Step 4
        { false, false } };  // Step 5

      // Wait to start
      if (!brakeTestStarted)
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(startingBrakeTest, __FILE__, __LINE__);

        // Wait for Standstill before starting the Brake Test Sequence
        if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
        {
          trace.write(ATC::briefTrace, "BrakeTestSequence Started");
          brakeTestStarted = true;
        }
      }
      // Test Steps
      else
      {
        trace.write(ATC::briefTrace, "BrakeTestSequenceStep: ", static_cast<uint32_t>(brakeTestSequenceStep));
        ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestInProgress, __FILE__, __LINE__);

        // Feedback verification ok?
        if (verifyBrakeTestFeedback())
        {
          ++brakeTestSequenceStep;
        }
      }

      if (brakeTestSequenceStep < numberOfBrakeTestSteps)
      {
        // Apply EB Orders for actual Brake Test Step
        eb1TestApplied = brakeTestOrderStatusArray[brakeTestSequenceStep].eb1TestApplied;
        eb2TestApplied = brakeTestOrderStatusArray[brakeTestSequenceStep].eb2TestApplied;
      }

      // Reached last test-step?
      return (brakeTestSequenceStep >= numberOfBrakeTestSteps);
    }

    /******************************************************************************
    * performBrakeTest
    ******************************************************************************/
    void AbstractBrake::performBrakeTest()
    {
      // Timeout for complete brake test?
      if (currentBrakeTestCycle > maxBrakeTestExecCycle)
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestExecTimeout, __FILE__, __LINE__);
        ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestFailed, __FILE__, __LINE__);
        brakeTestStatus = BrakeTestStatusFail;
        resetBrakeTestStateMachine();
      }
      else
      {
        // Any condition to abort brake test full-filled?
        if (!abortBrakeTest())
        {
          currentBrakeTestCycle++;
          brakeTestStatus = BrakeTestStatusInProgress;

          if (vitalDriverTestState == TestIdle)
          {
            // Run brake test...
            if (processBrakeTestStateMachine())
            {
              // Brake test part done
              performVitalDriverTest();
            }
          }
          else
          {
            // Test Vital driver...
            performVitalDriverTest();
            if (vitalDriverTestState == VitalDriverTestSucceeded)
            {
              trace.write(ATC::briefTrace, "Stop test vital driver %d", static_cast<int32_t>(vfwGetReferenceTime()));
              trace.write(ATC::briefTrace, "BrakeTestSequence Finished", static_cast<int32_t>(vitalDriverTestCycleCounter));

              struct timespec currentTimeSpec;
              vfwGetTimeOfDay(&currentTimeSpec, static_cast<tm *>(NULL));

              uint32_t curTime = static_cast<uint32_t>(currentTimeSpec.tv_sec);

              if (!AbstractConfig::corePtr()->setLastBrakeTestTime(curTime))
              {
                trace.write(ATC::briefTrace, "Failed to store Last performed Brake Test Time stamp!");
                writeToLog(ATC::BriefLog, "Failed to store Last performed Brake Test Time stamp!", __FILE__, __LINE__);
              }
              ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestSuccessful, __FILE__, __LINE__);

              brakeTestStatus = BrakeTestStatusSuccessful;
              if (!AbstractConfig::corePtr()->setBrakeTestReason(braketestReasonNone))
              {
                trace.write(ATC::briefTrace, "Failed to store the Brake Test Reason in NVS!");
                writeToLog(ATC::BriefLog, "Failed to store the Brake Test Reason in NVS!", __FILE__, __LINE__);
              }

              // Reset brake-test state-machine
              resetBrakeTestStateMachine();
            }
            else if (vitalDriverTestCycleCounter > 5U)
            {
              // Error
              ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestFailed, __FILE__, __LINE__);
              brakeTestStatus = BrakeTestStatusFail;
              resetBrakeTestStateMachine();
            }
            else
            {
              // Wait...
            }
          }
        }
        // Brake test aborted
        else
        {
          brakeTestStatus = BrakeTestStatusAborted;
          ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestAborted, __FILE__, __LINE__);
          resetBrakeTestStateMachine();
        }
      }

    }

    /******************************************************************************
    * performVitalDriverTest
    ******************************************************************************/
    void AbstractBrake::performVitalDriverTest()
    {
      switch (vitalDriverTestState)
      {
      case TestIdle:
        vitalDriverTestState = WaitForVitalOutputDeactivation;
        // Start by Deactivate the Vital IO driver
        IO::AbstractLocoIO::corePtr()->setVitalDriverIsActiveOrder(false);
        trace.write(ATC::briefTrace, "Start test vital driver %d\n", static_cast<int32_t>(vfwGetReferenceTime()));
        break;

      case WaitForVitalOutputDeactivation:
        waitForVitalOutputDeactivation();
        break;

      case VitalDriverTestSucceeded:
        break;

      default:
        break;
      }

      ++vitalDriverTestCycleCounter;
    }


    /******************************************************************************
    * getEbApplied
    ******************************************************************************/
    bool AbstractBrake::getEbApplied(void) const
    {
      return ebApplied;
    }

    /******************************************************************************
    * getSbApplied
    ******************************************************************************/
    bool AbstractBrake::getSbApplied(void) const
    {
      return sbApplied;
    }

    /******************************************************************************
    * getEbReleaseEnable
    ******************************************************************************/
    bool AbstractBrake::getEbReleaseEnable(void) const
    {
      return ebReleaseEnable;
    }

    /******************************************************************************
    * getSbReleaseEnable
    ******************************************************************************/
    bool AbstractBrake::getSbReleaseEnable(void) const
    {
      return sbReleaseEnable;
    }

    /******************************************************************************
    * getEb1TestApplied
    ******************************************************************************/
    bool AbstractBrake::getEb1TestApplied(void) const
    {
      return eb1TestApplied;
    }

    /******************************************************************************
    * getEb2TestApplied
    ******************************************************************************/
    bool AbstractBrake::getEb2TestApplied(void) const
    {
      return eb2TestApplied;
    }

    /******************************************************************************
    * getBrakeTestStatus
    ******************************************************************************/
    BrakeTestStatus AbstractBrake::getBrakeTestStatus(void) const
    {
      return brakeTestStatus;
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractBrake* AbstractBrake::corePtr(void)
    {
      return coreBrakeInstancePtr;
    }

    /******************************************************************************
    * getBrakeRelReqDispatcherSB
    ******************************************************************************/
    bool AbstractBrake::getBrakeRelReqDispatcherSB(void) const
    {
      return false;
    }

    /******************************************************************************
    * getBrakeRelReqATOSB
    ******************************************************************************/
    bool AbstractBrake::getBrakeRelReqATOSB(void) const
    {
      return false;
    }

    /******************************************************************************
    * getBrakeRelReqDriver
    ******************************************************************************/
    bool AbstractBrake::getBrakeRelReqDriverSB(void) const
    {
      return (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus()
        == DMICom::DMIButtonBrakeRelease);
    }

    /******************************************************************************
    * getBrakeRelReqDispatcherEB
    ******************************************************************************/
    bool AbstractBrake::getBrakeRelReqDispatcherEB(void) const
    {
      return false;
    }

    /******************************************************************************
    * getBrakeRelReqDriverEB
    ******************************************************************************/
    bool AbstractBrake::getBrakeRelReqDriverEB(void) const
    {
      return (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus()
        == DMICom::DMIButtonBrakeRelease);
    }

    /******************************************************************************
    * getcurrentSBReleaser
    ******************************************************************************/
    ATC::SBReleaser AbstractBrake::getcurrentSBReleaser(void) const
    {
      //Check if the SB releaser is set to ATO
      bool brakeReleaseReqATOSB = getBrakeRelReqATOSB();

      //Check if the SB releaser is set to Dispatcher
      bool brakeReleaseReqDispatcherSB = getBrakeRelReqDispatcherSB();

      //Check if the SB releaser is set to Driver
      bool brakeReleaseReqDriverSB = getBrakeRelReqDriverSB();

      ATC::SBReleaser currSBReleaser = ATC::NoSB;

      const Kernel::DriverLoginState atpDriverLoginState = Kernel::AbstractModeControl::corePtr()->getDriverLoginSeqState();

      //Release SB when driver is logged in with correct credentials.
      if (brakeReleaseReqDriverSB && (atpDriverLoginState == Kernel::DriverLoginSeq::DriverLoginSeq::driverLoggedIn))
      {
        currSBReleaser = ATC::DriverSB;
      }
      else if (brakeReleaseReqDispatcherSB)
      {
        currSBReleaser = ATC::DispatcherSB;
      }
      else if (brakeReleaseReqATOSB)
      {
        currSBReleaser = ATC::ATOSB;
      }
      else
      {
        // Do nothing 
      }

      return currSBReleaser;
    }

    /******************************************************************************
    * getcurrentEBReleaser
    ******************************************************************************/
    ATC::EBReleaser AbstractBrake::getCurrentEBReleaser(void) const
    {
      //Check if the EB releaser is set to Dispatcher
      bool brakeReleaseReqDispatcherEB = getBrakeRelReqDispatcherEB();

      //Check if the EB releaser is set to Driver
      bool brakeReleaseReqDriverEB = getBrakeRelReqDriverEB();
 
      ATC::EBReleaser currEBReleaser = ATC::NoEB;
      const Kernel::DriverLoginState atpDriverLoginState = Kernel::AbstractModeControl::corePtr()->getDriverLoginSeqState();
      //Release EB when driver is logged in with correct credentials.
      if (brakeReleaseReqDriverEB && (atpDriverLoginState == Kernel::DriverLoginSeq::DriverLoginSeq::driverLoggedIn))
      {
        currEBReleaser = ATC::DriverEB;
      }
      else if (brakeReleaseReqDispatcherEB)
      {
        currEBReleaser = ATC::DispatcherEB;
      }
      else
      {
        // Do nothing 
      }
      return currEBReleaser;
    }

    /******************************************************************************
    * waitForVitalOutputDeactivation
    ******************************************************************************/
    void AbstractBrake::waitForVitalOutputDeactivation()
    {
      const bool allVitalOutputDeactivated = IO::AbstractLocoIO::corePtr()->getAllVitalOutputDeactivated();

      if (allVitalOutputDeactivated)
      {
        vitalDriverTestState = VitalDriverTestSucceeded;
      }
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractBrake::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&eb1TestApplied));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&eb2TestApplied));

      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&ebApplied));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&sbApplied));

      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&ebReleaseEnable));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&sbReleaseEnable));

      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ATC::SBReleaser>(&currentSBReleaser));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ATC::EBReleaser>(&currentEBReleaser));

      crossCompare->addCrossCompareData(new Support::CrossCompareInt64(&tsbDecMaxTime));
      crossCompare->addCrossCompareData(new Support::CrossCompareInt64(&timeSbV0AddInZoneD));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&bSlipSideDetected));

      crossCompare->addCrossCompareData(new Support::CrossCompareInt64(&tebEBRDecMaxTime));
      crossCompare->addCrossCompareData(new Support::CrossCompareInt64(&tebDecMaxTime));
      crossCompare->addCrossCompareData(new Support::CrossCompareInt64(&tebStartTime));
      crossCompare->addCrossCompareData(new Support::CrossCompareInt64(&timeEbV0AddInZoneD));
 
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&initDone));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&cntLowRetSamplesInRowSB));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&cntLowRetSamplesInOneSB));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&cntLowRetSamplesInRowEB));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&cntLowRetSamplesInOneEB));

      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&ebBadReadingErrAcceleration));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&ebWhenInsufficientSBDeceleration));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&brakeRelayFeedbackErr));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&brakeTestInProgress));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&startingBrakeTest));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&brakeTestExecTimeout));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&brakeTestFailed));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&brakeTestAborted));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&brakeTestFailedEbInternal));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&cabinDeactivated));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&additionalBrakeOrders));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&brakeTestAbortedByDriver));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&emergencyAlertActive));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&brakeTestSuccessful));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&unableToStartBrakeTest));

      crossCompare->addCrossCompareData(new Support::CrossCompareInt64(&tInhibitTime));

      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<BrakeTestStatus>(&brakeTestStatus));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&brakeTestSequenceStep));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&feedbackWaitCycle));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&currentBrakeTestCycle));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&maxBrakeTestExecCycle));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&brakeTestStarted));
    }
    
    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool AbstractBrake::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments searches for the "help", "trace" or any other Console
      component specific command calls and handles it. Returns true if completely handled
      else returns false. returning false will let other components handle the call. help always returns false.
      */

      bool retVal = false;
      int32_t ret;
      char_t  buffer[512];

      // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        const char_t* const toWrite = "brake          To print out the enabled brakes";

        ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);
        retVal = false;
      }
      else if (ATC::isTextMatch(&argv[0][0], "brake", sizeof("brake")) && (argc == 1U))
      {
        // Print all status values
        //lint -e{586} snprintf is needed here
        ret = snprintf(&buffer[0], sizeof(buffer),
          "%-35s%-10d\n"
          "%-35s%-10d\n"
          "%-35s%-10d\n"
          "%-35s%-10d\n"
          "%-35s%-10d\n"
          "%-35s%-10d\n"
          "%-35s%-10d\n"
          "%-35s%-10d\n",
          "All brakes inhibited:", Kernel::AbstractModeControl::corePtr()->getInhibitAllBrakes(),
          "EB Applied:", ebApplied,
          "EB Release Enable:", ebReleaseEnable,
          "SB Applied:", sbApplied,
          "SB Release Enable:", sbReleaseEnable,
          "Brake Test Step:", brakeTestSequenceStep,
          "EB1 Test Applied:", eb1TestApplied,
          "EB2 Test Applied:", eb2TestApplied
        );

        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }
      
        retVal = true;
      }
      
      else
      {
        // Do Nothing
      }

      return retVal;
    }
  }
}
