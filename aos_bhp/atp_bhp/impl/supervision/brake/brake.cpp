/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file implements the brake component class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-09    arastogi    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-09-19    akushwah    Corrected Init function
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "atc_math.hpp"
#include "brake.hpp"
#include "config.hpp"
#include "loco_io.hpp"
#include "dmi_event_codes.hpp"
#include "dmi_bhp_event_codes.hpp"
#include "atp_application.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_odometry.hpp"
#include "abstract_dmi_handler.hpp"
#include "mode_control.hpp"
#include "brake_event_ids.hpp"
#include "abstract_analyzer_if.hpp"
#ifndef __GNUG__
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
    // Brake-test orders TCO, EB1, EB2 
    // Expected TCO Feedback (with only FB), Expected TCO Feedback (with TCO order and FB), Expected EB Feedback
    // ReleaseEvaluation
    static const struct BrakeTestOrderStatusAdap brakeTestOrderStatusArrayAdap[7] =
    {
      { false, false, false, Inactive, Inactive, Inactive, false},      // Step 0
      { true, false, false, Active, Active, Inactive, false },          // Step 1
      { false, false, false, Inactive, Inactive, Inactive, true },      // Step 2
      { false, true, false, Active, DontCare, Active, false },          // Step 3
      { false, false, false, Inactive, Inactive, Inactive, true },      // Step 4
      { false, false, true, Active, DontCare, Active, false },          // Step 5
      { false, false, false, Inactive, Inactive, Inactive, true }       // Step 6
    };

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    Brake::Brake(void) : AbstractBrake(),
      ebFeedbackBrakePressureFailed(ATC::Event::createSafetyHaltEvent(atpBrakeId, ATC::AdaptationContainer, eventIdEbFeedbackBrakePressureFailed,
        ATC::NoEB, DMICom::brakeEbSupv, "Safety Halt: EB Feedback Fault. Brake test mandatory after PowerUp.")),
      tcoFeedbackFailed(ATC::Event::createSafetyHaltEvent(atpBrakeId, ATC::AdaptationContainer, eventIdTcoFeedbackFailed,
        ATC::NoEB,DMICom::brakeTcoSupv, "Safety Halt: TCO Feedback Fault. Brake test mandatory after PowerUp.")),
      emergencyBrakesCutOut(ATC::Event::createSafetyHaltEvent(atpBrakeId, ATC::AdaptationContainer, eventIdEmergencyBrakesCutOut,
        ATC::NoEB, DMICom::emergencyBrakesCutOut, "Safety Halt: EB Cut Out. Brake test mandatory after PowerUp.")),
      errorReadingEbCutOutInputs(ATC::Event::createSafetyHaltEvent(atpBrakeId, ATC::AdaptationContainer, eventIdErrorReadingEbCutOutInputs,
        ATC::NoEB, DMICom::errorReadEBCutOut, "Safety Halt: Error reading EB Cut-Out. Brake test mandatory after PowerUp.")),
      brakeTestFailedEbExternal(ATC::Event::createLogEvent(atpBrakeId, ATC::AdaptationContainer, eventIdBrakeTestFailedEbExternal,
        DMICom::externalEbFbFailure, "Brake Test failed, due to external EB feedback failure.")),
      tcoRelayFeedbackErr(ATC::Event::createSafetyHaltEvent(atpBrakeId, ATC::AdaptationContainer, eventIdReportEventTcoRelayFb,
        ATC::NoEB, DMICom::brakeTcoRelaySupv, "Safety Halt: TCO relay feedback fault. Brake test mandatory after PowerUp.")),
      brakeTestFailedTcoInternal(ATC::Event::createLogEvent(atpBrakeId, ATC::AdaptationContainer, eventIdBrakeTestFailedTcoInternal,
        DMICom::internalTcoFbFailure, "Brake Test failed, due to TCO Internal Health Supervision failure.")),
      brakeTestFailedTcoExternal(ATC::Event::createLogEvent(atpBrakeId, ATC::AdaptationContainer, eventIdBrakeTestFailedTcoExternal,
        DMICom::externalTcoFbFailure, "Brake Test failed, due to external TCO feedback failure.")),
      bpFeedbackInvalid(ATC::Event::createLogEvent(atpBrakeId, ATC::AdaptationContainer, eventIdBpFeedbackInvalid, 0U,
        "Brake pressure Feedback Invalid.")),
      bpFeedbackInconsistent(ATC::Event::createLogEvent(atpBrakeId, ATC::AdaptationContainer, eventIdBpFeedbackInconsistent, 0U,
        "Brake pressure Feedback Inconsistent.")),
      bpFeedbackInaccurate(ATC::Event::createLogEvent(atpBrakeId, ATC::AdaptationContainer, eventIdBpFeedbackInaccurate, 0U,
        "Brake pressure Feedback Inaccurate.")),
      previousCycleExpectedTcoValue(false),
      previousCycleExpectedEbValue(false),
      tcoTestApplied(false),
      ebFeedbackOkCount(0U),
      tcoFeedbackOkCount(0U),
      tcoApplied(false),
      tcoStartTime(0),
      rapidPressureLossDetected(false),
      cycleRapidLossCnt(0U),
      cycleInconsistentBpCnt(0U),
      savedBP1(invalidBrakePressureVal),
      savedBP2(invalidBrakePressureVal),
      ebPressureBuildUpActive(true),
      ebTcoFeedbackMeassurement(0U),
      ebTcoOrderMeassurement(0U),
      oldBrakeTestSequenceStep(BrakeTestSequenceStepInit),
      brakePressureEbHistoryIndex(0U),
      adaptationInitDone(false)
    {
      for (uint8_t i = 0U; i < brakePressureEbHistoryListSize; ++i)
      {
        brakePressureEbHistory[i] = 0U;
      }
    }

    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    * (This info is not included in doxygen documentation but may be useful)
    *
    ******************************************************************************/
    Brake& Brake::instance(void)
    {
      static Brake theOnlyBrakeInstance;

      return theOnlyBrakeInstance;
    }
   
    /******************************************************************************
    * init
    ******************************************************************************/
    bool Brake::init(void)
    {
      bool coreInitValue = AbstractBrake::init();
      if ((!adaptationInitDone) && coreInitValue)
      {
        bool resEbTcoFeedback = ATC::AbstractAnalyzerIF::corePtr()->registerMeasurement("ebTcoFbOk",
          "EB/TCO Feedback ok (bit0=ebExt, bit1=tcoExt, bit2=ebInt, bit3=tcoInt)", "-", 0U, 255U, &ebTcoFeedbackMeassurement);
        bool resEbTcoOrder = ATC::AbstractAnalyzerIF::corePtr()->registerMeasurement("ebTcoFbOrder",
          "EB/TCO Order (bit0=eb1Ordered, bit1=eb2Ordered, bit2=tcoOrdered)", "-", 0U, 255U, &ebTcoOrderMeassurement);
        bool resBsRegistration = ATC::AbstractAnalyzerIF::corePtr()->registerMeasurement("brakeSeqStep",
          "break-sequence step", "step", 0U, 255U, &brakeTestSequenceStep);

        if (!(resEbTcoFeedback && resEbTcoOrder && resBsRegistration))
        {
          writeToLog(ATC::BriefLog, "Register measurement failed for analyzer", __FILE__, __LINE__);
        }

        adaptationInitDone = true;
      }

      return adaptationInitDone;
    }


    /******************************************************************************
    * run
    ******************************************************************************/
    void Brake::run(void)
    {
      AbstractBrake::run();

      //Evaluate the Rapid Pressure Loss
      evaluateRapidPressureLoss();

      bool isLcsReady;

      if (!(IO::AbstractLocoIO::corePtr()->getCoreDigitalInputValue(IO::AbstractLocoIO::LCSRdy, &isLcsReady)))
      {
        // Retrieval failed
        isLcsReady = false;
      }

      bool isEb1Inactive; // EB inputs are active low
      bool isEb2Inactive; // EB inputs are active low

      if (!(IO::AbstractLocoIO::corePtr()->getCoreDigitalOutputValue(IO::AbstractLocoIO::EmerBrake1, &isEb1Inactive)))
      {
        // Retrieval failed
        isEb1Inactive = true;
      }

      if (!(IO::AbstractLocoIO::corePtr()->getCoreDigitalOutputValue(IO::AbstractLocoIO::EmerBrake2, &isEb2Inactive)))
      {
        // Retrieval failed
        isEb2Inactive = true;
      }

      const bool isEbInactive = isEb1Inactive && isEb2Inactive;

      // LCS ready check is added to acquire Brake feedback only when LCS is connected. This prevents unnecessary logs at startup.
      // AND Brake test is not running. As during Brake test there is a gradual brake pressure buildup and during that time if
      // Brake pressure is less than the value what it should be to consider EB released. It will log Brake pressure values
      // inaccurate. This will still be the case when EB is released after EB application and brake pressure build gradually.

      // Need to monitor Brake pressure feed backs to be Valid, Accurate and Consistent.
      // 1. The Brake pressure evaluation is done when we have LCS ready to be able to have valid Analog pressures.
      // 2. When EB is ordered(either at the time of Brake test or EB order) this is taken care by Verify EB feedback and Evaluate
      //    Eb feedback calls.
      // 3. This function is called for the conditions other than when Brake test is active OR Brake pressure is getting built
      //    up after an EB application.
      if (isEbInactive && isLcsReady && (!brakeTestStarted) && (!ebPressureBuildUpActive))
      {
        evaluateBrakePressureFeedbackNoEB();
      }
    }

    /******************************************************************************
    * getBrakePressureFb
    ******************************************************************************/
    bool Brake::getBrakePressureFb(uint16_t &brakePressurevalue) const
    {
      bool retVal = false;
      bool io1ReadOk = true;
      bool io2ReadOk = true;

      uint16_t eb1ValueRead = 0U;
      uint16_t eb2ValueRead = 0U;

      uint8_t eBFeedbackSignalStatus = Config::instance().getEbFbSignalStatus();

      switch (eBFeedbackSignalStatus)
      {
        case eBFeedbackSignalStatusFirstEbFbOnly:
          io1ReadOk = IO::LocoIO::instance().getAdapAnalogInputValue(IO::LocoIO::BrakePressure1, eb1ValueRead);
          io2ReadOk = io1ReadOk;
          eb2ValueRead = eb1ValueRead;
          break;
        case eBFeedbackSignalStatusSecondEbFbOnly:
          io2ReadOk = IO::LocoIO::instance().getAdapAnalogInputValue(IO::LocoIO::BrakePressure2, eb2ValueRead);
          io1ReadOk = io2ReadOk;
          eb1ValueRead = eb2ValueRead;
          break;
        case eBFeedbackSignalStatusTwoEbFb:
          io1ReadOk = IO::LocoIO::instance().getAdapAnalogInputValue(IO::LocoIO::BrakePressure1, eb1ValueRead);
          io2ReadOk = IO::LocoIO::instance().getAdapAnalogInputValue(IO::LocoIO::BrakePressure2, eb2ValueRead);
          break;
        case eBFeedbackSignalStatusNotUsed:
        default:
          //Do nothing, it will not update eb1ValueRead and eb2ValueRead. Because EB feedback is not configured. 
          break;

      }

      if (io1ReadOk && io2ReadOk)
      {
        uint16_t readEbFeedbackDiff = (eb1ValueRead >= eb2ValueRead) ? (eb1ValueRead - eb2ValueRead) : (eb2ValueRead - eb1ValueRead);
        if (readEbFeedbackDiff <= Config::instance().getMaxEbFbDiff())
        {
          // Take the mean of the Feedback read from the two brake Pressure sensors
          const uint32_t meanEbSensorValue = static_cast<uint32_t>(eb1ValueRead) + static_cast<uint32_t>(eb2ValueRead);
          brakePressurevalue = static_cast<uint16_t>(ATC::ATCMath::instance().unsignDiv(meanEbSensorValue, 2U, __FILE__, __LINE__));
          retVal = true;
        }
      }
      return retVal;
    }

    /******************************************************************************
    * verifyExternalEbFeedback
    ******************************************************************************/
    bool Brake::verifyExternalEbFeedback(void)
    {
      const Config& cfg = Config::instance();
      // Fetch configuration if EB external feedback shall be used.
      uint8_t useEbTcoFeedback = cfg.getUseEbTcoFbDuringBrakeTest();

      if ((EbFeedbackDuringBrakeTest != useEbTcoFeedback) && (EbAndTcoFeedbackDuringBrakeTest != useEbTcoFeedback))
      {
        // EB feedback not used -> Return true, i e pretend numberOfConsecutiveReadingsFeedback correct consecutive readings
        ebFeedbackOkCount = numberOfConsecutiveReadingsFeedback;
      }
      else
      {
        uint16_t brakePressurevalue;
        const uint16_t brakePressureErrorMargin = cfg.getEbFbInaccuracy();

        if (getBrakePressureFb(brakePressurevalue))
        {
          bool expectedEbValue;

          const uint16_t oldestHistoryValue = brakePressureEbHistory[brakePressureEbHistoryIndex];
          brakePressureEbHistory[brakePressureEbHistoryIndex] = brakePressurevalue;
          brakePressureEbHistoryIndex = (brakePressureEbHistoryIndex + 1U) % brakePressureEbHistoryListSize;

          // Fetch proper expected feedback depending on configuration
          FeedbackTestResult expectedFeedbackStatus = brakeTestOrderStatusArrayAdap[brakeTestSequenceStep].ebExpectedFB;

          if (expectedFeedbackStatus == DontCare)
          {
            // Return true, i e pretend a number of correct consecutive readings
            ebFeedbackOkCount = numberOfConsecutiveReadingsFeedback;
          }
          else
          {
            expectedEbValue = (Active == expectedFeedbackStatus) ? true : false;

            // Check if the order from previous cycle was the same as current order -> If true, evaluate if another correct feedback is received.
            if ((previousCycleExpectedEbValue && expectedEbValue) || ((!previousCycleExpectedEbValue) && (!expectedEbValue)))
            {
              // Check if feedback is valid. If valid -> Increment counter, otherwise start from 1 again
              // to get a number of valid consecutive readings. 
              if (expectedEbValue)
              {
                if ((brakePressurevalue - brakePressureErrorMargin) <= cfg.getMaxEbApplyFeedback())
                {
                  ++ebFeedbackOkCount;
                }
                else
                {
                  ebFeedbackOkCount = 1U;
                }
              }
              else if ((brakePressurevalue + brakePressureErrorMargin) >= cfg.getMinEbReleaseFeedback())
              {
                const uint16_t pressureDiff = ATC::ATCMath::absDiff(brakePressurevalue, oldestHistoryValue);
                const uint32_t cyclesPerSecond = 1000U / Kernel::ATPApplication::atpAppCycleTime;

                // Also try to find the brake pressure plateau.
                if ((pressureDiff * cyclesPerSecond)  < (static_cast<uint32_t>(cfg.getBrakePressureStabilizeMargin()) *
                  static_cast<uint32_t>(brakePressureEbHistoryListSize)))
                {
                  ++ebFeedbackOkCount;
                }
                else
                {
                  ebFeedbackOkCount = 1U;
                }
              }
              else
              {
                ebFeedbackOkCount = 1U;
              }
            }
            // Reset number of consecutive readings if order has changed.
            else
            {
              ebFeedbackOkCount = 1U;

              // Remember old order value.
              previousCycleExpectedEbValue = expectedEbValue;
            }
          }
        }
      }

      // Return true if a number of consecutive readings was valid.
      return (ebFeedbackOkCount >= numberOfConsecutiveReadingsFeedback);
    }

    /******************************************************************************
    * verifyExternalTcoFeedback
    ******************************************************************************/
    bool Brake::verifyExternalTcoFeedback(void)
    {
      bool tcoFeedbackValue = false;

      // Fetch configuration if TCO external feedback shall be used.
      uint8_t tcoUsage = Config::instance().getTcoOrderAndTCOFb();

      // TCO feedback not available! Verification not possible.
      if ((tcoFbAndOrderNotUsed == tcoUsage) || (tcoOrderOnly == tcoUsage))
      {
        tcoFeedbackOkCount = 1U;
      }
      else
      {
        // Fetch TCO Feedback value
        bool isTcoValid = IO::LocoIO::instance().getAdapDigitalInputValue(IO::LocoIO::TCOFB, &tcoFeedbackValue);

        if (isTcoValid)
        {
          bool expectedTcoValue = false;

          // Fetch proper expected feedback depending on configuration: Only 2 configuration possible here,
          // already checked earlier for this condition.
          const FeedbackTestResult expectedFeedbackStatus = (tcoFbOnly == tcoUsage) ? 
            brakeTestOrderStatusArrayAdap[brakeTestSequenceStep].tcoExpectedFbWithOnlyFb :
            brakeTestOrderStatusArrayAdap[brakeTestSequenceStep].tcoExpectedFbWithOrderAndFb;

          if (expectedFeedbackStatus == DontCare)
          {
            // Return true, i e pretend a number of correct consecutive readings
            tcoFeedbackOkCount = numberOfConsecutiveReadingsFeedback;
          }
          else
          {
            expectedTcoValue = (Active == expectedFeedbackStatus) ? true : false;

            // If feedback is valid and order from previous cycle was the same as current order ->
            // Increment counter, otherwise start from 1 again to get a number of valid consecutive readings.
            if ((expectedTcoValue && tcoFeedbackValue && previousCycleExpectedTcoValue)  ||
                ((!expectedTcoValue) && (!tcoFeedbackValue) && (!previousCycleExpectedTcoValue)))
            {
              ++tcoFeedbackOkCount;
            }
            else
            {
              tcoFeedbackOkCount = 1U;
            }

            // Remember old order value.
            previousCycleExpectedTcoValue = expectedTcoValue;
          }
        }
        else
        {
          trace.write(ATC::briefTrace, "Could not read TCO Feedback.");
        }
      }

      // Return true if a number of consecutive readings was valid.
      return (tcoFeedbackOkCount >= numberOfConsecutiveReadingsFeedback);
    }

    /******************************************************************************
    * checkEbFeedback
    ******************************************************************************/
    void Brake::checkEbFeedback(const int64_t timeSinceEbApplied)
    {
      uint8_t eBFeedbackSignalStatus = Config::instance().getEbFbSignalStatus();
      bool ebFbOK = false;

      if (eBFeedbackSignalStatusNotUsed != eBFeedbackSignalStatus)
      {
        uint16_t brakePressurevalue;
        const uint16_t brakePressureErrorMargin = Config::instance().getEbFbInaccuracy();

        if (getBrakePressureFb(brakePressurevalue))
        {
          ebFbOK = (brakePressurevalue <= (Config::instance().getMaxEbApplyFeedback() + brakePressureErrorMargin));
        }

        if (!ebFbOK)
        {
          int64_t timeMaxEbFeedback = static_cast<int64_t> (Config::instance().getEbFeedbackTimeout()) * 1000; //ms
          ebPressureBuildUpActive = false; // Brake more than the value to be considered as brake applied.

          if (timeSinceEbApplied > timeMaxEbFeedback)
          {
            // Safety Halt when feedback not as expected within expected time
            ATC::AbstractEventHandler::corePtr()->reportEvent(ebFeedbackBrakePressureFailed, __FILE__, __LINE__);
            // Brake Test becomes mandatory after recovering from Safety Halt
            if (!Config::instance().setBrakeTestReason(brakeTestReasonEbFeedbackFault))
            {
              trace.write(ATC::briefTrace, "Failed to store the Brake Test Reason in NVS.");
              writeToLog(ATC::BriefLog, "Failed to store the Brake Test Reason in NVS.", __FILE__, __LINE__);
            }
          }
        }
        else
        {
          ebPressureBuildUpActive = true;
        }
      }
      uint8_t tcoFeedbackSignal = Config::instance().getTcoOrderAndTCOFb();

      //Check if any one either TCO order OR TCO  feedback is used or not through configuration parameters
      //If any one is used check TCO feedback too. Other checks are done inside this function.
      if (tcoFbAndOrderNotUsed != tcoFeedbackSignal)
      {
        checkTcoFeedback(timeSinceEbApplied);
      }
    }

    /******************************************************************************
    * checkTcoFeedback
    ******************************************************************************/
    void Brake::checkTcoFeedback(const int64_t timeSinceTcoApplied)
    {
      uint8_t tcoFeedbackSignalStatus = Config::instance().getTcoOrderAndTCOFb();

      // Is any of TCO Order OR TCO Fb used? Do nothing if none used.
      if (tcoFbAndOrderNotUsed != tcoFeedbackSignalStatus)
      {
        // Shall the TCO order relay be checked? This will be the case if TCO is available/used.
        if ((tcoOrderOnly == tcoFeedbackSignalStatus) || ((tcoFbAndOrderUsed == tcoFeedbackSignalStatus)))
        {
          bool resultTco;
          bool resFoundTco = IO::LocoIO::instance().getAdapDigitalOutputValue(IO::LocoIO::TCO, &resultTco);

          if (resFoundTco)
          {
            int64_t tTcoOrderFb = static_cast<int64_t>(AbstractConfig::corePtr()->getInternalRelaysTimeout());

            // TCO is active low, so output-value must be '1' and a certain time passed to issue an error.
            if ((resultTco) && (timeSinceTcoApplied > tTcoOrderFb))
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(tcoRelayFeedbackErr, __FILE__, __LINE__);

              // Brake Test becomes mandatory after recovering from Safety Halt
              if (!AbstractConfig::corePtr()->setBrakeTestReason(brakeTestReasonTCOOrderFault))
              {
                trace.write(ATC::briefTrace, "Failed to store the Brake Test Reason in NVS.");
                writeToLog(ATC::BriefLog, "Failed to store the Brake Test Reason in NVS.", __FILE__, __LINE__);
              }
            }
          }
          else
          {
            trace.write(ATC::briefTrace, "Could not read TCO I/O order.");
          }
        }

        if ((tcoFbOnly == tcoFeedbackSignalStatus) || ((tcoFbAndOrderUsed == tcoFeedbackSignalStatus)))
        {
          // Check the TCO-feedback signal
          bool tcoFeedbackValue;

          // Fetch TCO Feedback value
          bool isTcoValid = IO::LocoIO::instance().getAdapDigitalInputValue(IO::LocoIO::TCOFB, &tcoFeedbackValue);

          if (isTcoValid)
          {
            int64_t timeMaxTcoFeedback = static_cast<int64_t> (Config::instance().getTcoFeedbackTimeout()) * 1000; //ms

            // Output-value must be false and a certain time passed to issue an error.
            if ((!tcoFeedbackValue) && (timeSinceTcoApplied > timeMaxTcoFeedback))
            {
              if (timeSinceTcoApplied > timeMaxTcoFeedback)
              {
                // Safety Halt when feedback not as expected within expected time
                ATC::AbstractEventHandler::corePtr()->reportEvent(tcoFeedbackFailed, __FILE__, __LINE__);

                // Brake Test becomes mandatory after recovering from Safety Halt
                if (!Config::instance().setBrakeTestReason(brakeTestReasonTCOFeedbackFault))
                {
                  trace.write(ATC::briefTrace, "Failed to store the Brake Test Reason in NVS.");
                  writeToLog(ATC::BriefLog, "Failed to store the Brake Test Reason in NVS.", __FILE__, __LINE__);
                }
              }
            }
          }
          else
          {
            trace.write(ATC::briefTrace, "Could not read TCO I/O feedback.");
          }
        }
      }// End of if (tcoFbAndOrderNotUsed != tcoFeedbackSignalStatus)
    }

    /******************************************************************************
    * getMaxEbFbWaitCycle
    ******************************************************************************/
    uint32_t Brake::getMaxEbFbWaitCycle() const
    {
      const uint32_t ebFeedbackTime = static_cast<uint32_t>(Config::instance().getEbFeedbackTimeout()) * 1000U;
      return ATC::ATCMath::instance().unsignDiv(ebFeedbackTime, Kernel::AbstractATPApplication::atpAppCycleTime,  __FILE__, __LINE__);
    }

    /******************************************************************************
    * getMaxTcoFbWaitCycle
    ******************************************************************************/
    uint32_t Brake::getMaxTcoFbWaitCycle() const
    {
      const uint32_t tcoFeedbackTime = static_cast<uint32_t>(Config::instance().getTcoFeedbackTimeout()) * 1000U;
      return ATC::ATCMath::instance().unsignDiv(tcoFeedbackTime, Kernel::AbstractATPApplication::atpAppCycleTime, __FILE__, __LINE__);
    }

    /******************************************************************************
    * processEbCutOut
    ******************************************************************************/
    void Brake::processEbCutOut(void)
    {
      //Are EB Cut-out Inputs configured to be used?
      if (Config::instance().getEbCutOutConfigured())
      {
        bool eBCutOut1A;
        bool eBCutOut1B;
        bool eBCutOut2A;
        bool eBCutOut2B;
        bool eBCutOutFault = false;

        bool io1ReadOk = IO::LocoIO::instance().getAdapDigitalInputValue(IO::LocoIO::EBCutOut1A, &eBCutOut1A); //Active = Not 'cut-out'
        bool io2ReadOk = IO::LocoIO::instance().getAdapDigitalInputValue(IO::LocoIO::EBCutOut1B, &eBCutOut1B); //In-Active = Not 'cut-out'
        bool io3ReadOk = IO::LocoIO::instance().getAdapDigitalInputValue(IO::LocoIO::EBCutOut2A, &eBCutOut2A); //Active = Not 'cut-out' 
        bool io4ReadOk = IO::LocoIO::instance().getAdapDigitalInputValue(IO::LocoIO::EBCutOut2B, &eBCutOut2B); //In-Active = Not 'cut-out'

        if (io1ReadOk && io2ReadOk && io3ReadOk && io4ReadOk)
        {
          // Are Emergency Brakes Cut-out?
          if (!((eBCutOut1A && (!eBCutOut1B)) && (eBCutOut2A && (!eBCutOut2B))))
          {
            // Brake Test mandatory after recovering from Safety Halt
            ATC::AbstractEventHandler::corePtr()->reportEvent(emergencyBrakesCutOut, __FILE__, __LINE__);
            eBCutOutFault = true;
          }

        }
        else
        {
          // Brake Test mandatory after recovering from Safety Halt
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorReadingEbCutOutInputs, __FILE__, __LINE__);
          eBCutOutFault = true;
        }
        if (eBCutOutFault)
        {
          // Brake Test becomes mandatory after recovering from Safety Halt
          if (!Config::instance().setBrakeTestReason(brakeTestReasonEbCutOut))
          {
            trace.write(ATC::briefTrace, "Failed to store the Brake Test Reason in NVS.");
            writeToLog(ATC::BriefLog, "Failed to store the Brake Test Reason in NVS.", __FILE__, __LINE__);
          }
        }
      }
    }

    /******************************************************************************
    * processBrakeStateMachine
    ******************************************************************************/
    bool Brake::processBrakeTestStateMachine()
    {
      // Wait to start
      if (!brakeTestStarted)
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(startingBrakeTest, __FILE__, __LINE__);

        // Wait for Standstill before starting the Brake Test Sequence
        if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
        {
          trace.write(ATC::briefTrace, "BrakeTestSequence Started.");
          writeToLog(ATC::BriefLog, "BrakeTestSequence Started", __FILE__, __LINE__);
          brakeTestStarted = true;
        }
      }
      // Test Steps
      else
      {
        if (brakeTestSequenceStep != oldBrakeTestSequenceStep)
        {
          trace.write(ATC::briefTrace, "New BrakeTestSequenceStep: ", static_cast<uint32_t>(brakeTestSequenceStep));
          writeToLog(ATC::BriefLog, "New BrakeTestSequenceStep: ", static_cast<uint32_t>(brakeTestSequenceStep), __FILE__, __LINE__);

          char_t logStr[120];

          //lint -e{586} snprintf is needed here
          const int32_t res = snprintf(&logStr[0], sizeof(logStr), "Orders: eb1TestApplied=%u, eb2TestApplied=%u, tcoTestApplied=%u",
            eb1TestApplied, eb2TestApplied, tcoTestApplied);

          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
          {
            writeToLog(ATC::BriefLog, &logStr[0], __FILE__, __LINE__);
          }

          // Save old step for diagnostic purpose
          oldBrakeTestSequenceStep = brakeTestSequenceStep;
        }

        ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestInProgress, __FILE__, __LINE__);

        // Feedback verification ok?
        if (verifyBrakeTestFeedback())
        {
          // Fetch configuration if TCO external feedback shall be used.
          uint8_t tcoFeedbackSignalStatus = Config::instance().getTcoOrderAndTCOFb();

          // Test-steps 1 and 2 shall only be performed if the AOS is configured to order TCO -> Otherwise jump to EB-tests.
          if (((tcoFbAndOrderUsed != tcoFeedbackSignalStatus) && (tcoOrderOnly != tcoFeedbackSignalStatus)) &&
            (BrakeTestSequenceStepInit == brakeTestSequenceStep))
          {
            brakeTestSequenceStep += 3U;
          }
          else
          {
            ++brakeTestSequenceStep;
          }
        }
      }

      if (brakeTestSequenceStep < numberOfBrakeTestStepsAdap)
      {
        // Apply EB & TCO Orders for actual Brake Test Step
        eb1TestApplied = brakeTestOrderStatusArrayAdap[brakeTestSequenceStep].eb1TestApplied;
        eb2TestApplied = brakeTestOrderStatusArrayAdap[brakeTestSequenceStep].eb2TestApplied;
        tcoTestApplied = brakeTestOrderStatusArrayAdap[brakeTestSequenceStep].tcoTestApplied;
      }

      // Update the diagnostic measurement value for TCO/EB Orders
      ebTcoOrderMeassurement = eb1TestApplied ? 0x1U : 0x0U;
      ebTcoOrderMeassurement |= eb2TestApplied ? 0x2U : 0x0U;
      ebTcoOrderMeassurement |= tcoTestApplied ? 0x4U : 0x0U;

      // Reached last test-step?
      return (brakeTestSequenceStep >= numberOfBrakeTestStepsAdap);
    }
    
    /******************************************************************************
    * verifyInternalTCORelay
    ******************************************************************************/
    bool Brake::verifyInternalTCORelays() const
    {
      bool internalTcoRelayOk = false;
      bool readTcoValue = true;

      const uint8_t tcoConfigStatus = Config::instance().getTcoOrderAndTCOFb();
      const bool isValidTcoValue = IO::LocoIO::instance().getAdapDigitalOutputValue(IO::LocoIO::TCO, &readTcoValue);

      //For the cases TCO order is used/configured.
      if ((tcoFbAndOrderUsed == tcoConfigStatus) || (tcoOrderOnly == tcoConfigStatus))
      {
        if (isValidTcoValue)
        {
          // Read- and write value for the digital output shall be the same
          // TCO Order is active low
          if (((!readTcoValue) && tcoTestApplied) || ((readTcoValue) && (!tcoTestApplied)))
          {
            internalTcoRelayOk = true;
          }
        }
        else
        {
          trace.write(ATC::briefTrace, "Could not read internal TCO relays.");
        }
      }
      else
      {
        // TCO Order is not used thus, consider TCO relay to be okay.
        internalTcoRelayOk = true;
      }

      return internalTcoRelayOk;
    }

    /******************************************************************************
    * verifyBrakeTestFeedback
    ******************************************************************************/
    bool Brake::verifyBrakeTestFeedback()
    {
      // Fetch configuration if EB external feedback shall be used.
      uint8_t useEbTcoFeedback = Config::instance().getUseEbTcoFbDuringBrakeTest();

      bool ebExtFeedbackOk = true;
      bool tcoExtFeedbackOk = true;
      bool ebIntFeedbackOk = true;
      bool tcoIntFeedbackOk = true;
      bool brakeTestTimeOut = false;

      uint32_t maxEbWaitCycle = ATC::uint32Max;
      uint32_t maxTcoWaitCycle = ATC::uint32Max;

      // Select EB Internal/External verification status and timeout
      if ((EbFeedbackDuringBrakeTest == useEbTcoFeedback) || (EbAndTcoFeedbackDuringBrakeTest == useEbTcoFeedback))
      {
        ebExtFeedbackOk = verifyExternalEbFeedback();
        maxEbWaitCycle = getMaxEbFbWaitCycle();
      }
      else
      {
        ebIntFeedbackOk = verifyInternalEbRelays();
        maxEbWaitCycle = getMaxInternalRelaysFbWaitCycle();
      }

      // Select TCO Internal/External verification status and timeouts
      if ((TcoFeedbackDuringBrakeTest == useEbTcoFeedback) || (EbAndTcoFeedbackDuringBrakeTest == useEbTcoFeedback))
      {
        tcoExtFeedbackOk = verifyExternalTcoFeedback();
        maxTcoWaitCycle = getMaxTcoFbWaitCycle();
      }
      else
      {
        tcoIntFeedbackOk = verifyInternalTCORelays();
        maxTcoWaitCycle = getMaxInternalRelaysFbWaitCycle();
      }

      // All feedback ok -> Reset 'timer'
      if (ebExtFeedbackOk && tcoExtFeedbackOk && ebIntFeedbackOk && tcoIntFeedbackOk)
      {
        feedbackWaitCycle = 0U;
      }
      else if (((feedbackWaitCycle > maxEbWaitCycle) || (feedbackWaitCycle > maxTcoWaitCycle))
        && (!brakeTestOrderStatusArrayAdap[brakeTestSequenceStep].releaseEvaluation))
      {
        if (brakeTestSequenceStep == BrakeTestSequenceStepInit)
        {
          brakeTestTimeOut = true;
          // Unable to start Brake Test!
          ATC::AbstractEventHandler::corePtr()->reportEvent(unableToStartBrakeTest, __FILE__, __LINE__);
        }
        else if ((feedbackWaitCycle > maxTcoWaitCycle) && ((!tcoIntFeedbackOk) || (!tcoExtFeedbackOk)))
        {
          brakeTestTimeOut = true;
          if (!tcoIntFeedbackOk)
          {
            // Brake Test failed due to internal TCO Feedback failure
            ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestFailedTcoInternal, __FILE__, __LINE__);
          }
          else
          {
            // Brake Test failed due to external TCO Brake Pressure Feedback failure
            ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestFailedTcoExternal, __FILE__, __LINE__);
          }
        }
        else if ((feedbackWaitCycle > maxEbWaitCycle) && ((!ebIntFeedbackOk) || (!ebExtFeedbackOk)))
        {
          brakeTestTimeOut = true;
          if (!ebIntFeedbackOk)
          {
            // Brake Test failed due to internal EB brake relay 
            ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestFailedEbInternal, __FILE__, __LINE__);
          }
          else
          {
            // Brake Test failed due to external EB Brake Pressure Feedback failure
            ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestFailedEbExternal, __FILE__, __LINE__);
          }
        }
        else
        {
          //Do nothing
        }
      }
      else
      {
        //Do nothing
      }

      // Update the diagnostic measurement value for TCO/EB Feedback
      ebTcoFeedbackMeassurement = ebExtFeedbackOk ? 0x1U : 0x0U;
      ebTcoFeedbackMeassurement |= tcoExtFeedbackOk ? 0x2U : 0x0U;
      ebTcoFeedbackMeassurement |= ebIntFeedbackOk ? 0x4U : 0x0U;
      ebTcoFeedbackMeassurement |= tcoIntFeedbackOk ? 0x8U : 0x0U;

      if (brakeTestTimeOut)
      {
        char_t logStr[120];
        
        trace.write(ATC::briefTrace, "feedbackWaitCycle=", feedbackWaitCycle);
        trace.write(ATC::briefTrace, "maxEbWaitCycle=", maxEbWaitCycle);
        trace.write(ATC::briefTrace, "maxTcoWaitCycle=", maxTcoWaitCycle);

        //lint -e{586} snprintf is needed here
        int32_t res = snprintf(&logStr[0], sizeof(logStr), "Braketest timeout: feedbackWaitCycle=%u, maxEbWaitCycle=%u, maxTcoWaitCycle=%u",
          feedbackWaitCycle, maxEbWaitCycle, maxTcoWaitCycle);

        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
        {
          writeToLog(ATC::BriefLog, &logStr[0], __FILE__, __LINE__);
        }

        //lint -e{586} snprintf is needed here
        res = snprintf(&logStr[0],
          sizeof(logStr),
          "ebExtFeedbackOk=%s, tcoExtFeedbackOk=%s, ebIntFeedbackOk=%s, tcoIntFeedbackOk=%s",
          ebExtFeedbackOk ? "True" : "False",
          tcoExtFeedbackOk ? "True" : "False",
          ebIntFeedbackOk ? "True" : "False",
          tcoIntFeedbackOk ? "True" : "False");

        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
        {
          writeToLog(ATC::BriefLog, &logStr[0], __FILE__, __LINE__);
        }

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

      return (ebExtFeedbackOk && tcoExtFeedbackOk && ebIntFeedbackOk && tcoIntFeedbackOk);
    }

    /******************************************************************************
    * abortBrakeTest
    ******************************************************************************/
    bool Brake::abortBrakeTest(void)
    {
      bool brakeTestAbort = true;
      bool isEbApplied = getEbApplied();
      bool isSbApplied = getSbApplied();
      bool emergencyStopActive = false;

      LocoTypeAdap locoType = static_cast<LocoTypeAdap>(Config::instance().getLocoType());

      bool abortByDriver = (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonAbortBrakeTest) ? true : false;
      bool additionalBrakeEvents = (isEbApplied || isSbApplied);
      CabActiveStatus cabStatus = ATP::Kernel::AbstractModeControl::corePtr()->getActiveCab();

      // For EMD Loco the Emergency Stop Active signal shall not abort the brake test since the signal will be 
      // activated when the driver releases EB after each AOS EB order. 
      if (EMD != locoType)
      {
        emergencyStopActive = IO::AbstractLocoIO::corePtr()->getEmergencyStopActiveAlert();
      }
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
    void Brake::resetBrakeTestStateMachine(void)
    {
      AbstractBrake::resetBrakeTestStateMachine();

      tcoTestApplied = false;
      rapidPressureLossDetected = false;

      previousCycleExpectedEbValue = false;
      previousCycleExpectedTcoValue = false;

      ebFeedbackOkCount = 0U;
      tcoFeedbackOkCount = 0U;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void Brake::initCrossCompare() const
    {
      AbstractBrake::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&ebFeedbackBrakePressureFailed));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&tcoFeedbackFailed));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&emergencyBrakesCutOut));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&errorReadingEbCutOutInputs));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&brakeTestFailedEbExternal));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&tcoRelayFeedbackErr));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&brakeTestFailedTcoInternal));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&brakeTestFailedTcoExternal));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&bpFeedbackInvalid));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&bpFeedbackInconsistent));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&bpFeedbackInaccurate));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&previousCycleExpectedTcoValue));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&previousCycleExpectedEbValue));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&tcoTestApplied));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&ebFeedbackOkCount));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&tcoFeedbackOkCount));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&tcoApplied));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&tcoStartTime));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&rapidPressureLossDetected));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&cycleRapidLossCnt));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&cycleInconsistentBpCnt));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&savedBP1));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&savedBP2));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&ebPressureBuildUpActive));
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool Brake::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      static_cast<void>(AbstractBrake::consoleCall(argc, argv));

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
          "%-35s%-10d\n",
          "TCO Test Applied:", tcoTestApplied
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

    /******************************************************************************
    * getTcoTestApplied
    ******************************************************************************/
    bool Brake::getTcoTestApplied(void) const
    {
      return tcoTestApplied;
    }

    /******************************************************************************
    * getRapidLossInBrakePressureDetected
    ******************************************************************************/
    bool Brake::getRapidLossInBrakePressureDetected(void) const
    {
      return rapidPressureLossDetected;
    }

    /******************************************************************************
    * Return TCO signal output value
    ******************************************************************************/
    bool Brake::getTcoApplied(void) const
    {
      return tcoApplied;
    }

    /******************************************************************************
    * Return if the condition to apply TCO is set or not
    ******************************************************************************/
    bool Brake::isApplyTco() const
    {
      bool isApplyTcoStatus = false;
      if (AbstractBrake::corePtr()->getEbApplied())
      {
        isApplyTcoStatus = true;
      }
      return isApplyTcoStatus;
    }

    /******************************************************************************
    * Function to process the TCO feedback
    ******************************************************************************/
    void Brake::processTco()
    {
      uint8_t tcoFeedbackSignal = Config::instance().getTcoOrderAndTCOFb();

      //Check if both TCO order and feedback is unavailable. Else there is a need to check either TCO o/p OR TCO feedback
      //There is a need to get TCO feedback in case EB is applied and TCO feedback available/used.
      if (tcoFbAndOrderNotUsed != tcoFeedbackSignal)
      {
        //Check if TCO should be applied
        if (isApplyTco())
        {
          //Check if TCO is applied
          if (!tcoApplied)
          {
            //If not, set TCO applied to true and initiate TCO start time
            tcoApplied = true;
          }
          else
          {
            //Calculate the time since TCO is applied and check TCO feedback
            int64_t currentTime = vfwGetReferenceTime();
            checkTcoFeedback(currentTime - tcoStartTime);
          }
          tcoStartTime = vfwGetReferenceTime();
        }
        else
        {
          tcoApplied = false;
        }
      }
      else
      {
        tcoApplied = false;
      }
    }

    /******************************************************************************
    * evaluateRapidPressureLoss
    ******************************************************************************/
    void Brake::evaluateRapidPressureLoss(void)
    {
      if (0U == cycleRapidLossCnt)
      {
        const uint8_t  ebFbAvailable = Config::instance().getEbFbSignalStatus();
        const uint16_t deltaRapidLoss = Config::instance().getBPDeltaRapidLoss();
        bool  rapidBP1Detected = false;
        bool  rapidBP2Detected = false;

        rapidPressureLossDetected = false;

        if ((eBFeedbackSignalStatusFirstEbFbOnly == ebFbAvailable) || (eBFeedbackSignalStatusTwoEbFb == ebFbAvailable))
        {
          //Get Brake pressure 1 and compare with previous Brake pressure
          uint16_t val1;

          bool retVal1 = IO::LocoIO::instance().getAdapAnalogInputValue(IO::LocoIO::BrakePressure1, val1);

          if (retVal1)
          {
            // Cannot report for the first cycle.
            if ((savedBP1 != invalidBrakePressureVal) && (val1 < savedBP1))
            {
              const uint16_t deltaBP1 = savedBP1 - val1;

              if (deltaBP1 > deltaRapidLoss)
              {
                rapidBP1Detected = true;
              }
            }
            savedBP1 = val1;
          }
          else
          {
            writeToLog(ATC::BriefLog, "Invalid Brake pressure 1 values received", __FILE__, __LINE__);
            trace.write(ATC::briefTrace, "Invalid Brake pressure 1 values received");
          }
        }

        if ((eBFeedbackSignalStatusSecondEbFbOnly == ebFbAvailable) || (eBFeedbackSignalStatusTwoEbFb == ebFbAvailable))
        {
          //Get Brake pressure 2 and compare with previous Brake pressure
          uint16_t val2;

          bool retVal2 = IO::LocoIO::instance().getAdapAnalogInputValue(IO::LocoIO::BrakePressure2, val2);

          if (retVal2)
          {
            // Cannot report for the first cycle.
            if ((savedBP2 != invalidBrakePressureVal) && (val2 < savedBP2))
            {
              const uint16_t deltaBP2 = savedBP2 - val2;

              if (deltaBP2 > deltaRapidLoss)
              {
                rapidBP2Detected = true;
              }
            }
            savedBP2 = val2;
          }
          else
          {
            writeToLog(ATC::BriefLog, "Invalid Brake pressure 2 values received", __FILE__, __LINE__);
            trace.write(ATC::briefTrace, "Invalid Brake pressure 2 values received");
          }
        }

        // If either of the Brake pressure loss is detected. raise the flag
        if (rapidBP1Detected || rapidBP2Detected)
        {
          rapidPressureLossDetected = true;
        }

        const uint16_t timeDurationToCalcRapidLoss = static_cast<uint16_t>(Config::instance().getBPTimeRapidLoss()) * 100U; // [0.1 s] to [ms]
        cycleRapidLossCnt = timeDurationToCalcRapidLoss / ATC::cycleCntToMsec;
      }

      cycleRapidLossCnt--;
    }

    /******************************************************************************
    * evaluateEBFeedback
    ******************************************************************************/
    void Brake::evaluateBrakePressureFeedbackNoEB(void)
    {
      const uint8_t  ebFbAvailable = Config::instance().getEbFbSignalStatus();

      uint16_t valBP1 = 0U;
      uint16_t valBP2 = 0U;
      bool retValBP1 = false;
      bool retValBP2 = false;

      if ((eBFeedbackSignalStatusFirstEbFbOnly == ebFbAvailable) || (eBFeedbackSignalStatusTwoEbFb == ebFbAvailable))
      {
        retValBP1 = IO::LocoIO::instance().getAdapAnalogInputValue(IO::LocoIO::BrakePressure1, valBP1);

        if (retValBP1)
        {
          const uint16_t brakePressureErrorMargin = Config::instance().getEbFbInaccuracy();

          const bool bpFbOK = ((valBP1 + brakePressureErrorMargin) > Config::instance().getMaxEbApplyFeedback()) ? true : false;

          if (!bpFbOK)
          {
            //Brake pressure feedback is not accurate by the accuracy margins.
            ATC::AbstractEventHandler::corePtr()->reportEvent(bpFeedbackInaccurate, __FILE__, __LINE__);
          }
        }
        else
        {
          //Issue a Log event
          ATC::AbstractEventHandler::corePtr()->reportEvent(bpFeedbackInvalid, __FILE__, __LINE__);
        }
      }

      if ((eBFeedbackSignalStatusSecondEbFbOnly == ebFbAvailable) || (eBFeedbackSignalStatusTwoEbFb == ebFbAvailable))
      {
        retValBP2 = IO::LocoIO::instance().getAdapAnalogInputValue(IO::LocoIO::BrakePressure2, valBP2);

        if (retValBP2)
        {
          const uint16_t brakePressureErrorMargin = Config::instance().getEbFbInaccuracy();

          const bool bpFbOK = ((valBP2 + brakePressureErrorMargin) > Config::instance().getMaxEbApplyFeedback()) ? true : false;

          if (!bpFbOK)
          {
            //Brake pressure feedback is not accurate by the accuracy margins.
            ATC::AbstractEventHandler::corePtr()->reportEvent(bpFeedbackInaccurate, __FILE__, __LINE__);
          }
        }
        else
        {
          //Issue a Log event
          ATC::AbstractEventHandler::corePtr()->reportEvent(bpFeedbackInvalid, __FILE__, __LINE__);
        }
      }

      if (retValBP1 && retValBP2)
      {
        //If all okay. handle if the 2 values match or not.

        const uint16_t maxEbFbDiff = Config::instance().getMaxEbFbDiff();
        const uint16_t diff = ATC::ATCMath::absDiff(valBP1, valBP2);

        if (diff > maxEbFbDiff)
        {
          //Get cycle count from Seconds.
          uint16_t maxInconsistentCycleCnt = Config::instance().getMaxEbFbDiffTimeout() * static_cast<uint16_t>(10U);

          if (cycleInconsistentBpCnt > maxInconsistentCycleCnt)
          {
            //Issue a Log event
            ATC::AbstractEventHandler::corePtr()->reportEvent(bpFeedbackInconsistent, __FILE__, __LINE__);

            //Reset to avoid raising the event every cycle.
            cycleInconsistentBpCnt = 0U;
          }
          else
          {
            cycleInconsistentBpCnt++;
          }
        }
        else
        {
          cycleInconsistentBpCnt = 0U;
        }
      }
    }

    /******************************************************************************
    * getSbBrakeTestRequested
    ******************************************************************************/
    bool Brake::getSbBrakeTestRequested(void) const
    {
      return Kernel::ModeControl::instance().getSbBrakeTestRequested();
    }
  }
}
