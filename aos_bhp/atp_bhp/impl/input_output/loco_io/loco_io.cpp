/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The LOCO IO component deals with the interface between the vital IO and the AOS SW.
*  This is the implementation of the adaptation for the component
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-22    arastogi    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-06-10    adgupta     Implementation of LocoIO functionality
* 2016-09-19    akushwah    Corrected Init function
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "loco_io.hpp"
#include "brake.hpp"
#include "config.hpp"
#include "abstract_mode_control.hpp"
#include "dmi_bhp_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "loco_io_event_ids.hpp"
#include "abstract_analyzer_if.hpp"
#ifdef __GNUG__
#include <cstdio>
#endif


/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/
namespace
{
  const char_t expectedViohServerVersion[] = "1.8.00";
  const char_t expectedViohClientVersion[] = "1.8.00";
}

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace IO
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    LocoIO::LocoIO(void) :
      errorHiRailConfigInvalidState(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::AdaptationContainer, eventIdErrHiRailConfigInvalidState,
        ATC::NoEB, DMICom::locoIONotValid, "HiRail Configuration has an invalid state")),
      errorHiRailConfigRoadTransitionState(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::AdaptationContainer,
        eventIdErrHiRailConfigRoadTransitionState, ATC::NoEB, DMICom::locoIONotValid, "HiRail configuration is in Road or transition state")),
      initDone(false),
      AbstractLocoIO((static_cast<uint8_t>(NumOfAdapDigitalInputs) + static_cast<uint8_t>(NumOfCoreDigitalInputs)),
      (static_cast<uint8_t>(NumOfAdapDigitalOutputs) + static_cast<uint8_t>(NumOfCoreDigitalOutputs)),
        (static_cast<uint8_t>(NumOfCoreAnalogInputs) + static_cast<uint8_t>(NumOfAdapAnalogInputs)),
        NumOfCoreAnalogOutputs), scaledBP1Measurement(0U), scaledBP2Measurement(0U), tcoFBMeassurement(0U)
    {
      // The signal numbers are fixed and are channels on the hardware.

      // Input 3-4, 23-24 are Spare Inputs 

      // Initialize Digital Core Inputs.
      digitalInputs[IsolationA].setInitialValues(1U, "IsolationA", true, true);
      digitalInputs[IsolationB].setInitialValues(2U, "IsolationB", false, true);
      digitalInputs[Manual].setInitialValues(5U, "ATOManual", true, true);
      digitalInputs[Supervised].setInitialValues(6U, "ATOSupervised", false, true);
      digitalInputs[Automatic].setInitialValues(7U, "ATOAutomatic", false, true);
      digitalInputs[Cab1].setInitialValues(8U, "CabinAActive", false, false);
      digitalInputs[Cab2].setInitialValues(9U, "CabinBActive", false, false);
      digitalInputs[Fwd].setInitialValues(10U, "Forward", false, false);
      digitalInputs[Rev].setInitialValues(11U, "Reverse", false, false);
      digitalInputs[LCSRdy].setInitialValues(12U, "LocomotiveReady", false, false);
      digitalInputs[EmerStopActive].setInitialValues(13U, "EmergencyStopActive", true, false);
      digitalInputs[OFFIn].setInitialValues(14U, "AOSOffRequest", false, false);

      // Initialize Digital Adaptation Inputs. Adaptation Inputs are placed after Digital Core Inputs in the inputs array.
      const uint8_t startAdaptationIndex = static_cast<uint8_t>(NumOfCoreDigitalInputs);
      digitalInputs[startAdaptationIndex + static_cast<uint8_t>(RoadM)].setInitialValues(15U, "RoadMode", false, true);
      digitalInputs[startAdaptationIndex + static_cast<uint8_t>(RailM)].setInitialValues(16U, "RailMode", true, true);
      digitalInputs[startAdaptationIndex + static_cast<uint8_t>(TCOFB)].setInitialValues(17U, "TractionCutOffFB", false, true);
      digitalInputs[startAdaptationIndex + static_cast<uint8_t>(NONLEAD)].setInitialValues(18U, "NonLeadingLoco", false, false);
      digitalInputs[startAdaptationIndex + static_cast<uint8_t>(EBCutOut1A)].setInitialValues(19U, "EBCutOut1A", true, true);
      digitalInputs[startAdaptationIndex + static_cast<uint8_t>(EBCutOut1B)].setInitialValues(20U, "EBCutOut1B", false, true);
      digitalInputs[startAdaptationIndex + static_cast<uint8_t>(EBCutOut2A)].setInitialValues(21U, "EBCutOut2A", true, true);
      digitalInputs[startAdaptationIndex + static_cast<uint8_t>(EBCutOut2B)].setInitialValues(22U, "EBCutOut2B", false, true);

      
      // Initialize Analog Adaptation Inputs. Adaptation Analog Inputs are placed after Analog Core Inputs in the inputs array.
      analogInputs[static_cast<uint8_t>(NumOfCoreAnalogInputs) + static_cast<uint8_t>(BrakePressure1)].setInitialValues(1U, "BrakePressure1");
      analogInputs[static_cast<uint8_t>(NumOfCoreAnalogInputs) + static_cast<uint8_t>(BrakePressure2)].setInitialValues(2U, "BrakePressure2");

      analogOutputs[0U].setInitialValues(1U, "AnalogOutput1");
      analogOutputs[1U].setInitialValues(2U, "AnalogOutput2");

      // Initialize Digital Core Outputs. (Chapter 5.8.2 Digital Outputs 3NSS015103D0195)  
      digitalOutputs[EmerBrake1      ].setInitialValues(1U,  "EmerBrake1", true);        // OUT1
      digitalOutputs[EmerBrake2      ].setInitialValues(2U,  "EmerBrake2", true);        // OUT2
      digitalOutputs[EmerBrakeApplied].setInitialValues(3U,  "EmerBrakeApplied", false); // OUT3
      digitalOutputs[ServiceBrake    ].setInitialValues(4U,  "ServiceBrake", false);     // OUT4
      digitalOutputs[ATPOk           ].setInitialValues(7U,  "ATPOk", false);            // OUT7
      digitalOutputs[Buzzer          ].setInitialValues(8U,  "Buzzer", false);           // OUT8
      digitalOutputs[Lamp            ].setInitialValues(9U,  "Lamp", false);             // OUT9
      digitalOutputs[PowerOFF        ].setInitialValues(10U, "PowerOFF", false);         // OUT10

      // Initialize Digital Adaptation Outputs. Adaptation Outputs are placed after Digital Core Outputs in the outputs array.
      digitalOutputs[static_cast<uint8_t>(NumOfCoreDigitalOutputs) + static_cast<uint8_t>(TCO)].setInitialValues(5U, "TCO", false); // OUT5
    }

    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    *
    ******************************************************************************/
    LocoIO& LocoIO::instance(void)
    {
      static LocoIO theOnlyLocoIOInstance;

      return theOnlyLocoIOInstance;
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool LocoIO::init(void)
    {
      bool coreInitValue = AbstractLocoIO::init();
      if ((!initDone) && coreInitValue)
      {
        const uint16_t minValueBP1 = Config::instance().getRangeMinBP1();
        const uint16_t maxValueBP1 = Config::instance().getRangeMaxBP1();
        const uint16_t minValueBP2 = Config::instance().getRangeMinBP2();
        const uint16_t maxValueBP2 = Config::instance().getRangeMaxBP2();

        ATC::AbstractAnalyzerIF* const aif = ATC::AbstractAnalyzerIF::corePtr();

        const bool resBP1AIF = aif->registerMeasurement("bp1", "brake pressure 1", "kpa", minValueBP1, maxValueBP1, &scaledBP1Measurement);
        const bool resBP2AIF = aif->registerMeasurement("bp2", "brake pressure 2", "kpa", minValueBP2, maxValueBP2, &scaledBP2Measurement);
        const bool resTCOFBDIF = aif->registerMeasurement("tcoFbRaw", "TCO Feedback", "boolean", 0U, 255U, &tcoFBMeassurement);

        if (!(resBP1AIF && resBP2AIF && resTCOFBDIF))
        {
          writeToLog(ATC::BriefLog, "Register measurement failed for analyzer", __FILE__, __LINE__);
        }
        initDone = true;
      }
      return initDone;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void LocoIO::initCrossCompare() const
    {
      AbstractLocoIO::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorHiRailConfigInvalidState));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorHiRailConfigRoadTransitionState));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&initDone));
    }

    /******************************************************************************
    * runIn
    ******************************************************************************/
    void LocoIO::runIn(void)
    {
      AbstractLocoIO::runIn();
      handleRailRoadSignal();

      // Values registered for Analyzer
      if (!getAdapAnalogInputValue(IO::LocoIO::BrakePressure1, scaledBP1Measurement))
      {
        scaledBP1Measurement = 0U;
      }

      if (!getAdapAnalogInputValue(IO::LocoIO::BrakePressure2, scaledBP2Measurement))
      {
        scaledBP2Measurement = 0U;
      }

      bool tempTcoFb = false;

      if (getAdapDigitalInputValue(IO::LocoIO::TCOFB, &tempTcoFb))
      {
        tcoFBMeassurement = tempTcoFb ? 1U : 0U;
      }
      else
      {
        tcoFBMeassurement = 0U;
      }

    }

    /******************************************************************************
    * runOut
    ******************************************************************************/
    void LocoIO::runOut(void)
    {
      /*
      * Fetch the values of outputs defined in adaptation from respective components
      */

      bool tcoValue = true;

      // Fetch configuration if TCO external feedback shall be used.
      uint8_t tcoFeedbackSignalStatus = Config::instance().getTcoOrderAndTCOFb();

      // Is TCO order/TCO fb signal used?
      if (tcoFbAndOrderNotUsed != tcoFeedbackSignalStatus)
      {
        if (Kernel::AbstractModeControl::corePtr()->getStartUpPassed())
        {
          // Is Brake Test in Progress?
          if (Supv::AbstractBrake::corePtr()->getBrakeTestStatus() == Supv::BrakeTestStatusInProgress)
          {
            // Get value from TCO test 
            tcoValue = Supv::Brake::instance().getTcoTestApplied();
          }
          else
          {
            // Get value of TCO (Traction Cut Off), the getTcoValue() adaptation should return the same as the Emergency Brake output.
            tcoValue = Supv::Brake::instance().getTcoApplied();
          }
        }

        // TCO Order is active low
        static_cast<void>(setAdapDigitalOutputValue(TCO, !tcoValue));
      }
    
      AbstractLocoIO::runOut();
    }

    /******************************************************************************
    * getAdapDigitalInputValue
    ******************************************************************************/
    bool LocoIO::getAdapDigitalInputValue(const AdapDigitalInputs in, bool* const value) const
    {
      bool retVal = false;

      if (in < NumOfAdapDigitalInputs)
      {
        uint8_t inputNumber = static_cast<uint8_t>(in);
        inputNumber += static_cast<uint8_t>(NumOfCoreDigitalInputs);
        *value = getDigitalInputValue(inputNumber);
        retVal = true;
      }

      return retVal;
    }

    /******************************************************************************
    * getAdapDigitalOutputValue
    ******************************************************************************/
    bool LocoIO::getAdapDigitalOutputValue(const AdapDigitalOutputs out, bool* const value) const
    {
      bool retVal = false;

      if (out < NumOfAdapDigitalOutputs)
      {
        uint8_t outputNumber = static_cast<uint8_t>(out);
        outputNumber += static_cast<uint8_t>(NumOfCoreDigitalOutputs);
        *value = getDigitalOutputValue(outputNumber);
        retVal = true;
      }

      return retVal;
    }

    /******************************************************************************
    * setAdapDigitalOutputValue
    ******************************************************************************/
    bool LocoIO::setAdapDigitalOutputValue(const AdapDigitalOutputs out, const bool value)
    {
      bool retVal = false;

      if (out < NumOfAdapDigitalOutputs)
      {
        digitalOutputs[static_cast<uint8_t>(NumOfCoreDigitalOutputs) + static_cast<uint8_t>(out)].newValue = value;
        retVal = true;
      }

      return retVal;
    }

    /******************************************************************************
    * getAdapAnalogInputValue
    ******************************************************************************/
    bool LocoIO::getAdapAnalogInputValue(const AdapAnalogInputs in, uint16_t &value) const
    {
      bool success = false;

      if (static_cast<uint8_t>(in) < static_cast<uint8_t>(NumOfAdapAnalogInputs))
      {
        uint8_t inputNumber = static_cast<uint8_t>(in);
        inputNumber += static_cast<uint8_t>(NumOfCoreAnalogInputs);
        uint16_t rawValue;
        success = getAnalogInputRawValue(inputNumber, rawValue);

        if (success)
        {
          uint16_t minRangeOfScaledValue = 0U;
          uint16_t maxRangeOfScaledValue = 0U;
          uint16_t rangeOfScaledValue = 0U;

          switch (in)
          {
          case BrakePressure1:
            minRangeOfScaledValue = Config::instance().getRangeMinBP1();
            maxRangeOfScaledValue = Config::instance().getRangeMaxBP1();

            rangeOfScaledValue = (maxRangeOfScaledValue - minRangeOfScaledValue);
            break;

          case BrakePressure2:
            minRangeOfScaledValue = Config::instance().getRangeMinBP2();
            maxRangeOfScaledValue = Config::instance().getRangeMaxBP2();

            rangeOfScaledValue = maxRangeOfScaledValue - minRangeOfScaledValue;
            break;

          //putting the below case to remove lint error 788
          case NumOfAdapAnalogInputs:
          default:
            value = 0U;
            success = false;
            trace.write(1U, "Analog Input signal not yet defined");
            break;
          }

          if (success)
          {
            //Calculate and Set the Scaled value 
            if (rawValue < minAnalogRawValue)
            {
              value = minRangeOfScaledValue;
            }
            else if (rawValue > maxAnalogRawValue)
            {
              value = maxRangeOfScaledValue;
            }
            else
            {
              value = static_cast<uint16_t>(minRangeOfScaledValue +
                (((rawValue - minAnalogRawValue) * rangeOfScaledValue) / (maxAnalogRawValue - minAnalogRawValue)));
            }
          }
        }
      }

      return success;
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool LocoIO::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      const bool consoleFlag = AbstractLocoIO::consoleCall(argc, argv);

      bool retVal = false;
      char_t  buffer[512];

      const bool isTextMatching = ATC::isTextMatch(&argv[0][0], "LocoIO", sizeof("LocoIO"));

      // Check if Analog Inputs are available
      if (consoleFlag && isTextMatching)
      {
        const char_t* const toWrite = "AnalogInputs        Validity    RawValue       ScaledValue     ";

        // Write to console output
        ATC::AbstractConsole::corePtr()->writeWithNewline("-----------------------------------------------------------------------");
        ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);
        ATC::AbstractConsole::corePtr()->writeWithNewline("-----------------------------------------------------------------------");

        // Parse the input structure 
        for (uint8_t i = 0U; i < static_cast<uint8_t>(NumOfAdapAnalogInputs); i++)
        {
          uint16_t scaledAnalogValue;
          bool retValue = getAdapAnalogInputValue(static_cast<AdapAnalogInputs>(i), scaledAnalogValue);

          //lint -e{586} snprintf is needed here
          int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%-20s%-12s%-15d%-15d",
            analogInputs[i].analogSignalName,
            validStateStr(analogInputs[i].validityState),
            analogInputs[i].analogSignalvalue,
            (retValue == true) ? scaledAnalogValue : 0U);

          if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          }
        }
        retVal = true;
      }

      return retVal;
    }

    /******************************************************************************
    * getSleepingSignal
    ******************************************************************************/
    bool LocoIO::getSleepingSignal(void) const
    {
      bool returnSleepValue = false;
      // Get the Non Leading Loco Input value
      if (Config::instance().getNonLeadingLocoInput())
      {
        if (!getAdapDigitalInputValue(NONLEAD, &returnSleepValue))
        {
          // Report internal error
          ATC::aosHalt(__FILE__, __LINE__, "Internal error when reading Sleeping input");
        }

      }
      return returnSleepValue;
    }

    /******************************************************************************
     * handleRailRoadSignal
     ******************************************************************************/
    void LocoIO::handleRailRoadSignal(void)const
    {
      bool railModeValid = false;
      bool roadModeValid = false;

      // read states of Rail/Road Mode
      const bool railInputState = getAdapDigitalInputValue(RailM, &railModeValid);
      const bool roadInputState = getAdapDigitalInputValue(RoadM, &roadModeValid);

      // fetch configured locomotive type
      const LocoTypeAdap locoType = static_cast<LocoTypeAdap>(Config::instance().getLocoType());

      // fetch status of configured Rail/Road Input
      const bool isAvail = Config::instance().getRailRoadInputAvail();

      if ((HiRail == locoType) && isAvail)
      {

        // check if Rail/Road input has an invalid state
        if ((!railInputState) || (!roadInputState))
        {
          // Report Error - Trigger safetyHaltEvent
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorHiRailConfigInvalidState, __FILE__, __LINE__);
        }
        // check if Rail/Road input is in 'Road' or 'Transition' mode
        else if ((!railModeValid) || roadModeValid)
        {
          // Report Error - Trigger safetyHaltEvent
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorHiRailConfigRoadTransitionState, __FILE__, __LINE__);
        }
        else
        {
          // Do nothing
        }
      }
    }

    /******************************************************************************
    * getSbApplied
    ******************************************************************************/
    bool LocoIO::getSbApplied() const
    {
      bool sbApplied = AbstractLocoIO::getSbApplied();

      if (Supv::Brake::instance().getSbBrakeTestRequested())
      {
        sbApplied = true;
      }

      return sbApplied;
    }

    /******************************************************************************
    * validateViohVersion
    ******************************************************************************/
    bool LocoIO::validateViohVersion() const
    {
      const bool clientVersionOk = strncmp(&viohClientVersionString[0], &expectedViohClientVersion[0], viohVersionStringLength) == 0;
      const bool serverVersionOk = strncmp(&viohServerVersionString[0], &expectedViohServerVersion[0], viohVersionStringLength) == 0;

      return (clientVersionOk && serverVersionOk);
    }
  }
}
