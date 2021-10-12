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
*  AbstractLocoIO implements the core functionality of the component.
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
* 2016-04-21    lantback    Implemented corePtr()
* 2016-04-22    lantback    Added component type
* 2016-06-06    adgupta     Implementation of LocoIO functionality
* 2016-09-20    akushwah    remove the ifdef for vfwGetReferenceTime()
* 2016-09-26    spandita    Added functionality for console and direction controller
* 2016-09-29    spandita    Corrected the name from VIU to VOU with changes from magic number
                            to member functions
* 2016-09-29    spandita    Moved the function for loco direction to runin()
* 2016-10-03    spandita    Added statements in else case
* 2016-10-06    adgupta     Updated console call to print out correct sequence of Inputs
* 2016-10-18    arastogi    Moved function getLocovsTrainOrientation to Tsetup
* 2016-10-24    arastogi    Check both fwd are rev signal when deciding driver direction.
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include <vfw_string.h>

#include "atc_base.hpp"
#include "abstract_loco_io.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_log_handler.hpp"
#include "abstract_console.hpp"
#include "atc_math.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_brake.hpp"
#include "abstract_atp_application.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_config.hpp"
#include "dmi_event_codes.hpp"
#include <vfw_checkpoints.h>
#include "atp_application.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_array.hpp"
#include "abstract_supervise.hpp"
#include "abstract_analyzer_if.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_loco_io_event_ids.hpp"
#include "vio_types.h"

#ifdef WIN32
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#endif

/******************************************************************************
* LINT SUPPRESSIONS
******************************************************************************/
//lint -esym(586,snprintf) snprintf is needed here

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
  namespace IO
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractLocoIO::AbstractLocoIO(
      const uint8_t digitalInputsCount, const uint8_t digitalOutputsCount, const uint8_t analogInputsCounts, const uint8_t analogOutputsCounts) :
      // creating different set of objects for different type of events
      errorInputRegisterFail(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrInputRegisterFail, ATC::NoEB, DMICom::locoIOError, "VIOH input channel register Failed!")),
      errorInputRegisterResultFail(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrInputRegisterResultFail, ATC::NoEB, DMICom::locoIOError, "VIOH input channel register result Failed!")),
      errorInputRegisterResultIncorrect(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrInputRegisterResultIncorrect, ATC::NoEB, DMICom::locoIOError, "VIOH input channel register result incorrect!")),
      errorInputDeviceStateFail(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrInputDeviceStateFail, ATC::NoEB, DMICom::locoIOError, "VIOH input channel device state Failed!")),
      errorInputRevIdIncorrect(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrInputRevIdIncorrect, ATC::NoEB, DMICom::locoIOError, "VIOH input channel Revision Id Incorrect!")),
      errorInputHwConfIncorrect(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrInputHwConfIncorrect, ATC::NoEB, DMICom::locoIOError, "VIOH input channel hardware conf Incorrect!")),
      errorVIOCrossCompareFailed(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrVIOCrossCompareFailed, ATC::NoEB, DMICom::locoIOError, "VIOH Cross compare failed!")),
      errorOutputRegisterFail(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrOutputRegisterFail, ATC::NoEB, DMICom::locoIOError, "VIOH output channel register Failed! - VOURegister.")),
      errorOutputRegisterResultFail(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrOutputRegisterResultFail, ATC::NoEB, DMICom::locoIOError, "VIOH output channel register result Failed! - VOURegisterResult.")),
      errorOutputRegisterResultIncorrect(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrOutputRegisterResultIncorrect, ATC::NoEB, DMICom::locoIOError,
        "VIOH output channel register result Incorrect! - VOURegisterResult.")),
      errorOutputDeviceStateFail(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrOutputDeviceStateFail, ATC::NoEB, DMICom::locoIOError, "VIOH output channel device state Failed! - VOUGetDeviceState.")),
      errorOutputRevIdIncorrect(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrOutputRevIdIncorrect, ATC::NoEB, DMICom::locoIOError, "VIOH output channel Revision Id Incorrect! - VOUGetRevisionId.")),
      errorOutputHwConfIncorrect(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrOutputHwConfIncorrect, ATC::NoEB, DMICom::locoIOError, "VIOH output channel hardware conf Incorrect! - VOUGetHWConf.")),
      errorWritingVIOH(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrWritingVIOH, ATC::NoEB, DMICom::locoIOError, "Error Writing to Vital IO!")),
      vitalDriverReadFailure(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdFailureReadingVIOH, ATC::NoEB, DMICom::locoIOError, "Failure reading from Vital IO!")),
      vitalDriverReadError(ATC::Event::createLogEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrorReadingVIOH, 0x0U, "Error reading from Vital IO!")),
      vitalDriverFeedbackFailure(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdFeedbackFailureVIOH, ATC::NoEB, DMICom::locoIOError, "Failure reading feedback from Vital IO!")),
      vitalDriverFeedbackError(ATC::Event::createLogEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdFeedbackErrorVIOH, 0x0U, "Error reading feedback from Vital IO!")),
      errorSVDAlreadyTriggered(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrFeedbackSVDAlreadyTriggered, ATC::NoEB, DMICom::locoIOError,
        "GPIOTriggerSVD should not be registered here, Already Triggered !")),
      errorSVDTrigger(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrFeedbackSVDTrigger, ATC::NoEB, DMICom::locoIOError, "GPIOTriggerSVD was not successful!")),
      errorGPIORegisterSVDFail(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrFeedbackGPIORegisterSVDFail, ATC::NoEB, DMICom::locoIOError, "GPIO Register SVD result Failed! - GPIORegisterSVD.")),
      errorGPIORegisterSVDResultCallFail(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrFeedbackGPIORegisterSVDResultCallFail, ATC::NoEB, DMICom::locoIOError, 
        "GPIO Register SVD Result call Failed! - GPIORegisterSVDResult.")),
      errorCannotGetGPIORegisterSVDResult(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdErrFeedbackCannotGetGPIORegisterSVDResult, ATC::NoEB, DMICom::locoIOError,
        "Can't get GPIORegisterSVDResult ! - GPIORegisterSVDResult.")),
      errorDigitalOutputOutOfRange(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdDigitalOutputOutOfRange, ATC::NoEB, DMICom::locoIOError, "Digital Output to be set is out of range! ")),
      lcsReadyInactiveStandstill(ATC::Event::createStandstillEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdlcsReadyInactiveStandstill, ATC::NoSB, 0x0U, "Standstill when LCS ready Inactive")),
      noOrMoreThanOneATOSwitchModeSelected(ATC::Event::createStandstillEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdNoOrMoreThanOneATOSwitchModeSelected, ATC::NoSB, 0x0U, "No/More than one ATO Switch Mode Selected")),
      isolationSwitchNotInRunMode(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdIsolationSwitchNotInRunMode, ATC::NoEB, DMICom::isolationSwitchNotinRunMode, "Isolation Switch NOT in run mode! ")),
      ambiguousTravelDir(ATC::Event::createSBReqEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdAmbiguousTravelDir, ATC::NoSB, DMICom::ambiguousTravelDir, "Both Fwd and Reverse Direction active!")),
      standStillDrivingDirNotSet(ATC::Event::createStandstillEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdStandStillDrivingDirNotSet, ATC::NoSB, 0x0U, "Driving Direction not set")),
      vitalDriverFailure(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdVitalDriverFailure, ATC::NoEB, DMICom::vitalDriverFailure, "Vital Driver Health State Failure")),
      vitalDriverOutputFailure(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdVitalDriverOutputFailure, ATC::NoEB, DMICom::vitalDriverFailure, "Vital Driver Output Health State Failure")),
      vitalDriverOutputError(ATC::Event::createLogEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdVitalDriverOutputError, 0x0U, "Vital Driver Output Health State Error")),
      vitalDriverVersionMismatch(ATC::Event::createSafetyHaltEvent(atpLocoIOId, ATC::CoreContainer,
        eventIdVersionMismatchVIOH, ATC::NoEB, DMICom::vitalDriverFailure, "Vital Driver Version Mismatch:", true)),
      atoSwitchModeCounter(0U),
      buzzerType(BuzzerTypeNone),
      buzzerActive(false),
      buzzerCounter(0U),
      ambiguousTravelDirCounter(0U),
      triggerIsolNotInRunCounter(0U),
      numOfDigitalInputs(digitalInputsCount),
      numOfAnalogInputs(analogInputsCounts),
      numOfDigitalOutputs(digitalOutputsCount),
      numOfAnalogOutputs(analogOutputsCounts),
      locoTravelDirection(DirUndefined),
      atoModeSwitchPos(ATOModeUnknownPos),
      stateDigitalInputInit(InitCrossCompare),
      stateAnalogInputInit(RegisterRequest),
      stateDigitalOutputInit(RegisterRequest),
      stateSVDInit(RegisterRequest),
      currentSVDCounter(0U),
      nextCrossCompare(1U),  // Call CrossCompare in the beginning and after each hour passed
      displaySetupTimeout(displaySetupTimeoutValue),
      ioInitState(DigitalInputState),
      viuRevisionId(0U),
      vouRevisionId(0U),
      aiouRevisionId(0U),
      sbBrakeInfoForAnalyzer(0U),
      ebBrakeInfoForAnalyzer(0U),
      ATC::IOComponent(atpLocoIOId, "LocoIO", "LIO"),
      vitalDriverIsActiveOrder(false),
      vitalDriverValidityState(Invalid),
      vitalDriverTimestamp(0),
      dirMeassurement(0U),
      vitalDriverIsActiveFeedback(false),
      initMeasurementsDone(false)
    {

      if (coreLocoIOInstancePtr != NULL)
      {
        // Error to event handler
        ATC::aosHalt(__FILE__, __LINE__, "Loco IO Constructor already instantiated");
      }

      // Setup single instance pointer for core access
      coreLocoIOInstancePtr = this;

      //Acquire the VIOH Handle
      viohClientHandle = Kernel::AbstractATPApplication::corePtr()->getVIOHClientHandle();

      memset(&viohClientVersionString[0U], 0, sizeof(viohClientVersionString));
      memset(&viohServerVersionString[0U], 0, sizeof(viohServerVersionString));
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractLocoIO::init(void)
    {
      /*
      * The initialize function deals with the setting up of Loco IO to have all the
      * required setups ready for its functionality. This includes initializing
      * of input and output channels via Vital IO Handler to the Hardware Input and
      * output channels. Initialization of VFW and Vital IO Handler should already had
      * had been done in the ATP Main. Loco IO should get access to Vital IO handler
      * via the handle in ATP Main after initialization.
      */
      bool returnValue = false;
      
      //Initialize the list of Digital inputs with identifiers of all inputs to be used by application.
      digitalInputsList.listsize = numOfDigitalInputs;
      for (uint8_t count = 0x0U; count < digitalInputsList.listsize; count++)
      {
        digitalInputsList.list[count] = digitalInputs[count].signalIn;
      }

      for (uint8_t count = numOfDigitalInputs; count < maxDigitalInputs; count++)
      {
        digitalInputsList.list[count] = 0U;
      }

      //Initialize the list of Analog inputs with identifiers of all inputs to be used by application.
      analogInputsList.listsize = numOfAnalogInputs;
      for (uint8_t count = 0U; count < analogInputsList.listsize; count++)
      {
        analogInputsList.list[count] = analogInputs[count].analogSignalIn;
      }

      for (uint8_t count = numOfAnalogInputs; count < maxAnalogInputs; count++)
      {
        analogInputsList.list[count] = 0U;
      }

      //Initialize the list of Digital output with identifiers of all outputs to be used by application.
      digitalOutputsList.listsize = numOfDigitalOutputs;
      for (uint8_t count = 0x0U; count < digitalOutputsList.listsize; count++)
      {
        digitalOutputsList.list[count] = digitalOutputs[count].signalOut;
      }

      for (uint8_t count = numOfDigitalOutputs; count < maxDigitalOutputs; count++)
      {
        digitalOutputsList.list[count] = 0U;
      }

      //Initialize the list of Analog output with identifiers of all outputs to be used by application.
      analogOutputsList.listsize = numOfAnalogOutputs;
      for (uint8_t count = 0U; count < analogOutputsList.listsize; count++)
      {
        analogOutputsList.list[count] = analogOutputs[count].analogSignalOut;
      }

      for (uint8_t count = numOfAnalogOutputs; count < maxAnalogOutputs; count++)
      {
        analogOutputsList.list[count] = 0U;
      }

      //Start the Initialization of the IO Signals
      switch (ioInitState)
      {
      case DigitalInputState:
      {
        initDigitalInputs();
        break;
      }
      case DigitalOutputState:
      {
        initDigitalOutputs();
        break;
      }
      case AnalogInputState:
      {
        initAnalogInputs();
        break;
      }
      case GPIOState:
      {
        initGPIO();
        break;
      }
      case IOInitDone:
      {
        returnValue = true;
        break;
      }
      default:
        break;
      }

      if (!initMeasurementsDone)
      {
        const bool dirRegistration = ATC::AbstractAnalyzerIF::corePtr()->registerMeasurement(
          "direction", "direction(bit0=Fwd, bit1=Rev)", "dir", 0U, 255U, &dirMeassurement);

        if (!dirRegistration)
        {
          writeToLog(ATC::BriefLog, "Register measurement failed for analyzer", __FILE__, __LINE__);
        }

        initMeasurementsDone = true;
      }

      return returnValue;
    }

    /******************************************************************************
    * runIn
    ******************************************************************************/
    void AbstractLocoIO::runIn(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "LIO_runIn");

      if (Kernel::AbstractModeControl::corePtr()->getStartUpPassed())
      {
        readInputs();
        processIsolationSwitch();
      }
      else
      {
        atoModeSwitchPos = ATOModeManualPos;
        locoTravelDirection = DirNone;
        readInputsBeforeStartUpPassed();
        setVitalDriverIsActiveOrder(true);
      }

      readOutputFeedback();

      superviseVitalDriverHealthState();

      bool lcsRdyValue;
      bool lcsRdyValueValid;

      lcsRdyValueValid = getCoreDigitalInputValue(LCSRdy, &lcsRdyValue);

      if (lcsRdyValueValid && (!lcsRdyValue))
      {
        //Standstill
        ATC::AbstractEventHandler::corePtr()->reportEvent(lcsReadyInactiveStandstill, __FILE__, __LINE__);
      }

      if (currentSVDCounter == 0U)
      {
        //Trigger the SVD
        const VIOHnames::VIOH_clientResultType returnValue = viohClientHandle->GPIOTriggerSVD(static_cast<bool_t>(vitalDriverIsActiveOrder));

        trace.write(ATC::veryDetailedTrace, "GPIOTriggerSVD", static_cast<bool_t>(vitalDriverIsActiveOrder));


        if (VIOHnames::enCRT_OK != returnValue)
        {
          // Report Error
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorSVDTrigger, __FILE__, __LINE__);
        }
      }

      currentSVDCounter = (currentSVDCounter + 1U) % MaxCountReqForTriggeringSVD;

      // Update the diagnostic measurement value for HW direction
      bool fwd;
      bool rev;

      const bool fwdReadOk = getCoreDigitalInputValue(Fwd, &fwd);
      const bool revReadOk = getCoreDigitalInputValue(Rev, &rev);

      if (fwdReadOk && revReadOk)
      {
        dirMeassurement = fwd ? 0x1U : 0x0U;
        dirMeassurement |= rev ? 0x2U : 0x0U;
      }
      else
      {
        dirMeassurement = 0x00U;
      }

      // Run CrossCompare every hour
      runVIOCrossCompare();
    }

    /******************************************************************************
    * runOut
    ******************************************************************************/
    void AbstractLocoIO::runOut(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "LIO_runOut");

      //The runOut function is responsible for fetching the updated output values from 
      //different modules and writing them to VIOH.
      bool eb1Value = true;
      bool eb2Value = true;
      bool ebApplied = true;
      bool isATPOk = false;
      bool sbApplied = true;

      if (Kernel::AbstractModeControl::corePtr()->getStartUpPassed())
      {
        isATPOk = Kernel::AbstractModeControl::corePtr()->getATPOKStatus();
        sbApplied = getSbApplied();
        ebApplied = Supv::AbstractBrake::corePtr()->getEbApplied();

        //Is Brake Test in Progress?
        if ((Supv::AbstractBrake::corePtr()->getBrakeTestStatus() == Supv::BrakeTestStatusInProgress) && (!ebApplied))
        {
          //Set value of Emergency brake outputs
          eb1Value = Supv::AbstractBrake::corePtr()->getEb1TestApplied();
          eb2Value = Supv::AbstractBrake::corePtr()->getEb2TestApplied();
        }
        else
        {
          eb1Value = ebApplied;
          eb2Value = ebApplied;
        }
      }

      //Buzzer status
      manageBuzzerStatus();

      //Set value of Emergency brake outputs
      setCoreDigitalOutputValue(EmerBrake1, !eb1Value);
      setCoreDigitalOutputValue(EmerBrake2, !eb2Value);

      //Set value of Emergency brake output
      setCoreDigitalOutputValue(EmerBrakeApplied, !(eb1Value || eb2Value));

      // Service Brake output is Active Low!
      //set value for service brake output
      setCoreDigitalOutputValue(ServiceBrake, !sbApplied);

      //set value for ATPoK output signal
      setCoreDigitalOutputValue(ATPOk, isATPOk);
      //set value for Lamp output signal
      setCoreDigitalOutputValue(Lamp, Kernel::AbstractModeControl::corePtr()->getLampStatus());
      setCoreDigitalOutputValue(PowerOFF, Kernel::AbstractModeControl::corePtr()->getPowerOffValue());

      //set value for Buzzer output signal
      setCoreDigitalOutputValue(Buzzer, buzzerActive);

      if (vitalDriverIsActiveOrder)
      {
        // All the core output values are updated now. Write these outputs.
        writeOutputs();
      }

      //Report current status of emergency and service brake to Analyzer
      if (ebApplied)
      {
        ebBrakeInfoForAnalyzer = 1U;
      }
      else
      {
        ebBrakeInfoForAnalyzer = 0U;
      }

      if (sbApplied)
      {
        sbBrakeInfoForAnalyzer = 1U;
      }
      else
      {
        sbBrakeInfoForAnalyzer = 0U;
      }
    }

    /******************************************************************************
    * getSbApplied
    ******************************************************************************/
    bool AbstractLocoIO::getSbApplied() const
    {
      return Supv::AbstractBrake::corePtr()->getSbApplied();
    }

    /******************************************************************************
    * runCrossCompare
    ******************************************************************************/
    void AbstractLocoIO::runVIOCrossCompare(void)
    {
      if (nextCrossCompare == 0U)
      {
        // Call every hour (1170 GSP2_SIR98)
        nextCrossCompare = 3600U * (1000U / Kernel::ATPApplication::atpAppCycleTime);

        const VIOHnames::VIOH_clientResultType r = viohClientHandle->CrossCompare();

        if ((r == VIOHnames::enCRT_VITAL_MISMATCH)) // lint: lint cannot find enCRT_VITAL_MISMATCH
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorVIOCrossCompareFailed, __FILE__, __LINE__);
        }
      }
      else
      {
        nextCrossCompare--;
      }
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractLocoIO::initCrossCompare() const
    {
      uint8_t i;

      // Add all digital inputs to cross compare
      for (i = 0U; i < numOfDigitalInputs; ++i)
      {
        digitalInputs[i].initCrossCompare();
      }

      // Add all analog inputs to cross compare
      for (i = 0U; i < numOfAnalogInputs; ++i)
      {
        analogInputs[i].initCrossCompare();
      }

      // Add all digital outputs to cross compare
      for (i = 0U; i < numOfDigitalOutputs; ++i)
      {
        digitalOutputs[i].initCrossCompare();
      }

      // Add all analog outputs to cross compare
      for (i = 0U; i < numOfAnalogOutputs; ++i)
      {
        analogOutputs[i].initCrossCompare();
      }

      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorInputRegisterFail));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorInputRegisterResultFail));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorInputRegisterResultIncorrect));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorInputDeviceStateFail));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorInputRevIdIncorrect));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorInputHwConfIncorrect));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorVIOCrossCompareFailed));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorOutputRegisterFail));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorOutputRegisterResultFail));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorOutputRegisterResultIncorrect));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorOutputDeviceStateFail));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorOutputRevIdIncorrect));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorOutputHwConfIncorrect));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorWritingVIOH));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&vitalDriverReadFailure));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&vitalDriverReadError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&vitalDriverFeedbackFailure));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&vitalDriverFeedbackError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorSVDAlreadyTriggered));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorSVDTrigger));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorGPIORegisterSVDFail));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorGPIORegisterSVDResultCallFail));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorCannotGetGPIORegisterSVDResult));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorDigitalOutputOutOfRange));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&lcsReadyInactiveStandstill));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&noOrMoreThanOneATOSwitchModeSelected));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&isolationSwitchNotInRunMode));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&ambiguousTravelDir));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&standStillDrivingDirNotSet));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&vitalDriverFailure));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&vitalDriverOutputFailure));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&vitalDriverOutputError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&vitalDriverVersionMismatch));

      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&atoSwitchModeCounter));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<BuzzerType>(&buzzerType));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&buzzerActive));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&buzzerCounter));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&ambiguousTravelDirCounter));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&triggerIsolNotInRunCounter));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&numOfDigitalInputs));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&numOfAnalogInputs));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&numOfDigitalOutputs));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&numOfAnalogOutputs));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<TravelDir>(&locoTravelDirection));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ATOModeSwitchPos>(&atoModeSwitchPos));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&currentSVDCounter));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&nextCrossCompare));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&displaySetupTimeout));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<IOInitState>(&ioInitState));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&viuRevisionId));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&vouRevisionId));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&aiouRevisionId));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ValidityState>(&vitalDriverValidityState));

      crossCompare->addCrossCompareData(new Support::CrossCompareInt64(&vitalDriverTimestamp));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&vitalDriverIsActiveFeedback));

      crossCompare->addCrossCompareData(new Support::CrossCompareArray<char_t>(&viohClientVersionString[0], viohVersionStringLength));
      crossCompare->addCrossCompareData(new Support::CrossCompareArray<char_t>(&viohServerVersionString[0], viohVersionStringLength));
    }

    /******************************************************************************
    * getInputValue
    ******************************************************************************/
    bool AbstractLocoIO::getDigitalInputValue(const uint8_t in) const
    {
      return digitalInputs[in].value;
    }

    /******************************************************************************
    * getDigitalOutputValue
    ******************************************************************************/
    bool AbstractLocoIO::getDigitalOutputValue(const uint8_t out) const
    {
      return digitalOutputs[out].currentValue;
    }

    /******************************************************************************
    * setCoreDigitalOutputValue
    ******************************************************************************/
    void AbstractLocoIO::setCoreDigitalOutputValue(const CoreDigitalOutputs out, const bool value)
    {
      if (out < NumOfCoreDigitalOutputs)
      {
        digitalOutputs[static_cast<uint8_t>(out)].newValue = value;
      }
      else
      {
        //Issue a SafetyHalt when digital output is out of range
        ATC::AbstractEventHandler::corePtr()->reportEvent(errorDigitalOutputOutOfRange, __FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * getCoreDigitalInputValue
    ******************************************************************************/
    bool AbstractLocoIO::getCoreDigitalInputValue(const CoreDigitalInputs in, bool* const value) const
    {
      bool retVal = false;
      if (in < NumOfCoreDigitalInputs)
      {
        *value = getDigitalInputValue(static_cast<uint8_t>(in));
        retVal = true;
      }

      return retVal;
    }

    /******************************************************************************
    * getCoreAnalogInputValue
    ******************************************************************************/
    bool AbstractLocoIO::getCoreAnalogInputValue(const CoreAnalogInputs in, uint16_t* const value) const
    {
      bool success = false;

      if (in < NumOfCoreAnalogInputs)
      {
        success = getAnalogInputRawValue(static_cast<uint8_t>(in), *value);
      }

      return success;
    }

    /******************************************************************************
    * getAnalogInputRawValue
    ******************************************************************************/
    bool AbstractLocoIO::getAnalogInputRawValue(const uint8_t in, uint16_t& value) const
    {
      bool success = false;

      if (in < maxAnalogInputs)
      {
        value = analogInputs[in].analogSignalvalue;
        success = true;
      }

      return success;
    }

    /******************************************************************************
    * getCoreDigitalOutputValue
    ******************************************************************************/
    bool AbstractLocoIO::getCoreDigitalOutputValue(const CoreDigitalOutputs out, bool* const value) const
    {
      bool retVal = false;
      if (out < NumOfCoreDigitalOutputs)
      {
        *value = getDigitalOutputValue(static_cast<uint8_t>(out));
        retVal = true;
      }

      return retVal;
    }

    /******************************************************************************
    * writeOutputs
    ******************************************************************************/
    void AbstractLocoIO::writeOutputs(void)
    {
      uint8_t outputsCount;

      for (outputsCount = 0U; outputsCount < numOfDigitalOutputs; outputsCount++)
      {
        DigitalOutput& digitalOutput = digitalOutputs[outputsCount];
        //Got updated value?
        if ((digitalOutput.newValue != digitalOutput.currentValue) || ((digitalOutput.validityState == Invalid) &&
          (digitalOutput.timestamp == 0)))
        {
          //Set the new value
          const VIOHnames::VIOH_clientResultType retVal = viohClientHandle->VOUSetOutput(
            digitalOutput.signalOut, digitalOutput.newValue, static_cast<bool_t>(true));  //sync is true for phase 1

          trace.write(ATC::briefMessageTrace, "Set: ", static_cast<uint32_t>(digitalOutput.signalOut), false);
          trace.write(ATC::briefMessageTrace, " value: ", digitalOutput.newValue);

          if (VIOHnames::enCRT_OK != retVal)
          {
            //Cannot set the output
            digitalOutput.validityState = Invalid;
            digitalOutput.timestamp = 0; //Invalidating the time too

            //Send event to event handler
            ATC::AbstractEventHandler::corePtr()->reportEvent(errorWritingVIOH, __FILE__, __LINE__);
            break;
          }
          else
          {
            //update the output value as soon as it is sent successfully.
            digitalOutput.currentValue = digitalOutput.newValue;

            digitalOutput.validityState = ValidationPending;
            digitalOutput.timestamp = vfwGetReferenceTime();

            ATC::AbstractLogHandler::corePtr()->logRU(
              ATC::AbstractLogHandler::Ifc_IO, ATC::AbstractLogHandler::Ifc_Out, outputsCount, digitalOutput.newValue);
          }
        }//end of if(outputs)
      }//end of for(outputsCount)
    }

    /******************************************************************************
    * getAllVitalOutputDeactivated
    ******************************************************************************/
    bool AbstractLocoIO::getAllVitalOutputDeactivated() const
    {
      uint8_t outputsCount;

      // Check if Vital driver is deactivated...
      bool allVitalOutputDeactivated = !vitalDriverIsActiveFeedback;

      for (outputsCount = 0U; outputsCount < numOfDigitalOutputs; ++outputsCount)
      {
        const DigitalOutput& digitalOutput = digitalOutputs[outputsCount];

        if (digitalOutput.isVital)
        {
          allVitalOutputDeactivated = allVitalOutputDeactivated && (!digitalOutput.feedbackValue);
        }
      }//end of for(outputsCount)

      return allVitalOutputDeactivated;
    }

    /******************************************************************************
    * getViohClientVersionString
    ******************************************************************************/
    const char_t * const AbstractLocoIO::getViohClientVersionString() const
    {
      return viohClientVersionString;
    }

    /******************************************************************************
    * getViohServerVersionString
    ******************************************************************************/
    const char_t * const AbstractLocoIO::getViohServerVersionString() const
    {
      return viohServerVersionString;
    }

    /******************************************************************************
    * readInputs
    ******************************************************************************/
    void AbstractLocoIO::readInputs(void)
    {
      //The readDigitalInputs function iterates over all inputs (both core and adaptation inputs)
      //and fetches their values from VIOHinterface. If the read is OK it updates the
      //values in the LocoIO data structures.

      uint8_t inputsCount;
      //update Digital Input values for all Vital IO
      for (inputsCount = 0U; inputsCount < numOfDigitalInputs; inputsCount++)
      {
        if (!(readDigitalInput(static_cast<CoreDigitalInputs>(inputsCount))))
        {
        //Exit after 1st error itself.
          break;
        }
      }

       //update Analog Input values for all Vital IO
      for (inputsCount= 0U; inputsCount<numOfAnalogInputs;inputsCount++)
      {
        if(!(readAnalogInput(static_cast<CoreAnalogInputs>(inputsCount))))
        {
         //Exit after 1st error itself.
          break;
        }
      }//End of for(inputsCount)

      //calculate current Driving Direction
      calculateDrivingDirection();

      //calculate ATO Mode Switch Position
      calculateATOModeSwitchPosition();

    }

    /******************************************************************************
    * readDigitalInput
    ******************************************************************************/
    bool AbstractLocoIO::readDigitalInput(const CoreDigitalInputs inputIndex)
    {
      bool_t state = ATC::falseVfw;
      bool isReadDigitalInput = true;
      VIOHnames::VIOH_healthStateType healthState = VIOHnames::VIOResOK;
      const VIOHnames::VIOH_clientResultType retVal = viohClientHandle->VIUGetState(digitalInputs[inputIndex].signalIn, &state, &healthState);

      if ((VIOHnames::VIOResOK == healthState) && (VIOHnames::enCRT_OK == retVal))
      {
        const bool digitalValue = ((ATC::falseVfw != state) ? true : false);
        const bool logToRU =
          (digitalInputs[inputIndex].validityState != Valid) ||
          (digitalInputs[inputIndex].value != digitalValue); //lint !e731 Comparing boolean variables is ok

        //return is successful. Read OK! Update local structure.
        digitalInputs[inputIndex].value = digitalValue;
        digitalInputs[inputIndex].validityState = Valid;
        digitalInputs[inputIndex].warningCount = 0U;
        trace.write(ATC::veryDetailedTrace, "DigitalInput signal :",
          static_cast<uint32_t>(digitalInputs[inputIndex].signalIn));
        trace.write(ATC::veryDetailedTrace, " Is DigitalInput signal valid status:",
          static_cast<uint32_t>(digitalInputs[inputIndex].validityState));

        if (logToRU)
        {
          ATC::AbstractLogHandler::corePtr()->logRU(
            ATC::AbstractLogHandler::Ifc_IO, ATC::AbstractLogHandler::Ifc_In, static_cast<uint8_t>(inputIndex), digitalValue);
        }
      }
      else if ((VIOHnames::enCRT_OK == retVal) && (0U != (healthState & VIOHnames::VIORes_CHECKVERSION)))
      {
        ATC::aosHalt(__FILE__, __LINE__, "Version mismatch for VIU");
      }
      else if ((VIOHnames::VIORes_A_B_MM_SHORT == healthState) && (VIOHnames::enCRT_OK == retVal) &&
        (digitalInputs[inputIndex].warningCount < maxNumAllowedWarnings))
      {
        digitalInputs[inputIndex].warningCount++;

        char_t buffer[80];
        const int32_t ret = snprintf(&buffer[0], sizeof(buffer),
          "Warning: got health state %u for digital input %u", healthState, digitalInputs[inputIndex].signalIn);
        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, &buffer[0], "LIO", __FILE__, __LINE__);
        }
      }
      else
      {
        //VIO function failed!

        digitalInputs[inputIndex].validityState = Invalid;
        digitalInputs[inputIndex].warningCount = 0U;
        trace.write(0U, "DigitalInput signal :", static_cast<uint32_t>(digitalInputs[inputIndex].signalIn));
        trace.write(0U, "Is DigitalInput signal valid status:", static_cast<uint32_t>(digitalInputs[inputIndex].validityState));
        trace.write(0U, "DigitalInput signal healthstate:", static_cast<uint32_t>(healthState));
        trace.write(0U, "DigitalInput signal retVal:", static_cast<uint32_t>(retVal));
        //Send event to event handler
        if (digitalInputs[inputIndex].isVital)
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverReadFailure, __FILE__, __LINE__);
        }
        else
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverReadError, __FILE__, __LINE__);
        }

        //return false after getting error
        isReadDigitalInput = false;
      }
      return isReadDigitalInput;
    }


    /******************************************************************************
    * readAnalogInput
    ******************************************************************************/
    bool AbstractLocoIO::readAnalogInput(const CoreAnalogInputs inputIndex)
    {
        bool isReadAnalogInput = true;
        uint16_t analogValue;
        VIOHnames::VIOH_healthStateType healthState = VIOHnames::VIOResOK;

        const VIOHnames::VIOH_clientResultType retVal = viohClientHandle->AIOUGetState(
            analogInputs[inputIndex].analogSignalIn, &analogValue, &healthState);
        const uint16_t ruLogValueDiff = ATC::AbstractConfigBase::basePtr()->getRuLogValueDiff();

        if ((VIOHnames::VIOResOK == healthState) && (VIOHnames::enCRT_OK == retVal))
        {
            const uint16_t valueDiff = ATC::ATCMath::absDiff(analogInputs[inputIndex].analogSignalvalue, analogValue);
            const bool logToRU = (analogInputs[inputIndex].validityState != Valid) || (valueDiff > ruLogValueDiff);
            //return is successful. Read OK! Update local structure.
            analogInputs[inputIndex].analogSignalvalue = analogValue;
            analogInputs[inputIndex].validityState = Valid;
            analogInputs[inputIndex].warningCount = 0U;
            trace.write(ATC::veryDetailedTrace, "Analog Input signal :", static_cast<uint32_t>(analogInputs[inputIndex].analogSignalIn));
            trace.write(ATC::veryDetailedTrace, "Analog Input signal Value:", static_cast<uint32_t>(analogInputs[inputIndex].analogSignalvalue));
            trace.write(ATC::veryDetailedTrace, "Analog Input signal Validity:", static_cast<uint32_t>(analogInputs[inputIndex].validityState));
            trace.write(ATC::veryDetailedTrace, "Analog Input healthState:", static_cast<uint32_t>(healthState));
            trace.write(ATC::veryDetailedTrace, "Analog Input retVal:", static_cast<uint32_t>(retVal));

            if (logToRU)
            {
                ATC::AbstractLogHandler::corePtr()->logRU(
                    ATC::AbstractLogHandler::Ifc_IO, ATC::AbstractLogHandler::Ifc_In, static_cast<uint8_t>(inputIndex), analogValue);
            }
        }
        else if ((VIOHnames::enCRT_OK == retVal) && (0U != (healthState & VIOHnames::VIORes_CHECKVERSION)))
        {
          ATC::aosHalt(__FILE__, __LINE__, "Version mismatch for AIOU");
        }
        else if (((VIOHnames::VIORes_A_B_MM_SHORT == healthState) || (VIOHnames::VIORes_AIOU_CHANGING == healthState)) &&
            (VIOHnames::enCRT_OK == retVal) && (analogInputs[inputIndex].warningCount < maxNumAllowedWarnings))
        {
          analogInputs[inputIndex].warningCount++;

          char_t buffer[80];
          const int32_t ret = snprintf(&buffer[0], sizeof(buffer),
              "Warning: got health state %u for analog input signal %u", healthState, analogInputs[inputIndex].analogSignalIn);
          if ((ret != -1) && (static_cast<size_t>(ret) < sizeof(buffer)))
          {
              ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, &buffer[0], "LIO", __FILE__, __LINE__);
          }
        }
        else
        {

          trace.write(0U, "Analog Input signal :", static_cast<uint32_t>(analogInputs[inputIndex].analogSignalIn));
          trace.write(0U, "Analog Input signal validity:", static_cast<uint32_t>(analogInputs[inputIndex].validityState));
          trace.write(0U, "Analog Input signal old value:", static_cast<uint32_t>(analogInputs[inputIndex].analogSignalvalue));
          trace.write(0U, "Analog Input signal new value:", static_cast<uint32_t>(analogValue));
          trace.write(0U, "Analog Input healthState:", static_cast<uint32_t>(healthState));
          trace.write(0U, "Analog Input retVal:", static_cast<uint32_t>(retVal));
          analogInputs[inputIndex].validityState = Invalid;
          analogInputs[inputIndex].warningCount = 0U;
          //Send event to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverReadFailure, __FILE__, __LINE__);

            //Exit after 1st error itself.
            isReadAnalogInput = false;
        }
        return isReadAnalogInput;
    }

    /******************************************************************************
    * readInputsBeforeStartUpPassed
    ******************************************************************************/
    void AbstractLocoIO::readInputsBeforeStartUpPassed(void)
    {
      bool isReadLCSRdy = readDigitalInput(LCSRdy);
      bool isReadOFFIn = readDigitalInput(OFFIn);
      if (!(isReadOFFIn || isReadLCSRdy))
      {
        writeToLog(ATC::BriefLog, "readDigitalInput returns:False", __FILE__, __LINE__);

      }
    }

    /******************************************************************************
    * readOutputFeedback
    ******************************************************************************/
    void AbstractLocoIO::readOutputFeedback(void)
    {
      uint8_t outputsCount;
      const int64_t currTime = vfwGetReferenceTime();

      VIOHnames::VIOH_healthStateType healthState = VIOHnames::VIOResOK;

      //update local values for all outputs
      for (outputsCount = 0U; outputsCount < numOfDigitalOutputs; outputsCount++)
      {
        // Read the feed back of the outputs written in the previous cycle.
        bool_t vioRelayFeedback = ATC::falseVfw;

        DigitalOutput& digitalOutput = digitalOutputs[outputsCount];

        const VIOHnames::VIOH_clientResultType retVal = viohClientHandle->VOUGetState(digitalOutput.signalOut, &vioRelayFeedback, &healthState);

        if (retVal != VIOHnames::enCRT_OK)
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverOutputFailure, __FILE__, __LINE__);
        }

        digitalOutput.feedbackValue = (ATC::falseVfw != vioRelayFeedback);  // convert from bool_t to bool

        const uint32_t validStates = VIOHnames::VIORes_OK | VIOHnames::VIORes_A_B_MM_SHORT | VIOHnames::VIORes_FEEDBACK_SHORT |
          VIOHnames::VIORes_ANTIVALENCE_SHORT | VIOHnames::VIORes_PSVDU_TIMEOUT | VIOHnames::VIORes_FEEDBACK_LONG;

        bool healthStateError = false;

        if ((healthState & (~validStates)) == 0U)
        {
          // Individually for each used output on the vital output unit(VOU), the ATP shall every cycle supervise that the
          // health state provided by the GSP2 is no other than
          //  VIORes_OK, or
          //  VIORes_A_B_MM_SHORT, or
          //  VIORes_FEEDBACK_SHORT, or
          //  VIORes_ANTIVALENCE_SHORT, or
          //  VIORes_PSVDU_TIMEOUT
          if ((healthState & VIOHnames::VIORes_FEEDBACK_LONG) != 0U)
          {
            // Exception: When the Vital Driver is de-activated (safe state), e.g. during test of the Vital Driver
            // or due to a safe reaction using the Vital Driver, all previously energized relays drop to de-energized
            // state and due to this VIORes_FEEDBACK_LONG will be reported while the Vital Driver is de-activated.
            // This effect needs to be considered when evaluating the health state.
            healthStateError = (vitalDriverValidityState == Valid) && vitalDriverIsActiveOrder;
          }
        }
        else
        {
          if (0U != (healthState & VIOHnames::VIORes_CHECKVERSION))
          {
            ATC::aosHalt(__FILE__, __LINE__, "Version mismatch for VOU");
          }
          healthStateError = true;
        }

        if (healthStateError)
        {
          char_t  buffer[512];
          const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "Digital output %d, health state: %d", digitalOutput.signalOut, healthState);
          if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
          {
            writeToLog(ATC::BriefLog, &buffer[0], __FILE__, __LINE__);
          }

          if (digitalOutput.isVital)
          {
            // The ATP shall react safety halt event if the received health state is different from the listed health states above.
            ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverOutputFailure, __FILE__, __LINE__);
          }
          else
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverOutputError, __FILE__, __LINE__);
          }
        }

        if (!vitalDriverIsActiveOrder)
        {
          // Exception: When the Vital Driver is de-activated (safe state), e.g. during test of the Vital Driver or due to a safe reaction
          // using the Vital Driver, all previously energised relays drop to de-energised state. This effect needs to be considered when 
          // evaluating the internal relay feedback
          digitalOutput.validityState = Invalid;
          digitalOutput.timestamp = 0; //Invalidating the time too
        }
        else if ((digitalOutput.validityState == ValidationPending) &&  // Verify if the Outputs are ready to be used.
          ((currTime - digitalOutput.timestamp) < outputLagTime))
        {
          // It shall ignore the internal Vital Driver relay feedback for 500 ms and thereafter check that the feedback corresponds to the
          // given order.
          trace.write(ATC::veryDetailedTrace, "Wait for lag time to pass to validate Output Number:", static_cast<uint32_t>(outputsCount));
        }
        else if (digitalOutput.feedbackValue == digitalOutput.newValue) //lint !e731 Comparing boolean variables is ok
        {
          //feedback value same as value to be updated.
          digitalOutput.validityState = Valid;
        }
        // Lag time complete, relays should be set by now.
        else
        {
          //Output couldn't be set!
          digitalOutput.validityState = Invalid;
          digitalOutput.timestamp = 0; //Invalidating the time too
          //write to trace
          trace.write(ATC::briefTrace, "Output Signal :", static_cast<uint32_t>(digitalOutput.signalOut));
          trace.write(ATC::briefTrace, "Status of output signal :", static_cast<uint32_t>(digitalOutput.validityState));

          char_t  buffer[512];
          const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "Digital output %d, Feedback error: %d != %d", digitalOutput.signalOut, 
            static_cast<int32_t>(digitalOutput.newValue), static_cast<int32_t>(digitalOutput.feedbackValue));
          if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
          {
            writeToLog(ATC::BriefLog, &buffer[0], __FILE__, __LINE__);
          }

          if (digitalOutput.isVital)
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverFeedbackFailure, __FILE__, __LINE__);
          }
          else
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverFeedbackError, __FILE__, __LINE__);
          }
        }
      }
    }

    /******************************************************************************
    * superviseVitalDriverHealthState
    ******************************************************************************/
    void AbstractLocoIO::superviseVitalDriverHealthState(void)
    {
      const uint32_t validStates = VIOHnames::VIORes_OK | VIOHnames::VIORes_FEEDBACK_SHORT | VIOHnames::VIORes_ANTIVALENCE_SHORT |
        VIOHnames::VIORes_BUSY | VIOHnames::VIORes_PSVDU_TIMEOUT;

      bool_t isActive = ATC::falseVfw;
      bool_t feedback = ATC::falseVfw;
      VIOHnames::VIOH_healthStateType healthState = VIOHnames::VIOResOK;

      const VIOHnames::VIOH_clientResultType retVal = viohClientHandle->GPIOGetSVDState(&isActive, &feedback, &healthState);


      if ((VIOHnames::enCRT_OK == retVal) && (0U != (healthState & VIOHnames::VIORes_CHECKVERSION)))
      {
        ATC::aosHalt(__FILE__, __LINE__, "Version mismatch for GPIO");
      }
      else if (retVal == VIOHnames::enCRT_OK)
      {
        vitalDriverIsActiveFeedback = (feedback != ATC::falseVfw);
      }
      else if ((vitalDriverValidityState == ValidationPending) && (retVal == VIOHnames::enCRT_UNKNOWN_INIT))
      {
        // Can occur during operation temporarily similar to VIORes_A_B_MM_SHORT if different values are read on A side and B side
      }
      else
      {
        writeToLog(ATC::BriefLog, "GPIOGetSVDState returns:", static_cast<int32_t>(retVal), __FILE__, __LINE__);

        vitalDriverValidityState = Invalid;
        ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverFailure, __FILE__, __LINE__);
      }

      switch (vitalDriverValidityState)
      {
        case ValidationPending:
        {
          const int64_t currTime = vfwGetReferenceTime();

          if ((currTime - vitalDriverTimestamp) >= outputLagTime)
          {
            vitalDriverValidityState = Valid;
          }

          if ((healthState & (~validStates)) == 0U)
          {
            //  VIORes_OK, or
            //  VIORes_FEEDBACK_SHORT, or
            //  VIORes_ANTIVALENCE_SHORT, or
            //  VIORes_BUSY
            // Exception: In case VIORes_PSVDU_TIMEOUT is received during intended deactivation of the Vital Driver
            // no reaction shall apply.In all other cases this health state shall result in a failure reaction.
          }
          else
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverFailure, __FILE__, __LINE__);
            vitalDriverValidityState = Invalid;
          }
          break;
        }

        case Valid:
        {
          // Does our order match the feedback?
          if ((vitalDriverIsActiveOrder && vitalDriverIsActiveFeedback) || ((!vitalDriverIsActiveOrder) && (!vitalDriverIsActiveFeedback)))
          {
            vitalDriverValidityState = Valid;
            vitalDriverTimestamp = 0;

            if ((healthState & (~validStates)) == 0U)
            {
              //  VIORes_OK, or
              //  VIORes_FEEDBACK_SHORT, or
              //  VIORes_ANTIVALENCE_SHORT, or
              //  VIORes_BUSY
              if (vitalDriverIsActiveOrder && ((healthState & VIOHnames::VIORes_PSVDU_TIMEOUT) != 0U))
              {
                // Exception: In case VIORes_PSVDU_TIMEOUT is received during intended deactivation of the Vital Driver
                // no reaction shall apply.In all other cases this health state shall result in a failure reaction.
                ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverFailure, __FILE__, __LINE__);
                vitalDriverValidityState = Invalid;
              }
            }
            else
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverFailure, __FILE__, __LINE__);
              vitalDriverValidityState = Invalid;
            }
          }
          else
          {
            // Feedback does not match order
            ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverFailure, __FILE__, __LINE__);
            vitalDriverValidityState = Invalid;
          }
          break;
        }

        case Invalid:
          // Fall through
        default:
          vitalDriverValidityState = Invalid;
          ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverFailure, __FILE__, __LINE__);
          break;
      }
    }
    
    /******************************************************************************
    * processIsolationSwitch
    ******************************************************************************/
    void AbstractLocoIO::processIsolationSwitch(void)
    {
      bool isolationA;
      bool isolationB;

      bool isoAReadOk = getCoreDigitalInputValue(IsolationA, &isolationA); //Active High Input
      bool isoBReadOk = getCoreDigitalInputValue(IsolationB, &isolationB); //Active Low Input

      if (isoAReadOk && isoBReadOk)
      {
        if (isolationA && (!isolationB))
        {
          // Isolation Switch is in 'Run Mode'. OK!
          triggerIsolNotInRunCounter = 0U;
        }
        else
        {
          if (triggerIsolNotInRunCounter >= triggerIsolNotInRunVal)
          {
            // Isolation Switch is not in 'Run Mode'. Safety Halt!
            ATC::AbstractEventHandler::corePtr()->reportEvent(isolationSwitchNotInRunMode, __FILE__, __LINE__);

            // Brake Test becomes mandatory after recovering from Safety Halt
            if (!AbstractConfig::corePtr()->setBrakeTestReason(brakeTestReasonIsolABStatusInput))
            {
              trace.write(ATC::briefTrace, "Failed to store the Brake Test Reason in NVS!");
              writeToLog(ATC::BriefLog, "Failed to store the Brake Test Reason in NVS!", __FILE__, __LINE__);
            }
          }
          else
          {
            triggerIsolNotInRunCounter++;
          }
        }
      }
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractLocoIO* AbstractLocoIO::corePtr(void)
    {
      return coreLocoIOInstancePtr;
    }

    /******************************************************************************
    * Constructor for DigitalInput
    ******************************************************************************/
    AbstractLocoIO::DigitalInput::DigitalInput()
    {
      signalIn = 0U;
      isVital = true;
      validityState = Invalid;
      value = false;
      warningCount = 0U;
      memset(&signalName[0], 0, sizeof(signalName));
    }

    /******************************************************************************
    * init for DigitalInput
    ******************************************************************************/
    void AbstractLocoIO::DigitalInput::setInitialValues(const uint8_t sig, const char_t *const sigName, const bool defaultValue, const bool vital)
    {
      // All the inputs are treated as vital in sw and are made valid to start with.
      signalIn = sig;
      isVital = vital;
      value = defaultValue;
      warningCount = 0U;

      static_cast<void>(vfw_strlcpy(&signalName[0], sigName, sizeof(signalName)));
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractLocoIO::DigitalInput::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&signalIn));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&isVital));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ValidityState>(&validityState));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&value));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&warningCount));
      crossCompare->addCrossCompareData(new Support::CrossCompareArray<char_t>(&signalName[0], maxCharsSignalName));
    }

    /******************************************************************************
    * Constructor for AnalogDigitalInput
    ******************************************************************************/
    AbstractLocoIO::AnalogInput::AnalogInput()
    {
      analogSignalIn = 0U;
      validityState = Invalid;
      analogSignalvalue = 0U;
      warningCount = 0U;
      memset(&analogSignalName[0], 0, sizeof(analogSignalName));
    }

    /******************************************************************************
    * init for AnalogInput
    ******************************************************************************/
    void AbstractLocoIO::AnalogInput::setInitialValues(const uint8_t sig, const char_t *const sigName)
    {
      analogSignalIn = sig;
      warningCount = 0U;

      static_cast<void>(vfw_strlcpy(&analogSignalName[0], sigName, sizeof(analogSignalName)));
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractLocoIO::AnalogInput::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&analogSignalIn));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ValidityState>(&validityState));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&analogSignalvalue));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&warningCount));
      crossCompare->addCrossCompareData(new Support::CrossCompareArray<char_t>(&analogSignalName[0], maxCharsSignalName));
    }

    /******************************************************************************
    * Constructor for DigitalOutput
    ******************************************************************************/
    AbstractLocoIO::DigitalOutput::DigitalOutput() :
      signalOut(0U),
      isVital(true),
      defaultValue(false),
      currentValue(false),
      newValue(false),
      feedbackValue(false),
      validityState(Invalid),
      timestamp(0)
    {
      memset(&signalName[0], 0, sizeof(signalName));
    }

    /******************************************************************************
    * Init for DigitalOutput
    ******************************************************************************/
    void AbstractLocoIO::DigitalOutput::setInitialValues(const uint8_t sig, const char_t *const sigName, const bool vital)
    {
      signalOut = sig;
      isVital = vital;

      static_cast<void>(vfw_strlcpy(&signalName[0], sigName, sizeof(signalName)));
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractLocoIO::DigitalOutput::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&signalOut));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&isVital));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&defaultValue));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&currentValue));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&newValue));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&feedbackValue));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ValidityState>(&validityState));
      crossCompare->addCrossCompareData(new Support::CrossCompareInt64(&timestamp));
      crossCompare->addCrossCompareData(new Support::CrossCompareArray<char_t>(&signalName[0], maxCharsSignalName));
    }

    /******************************************************************************
    * Constructor for AnalogOutput
    ******************************************************************************/
    AbstractLocoIO::AnalogOutput::AnalogOutput()
    {
      analogSignalOut = 0U;
      analogCurrentValue = 0U;
      memset(&analogSignalName[0], 0, sizeof(analogSignalName));
    }

    /******************************************************************************
    * Init for AnalogOutput
    ******************************************************************************/
    void AbstractLocoIO::AnalogOutput::setInitialValues(const uint8_t sig, const char_t *const sigName)
    {
      analogSignalOut = sig;

      static_cast<void>(vfw_strlcpy(&analogSignalName[0], sigName, sizeof(analogSignalName)));
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractLocoIO::AnalogOutput::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&analogSignalOut));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&analogCurrentValue));
      crossCompare->addCrossCompareData(new Support::CrossCompareArray<char_t>(&analogSignalName[0], maxCharsSignalName));
    }

    /******************************************************************************
    * Returns string as validity of state
    ******************************************************************************/
    const char_t *AbstractLocoIO::validStateStr(const ValidityState validState) const
    {
      const char_t *validityStateStr;
      switch (validState)
      {
      case Invalid:
        validityStateStr = "Invalid";
        break;
      case ValidationPending:
        validityStateStr = "ValidationPending";
        break;
      case Valid:
        validityStateStr = "Valid";
        break;
      default:
        validityStateStr = "?";
        break;
      }
      return validityStateStr;
    }


    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool AbstractLocoIO::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      bool retVal = false;
      char_t  buffer[512];

      // Check for argument passed is "help"
      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        const char_t* const toWrite = "LocoIO        To get the current Value of Inputs and outputs of LocoIO";

        // Write to console output
        ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);
      }
      // Check if Argument match with LocoIO string
      else if (ATC::isTextMatch(&argv[0][0], "LocoIO", sizeof("LocoIO")))
      {
        switch (argc)
        {
        case 1:
        {
          // Check if Inputs are available
          if (numOfDigitalInputs > 0U)
          {
            const char_t* const toWrite = "DigitalInputs     VitalStatus    Validity    Value";

            // Write to console output
            ATC::AbstractConsole::corePtr()->writeWithNewline("-------------------------------------------------------------");
            ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);
            ATC::AbstractConsole::corePtr()->writeWithNewline("-------------------------------------------------------------");

            // Parse the input structure
            for (uint8_t inputCount = 0U; inputCount < numOfDigitalInputs; inputCount++)
            {
              const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%-20s%-15d%-12s%-5d",
                digitalInputs[inputCount].signalName,
                digitalInputs[inputCount].isVital,
                validStateStr(digitalInputs[inputCount].validityState),
                digitalInputs[inputCount].value);

              if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
              {
                ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
              }
            }
          }
          else
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline("DigitalInput list is empty !!");
          }

          // Check if Outputs are available
          if (numOfDigitalOutputs > 0U)
          {
            const char_t* const toWrite = 
              "Outputs              VFW_Side  isVital    DefaultValue  CurrentValue  RelayFB  NewValue  IsValid   Timestamp";

            // Write to console output
            ATC::AbstractConsole::corePtr()->writeWithNewline
            ("----------------------------------------------------------------------------------------------------");
            ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);
            ATC::AbstractConsole::corePtr()->writeWithNewline
            ("----------------------------------------------------------------------------------------------------");

            // Parse the output structure
            for (uint8_t outputCount = 0U; outputCount < numOfDigitalOutputs; outputCount++)
            {
              const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%-21s%-10s%-11d%-14d%-14d%-9d%-10d%-10s%-9d",
                digitalOutputs[outputCount].signalName,
                ATC::getVfwSideString(vfwGetSide()),
                digitalOutputs[outputCount].isVital,
                digitalOutputs[outputCount].defaultValue,
                digitalOutputs[outputCount].currentValue,
                digitalOutputs[outputCount].newValue,
                digitalOutputs[outputCount].feedbackValue,
                validStateStr(digitalOutputs[outputCount].validityState),
                static_cast<int32_t>(digitalOutputs[outputCount].timestamp));

              if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
              {
                ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
              }
            }
          }
          else
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline("Output list is empty !!");
          }
          break;
        }
        default:
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline("Illegal Argument for LocoIO as LocoIO takes no arguments !! ");
          break;
        }
        }
        retVal = true;
      }
      else
      {
        // Do nothing
      }
      return retVal;
    }

    /******************************************************************************
    * getLocoDirection()
    ******************************************************************************/
    TravelDir AbstractLocoIO::getLocoDirection() const
    {
      return locoTravelDirection;
    }

    /******************************************************************************
    * getEmergencyStopActiveAlert()
    ******************************************************************************/
    bool AbstractLocoIO::getEmergencyStopActiveAlert(void) const
    {
      // Check if Emergency Stop Active is configured
      bool isESAConfigured = AbstractConfig::corePtr()->getESAInputAvail();
      bool isEMSActive = false;
      // Read the LocoIO value of EMS Signal if it is configured
      if (isESAConfigured)
      {
        bool retValue = getCoreDigitalInputValue(EmerStopActive, &isEMSActive);
        if (!retValue)
        {
          isEMSActive = false;
          trace.write(ATC::briefTrace, "Could not read EmergencyStopActive I/O.");
        }
      }
      return isEMSActive;
    }

    /******************************************************************************
    * fillDisplayText()
    ******************************************************************************/
    void AbstractLocoIO::fillDisplayText(VIOHnames::VIOH_displayTextType &d, const char_t * const text) const
    {
      uint32_t i;
      const char_t* p = text;

      // display max 8 characters
      for (i = 0U; i < sizeof(d.displayText); i++)
      {
        d.displayText[i] = static_cast<uint8_t>(' ');
        const char_t ch = *p;
        if (ch != '\0')
        {
          d.displayText[i] = static_cast<uint8_t>(ch);
          ++p;
        }
      }
    }

    /******************************************************************************
    * setDisplayText()
    ******************************************************************************/
    bool AbstractLocoIO::setDisplayText(const char_t * const text)
    {
      VIOHnames::VIOH_displayTextType d;
      bool retVal = false;

      const bool_t r = viohClientHandle->GPIODisplayIsBusy();
      if (r == ATC::falseVfw)
      {
        fillDisplayText(d, text);

        const VIOHnames::VIOH_clientResultType ret = viohClientHandle->GPIOSetDisplay(&d,
          VIOHnames::enMEDIUMHIGH,
          0,
          0);

        if (VIOHnames::enCRT_OK == ret)
        {
          retVal = true;
        }
        else
        {
          trace.write(ATC::briefTrace, "GPIOSetDisplay() failed");
        }
      }

      return retVal;
    }

    /******************************************************************************
    * getSleepingSignal()
    ******************************************************************************/
    bool AbstractLocoIO::getSleepingSignal(void) const
    {
      // Default implementation is not sleeping
      return false;
    }

    /******************************************************************************
    * getATOModeSwitchPosition()
    ******************************************************************************/
    ATOModeSwitchPos AbstractLocoIO::getATOModeSwitchPosition() const
    {
      return atoModeSwitchPos;
    }

    /******************************************************************************
    * calculateDrivingDirection()
    ******************************************************************************/
    void AbstractLocoIO::calculateDrivingDirection(void)
    {
      //Get the value of forward Signal
      bool fwdStatus = getDigitalInputValue(static_cast<uint8_t>(Fwd));
      //Get the Value of reverse Signal
      bool revStatus = getDigitalInputValue(static_cast<uint8_t>(Rev));
      //Locomotive ready Status
      bool locoStatus = getDigitalInputValue(static_cast<uint8_t>(LCSRdy));
      //check for locomotive is ready?
      if (!locoStatus)
      {
        locoTravelDirection = DirNone; //neutral
      }
      //if the forward and reverse input is high
      else if ((fwdStatus) &&
        (revStatus))
      {
        locoTravelDirection = DirUndefined; //undefined
        // If both direction inputs are active at the same time for 1 second Issue a Brake event.
        if (ambiguousTravelDirCounter >= getCounterToWaitForAmbiguousTravelDir())
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(ambiguousTravelDir, __FILE__, __LINE__);
        }
        else
        {
          ambiguousTravelDirCounter++;
        }
      }
      //if forward and reverse signal is low
      else if ((!fwdStatus) && (!revStatus))
      {

        locoTravelDirection = DirNone; //neutral
        ambiguousTravelDirCounter = 0U;
       
        const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
        const Kernel::LocationModeState locationModeState = Kernel::AbstractModeControl::corePtr()->getLocationModeState();
        const bool freeRollingInUnloadLocation = Kernel::AbstractModeControl::corePtr()->getFreeRolling()
          && (currentMode == ATPModeLocation) && (locationModeState == Kernel::LocationMode::locationUnloadLocation);

        if ((currentMode != ATPModeSleeping)
           && (!freeRollingInUnloadLocation))
        {    
          /*Raise standstill if both the Forward and Reverse input signals are not Active if not in 
          ATP Mode Location of type Unload with train state Free Rolling */

          ATC::AbstractEventHandler::corePtr()->reportEvent(standStillDrivingDirNotSet, __FILE__, __LINE__);
        }
      }
      else
      {
        LocoVsTrainDir locoVsTrainDir = DS::AbstractTSetup::corePtr()->getLocovsTrainOrientation();
        //if forward input signal is high and rev is low
        if ((fwdStatus) && (!revStatus))
        {//if A is at front side
          if (locoVsTrainADirection == locoVsTrainDir)
          { //set the loco direction to forward 
            locoTravelDirection = DirForward;
          }//if B is at front side
          else if (locoVsTrainBDirection == locoVsTrainDir)
          { //set the loco direction to reverse
            locoTravelDirection = DirReverse;
          }
          else
          {
            locoTravelDirection = DirUndefined;
          }
        }
        // Reverse input signal is high and fwd is low
        else if ((revStatus) && (!fwdStatus))
        {//if A is at front side
          if (locoVsTrainADirection == locoVsTrainDir)
          {
            locoTravelDirection = DirReverse;
          }//if B is at front side
          else if (locoVsTrainBDirection == locoVsTrainDir)
          {
            locoTravelDirection = DirForward;
          }
          else
          {
            locoTravelDirection = DirUndefined;
          }
        }
        else
        {
          locoTravelDirection = DirNone;
        }

        ambiguousTravelDirCounter = 0U;
      }//end of else
    }

    /******************************************************************************
    * calculateATOModeSwitchPosition()
    ******************************************************************************/
    void AbstractLocoIO::calculateATOModeSwitchPosition(void)
    {
      if (AbstractConfig::corePtr()->getAtoEnable())
      {
        //Get the value of ATO Manual Signal
        bool atoManualStatus = getDigitalInputValue(static_cast<uint8_t>(Manual));
        //Get the Value of ATO Supervised Signal
        bool atoSupervisedStatus = getDigitalInputValue(static_cast<uint8_t>(Supervised));
        //Get the Value of ATO Automatic Status
        bool atoAutomaticStatus = getDigitalInputValue(static_cast<uint8_t>(Automatic));

        if ((atoManualStatus) && (!atoSupervisedStatus) && (!atoAutomaticStatus))
        {
          atoModeSwitchPos = ATOModeManualPos;
          atoSwitchModeCounter = 0U;
        }
        else if ((!atoManualStatus) && (atoSupervisedStatus) && (!atoAutomaticStatus))
        {
          atoModeSwitchPos = ATOModeSupervisedPos;
          atoSwitchModeCounter = 0U;
        }
        else if ((!atoManualStatus) && (!atoSupervisedStatus) && (atoAutomaticStatus))
        {
          atoModeSwitchPos = ATOModeAutomaticPos;
          atoSwitchModeCounter = 0U;
        }
        else
        {
          //Check for tolerance value before indicating as invalid
          if (atoSwitchModeCounter < getATOSwitchModeSelectionTime())
          {
            atoSwitchModeCounter++;
          }
          else
          {
            atoModeSwitchPos = ATOModeUnknownPos;
            //Standstill Event
            ATC::AbstractEventHandler::corePtr()->reportEvent(noOrMoreThanOneATOSwitchModeSelected, __FILE__, __LINE__);
          }
        }
      }
      else
      {
        atoModeSwitchPos = ATOModeManualPos;
      }
    }

    /******************************************************************************
    * getATOSwitchModeSelectionTime
    ******************************************************************************/
    uint8_t AbstractLocoIO::getATOSwitchModeSelectionTime(void) const
    {
      return atoSwitchModeTolerance;
    }

    /******************************************************************************
    * manageBuzzerStatus
    ******************************************************************************/
    void AbstractLocoIO::manageBuzzerStatus()
    {

      if (getBuzzerStatus())
      {
        buzzerCounter = 0U;
      }

      switch (buzzerType)
      {
      case BuzzerTypeNone:
        {
          buzzerActive = false;
          buzzerCounter = 0U;
        }
        break;

      case BuzzerTypeOneBeep:
        if (buzzerCounter < buzzerOnCycleCount)
        {
          buzzerActive = true;
          buzzerCounter++;
        }
        else
        {
          buzzerType = BuzzerTypeNone;
          buzzerActive = false;
          buzzerCounter = 0U;
        }
        break;

      case BuzzerTypeTwoBeep:
        {
          //TwoBeeps 300ms (On) followed by 200 ms (Off) followed by 300 ms (On) and then Off
          uint16_t totalCycleCount = (2U * buzzerOnCycleCount) + buzzerOffCycleCount;
          if (buzzerCounter < totalCycleCount)
          {
            if ((buzzerCounter < buzzerOnCycleCount) ||
              (buzzerCounter > (buzzerOnCycleCount + buzzerOffCycleCount)))
            {
              buzzerActive = true;
              buzzerCounter++;
            }
            else
            {
              buzzerActive = false;
              buzzerCounter++;
            }
          }
          else
          {
            buzzerType = BuzzerTypeNone;
            buzzerActive = false;
            buzzerCounter = 0U;
          }
        }
        break;

      case BuzzerTypeTwoBeepsPerSec:
        {
          //TwoBeepsPerSec 300ms (On) followed by 200 ms (Off) followed by 300 ms (On) and then 200ms (Off) and repeat
          if (buzzerCounter < buzzerOnCycleCount)
          {
            buzzerActive = true;
            buzzerCounter++;
          }
          else if (buzzerCounter < (buzzerOnCycleCount + buzzerOffCycleCount))
          {
            buzzerActive = false;
            buzzerCounter++;
          }
          else
          {
            buzzerCounter = 0U;
          }
        }
        break;

      case BuzzerTypeFor30Sec:
        {
          //buzzer for 30 sec
          if (buzzerCounter < atpCycleFor30SecBuzzer)
          {
            buzzerActive = true;
            buzzerCounter++;
          }
          else
          {
            buzzerType = BuzzerTypeNone;
            buzzerActive = false;
            buzzerCounter = 0U;
          }
        }
        break;

      case BuzzerTypeConstantBeep:
        {
          buzzerActive = true; // Buzzer will be activated until BuzzerTypeUndefined will be raised by the component
        }
        break;

      default:
        break;
      }
    }

    /******************************************************************************
    * getBuzzerStatus
    ******************************************************************************/
    bool AbstractLocoIO::getBuzzerStatus()
    {
      bool buzzerTypeChanged = false;

      //Get buzzer request raised by mode control
      BuzzerType buzzerByMC = Kernel::AbstractModeControl::corePtr()->getBuzzerRequest();
      //Get buzzer request raised by supervision
      BuzzerType buzzerBySupv = Supv::AbstractSupervise::corePtr()->getBuzzerRequest();

      //select the high priority buzzer raised by mode control or supervision
      BuzzerType highestPrioBuzzerRequested = (buzzerByMC > buzzerBySupv) ? buzzerByMC : buzzerBySupv;


      if (highestPrioBuzzerRequested > buzzerType)
      {
        // change current playing buzzer if high priority buzzer has been raised
        buzzerType = highestPrioBuzzerRequested;
        buzzerTypeChanged = true;
      }
      // BuzzerTypeTwoBeepsPerSec, BuzzerTypeConstantBeep have unspecified duration. Check if still raised by component
      else if (((buzzerType == BuzzerTypeTwoBeepsPerSec)
        || (buzzerType == BuzzerTypeConstantBeep)) && (buzzerType != buzzerByMC) && (buzzerType != buzzerBySupv))
      {
        //BuzzerTypeTwoBeepsPerSec, BuzzerTypeConstantBeep no longer requested neither by Mode Control nor by Supervision
        // So set current buzzerType to low priority buzzer (most of the time BuzzerTypeUndefined)
        buzzerType = highestPrioBuzzerRequested;
        buzzerTypeChanged = true;
      }
      else
      {
        //Do nothing!
      }
      return buzzerTypeChanged;
    }

    /******************************************************************************
    * getCounterToWaitForAmbiguousTravelDir
    ******************************************************************************/
    uint8_t AbstractLocoIO::getCounterToWaitForAmbiguousTravelDir() const
    {
      //Counter value that allows waiting for 1 second. each cycle is of 100ms duration.
      uint8_t triggerAmbiguousTravelDirVal = 10U;

      return triggerAmbiguousTravelDirVal;
    }

    /******************************************************************************
    * initDigitalInputs
    ******************************************************************************/
    void AbstractLocoIO::initDigitalInputs(void)
    {
      VIOHnames::VIOH_confRespType viohRegisterResult;
      VIOHnames::VIOH_healthStateType viohHealthState = VIOHnames::VIOResOK;
      // Initialization of Digital input channel
      switch (stateDigitalInputInit)
      {
        // initialize cross-compare
      case InitCrossCompare:
      {
        initCrossCompare();

        stateDigitalInputInit = RegisterRequest;
        break;
      }
      //Register with VIOH client
      case RegisterRequest:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->VIURegister(&digitalInputsList);
        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          //All good! Move to next state.
          stateDigitalInputInit = RegisterPending;
        }
        else if (VIOHnames::enCRT_OKNU == viohClientRetValue)
        {
          // register result not available yet, wait another cycle
          trace.write(ATC::veryDetailedTrace, "VIU register result is pending!");
        }
        else
        {
          //Send Event to event Handler.
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorInputRegisterFail, __FILE__, __LINE__);
        }

        break;
      }

      //Check the result of register
      case RegisterPending:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->VIURegisterResult(&viohRegisterResult);
        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          if (VIOHnames::enCFG_OK == viohRegisterResult)
          {
            //All good! Move to next state.
            stateDigitalInputInit = GetDevState;
          }
          else if (VIOHnames::enCFG_PEND == viohRegisterResult)
          {
            //Wait for another cycle
          }
          else
          {
            //Event to event handler
            ATC::AbstractEventHandler::corePtr()->reportEvent(errorInputRegisterResultIncorrect, __FILE__, __LINE__);
          }
        }
        else
        {
          //Event to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorInputRegisterResultFail, __FILE__, __LINE__);
        }

        break;
      }

      //Get device state after registration
      case GetDevState:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->VIUGetDeviceState(&viohHealthState);
        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          if (0U != (viohHealthState & VIOHnames::VIORes_CHECKVERSION))
          {
            ATC::aosHalt(__FILE__, __LINE__, "Version mismatch for VIU");
          }
          //All good! Move to next state.
          stateDigitalInputInit = GetRevisionId;
        }
        else
        {
          //Event to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorInputDeviceStateFail, __FILE__, __LINE__);
        }

        break;
      }

      //VIU hardware should match expected result
      case GetRevisionId:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->VIUGetRevisionId(&viuRevisionId);
        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          //All good! Move to next state.
          stateDigitalInputInit = GetHWConf;
        }
        else
        {
          //Event to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorInputRevIdIncorrect, __FILE__, __LINE__);
        }

        break;
      }

      //Hardware configuration for inputs
      case GetHWConf:
      {
        uint8_t channel = 255U;
        uint8_t voltageRange = 255U;

        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->VIUGetHWConf(&channel, &voltageRange, &viohHealthState);
        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          if (0U != (viohHealthState & VIOHnames::VIORes_CHECKVERSION))
          {
            ATC::aosHalt(__FILE__, __LINE__, "Version mismatch for VIU");
          }
          //All good! Move to next state.
          stateDigitalInputInit = InitDone;
        }
        else
        {
          //Event to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorInputHwConfIncorrect, __FILE__, __LINE__);
        }

        break;
      }


      //Initialization for Digital inputs done
      case InitDone:
      {
        //Initialization of Digital Input channels complete. 
        ioInitState = DigitalOutputState;
        break;
      }

      case initDisplayRegister:
        // Fall through
      case initDisplayWaitRegister:
        // Fall through
      case initDisplaySetText:
        // Fall through
      default:
      {
        //Unknown State
        break;
      }
      }//End of switch(stateDigitalInputInit)
    }


    /******************************************************************************
    * initDigitalOutputs
    ******************************************************************************/
    void AbstractLocoIO::initDigitalOutputs(void)
    {
      VIOHnames::VIOH_confRespType viohRegisterResult;
      VIOHnames::VIOH_healthStateType viohHealthState = VIOHnames::VIOResOK;
      //VOU state machine
      //Input initialization successful. Start Digital Output initialization.
      switch (stateDigitalOutputInit)
      {
        //Register with VIOH client
      case RegisterRequest:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->VOURegister(&digitalOutputsList,
          (static_cast<VIOHnames::VIOH_listType *const>(NULL)), (static_cast<VIOHnames::VIOH_listType *const>(NULL)));

        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          //All good! Move to next state.
          stateDigitalOutputInit = RegisterPending;
        }
        else
        {
          //Send Event to event Handler.
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorOutputRegisterFail, __FILE__, __LINE__);
        }

        break;
      }

      //Check the result of register
      case RegisterPending:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->VOURegisterResult(&viohRegisterResult);
        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          if (VIOHnames::enCFG_OK == viohRegisterResult)
          {
            //All good! Move to next state.
            stateDigitalOutputInit = GetDevState;
          }
          else if (VIOHnames::enCFG_PEND == viohRegisterResult)
          {
            //Wait for another cycle
          }
          else
          {
            //Event to event handler
            ATC::AbstractEventHandler::corePtr()->reportEvent(errorOutputRegisterResultIncorrect, __FILE__, __LINE__);
          }
        }
        else
        {
          //Event to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorOutputRegisterResultFail, __FILE__, __LINE__);
        }

        break;
      }

      //Get device state after registration
      case GetDevState:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->VOUGetDeviceState(&viohHealthState);
        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          if (0U != (viohHealthState & VIOHnames::VIORes_CHECKVERSION))
          {
            ATC::aosHalt(__FILE__, __LINE__, "Version mismatch for VOU");
          }
          //All good! Move to next state.
          stateDigitalOutputInit = GetRevisionId;
        }
        else
        {
          //Event to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorOutputDeviceStateFail, __FILE__, __LINE__);
        }

        break;
      }

      //VOU hardware should match expected result
      case GetRevisionId:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->VOUGetRevisionId(&vouRevisionId);
        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          //All good! Move to next state.
          stateDigitalOutputInit = GetHWConf;
        }
        else
        {
          //Event to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorOutputRevIdIncorrect, __FILE__, __LINE__);
        }

        break;
      }

      //Hardware configuration for outputs
      case GetHWConf:
      {
        uint8_t channel = 255U;
        uint8_t voltageRange = 255U;

        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->VOUGetHWConf(&channel, &voltageRange, &viohHealthState);
        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          if (0U != (viohHealthState & VIOHnames::VIORes_CHECKVERSION))
          {
            ATC::aosHalt(__FILE__, __LINE__, "Version mismatch for VOU");
          }
          //All good! Move to next state.
          stateDigitalOutputInit = InitDone;
        }
        else
        {
          //Event to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorOutputHwConfIncorrect, __FILE__, __LINE__);
        }

        break;
      }

      //Initialization for outputs done
      case InitDone:
      {
        //Initialization of Output channels complete.
        ioInitState = AnalogInputState;
        break;
      }

      case InitCrossCompare:
        // Fall through
      case initDisplayRegister:
        // Fall through
      case initDisplayWaitRegister:
        // Fall through
      case initDisplaySetText:
        // Fall through
      default:
      {
        //Unknown State
        break;
      }
      }//End of switch(stateDigitalOutputInit)
    }

    /******************************************************************************
    * initAnalogInputs
    ******************************************************************************/
    void AbstractLocoIO::initAnalogInputs(void)
    {
      VIOHnames::VIOH_confRespType viohRegisterResult;
      VIOHnames::VIOH_healthStateType viohHealthState = VIOHnames::VIOResOK;

      //Analog Inputs State Machine
      // Initialization of Analog input channel
      switch (stateAnalogInputInit)
      {
        //Register with VIOH client
      case RegisterRequest:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->AIOURegisterSync(
          &analogInputsList, &analogOutputsList,&LimitLow[0], &LimitHigh[0]);

        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          //All good! Move to next state.
          stateAnalogInputInit = RegisterPending;
        }
        else if (VIOHnames::enCRT_OKNU == viohClientRetValue)
        {
          // register result not available yet, wait another cycle
          trace.write(ATC::veryDetailedTrace, "AIOU register request failed: an request is still pending!");
          writeToLog(ATC::BriefLog, "AIOU register request failed: an request is still pending", __FILE__, __LINE__);
        }
        else if (VIOHnames::enCRT_IPARA == viohClientRetValue)
        {
          trace.write(ATC::veryDetailedTrace, "AIOU register request failed: Invalid parameter!");
          writeToLog(ATC::BriefLog, "AIOU register request failed: Invalid parameter!", __FILE__, __LINE__);
        }
        else if (VIOHnames::enCRT_NO_INI == viohClientRetValue)
        {
          trace.write(ATC::veryDetailedTrace, "AIOU register request failed: Non vital application calling AIOURegisterSync not allowed!");
          writeToLog(ATC::BriefLog, "AIOU register request failed: Non vital application calling AIOURegisterSync not allowed!", __FILE__, __LINE__);
        }
        else if (VIOHnames::enCRT_NOREG == viohClientRetValue)
        {
          trace.write(ATC::veryDetailedTrace, "AIOU register request failed: application is already registered for this input/output.!");
          writeToLog(ATC::BriefLog, "AIOU register request failed: application is already registered for this input/output.!", __FILE__, __LINE__);
        }
        else if (VIOHnames::enCRT_OUT_OF_RANGE == viohClientRetValue)
        {
          trace.write(ATC::veryDetailedTrace, "AIOU register request failed: too many outputs requested");
          writeToLog(ATC::BriefLog, "AIOU register request failed: too many outputs requested", __FILE__, __LINE__);
        }
        else
        {
          //Send Event to event Handler.
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorInputRegisterFail, __FILE__, __LINE__);
        }

        break;
      }

      //Check the result of register
      case RegisterPending:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->AIOURegisterResult(&viohRegisterResult);
        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          if (VIOHnames::enCFG_OK == viohRegisterResult)
          {
            //All good! Move to next state.
            stateAnalogInputInit = GetDevState;
          }
          else if (VIOHnames::enCFG_PEND == viohRegisterResult)
          {
            // register result not available yet, wait another cycle
            trace.write(ATC::veryDetailedTrace, "AIOU register result is pending!");
          }
          else
          {
            //Event to event handler
            ATC::AbstractEventHandler::corePtr()->reportEvent(errorInputRegisterResultIncorrect, __FILE__, __LINE__);
          }
        }
        else
        {
          //Event to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorInputRegisterResultFail, __FILE__, __LINE__);
        }

        break;
      }

      //Get device state after registration
      case GetDevState:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->AIOUGetDeviceState(&viohHealthState);
        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          if (0U != (viohHealthState & VIOHnames::VIORes_CHECKVERSION))
          {
            ATC::aosHalt(__FILE__, __LINE__, "Version mismatch for AIOU");
          }
          // Analogue input output unit should feel well
          //All good! Move to next state.
          stateAnalogInputInit = GetRevisionId;
        }
        else
        {
          //Event to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorInputDeviceStateFail, __FILE__, __LINE__);
        }

        break;
      }

      //AIOU hardware should match expected result
      case GetRevisionId:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->AIOUGetRevisionId(&aiouRevisionId);
        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          //All good! Move to next state.
          stateAnalogInputInit = InitDone;
        }
        else
        {
          //Event to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorInputRevIdIncorrect, __FILE__, __LINE__);
        }

        break;
      }

      //Initialization for Analog inputs done
      case InitDone:
      {
        //Initialization of Analog Input channels complete.
        ioInitState = GPIOState;
        break;
      }

      case InitCrossCompare:
        // Fall through
      case initDisplayRegister:
        // Fall through
      case initDisplayWaitRegister:
        // Fall through
      case initDisplaySetText:
        // Fall through
      case GetHWConf:
        // Fall through
      default:
      {
        //Unknown State

        break;
      }
      }//End of switch(stateAnalogInputInit)
    }

    /******************************************************************************
    * initAnalogOutputs
    ******************************************************************************/
    void AbstractLocoIO::initGPIO(void)
    {
      VIOHnames::VIOH_confRespType gpioRegisterSVDResult;

      //Start Initialization of GPIO 
       //Output initialization successful. Start GPIO initialization.
      switch (stateSVDInit)
      {
        //Register with GPIO
      case RegisterRequest:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->GPIOTriggerSVD(static_cast<bool_t>(false));
        if (VIOHnames::enCRT_OK == viohClientRetValue)
        {
          // if result is OK before registration something is wrong! keep state for ever
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorSVDAlreadyTriggered, __FILE__, __LINE__);
        }
        else if (VIOHnames::enCRT_OK != viohClientHandle->GPIORegisterSVD())  // if Trigger is rejected as expected try next step and register
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorGPIORegisterSVDFail, __FILE__, __LINE__);
        }
        else
        {
          // successful call to registration
          stateSVDInit = RegisterPending;
        }
        break;
      }

      //Check the result of register
      case RegisterPending:
      {
        const VIOHnames::VIOH_clientResultType viohClientRetValue = viohClientHandle->GPIORegisterSVDResult(&gpioRegisterSVDResult);
        if (VIOHnames::enCRT_OK != viohClientRetValue)
        {
          //call failed - keep state for ever
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorGPIORegisterSVDResultCallFail, __FILE__, __LINE__);
        }
        else if (VIOHnames::enCFG_OK != gpioRegisterSVDResult) // call OK - check result
        {
          //call failed - keep state for ever
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorCannotGetGPIORegisterSVDResult, __FILE__, __LINE__);
        }
        else
        {
          // successful continue with LCD display setup
          stateSVDInit = initDisplayRegister;
        }
        break;
      }

      case GetDevState:
      case GetRevisionId:
      case GetHWConf:
      {
        writeToLog(ATC::BriefLog, "Undefined State while registering GPIO", __FILE__, __LINE__);
        break;
      }

      case initDisplayRegister:
      {
        VIOHnames::VIOH_displayTextType d;
        fillDisplayText(d, ATC::AbstractApplicationBase::corePtr()->getApplicationVersionString());
        const VIOHnames::VIOH_clientResultType r = viohClientHandle->GPIORegisterDisplay(0, &d);

        if (r == VIOHnames::enCRT_OK)
        {
          stateSVDInit = initDisplayWaitRegister;
          trace.write(ATC::detailedTrace, "Called GPIORegisterDisplay()");
        }
        else if (displaySetupTimeout != 0U)
        {
          setInitDone();
          trace.write(ATC::briefTrace, "Failed to call GPIORegisterDisplay(), skipping Display setup.");
        }
        else
        {
          // Wait for timeout...
          --displaySetupTimeout;
        }
        break;
      }

      case initDisplayWaitRegister:
      {
        VIOHnames::VIOH_confRespType pResult;
        const VIOHnames::VIOH_clientResultType r = viohClientHandle->GPIORegisterDisplayResult(&pResult);

        if ((displaySetupTimeout == 0U) || (VIOHnames::enCRT_OK != r))
        {
          setInitDone();
          trace.write(ATC::briefTrace, "Failed to call GPIORegisterDisplayResult(), skipping Display setup.");
        }
        else if (pResult == VIOHnames::enCFG_OK)
        {
          stateSVDInit = initDisplaySetText;
        }
        else
        {
          // continue waiting
        }

        --displaySetupTimeout;
        break;
      }


      case initDisplaySetText:
      {
        if (displaySetupTimeout == 0U)
        {
          setInitDone();
          trace.write(ATC::briefTrace, "Failed setDisplayText, skipping Display setup.");
        }
        else if (setDisplayText(ATC::AbstractApplicationBase::corePtr()->getApplicationVersionString())) // try until success or timeout
        {
          // Display is set, continue
          setInitDone();
        }
        else
        {
          // Continue waiting
        }
        --displaySetupTimeout;
        break;
      }

      //Initialization for GPIO done
      case InitDone:
      {
        //Initialization of GPIO complete.
        ioInitState = IOInitDone;
        bool resSBAIF = ATC::AbstractAnalyzerIF::corePtr()->registerMeasurement(
          "sbApplication", "Indication for Service Brake applied", "boolean", 0U, 255U, &sbBrakeInfoForAnalyzer);
        bool resEBAIF = ATC::AbstractAnalyzerIF::corePtr()->registerMeasurement(
          "ebApplication", "Indication for Emergency Brake applied", "boolean", 0U, 255U, &ebBrakeInfoForAnalyzer);

        if (!(resSBAIF && resEBAIF))
        {
          writeToLog(ATC::BriefLog,"Register measurement failed for analyzer", __FILE__, __LINE__);
        }

        break;
      }

      case InitCrossCompare:
        break;

      default:
        //Unknown State
        break;
      }
    }

    /******************************************************************************
    * setInitDone
    ******************************************************************************/
    void AbstractLocoIO::setInitDone()
    {
      stateSVDInit = InitDone;

      const VIOHnames::VIOH_clientResultType result = viohClientHandle->GetSwVersion(&viohClientVersionString[0U], &viohServerVersionString[0U]);

      if (result == VIOHnames::enCRT_OK)
      {
        if (!validateViohVersion())
        {
          vitalDriverVersionMismatch.setDynamicText(&viohClientVersionString[0U]);
          ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverVersionMismatch, __FILE__, __LINE__);
        }
      }
      else
      {
        // Should be ready by now, this is an error...
        vitalDriverVersionMismatch.setDynamicText("?");
        ATC::AbstractEventHandler::corePtr()->reportEvent(vitalDriverVersionMismatch, __FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * getRevisionId
    ******************************************************************************/
    void AbstractLocoIO::getRevisionId(uint32_t &digitalInputRevisionId, uint32_t &digitalOutputRevisionId, uint32_t &analogInputRevisionId) const
    {
      digitalInputRevisionId = viuRevisionId;
      digitalOutputRevisionId = vouRevisionId;
      analogInputRevisionId = aiouRevisionId;
    }

    /******************************************************************************
    * setVitalDriverIsActiveOrder
    ******************************************************************************/
    void AbstractLocoIO::setVitalDriverIsActiveOrder(const bool active)
    {
      if (active != vitalDriverIsActiveOrder) //lint !e731 Comparing boolean variables is ok
      {
        currentSVDCounter = 0U; // Update already this cycle..
        vitalDriverTimestamp = vfwGetReferenceTime();
        vitalDriverValidityState = ValidationPending;
        vitalDriverIsActiveOrder = active;
      }
    }

    /******************************************************************************
    * getVitalDriverValidityState
    ******************************************************************************/
    AbstractLocoIO::ValidityState AbstractLocoIO::getVitalDriverValidityState() const
    {
      return vitalDriverValidityState;
    }

  } // namespace IO

} // namespace ATP

//lint +esym(586,snprintf)
