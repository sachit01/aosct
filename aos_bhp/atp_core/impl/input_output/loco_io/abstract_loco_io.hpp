#ifndef AbstractLocoIO_hpp
#define AbstractLocoIO_hpp
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
* 2016-06-06    adgupta     Implementation of LocoIO functionality
* 2016-09-26    spandita    Added function declaration for getvaliditystate and
                            getLocovstrain related functions
* 2016-10-18    arastogi    Moved function getLocovsTrainOrientation to Tsetup
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "atp_types.hpp"
#include <vfw_identity.h>
#include <vfw_sync.h>
#include "vio_types.h"
#include "vioh_client.hpp"
#include "event.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace IO
  {
    /**
    * Define timout to wait while initializing LCD display (LCD display is slow)
    */
    static const uint32_t displaySetupTimeoutValue  = 10U*10U;

    /**
    * maximum number of Digital input channels available with the GSP2
    */
    static const uint8_t maxDigitalInputs = 24U;

    /**
    * maximum number of Digital output channels available with the GSP2
    */
    static const uint8_t maxDigitalOutputs = 12U;

    /**
    * maximum number of Analog input channels available with the GSP2
    */
    static const uint8_t maxAnalogInputs = 8U;
    
    /**
    * maximum number of Analog output channels available with the GSP2
    */
    static const uint8_t maxAnalogOutputs = 2U;

    /**
    * Measurement accuracy. The measurement accuracy is needed when doing input value 
    * cross-compare in the vital I/O server in order to determine whether the values 
    * from A- and B-channel matches.
    */
    static const uint16_t LimitLow[maxAnalogInputs] = { 12U,   12U,   12U,   12U,   12U,  12U,  12U,  12U };

    /**
    * Maximum change within one server cycle. The data is needed in order to do 
    * plausibility checks on the input value. E.g. there is a maximum delta for 
    * the brake pressure in 100ms.
    */
    static const uint16_t LimitHigh[maxAnalogInputs] = { 4000U, 4000U, 4000U, 4000U, 4000U, 4000U, 4000U, 4000U };

    /**
    * List of inputs of Vital I/O
    */
    static VIOHnames::VIOH_listType digitalInputsList;

    /**
    * List of Analog inputs
    */
    static VIOHnames::VIOH_listType analogInputsList;

    /**
    * List of outputs of Vital I/O
    */
    static VIOHnames::VIOH_listType digitalOutputsList;

    /**
    * List of Analog outputs
    */
    static VIOHnames::VIOH_listType analogOutputsList;

    /**
    * The time lag between writing an output and checking the output feedback (ms).
    * It can take upto 500ms(5 ATP cycles) for the Output relays to be valid (starting from the time SVD was triggered).
    * We wait for one additional cycle to be safe. 
    * outputLagTime = (5*100) + (1*100);
    */
    static const int64_t outputLagTime = 600;

    /**
    * Maximum chars for the signal-name
    */
    static const uint8_t maxCharsSignalName = 20U;

    /**
    * max value of Analog Raw Value
    */
    static const uint16_t maxAnalogRawValue = 4095U;

    /**
    * Raw value Corresponding to 4mA Analog signal 
    */
    static const uint16_t minAnalogRawValue = 810U;

    /**
    * ATP cycle value to Activate buzzer
    */
    static const uint8_t buzzerOnCycleCount = 3U;

    /**
    * ATP cycle value to Deactivate buzzer
    */
    static const uint8_t buzzerOffCycleCount = 2U;

    /**
    * ATP cycle value to activate the buzzer for 30 sec
    */
    static const uint16_t atpCycleFor30SecBuzzer = 300U;

    /**
    * Counter value to trigger Isolation switch not in Run mode.
    */
    static const uint8_t triggerIsolNotInRunVal = 5U;

    class AbstractLocoIO;
    /**
    * Static variable to store the single instance of AbstractLocoIO
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractLocoIO* coreLocoIOInstancePtr = static_cast<AbstractLocoIO*>(NULL);

    /**
    * The class AbstractLocoIO implements the interface defined by the IOComponent class.
    *
    */
    class AbstractLocoIO : public ATC::IOComponent
    {
    public:

      /**
      * Validity state for channels
      */
      enum ValidityState
      {
        Invalid,            //!<Invalid
        ValidationPending,  //!<Still not verified
        Valid               //!<Valid
      };

      /**
      * The states of Vital IO to be used by Loco IO
      */
      enum VIOStates
      {
        InitCrossCompare, //!<Call initCrossCompare first, only used by stateDigitalInputInit
        RegisterRequest, //!<Request to register to Vital IO Handler
        RegisterPending, //!<Request sent, get result
        GetDevState,     //!<Get device state
        GetRevisionId,   //!<Get revision Id
        GetHWConf,       //!<Get Hardware configuration
        initDisplayRegister,    //!<Initialize LCD Display: GPIORegisterDisplay
        initDisplayWaitRegister,    //!<Initialize LCD Display: GPIORegisterDisplayReply
        initDisplaySetText,    //!<Initialize LCD Display : GPIODisplayIsBusy and GPIOSetDisplay
        InitDone         //!<Initialization Complete
      };

      /**
      * The Core DigitalOutput enum used by AbstractLocoIO.
      *
      */
      enum CoreDigitalOutputs
      {
        EmerBrake1,      //!<Emergency Brake Valve 1
        EmerBrake2,      //!<Emergency Brake Valve 2
        EmerBrakeApplied,//!<Emergency Brake Active
        ServiceBrake,    //!<Service Brake
        ATPOk,           //!<AOS is okay
        Buzzer,          //!<Buzzer
        Lamp,            //!<ATP indication
        PowerOFF,        //!<Power Off
        NumOfCoreDigitalOutputs
      };

      /**
      * The Core Input enum used by AbstractLocoIO (shall be in the right order, lowest signal number first).
      *
      */
      enum CoreDigitalInputs
      {
        IsolationA,     //!< Isolation A
        IsolationB,     //!< Isolation B
        Manual,         //!<Mode ip1 - Manual
        Supervised,     //!<Mode ip2 - Supervised
        Automatic,      //!<Mode ip2 - Automatic
        Cab1,           //!<Cabin 1 active
        Cab2,           //!<Cabin 2 active
        Fwd,            //!<Forward
        Rev,            //!<Reverse
        LCSRdy,         //!<Locomotive ready
        EmerStopActive, //!<Emergency Stop Active
        OFFIn,          //!<ATP Off
        NumOfCoreDigitalInputs
      };

      /**
      * The Core Analog Input enum used by AbstractLocoIO 
      * The board provides 8 inputs and two outputs. 
      * AOS only uses analog inputs and the outputs are not described/defined
      */
      enum CoreAnalogInputs
      {
        NumOfCoreAnalogInputs
      };

      /**
      * The board provides 8 inputs and two outputs.
      * AOS only uses analog inputs and the outputs are not described/defined
      * TODO: Analog Outputs are not yet defined
      */
      static const uint8_t NumOfCoreAnalogOutputs = 2U;

      /**
      * Enum for Init State of Input Output Signals
      */
      enum IOInitState
      {
        DigitalInputState = 0,
        DigitalOutputState,
        AnalogInputState,
        GPIOState,
        IOInitDone
      };

      /**
      * The class defining each input.
      *
      */
      class DigitalInput
      {
      public:

        uint8_t signalIn;                       //!<DigitalInput signal number
        bool isVital;                           //!<Is the input vital
        ValidityState validityState;            //!<Is the input valid
        bool value;                             //!<Value of the signal
        uint8_t warningCount;                   //!<Number of consecutive warnings received
        char_t signalName[maxCharsSignalName];  //!<Name of signal
        
        /**
        * Default constructor for DigitalInput - Initializing to default values.
        *
        */
        DigitalInput();

        /**
        * Implements the init function of Digital Input.
        *
        * @param[in] sig             The input signal number
        * @param[in] sigName         The input signal name
        * @param[in] defaultValue    The input signal default value
        * @param[in] vital           True if the output is vital, false non-vital
        */
        void setInitialValues(const uint8_t sig, const char_t *const sigName, const bool defaultValue, const bool vital);

        /**
        * Initialize the Cross compare
        */
        void initCrossCompare() const;
      };

      /**
      * The class defining each input.
      *
      */
      class AnalogInput
      {
      public:

        uint8_t analogSignalIn;                         //!<Analog Input signal number
        ValidityState validityState;                          //!<Is Analog input valid
        uint16_t analogSignalvalue;                     //!<Value of the signal
        uint8_t warningCount;                           //!<Number of consecutive warnings received
        char_t  analogSignalName[maxCharsSignalName];   //!<Name of signal

        /**
        * Default constructor for Input - Initializing to default values.
        *
        */
        AnalogInput();

        /**
        * Implements the init function of Analog Input.
        *
        * @param[in] sig      The input signal number
        * @param[in] sigName  The input signal name
        */
        void setInitialValues(const uint8_t sig, const char_t *const sigName);

        /**
        * Initialize the Cross compare
        */
        void initCrossCompare() const;
      };

      /**
      * The class defining each output.
      *
      */
      class DigitalOutput
      {
      public:

        uint8_t signalOut;                      //!<DigitalOutput Signal Number
        bool isVital;                           //!<Is the output vital
        const bool defaultValue;                //!<Default value of output
        bool currentValue;                      //!<Current value of output
        bool newValue;                          //!<New value of output
        bool feedbackValue;                     //!<Feedback value
        ValidityState validityState;            //!<Is the output valid
        int64_t timestamp;                      //!<Timestamp (reference time, milliseconds)
        char_t signalName[maxCharsSignalName];  //!<Name of signal
        
        /**
        * Default constructor for DigitalOutput - Initializing to default values.
        *
        */
        DigitalOutput();

        /**
        * Implements the init function of Digital Output.
        *
        * @param[in] sig      The output signal number
        * @param[in] sigName  The output signal name
        * @param[in] vital    True if the output is vital, false non-vital
        */
        void setInitialValues(const uint8_t sig, const char_t *const sigName, const bool vital);

        /**
        * Initialize the Cross compare
        */
        void initCrossCompare() const;
      };

      /**
      * The class defining each Analog output.
      *
      */
      class AnalogOutput
      {
      public:

        uint8_t analogSignalOut;                      //!<Analog Output Signal Number
        uint16_t analogCurrentValue;                  //!<Current value of output
        char_t analogSignalName[maxCharsSignalName];  //!<Name of signal

        /**
        * Default constructor for Output - Initializing to default values.
        *
        */
        AnalogOutput();

        /**
        * Implements the init function of Analog Output.
        *
        * @param[in] sig      The output signal number
        * @param[in] sigName  The output signal name
        */
        void setInitialValues(const uint8_t sig, const char_t *const sigName);

        /**
        * Initialize the Cross compare
        */
        void initCrossCompare() const;
      };

      /**
      * Implements the virtual init function.
      * Fills in the values for the core inputs and outputs in the structure.
      *
      * @return Returns true when initialization completed
      */
      virtual bool init();

      /**
      * Owner or scheduler shall call runIn() once per activation.
      * Reads all the digital inputs and feedback for the outputs.
      */
      virtual void runIn();

      /**
      * Owner or scheduler shall call runOut() once per activation.
      * Fetches data for all the outputs from various components.
      * Write the data out to the outputs.
      */
      virtual void runOut();

      /**
      * Get the current value of an input from CoreDigitalInputs enum.
      *
      *  @param[in] in   The input whose value is required
      *  @param[out] in   The value of the input required
      *
      *  @return true when value is found
      */
      bool getCoreDigitalInputValue(CoreDigitalInputs in, bool* const value) const;

      /**
      * Get the current value of an analog input from CoreAnalogInputs enum.
      *
      *  @param[in] in   The input whose value is required
      *  @param[out] value   The value of the input required
      *
      *  @return true when value is found
      */
      bool getCoreAnalogInputValue(CoreAnalogInputs in, uint16_t* const value) const;

      /**
      * Get the current value of an output from CoreDigitalOutputs enum.
      *
      *  @param[in] out  The output whose value is required
      *  @param[out] out  The value of output required
      *
      *  @return true when value is found
      */
      bool getCoreDigitalOutputValue(CoreDigitalOutputs out, bool* const value) const;

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractLocoIO* corePtr();

      /**
      * Interface to call  Console Command
      *
      * @param[in] argc  Number of arguments in the argument array argv
      * @param[in] argv  Arguments array
      *
      * @return true if the Call is successful.
      */
      virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

      /**
      * Get the Locomotive Direction
      *
      * @return Direction of Locomotive.
      */
      TravelDir getLocoDirection(void) const;

      /**
      * Get actual position of the ATO Mode Switch 
      *
      * @return ATO Mode Switch position.
      */
      ATOModeSwitchPos getATOModeSwitchPosition(void) const;

      /**
      * Get the value of the Sleeping Signal
      *
      * @return the value of the sleeping-signal (true = activated, false = deactivated)
      */
      virtual bool getSleepingSignal(void) const;

      /**
      * Default timeout while waiting for selection of ATO Switch Mode
      *
      * @return atoSwitchModeTolerance in sec/10 (100 millisecond).
      */
      virtual uint8_t getATOSwitchModeSelectionTime(void) const;

      /**
      * Get the value of Emergency Stop Active Alert
      *
      * @return true if Emergency Stop Active Signal is valid
      */
      bool getEmergencyStopActiveAlert(void) const;

      /**
      * Get the revision Id for Digital Input and Output
      *
      *  @param[out] digitalInputRevisionId  The digital input revision Id.
      *  @param[out] digitalOutputRevisionId  The digital output revision Id.
      *  @param[out] analogInputRevisionId  The Analog output revision Id.
      */
      void getRevisionId(uint32_t &digitalInputRevisionId, uint32_t &digitalOutputRevisionId, uint32_t &analogInputRevisionId) const;


      /**
      * Interface to turn vital driver active or non active
      *
      * @param[in] active  true if it should be active, false non-active
      */
      void setVitalDriverIsActiveOrder(const bool active);


      /**
      * Get the vital driver validity state
      *
      */
      ValidityState getVitalDriverValidityState() const;

      /**
      * Check if all vital output signals are deactivated
      *
      * @return true if all vital output signals are deactivated.
      */
      bool getAllVitalOutputDeactivated() const;

      /**
      * Get the Vioh client version string
      * @return vioh client version string, if set, if not an empty null terminated string is returned.
      */
      const char_t* const getViohClientVersionString() const;

      /**
      * Get the Vioh server version string
      * @return vioh server version string, if set, if not an empty null terminated string is returned.
      */
      const char_t* const getViohServerVersionString() const;

    protected:

      /**
      * Indicates whether service brake should be applied.
      *
      * This is a virtual function to allow the adaptation to override the core behavior.
      */
      virtual bool getSbApplied() const;

      /**
      * Set the new value of an output using output number.
      *
      *  @param[in] out    The output whose value is to be set.
      *
      *  @param[in] value  The value of the output specified.
      */
      void setCoreDigitalOutputValue(const CoreDigitalOutputs out, const bool value);
     
      /** 
      * Constructor for the AbstractLocoIO
      *
      * @param[in] inputsCount   The total number of inputs (core + adaptation).
      * @param[in] outputsCount  The total number of outputs (core + adaptation).
      */
      AbstractLocoIO(const uint8_t digitalInputsCount, const uint8_t digitalOutputsCount, const uint8_t analogInputsCounts, const uint8_t analogOutputsCounts);

      /**
      * Get the current value of an input using input number.
      *
      *  @param[in] in   The input whose value is required
      *
      *  @return the value of the input specified
      */
      bool getDigitalInputValue(const uint8_t in) const;
      /**
      * Get the current value of an output using output number.
      *
      *  @param[in] out  The output whose value is required
      *
      *  @return the current value of the output specified
      */
      bool getDigitalOutputValue(const uint8_t out) const;

      /**
      * Get the current Raw value of an Analog input using input number.
      *
      * @param[in]  in     The input whose value is required
      * @param[out] value  The value of the input specified
      *
      * @return true if the call is successful.
      */
      bool getAnalogInputRawValue(const uint8_t in, uint16_t& value) const;

      /**
      * Get the Validity of state in string format.
      * @return State in string format
      */
      const char_t *validStateStr(const ValidityState validState) const;

      /**
      * Test weather CrossCompare call is due and call
      */
      void runVIOCrossCompare(void);

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

      /**
      * To manage buzzer output activation/deactivation on basis of input from other component
      */
      virtual void manageBuzzerStatus();

      /**
      * To check if any external component (Mode Control/Supervision) has activate the buzzer
      * And Set buzzer according to priority
      * @return true if current buzzer type is replaced by new buzzer raised by component
      */
      virtual bool getBuzzerStatus();

      /**
      * To get the number of cycles/counter value to trigger Ambiguous Travel Direction Brake event.
      * @return value of the number of cycles/counters. This is modifiable by the Adaptation.
      */
      virtual uint8_t getCounterToWaitForAmbiguousTravelDir() const;

      /**
      * An event to report error in input register to vital IO Handler via VIURegister()
      */
      const ATC::Event errorInputRegisterFail;

      /**
      * An event to report error in input register result from vital IO Handler via VIURegisterResult()
      */
      const ATC::Event errorInputRegisterResultFail;

      /**
      * An event to report incorrect in input register result return from vital IO Handler via VIURegisterResult()
      */
      const ATC::Event errorInputRegisterResultIncorrect;

      /**
      * An event to report error in getting device state from vital IO Handler via VIUGetDeviceState()
      */
      const ATC::Event errorInputDeviceStateFail;

      /**
      * An event to report error in getting revision Id from vital IO Handler via VIUGetRevisionId()
      */
      const ATC::Event errorInputRevIdIncorrect;

      /**
      * An event to report incorrect hardware configuration from vital IO Handler via VIUGetHWConf()
      */
      const ATC::Event errorInputHwConfIncorrect;

      /**
      * An event to report that VIOClient CrossCompare failed
      */
      const ATC::Event errorVIOCrossCompareFailed;
      
      /**
      * An event to report error in output register to vital IO Handler via VOURegister()
      */
      const ATC::Event errorOutputRegisterFail;
     
      /**
      * An event to report error in output register result from vital IO Handler via VOURegisterResult()
      */
      const ATC::Event errorOutputRegisterResultFail;

      /**
      * An event to report incorrect in output register result return from vital IO Handler via VOURegisterResult()
      */
      const ATC::Event errorOutputRegisterResultIncorrect;

      /**
      * An event to report error in getting device state from vital IO Handler via VOUGetDeviceState()
      */
      const ATC::Event errorOutputDeviceStateFail;

      /**
      * An event to report error in getting revision Id from vital IO Handler via VOUGetRevisionId()
      */
      const ATC::Event errorOutputRevIdIncorrect;

      /**
      * An event to report incorrect hardware configuration from vital IO Handler via VOUGetHWConf()
      */
      const ATC::Event errorOutputHwConfIncorrect;

      /**
      * An event to report error in writing to Vital IO
      */
      const ATC::Event errorWritingVIOH;

      /**
      * An event to report failure in reading from Vital IO
      */
      const ATC::Event vitalDriverReadFailure;

      /**
      * An event to report error in reading from Vital IO
      */
      const ATC::Event vitalDriverReadError;

      /**
      * An event to report error in reading from Vital IO
      */
      const ATC::Event vitalDriverFeedbackFailure;

      /**
      * An event to report error in reading from Vital IO
      */
      const ATC::Event vitalDriverFeedbackError;

      /**
      * An event to report error for checking SVD is already triggered or not
      */
      const ATC::Event errorSVDAlreadyTriggered;

      /**
      * An event to report error if SVD trigger was not successful
      */
      const ATC::Event errorSVDTrigger;

      /**
      * An event to report error in GPIO register SVD to IO Handler via GPIORegisterSVD()
      */
      const ATC::Event errorGPIORegisterSVDFail;

      /**
      * An event to report error in GPIO register SVD Result Call Fail
      */
      const ATC::Event errorGPIORegisterSVDResultCallFail;

      /**
      * An event to report error in calling GPIO register SVD Result
      */
      const ATC::Event errorCannotGetGPIORegisterSVDResult;
      
      /**
      * An event to report error Digital Output is out of Range while setting 
      */
      const ATC::Event errorDigitalOutputOutOfRange;

      /**
      * An event for standstill when LCSReady is inactive
      */
      const ATC::Event lcsReadyInactiveStandstill;

      /**
      * An event for standstill when No or More than One ATO Switch is selected
      */
      const ATC::Event noOrMoreThanOneATOSwitchModeSelected;

      /**
      * Safety Halt event to report when Isolation Switch is NOT in "Run Mode"
      */
      const ATC::Event isolationSwitchNotInRunMode;

      /**
      * Brake event ambiguous Direction
      */
      const ATC::Event ambiguousTravelDir;

      /**
      * An event to raise stand still event if both input signals are inactive
      */
      const ATC::Event standStillDrivingDirNotSet;

      /**
      * Safety halt for incorrect Vital Driver health State
      */
      const ATC::Event vitalDriverFailure;

      /**
      * Safety halt event for incorrect Vital Driver output
      */
      const ATC::Event vitalDriverOutputFailure;

      /**
      * An event for incorrect Vital Driver output
      */
      const ATC::Event vitalDriverOutputError;

      /**
      * Safety halt event for incorrect Vital Driver versions
      */
      const ATC::Event vitalDriverVersionMismatch;

      /**
      * Allocate max inputs available.
      */
      DigitalInput digitalInputs[maxDigitalInputs];

      /**
      * Allocate max Analog Inputs available.
      */
      AnalogInput analogInputs[maxAnalogInputs];

      /**
      * Allocate max outputs available.
      */
      DigitalOutput digitalOutputs[maxDigitalOutputs];

      /**
      * Allocate max Analog Outputs available.
      */
      AnalogOutput analogOutputs[maxAnalogOutputs];

      /**
      * VIOH Client and VIOH Server software version length string length (maximum 16 characters including a final 0x00).
      */
      static const uint8_t viohVersionStringLength = 16U;

      /**
      * Vioh client version string
      */
      char_t viohClientVersionString[viohVersionStringLength];

      /**
      * Vioh server version string
      */
      char_t viohServerVersionString[viohVersionStringLength];

    private:

      /**
      * Default constructor. Not used though. Created to avoid Lint error.
      *
      */
      AbstractLocoIO();

      /**
      * Set the LED display text on the GSP-2 front pannel
      *
      * @param[in] text   The text to be displayed, currently only 8 chars will be displayed, no scrolling
      * @return true if text was set, otherwise try again
      */
      bool setDisplayText(const char_t * const text);

      /**
      * Fill VIOH_displayTextType with string
      *
      *  @param[in] d  The structure to be filled
      *  @param[in] text  The text to use
      */
      void fillDisplayText(VIOHnames::VIOH_displayTextType &d, const char_t * const text) const;

      /**
      * Write the value of all the outputs.
      *
      */
      void writeOutputs(void);

      /**
      * Read the value of all the inputs.
      *
      */
      void readInputs(void);

      /**
      * Read the value of the Vital Digital input.
      *
      * @param[in] inputIndex The input index in the digitalInput array.
      *
      * @return false if VIO function failed!
      */
      bool readDigitalInput(const CoreDigitalInputs inputIndex);

      /**
      * Read the value of the Vital Analog input.
      *
      * @param[in] inputIndex The input index in the analogInput array.
      *
      * @return false if VIO function failed!
      */
      bool readAnalogInput(const CoreAnalogInputs inputIndex);

      /**
      * Read digital inputs before start up Passed.
      */
      void readInputsBeforeStartUpPassed(void);

      /**
      * Read the feedback value for all the outputs.
      *
      */
      void readOutputFeedback(void);

      /**
      * Supervise the Vital Driver Health state
      *
      */
      void superviseVitalDriverHealthState(void);

      /**
      * Function to process the Isolation inputs used for the Isolation Switch.
      */
      void processIsolationSwitch(void);

      /**
      * calculate Driving direction.
      *
      */
      void calculateDrivingDirection(void);

      /**
      * calculate ATO Mode Switch Position
      *
      */
      void calculateATOModeSwitchPosition(void);

      /**
      * Implements the Digital Inputs initialization function.
      * Fills in the values for the Digital inputs in the structure.
      */
      void initDigitalInputs(void);

      /**
      * Implements the Digital Outputs initialization function.
      * Fills in the values for the Digital Outputs in the structure.
      */
      void initDigitalOutputs(void);

      /**
      * Implements the Analog Inputs initialization function.
      * Fills in the values for the Analog inputs in the structure.
      */
      void initAnalogInputs(void);

      /**
      * Implements the General Purpose Input/Output initialization function.
      * Fills in the values for the GPIO in the structure.
      */
      void initGPIO(void);

      /**
      * Here we handle the things when the state changes to Init Done.
      * Stores the SW versions and changes the state to InitDone.
      */
      void setInitDone();

      /**
      * Implements the specific adaptation function of the version validation of the vioh versions
      * @return true if the version is validated
      */
      virtual bool validateViohVersion() const = 0;

      /**
      * Counter to trigger SVD
      */
      static const uint8_t MaxCountReqForTriggeringSVD = 5U;

      /**
      * Maximum tolerance for ATO SWitch Mode is 1 secs this should be replaced with config in future.
      * since ATP is running at 100 millisecond, hence total count for 1 secs is 10
      */
      static const uint8_t atoSwitchModeTolerance = 10U;

      /**
      * Maximum number of allowed warnings
      */
      static const uint8_t maxNumAllowedWarnings = 5U;

      /**
      * current ATO Switch Mode counter
      */
      uint8_t atoSwitchModeCounter;

      /**
      * Parameter to hold buzzer type to play
      */
      BuzzerType buzzerType;

      /**
      * Parameter to hold buzzer is active
      */
      bool buzzerActive;

      /**
      * Parameter to hold buzzer counter
      */
      uint16_t buzzerCounter;

      /**
      * Counter to measure the ambiguous travel direction.
      */
      uint8_t ambiguousTravelDirCounter;

      /**
      * Counter to measure the isolation Switch not in Run Mode.
      */
      uint8_t triggerIsolNotInRunCounter;

      /**
      * The total number of Inputs.
      *
      */
      const uint8_t numOfDigitalInputs;

      /**
      * The total number of Analog Inputs.
      *
      */
      const uint8_t numOfAnalogInputs;

      /**
      * The total number of Outputs.
      *
      */
      const uint8_t numOfDigitalOutputs;

      /**
      * The total number of Analog Outputs.
      *
      */
      const uint8_t numOfAnalogOutputs;

      /**
       * Loco travel direction currently used
       */
      TravelDir locoTravelDirection;

      /**
      * ATO Mode Switch Position currently used
      */
      ATOModeSwitchPos atoModeSwitchPos;

      /**
      * Handle to the Vital IO client. Used to access VIO handler.
      */
      VIOHnames::VIOHClient* viohClientHandle;

      /**
      * State of Vital IO Handler Inputs
      */
      VIOStates stateDigitalInputInit;

      /**
      * State of Analog Inputs Handler
      */
      VIOStates stateAnalogInputInit;

      /**
      * State of Vital IO Handler Outputs
      */
      VIOStates stateDigitalOutputInit;

      /**
      * State of Synchronized Driver unit (SVD)
      */
      VIOStates stateSVDInit;

      /**
      * current SVD counter
      */
      uint8_t currentSVDCounter;

      /* decremented counter to determine when next CrossCompare call is due */
      uint32_t nextCrossCompare;

      /* timeout for LCD Display initialization */
      uint32_t displaySetupTimeout;

      /**
      * Variable to hold the value during initialization state machine
      */
      IOInitState ioInitState;

      /**
      * Variable to hold the value of VIURevisionId
      */
      uint32_t viuRevisionId;

      /**
      * Variable to hold the value of VOURevisionId
      */
      uint32_t vouRevisionId;

      /**
      * Variable to hold the value of AIOURevisionId
      */
      uint32_t aiouRevisionId;

      /**
      * Variable to register service brake information with AOS Analyzer
      *
      */
      uint8_t sbBrakeInfoForAnalyzer;

      /**
      * Variable to register emergency brake information with AOS Analyzer
      *
      */
      uint8_t ebBrakeInfoForAnalyzer;

      /**
      * Variable to hold if Vital driver is active order
      *
      */
      bool vitalDriverIsActiveOrder;

      /**
      * Variable to hold if Vital driver is valid
      *
      */
      ValidityState vitalDriverValidityState;

      /**
      * Variable to hold if Vital driver started to change state
      *
      */
      int64_t vitalDriverTimestamp;

      /**
      * Diagnostic value for Analyzer (bit0=Forward, bit1=Reverse)
      */
      uint8_t dirMeassurement;

      /**
      * Vital driver feedback
      *
      */
      bool vitalDriverIsActiveFeedback;

      /**
      * Flag to prevent multiple initialization of measurements
      */
      bool initMeasurementsDone;
    };
  }
}

#endif
