#ifndef AbstractBrake_hpp
#define AbstractBrake_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
* This file defines AbstractBrake class which contains the core braking logic 
* used by the AOS. 
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
* 2016-04-21    lantback    Implemented corePtr()
* 2016-07-18    saprasad    Added SB & EB Brake function prototype and variables.
* 2016-07-27    saprasad    Added testing function prototype ,Fixed review comments
*                           Fixes Lint Error .
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-19    arastogi    Speeds should be unsigned
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "atp_types.hpp"
#include "event.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Supv
  {
    enum BrakeTestStatus
    {
      /** Brake Test not started yet */
      BrakeTestStatusIdle = 0,
      /** Brake Test in Progress */
      BrakeTestStatusInProgress = 1,
      /** Brake Test Aborted */
      BrakeTestStatusAborted = 2,
      /** Brake Test Failed */
      BrakeTestStatusFail = 3,
      /** Brake Test Complete and Successful */
      BrakeTestStatusSuccessful = 4
    };

    class AbstractBrake;
    /** 
    * Static variable to store the single instance of AbstractBrake
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only 
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractBrake* coreBrakeInstancePtr = static_cast<AbstractBrake*>(NULL);

    /**
    * The class AbstractBrake implements the core logic for applying, releasing.
    * and supervising emergency and service brakes.
    */
    class AbstractBrake : public ATC::ProcComponent
    {
    public:
      /**
      * Implements the init function.
      *
      * @return Returns true when initialization completed
      */
      virtual bool init(void);

      /**
      * Implement the run function.
      * @return nothing  when run function completed
      */
      virtual void run(void);

      /**
      * Interface to call different level of Console Command
      *
      * @param[in] argc  Number of arguments in the argument array argv
      * @param[in] argv  Arguments array
      *
      * @return true if the Call is successful.
      */
      virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);
      
      /**
      * Getter function for ebApplied.
      * @return Returns true when EB brake is applied
      */
      bool getEbApplied(void) const;

      /**
      * Getter function for sbApplied.
      * @return Returns true when SB brake is applied
      */
      bool getSbApplied(void) const;

      /**
      * Getter function for ebReleaseEnable.
      @return Returns true when EbRelease is Enable
      */
      bool getEbReleaseEnable(void) const;

      /**
      * Getter function for sbReleaseEnable.
      * @return Returns true when SbRelease is Enable
      */
      bool getSbReleaseEnable(void) const;

      /**
      * Getter function to read the Brake Test status.
      * @ return Returns the current status of Brake Test
      */
      BrakeTestStatus getBrakeTestStatus(void) const;

      /**
      * Access function which returns True if EB1 is applied
      * during a Brake Test.
      */
      bool getEb1TestApplied(void) const;

      /**
      * Access function which returns True if EB2 is applied
      * during a Brake Test.
      */
      bool getEb2TestApplied(void) const;

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractBrake* corePtr();

    protected:
      /**
      * Default Constructor
      */
      AbstractBrake();

      /**
      * Pure Virtual function to process EB Cut-Out inputs.
      */
      virtual void processEbCutOut(void) = 0;

      /**
      * Get the Max number of allowed cycles to wait for the expected internal feedback.
      *
      * @return Max number of wait cycles.
      */
      uint32_t getMaxInternalRelaysFbWaitCycle() const;

      /**
      * Pure virtual function to check the EB external feedback and take any action needed.
      */
      virtual void checkEbFeedback(const int64_t timeSinceEbApplied) = 0;

      /**
      * Function to check the TCO external feedback and take any action needed.
      */
      virtual void checkTcoFeedback(const int64_t timeSinceTcoApplied) = 0;

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

      /**
      * Function to evaluate the need to abort a Brake Test
      * @return True if Brake test should be aborted
      */
      virtual bool abortBrakeTest(void);

      /**
      * Reset the flags and sub-states used while executing a Brake Test
      */
      virtual void resetBrakeTestStateMachine(void);

      /**
      * Read and verify the corresponding feedback signals during brake-test.
      *
      * @return True if feedback is validated ok.
      */
      virtual bool verifyBrakeTestFeedback();

      /**
      * Brake-test state-machine
      */
      virtual bool processBrakeTestStateMachine();

      /**
      * Read and verify if the internal EB1 and EB2 relays reflect the expected values.
      *
      * @return True is the relays match the expected EB1 and EB2 values
      */
      bool verifyInternalEbRelays() const;

      /**
      * Perform test of vital driver
      */
      void performVitalDriverTest();

      /**
      * min slip side time(ms)
      */
      static const uint16_t tMinSlipSide = 250U;

      /**
      * max slip side time(ms)
      */
      static const uint16_t tMaxSlipSide = 3000U;

      /**
      * Train stopped conditions when velocity is zero
      */
      static const uint16_t trainStopVelocity = 0U;
      
      /** The boolean to indicate if EB1 is applied during a Brake Test.
      *
      */
      bool eb1TestApplied;

      /**
      * The boolean to indicate if EB2 is applied during a Brake Test.
      *
      */
      bool eb2TestApplied;

      /**
      * The boolean to indicate if any eb is Applied or not.
      *
      */
      bool ebApplied;

      /**
      * The boolean to indicate if any sb is Applied or not.
      *
      */
      bool sbApplied;

      /**
      * The boolean to indicate if any ebRelease is Enable or not.
      *
      */
      bool ebReleaseEnable;

      /**
      * The boolean to indicate if any sbRelease is Enable or not.
      *
      */
      bool sbReleaseEnable;

      /**
      * SBReleaser variable to indicate SBcurrent Releaser name.
      *
      */
      ATC::SBReleaser currentSBReleaser;

      /**
      * EBReleaser variable to indicate EBcurrent Releaser name.
      *
      */
      ATC::EBReleaser currentEBReleaser;

      /**
      Maximum time ,when SB reached for supervision case
      */
      int64_t tsbDecMaxTime;

      /**
      * timeSbV0AddInZoneD
      */
      int64_t timeSbV0AddInZoneD;

      /**
      *  when Slip- Side Detected
      */
      bool bSlipSideDetected;

      /**
      * Maximum time ,when Feedback Relay condition reached
      */
      int64_t tebEBRDecMaxTime;

      /**
      * Maximum time ,when EB reached for supervision case 
      */
      int64_t tebDecMaxTime;

      /**
      * Start time when EB is applied
      */
      int64_t tebStartTime;

      /**
      * timeEbV0AddInZoneD
      */
      int64_t timeEbV0AddInZoneD;
      
      /**
      * Current active step in brake-test
      */
      uint8_t brakeTestSequenceStep;

      /**
      * Standstill event when Brake Test is in Progress
      */
      const ATC::Event brakeTestInProgress;

      /**
      * Standstill event before the start of a Brake Test
      */
      const ATC::Event startingBrakeTest;

      /**
      * An event to report when emergency brake failed
      */
      const ATC::Event ebBadReadingErrAcceleration;

      /**
      * An event to report an EB when insufficient deceleration is observed after SB
      */
      const ATC::Event ebWhenInsufficientSBDeceleration;

      /**
      * An event to report when EB relay feedback test failed
      */
      const ATC::Event brakeRelayFeedbackErr;

      /**
      * Log event when Brake Test Execution Timeout
      */
      const ATC::Event brakeTestExecTimeout;

      /**
      * Standstill event when Brake Test Fails
      */
      const ATC::Event brakeTestFailed;

      /**
      * Standstill event when Brake Test is aborted
      */
      const ATC::Event brakeTestAborted;

      /**
      * Log event when Brake Test Fails due to EB internal health supervision
      * failure
      */
      const ATC::Event brakeTestFailedEbInternal;

      /**
      * DMI event when Cabin is deactivated during Brake Test
      */
      const ATC::Event cabinDeactivated;

      /**
      * DMI event when additional Brake orders are active during Brake Test
      */
      const ATC::Event additionalBrakeOrders;

      /**
      * DMI event when Brake Test is aborted by the driver
      */
      const ATC::Event brakeTestAbortedByDriver;

      /**
      * DMI event when Emergency Stop Active Signal is detected during Brake Test
      */
      const ATC::Event emergencyAlertActive;

      /**
      * Log event when Brake Test is successful
      */
      const ATC::Event brakeTestSuccessful;

      /**
      * Log event when Unable to Start Brake Test
      */
      const ATC::Event unableToStartBrakeTest;

      /**
      * Cycle counter to monitor the feedback time for the expected feedback
      */
      uint32_t feedbackWaitCycle;
      
      /**
      * Current Brake Test Status
      */
      BrakeTestStatus brakeTestStatus;

      /**
      * Cycle counter to monitor the Brake test execution time
      */
      uint32_t currentBrakeTestCycle;

      /**
      * Max execution time for a complete Brake Test
      */
      uint32_t maxBrakeTestExecCycle;

      /**
      * Brake test started
      */
      bool brakeTestStarted;

      /**
      * Percentage multiplier for the calculating the expected deceleration limit of SB and EB
      */
      static const int8_t percentageMultiplier = -100;

      /**
      * Number of cycles per second
      */
      static const uint8_t noOfCyclePerSecond = 10U;

      /**
      * Initial brake-step
      */
      static const uint8_t BrakeTestSequenceStepInit = 0U;
      
    private:

      /**
      * Different sources of orders for brake-test
      */
      struct BrakeTestOrderStatus
      {
        /**
        * The boolean to indicate if EB1 applied during a Brake Test.
        *
        */
        bool eb1TestApplied;
        /**
        * The boolean to indicate if EB2 applied during a Brake Test.
        *
        */
        bool eb2TestApplied;
      };

      /**
      * Vital driver test states
      */
      enum VitalDriverTestState
      {
        TestIdle,
        WaitForVitalOutputDeactivation,
        VitalDriverTestSucceeded
      };

      /**
      * Run the service brake process.
      */
      void processServiceBrake(void);

      /**
      * Run the emergency brake process.
      */
      void processEmergencyBrake(void);

      /**
      * Run checkAndApplySBrakeReq.
      */
      void checkAndApplySBrakeReq(void);

      /**
      * Run performSBrakeReleaseControl.
      */
      void performSBrakeReleaseControl(void);

      /**
      * Run performSBrakeDecControl.
      */
      void performSBrakeDecControl(void);

      /**
      * Run checkAndApplyEBrakeReq.
      */
      void checkAndApplyEBrakeReq(void);

      /**
      * Run performEBrakeReleaseControl.
      */
      void performEBrakeReleaseControl(void);

      /**
      * Run performEBrakeDecControl.
      */
      void performEBrakeDecControl(void);

      /**
      * reset SB parameter.
      */
      void resetSBParams(void);

      /**
      * reset EB parameter.
      */
      void resetEBParams(void);

      /**
      * Perform the Brake Test Sequence
      */
      void performBrakeTest(void);

      /**
      * Function to check TCO feedback
      */
      virtual void processTco(void) = 0;

      /**
      * calculate Expected Deceleration.
      *
      * @param[in] accValue Deceleration from the brakes.
      * @param[in] upperSpeedDec Upper Speed boundary value
      * @return   Expected deceleration based on gradient and speed.
      */
      int16_t getExpectedDeceleration(const uint16_t accValue, const uint32_t upperSpeedDec)const;

      /**
      * Calculate expected deceleration with gradient.
      *
      * @param[in] accValue Deceleration from the brakes.
      * @return   Expected deceleration after taking into account the gradient.
      */
      int16_t addGradToConstAcc(const uint16_t accValue) const;

      /**
      * Set sb releaser as per current request.
      * @return true if the brake can be released false otherwise.
      */
      bool isSBReleaseReqActive();

      /**
      * Set eb releaser as per current request.
      * @return true if the brake can be released false otherwise.
      */
      bool isEBReleaseReqActive();

      /**
      * Access function to return if SB releaser is Dispatcher or not
      */
      bool getBrakeRelReqDispatcherSB() const;

      /**
      * Access function to return if SB releaser is ATO or not
      */
      bool getBrakeRelReqATOSB() const;

      /**
      * Access function to return if SB releaser is Driver or not
      */
      bool getBrakeRelReqDriverSB() const;

      /**
      * Access function to fetch the value for current SB releaser
      */
      ATC::SBReleaser getcurrentSBReleaser() const;

      /**
      * Access function to return if EB releaser is Dispatcher or not
      */
      bool getBrakeRelReqDispatcherEB() const;

      /**
      * Access function to return if EB releaser is Driver or not
      */
      bool getBrakeRelReqDriverEB() const;

      /**
      * Access function to fetch the value for current EB releaser
      */
      ATC::EBReleaser getCurrentEBReleaser() const;

      /**
      * Handle state waitForDriverFeedback
      */
      void waitForVitalOutputDeactivation();

      /**
      * Number of steps in brake-test
      */
      static const uint8_t numberOfBrakeTestSteps = 5U;

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * Counter for bad reading when current acceleration is 60% of expected acceleration
      * in SB Case.
      */
      uint16_t cntLowRetSamplesInRowSB;

      /**
      * Number of bad reading ,allowed when present acceleration is at least 60% (3/5) of
      * the expected deceleration in one SB Cycle.
      */
      uint16_t cntLowRetSamplesInOneSB;

      /**
      * Number of bad reading ,allowed when present acceleration is at least 60% (3/5) of
      * the expected deceleration in EB Case.
      */
      uint16_t cntLowRetSamplesInRowEB;

      /**
      * Number of bad reading ,allowed when present acceleration is at least 60% (3/5) of
      * the expected deceleration in one EB Cycle.
      */
      uint16_t cntLowRetSamplesInOneEB;

      /**
      * An event to reportevent when EB brake applied is failed
      */
      int64_t tInhibitTime;

      /**
      * Vital driver test state
      */
      VitalDriverTestState vitalDriverTestState;

      /**
      * Vital driver test cycle counter
      */
      uint8_t vitalDriverTestCycleCounter;

      /**
      * Flag to store SB escalated to EB scenario.
      */
      bool sbEscalatedToEB;

     };

  }
}
#endif
