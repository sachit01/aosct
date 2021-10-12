#ifndef Brake_hpp
#define Brake_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file defines the adaptation class for brake handling.
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
#include "abstract_brake.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace Supv
  {
    /**
    * Feedback result type
    */
    enum FeedbackTestResult
    {
      Inactive  = 0,
      Active    = 1,
      DontCare  = 2
    };

    /**
    * Order- and Expected feedback status for brake-test
    */
    struct BrakeTestOrderStatusAdap {
      bool tcoTestApplied;  //!< Indicates that TCO order shall be applied
      bool eb1TestApplied;  //!< Indicates that EB1 order shall be applied
      bool eb2TestApplied;  //!< Indicates that EB2 order shall be applied 
      enum FeedbackTestResult tcoExpectedFbWithOnlyFb;      //!< The expected TCO Feedback when only TCO Feedback is used
      enum FeedbackTestResult tcoExpectedFbWithOrderAndFb;  //!< The expected TCO Feedback when both TCO Order and TCO Feedback is used
      enum FeedbackTestResult ebExpectedFB;                 //!< The expected EB Feedback
      bool releaseEvaluation;     //!< Release evaluation
    };

    /**
    * The class Brake implements the adaptation logic for brake calculations
    * used by the AOS.
    *
    */
    class Brake : public AbstractBrake
    {
    public:

      /**
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      */
      static Brake& instance(void);

      /**
      * Implements the virtual run function.
      */
      virtual void run(void);

      /**
      * Implements the virtual init function.
      */
      virtual bool init();

      /** Safety halt raised due to fault in EB cut-out inputs, Brake Test is now mandatory after restart! */
      static const BrakeTestReasonType brakeTestReasonEbCutOut = 11U;

      /** Safety halt raised due to Eb Feedback failure, Brake Test is now mandatory after restart! */
      static const BrakeTestReasonType brakeTestReasonEbFeedbackFault = 12U;

      /** Safety halt raised due to fault with TCO Orders, Brake Test is now mandatory after restart! */
      static const BrakeTestReasonType brakeTestReasonTCOOrderFault = 13U;

      /** Safety halt raised due to TCO Feedback failure, Brake Test is now mandatory after restart! */
      static const BrakeTestReasonType brakeTestReasonTCOFeedbackFault = 14U;

      /** Return the TCO signal output value
      *
      * Called by LocoIO to determine the output value for the TCO signal,
      * adaptation for BHP
      */
      bool getTcoApplied(void) const;

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
      * Access function for TCO order during Brake Test
      *
      * @ return True if TCO order is applied during Brake Test
      */
      bool getTcoTestApplied(void) const;

      /**
      * Access function for Rapid loss of brake pressure detected
      *
      * @ return True, if Rapid loss of brake pressure detected
      */
      bool getRapidLossInBrakePressureDetected(void) const;

      /**
      * Indicates whether SB brake test is requested
      *
      * @return true if SB Brake Test is requested
      */
      bool getSbBrakeTestRequested(void) const;

      /**
      * Reads the two Brake pressure valves and evaluates their validity.
      * @param[out] brakePressurevalue  The evaluated Brake Pressure value
      * @return True if the evaluated Brake pressure Values are valid.
      */
      bool getBrakePressureFb(uint16_t &brakePressurevalue) const;

    protected:

      /**
      * Verify EB feedback from the Brake Pressure valve and verify it against the expected value.
      * @ return True if the EB feedback matches the expected brake pressure level.
      */
      bool verifyExternalEbFeedback(void);

      /**
      * Read TCO feedback from the I/O and verify it against the expected value.
      * @ return True if the TCO feedback matches the expected order value (depending on configuration, EB or TCO order).
      */
      bool verifyExternalTcoFeedback(void);

      /**
      * Read EB feedback from the Brake Pressure sensors and verify against the expected value.
      * Issue a Safety Halt if feedback not as expected.
      */
      virtual void checkEbFeedback(const int64_t timeSinceEbApplied);

      /**
      * Read TCO feedback
      * Issue a Safety Halt if feedback not as expected.
      */
      virtual void checkTcoFeedback(const int64_t timeSinceTcoApplied);

      /**
      * Get the Max number of allowed cycles to wait for the expected EB feedback.
      * This in turn calls the getEbFeedbackTimeout() in Config to obtain the EbFeedbackTimeout
      * 
      * @return maximum number of wait cycles for the expected EB feedback.
      */
      virtual uint32_t getMaxEbFbWaitCycle() const;

      /**
      * Get the Max number of allowed cycles to wait for the expected TCO feedback.
      * This in turn calls the getTcoFeedbackTimeout() in Config to obtain the TcoFeedbackTimeout
      * 
      * @return maximum number of wait cycles for the expected TCO feedback.
      */
      virtual uint32_t getMaxTcoFbWaitCycle() const;

      /**
      * Function to process EB Cut-Out inputs.
      */
      virtual void processEbCutOut(void);

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

      /**
      * Brake-test state-machine to perform all steps in the brake-test sequence
      *
      * @return True when state-machine has finished.
      */
      virtual bool processBrakeTestStateMachine();

      /**
      * Function to evaluate the need to abort a Brake Test
      * @return True if Brake test should be aborted
      */
      virtual bool abortBrakeTest(void);

      /**
      * Read and verify the EB1,EB2 and TCO relays and/or the external feedback.
      *
      * @return True if the relays/feedback match the expected values.
      */
      virtual bool verifyBrakeTestFeedback();

      /**
      * Reset the flags and sub-states used while executing a Brake Test
      */
      virtual void resetBrakeTestStateMachine(void);

    private:
      /**
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      Brake();

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      Brake(const Brake&);

      /**
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      Brake& operator = (const Brake&);

      /**
      * Read and verify if the TCO relay reflects the expected value.
      * @return True is the relay matches the expected TCO value
      */
      bool verifyInternalTCORelays() const;
      
      /**
      * Function to check if condition to apply TCO is set or not
      */
      bool isApplyTco(void) const;

      /**
      * Function to process TCO feedback
      */
      virtual void processTco(void);

      /**
      * Invalid value of Brake pressure.
      */
      static const uint16_t invalidBrakePressureVal = 0xFFFFU;

      /**
      * An event to report when EB feedback from Brake Pressure sensor failed during EB application
      */
      const ATC::Event ebFeedbackBrakePressureFailed;

      /**
      * An event to report when TCO feedback failed during TCO application
      */
      const ATC::Event tcoFeedbackFailed;

      /**
      * Safety Halt event to report when Emergency Brakes have been cut-out (when configured for use)
      */
      const ATC::Event emergencyBrakesCutOut;

      /**
      * Safety Halt event to report when there is an error reading the EB Cut-out inputs (when configured for use)
      */
      const ATC::Event errorReadingEbCutOutInputs;

      /**
      * Log event when Brake Test Fails due to erroneous external EB feedback
      */
      const ATC::Event brakeTestFailedEbExternal;

      /**
      * An event to report when TCO relay feedback test failed
      */
      const ATC::Event tcoRelayFeedbackErr;
      
      /**
      * Log event when Brake Test Fails due to internal TCO health supervision
      * failure
      */      
      const ATC::Event brakeTestFailedTcoInternal;
      
      /**
      * Log event when Brake Test Fails due to erroneous external TCO feedback
      */
      const ATC::Event brakeTestFailedTcoExternal;

      /**
      * Brake pressure Feedback invalid: The Analog input Data values are not Valid.
      */
      const ATC::Event bpFeedbackInvalid;

      /**
      * Brake pressure Feedback inconsistent: The Analog data being received are not consistent with the Brake orders. brake pressure
      * too low for EB NOT ordered condition.
      */
      const ATC::Event bpFeedbackInconsistent;

      /**
      * Brake pressure Feedback inaccurate: BP1 and BP2 are not same upto the tolerable(configured) value.
      */
      const ATC::Event bpFeedbackInaccurate;

      /**
      * Number of steps in the brake-test
      */
      static const uint8_t numberOfBrakeTestStepsAdap = 7U;

      /**
      * Number of consecutive readings of the input that shall indicate the expected state.
      */
      static const uint8_t numberOfConsecutiveReadingsFeedback = 5U;

      /**
      * Number of positions in the EB pressure history list
      */
      static const uint8_t brakePressureEbHistoryListSize = 10U;

      /**
      * Previous cycle TCO order
      */
      bool previousCycleExpectedTcoValue;

      /**
      * Previous cycle EB order
      */
      bool previousCycleExpectedEbValue;

      /**
      * The boolean to indicate if TCO is applied during a Brake Test.
      *
      */
      bool tcoTestApplied;
      
      /**
      * Counter for number of valid consecutive readings of EB feedback
      */
      uint8_t ebFeedbackOkCount;

      /**
      * Counter for number of valid consecutive readings of TCO feedback
      */
      uint8_t tcoFeedbackOkCount;

      /**
      * The boolean to indicate if TCO is applied or not.
      */
      bool tcoApplied;

      /**
      * Start time when TCO is applied
      */
      int64_t tcoStartTime;

      /**
      * The boolean to indicate if Rapid loss of brake pressure detected.
      */
      bool rapidPressureLossDetected;

      /**
      * Cycle counter to know when to recheck the Rapid Loss variable
      */
      uint16_t cycleRapidLossCnt;

      /**
      * Cycle counter to know when to recheck the Inconsistent Brake pressure values from 2 sensors.
      */
      uint16_t cycleInconsistentBpCnt;

      /**
      * Brake pressure 1 saved for the last cycle
      */
      uint16_t savedBP1;

      /**
      * Brake pressure 2 saved for the last cycle
      */
      uint16_t savedBP2;

      /**
      * Flag to know that the Brake pressure is getting built up after EB application
      */
      bool ebPressureBuildUpActive;
      
      /**
      * Diagnostic value for Analyzer (bit0=ebExtFeedbackOk, bit1=tcoExtFeedbackOk, bit2=ebIntFeedbackOk, bit3=tcoIntFeedbackOk)
      */
      uint8_t ebTcoFeedbackMeassurement;

      /**
      * Diagnostic value for Analyzer (bit0=eb1Order, bit1=eb2Order, bit2=tcoOrder)
      */
      uint8_t ebTcoOrderMeassurement;

      /**
      * Previous step in brake-test
      */
      uint8_t oldBrakeTestSequenceStep;

      /**
      * History list to keep track of EB pressure history
      */
      uint16_t brakePressureEbHistory[brakePressureEbHistoryListSize];

      /**
      * Current position of the EB pressure history list
      */
      uint8_t brakePressureEbHistoryIndex;

      /**
      * Function to evaluate Rapid Pressure Loss
      */
      void evaluateRapidPressureLoss(void);

      /**
      * Function to evaluate Brake pressure feedback when Eb is not ordered
      */
      void evaluateBrakePressureFeedbackNoEB(void);

      /**
      * Flag to prevent multiple initialization.
      */
      bool adaptationInitDone;
    };
  }
}
#endif
