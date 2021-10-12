#ifndef AbstractBTMHandler_hpp
#define AbstractBTMHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The BTM Handler component deals with the interface between the OPC Agent and the AOS SW.
*  AbstractBTMHandler implements the core functionality of the component.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-29    arastogi    Created
* 2016-09-23    adgupta     Implementation for BTM Handler
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "atp_types.hpp"
#include "spl_handler.hpp"
#include "gp_list.hpp"
#include "btm_command_message.hpp"
#include "btm_status_message.hpp"
#include "btm_telegram.hpp"
#include <vfw_sync.h>
#include "event.hpp"
#include "abstract_console.hpp"
#include "channel_config.hpp"
#include <vfw_buffer.h>

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace IO
  {
    /**Class Errorhandler is being used for error code
    * of GP list */
    class ErrorHandler
    {
    public:
      /**
      * report the error
      *
      * @param[in] str - writes the error
      */
      static void report(const char_t *const str)
      {
        if (ATC::AbstractConsole::corePtr() != NULL)
        {
          ATC::AbstractConsole::corePtr()->write(str);
        }
        else
        {
          //TODO add the error handler here
          //For the time being adding printf to print any error. This should be removed later.
          //printf("%s", str);
        }

      }
    };

    class AbstractBTMHandler;
    /**
    * Static variable to store the single instance of AbstractBTMHandler
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractBTMHandler* coreBTMHandlerInstancePtr = static_cast<AbstractBTMHandler*>(NULL);
    /**
    * The class AbstractBTMHandler implements the interface defined by the IOComponent class.
    *
    */
    class AbstractBTMHandler : public ATC::IOComponent
    {
    public:

      /**
      * Implements the virtual preInit function.
      *
      */
      virtual void preInit();

      /**
      * Implements the virtual init function.
      *
      * @return true when the init is done, false otherwise.
      */
      virtual bool init();

      /**
      * Implements the virtual runIn() function.
      * Owner or scheduler shall call runOut() once per activation.
      */
      virtual void runIn();

      /**
      * Get if the Balise service is available.
      * This is API for other components
      *
      * @return true if balise service is available, else false
      */
      bool isBTMServiceAvailable() const;

      /**
      * Get the current received BTM Telegram
      *
      * @param [in] telegram Reference where the telegram value is returned
      * @return true if valid BtmTelegram is available, otherwise false
      */
      bool getBTMTelegram(BtmTelegram& telegram);

      /**
      * To handle console calls for the BTM Handler.
      * This functions parses the arguments searches for the "help"  or any other Console
      * component specific command calls and handles it. Returns true if completely handled
      * else returns false. returning false will let other components handle the call. help always returns false.
      *
      * @param[in] argc - Number of arguments in the argument array argv
      *
      * @param[in] argv - Arguments array
      *
      * @return - returns true if handled successfully, except "help"(it always returns false)
      */
      virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

      /**
      * Implements the virtual runOut() function.
      * Owner or scheduler shall call runOut() once per activation.
      */
      virtual void runOut();

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractBTMHandler* corePtr();

      /**
      * This will process the BTM telegram message and update the telegram list after processing.
      *
      * @param[in] vfwParseBuffer - VFW buffer to parse the telegram message
      */
      void processTelegramMessage(VFW_Buffer& vfwParseBuffer);

      /**
      * This will process the BTM Status message and update the member variables after processing.
      *
      * @param[in] vfwParseBuffer - VFW buffer to parse the BTM status message
      */
      void processStatusMessage(VFW_Buffer& vfwParseBuffer);

      /**
      * Set the OPC reference time offset.
      *
      * @param[in] offset - The offset to the OPC reference time
      */
      void setOpcReferenceTimeOffset(const int32_t offset);

      /**
      * Get the OPC reference time offset.
      *
      * @return Returns the offset to the OPC reference time
      */
      int32_t getOpcReferenceTimeOffset() const;

      /**
      * Check if routine test is mandatory
      *
      * @return true if routine test is mandatory
      */
      bool getIsRoutineTestMandatory() const;

      /**
      * Check if routine test is needed
      *
      * @return true if routine test is needed (and not mandatory)
      */
      bool getIsRoutineTestNeeded() const;

      /**
      * Check if routine test is possible to run
      *
      * @return true if routine test is possible
      */
      bool getIsRoutineTestPossible() const;

      /**
      * Return the remaining minutes util the routine test is mandatory
      *
      * @return the remaining minutes util the routine test is mandatory
      */
      uint16_t getRemainingTimeToMandatoryRoutineTest() const;

      /**
      * Update the BTM calendar time
      *
      * @param[in] calendarTime - The new calendar time.
      */
      void updateBtmCalendarTime(const uint32_t calendarTime) const;

      /**
      * OPC version string
      * @return the OPC version string if received, null terminated string otherwise
      */
      const OpcVersionString& getOpcVersionString() const;

    protected:

      /**
      * Constructor for the AbstractBTMHandler
      */
      AbstractBTMHandler();

      /**
      * Returns the maximum distance that we can travel after a balise was detected
      * at standstill, when BTM test is mandatory.
      *
      * Can be overridden by adaptation.
      *
      * @return The maximum distance (cm)
      */
      virtual OdoPosition getMaxDistanceFromStandStill();

    private:

      /**
      * Antenna power states
      */
      enum BtmAntennaPowerState
      {
        BtmAntennaOff = 0,
        BtmAntennaOn = 1
      };

      /**
      * BTM Routine Test State
      */
      enum BtmRoutineTestState
      {
        RoutineTestNeeded,
        RoutineTestStarted,
        RoutineTestInProgress,
        RoutineTestOK,
        RoutineTestMandatory
      };

      /**
      * BTM Service State
      */
      enum BtmServiceState
      {
        Startup,
        ServiceOK,
        ServiceFailed
      };

      /**
      * Run the Routine test state machine.
      */
      void runRoutineTestStateMachine();

      /**
      * Run the BTM Antenna Power State Machine.
      */
      void runAntennaPowerStateMachine();

      /**
      * This will populate the BTM Command message and send it.
      */
      void populateAndSendBtmCommand();

      /**
      * This will populate the BTM odo command message and send it
      */
      void populateAndSendOdoCommand();

      /**
      * Initialize the Cross compare
      */
      void initCrossCompare() const;

      /**
      * Set state to BTM Service Failed
      *
      * @param[in] errorMessage  string that describes the error
      */
      void setBaliseServiceFailed(const char_t* const errorMessage);

      /**
      * Start the routine test
      */
      void startRoutineTest();

      /**
      * Handle routine test failed
      */
      void routineTestFailed(const uint32_t timeSinceLastPassTest);

      /**
      * Validates the OPC version
      *
      * @param[in] opcVersion  version string to be validated
      *
      * @return true if the OPC version is acceptable
      */
      virtual bool validateOpcVersion(const OpcVersionString& opcVersion) const;

      /**
      * Handle BTM status message
      */
      void handleBtmStatusMessage();

      /**
      * Time out for direction controller to be in neutral. After this the BTM Antenna state
      * machine should move to Antenna Off state.(in ms)
      */
      static const int64_t timeoutValueDirectionController = 10000; // 10 Seconds

      /**
      * Timeout for command supervision in ms
      */
      static const int64_t timeoutValueCommandSupervision = 2000; // 2 Seconds

      /**
      * Timeout for routine test in ms
      */
      static const int64_t timeoutValueRoutineTest = 90000; // 90 Seconds

      /** Timeout for the Balise Present (in ms)
      */
      static const int64_t timeoutValueBalisePresent = 1000; // 1 Second

      /** Tele powering status Tele-powering on */
      static const uint8_t telePoweringOn = 1U;

      /**
      * BTM Command Options Bit0:1, Bit1:1, Bit2:1, Bit3:1, rest unused
      * Do not automatically perform a routine test at start - up
      * Do not let the results of a routine test affect BSA.
      * Send a balise report at least each 100 ms as long as a balise is present.
      * Runtime test failures shall give BSA = (FALSE)Temporary failure.*/
      static const uint8_t btmCommandOptions = 0x0FU;

      /** All bits set, but in the other order as BTM Options above. Since they all are set, the value is the same now */
      static const uint8_t btmStatusMessageOptions = 0x0FU;

      /**
      * The maximum distance that we can travel after a balise was detected
      * at standstill, when BTM test is mandatory. (cm)
      */
      static const OdoPosition maxDistanceFromStandStill = 1000;

      /** Event to report Error with sequence number/Balise number the Abstract BTM Handler */
      const ATC::Event errorSequenceOrBaliseNumber;

      /** Event to report routine test in progress */
      const ATC::Event eventRoutineTestInProgress;

      /** Event to report routine test mandatory */
      const ATC::Event eventRoutineTestMandatory;

      /** Event to report routine test succeeded */
      const ATC::Event eventRoutineTestSucceded;

      /** Event to report routine test failed */
      const ATC::Event eventRoutineTestFailed;

      /** Event to report mandatory routine test failed */
      const ATC::Event eventMandatoryRoutineTestFailed;

      /** Event to report Error if supervision of BTM fails */
      const ATC::Event eventErrorBtmSupervisionFailed;

      /** Event to report Error if BTM Telegram Format is incorrect */
      const ATC::Event eventIncorrectBTMTelegramFormat;

      /** Inform driver that balise is present at standstill */
      const ATC::Event eventBalisePresentAtStandstill;

      /** Inform driver that balise reading might be faulty */
      const ATC::Event eventBtmCouldBeFaulty;

      /** Inform driver that The test indicates that the BTM + antenna are likely to give full performance when the train starts rolling. */
      const ATC::Event eventBtmAvailabilityIsGreen;

      /** Inform driver that sporadic error occurred */
      const ATC::Event eventSporadicErrorOccurred;

      /** Inform driver about failed BTM */
      const ATC::Event informDriverFailedBtm;

      /** Current state of the btm antenna power state machine */
      BtmAntennaPowerState antennaPowerState;

      /** Current state of the routine test state machine */
      BtmRoutineTestState routineTestState;

      /** Last received valid Telegram message sequence number stored for next message checks. */
      uint8_t lastTgmSequenceNum;

      /** Current BTM Status message */
      BtmStatusMessage btmStatusMsg;

      /** Current BTM Command message */
      BtmCommandMessage btmCmdMsg;

      /** A list of BTMTelegram objects */
      typedef ATC::GPList<BtmTelegram, ATC::maxBtmTgmListSize, ErrorHandler> BtmTelegramListType;

      /** BTM Telegram list */
      BtmTelegramListType  btmTgmList;

      /** Initialize flag to know if initialization is done  */
      bool initDone;

      /** Flag to know if faulty event is reported  */
      bool eventBtmCouldBeFaultyReported;

      /** Time when Direction Controller went to neutral. */
      int64_t timeDirControllerInNeutral;

      /** Time when Direction Controller went to neutral. */
      int64_t timeTelePoweringChanged;

      /** Time when balise was present. */
      int64_t timeBalisePresent;

      /** Time when last routine test was passed*/
      uint32_t timeLastRoutineTestPassed;

      /** Is it possible to run a routine test*/
      bool isRoutineTestPossible;

      /** Last received Balise Telegram */
      BtmTelegram lastReceivedBaliseTelegram;

      /**
      * OPC reference time offset
      */
      int32_t opcReferenceTimeOffset;

      /**
      * OPC previous time stamp
      */
      uint32_t previousTimeStamp;

      /**
      * Balise Service State
      */
      BtmServiceState baliseServiceState;

      /**
      * Remaining minutes until the routine test is mandatory
      *
      */
      uint16_t remainingTimeToMandatoryRoutineTest;

      /**
      * Was a balise present at standstill?
      *
      */
      bool balisePresentAtStandStill;

      /**
      * The position at which @ref balisePresentAtStandStill was set to true.
      * Only valid when that variable is true.
      */
      OdoPosition balisePresentAtStandStillPos;

      /**
      * Indicates whether we must apply SB and then (when in standstill)
      * start the routine test even if a balise is detected.
      */
      bool forceStopAndTest;

      /**
      * SPLH connected, true if it has been connected
      *
      */
      bool splhConnected;

      /**
      * Postpone the test if not mandatory
      *
      */
      bool postponeTest;

      /**
      * Timeout when first command telegram is sent
      *
      */
      int64_t commandTelegramSentTimeout;

      /**
      * balise service available from the status message
      *
      */
      uint8_t baliseServiceAvailable;

      /**
      * BSA Counter from the status message
      *
      */
      uint8_t bsaCounter;

      /**
      * Timeout value if a sporadic error is true
      *
      */
      int64_t sporadicErrorTimeout;

      /**
      * Timeout for routine test running
      *
      */
      int64_t routineTestTimeout;

      /**
      * Flag in order not to send more than one message to the driver about a failed BTM
      *
      */
      bool informDriverFailedBtmSent;

      /**
      * Flag in order not to send more than one event to RU.
      *
      */
      bool isTelePoweringMismatching;
    };
  }
}

#endif
