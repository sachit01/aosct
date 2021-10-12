#ifndef AbstractModeControl_hpp
#define AbstractModeControl_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Abstract Mode Control component is responsible for the current operating mode
*  of the system. It will manage the current mode and transitions between the modes.
*  It will also manage and execute independent state machines called sequences.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-20    arastogi      Created
* 2016-10-17    arastogi      Added ATP reset variable access functions.
* 2017-01-12    saprasad      Added event when register measurement for Analyzer IF.
* 2017-04-11    skothiya      Updated for cabin handling and authorization requirement implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "train_config_mode.hpp"
#include "power_up_mode.hpp"

#include "train_registration_mode.hpp"
#include "balise_search_mode.hpp"
#include "emergency_alert_seq.hpp"
#include "driver_login_seq.hpp"
#include "shunting_route_mode.hpp"
#include "staff_responsible_mode.hpp"
#include "join_mode.hpp"
#include "split_mode.hpp"
#include "shunting_mode.hpp"
#include "yard_mode.hpp"
#include "sleeping_mode.hpp"
#include "location_mode.hpp"
#include "possession_mode.hpp"
#include "possession_mode_request_seq.hpp"
#include "yard_mode_request_seq.hpp"
#include "shunting_mode_request_seq.hpp"
#include "config_mode_request_seq.hpp"
#include "abstract_loco_io.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    class AbstractModeControl;

    /**
    * Static variable to store the single instance of AbstractOdometry
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractModeControl* coreModeControlInstancePtr = static_cast<AbstractModeControl*>(NULL);

    /**
    * Constants related to Cabin Handling
    */
    static const uint8_t cabinAOnly = 1U;

    /**
    * The class AbstractModeControl implements the interface defined by the ComponentBase class.
    *
    */
    class AbstractModeControl : public ATC::ProcComponent
    {
    public:
      /**
      * Constants related to Powering down Possession Message Timeout(seconds)
      */
      static const uint32_t poweringDownPosMessageTimeoutSec = 30U;

      /**
      * Constants related to Powering down Blinking Duty (Milli seconds)
      */
      static const uint32_t poweringDownBlinkDutyMs = 1000U * 2U;

      /**
      * Constants related to Powering down number of Possession Messages
      */
      static const uint32_t poweringDownNumPosMessages = 2U;

      /**
      * Implements the virtual init function.
      */
      virtual bool init(void);

      /**
      * Implements the virtual run function.
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
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractModeControl* corePtr();

      /**
      * Get current operating mode
      *
      * @return Enum of the current operating mode.
      */
      ATPMode getCurrentMode() const;

      /**
      * Get previous operating mode
      *
      * @return Enum of the previous operating mode.
      */
      ATPMode getPreviousMode() const;

      /**
      * Get the internal state of mode
      *
      * @return current state of mode.
      */
      ModeState getModeState(const ATPMode atpMode) const;

      /**
      * Get the current ATO mode
      *
      * @return current ATO Mode
      */
      ATOMode getATOMode() const;

      /**
      * Get the previous ATO mode
      *
      * @return previous ATO Mode
      */
      ATOMode getPreviousATOMode() const;

      /**
      * Get the internal state of PowerUp mode
      *
      * @return current state of PowerUpMode.
      */
      PowerUpModeState getPowerUpModeState() const;

      /**
      * Get the internal state of TrainConfig mode
      *
      * @return current state of TrainConfig.
      */
      TrainConfigModeState getTrainConfigModeState() const;

      /**
      * Get the internal state of TrainRegistration mode
      *
      * @return current state of TrainRegistration.
      */
      TrainRegistrationModeState getTrainRegistrationModeState() const;

      /**
      * Get the internal state of BaliseSearch mode
      *
      * @return current state of BaliseSearch.
      */
      BaliseSearchModeState getBaliseSearchModeState() const;

      /**
      * Get the internal state of Staff Responsible mode
      *
      * @return current state of Staff responsible.
      */
      StaffResponsibleModeState getStaffResponsibleModeState() const;

      /**
      * Get the internal state of Join mode
      *
      * @return current state of Join mode.
      */
      JoinModeState getJoinModeState() const;

      /**
      * Get the internal state of Split mode
      *
      * @return current state of Split mode.
      */
      SplitModeState getSplitModeState() const;

      /**
      * Get the internal state of Shunting mode
      *
      * @return current state of Shunting mode.
      */
      ShuntingModeState getShuntingModeState() const;

      /**
      * Get the internal state of Yard mode
      *
      * @return current state of Yard mode.
      */
      YardModeState getYardModeState() const;

      /**
      * Get the internal state of Possession mode
      *
      * @return current state of Possession mode.
      */
      PossessionModeState getPosModeState() const;

      /**
      * Get the internal state of Sleeping mode
      *
      * @return current state of Sleeping mode.
      */
      SleepingModeState getSleepingModeState() const;

      /**
      * Get the internal state of location mode
      *
      * @return current state of location mode.
      */
      LocationModeState getLocationModeState() const;

      /**
      * Get the internal sequence state of DriverLogin sequence
      *
      * @return current state of DriverLogin.
      */
      DriverLoginState getDriverLoginSeqState() const;

      /**
      * Get the internal sequence state of Emergency alert sequence
      *
      * @return current state of Emergency alert.
      */
      EmergencyAlertState getEmergencyAlertSeqState() const;

      /**
      * Get the internal sequence state of config mode request sequence
      *
      * @return current state of config Mode Request Sequence.
      */
      ModeRequestSeqState getConfigModReqSeqState() const;

      /**
      * Get the internal sequence state of shunting mode request sequence
      *
      * @return current state of shunting Mode request sequence.
      */
      ModeRequestSeqState getShuntModReqSeqState() const;

      /**
      * Get the internal sequence state of yard mode request sequence
      *
      * @return current state of yard mode request sequence.
      */
      ModeRequestSeqState getYardModReqSeqState() const;

      /**
      * Get the internal sequence state of possession mode request sequence
      *
      * @return current state of possession mode request sequence.
      */
      ModeRequestSeqState getPosModReqSeqState() const;

      /**
      * Get if the last train configuration was new configuration
      *
      * @return true if new configuration, false if re configuration.
      */
      bool isNewTrainConfiguration() const;

      /**
      * Getter function for idle train state
      *
      * @return true if idle state active.
      */
      bool getIdleState() const;

      /**
      * Getter function for idle train state
      *
      * @return true if Handling Done state is active
      */
      bool getHandlingDone() const;

      /**
      * Getter function for Free Rolling status
      *
      * @return true if Free Rolling train state is active
      */
      bool getFreeRolling() const;

      /**
      * Getter function for Odometer Invalid status
      *
      * @return true Odometer Invalid train state is active
      */
      bool getOdometerInvalid() const;

      /**
      * Get the value of MA Timeout.
      *
      * @return true if MA has timed out false otherwise
      */
      bool getMATimeOut(void) const;

      /**
      * Get the active cab.
      *
      * @return the cab active value.
      */
      CabActiveStatus getActiveCab(void) const;

      /**
      * Get the previous active cab.
      *
      * @return the previous cab active value.
      */
      CabActiveStatus getPrevActiveCab(void) const;

      /**
      * Process the transition to ATP Mode Safety Halt.
      */
      void gotoATPModeSafetyHalt();

      /**
      * Get the status if request shall be sent for yard mode change.
      *
      * @return  true if request for yard mode shall be sent.
      */
      bool timeToSendYardRequest() const;

      /**
      * Get the status if request shall be sent for possesion mode change.
      *
      * @return  true if request for possesion mode shall be sent.
      */
      bool timeToSendPossesionReqest() const;

      /**
      * Get the status if request shall be sent for shunting mode change.
      *
      * @return  true if request for shunting mode shall be sent.
      */
      bool timeToSendShuntRequest() const;

      /**
      * To get string for mode.
      *
      * @param[in] atpMode mode enum to get string for.
      * @param[out] buffer the mode string is copied to the buffer
      */
      void getModeString(const ATPMode atpMode, char_t* const buffer) const;

      /**
      * To indicate if ATP has been reset.
      *
      * @return true if ATP has been reset, false otherwise
      */
      bool getATPReset() const;

      /**
      * To indicate if ATP needs reset.
      *
      * @return true if ATP needs reset, false otherwise
      */
      bool getATPNeedsReset() const;

      /**
      * To indicate if the stop train message is received from TCC.
      *
      * @return true when Stop train message is received and until the train stop,
      *         false otherwise
      */
      bool getStopTrainActive() const;

      /**
      * To indicate  ATPOK Status.
      *
      * @return true if ATPOK signal is set, false otherwise
      */
      bool getATPOKStatus() const;

      /**
      * To indicate Lamp Status.
      *
      * @return true if lamp status flag is set
      */
      bool getLampStatus() const;

      /**
      * To indicate Brake Test is requested to the Brake Component
      *
      * @return true if Brake Test is requested
      */
      bool getBrakeTestRequested() const;

      /**
      * To indicate that it is time for Mandatory Brake Test.
      *
      * @return true if Brake Test is mandatory
      */
      bool getBrakeTestMandatory() const;

      /**
      * To indicate that Brake Test is possible, although not mandatory.
      *
      * @return true if Brake Test is possible
      */
      bool getBrakeTestPossible() const;

      /** Get buzzer type
      *
      * @returns buzzer type requested by mode
      */
      BuzzerType getBuzzerRequest(void) const;

      /**
      * Access function to obtain the time remaining until the next mandatory Brake test
      *
      * @return Time until next Mandatory Brake test in minutes
      */

      uint16_t getRemainingTimeToMandatoryBrakeTest() const;

      /**
      * To indicate if it is time to notify the Driver about the next mandatory brake test.
      *
      * @return true if it is time to notify the driver about the next mandatory Brake Test.
      */
      bool getBrakeTestNotification() const;

      /**
      * Get the internal mode state of Shunting Route mode
      *
      * @return current mode state of Shunting Route.
      */
      TrainShuntingRouteModeState getShuntingRouteModeState() const;

      /**
      * Get the valid status of Q route type in current mode
      *
      * @return true if the Q route type is valid
      */
      bool isValidQRouteType(const RouteType routeType) const;

      /**
      * To check the validity of unconditional shortening message in respective modes
      *
      * @return true if unconditional shortening message is valid in respective modes
      */
      bool isValidUncondShorteningMsg() const;

      /**
      * Log powering down sequence start via writeLog, called by PowerDownMode
      */
      void writePowerDownLogMessage(void) const;

      /**
      * Default timeout while waiting for 2 positional messages to be sent
      *
      * @return timeout in seconds
      */
      virtual uint32_t getPoweringDownPosMessageTimeoutSec(void) const;

      /**
      * Default number of positional message to wait for before going into PowerDownMode
      *
      * @return number of message to wait for
      */
      virtual uint32_t getPoweringDownNumPosMessages(void) const;

      /**
      * Default duration until lamp toggle during powering down in milliseconds
      *
      * @return duty cycle in ms
      */
      virtual uint32_t getPoweringDownBlinkDutyMs(void) const;

      /**
      * return the signal out value of Power off used by LocoIO
      *
      * @return value of Power off signal
      */
      bool getPowerOffValue(void) const;

      /**
      * To indicate 'start up'
      *
      * @return true the startup has passed
      */
      bool getStartUpPassed() const;

      /**
      * Evaluate if sleep-mode is allowed
      *
      * @return true if allowed to enter SleepMode
      */
      bool isAllowedToEnterSleepMode() const;

      /**
      * Evaluate if configuration-mode is allowed
      *
      * @return true if allowed to enter configuration-mode
      */
      bool isAllowedToEnterConfigMode() const;

      /**
      * Evaluate if possession-mode button is allowed on dmi
      *
      * @return true if allowed to display the possession mode buttons on dmi
      */
      bool isAllowedToEnterPosMode() const;

      /**
      * Evaluate if shunting-mode button is allowed on dmi
      *
      * @return true if allowed to display the shunting mode buttons on dmi
      */
      bool isAllowedToEnterShuntMode() const;

      /**
      * Evaluate if yard-mode button is allowed on dmi
      *
      * @return true if allowed to display the yard mode buttons on dmi
      */
      bool isAllowedToEnterYardMode() const;

      /**
      * Evaluate if login button is allowed on dmi
      *
      * @return true if allowed to display the login buttons on dmi
      */
      bool isAllowedToLogin() const;

      /**
      * Evaluate if handling done button is allowed on DMI
      *
      * @return true if allowed to display the handling buttons on DMI
      */
      virtual bool isAllowedToDisplayHandlingDone() const;

      /**
      * Evaluate if free rolling button is allowed on DMI
      *
      * @return true if allowed to display the free rolling buttons on DMI
      */
      bool isAllowedToDisplayFreeRolling() const;

      /**
      * Evaluate if confirm free rolling button is allowed on DMI
      *
      * @return true if allowed to display the confirm free rolling buttons on DMI
      */
      bool isAllowedToDisplayConfirmFreeRolling() const;

      /**
      * Evaluate if Abort setup button is allowed on dmi
      *
      * @return true if allowed to abort the setup
      */
      bool isAllowedToAbortSetup() const;

      /**
      * Toggle with default values of frequency
      *
      * @return aosStatusToggleFrequency in sec/10 (100 millisecond).
      */
      virtual uint8_t getToggleFrequencyAOSStatus(void) const;

      /**
      * Get status Inhibit the EB and SB by checking if ATP is in sleeping mode and in a state that inhibits brake-activation
      *
      */
      bool getInhibitAllBrakes() const;

      /**
      * To indicate travel driving direction.
      *
      * @return DirDirection  Driving direction of train.
      */
      TravelDir getCurrentDrivingDirection() const;

      /**
      * To set the maximum allowed speed in location mode as defined in LOCATION_BORDERS
      *
      * @param[in] speed  speed of location border
      */
      void setMaxAllowedSpeedInLoc(const uint16_t speed);

      /**
      * Getter for the the maximum allowed speed in location mode
      *
      * @return maximum allowed speed value.
      */
      const uint32_t geMaxAllowedSpeedInLoc() const;

      /**
      * Get the odo-direction in new registration
      *
      * @return Returns the chosen orientation in track during new registration
      */
      OdoDir getOdoDirInNewRegistration() const;

      /**
      * Get the NotReadyToDrive status
      *
      * @return NotReadyToDrive status
      */
      bool getNotReadyToDrive() const;

      /**
      * indication for message handler if required to send "AbortSetup" message to TCC
      * @param[in] abortReason  To get the Abort Reason
      * @return true if required to send "AbortSetup" message to TCC
      *
      */
      bool sendAbortSetupToTCC(AbortReason &abortReason) const;

    protected:

      /**
      * Constructor
      */
      AbstractModeControl();

      /**
      * Create object of PowerUp mode
      */
      virtual AbstractMode* createPowerUpModeObj();

      /**
      * Create object of TrainConfig mode
      */
      virtual AbstractMode* createTrainConfigModeObj();

      /**
      * Create object of TrainRegistration mode
      */
      virtual AbstractMode* createTrainRegistrationModeObj();

      /**
      * Create object of BaliseSearch mode
      */
      virtual AbstractMode* createBaliseSearchModeObj();

      /**
      * Create object of Normal mode
      */
      virtual AbstractMode* createNormalModeObj();

      /**
      * Create object of Yard mode
      */
      virtual AbstractMode* createYardModeObj();

      /**
      * Create object of SafeBrakeToStop mode
      */
      virtual AbstractMode* createSafeBrakeToStopModeObj();

      /**
      * Create object of PowerDown mode
      */
      virtual AbstractMode* createPowerDownModeObj();

      /**
      * Create object of Unregistered mode
      */
      virtual AbstractMode* createUnregisteredModeObj();

      /**
      * Create object of Safety Halt mode
      */
      virtual AbstractMode* createSafetyHaltModeObj();

      /**
      * Create object of Driver Login sequence
      */
      virtual DriverLoginSeq* createDriverLoginSeqObj();

      /**
      * Create object of Emergency Alert sequence
      */
      virtual EmergencyAlertSeq* createEmergencyAlertSeqObj();

      /**
      * Create object of possession button sequence
      */
      virtual ModeRequestSeq* createPosButtonSeqObj();

      /**
      * Create object of Yard button sequence
      */
      virtual ModeRequestSeq* createYardButtonSeqObj();

      /**
      * Create object of shunting mode sequence
      */
      virtual ModeRequestSeq* createShuntButtonSeqObj();

      /**
      * Create object of config mode sequence
      */
      virtual ModeRequestSeq* createConfigButtonSeqObj();

      /**
      * Create object of Possession mode
      */
      virtual AbstractMode* createPossessionModeObj();

      /**
      * Create object of Shunting mode
      */
      virtual AbstractMode* createShuntingModeObj();

      /**
      * Create object of Location mode
      */
      virtual AbstractMode* createLocationModeObj();

      /**
      * Create object of Staff Responsible mode
      */
      virtual AbstractMode* createStaffResponsibleModeObj();

      /**
      * Create object of Sleeping mode
      */
      virtual AbstractMode* createSleepingModeObj();

      /**
      * Create object of Split mode
      */
      virtual AbstractMode* createSplitModeObj();

      /**
      * Create object of Join mode
      */
      virtual AbstractMode* createJoinModeObj();

      /**
      * To manage the cab active status.
      */
      void manageCabActiveStatus(void);

      /**
      * To manage the Sleeping Signal Status.
      */
      void manageSleepingSignal(void);

      /**
      * To manage the Brake Test activation and status.
      */
      void manageBrakeTest(void);

      /**
      * To manage the NotReadyToDrive Status.
      */
      void manageNotReadyToDrive(void) const;

      /**
      * To manage the logging of various AOS-Version information to TCC.
      */
      void manageVersionLoggingToTCC(void);

      /**
      * To manage transition into Powering down mode.
      *
      * @return return true if transition to PoweringDown is requested
      */
      bool checkRequestPowerdown(void) const;

      /**
      * Checks if there is a possibility to perform Brake Test when Brake Test is not mandatory.
      */
      void evaluateBrakeTestPossible(void);

      /**
      * Create object of Shunting Route mode
      */
      virtual AbstractMode* createShuntingRouteModeObj();

      /**
      * @return returns the time after which it is needed to reset ATP
      */
      virtual uint32_t getTimeDueToIntimidateTCCForATPReset();

      /**
      * Handle location exit scenarios
      */
      virtual void handleLocationData();

      /**
      * Mode dependent train states
      */
      CommonDataForModes commonData;

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

      /**
      * Function for logging Software, Configuration and Hardware version to TCC
      */
      virtual void logVersionToTCC();

      /**
      * Get pointer to mode handler
      *
      *  @param[in] mode   ATP mode
      *
      * returns pointer to the mode handler object
      */
      const AbstractMode* getModeObj(const ATPMode mode) const;

      /**
      * To apply service break in case both cabin active.
      */
      const ATC::Event bothCabinActiveSB;

      /**
      * To apply standstill event in case No cabin active. and mode is other then sleeping
      */
      const ATC::Event noCabinActiveStandstill;

      /**
      * SB event when Brake Test is Mandatory
      */
      const ATC::Event brakeTestIsMandatorySB;

      /**
      * Standstill event when Brake Test is Mandatory
      */
      const ATC::Event brakeTestIsMandatorySS;

      /**
      * Event for Safety Halt in Safe break to stop mode as previous mode was Shunting Route, Split or Join
      */
      const ATC::Event safetyHaltInSBTSInvalidPreviousMode;

      /**
      * Event to Report ATP needs to be reset.
      */
      const ATC::Event atpNeedsResetEvent;

      /**
      * Log Event when Sleeping Signal is Inactive in ATP Mode Sleeping.
      */
      const ATC::Event sleepingSigInactiveInSleeping;

      /**
      * Log Event when Sleeping Signal is active in an illegal mode.
      */
      const ATC::Event sleepingSigActiveIllegalMode;

      /**
      * Event for service brake due to operation more than maximum allowed running time .
      */
      const ATC::Event exceedMaxAllowedRunTime;

      /**
      * Event for safety halt due to operation more than maximum allowed running time.
      */
      const ATC::Event safetyHaltExceedMaxRunTime;

      /**
      * Event for service brake when NotReadyToDrive is activated during driving.
      */
      const ATC::Event notReadyToDriveWhileMoving;

      /**
      * Event for Mode Change
      */
      const ATC::Event modeChanged;

    private:

      /**
      * Run all the sequences.
      */
      void runAllSeqs();

      /**
      * To create mode object based on the mode enum.
      *
      * @param[in] mode enum of the mode to create object of.
      * @param[in] modePtr pointer to the mode object. null if no mode is created.
      * @return true if mode object is created, false otherwise.
      */
      bool createModeObject(const ATPMode mode, AbstractMode* &modePtr);

      /**
      * To trace out the current mode.
      */
      void traceMode(void) const;

      /**
      * Handles events and mode changes applicable for all modes before the actual mode is processed.
      */
      void preProcessEventsForAllModes();

      /**
      * Handles events for all modes after the actual mode was processed.
      */
      void postProcessEventsForAllModes();

      /**
      * Function to handle ATP reset operation time
      */
      void handleATPResetCondition();

      /**
      * AOS Status toggle with default frequency(0.25 Hz) with duty cycle 50%
      * since ATP is running at 100 millisecond, hence frequency count is 20
      */
      static const uint8_t aosStatusToggleFrequency = 20U;



      /**
      * Flag to prevent multiple initialization.
      */
      bool coreInitDone;

      /**
      * Current Operating mode of the system
      */
      ATPMode currentMode;

      /**
      * Previous Operating mode of the system
      */
      ATPMode previousMode;

      /**
      * Array of pointers to the objects of all operating modes of the system
      */
      AbstractMode* modeList[ATPModesCount];

      /**
      * Pointer to the Driver login sequence object
      */
      DriverLoginSeq* driverLoginSeq;

      /**
      * Pointer to the emergency alert sequence object
      */
      EmergencyAlertSeq* emergencyAlertAlarmSeq;

      /**
      * Pointer to the shunting button sequence object
      */
      ModeRequestSeq* shuntSeq;

      /**
      * Pointer to the possession button sequence object
      */
      ModeRequestSeq* posSeq;

      /**
      * Pointer to the yard button sequence object
      */
      ModeRequestSeq* yardSeq;

      /**
      * Pointer to the config button sequence object
      */
      ModeRequestSeq* configSeq;

      /**
      * To indicate the active cab.
      */
      CabActiveStatus cabActive;

      /**
      * To indicate Previous active cab.
      */
      CabActiveStatus prevActiveCab;

      /**
      * Flag to indicate Brake Test is requested by the Driver.
      * Evaluated every cycle in manageBrakeTest
      */
      bool brakeTestRequested;

      /**
      * Flag to indicate Brake Test is Mandatory.
      * Evaluated every cycle in manageBrakeTest
      */
      bool brakeTestMandatory;

      /**
      * Flag to indicate that Brake Test is possible, although not mandatory.
      * Evaluated every cycle in manageBrakeTest
      */
      bool brakeTestPossible;

      /**
      * Flag to indicate that it is time to notify the driver about the next mandatory brake Test
      * Evaluated every cycle in manageBrakeTest
      */
      bool isTimeForBrakeTestNotification;

      /**
      * Time remaining until next mandatory Brake test in minutes
      * Evaluated every cycle in manageBrakeTest
      */
      uint16_t remainingTimeToMandatoryBrakeTest;

      /**
      * flag to state whether ATP reset required or not
      */
      bool atpResetRequired;

      /**
      * Flag to state if there is a need to log Sleeping Signal Status
      */
      bool logSleepingSignalStatus;

      /** Time in seconds allowable for AOS to run without reset*/
      uint32_t maxAllowableTimeAOSRunWithoutReset;

      /**
      * Flag to state whether the SW name and version, Configuration name and version,
      * and HW name and version has been logged to TCC.
      */
      bool logVersionsToTCCFlag;

      /** Current max allowed speed in location mode*/
      uint32_t currentMaxAllowedSpeedInLocation;

    };
  }
}
#endif
