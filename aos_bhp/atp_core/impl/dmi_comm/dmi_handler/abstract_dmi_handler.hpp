#ifndef AbstractDMIHandler_hpp
#define AbstractDMIHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*
*  The DMI Handler component deals with managing interaction between ATP and DMI
*  via DMI channels. It also provides other components a way to send required
*  information to the DMI and collect informations/data from the DMI. It creates,
*  manages and owns the DMI channels over which all the communications to DMI
*  takes place.
*
******************************************************************************/


/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-29    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
* 2017-04-11    skothiya    Updated for implementation of cabin handling and authorization
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vector>
#include <map>
#include "atc_base.hpp"
#include "dmi_channel.hpp"
#include "abstract_dmi_message_in.hpp"
#include "abstract_dmi_message_out.hpp"


/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {

    /**
    * Unacknowledged Message Number
    */
    static const uint8_t unacknowledgedMessageNumber = 0U;

    /**
    * The DMI message received and its source DMI channel Id, appended with one char to
    * enable null-termination.
    */
    struct DriverIdAndPassword
    {
      char_t   driverID[driverIdMaxLength + 1U];      //!< Driver ID 
      char_t   password[dmiPasswordIdMaxLength + 1U];    //!< Driver Password 
    };

    /**
    * Enum for the DMI-ATP modes
    */
    enum DMIATPMode
    {
      DMIATPModeUndefined = 0,
      DMIATPModePowerUp,
      DMIATPModeTrainConfiguration,
      DMIATPModeTrainRegistration,
      DMIATPModeBaliseSearchMode,
      DMIATPModeNormal,
      DMIATPModeShunting,
      DMIATPModeLocation,
      DMIATPModeYardMode,
      DMIATPModeUnregistered,
      DMIATPModePowerDown,
      DMIATPModeSafetyHalt,
      DMIATPModeSleeping,
      DMIATPModeStaffResponsible,
      DMIATPModeShuntingRoute,
      DMIATPModePossession,
      DMIATPModeSplit,
      DMIATPModeJoin,
      DMIATPModeSafeBrakeToStop
    };

    /**
    * Enum for the DMI-ATP States
    */
    //lint -esym(769,ATP::DMICom::DMIATPStat*) Might be needed by adaptation
    enum DMIATPStates
    {
      DMIATPStatusUndefined = 0,
      DMIATPStatusBasicSystemStartUp,
      DMIATPStatusApplicationStartUp,
      DMIATPStatusInactive,
      DMIATPStatusActivationInitiation,
      DMIATPStatusActivationTest,
      DMIATPStatusActive,
      DMIATPStatusFatalFailureState,
      DMIATPStatusSystemRestart,
      DMIATPStatusPowerDown
    };

    /**
    * Enum for DMI Button Status
    */
    enum DMIButtonStatus
    {
      DMIButtonUndefined = 0U,
      DMIButtonBrakeRelease = 1U,
      DMIButtonTIMSInhibit = 2U,
      DMIButtonTIMSResume = 3U,
      DMIButtonTrainConfig = 4U,
      DMIButtonSpare5 = 5U,
      DMIButtonHandlingDone = 6U,
      DMIButtonEnterYardMode = 7U,
      DMIButtonManualTrainIntegrity = 8U,
      DMIButtonConfirmManualTrainIntegrity = 9U,
      DMIButtonCancelManualTrainIntegrity = 10U,
      DMIButtonRetryConfig = 11U,
      DMIButtonAbortSetup = 12U,
      DMIButtonLogoutDriver = 13U,
      DMIButtonEnterPossessionMode = 14U,
      DMIButtonShuntingMode = 15U,
      DMIButtonSpare16 = 16U,
      DMIButtonStartBrakeTest = 17U,
      DMIButtonAbortBrakeTest = 18U,
      DMIButtonStartBtmTest = 19U,
      DMIButtonAbortLastCarBrakeTest = 20U,
      DMIButtonConfirmYard = 21U,
      DMIButtonConfirmShuntingRoute = 22U,
      DMIButtonConfirmStaffResponsible = 23U,
      DMIButtonConfirmJoin = 24U,
      DMIButtonConfirmSleep = 25U,
      DMIButtonConfirmSplit = 26U,
      DMIButtonConfirmDeparture = 27U,
      DMIButtonAcceptAutomaticConfig = 28U,
      DMIButtonRequestFreeRolling = 29U,
      DMIButtonConfirmFreeRollingCleared = 30U,
      DMIButtonConfirmStaffResponsibleMA = 31U,
      DMIButtonConfirmShuntingRouteMA = 32U,
      DMIButtonConfirmJoinMA = 33U,
      DMIButtonCancelRegistrationArea = 34U,
      DMIButtonConfirmChangeOfTrainLoaded = 35U,
      DMIButtonCancelChangeOfTrainLoaded = 36U,
      DMIButtonConfirmAbortLastCarBrakeTest = 37U,
      DMIButtonCancelAbortLastCarBrakeTest = 38U,
      DMIButtonConfirmTachometer1Failure = 39U,
      DMIButtonConfirmTachometer2Failure = 40U,
      DMIButtonConfirmDopplerFailure = 41U,
      DMIButtonMaxCount = 42U
    };

    /**
    * Enum for Loco Vs Train Direction
    */
    enum LocoVsTrainDirection //Name updated to avoid conflict with LocoVsTrainDir in atp_types(LINT Warning)
    {
      DMIATPLocoVsTrainDirUndefined = 0,
      DMIATPcarsConnectedAtBEnd,
      DMIATPcarsConnectedAtAEnd
    };

    /**
    * Vehicle Data Block
    */
    struct VehicleDataBlock
    {
      uint16_t noOfVehicleInBlock;              //!< Number of vehicle in block
      uint8_t vehicleType;                      //!< Vehicle Type
      uint16_t vehicleNodeId;                    //!< Vehicle Node address or Id
      char_t vehicleName[vehicleNameMaxLength + 1U];     //!< Car Name, Size of carName Array is 20 as per IF Spec ATP-MMI
    };

    /**
    * Vehicle Type Block
    */
    struct VehicleTypeBlock
    {
      char_t vehicleTypeName[vehicleTypeNameMaxLength + 1U];     //!< Vehicle Type Name size is 20 as per IF Spec ATP-MMI
    };

    /**
    * Enum for Confirmation data
    */
    enum Confirmation
    {
      DMIATPConfirmationUndefined = 0,
      DMIATPConfirmationOK,
      DMIATPConfirmationNOK
    };

    /**
    * Enum for Train Vs Track Direction Data
    */
    enum TrainVsTrackDirection
    {
      DMIATPTrainVsTrackDirectionUndefined = 0,
      DMIATPTrainVsTrackDirectionForwardTrackTravelForward,
      DMIATPTrainVsTrackDirectionReverseTrackTravelForward,
      DMIATPTrainVsTrackDirectionForwardTrackTravelReverse,
      DMIATPTrainVsTrackDirectionReverseTrackTravelReverse
    };

    /**
    * Enum for ATP mode sub state
    */
    enum ATPModeSubState
    {
      UndefinedModeSubState = 0,
      ATPModeSubStateConfigManualOrPowerUpSelectMode,
      ATPModeSubStateConfigReReg,
      ATPModeSubStateConfigReStarted,
      ATPModeSubStateAutoConfig
    };

    /**
    * Enum for Driver verification state
    */
    enum DriverVerificationState
    {
      DMIATPDriverVerificationStateUndefined = 0,
      DMIATPDriverVerificationStateNoActionState,
      DMIATPDriverVerificationStateInputState,
      DMIATPDriverVerificationStateVerificationState,
      DMIATPDriverVerificationStateRedoInputState,
      DMIATPDriverVerificationStateLoggedOn
    };

    /**
    * Enum for ATO Mode
    */
    enum DMIATOMode
    {
      //lint -esym(769,ATP::DMICom::DMIATOMode*) Might be needed by adaptation
      DMIATOModeUndefined = 0,
      DMIATOModeManual,
      DMIATOModeSupervisedAutomatic,
      DMIATOModeAutomatic,
      DMIATOModeRemote
    };

    /**
    * Enum for Wanted ATO Mode
    */
    //lint -esym(769,ATP::DMICom::DMIWantedATOSwitchPos*) Might be needed by adaptation
    enum DMIWantedATOSwitchPos
    {
      DMIWantedATOSwitchPosUndefined = 0,
      DMIWantedATOSwitchPosManual,
      DMIWantedATOSwitchPosSupervisedAutomatic,
      DMIWantedATOSwitchPosAutomatic,
      DMIWantedATOSwitchPosIllegal
    };

    /**
    * Enum for reason of change in Ceiling Speed
    */
    //lint -esym(769,ATP::DMICom::DMICeilingSpeedChangeReason*) Might be needed by adaptation
    enum DMICeilingSpeedChangeReason
    {
      DMICeilingSpeedChangeReasonUndefined = 0,
      DMICeilingSpeedChangeReasonPointStraight = 1,
      DMICeilingSpeedChangeReasonPointCurve = 2,
      DMICeilingSpeedChangeReasonPointPassed = 3,
      DMICeilingSpeedChangeReasonLocation = 4,
      DMICeilingSpeedChangeReasonTSR = 5,
      DMICeilingSpeedChangeReasonRestrictiveSection = 6,
      DMICeilingSpeedChangeReasonEndOfMA = 7,
      DMICeilingSpeedChangeReasonConditionalTarget = 8,
      DMICeilingSpeedChangeReasonPantographShiftToNone = 9,
      DMICeilingSpeedChangeReasonPantographShiftToRoof = 10,
      DMICeilingSpeedChangeReasonPantographShiftToSide = 11,
      //Values after 129 will be used in adaptation
      DMICeilingSpeedChangeReasonReferenceForOdometerValue = 254,
      DMICeilingSpeedChangeReasonOther = 255
    };

    /**
    * Ceiling Speed List Data Block
    */
    struct CeilingSpeedListDataBlock
    {
      int32_t odoPosition;                                   //!< Odometer Position(m)
      uint8_t  newSpeed;                                     //!< New Speed (Km/h)
      uint8_t ceilingSpeedChangeReason;  //!< Reason for Speed Change
    };

    /**
    * Gradient Data List Data Block
    */
    struct GradientDataListDataBlock
    {
      int32_t odoPosition;                                  //!< Odometer Position(m)
      int8_t newGradient;                                   //!< New Gradient (0.1%)
    };

    /**
    * Enum for Change Mode Confirmation
    */
    enum DMIConfirmModeChange
    {
      DMIConfirmModeChangeUndefined = 0,
      DMIConfirmModeChangeToYard,
      DMIConfirmModeChangeToShuntingRoute,
      DMIConfirmModeChangeToStaffResponsible,
      DMIConfirmModeChangeToJoin,
      DMIConfirmModeChangeToSleep,
      DMIConfirmModeChangeToSplit
    };

    class AbstractDMIHandler;
    /**
    * Static variable to store the single instance of AbstractDMIHandler
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractDMIHandler* coreDMIHandlerInstancePtr = static_cast<AbstractDMIHandler*>(NULL);

    /** The AbstractDMIHandler implements the core DMI Handler functionality
    *  but the adaptation is implemented in the DMIHandler class
    */
    class AbstractDMIHandler : public ATC::IOComponent
    {
    public:

      /**
      * Implements the virtual preInit function.
      *
      * Calls preInit for all DMI channels.
      */
      virtual void preInit(void);

      /**
      * Implements the virtual init function.
      *
      * Create the DMIChannel objects and store in container.
      *
      * @return Returns true when initialization completed
      */
      virtual bool init(void);

      /**
      * Implements the virtual runIn function.
      */
      virtual void runIn(void);

      /**
      * Implements the virtual runOut function.
      */
      virtual void runOut(void);

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
      *  Access-function for any published ConfirmationStatus
      *
      * @return confirmation Status
      */
      Confirmation getConfirmation();

      /** 
      * Access-function for any published DMIStartup Status
      *
      * @param[out] status received in DMIStartup - message
      * @return DMI Startup message received
      */
      bool getDMIStartupStatus(uint16_t &status);

      /**
      * Access-function for any published DMI Compatibility Version
      *
      * @param[out] compatibilityVersion  DMI Compatibility Version
      *
      * @return true if compatibilityVersion is available
      */
      bool getDMICompatibilityVersion(uint8_t &compatibilityVersion);

      /**Access-function for any published DMIStatusWord
      *
      * @return DMI Status Word
      */
      uint16_t getDMIStatusWord();

      /**
      * Access-function for any published DMI Button Status
      *
      * @return DMI Button Status
      */
      DMIButtonStatus getDMIButtonStatus();

      /**
      * Access-function for add Start up history message
      *
      * param[in] str   Message string
      */
      void addStartupMsg(const char_t * const str);

      /**
      * Access-function for any published DriverId And Password
      *
      * @param[out] driverIdAndPass current DriverId and password
      *
      * @return true if DriverId and password is available
      */
      bool getDriverIdAndPassword(DriverIdAndPassword &driverIdAndPass);


      /**
      * Access-function for any publishedRegistration Area
      *
      * @param[out] registrationAreaId Area Id of the Registration Area
      *
      * @return true if Registration Area is available
      */
      bool getRegistrationArea(uint8_t &registrationAreaId);

      /**
      *Access-function for any published LocoVsTrainDir
      *
      * @return LocoVsTrainDir
      */
      LocoVsTrainDirection getLocoVsTrainDir();

      /**
      * Access-function for any published TrainVsTrackDirection
      *
      *  @return TrainVsTrackDir
      */
      TrainVsTrackDirection getTrainVsTrackDirection();

      /**
      * Function to get the next Message Number
      *
      *  @return current message number
      */
      uint8_t getNextMessageNumber();

      /**
      * Function to set the DMI Compatibility Version
      */
      void setDMICompatibilityVersionAccepted();

      /**
      * Function to get the DMI Compatibility Version
      *
      *  @return true if dmiCompatibilityVersionAccepted flag is available
      */
      bool getDMICompatibilityVersionAccepted() const;

      /**
      * Get the DMI New Train Setup is Confirmed or not
      *
      *  @return true if New Train Setup is confirmed
      */
      bool getDMINewTrainSetUpConfirmed(void);

      /**
       * Get core instance pointer
       *
       * @return Pointer to single instance core object.
       */
      static AbstractDMIHandler* corePtr();

      /**
      * Function to get the Cycle Count
      *
      *  @return current cycle count
      */
      uint16_t getCycleCount() const;

      /**
      * Start up and health supervision test for DMI
      *
      *  @return true if test is successful
      */
      bool startupAndHealthSupTest() const;

      /** Get status of DMI channel
      *
      *  @return true if a DMI channel is connected
      */
      bool isConnected() const;

      /**
      * Function to get the LocoVsTrainDir data
      *
      * @return current LocoVsTrainDir data
      */
      LocoVsTrainDirection getLocoVsTrainDirData() const;

      /**
      * Function to set the LocoVsTrainDir data
      *
      * @param[out] locoVsTrainDirValue  current LocoVsTrainDir
      */
      void setLocoVsTrainDirData(const LocoVsTrainDirection &locoVsTrainDirValue);

      /**
      *Access-function to reset the LocoVsTrainDir Data
      */
      void resetLocoVsTrainDirData();

      /**
      * Access-function to get the changed train Name
      *
      * param[in] changedTrainName   changed Train Name
      */
      bool getChangedTrainName(char_t * const changedTrainName);

      /**
      * Access function to get the Tachometer1 failure from Odometry
      *
      * @return Tachometer1 failure status
      */
      bool getOdometerTacho1Failure() const;

      /**
      * Access function to get the Tachometer1 failure from Odometry
      *
      * @return Tachometer2 failure status
      */
      bool getOdometerTacho2Failure() const;

      /**
      * Access function to get the Doppler failure from Odometry
      *
      * @return Doppler failure status
      */
      bool getOdometerDopplerFailure() const;

      /**
      * Access function to get the Driver confirmation status on Tachometer1 failure
      *
      * @return if driver confirmed the tachometer1 failure
      */
      bool getDriverTacho1FailureConfirmation() const;

      /**
      * Access function to get the Driver confirmation status on Tachometer2 failure
      *
      * @return if driver confirmed the tachometer2 failure
      */
      bool getDriverTacho2FailureConfirmation() const;

      /**
      * Access function to get the Driver confirmation status on doppler failure
      *
      * @return if driver confirmed the doppler failure
      */
      bool getDriverDopplerFailureConfirmation() const;

      /**
      * Compatibility version for DMI
      *
      * @return the compatibility version for the DMI
      */
      virtual uint8_t getCompatibilityVersion() const = 0;

    protected:
      /**
      * Constructor
      */
      AbstractDMIHandler();

      /**
      * The container used for the DMIChannel objects
      */
      std::vector<DMIChannel*> dmiChannels;

      /**
      * The DMI Active Channel
      */
      DMIChannel *activeChannel;

      /**
      * The container of the pointers to parsers for incoming DMI messages
      */
      std::map<DMIMessageType, AbstractDMIMessageIn*>dmiMessageInParser;

      /**
      * The container of the pointers to creators of outgoing DMI messages
      */
      std::vector<AbstractDMIMessageOut*>dmiMessageOutCreator;

      /**
      * Unexpected DMI Message Type Error
      */
      const ATC::Event unexpectedDMIMessageType;

    private:

      /**
      *  DMI Channel for DMI1
      */
      DMIChannel dmiChannel1;

      /**
      * DMI Channel for DMI2
      */
      DMIChannel dmiChannel2;

      /**
      * Initialization status for DMIChannel1
      */
      bool initDoneChnl1;

      /**
      * Initialization status for DMIChannel2
      */
      bool initDoneChnl2;

      /**
      * Message Number Counter
      */
      uint8_t messageNumberCounter;

      /**
      * Cycle Count
      */
      uint16_t cycleCount;

      /**
      * Flag to check whether DMI Compatibility Version is Accepted or not
      */
      bool dmiCompatibilityVersionAccepted;

      /**
      * Indicates Tachometer1 Failure, Gets the value from Odometer.
      * These values store the flags from Odomter until they are confirmed by driver
      */
      bool OdoFailureTachometer1;

      /**
      * Indicates Tachometer2 Failure, Gets the value from Odometer.
      * These values store the flags from Odomter until they are confirmed by driver
      */
      bool OdoFailureTachometer2;

      /**
      * Indicates doppler Failure, Gets the value from Odometer.
      * These values store the flags from Odomter until they are confirmed by driver
      */
      bool OdoFailureDoppler;

      /**
      * Indicates whether driver confirmed Tachometer1 Failure
      */
      bool isDriverConfirmedTachometer1Failure;

      /**
      * Indicates whether driver confirmed Tachometer2 Failure
      */
      bool isDriverConfirmedTachometer2Failure;

      /**
      * Indicates whether driver confirmed doppler Failure
      */
      bool isDriverConfirmedDopplerFailure;

      /** Event for when driver confirmed tachometer1  sensor failure .
      */
      const ATC::Event driverConfirmedTachometer1Failure;

      /** Event for when driver confirmed tachometer2  sensor failure .
      */
      const ATC::Event driverConfirmedTachometer2Failure;

      /** Event for when driver confirmed doppler sensor failure .
      */
      const ATC::Event driverConfirmeddopplerFailure;

      /**
      * variable to store the LocoVsTrainDir data
      */
      LocoVsTrainDirection locoVsTrainDirData;

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      void initCrossCompare() const;

      /**
      * Function to handle DMI buttons related to odometer failure
      */
      void getDriverOdoFailDMIButtonConfirmation();

      /**
      * Set the status of Tachometer1,Tachometer2 and doppler failures from Odometry
      */
      void handleOdometerFailures();

    };
  }
}

#endif
