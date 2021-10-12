#ifndef RadioMessageTypes_hpp
#define RadioMessageTypes_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Types related to FFFIS TCC-AOS messages.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-21    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include <vector>
#include "base_config_item.hpp"
#include "abstract_tracks.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * RadioMessageType corresponds with NID_MESSAGE_TYPE (Exception is PositionReport which needs one instance for
    * each of the regions.)
    * Messages sent from TCC to AOS has a NID_MESSAGE_TYPE < 128
    * Messages sent from AOS to TCC has a NID_MESSAGE_TYPE >= 128
    */
    enum RadioMessageType
    {
      MTypeNotImplemented = 0,  // Used if not implemented
      // TCC to AOS
      MTypePositionReportRequest = 1,
      MTypeDriverLogonStatus = 2,
      MTypeEmergencyAlert = 3,
      MTypeMovementAuthority = 4,
      MTypeTrainSetup = 5,
      MTypeUnregister = 6,
      MTypeATORemoteControl = 7,
      MTypeStopTrain = 8,
      MTypeRevokeEmergencyAlert = 9,
      MTypeApproximatePosition = 10,
      MTypePossessionAcknowledge = 11,
      MTypeShuntingAcknowledge = 12,
      MTypeJoinCommand = 13,
      MTypeExternalData = 14,
      MTypeConfigurationData = 15,
      MTypeCommandMessage = 16,
      MTypePath = 17,
      MTypeRejectConfiguration = 18,
      MTypeAreaRequestMessage = 19,
      MTypeYardAcknowledge = 20,
      MTypeUnconditionalShortening = 21,
      // Insert any incoming messages type before any common messages type
      // Start of message type for common Incoming messages
      MTypeProtocolVersionIncoming = 22,
      MTypeTCCToAOSMax = 23,

      // AOS to TCC
      MTypeDriverInformation = 128,
      MTypeStartUpMessage = 129,
      MTypeAbortSetup = 130,
      MTypeTrainRegistrationInformation = 131,
      MTypePositionReportRegion1 = 132,
      MTypePositionReportRegion2 = 133,
      MTypeRegistrationAreaMessage = 134,

      // Insert any outgoing messages type before any common messages type
      // Start of message type for common outgoing messages
      MTypeProtocolVersionOutgoing = 135,
      MTypeAOSToTCCMax = 136,

      //Common Message
      MTypeProtocolVersion = 200
    };

    /******************************************************************************
    * Field Descriptions
    ******************************************************************************/

    /**
    * EmAlertReasonInfo is the uint8_t representation of the field Q_ALERT
    */
    typedef uint8_t EmAlertReasonInfo;

    /**
    * EmAlertUndefined is the undefined value of Q_ALERT
    */
    static const EmAlertReasonInfo EmAlertUndefined = 0U;

    /**
    * EmAlertUnknown is the unknown value of Q_ALERT
    */
    static const EmAlertReasonInfo EmAlertUnknown = 9U;

    /**
    * UnregInfo is the info associated with an Unregistration corresponding with the field Q_UNREGISTRATION
    */
    typedef uint8_t UnregInfo;

    /**
    * 4..255 is Unknown Reason, why the TCC system has unregistered the train.
    */
    static const UnregInfo unregUnknownReason = 4U;

    /**
    * Reject configuration is the info associated with a RejectConfiguration with the field Q_REJECT_CONFIGURATION
    */
    typedef uint8_t RejectConfigInfo;

    /**
    * 8..255 is Unknown Reason for TCC to reject the configuration
    */
    static const UnregInfo rejectConfigUnknownReason = 8U;

    /**
    * DriverLogonStatus is the info associated with a DriverLogonStatus corresponding with the field Q_LOGON_STATUS
    */
    enum LogonStatus
    {
      DriverLogonFailed = 0,
      DriverLogonSuccesful = 1
    };

    /**
    * TypeOfSignal is the type of acoustic signal corresponding with the field Q_SIGNAL
    */
    enum TypeOfSignal
    {
      TypeOfSignalUndefined = 0,
      TypeOfSignalSoundType1,
      TypeOfSignalSoundType2,
      TypeOfSignalSoundType3
    };

    /**
    * TicStatus is the info associated with a TicAvailable corresponding with the field Q_CONFIG_SOURCE
    */
    enum ConfigSource
    {
      ManuallyEnteredByDriver = 0,
      AutoCollectedByTic = 1
    };

    /**
    * TimsStatus is the info associated with a TimsAvailable corresponding with the field Q_TIMS_AVAILABLE
    */
    enum TimsStatus
    {
      TimsNotAvailable = 0,
      TimsAvailable = 1
    };

    /**
    * TimsSupervision is the info associated with a Tims Supervision corresponding with the field Q_TIMS_SUPERVISION
    */
    enum TimsSupStatus
    {
      TimsSupNotReq = 0,
      TimsSupReq = 1
    };

    /**
    * Power is the info associated with a Locomotive power up corresponding with the field Q_POWER
    */
    enum Power
    {
      PowerDown = 0,
      PowerUp = 1
    };

    /**
    * Abort is the info associated with a setup abort corresponding with the field Q_ABORT
    */
    enum AbortReason
    {
      AbortedByAos = 0,
      AbortedByDriver = 1
    };

    /**
    * TrainSetupReason is the info associated with a TrainSetup corresponding with the field Q_SETUP
    */
    enum TrainSetupReason
    {
      TrainSetupRegistration = 0,
      TrainSetupReconfiguration,
      TrainSetupReregistration,
      TrainSetupReposition
    };

    /**
    * QInitiate is the info associated with a PRR corresponding with the field Q_INITIATE
    */
    enum InitiateConfigReason
    {
      ConfigUnknownByTCC = 0,
      ConfigKnownByTCC
    };

    /**
    * Used for acknowledge of a request corresponding with the field Q_ACKNOWLEDGE
    */
    enum Acknowledge
    {
      RequestNotAcknowledged = 0,
      RequestAcknowledged = 1
    };

    /**
    * Protocol version status corresponding with the field Q_PROTOCOL_RESPONSE
    */
    enum ProtocolResponse
    {
      TCCRequest = 0,
      AOSResponseMatch = 1,
      AOSResponseMisMatch = 2,
      TCCResponseUnrecoverableMisMatch = 3
    };

    /******************************************************************************
    * Object Type Identifiers
    ******************************************************************************/

    /******************************************************************************
    * Radio Block Data Types
    ******************************************************************************/

    /**
    * RadioBlockType corresponds with NID_BLOCK_TYPE
    */
    typedef uint8_t RadioBlockType;

    static const RadioBlockType BTypeUndefined           = 0U;
    static const RadioBlockType BTypeLocationData        = 1U;
    static const RadioBlockType BTypeProtocolVersion     = 2U;
    static const RadioBlockType BTypeTrackData           = 3U;
    static const RadioBlockType BTypeBaliseData          = 4U;
    static const RadioBlockType BTypeGradientData        = 5U;
    static const RadioBlockType BTypeCeilingSpeedData    = 6U;
    static const RadioBlockType BTypeTrainName           = 7U;
    static const RadioBlockType BTypeSetTime             = 8U;
    static const RadioBlockType BTypeATOStopPosition     = 10U;
    static const RadioBlockType BTypeReleaseBrake        = 12U;
    static const RadioBlockType BTypeLoadFinished        = 13U;
    static const RadioBlockType BTypeTextMessage         = 14U;
    static const RadioBlockType BTypeWaitingTime         = 15U;
    static const RadioBlockType BTypeTrackDataItem       = 16U;
    static const RadioBlockType BTypeMaxSearchDist       = 17U;
    static const RadioBlockType BTypePartlyMa            = 19U;
    static const RadioBlockType BTypeETAConfirmation     = 20U;
    static const RadioBlockType BTypeDepartureWarning    = 22U;
    static const RadioBlockType BTypeVehicleData         = 23U;
    static const RadioBlockType BTypeEventDataText       = 24U;
    static const RadioBlockType BTypeCancelArea          = 25U;
    static const RadioBlockType BTypeLastBalise          = 27U;
    static const RadioBlockType BTypePossesionRequest    = 28U;
    static const RadioBlockType BTypeShuntingRequest     = 29U;
    static const RadioBlockType BTypeEventData           = 30U;
    static const RadioBlockType BTypeVehicleTypeData     = 31U;
    static const RadioBlockType BTypeYardRequest         = 32U;
    static const RadioBlockType BTypeExternalData        = 33U;
    static const RadioBlockType BTypeInitiateConfig      = 34U;
    static const RadioBlockType BTypeLocationBorder      = 35U;
    static const RadioBlockType BTypeConfigConfirmation  = 37U;
    static const RadioBlockType BTypeConfigurationData   = 38U;
    static const RadioBlockType BTypeETARequest          = 39U;
    static const RadioBlockType BTypeSpeedChangePosition = 40U;
    static const RadioBlockType BTypeTracks              = 41U;
    static const RadioBlockType BTypeArea                = 42U;
    static const RadioBlockType BTypeBaliseIdentity      = 43U;
    static const RadioBlockType BTypeMessageAcknowledge  = 44U;
    static const RadioBlockType BTypePartlyTrackData     = 45U;
    static const RadioBlockType BTypeStopDistData        = 46U;

    /**
    * Max length of N_LENGTH
    */
    static const uint16_t maxSizeAppData = 6000U; // Due to position report, size should not exceed the maximum radio message size.

    /** Max length of the Text Message
    */
    static const uint8_t maxTextLength = 99U;

    /**
    * Max number of TRACK_DATA blocks
    */
    static const uint8_t approxPosTrackDataSize = 10U;

    /**
    * Maximum number of tracks blocks
    */
    static const uint16_t maxPathTracksSize = 150U;

    /**
    * Max number of SPEED_CHANGE_POSITION blocks
    */
    static const uint8_t maxSpeedChangePositionSize = 50U;

    /**
    * Max number of Region Area identification blocks
    */
    static const uint8_t regionAreaDataSize = 10U;

    /**
    *  MessageAck corresponds with MESSAGE_ACKNOWLEDGE
    */
    struct MessageAck
    {
      uint8_t msgId;        //!< Identification of message to acknowledge.
      bool messageAccepted; //!< Accepted or not.
    };

    /**
    *  StopDistData corresponds with STOP_DIST_DATA
    */
    struct StopDistData
    {
      int8_t gradient;        //!< Gradient
      uint32_t unloadedDist;  //!< Unloaded train stop distance
      uint32_t loadedDist;    //!< Loaded train stop distance
    };

    /**
    *  ProtocolVersion corresponds with PROTOCOL_VERSION
    */
    struct ProtocolVersion
    {
      /**
      * Default Constructor
      */
      ProtocolVersion();

      /**
      * Constructor
      */
      ProtocolVersion(const uint8_t maj, const uint8_t min);

      /**
      * Checks if the track is the same, bdirection is ignored
      * @param[in] other     Track to compare
      *
      * @return true if the track is the same, bdirection is ignored
      */
      bool operator==(const ProtocolVersion& other) const;


      uint8_t majorVersion; //!< Major version
      uint8_t minorVersion; //!< Minor version

    private:
    };


    /**
    *  LastBalise corresponds with LAST_BALISE
    */
    struct LastBalise
    {
      uint16_t  idLastReadBalise;  //!< ID of last read balise
    };

    /**
    *  VehicleIdData corresponds with VEHICLE_DATA
    */
    struct VehicleData
    {
      uint16_t    noOfVeh;  //!< N_VALUE, Number of vehicles. Value shall be >= 1.
      uint8_t     vehicleType;  //!< Type of vehicle(s). If available otherwise set to 0.
      uint16_t    vehicleNodeAddress;  //!< Vehicle node address.Only to be used if N_VALUE is set to 1. Value shall be set to zero if N_VALUE is >1.
      char_t      vehicleName[vehicleNameMaxLength + 1U]; //!< Textual identity of the vehicle(s), if available. Size is incremented by 1 is to adapt Null termination for VFW.
    };

    /**
    *  vehicleTypeData corresponds with VEHICLE_TYPE_DATA
    */
    struct VehicleTypeData
    {
      uint8_t     vehicleType;                   //!< Type of vehicle
      uint16_t    dynamicWeightLoaded;           //!< Dynamic weight loaded
      uint16_t    dynamicWeightEmpty;            //!< Dynamic weight empty
      uint16_t    brakeWeightLoadedBrakeSystem1; //!< Brake weight loaded, brake system type 1
      uint16_t    brakeWeightEmptyBrakeSystem1;  //!< Brake weight empty, brake system type 1
      uint16_t    brakeWeightLoadedBrakeSystem2; //!< Brake weight loaded, brake system type 2
      uint16_t    brakeWeightEmptyBrakeSystem2;  //!< Brake weight empty, brake system type 2
      uint16_t    brakeWeightLoadedBrakeSystem3; //!< Brake weight loaded, brake system type 3
      uint16_t    brakeWeightEmptyBrakeSystem3;  //!< Brake weight empty, brake system type 3
    };

    /**
    *  ConfigConfirmation corresponds with CONFIG_CONFIRMATION
    */
    struct ConfigConfirmation
    {
      Acknowledge       configConfirmationAcknowledge; //!< Accepted or not
    };

    /**
    *  EventData corresponds with EVENT_DATA
    */
    struct EventData
    {
      uint32_t      idEventData; //!< ID of event
    };

    /** Max length of event data text field
    */
    static const uint8_t maxEventDataTextLength = 20U;

    /**
    *  EventDataText corresponds with EVENT_DATA_TEXT
    */
    struct EventDataText
    {
      uint32_t      idEventDataText;                               //!< ID of event
      char_t        text[maxEventDataTextLength + 1U];             //!< Size is incremented by 1 for Null termination.
    };

    /**
    * Type of acoustic signal corresponding with the field Q_SIGNAL
    * 0, Undefined
    * 1-127, Reserved for future core expansion
    * 128-255, Reserved for adaptations
    * All reserved values treated as unknown reason, unless defined in adaptation
    */
    typedef uint8_t AcousticSignal;

    /**
    * Undefined acoustic signal
    */
    static const AcousticSignal acousticSignalUndefined = 0U;

    /**
    *  DepartureWarning corresponds with DEPARTURE_WARNING
    */
    struct DepartureWarning
    {
      AcousticSignal        signalAsDepartureWarning; //!< Signal to use as departure warning in ATO Automatic mode
    };

    /**
    *  MaxSearchDist corresponds with MAX_SEARCH_DIST
    */
    struct MaxSearchDist
    {
      uint32_t maxDistBalise; //!< Maximum distance to search for balise.
    };

    /**
    *  LocationBorder corresponds with LOCATION_BORDERS
    */
    struct LocationBorder
    {
      TrackAndPos startOfLocation; //!< Location start track
      TrackAndPos endOfLocation;   //!< Location end track
      uint16_t allowedSpeed;       //!< Maximum allowed speed when operating within location border
      int8_t gradTwrdsLocStart;    //!< Aggregated gradient when moving inside location borders towards location start
      int8_t gradTwrdsLocEnd;      //!< Aggregated gradient when moving inside location borders towards location end
    };

    /**
    *  LocationData corresponds with LOCATION_DATA
    */
    struct LocationData
    {
      char_t locationName[maxLocationNameLength + 1U]; //!< Location name. Size is incremented by 1 is to adapt Null termination for VFW.
      LocationType locationType; //!< Location Type
    };

    /**
    *  ATOStopPosition corresponds with ATO_STOP_POSITION
    */
    struct ATOStopPosition
    {
      TrackAndPos         trainStopTrackAndPosition;  //!< Identification of Track and position of the item in Track
    };


    /**
    *  TrackData corresponds with TRACK_DATA
    */
    struct TrackData
    {
      uint16_t  track; //!< Track id
      uint32_t  length; //!< Track length [cm]
      uint8_t   bdirection; //!< B-direction
      TravelDir trvDir; //!< Travel direction on track (from B-direction)
      OdoDir    odoDir; //!< Orientation in track (from B-direction)
      uint16_t  previousTrack; //!< Track id of previous track

      /**
      * Checks if the track is the same, bdirection is ignored
      * @param[in] other     Track to compare
      *
      * @return true if the track is the same, bdirection is ignored
      */
      bool operator==(const TrackData& other) const;
    };

    /**
    *  Function class to compare track in TrackData
    */
    struct TrackDataTrackIdComp
    {
      /**
      *  Default constructor
      */
      TrackDataTrackIdComp();

      /**
      *  Constructor
      * @param[in] trackId  Track to copy
      */
      TrackDataTrackIdComp(const uint16_t trackId);

      /**
      * Compare operator
      * @param[in] t  Track to compare
      */
      bool operator() (const TrackData &t) const;

      const uint16_t trackIdToFind;//!< Track id to find
    };

    /**
    *  BaliseData corresponds with BALISE_DATA
    */
    struct BaliseData
    {
      /**
      *  Constructor
      */
      BaliseData();

      /**
      *  Constructor
      * @param[in] balId             Identification of balise
      * @param[in] trackAndPosition  Identification of Track and position of the item in Track
      */
      BaliseData(const uint16_t balId, const TrackAndPos& trackAndPosition);

      TrackAndPos  baliseTrackAndPosition;  //!< Identification of Track and position of the item in Track
      uint16_t  baliseId; //!< Identification of balise

      /**
      * Compare operator
      * @param[in] other  Balise to compare
      */
      bool operator==(const BaliseData& other) const;
    };

    /**
    * Function class to compare id in BaliseData
    */
    struct BaliseDataBaliseIdComp
    {
      /**
      *  Constructor
      */
      BaliseDataBaliseIdComp();

      /**
      *  Constructor
      * @param[in] baliseId  Identification of balise
      */
      BaliseDataBaliseIdComp(const uint16_t baliseId);

      /**
      * Compare operator 
      * @param[in] b  Balise to compare
      */
      bool operator() (const BaliseData &b) const;

      const uint16_t baliseIdToFind;//!< Identification of balise to find
    };

    /**
    *  GradientData corresponds with GRADIENT_DATA
    */
    struct GradientData
    {
      TrackAndPos  trackAndPosition;  //!< Identification of Track and position of the item in Track
      int8_t    gradient; //!< Gradient
    };

    /**
    *  CeilingSpeedData corresponds with CEILING_SPEED_DATA
    */
    struct CeilingSpeedData
    {
      TrackAndPos  trackAndPosition;  //!< Identification of Track and position of the item in Track
      uint16_t  ceilingSpeed; //!< New ceiling speed
      SpeedChangeReason speedChangeReason; //!< Reason for change in ceiling speed
    };

    /**
    * Valid Direction corresponding with the field Q_DIRECTION
    */
    enum ValidDirection
    {
      ValidDirectionUndefined,
      ValidDirectionForward,
      ValidDirectionReverse,
      ValidDirectionBoth
    };


    /**
    *  TrackDataItem corresponds with TRACK_DATA_ITEM
    */
    struct TrackDataItem
    {
      TrackDataType   trackDataType;     //!< Type of track data
      TrackAndPos     trackAndPosition;  //!< Identification of Track and position of the item in Track
      ValidDirection  validDir;          //!< Valid driving direction for this TDI.
      uint16_t        optionalValue;     //!< Optional track data type
    };

    /**
    *  TrainName corresponds with TRAIN_NAME
    */
    struct TrainName
    {
      char_t  trainName[trainNameMaxLength + 1U];  //!< Size is incremented by 1 is to adapt Null termination for VFW.
    };

    /**
    *  EtaConfirmation corresponds with ETA_CONFIRMATION (Confirmation of Estimated Time of Arrival)
    */
    struct ETAConfirmation
    {
      Acknowledge     arrivalTimeAcknowledge; //!< Arrival time confirmed or not
      uint64_t        newETATime; //!< New ETA or 0 if N/A
    };

    /**
    * Max length of NID_TEXT_STRING
    */
    static const uint8_t maxTextStringLength = 20U;

    /**
    *  Structure to store CONFIGURATION_DATA received from TCC
    */
    struct ConfigDataStruct
    {
      char_t  textStringName[maxTextStringLength + 1U];        //!< Parameter name,Size is incremented by 1 is to adapt Null termination for VFW.
      char_t  textStringValue[maxTextStringLength + 1U];       //!< Value string , Size is incremented by 1 is to adapt Null termination for VFW.
      ATC::ConfigIdType configurationId;                       //!< Configuration ID fetched from Config Component
    };

    /**
    *  ExternalData corresponds with EXTERNAL_DATA
    */
    struct ExternalData
    {
      uint8_t     nidSystem;                 //!< Source or destination system
      uint16_t    noOfAppData;               //!< Number of bytes application data
      uint8_t     appdata[maxSizeAppData];   //!< Application data type is not yet defined in FFFIS 
    };

    /**
    *  SetTime corresponds with SET_TIME
    */
    struct SetTime
    {
      uint64_t  currentTime; //!< Current time
    };

    /**
    *  TrackStruct corresponds with TRACKS
    */
    struct TrackStruct
    {
      uint16_t  nidTrack; //!< Identification of Track
    };

    /**
    *  ETARequestStruct corresponds with ETA_REQUEST
    */
    struct ETARequestStruct
    {
      uint64_t  nextTargetArrivalTime;  //!< Requested time of arrival at the Next Target
    };

    /**
    *  SpeedChangePositionStruct corresponds with SPEED_CHANGE_POSITION
    */
    struct SpeedChangePositionStruct
    {
      TrackAndPos  trackAndPosition;  //!< Identification of Track and position of the item in Track
      uint16_t  newSpeed;             //!< New Speed
    };

    /**
    *  TextMessage corresponds with TEXT_MESSAGE
    */
    struct TextMessage
    {
      char_t  text[maxTextLength + 1U];   //!< Text Message string , Size is incremented by 1 is to adapt Null termination for VFW.
    };

    /**
    *   baliseIdentity corresponds with BALISE_IDENTITY
    */
    struct baliseIdentity
    {
      uint16_t baliseID;   //!< Identification of balise
    };

    /**
    * Defines the storage of the parsed information of a received AREA from TCC
    *
    */
    struct TCCAreas
    {
      uint8_t numAreas;  //!< Number of areas available in the request message
      uint8_t areaId[regionAreaDataSize];  //!< Area Id array
    };


    /**
    *  WaitingTime corresponds with WAITING_TIME
    */
    struct WaitingTime
    {
      uint8_t  estimatedTimeToWait;    //!< Estimated time to wait (Minutes) 
    };

    /**
    *   InitiateConfig corresponds with INITIATE_CONFIG
    */
    struct InitiateConfig
    {
      InitiateConfigReason  statusOfTrainConfig;   //!< Requested configuration to perform.
    };

    /**
    * Defines the storage of the parsed information of a received PossessionAcknowledge
    *
    */
    struct PossessionAcknowledge
    {
      Acknowledge       possAcknowledge; //!< Possession mode granted or not
      uint16_t          allowedSpeedInPossession; //!< Maximum allowed speed in Possession
      std::vector<baliseIdentity> baliseIdVec; //!< List of balise identities allowed to pass in Possession mode

      /**
      * Constructor
      */
      PossessionAcknowledge();

    private:

      /**
      * Disabled copy constructor
      */
      PossessionAcknowledge(const PossessionAcknowledge&);

      /**
      * Disabled assignment operator
      */
      PossessionAcknowledge& operator = (const PossessionAcknowledge&);
    };

    /**
    *  Function class to compare Vehicle type (NID_VEHICLE_TYPE) in train setup
    */
    struct VehTypeComp
    {
      /**
      * Constructor
      */
      VehTypeComp(const uint8_t vehType);

      /**
      * Checks if the track is type ID is the same
      * @param[in] vehTypeData Type data to compare
      *
      * @return true if the vehicle type ID the same
      */
      bool operator() (const ATP::Kernel::VehicleTypeData& vehTypeData) const;

    private:

      /**
      * Disabled Constructor
      */
      VehTypeComp();

      /**
      * Vehicle type ID to find
      */
      const uint8_t vehTypeToFind;
    };

    /**
    * Defines the storage of the parsed information of a received ShuntingAcknowledge
    *
    */
    struct ShuntingAcknowledge
    {
      Acknowledge       shuntingAcknowledge; //!< Shunting mode granted or not
      uint16_t          allowedSpeedInShunting; //!< Maximum allowed speed during Shunting

      /**
      * Constructor
      */
      ShuntingAcknowledge();
    };

    /**
    * Defines the storage of the parsed information of a received YardAcknowledge
    *
    */
    struct YardAcknowledge
    {
      Acknowledge       yardAcknowledge; //!< Yard mode granted or not
      uint16_t          allowedSpeedInYard; //!< Maximum allowed speed during Yard mode
      /**
      * Constructor
      */
      YardAcknowledge();
    };

    /**
    * Defines the storage of the parsed information of a received Path
    *
    */
    struct Path
    {
      uint16_t                    speedAtBeginningOfPath; //!< Speed at the beginning of the Path
      TrackAndPos                 pathNextTarget; //!< Next Target
      uint16_t                    trackIdList[maxPathTracksSize]; //!< List of tracks in path
      uint16_t                    numberOfTracks; //!< Number of tracks
      uint16_t                    numberOfSpeedChanges; //!< Number of Speed changes
      ETARequestStruct            etaRequestStruct; //!< Estimated time of arrival
      SpeedChangePositionStruct   speedChangePosition[maxSpeedChangePositionSize]; //!< List of Speed change positions

      /**
      * Invalidate all attributes
      */
      void invalidate();
    };

    /**
    * Defines the storage of the parsed information of a received UnconditionalShortening
    *
    */
    struct UnconditionalShortening
    {
      TrackAndPos  endTrack; //!< End of MA position
      uint16_t     allowedMargin; //!< Allowed margin for the vehicle to stop before the end of the MA
    };
  }
}
#endif
