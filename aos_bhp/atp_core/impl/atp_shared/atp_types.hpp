#ifndef ATPTypes_hpp
#define ATPTypes_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*
* DESCRIPTION:
*
* The ATP Types declares common types used by all components
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-01-11    bhermans    Created
* 2016-03-07    lantback    Added TrackAndPos + uintXX
* 2016-03-23    lantback    Correction after review
* 2016-04-03    bhermans    Added enum declarations corresponding with radio-messages
* 2016-04-20    bhermans    Added VehicleType
* 2016-04-22    lantback    Added component type definitions
* 2016-05-05    bhermans    Added maxTrainNameLength, maxCarNameLength
* 2016-07-25    spandita    Updated the type of trackid in TracKandPos Struct
* 2016-08-17    spandita    Removed extra staff responsible mode
* 2016-09-09    Hidaji      Changed atpSelectId to atpTargetCalcId
* 2016-09-13    Hidaji      Added buzzer types
* 2016-09-20    nsyed       Added Config parameter IDs
* 2016-09-21    marlundg    Moved pure radio-related types to radio_message_types.hpp
* 2016-09-28    bhidaji     Added atpBrakeCalcId
* 2016-10-03    arastogi    Added enum for active cab.
* 2016-10-05    bhidaji     Adjusted AlarmType
* 2016-10-18    arastogi    Added enum LocoVsTrainDir
* 2017-03-14    spandita    Added Index for DMI text message
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
   /**
   * Declaration of ATP component ID's
   * Range : 51-151
   */
   const ATC::ComponentIDType atpATPMainId = 51;   //! Event handler component ID
   const ATC::ComponentIDType atpModeControlId = 52;   //! Event handler component ID
   const ATC::ComponentIDType atpMessageHandlerId = 53;   //! Event handler component ID
   const ATC::ComponentIDType atpTargetCalcId = 54;   //! Event handler component ID
   const ATC::ComponentIDType atpSuperviseId = 55;   //! Event handler component ID
   const ATC::ComponentIDType atpBrakeId = 56;   //! Event handler component ID
   const ATC::ComponentIDType atpBrakeCalcId = 57;   //! ID for brake calculation functions. brake calculation is not a component  
   const ATC::ComponentIDType atpDecodeId = 58;   //! Event handler component ID
   const ATC::ComponentIDType atpPositionId = 59;   //! Event handler component ID
   const ATC::ComponentIDType atpOdometryId = 60;   //! Event handler component ID
   const ATC::ComponentIDType atpRadioHandlerId = 61;   //! Event handler component ID
   const ATC::ComponentIDType atpRadioChannelId = 62;   //! Event handler component ID
   const ATC::ComponentIDType atpDMIHandlerId = 63;   //! Event handler component ID
   const ATC::ComponentIDType atpDMIChannelId = 64;   //! Event handler component ID
   const ATC::ComponentIDType atpLocoIOId = 65;   //! Event handler component ID
   const ATC::ComponentIDType atpBTMHandlerId = 66;   //! Event handler component ID
   const ATC::ComponentIDType atpTimsId = 67;   //! Event handler component ID
   const ATC::ComponentIDType atpATOComId = 68;   //! Event handler component ID
   const ATC::ComponentIDType atpVehicleComId = 69;   //! Event handler component ID
   const ATC::ComponentIDType atpVitalCompareId = 70;   //! Event handler component ID
   const ATC::ComponentIDType atpTSetupId = 71;   //! Event handler component ID
   const ATC::ComponentIDType atpTargetsId = 72;   //! Event handler component ID
   const ATC::ComponentIDType atpTracksId = 73;   //! Event handler component ID
   const ATC::ComponentIDType atpCrossCompareId = 74;   //! Event handler component ID
   const ATC::ComponentIDType atpTicId = 75;   //! Event handler component ID
   const ATC::ComponentIDType atpOPCHandlerId = 76;   //! Event handler component ID

   /******************************************************************************
   * Radio Types (used also as AOS internal)
   ******************************************************************************/

   /**
   * ATP modes corresponding with the field Q_ATP_MODE
   */
   enum ATPMode
   {
      ATPModeUndefined = 0,
      ATPModePowerUp,
      ATPModeConfiguration,
      ATPModeRegistration,
      ATPModeBaliseSearch,
      ATPModeNormal,
      ATPModeShunting,
      ATPModeLocation,
      ATPModeYard,
      ATPModeUnregistered,
      ATPModePoweringDown,
      ATPModeSafetyHalt,
      ATPModeSleeping,
      ATPModeStaffResponsible,
      ATPModeShuntingRoute,
      ATPModePossession,
      ATPModeSplit,
      ATPModeJoin,
      ATPModeSafeBrakeToStop,
      ATPModesCount
   };

   /** Route type constants (Q_ROUTE_TYPE)
   */
   enum RouteType
   {
      /** Route type: Undefined */
      RtUndefined = 0,
      /** Route type: Re-Registration */
      RtReRegistration = 3,
      /** Route type: Shunting, used during ATP mode ShuntingRoute */
      RtShuntingRoute = 4,
      /** Route type: Normal */
      RtNormal = 5,
      /** Route type: Join */
      RtJoin = 6,
      /** Route type: Split */
      RtSplit = 7,
      /** Route type: Staff Responsible */
      RtStaffResponsible = 8
   };

   /** Reason for speed change (Q_SPEED)
   */
   enum SpeedChangeReason
   {
      /** Speed change reason: ScrUndefined */
      ScrUndefined = 0,
      /** Speed change reason: ScrPointsStraight */
      ScrPointsStraight = 1,
      /** Speed change reason: ScrPointsCurve */
      ScrPointsCurve = 2,
      /** Speed change reason: ScrPointsPassed */
      ScrPointsPassed = 3,
      /** Speed change reason: ScrLocation */
      ScrLocation = 4,
      /** Speed change reason: ScrTSR */
      ScrTSR = 5,
      /** Speed change reason: ScrRestrictiveSection*/
      ScrRestrictiveSection = 6,
      /** Speed change reason: scrOther */
      ScrOther = 255
   };

   /**
   * Defines the vehicle types corresponding to the field VEHICLE_DATA
   */
   static const uint8_t vehicleUndefined = 0U;
   static const uint8_t vehicleCarMin = 1U;
   static const uint8_t vehicleCarMax = 20U;
   static const uint8_t vehicleLocomotivesMin = 128U;
   static const uint8_t vehicleLocomotivesMax = 254U;
   static const uint8_t vehicleUnknown = 255U;

   /**
   * Defines the bits for B_DIRECTION
   */
   static const uint8_t trainDrivingDirection = 0x01;   //!< Reverse if set, otherwise forwards
   static const uint8_t trainOrientationInTrack = 0x02; //!< Loco closest to leg 0 if set, otherwise leg 1
   static const uint8_t trainLocoOrientation = 0x04;    //!< A end facing cars if set, otherwise B end


   /******************************************************************************
   * AOS Internal
   ******************************************************************************/

   /** Interflo150 position
   *
   * Data structure used to store a full Interflo150 position with track and position.
   */
   struct TrackAndPos
   {
      uint16_t  track;     //!< Identification of Track
      uint32_t  position;  //!< Position of item in Track

      bool operator==(const TrackAndPos& trkAndPos) const
      {
         return((track == trkAndPos.track) &&
            (position == trkAndPos.position));
      }
   };


   /** Travel direction constants (AOS internal)
   */
   enum TravelDir
   {
      /** Travel direction: Unknown */
      DirUndefined = 0,
      /** Travel direction: None */
      DirNone = 1,
      /** Travel direction: Forward */
      DirForward = 2,
      /** Travel direction: Reverse */
      DirReverse = 3,
      /** Travel direction: Both */
      DirBoth = 4
   };

   /** Odometer direction constants (AOS internal)
   */
   enum OdoDir
   {
      /** Odometer direction: Unknown */
      OdoUndefined = 0,
      /** Odometer direction: counting positive */
      OdoPositive = 1,
      /** Odometer direction: counting negative */
      OdoNegative = 2
   };

   /**
   * Locomotive Orientation
   */
   enum LocoVsTrainDir
   {
      locoVsTrainADirection = 0, //!<A end is forward
      locoVsTrainBDirection = 1 //!<B end is forward
   };

   /**
   * The available type of buzzer (LOCOIO output signal) that can be requested by a component
   * buzzer types are defined in order of priority (lower to higher)
   */
   enum BuzzerType
   {
      /** Buzzer Type None: Deactivate the buzzer
      *this will also be use to reset the unspecified duration buzzer
      */
      BuzzerTypeNone = 0,
      /** Buzzer Type One Beep/ short beep: Activate buzzer for 300ms then Off*/
      BuzzerTypeOneBeep = 1,
      /** Buzzer Type two Beep: Activate buzzer for 300ms (On) followed by 200 ms (Off) followed by 300 ms (On) and then Off*/
      BuzzerTypeTwoBeep = 2,
      /** Buzzer Type two Beep per sec: Activate buzzer for 300ms (On) followed by 200 ms (Off)
      *   followed by 300 ms (On) and then 200ms (Off) and repeat.
      *   unspecified time buzzer will be reset by BuzzerTypeNone
      */
      BuzzerTypeTwoBeepsPerSec = 3,
      /** Buzzer Type 30 sec Beep: Activate buzzer for 30s (On) then Off.*/
      BuzzerTypeFor30Sec = 4,
      /** Buzzer Type constant Beep: Activate buzzer for unspecified time and will reset by BuzzerTypeNone*/
      BuzzerTypeConstantBeep = 5
   };

   /**
   * Odometer Position
   */
   typedef int32_t OdoPosition;

   /**
   * DMIMessageType corresponds with MESSAGE TYPE
   */
   enum DMIMessageType
   {
      MTypeDMIMessageTypeUndefined = 0,
      MTypeATPModeAndState = 1,
      MTypeDriverinfo = 3,
      MTypeSpeedAndDistance = 5,
      MTypeTrainConfigData = 8,
      MTypeErrorMessage = 10,
      MTypeManualConfigSelected = 13,
      MTypeTime = 18,
      MTypeVehicleData = 19,
      MTypeTrainVsTrackDirWanted = 20,
      MTypeDMIStatus = 22,
      MTypeConfirmation = 26,
      MTypeDriverIdAndPassword = 27,
      MTypeLocoVsTrainDir = 33,
      MTypeMMIToATPData = 34,
      MTypeTrainVsTrackDir = 39,
      MTypeErasePlanningArea = 40,
      MTypeTextMessage = 42,
      MTypeCelingSpeedList = 46,
      MTypeGradientDataList = 47,
      MTypeDMIStartup = 48,
      MTypeRegistrationArea = 49,
      MTypeReconfigurationSelected = 51,
      MTypeTrainName = 52,
      MTypeRadioChannel = 53,
      MTypeLocationData = 54,
      MTypeTrainLoaded = 55,
      MTypePredefinedTextMessage = 57,
      MTypeATPNotification = 58,
      MTypeTypicalConfig = 59,
      MTypeStartupHistory = 60,
      MTypeVersion = 61,
      MTypeAreaRequest = 62,
      MTypeVehicleType = 63,
      MTypeDMIMessageETA = 64,
      MTypeDMIMessageTrainWeight = 65,
      MTypeDMIMessageTypeMax = 66
   };

   /**
   * Indication if train is loaded or empty corresponding to M_LOADED
   */
   enum TrainLoaded
   {
      TrainIsEmpty = 0, //!<Train is empty
      TrainIsLoaded = 1 //!< Train is loaded
   };

   /**
   * Defines the storage of the parsed MA head information of an arriving MA from TCC
   *
   */
   struct MAHead
   {
      uint8_t      maID;
      uint8_t      timeout;
      uint16_t     ceilingSpeed;
      int8_t       gradient;
      TrainLoaded  trainLoadStatus;
      uint8_t      adhesionValue;
      uint8_t      trainDirection;
      RouteType    routeType;
      TrackAndPos  endOfMATrackAndPos;
      TrackAndPos  startOfMATrackAndPos;
      uint16_t     maMarginCm;
      uint16_t     overlapValue;
   };

   enum CabActiveStatus
   {
      NoCabActive,
      CabAActive,
      CabBActive
   };

   /**
   * Indicates the state of the train setup corresponding with the field Q_TS_STATE
   * Train Setup State Preliminary will be used only for internal functionality.
   */
   enum TrainSetupState
   {
      TrainSetupStateTemporary = 1,
      TrainSetupStatePermanent = 2
   };

   /** Max no of vehicles.
   *
   **/
   static const uint16_t maxVehicleCount = 350U;

   /** Min no. of Vehicles
   *
   **/
   static const uint16_t minVehicleCount = 1U; //!< At least one locomotive

   /**
   * Bit Value for Train Status SafetyHalt ( Data 8 Bit- 0)
   */
   static const uint32_t trainStatusSafetyHalt = 0x00000001U;

   /**
   * Bit Value for Train Status TIMS Integrity Broken ( Data 8 Bit- 2)
   */
   static const uint32_t trainStatusTIMSIntegrityBroken = 0x00000004U;

   /**
   * Bit Value for Train Status Braking Event ( Data 8 Bit- 3)
   */
   static const uint32_t trainStatusBrakingEvent = 0x00000008U;

   /**
   * Bit Value for Train Status Handling Done ( Data 8 Bit- 4)
   */
   static const uint32_t trainStatusHandlingDone = 0x00000010U;

   /**
   * Bit Value for Train Status Train Idling ( Data 8 Bit- 5)
   */
   static const uint32_t trainStatusTrainIdling = 0x00000020U;

   /**
   * Bit Value for Train Status TIMS Integrity Manual Override from Driver ( Data 8 Bit- 6)
   */
   static const uint32_t trainStatusIntegrityInhibitedByDriver = 0x00000040U;

   /**
   * Bit Value for Train Status MA_TimeOut ( Data 8 Bit- 7)
   */
   static const uint32_t trainStatusMaTimeOut = 0x00000080U;

   /**
   * Bit Value for Train Status ATP_Reset ( Data 7 Bit- 0) 
   */
   static const uint32_t trainStatusATPReset = 0x00000100U;

   /**
   * Bit Value for Train Status ATP needs restart ( Data 7 Bit- 1) 
   */
   static const uint32_t trainStatusATPNeedsRestart = 0x00000200U;

   /**
   * Bit Value for Train Status ATPIntervention ( Data 7 Bit- 2) 
   */
   static const uint32_t trainStatusATPIntervention = 0x00000400U;

   /**
   * Bit Value for Train Status Brake release Requested ( Data 7 Bit- 3) 
   */
   static const uint32_t trainStatusBrakeReleaseRequested = 0x00000800U;

   /**
   * Bit Value for Train Status Slip detected ( Data 7 Bit- 5) 
   */
   static const uint32_t trainStatusSlipDetected = 0x00002000U;

   /**
   * Bit Value for Train Status Free rolling ( Data 7 Bit- 6) 
   */
   static const uint32_t trainStatusFreeRolling = 0x0004000U;

   /**
   * Bit Value for Train Status Emergency Alert Active ( Data 7 Bit- 7) 
   */
   static const uint32_t trainStatusEmergencyAlertActive = 0x0008000U;

   /**
   * Bit Value for Train Status Attention Needed (ATO) ( Data 6 Bit- 0) 
   */
   static const uint32_t trainStatusAttentionNeededATO = 0x0010000U;

   /**
   * Bit Value for Train Status Not ready to drive ( Data 6 Bit- 1) 
   */
   static const uint32_t trainStatusNotReadyToDrive = 0x0020000U;

   /**
   * Bit Value for AOS down loadable parameters not received ( Data 6 Bit- 4) 
   */
   static const uint32_t trainStatusAosDownloadedParameterNotRecv = 0x0080000U;

   /** ATO Mode selector type
   */
   enum ATOMode
   {
      ATOModeUndefined = 0,
      ATOModeManual,
      ATOModeSupervisedAutomatic,
      ATOModeAutomatic,
      ATOModeRemote
   };

   /**
   * TrackDataType declares the types corresponding with the field Q_TRACK_DATA_TYPE
   */
   enum TrackDataType
   {
      TrackDataTypeUndefined = 0,
      TrackDataTypePowerSectionLimit,
      TrackDataTypeWheelLubricationStart,
      TrackDataTypeWheelLubricationEnd,
      TrackDataTypeOdometerInvalidStart,
      TrackDataTypeOdometerInvalidEnd,
      TrackDataTypeFreeRollingStart,
      TrackDataTypeFreeRollingEnd,
      TrackDataTypeSafetyMarginChange,
      TrackDataTypeAdhesionChange,
      TrackDataTypeAcousticSignal
   };

   /**
   * Max number of brake systems, used in train setup and brakeability.
   */
   static const uint8_t maxBrakeSystems = 3U;

   /**
   * Maximum size of the Brake System string (including the NULL character)
   */
   static const uint8_t maxBrakeSystemTextSize = 21U;

   /**
    * Max string length of driver field of an ATP-DMI and TCC-AOS communication packet,
    * used in ATP::Kernel::DriverInformation and ATP::DMICom::DriverIdAndPassword
    */
   static const uint8_t driverIdMaxLength = 20U;

   /**
   * Max string length of the password field of a TCC-AOS a communication packet, used in ATP::Kernel::DriverInformation
   */

   static const uint8_t tccPasswordIdMaxLength = 20U;
   /**
    * Max string length of the password field of an ATP-DMI communication packet, used in ATP::DMICom::DriverIdAndPassword
    */
   static const uint8_t dmiPasswordIdMaxLength = 20U;

   /**
   * Max string length of train name, used in:
   * - ATP::DS::AbstractTSetup::TrainNameStorage::trainName
   * - ATP::DMICom::DMIMessageOutTrainConfigData::trainName
   * - ATP::DMICom::DMIMessageOutTrainName::trainName
   * - ATP::Kernel::TrainName::trainName
   */
   static const uint8_t trainNameMaxLength = 20U;

   /**
   * Max string length of vehicle name
   */
   static const uint8_t vehicleNameMaxLength = 20U;

   /**
   * Max string length of vehicle type name
   */
   static const uint8_t vehicleTypeNameMaxLength = 20U;

   /**
   * Max vehicle types Blocks
   */
   static const uint8_t maxVehicleTypeBlocks = 20U;

   /** Max length of location data field
   */
   static const uint8_t maxLocationNameLength = 20U;

   /**
   * The type definition for the various triggers of a Mandatory Brake Test
   * Declaration of Mandatory Brake Test Reasons
   * Range: 0 : Mandatory Brake Test not requested by ATP
   *        1-10 : Core
   *        11-20: Adaptation
   */
   typedef uint8_t BrakeTestReasonType;

   static const BrakeTestReasonType braketestReasonNone = 0U; //!< No mandatory Brake test requested
   static const BrakeTestReasonType brakeTestReasonTimer = 1U; //!< Timer for Mandatory Brake Test has expired
   static const BrakeTestReasonType brakeTestReasonEbOrderFault = 2U; //!<Safety halt raised due to fault with Eb Orders, Brake Test is now mandatory after restart!
   static const BrakeTestReasonType brakeTestReasonBrakeRelayFault = 3U; //!<Safety halt raised due to fault with Eb Orders, Brake Test is now mandatory after restart!
   static const BrakeTestReasonType brakeTestReasonNewInstallation = 4U; //!<New installation of AOS, Brake Test is mandatory at Power Up.
   static const BrakeTestReasonType brakeTestReasonIsolABStatusInput = 5U; //!<Safety halt raised due to Isolation Switches A/B Status Input, Brake Test is now mandatory after restart!


   /** Brake Type, corresponding with the field M_BRAKE_SYSTEM
   */
   enum BrakeSystemType
   {
      BrakeSystemTypeUndefined = 0,
      BrakeSystemType1 = 1,
      BrakeSystemType2 = 2,
      BrakeSystemType3 = 3
   };

   /** Brake Parameter
   */
   enum BrakeParameter
   {
      BrakeParameterA1 = 0,
      BrakeParameterA2,
      BrakeParameterA3,
      BrakeParameterB1,
      BrakeParameterB2,
      BrakeParameterB3,
      BrakeParameterV1,
      BrakeParameterV2,
      BrakeParameterT1SB,
      BrakeParameterT2SB,
      BrakeParameterT3SB,
      BrakeParameterT1EB,
      BrakeParameterT2EB,
      BrakeParameterT3EB
   };

   /**
   * Enum for ATO Mode Switch
   */
   enum ATOModeSwitchPos
   {
      ATOModeUnknownPos = 0,
      ATOModeManualPos,
      ATOModeSupervisedPos,
      ATOModeAutomaticPos
   };

   /**
   * LocationType declares the locations corresponding with the field NID_LOCATION_TYPE
   */
   enum LocationType 
   {
      UndefinedLocationType = 0,
      LocationTypeManualHandling = 1,
      LocationTypeRemoteLoad = 2,
      LocationTypeYardMode = 3,
      LocationtypeUnloadLocation = 4,
      LocationTypeFutureCoreMin = 5,
      LocationTypeFutureCoreMax = 128,
      LocationTypeFutureAdapMin = 129,
      LocationTypeFutureAdapMax = 255,
   };

   /**
   * Max string length of Radio Channel Name
   */
   static const uint8_t radioChannelNameMaxLength = 20U;

   /**
   * The maximum time [ms] to wait for a confirmation from Driver by ACP Procedure .
   */
   static const int64_t maxConfirmationTime = 5000;

   /**
   * Maximum Number of Speed Target
   */
   static const uint16_t maxNumberOfSpeedTargets = 50U;

   /**
   * Maximum Number of Gradient Targets
   */
   static const uint16_t maxNumberOfGradientTargets = 50U;

   /**
   * Maximum Number of TrackDataItem Targets
   */
   static const uint16_t maxNumberOfTrackDataItemTargets = 50U;

   /**
   * Maximum Number of Primary Targets
   */
   static const uint16_t maxNumberOfPrimaryTargets = 3U;

   /**
   * Maximum Number of Total Targets
   */
   static const uint16_t maxNumberOfTotalTargets = (maxNumberOfTrackDataItemTargets + maxNumberOfSpeedTargets + maxNumberOfGradientTargets + maxNumberOfPrimaryTargets);

   /**
   * Maximum Number of supervised speed Targets per speed target
   */
   static const uint16_t maxNrSupvSpeedTargets = 4U;

   /**
   * Maximum Number of supervised Gradient Targets per gradient targets
   */
   static const uint16_t maxNrSupvGradTargets = 5U;

   /**
   * Maximum Number of supervised primary Targets per primary targets
   */
   static const uint16_t maxNrSupvPrimaryTargets = 4U;

   /**
   * Maximum Number of Total Supervised Targets
   */
   static const uint16_t maxNumberOfSupvTotalTargets = ((maxNumberOfSpeedTargets*maxNrSupvSpeedTargets) + (maxNumberOfGradientTargets*maxNrSupvGradTargets) + (maxNumberOfPrimaryTargets*maxNrSupvPrimaryTargets));
}

#endif
