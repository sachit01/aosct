#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          LCSSimConsts.h %
*
*  %version:       9 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-07-12 18:40 %
*
*  DESCRIPTION:     Declaration of constants used in LCSSim
*
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2013-10-26    Antbäck     File created
* 2017-02-27    Marlundg    Adapted for BHP Project
*
*******************************************************************************/

/*******************************************************/
/* Definition of sizes                                 */
/*******************************************************/
#define TRAIN_STATUS_GPS_SIZE   45

/*******************************************************/
/* Constants                                 */
/*******************************************************/
// Max Vehicles in ECPB Train Composition Message
const int maxVehicles = 350;

// Max number of Tracks in Path in Path Message
const int maxPathTracks = 200;// From Requirements

// Max number of speed changes in Path in Path Message
const int maxSpeedChange = 100;// From Requirements

// Max number of Speed Curve Points in the Warning Curve Message
const int maxSpeedCurvePoints = 200;

// Number of cycles to scan train config
const unsigned short cyclesToScan = 30; // 100ms * 30 = 3 seconds

// Number of bytes reserved for the GPS of locomotive front
const unsigned short bytesForGPSFrontPos = 13;

// Number of bytes reserved for the GPS of rear position
const unsigned short bytesForGPSRearPos = 13;

                                        
/*******************************************************/
/* Enums                                               */
/*******************************************************/
typedef enum
{
    DirectionUndef = 0,
    DirectionNeutral = 1,
    DirectionForward = 2,
    DirectionReverse = 3
} DirectionEnum;



typedef enum
{
    ATOModeUndefined = 0,
    ATOModeManual = 1,
    ATOModeSupervised = 2,
    ATOModeAutomatic = 3
} ATOModeEnum;

typedef enum
{
    DS_NotMoving = 0,
    DS_Streching = 1,
    DS_Accelerating = 2,
    DS_Coasting = 3,
    DS_Retarding = 4,
    DS_Stopping = 5,
    DS_vRefCoastingReached = 6,
    DS_RemoteControl = 7,
    DS_FreeRolling = 8,
    DS_Undefined = 0xFF
} DriverStateEnum;

typedef enum
{
    PantoPosNone = 0,
    PantoPosRoof = 1,
    PantoPosSide = 2,
    PantoPosUndef = 3
} PantographReqEnum;

typedef enum
{
    PantoPSUndef = 0,
    PantoPSBattery = 1,
    PantoPSTrolley = 2
} PantoPowerSourceEnum;

typedef enum
{
    LCSAlarm_Reset = 0,
    LCSAlarm_SystemError = 1,
    LCSAlarm_Warning = 2,
    LCSAlarm_LowSeverity = 3,
    LCSAlarm_HighSeverity = 4,
} AlarmClassEnum;

typedef enum
{
    VSIMCon_Undef,
    VSIMCon_NotConnected,
    VSIMCon_Connected,
    VSIMCon_Running
} VSIMComStatusEnum;

typedef enum
{
    LCSStateNormal = 0
} LCSStateEnum;

typedef enum
{
    ATOModeCSSManual = 0,
    ATOModeCSSSupervised = 1,
    ATOModeCSSAutomatic = 2,
    ATOModeCSSUndefined = 255
} ATOModeCabinSelectorStatusEnum;

typedef enum
{
    ATODrivingModeNone = 0,
    ATODrivingModeETAPacing = 1,
    ATODrivingModeCeilingSpeed = 2,
    ATODrivingModeLoading = 3,
    ATODrivingModePrecisionStop = 4,
    ATODrivingModeUnloading = 5,
    ATODrivingModeNotAsserted = 255
} ATODrivingModeEnum;

typedef enum
{
    NormalMode = 0,
    FreeRollingMode = 1
} FreeRollingStatusEnum;

typedef enum
{
    BlueFlagStatusInactive = 0,
    BlueFlagStatusActivated = 1
} BlueFlagStatusEnum;

typedef enum
{
    BlueFlagNotRequested = 0,
    BlueFlagActivationRequest = 1,
    BlueFlagDeActivationRequest = 2
} BlueFlagRequestEnum;

typedef enum
{
    AdsEtaStatusNotDefined = 0,
    AdsEtaStatusCalculating = 1,
    AdsEtaStatusRequestAccepted = 2,
    AdsEtaStatusRequestRejected = 3
} AdsEtaStatusEnum;

typedef enum
{
    LCSATONotReady = 0,
    LCSATOReady = 1,
    LCSATONotAsserted = 255
} LCSATOReadyEnum;

typedef enum
{
    ECPBSequenceStatusUnknown = 0,
    ECPBSequenceStatusScanning = 1,
    ECPBSequenceStatusConfigKnown = 2
} ECPBSequenceStatusEnum;

typedef enum
{
    ReadyForPrecisionStopNoAction = 0,
    ReadyForPrecisionStopReady = 1
} ReadyForPrecisionStopEnum;

typedef enum
{
    BrakeSystemInUsePneumatic = 0,
    BrakeSystemInUseECPB = 1
} BrakeSystemInUseEnum;

typedef enum
{
    ECPBOperatingModeInitialization = 1,
    ECPBOperatingModeRun = 0,
    ECPBOperatingModeSwitch = 2,
    ECPBOperatingModeCutOut = 3,
    ECPBOperatingModeNotAvailable = 7
} ECPBOperatingModeEnum;

typedef enum
{
    TrainIntegrityStatusECPBWaiting = 0,
    TrainIntegrityStatusECPBConfirmed = 1,
    TrainIntegrityStatusECPBNotAsserted = 255
} TrainIntegrityStatusECPBEnum;

typedef enum
{
    LCSMessageTypeToATPNoMessage = 0,
    LCSMessageTypeToATPTrainStatus = 50101,
    LCSMessageTypeToATPTrainComposition = 50102,
    LCSMessageTypeToATPRclStatus = 50103
} LCSMessageTypeToATPEnum;

typedef enum 
{
  HandlingDoneUndefined = 0,
  HandlingDoneRequest = 1,
  HandlingDoneNotRequest = 2
} HandlingDoneEnum;

/* From ATO */

typedef enum
{
    ATOModeCabinSelectorManual = 0,
    ATOModeCabinSelectorSupervised = 1,
    ATOModeCabinSelectorAutomatic = 2,
    ATOModeCabinSelectorUndefined = 255
} ATOModeCabinSelectorEnum;

typedef enum
{
    TrainIdlingMaExists = 0,
    TrainIdlingTrainIsIdling = 1,
    TrainIdlingNOtAsserted = 255
} TrainIdlingEnum;

typedef enum
{
    TrainOrientationLeadLocoToLeg0 = 0,
    TrainOrientationLeadLocoToLeg1 = 1
} TrainOrientationEnum;

typedef enum
{
    TravelDirectionLocoLeading = 0,
    TravelDirectionLocoTrailing = 1
} TravelDirectionEnum;

typedef enum
{
    BlueFlagNotActive = 0,
    BlueFlagActive = 1
} BlueFlagEnum;


typedef enum
{
  NotRequested = 0,
  Requested = 1,
  NotAsserted = 255
} CommandMessageEnum;

typedef enum
{
  LocoLeading  = 0,
  LocoTrailing = 1,
  Undefined    = 255
} MADirectionEnum;

typedef enum
{
    LCSMessageTypeFromATPNoMessage = 0,
    LCSMessageTypeFromATPAOSStatus = 50001,
    LCSMessageTypeFromATPATPCommand = 50002,
    LCSMessageTypeFromATPATOCommand = 50012,
    LCSMessageTypeFromATPATODriving = 50003,
    LCSMessageTypeFromATPMovementAuthority = 50004,
    LCSMessageTypeFromATPWarningCurve = 50005,
    LCSMessageTypeFromATPTrainComposition = 50009,
    LCSMessageTypeFromATPPath = 50011,
    LCSMessageTypeFromATPRclInformation = 50013
} LCSMessageTypeFromATPEnum;

typedef enum
{
  NotActive = 0,
  Active = 1,
  ModeNotAsserted =255
} LimitedSupModeEnum;

typedef enum
{
    VehicleTypeUnknown = 0,
    VehicleTypeLocomotive = 1,
    VehicleTypeCar = 2,
    VehicleTypeNotDefined = 255
} VehicleTypeEnum;

typedef enum
{
  GPSPosAccuracyNoFix = 0,
  GPSPosAccuracyGPSFix = 1,
  GPSPosAccuracyDiffGPSFix = 2
}GPSPosAccuracyEnum;

typedef enum
{
  BreakNotActive = 0,
  BreakActive = 1
} LocoPenaltyBrkActiveEnum;


typedef enum
{
  AosOperationalModeUndefined = 0,
  AosOperationalModeNormal = 1,
  AosOperationalModeLocation = 2,
  AosOperationalModeOther = 3
} AosOperationalModeEnum;

typedef enum
{
  AosInterventionNotApplied = 0,
  AosInterventionApplied = 1
} AosInterventionEnum;

typedef enum
{
  AllowedTrainMovementUndefined = 0,
  AllowedTrainMovementForward = 1,
  AllowedTrainMovementReverse = 2,
  AllowedTrainMovementBoth = 3,
  AllowedTrainMovementNone = 4
} AllowedTrainMovementEnum;

typedef enum
{
  RclTrainOrientationUndefined = 0,
  RclTrainOrientationTrainForwardLocomotiveForward = 1,
  RclTrainOrientationTrainForwardLocomotiveReverse = 2
} RclTrainOrientationEnum;
