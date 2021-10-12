#ifndef AbstractLCSMessageCommon_hpp
#define AbstractLCSMessageCommon_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:  
* Definitions of general helper-functions for validating, converting and 
* collecting data in LCS Message Handler.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-23    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vector>
#include "atp_types.hpp"
#include "trace_interface.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace TG
  { 
    /** Trace Levels
    */
    extern const uint8_t briefTrace;        //<! Trace-level, brief trace 
    extern const uint8_t detailedTrace;     //<! Trace-level, detailed trace
    extern const uint8_t veryDetailedTrace; //<! Trace-level, very detailed trace

    /**
    * Trace binary data in hex-format.
    *
    *  @param[in] trace       Trace interface to be used
    *  @param[in] level       Tracelevel
    *  @param[in] data        Pointer to binarydata to trace
    *  @param[in] dataLength  The number of bytes to trace
    *
    */
    void traceBinaryData(ATC::TraceInterface const * const trace, const uint8_t level, const uint8_t data[], const uint32_t dataLength);


    /** Enum of the data processing state
    */
    enum DataProcessState
    {
      NoDataAvailable = 0,    //<! No data is available for processing
      DataAvailable,          //<! Data is available for validation
      DataValidated           //<! Data is validated and published
    };

    /**
    * Message types between AOS-LCS
    * Messages sent from AOS to LCS has a LCSMessageType < 50101
    * Messages sent from LCS to AOS has a LCSMessageType >= 50101
    */
    enum LCSMessageType
    {
      // AOS to LCS
      LCSMTypeStatusMessage = 50001,
      LCSMTypeATPCommandMessage = 50002,
      LCSMTypeAtoDrivingData = 50003,
      LCSMTypeMovementAuthority = 50004,
      LCSMTypeWarningCurveMessage = 50005,
      LCSMTypeSpare1 = 50006,
      LCSMTypeSpare2 = 50007,
      LCSMTypeSpare3 = 50008,
      LCSMTypeTrainCompositionMessage = 50009,
      LCSMTypeSpare4 = 50010,
      LCSMTypePath = 50011,
      LCSMTypeAtoCommand = 50012,
      LCSMTypeRclInformation = 50013,
      LCSMTypeAOSToLCSMax = 50014,

      // LCS to AOS
      LCSMTypeTrainStatusMessage = 50101,
      LCSMTypeECPBTrainCompositionMessage = 50102,
      LCSMTypeRCLStatusMessage = 50103,
      LCSMTypeLCStoAOSMax = 50104
    };
    
    /** ATO Mode Cabin Selector Status
    */
    enum ATOModeCabinSelectorType
    {
      ATOModeCabinManual = 0,
      ATOModeCabinSupervised = 1,
      ATOModeCabinAutomatic = 2,
      ATOModeCabinNotAsserted = 255
    };

    /** ATO Driving
    */
    enum ATODrivingModeType
    {
      ATODrivingNone = 0,
      ATODrivingETAPacing = 1,
      ATODrivingCeilingSpeed = 2,
      ATODrivingLoading = 3,
      ATODrivingPrecisionStop = 4,
      ATODrivingUnloading = 5,
      ATODrivingNotAsserted = 255
    };

    /** Free Rolling
    */
    enum FreeRollingStatusType
    {
      FreeRollingStatusNormalOperation = 0,
      FreeRollingStatusFreeRoll
    };

    /** Blue flag status
    */
    enum BlueFlagStatusType
    {
      BlueFlagInactive = 0,
      BlueFlagActivated
    };

    /** Blue flag request
    */
    enum BlueFlagRequestType
    {
      BlueFlagNotRequested = 0,
      BlueFlagActivationRequest,
      BlueFlagDeactivationRequest
    };

    /** ADS ETA Status
    */
    enum AdsEtaStatus
    {
      AdsEtaNotdefined = 0,       //<! Not Defined/Calculated
      AdsEtasCalculating,
      AdsEtaEtaAccepted,
      AdsEtaRejected
    };

    /** ATO Ready
    */
    enum LcsAtoStatusType
    {
      LcsAtoNotReady = 0,
      LcsAtoReady = 1,
      LcsAtoNotAsserted = 255
    };

    /** ECPB sequence status
    */
    enum EcpbSequenceStatusType
    {
      EcpbSequenceUnknown = 0,
      EcpbSequenceScanning,
      EcpbSequenceConfigurationKnown
    };

    /** Ready for precision stop
    */
    enum ReadyForPrecisionStopType
    {
      PrecisionStopNoAction = 0,
      PrecisionStopReady
    };

    /** Reported ECPB brake system
    */
    enum ReportedEcpbBrakeSystem
    {
      BrakeSystemPneumatic = 0,
      BrakeSystemEcpb
    };

    /** ECPB operating modes
    */
    enum EcpbOperatingModesType
    {
      EcpbOperatingNotAvailable = 7,
      EcpbOperatingInitializationMode = 1,
      EcpbOperatingRunMode = 0,
      EcpbOperatingSwitchMode = 2,
      EcpbOperatingCutOutMode = 3
    };

    /** Train integrity status ECPB
    */
    enum TrainIntegrityStatusEcpbType
    {
      TrainIntegrityWaitingForConfirmation = 0,
      TrainIntegrityConfirmed = 1,
      TrainIntegrityBroken = 2,
      TrainIntegrityNotAsserted = 255
    };

    /** Train idling
    */
    enum TrainIdlingType
    {
      TrainIdlingMaExists = 0,
      TrainIdlingTrainIsIdling = 1,
      TrainIdlingNotAsserted = 255
    };
    
    /** Train orientation on front track
    */
    enum TrainOrientationFrontTrackType
    {
      TrainOrientationFrontTrackLeadTowardsLeg0 = 0,
      TrainOrientationFrontTrackLeadTowardsLeg1
    };

    /** Train orientation on rear track
    */
    enum TrainOrientationRearTrackType
    {
      TrainOrientationRearTrackLeadTowardsLeg0 = 0,
      TrainOrientationRearTrackLeadTowardsLeg1
    };

    /** Travel direction
    */
    enum TravelDirectionType
    {
      TravelDirectionLocoLeading = 0,
      TravelDirectionLocoTrailing
    };
    
    /** Vehicle Type
    */
    enum LCSVehicleType
    {
      LCSVehicleTypeUnknown = 0,
      LCSVehicleTypeLocomotive,
      LCSVehicleTypeCar
    };

    /** Limited Supervised Mode
    */
    enum LimitedSupervisedModeType
    {
      LimitedSupervisedModeNotActive = 0,
      LimitedSupervisedModeActive = 1,
      LimitedSupervisedModeNotAsserted = 255
    };
    
    /** Penalty Break Active
    */
    enum PenaltyBreakActiveType
    {
      PenaltyBreakNotActive = 0,
      PenaltyBreakActive = 1
    };

    /** Handling Done
    */
    enum HandlingDoneType
    {
      HandlingDoneUndefined = 0,
      HandlingDoneRequest = 1,
      HandlingDoneNotRequest = 2
    };

    /** AOS Operational mode
    */
    enum AosOperationalModeType
    {
      AosOperationalModeUndefined = 0,
      AosOperationalModeNormal = 1,
      AosOperationalModeLocation = 2,
      AosOperationalModeOther = 3
    };

    /** AOS intervention applied
    */
    enum AosInterventionType
    {
      AosInterventionNotApplied = 0,
      AosInterventionApplied = 1
    };

    /** Allowed train movement
    */
    enum AllowedTrainMovementType
    {
      AllowedTrainMovementUndefined = 0,
      AllowedTrainMovementForward = 1,
      AllowedTrainMovementReverse = 2,
      AllowedTrainMovementBoth = 3,
      AllowedTrainMovementNone = 4
    };
    

    /** Train Orientation
    */
    enum TrainOrientationType
    {
      TrainOrientationUndefined = 0,
      TrainOrientationTrainForwardLocomotiveForward = 1,
      TrainOrientationTrainForwardLocomotiveReverse = 2
    };


    /** Locomotive System Faults
    */
    static const uint32_t lossOfLeaderComm = 0x01U;
    static const uint32_t lossOfAirBrakeComm = 0x02U;
    
    /** Maximum Length of GPS Position Data
    */
    static const uint8_t gpsPosMaxLength = 13U;

    /** ADS System Faults
    */
    static const uint8_t pathDefined              = 0x01U;
    static const uint8_t maDefined                = 0x02U;
    static const uint8_t atpWarningCurveDefined   = 0x04U;
    static const uint8_t trainConfigDefined       = 0x08U;
    static const uint8_t weightDefined            = 0x10U;
    
    /**
    * LCS Train Status
    */
    struct LCSTrainStatusType
    {
      /**
      * Default constructor. Initializes all members of the struct.
      */
      LCSTrainStatusType();

      /**
      * Add members to cross compare
      */
      void initCrossCompare() const;

      ATOModeCabinSelectorType   atoModeCabinSelectorStatus;
      ATODrivingModeType  atoDrivingMode;
      FreeRollingStatusType  freeRollingStatus;
      BlueFlagStatusType   blueFlagStatus;
      BlueFlagRequestType   blueFlagRequest;
      AdsEtaStatus adsEtaStatus;
      uint32_t  adsEta;
      LcsAtoStatusType   lcsAtoStatus;
      EcpbSequenceStatusType   ecpbSequenceStatus;
      ReadyForPrecisionStopType   readyForPrecisionStop;
      ReportedEcpbBrakeSystem  reportedEcpbBrakeSystem;
      EcpbOperatingModesType  ecpbOperatingModes;
      TrainIntegrityStatusEcpbType   trainIntegrityStatusEcpb;
      uint8_t percentageOfOperativeBrakesEcpb;
      uint8_t lastCarBrakePressure;
      uint8_t gpsPositionLoco[gpsPosMaxLength];
      uint8_t gpsPositionLastCar[gpsPosMaxLength];
      uint16_t versionOfAdsMap;
      PenaltyBreakActiveType penaltyBreakStatus;
      uint32_t  locomotiveSystemFaults;
      uint32_t  adsStatus;
      uint32_t  timeStamp;
    };
    
    /**
    * Rolling Stock Position Items with road number and vehicle type
    */
    struct RollingStockPositionType
    {
      LCSVehicleType  vechicleType;
      uint16_t        roadNumber;
    };

    /**
    * ECPBTrainComposition
    */
    struct ECPBTrainCompositionType
    {
      /**
      * Add members to cross compare
      */
      void initCrossCompare() const;

      uint16_t              numberOfDetectedVehiclesByECP;
      uint16_t              numberOfDetectedVehiclesUnknownPos;
      uint16_t              numberOfNotDetectedVehicles;
      RollingStockPositionType  rollingStockPosition[maxVehicleCount];
    };

    /**
    * Converts from LCS Vehicle Type to AOS VehicleType
    *
    *  @param[in] lcsVtype   LCS Vehicle Type
    *
    *  @return Corresponding AOS Vehicle Type
    */
    uint8_t lcsToAosVtype(const LCSVehicleType lcsVtype);

    /**
    * Converts from AOS VehicleType LCS Vehicle Type
    *
    *  @param[in] aosVtype   AOS Vehicle Type
    *
    *  @return Corresponding LCS Vehicle Type
    */
    LCSVehicleType aosToLcsVtype(const uint8_t aosVtype);

    /**
    * Validating the ATOModeCabinSelectorStatus parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateATOModeCabinSelectorStatus(const uint8_t val);

    /**
    * Validating the ATODrivingMode parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateATODrivingMode(const uint8_t val);

    /**
    * Validating the TractionMode parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateTractionMode(const uint8_t val);

    /**
    * Validating the BlueFlagStatus parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateBlueFlagStatus(const uint8_t val);

    /**
    * Validating the BlueFlagRequest parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateBlueFlagRequest(const uint8_t val);

    /**
    * Validating the AdsEtaStatus parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateAdsEtaStatus(const uint8_t val);

    /**
    * Validating the LcsAtoReady parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateLcsAtoReady(const uint8_t val);

    /**
    * Validating the EcpbSequenceStatus parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateEcpbSequenceStatus(const uint8_t val);

    /**
    * Validating the ReadyForPrecisionStop parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateReadyForPrecisionStop(const uint8_t val);

    /**
    * Validating the BrakeSystemInUse parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateBrakeSystemInUse(const uint8_t val);

    /**
    * Validating the EcpbOperatingModes parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateEcpbOperatingModes(const uint8_t val);

    /**
    * Validating the TrainIntegrityStatusEcpb parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateTrainIntegrityStatusEcpb(const uint8_t val);

    /**
    * Validating the Total Rolling Stock Vehicles detected by ECP
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool rangeCheckRollingStockVehiclesDetectedByECP(const uint16_t val);

    /**
    * Validating the maximum rolling stock vehicles parameter
    *
    *  @param[in] val Value to be validated
    *  @param[in] maxVal Max-value
    *
    *  @return True if value is within limits
    *
    */
    bool validateMaxRollingStockVehicles(const uint16_t val, const uint16_t maxVal);

    /**
    * Validating the VehicleType parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateVehicleType(const uint8_t val);

    /**
    * Validating the PenaltyBreakActive parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validatePenaltyBreakActive(const uint8_t val);

    /**
    * Validating the Locomotive System Faults
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateLocomotiveSystemFaults(const uint32_t val);

    /**
    * Validating the ADS Status parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateAdsStatus(const uint32_t val);

    /**
    * Validating the RCL Status parameter
    *
    *  @param[in] val Value to be validated
    *
    *  @return True if value is within limits
    *
    */
    bool validateRclStatus(const uint8_t val);

  }
}
#endif
