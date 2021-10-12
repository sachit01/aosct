/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Implementation of general helper-functions for validation, convertion and
* collecting data in LCS Message Handler.
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
#include <stdio.h>
#include <vfw_types.h>

#include "lcs_message_common.hpp"
#include "abstract_vehicle_com.hpp"
#include "vehicle_com.hpp"
#include "abstract_cross_compare.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace TG
  {

    /**
    * Max size of binary data to trace.
    */
    static const uint16_t MaxBytesInTraceBuffer = 1024U;


    /******************************************************************************
    * traceMessageData
    ******************************************************************************/
    void traceBinaryData(ATC::TraceInterface const * const trace, const uint8_t level, const uint8_t data[], const uint32_t dataLength)
    {
      uint8_t currentTraceLevel = 0U;
      bool isEnabled = false;

      trace->getTraceDetails(currentTraceLevel, isEnabled);

      // Filter on trace-level
      if ((currentTraceLevel >= level) && isEnabled)
      {
        const uint16_t charsPerByte = 3U;
        static char_t tmpTraceBuffer[(MaxBytesInTraceBuffer * charsPerByte) + 1U]; // add one for NUL termination
        tmpTraceBuffer[0] = '\0';

        // Max number of bytes are MaxBytesInTraceBuffer
        uint32_t nrOfBytes = ((dataLength > MaxBytesInTraceBuffer) ? MaxBytesInTraceBuffer : dataLength);

        bool errorInString = false;

        for (uint32_t i = 0U; i < nrOfBytes; ++i)
        {
          //lint -e{586} snprintf is needed here
          const int32_t retVal = snprintf(&tmpTraceBuffer[charsPerByte * i], charsPerByte + 1U, "%02X:", data[i]); // add one for NUL termination

          if ((retVal > 0) && (static_cast<size_t>(retVal) <= charsPerByte))
          {
            // OK
          }
          else
          {
            errorInString = true;
          }
        }

        if (!errorInString)
        {
          trace->write(ATC::veryDetailedTrace, &tmpTraceBuffer[0]);
        }
      }
    }

    /******************************************************************************
    * lcsToAosVtype
    ******************************************************************************/
    uint8_t lcsToAosVtype(const LCSVehicleType lcsVtype)
    {
      uint8_t vehType = vehicleUndefined;
      if (validateVehicleType(static_cast<uint8_t>(lcsVtype)))
      {
        //Do nothing used to avoid lint
      }
      return vehType;
    }

    /******************************************************************************
    * aosToLcsVtype
    ******************************************************************************/
    LCSVehicleType aosToLcsVtype(const uint8_t aosVtype)
    {
      LCSVehicleType lcsVehicleType = LCSVehicleTypeUnknown;

      if ((vehicleCarMin <= aosVtype) && (aosVtype <= vehicleCarMax))
      {
        lcsVehicleType = LCSVehicleTypeCar;
      }
      else if ((vehicleLocomotivesMin <= aosVtype) && (aosVtype <= vehicleLocomotivesMax))
      {
        lcsVehicleType = LCSVehicleTypeLocomotive;
      }
      else if (vehicleUnknown == aosVtype)
      {
        lcsVehicleType = LCSVehicleTypeCar;
      }
      else
      {
        //do nothing
      }

      return lcsVehicleType;
    }

    /******************************************************************************
    * Start validateXXX functions
    ******************************************************************************/

    bool validateATOModeCabinSelectorStatus(const uint8_t val)
    {
      return((val <= static_cast<uint8_t>(ATOModeCabinAutomatic)) || (static_cast<uint8_t>(ATOModeCabinNotAsserted) == val));
    };

    bool validateATODrivingMode(const uint8_t val)
    {
      return((val <= static_cast<uint8_t>(ATODrivingUnloading)) || (static_cast<uint8_t>(ATODrivingNotAsserted) == val));
    };

    bool validateTractionMode(const uint8_t val)
    {
      return(val <= static_cast<uint8_t>(FreeRollingStatusFreeRoll));
    };

    bool validateBlueFlagStatus(const uint8_t val)
    {
      return(val <= static_cast<uint8_t>(BlueFlagActivated));
    };

    bool validateBlueFlagRequest(const uint8_t val)
    {
      return(val <= static_cast<uint8_t>(BlueFlagDeactivationRequest));
    };

    bool validateAdsEtaStatus(const uint8_t val)
    {
      return(val <= static_cast<uint8_t>(AdsEtaRejected));
    };

    bool validateLcsAtoReady(const uint8_t val)
    {
      return((val <= static_cast<uint8_t>(LcsAtoReady)) || (static_cast<uint8_t>(LcsAtoNotAsserted) == val));
    };

    bool validateEcpbSequenceStatus(const uint8_t val)
    {
      return(val <= static_cast<uint8_t>(EcpbSequenceConfigurationKnown));
    };

    bool validateReadyForPrecisionStop(const uint8_t val)
    {
      return(val <= static_cast<uint8_t>(PrecisionStopReady));
    };

    bool validateBrakeSystemInUse(const uint8_t val)
    {
      return(val <= static_cast<uint8_t>(BrakeSystemEcpb));
    };

    bool validateEcpbOperatingModes(const uint8_t val)
    {
      return((val <= static_cast<uint8_t>(EcpbOperatingCutOutMode)) || (static_cast<uint8_t>(EcpbOperatingNotAvailable) == val));
    }

    bool validateTrainIntegrityStatusEcpb(const uint8_t val)
    {
      return((val <= static_cast<uint8_t>(TrainIntegrityBroken)) || (static_cast<uint8_t>(TrainIntegrityNotAsserted) == val));
    };

    bool validateMaxRollingStockVehicles(const uint16_t val, const uint16_t maxVal)
    {
      return(val <= maxVal);
    };

    bool rangeCheckRollingStockVehiclesDetectedByECP(const uint16_t val)
    {
      return((val <= maxVehicleCount) && (val >= minVehicleCount));
    };

    bool validateVehicleType(const uint8_t val)
    {
      return(val <= static_cast<uint8_t>(LCSVehicleTypeCar));
    };

    bool validatePenaltyBreakActive(const uint8_t val)
    {
      return(val <= static_cast<uint8_t>(PenaltyBreakActive));
    };

    bool validateLocomotiveSystemFaults(const uint32_t val)
    {
      return(val <= static_cast<uint32_t>(lossOfLeaderComm | lossOfAirBrakeComm));
    };

    bool validateAdsStatus(const uint32_t val)
    {
      return(val <= static_cast<uint32_t>(weightDefined | trainConfigDefined | atpWarningCurveDefined | maDefined | pathDefined));
    };

    bool validateRclStatus(const uint8_t val)
    {
      return(val <= static_cast<uint8_t>(HandlingDoneNotRequest));
    };

    /******************************************************************************
    * End validateXXX functions
    ******************************************************************************/

    /******************************************************************************
    * LCSTrainStatusType::LCSTrainStatusType()
    ******************************************************************************/
    LCSTrainStatusType::LCSTrainStatusType() :
      atoModeCabinSelectorStatus(ATOModeCabinNotAsserted),
      atoDrivingMode(ATODrivingNotAsserted),
      freeRollingStatus(FreeRollingStatusNormalOperation),
      blueFlagStatus(BlueFlagInactive),
      blueFlagRequest(BlueFlagNotRequested),
      adsEtaStatus(AdsEtaNotdefined),
      adsEta(0U),
      lcsAtoStatus(LcsAtoNotReady),
      ecpbSequenceStatus(EcpbSequenceUnknown),
      readyForPrecisionStop(PrecisionStopNoAction),
      reportedEcpbBrakeSystem(BrakeSystemPneumatic),
      ecpbOperatingModes(EcpbOperatingNotAvailable),
      trainIntegrityStatusEcpb(TrainIntegrityNotAsserted),
      percentageOfOperativeBrakesEcpb(0U),
      lastCarBrakePressure(0U),
      versionOfAdsMap(0U),
      penaltyBreakStatus(PenaltyBreakNotActive),
      locomotiveSystemFaults(0U),
      adsStatus(0U),
      timeStamp(0U)
    {
      memset(&gpsPositionLoco[0], 0, sizeof(gpsPositionLoco));
      memset(&gpsPositionLastCar[0], 0, sizeof(gpsPositionLastCar));
    }

    /******************************************************************************
    * LCSTrainStatusType::initCrossCompare()
    ******************************************************************************/
    void LCSTrainStatusType::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ATOModeCabinSelectorType>(&atoModeCabinSelectorStatus));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ATODrivingModeType>(&atoDrivingMode));

      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<FreeRollingStatusType>(&freeRollingStatus));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<BlueFlagStatusType>(&blueFlagStatus));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<BlueFlagRequestType>(&blueFlagRequest));

      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<AdsEtaStatus>(&adsEtaStatus));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&adsEta));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<LcsAtoStatusType>(&lcsAtoStatus));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<EcpbSequenceStatusType>(&ecpbSequenceStatus));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ReadyForPrecisionStopType>(&readyForPrecisionStop));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ReportedEcpbBrakeSystem>(&reportedEcpbBrakeSystem));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<EcpbOperatingModesType>(&ecpbOperatingModes));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<TrainIntegrityStatusEcpbType>(&trainIntegrityStatusEcpb));

      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&percentageOfOperativeBrakesEcpb));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&lastCarBrakePressure));

      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&gpsPositionLoco[0]));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&gpsPositionLastCar[0]));

      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&versionOfAdsMap));

      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<PenaltyBreakActiveType>(&penaltyBreakStatus));

      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&locomotiveSystemFaults));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&adsStatus));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&timeStamp));
    }

    /******************************************************************************
    * ECPBTrainCompositionType::initCrossCompare()
    ******************************************************************************/
    void ECPBTrainCompositionType::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&numberOfDetectedVehiclesByECP));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&numberOfDetectedVehiclesUnknownPos));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&numberOfNotDetectedVehicles));

      // Add all the cars...
      for (uint16_t i = 0U; i < maxVehicleCount; ++i)
      {
        const RollingStockPositionType& positionType = rollingStockPosition[i];
        crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&positionType.roadNumber));
      }
    }
  }
}
