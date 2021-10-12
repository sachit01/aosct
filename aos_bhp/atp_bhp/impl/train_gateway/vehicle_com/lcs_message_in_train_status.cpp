/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (LCS->AOS) has an associated parser class inherited from AbstractLCSMessageIn.
* This file implements the parser for the TrainStatus message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-24    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "lcs_message_in_train_status.hpp"
#include "lcs_message_common.hpp"
#include "abstract_config_base.hpp"

#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#endif

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
    /******************************************************************************
    * LCSMessageInTrainStatus constructor, Note: Update version (2nd parameter) if message is updated
    ******************************************************************************/
    LCSMessageInTrainStatus::LCSMessageInTrainStatus() : AbstractLCSMessageIn(LCSMTypeTrainStatusMessage, 1U, true)
    {
    }

    /******************************************************************************
    * LCSMessageInTrainStatus::getTrainStatus
    ******************************************************************************/
    bool LCSMessageInTrainStatus::getTrainStatus(LCSTrainStatusType & status) const
    {
      const bool dataIsValidated = (DataValidated == getDataProcessState());
      if (dataIsValidated)
      {
        status = trainStatus;
      }

      return dataIsValidated;
    }

    /******************************************************************************
    * LCSMessageInTrainStatus::validate
    ******************************************************************************/
    bool LCSMessageInTrainStatus::validate(EmpMsg* const mData)
    {
      getTracer().write(ATC::briefTrace, "Validating TrainStatus");

      // Parse, validate and publish data
      if (parseMessageData(mData))
      {
        setDataProcessState(DataValidated);
      }

      return (DataValidated == getDataProcessState());
    }

    /******************************************************************************
    * LCSMessageInTrainStatus::parseMessageData
    ******************************************************************************/
    bool LCSMessageInTrainStatus::parseMessageData(EmpMsg* const messageData)
    {
      bool parseDataValid = true;
      uint8_t tmpValU8;
      uint32_t tmpValU32;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      const uint32_t messageDataLen = messageData->getEMPMessageMaxBodyLen();
      vfwInitBuffer(&buffer, messageData->getEMPBodyBuffer(), messageDataLen);
      vfwSetReadBuffer(&buffer, messageDataLen);

      // Read & Validate ATO Mode Cabin Selector Status
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateATOModeCabinSelectorStatus(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "ATOModeCabinSelectorStatus invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.atoModeCabinSelectorStatus = static_cast<ATOModeCabinSelectorType>(tmpValU8);
      }

      // Read & Validate ATODrivingMode
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateATODrivingMode(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "ATODrivingMode invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.atoDrivingMode = static_cast<ATODrivingModeType>(tmpValU8);
      }

      // Read & Validate TractionMode
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateTractionMode(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "TractionMode invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.freeRollingStatus = static_cast<FreeRollingStatusType>(tmpValU8);
      }

      // Read & Validate BlueFlagStatus
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateBlueFlagStatus(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "BlueFlagStatus invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.blueFlagStatus = static_cast<BlueFlagStatusType>(tmpValU8);
      }

      // Read & Validate BlueFlagRequest
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateBlueFlagRequest(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "BlueFlagRequest invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.blueFlagRequest = static_cast<BlueFlagRequestType>(tmpValU8);
      }

      // Read & Validate AdsEtaStatus
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateAdsEtaStatus(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "AdsEtaStatus invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.adsEtaStatus = static_cast<AdsEtaStatus>(tmpValU8);
      }

      // Read ADS ETA
      trainStatus.adsEta = vfwGetU32(&buffer);

      // Read & Validate LcsAtoReady
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateLcsAtoReady(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "LcsAtoStatus invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.lcsAtoStatus = static_cast<LcsAtoStatusType>(tmpValU8);
      }

      // Read & Validate EcpbSequenceStatus
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateEcpbSequenceStatus(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "EcpbSequenceStatus invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.ecpbSequenceStatus = static_cast<EcpbSequenceStatusType>(tmpValU8);
      }

      // Read & Validate ReadyForPrecisionStop
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateReadyForPrecisionStop(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "ReadyForPrecisionStop invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.readyForPrecisionStop = static_cast<ReadyForPrecisionStopType>(tmpValU8);
      }

      // Read & Validate BrakeSystemInUse
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateBrakeSystemInUse(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "BrakeSystemInUse invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.reportedEcpbBrakeSystem = static_cast<ReportedEcpbBrakeSystem>(tmpValU8);
      }

      // Read & Validate EcpbOperatingModes
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateEcpbOperatingModes(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "EcpbOperatingModes invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.ecpbOperatingModes = static_cast<EcpbOperatingModesType>(tmpValU8);
      }

      // Read & Validate TrainIntegrityStatusEcpb
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateTrainIntegrityStatusEcpb(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "TrainIntegrityStatusEcpb invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.trainIntegrityStatusEcpb = static_cast<TrainIntegrityStatusEcpbType>(tmpValU8);
      }

      // Read Percentage of operative brakes
      trainStatus.percentageOfOperativeBrakesEcpb = vfwGetU8(&buffer);

      // Read Last car brake pressure
      trainStatus.lastCarBrakePressure = vfwGetU8(&buffer);

      // Read GPS Position for Loco
      memset(&trainStatus.gpsPositionLoco[0], 0, sizeof(trainStatus.gpsPositionLoco)); // Reset the data to 0
      // Copy the data from VFW_buffer 'buffer' to  13 byte 'gpsPositionLoco'
      vfwCpyToRawBuffer(&(trainStatus.gpsPositionLoco[0]), &buffer, sizeof(trainStatus.gpsPositionLoco));

      // Read GPS Position for Last Car
      memset(&trainStatus.gpsPositionLastCar[0], 0, sizeof(trainStatus.gpsPositionLastCar)); // Reset the data to 0
      // Copy the data from VFW_buffer 'buffer' to  13 byte 'gpsPositionLoco'
      vfwCpyToRawBuffer(&(trainStatus.gpsPositionLastCar[0]), &buffer, sizeof(trainStatus.gpsPositionLastCar));

      // Read Version of ADS Map
      trainStatus.versionOfAdsMap = vfwGetU16(&buffer);


      // Read & Validate Penalty Break Active
      tmpValU8 = vfwGetU8(&buffer);

      if (!validatePenaltyBreakActive(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "PenaltyBreakActive invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.penaltyBreakStatus = static_cast<PenaltyBreakActiveType>(tmpValU8);
      }

      // Read locomotiveSystemFaults
      tmpValU32 = vfwGetU32(&buffer);

      if (!validateLocomotiveSystemFaults(tmpValU32))
      {
        getTracer().write(ATC::detailedTrace, "Locomotive System Faults invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.locomotiveSystemFaults = tmpValU32;
      }

      // Read ADS Status
      tmpValU32 = vfwGetU32(&buffer);

      if (!validateAdsStatus(tmpValU32))
      {
        getTracer().write(ATC::detailedTrace, "ADS Status invalid");
        parseDataValid = false;
      }
      else
      {
        trainStatus.adsStatus = tmpValU32;
      }

      trainStatus.timeStamp = messageData->getEMPMsgHeader().msgTime;

      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * LCSMessageInTrainStatus::invalidate
    ******************************************************************************/
    void LCSMessageInTrainStatus::invalidate()
    {
      setDataProcessState(NoDataAvailable);
    }

    /******************************************************************************
    * LCSMessageInTrainStatus::logToRU
    ******************************************************************************/
    void LCSMessageInTrainStatus::logToRU(const EmpMsg* const mData) const
    {
      static int64_t lastLogTime = 0L; // seconds
      static LCSTrainStatusType lastTrainStatus;

      const int64_t timeNow = vfwGetReferenceTime() / 1000; // millis to seconds
      const int64_t logPeriod = static_cast<int64_t>(ATC::AbstractConfigBase::basePtr()->getRuLogDynValuePeriod()); // seconds

      if (((timeNow - lastLogTime) >= logPeriod) ||
        (trainStatus.atoModeCabinSelectorStatus != lastTrainStatus.atoModeCabinSelectorStatus) ||
        (trainStatus.atoDrivingMode != lastTrainStatus.atoDrivingMode) ||
        (trainStatus.freeRollingStatus != lastTrainStatus.freeRollingStatus) ||
        (trainStatus.blueFlagStatus != lastTrainStatus.blueFlagStatus) ||
        (trainStatus.blueFlagRequest != lastTrainStatus.blueFlagRequest) ||
        (trainStatus.lcsAtoStatus != lastTrainStatus.lcsAtoStatus) ||
        (trainStatus.ecpbSequenceStatus != lastTrainStatus.ecpbSequenceStatus) ||
        (trainStatus.readyForPrecisionStop != lastTrainStatus.readyForPrecisionStop) ||
        (trainStatus.reportedEcpbBrakeSystem != lastTrainStatus.reportedEcpbBrakeSystem) ||
        (trainStatus.ecpbOperatingModes != lastTrainStatus.ecpbOperatingModes) ||
        (trainStatus.trainIntegrityStatusEcpb != lastTrainStatus.trainIntegrityStatusEcpb) ||
        (trainStatus.versionOfAdsMap != lastTrainStatus.versionOfAdsMap) ||
        (trainStatus.penaltyBreakStatus != lastTrainStatus.penaltyBreakStatus) ||
        (trainStatus.locomotiveSystemFaults != lastTrainStatus.locomotiveSystemFaults) ||
        (trainStatus.adsStatus != lastTrainStatus.adsStatus))
      {
        lastLogTime = timeNow;
        lastTrainStatus = trainStatus;

        AbstractLCSMessageIn::logToRU(mData);
      }
    }
  }
}

