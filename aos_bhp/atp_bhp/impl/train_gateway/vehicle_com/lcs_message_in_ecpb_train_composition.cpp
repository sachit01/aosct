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
* This file implements the parser for the ECPBTrainComposition message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-10    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include <vfw_buffer.h>
#include "lcs_message_in_ecpb_train_composition.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tsetup.hpp"
#include "vehicle_com.hpp"

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
    * LCSMessageInECPBTrainComposition constructor, Note: Update version (2nd parameter) if message is updated
    ******************************************************************************/
    LCSMessageInECPBTrainComposition::LCSMessageInECPBTrainComposition() : AbstractLCSMessageIn(LCSMTypeECPBTrainCompositionMessage, 1U, true)
    {
      ECPBTrainComposition.numberOfDetectedVehiclesByECP = 0U;
      ECPBTrainComposition.numberOfDetectedVehiclesUnknownPos = 0U;
      ECPBTrainComposition.numberOfNotDetectedVehicles = 0U;

      memset(&ECPBTrainComposition.rollingStockPosition[0], 0, sizeof(ECPBTrainComposition.rollingStockPosition));
    }

    /******************************************************************************
    * LCSMessageInECPBTrainComposition::getECPBTrainComposition
    ******************************************************************************/
    bool LCSMessageInECPBTrainComposition::getECPBTrainComposition(ECPBTrainCompositionType & trainComposition) const
    {
      const bool dataIsValidated = (DataValidated == getDataProcessState());
      if (dataIsValidated)
      {
        trainComposition = ECPBTrainComposition;
      }

      return dataIsValidated;
    }


    /******************************************************************************
    * LCSMessageInECPBTrainComposition::validate
    ******************************************************************************/
    bool LCSMessageInECPBTrainComposition::validate(EmpMsg* const mData)
    {
      getTracer().write(ATC::briefTrace, "Validating ECPBTrainComposition");

      // Parse, validate and publish data
      if (parseMessageData(mData))
      {
        if (validateMode())
        {
          if (publishData())
          {
            setDataProcessState(DataValidated);
          }
        }
      }

      return (getDataProcessState() == DataValidated);
    }

    /******************************************************************************
    * LCSMessageInECPBTrainComposition::parseMessageData
    ******************************************************************************/
    bool LCSMessageInECPBTrainComposition::parseMessageData(EmpMsg* const messageData)
    {
      bool parseDataValid = true;

      uint8_t tmpValU8;
      uint16_t tmpValU16;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      const uint32_t messageDataLen = messageData->getEMPMessageMaxBodyLen();
      vfwInitBuffer(&buffer, messageData->getEMPBodyBuffer(), messageDataLen);
      vfwSetReadBuffer(&buffer, messageDataLen);

      // Read & Validate number of vehicles detected by ECPB
      // Total number of detected vehicles can not exceed Max Vehicles for system.
      tmpValU16 = vfwGetU16(&buffer);
      if (!rangeCheckRollingStockVehiclesDetectedByECP(tmpValU16))
      {
        getTracer().write(ATC::detailedTrace, "Number of Rolling Stock Vehicles detected by ECP out of range");
        parseDataValid = false;
      }
      else
      {
        ECPBTrainComposition.numberOfDetectedVehiclesByECP = tmpValU16;
      }

      // Read & Validate number of vehicles with unknown position
      // Total number of vehicles with unknown position can not exceed number of detected vehicles.
      tmpValU16 = vfwGetU16(&buffer);
      if (!validateMaxRollingStockVehicles(tmpValU16, ECPBTrainComposition.numberOfDetectedVehiclesByECP))
      {
        getTracer().write(ATC::detailedTrace, "Number of Rolling Stock Vehicles with unknown position detected by ECP out of range");
        parseDataValid = false;
      }
      else
      {
        ECPBTrainComposition.numberOfDetectedVehiclesUnknownPos = tmpValU16;
      }

      // Read & Validate number of vehicles not detected by ECP system
      // Total number of undetected and detected vehicles can not exceed Max Vehicles for system
      tmpValU16 = vfwGetU16(&buffer);
      if (!validateMaxRollingStockVehicles(tmpValU16, maxVehicleCount - ECPBTrainComposition.numberOfDetectedVehiclesByECP))
      {
        getTracer().write(ATC::detailedTrace, "Number of Rolling Stock Vehicles not detected by ECP out of range");
        parseDataValid = false;
      }
      else
      {
        ECPBTrainComposition.numberOfNotDetectedVehicles = tmpValU16;
      }

      // Read & Validate the vehicles
      for (uint16_t i = 0U; i < ECPBTrainComposition.numberOfDetectedVehiclesByECP; i++)
      {
        // Read & Validate VehicleType
        tmpValU8 = vfwGetU8(&buffer);
        if (!validateVehicleType(tmpValU8))
        {
          getTracer().write(ATC::detailedTrace, "VehicleType invalid");
          parseDataValid = false;
        }
        else
        {
          ECPBTrainComposition.rollingStockPosition[i].vechicleType = static_cast<LCSVehicleType>(tmpValU8);
        }

        // Read RoadNumber
        ECPBTrainComposition.rollingStockPosition[i].roadNumber = vfwGetU16(&buffer);
      }

      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * LCSMessageInECPBTrainComposition::validateMode
    ******************************************************************************/
    bool LCSMessageInECPBTrainComposition::validateMode() const
    {
      bool modeValid = false;

      ATPMode mode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      Kernel::TrainConfigModeState tcModeState = Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();

      // Waiting for TIC configuration
      if ((ATPModeConfiguration == mode) && (Kernel::TrainConfigMode::trainConfigWaitTIC == tcModeState))
      {
        modeValid = true;
      }

      return modeValid;
    }

    /******************************************************************************
    * LCSMessageInECPBTrainComposition::publishData
    ******************************************************************************/
    bool LCSMessageInECPBTrainComposition::publishData() const
    {
      bool publishValid = false;

      DS::TrainSetup  tSetup;

      tSetup.orientation = 0U; // Will be set by DMI
      tSetup.vehicleCount = ECPBTrainComposition.numberOfDetectedVehiclesByECP;

      // Set preliminary Train Setup
      if (DS::AbstractTSetup::corePtr()->setPreliminaryTrainSetup(tSetup))
      {
        for (uint16_t i = 0U; i < tSetup.vehicleCount; ++i)
        {
          DS::VehicleSetup    tVehicleSetup;

          // Fetch setup if already exist
          if (!DS::AbstractTSetup::corePtr()->getPreliminaryVehicleSetup(i, tVehicleSetup))
          {
            getTracer().write(ATC::veryDetailedTrace, "Vehicle Com: New Vehicle-Setup for this index:", static_cast<int32_t>(i));
          }

          tVehicleSetup.vehicleType = lcsToAosVtype(ECPBTrainComposition.rollingStockPosition[i].vechicleType);
          tVehicleSetup.nodeAdress = ECPBTrainComposition.rollingStockPosition[i].roadNumber;

          // Store new car setup
          if ((DS::AbstractTSetup::corePtr()->setPreliminaryVehicleSetup(i, tVehicleSetup)))
          {
            publishValid = true;
          }
          else
          {
            getTracer().write(ATC::briefTrace, "Vehicle Com: Vehicle Setup is not set properly this index:", static_cast<int32_t>(i));
          }
        }
      }

      return publishValid;
    }


    /******************************************************************************
    * LCSMessageInECPBTrainComposition::invalidate
    ******************************************************************************/
    void LCSMessageInECPBTrainComposition::invalidate()
    {
      setDataProcessState(NoDataAvailable);
    }
  }
}
