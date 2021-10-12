/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->DMI) has an associated creator class inherited from AbstractDMIMessageOut.
* This file implements the creator for the outgoing Vehicle Name List DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-03-30    skothiya    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_vehicle_data.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_message_handler.hpp"

#include <vfw_string.h>

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
  namespace DMICom
  {
    /******************************************************************************
    * DMIMessageOutVehicleData constructor
    ******************************************************************************/
    DMIMessageOutVehicleData::DMIMessageOutVehicleData() : AbstractDMIMessageOut(MTypeVehicleData)
    {
      noOfVehicleDatablocks = 0U;
      memset(&vehicleData[0], 0, sizeof(vehicleData));

    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutVehicleData::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message :Vehicle Data");

        if (assembleDMIMessageData())
        {
          dmiDataProcessState = DMIDataValidated;
        }
      }
      return(DMIDataValidated == dmiDataProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageOutVehicleData::invalidate()
    {
      //clear data
      noOfVehicleDatablocks = 0U;
      memset(&vehicleData[0], 0, sizeof(vehicleData));
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutVehicleData::collectData()
    {
      uint16_t startupStatus;
      const bool dmiStartupStatus = AbstractDMIHandler::corePtr()->getDMIStartupStatus(startupStatus);
      const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      const bool isModeConfig = (currentMode == ATPModeConfiguration);
      // Fetch train-setup from preliminary Train- and VehicleSetup if processing a re-registration.
      const Kernel::TrainConfigModeState trainConfigModeState = Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();
      const bool isSubModeTSetupAccepted = (trainConfigModeState == Kernel::TrainConfigMode::trainConfigTSetupAccepted);
      const bool isSubModeSendReReg = (trainConfigModeState == Kernel::TrainConfigMode::trainConfigSendReRegDataToDMI);

      /* Send vehicle data to DMI when TSetup is accepted in below modes:
      - Configuration
      - Normal
      - Shunting Route
      - Staff Responsible
      */

      //local variables needed for function call
      uint8_t  id = 0U;
      uint16_t receivedChannelId = 0U;
      const bool isTsetupReceived = Kernel::AbstractMessageHandler::corePtr()->getTrainSetupReceived(id, receivedChannelId);
      const bool isTsetupRejected = Kernel::AbstractMessageHandler::corePtr()->isTrainSetupRejectedByAOS();
      const bool isTsetupAccepted = isTsetupReceived && (!isTsetupRejected);

      const bool validToSendDataInOtherModes = (((ATPModeNormal == currentMode)
        || (ATPModeShuntingRoute == currentMode)
        || (ATPModeStaffResponsible == currentMode))
        && isTsetupAccepted);

      //In case of re-registration
      if (isSubModeSendReReg && isModeConfig)
      {
        DS::TrainSetup  trainSetup;
        DS::VehicleSetup  vehicleSetup;

        if (DS::AbstractTSetup::corePtr()->getPreliminaryTrainSetup(trainSetup))
        {
          uint16_t vehicleDataIndex = 0U;
          // Get the Vehicle ID Data
          for (uint16_t vehicleIndex = 0U; vehicleIndex < trainSetup.vehicleCount; ++vehicleIndex)
          {
            if (DS::AbstractTSetup::corePtr()->getPreliminaryVehicleSetup(vehicleIndex, vehicleSetup))
            {
              vehicleData[vehicleDataIndex].noOfVehicleInBlock = 1U;
              vehicleData[vehicleDataIndex].vehicleType = vehicleSetup.vehicleType;
              static_cast<void>(vfw_strlcpy(&vehicleData[vehicleDataIndex].vehicleName[0], &vehicleSetup.vehicleName[0],
                sizeof(vehicleData[vehicleDataIndex].vehicleName)));
              vehicleData[vehicleDataIndex].vehicleNodeId = vehicleSetup.nodeAdress;
              ++vehicleDataIndex;
            }
            else
            {
              trace->write(ATC::detailedTrace, "Unable to fetch preliminary Vehicle-Setup");
            }
          }

          noOfVehicleDatablocks = static_cast<uint16_t>(vehicleDataIndex);
          dmiDataProcessState = DMIDataAvailable;
        }
        else
        {
          trace->write(ATC::detailedTrace, "Unable to fetch preliminary train-setup.");
        }
      }
      //In Case of Registration process or in case TSetup accepted in other modes 
      else if ((isSubModeTSetupAccepted && isModeConfig) || dmiStartupStatus || validToSendDataInOtherModes)
      {
        DS::VehicleSetup  vehicleSetup;
        const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

        if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
        {
          uint16_t vehicleDataIndex = 0U;
          // Get the Vehicle ID Data
          for (uint16_t vehicleIndex = 0U; vehicleIndex < pTrainSetup->vehicleCount; ++vehicleIndex)
          {
            if (DS::AbstractTSetup::corePtr()->getVehicleSetup(vehicleIndex, vehicleSetup))
            {
              vehicleData[vehicleDataIndex].noOfVehicleInBlock = 1U;
              vehicleData[vehicleDataIndex].vehicleType = vehicleSetup.vehicleType;
              static_cast<void>(vfw_strlcpy(&vehicleData[vehicleDataIndex].vehicleName[0], &vehicleSetup.vehicleName[0],
                sizeof(vehicleData[vehicleDataIndex].vehicleName)));
              vehicleData[vehicleDataIndex].vehicleNodeId = vehicleSetup.nodeAdress;
              ++vehicleDataIndex;
            }
            else
            {
              trace->write(ATC::detailedTrace, "Unable to fetch Vehicle-Setup");
            }
          }

          noOfVehicleDatablocks = static_cast<uint16_t>(vehicleDataIndex);
          dmiDataProcessState = DMIDataAvailable;
        }
        else
        {
          trace->write(ATC::detailedTrace, "Unable to fetch train-setup.");
        }
      }
      else
      {
        // Do nothing
      }
    }

    /******************************************************************************
    *assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutVehicleData::assembleDMIMessageData()
    {
      bool assembleDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));

      // Header Type
      messageData.headerType = dmiHeaderTypeAckMsg;
      // Message Number
      messageData.msgNumber = AbstractDMIHandler::corePtr()->getNextMessageNumber();

      // Get MSB for the acknowledged DMIMessageType 
      messageData.dmiData.msgType = static_cast<uint8_t>(static_cast<uint8_t>(messageType) | 0x80U);

      // Assemble data and write in network order
      vfwPutU16(&buffer, noOfVehicleDatablocks);

      for (uint16_t i = 0U; i < noOfVehicleDatablocks; i++)
      {
        vfwPutU16(&buffer, vehicleData[i].noOfVehicleInBlock);
        vfwPutU8(&buffer, vehicleData[i].vehicleType);
        vfwPutU16(&buffer, vehicleData[i].vehicleNodeId);

        // Vehicle Name
        for (uint8_t pos = 0U; pos < vehicleNameMaxLength; pos++)
        {
          vfwPutI8(&buffer, static_cast<int8_t>(vehicleData[i].vehicleName[pos]));
        }
      }

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      // Write the Trace regarding assembling of Data
      traceParseData(assembleDataValid);

      return assembleDataValid;
    }
  }
}
