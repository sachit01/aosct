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
* This file implements the creator for the outgoing vehicle type DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-03-28    skothiya    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_vehicle_types.hpp"
#include "abstract_config.hpp"
#include "abstract_mode_control.hpp"
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
    * DMIMessageOutVehicleTypes constructor
    ******************************************************************************/
    DMIMessageOutVehicleTypes::DMIMessageOutVehicleTypes() : AbstractDMIMessageOut(MTypeVehicleType)
    {
      noOfVehicleTypeBlocks = 0U;
      memset(&vehicleTypes[0], 0, sizeof(vehicleTypes));
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutVehicleTypes::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message :Vehicle Type");

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
    void DMIMessageOutVehicleTypes::invalidate()
    {
      // Clear all data
      noOfVehicleTypeBlocks = 0U;
      memset(&vehicleTypes[0], 0, sizeof(vehicleTypes));
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutVehicleTypes::collectData()
    {
      const bool isAnyValidMode = (Kernel::AbstractModeControl::corePtr()->getCurrentMode() != ATPModePowerUp);
      uint16_t startupStatus;
      const bool dmiStartupStatus = AbstractDMIHandler::corePtr()->getDMIStartupStatus(startupStatus);
      dmiDataProcessState = DMINoDataAvailable;
      if (Kernel::AbstractMessageHandler::corePtr()->getConfigDataReceived() || (isAnyValidMode && dmiStartupStatus))
      {
        //Collect Data from Configuration
        uint8_t vehicleTypeIndex = 0U;
        //Fetch Vehicle Type Name from config
        const char_t* vehicleTypeName = AbstractConfig::corePtr()->getVehicleType1();
        const size_t vehicleTypeNameLength = sizeof(vehicleTypes[vehicleTypeIndex].vehicleTypeName);

        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 2nd Vehicle Type 
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType2();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 3rd Vehicle Type 
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType3();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 4th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType4();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 5th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType5();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 6th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType6();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 7th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType7();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 8th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType8();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 9th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType9();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 10th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType10();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 11th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType11();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 12th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType12();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 13th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType13();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 14th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType14();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 15th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType15();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 16th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType16();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 17th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType17();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 18th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType18();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 19th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType19();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));
        ++vehicleTypeIndex;

        //Fetch 20th Vehicle Type
        vehicleTypeName = AbstractConfig::corePtr()->getVehicleType20();
        static_cast<void>(vfw_strlcpy(&vehicleTypes[vehicleTypeIndex].vehicleTypeName[0], vehicleTypeName, vehicleTypeNameLength));

        noOfVehicleTypeBlocks = maxVehicleTypeBlocks;
        dmiDataProcessState = DMIDataAvailable;
      }
    }

    /******************************************************************************
    *assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutVehicleTypes::assembleDMIMessageData()
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
      vfwPutU8(&buffer, noOfVehicleTypeBlocks);
      uint8_t vehicleType = 1U;
      for (uint16_t i = 0U; i < noOfVehicleTypeBlocks; i++)
      {
        //Vehicle Type
        vfwPutU8(&buffer, vehicleType);
        ++vehicleType;
        // Vehicle Type Name
        for (uint8_t pos = 0U;  pos < vehicleTypeNameMaxLength; pos++)
        {
          vfwPutI8(&buffer, static_cast<int8_t>(vehicleTypes[i].vehicleTypeName[pos]));
        }
      }

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer)) + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      // Write the Trace regarding Parsing of Data
      traceParseData(assembleDataValid);
      return assembleDataValid;
    }
  }
}

