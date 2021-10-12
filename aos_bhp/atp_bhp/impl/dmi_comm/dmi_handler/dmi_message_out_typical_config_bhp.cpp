/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->DMI) has an associated creator class inherited from AbstractDMIMessageOut.
* This file implements the creator for the outgoing typical config DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-12-14    csundin     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "dmi_message_out_typical_config_bhp.hpp"
#include "abstract_dmi_handler.hpp"

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
    * DMIMessageOutTypicalConfigBHP constructor
    ******************************************************************************/
    DMIMessageOutTypicalConfigBHP::DMIMessageOutTypicalConfigBHP() :AbstractDMIMessageOut(MTypeTypicalConfig)
    {
      noOfConfigs = 0U;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutTypicalConfigBHP::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
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
    void DMIMessageOutTypicalConfigBHP::invalidate()
    {
      //clear all data
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutTypicalConfigBHP::collectData()
    {
      uint8_t newNoOfConfigs = 0U;
      BHPTypicalConfig config;
      const BHPTypicalConfig emptyConfig;
      bool dataChanged = false;

      for (uint8_t i = 0U; i < maxNoOfConfigs; ++i)
      {
        Config::instance().getTypicalConfig(i, config);

        if (config != emptyConfig)
        {
          if (config != configs[newNoOfConfigs])
          {
            configs[newNoOfConfigs] = config;
            dataChanged = true;
          }

          ++newNoOfConfigs;
        }
      }

      if (newNoOfConfigs != noOfConfigs)
      {
        noOfConfigs = newNoOfConfigs;
        dataChanged = true;
      }

      if (dataChanged)
      {
        dmiDataProcessState = DMIDataAvailable;
      }
    }

    /******************************************************************************
    * DMIMessageOutTypicalConfigBHP::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutTypicalConfigBHP::assembleDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));

      messageData.headerType = dmiHeaderTypeAckMsg;
      messageData.msgNumber = AbstractDMIHandler::corePtr()->getNextMessageNumber();
      messageData.dmiData.msgType = static_cast<uint8_t>(static_cast<uint8_t>(messageType) | 0x80U);

      //Assemble the data in network order
      vfwPutU8(&buffer, noOfConfigs);

      for (uint8_t i = 0U; i < noOfConfigs; ++i)
      {
        char_t configName[bhpConfigNameLength];
        char_t vehTypeName[bhpConfigNameLength];
        memset(&configName[0], 0, sizeof(configName));
        memset(&vehTypeName[0], 0, sizeof(vehTypeName));

        strncpy(&configName[0], &configs[i].configName[0], sizeof(configName));
        strncpy(&vehTypeName[0], &configs[i].vehTypeName[0], sizeof(vehTypeName));
        const uint8_t noOfCars = configs[i].noOfCars;

        for (uint8_t j = 0U; j < sizeof(configName); ++j)
        {
          vfwPutU8(&buffer, static_cast<uint8_t>(configName[j]));
        }
        for (uint8_t j = 0U; j < sizeof(vehTypeName); ++j)
        {
          vfwPutU8(&buffer, static_cast<uint8_t>(vehTypeName[j]));
        }
        vfwPutU8(&buffer, noOfCars);
      }

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      traceParseData(parseDataValid);

      return parseDataValid;
    }
  }
}
