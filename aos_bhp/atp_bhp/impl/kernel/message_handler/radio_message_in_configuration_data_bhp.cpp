/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2019
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (TCC->AOS) has an associated parser class inherited from AbstractRadioMessageIn.
* This file implements the parser for the ConfigurationData message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2019-02-27    csundin     Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "config.hpp"
#include "radio_message_in_configuration_data_bhp.hpp"

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
  namespace Kernel
  {
    /******************************************************************************
    * RadioMessageInConfigurationDataBHP constructor
    ******************************************************************************/
    RadioMessageInConfigurationDataBHP::RadioMessageInConfigurationDataBHP()
      : RadioMessageInConfigurationData()
    {
    }

    /******************************************************************************
    * RadioMessageInConfigurationDataBHP::validateData
    ******************************************************************************/
    bool RadioMessageInConfigurationDataBHP::validateData()
    {
      bool success = RadioMessageInConfigurationData::validateData();

      if (success)
      {
        for (size_t i = 0U; i < configData.configDataVec.size(); ++i)
        {
          const ConfigDataStruct& cfgParameter1 = configData.configDataVec[i];

          if ( (cfgParameter1.configurationId == typicalConfigCarType1Id)
            || (cfgParameter1.configurationId == typicalConfigCarType2Id)
            || (cfgParameter1.configurationId == typicalConfigCarType3Id)
            || (cfgParameter1.configurationId == typicalConfigCarType4Id)
            || (cfgParameter1.configurationId == typicalConfigCarType5Id) )
          {
            bool found = false;

            for (size_t j = 0U; j < configData.configDataVec.size(); ++j)
            {
              const ConfigDataStruct& cfgParameter2 = configData.configDataVec[j];

              if ( (cfgParameter2.configurationId >= vehicleType1Id)
                && (cfgParameter2.configurationId <= vehicleType20Id) )
              {
                if (strncmp(&cfgParameter1.textStringValue[0], &cfgParameter2.textStringValue[0],
                  sizeof(cfgParameter2.textStringValue)) == 0)
                {
                  found = true;
                  break;
                }
              }
            }

            if (!found)
            {
              success = false;
              trace->write(ATC::detailedTrace, "Unknown car type in Typical Config");
            }
          }
        }
      }

      return success;
    }
  }
}
