/****************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the parser for the Path message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-03-21    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "radio_message_types_bhp.hpp"
#include "radio_message_in_path_bhp.hpp"
#include "mode_control.hpp"
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
    * constructor
    ******************************************************************************/
    RadioMessageInPathBHP::RadioMessageInPathBHP() : RadioMessageInPath()
    {
      bhpConfigVersion.noOfBytesApplicationData = 0U;
      bhpConfigVersion.configurationMajorVersion = 0U;
      bhpConfigVersion.configurationMinorVersion = 0U;
    }


    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void RadioMessageInPathBHP::invalidate()
    {
      bhpConfigVersion.noOfBytesApplicationData = 0U;
      bhpConfigVersion.configurationMajorVersion = 0U;
      bhpConfigVersion.configurationMinorVersion = 0U;
      RadioMessageInPath::invalidate();
    }

    /******************************************************************************
    * parseAdditionalBlocks
    ******************************************************************************/
    bool RadioMessageInPathBHP::parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType)
    {
      bool retvalue = false;

      switch (adapBlockType)
      {
      case BTypeBHPBConfigVersion:
      {
        bhpConfigVersion.noOfBytesApplicationData = vfwGetU16(buffer);
        bhpConfigVersion.configurationMajorVersion = vfwGetU8(buffer);
        bhpConfigVersion.configurationMinorVersion = vfwGetU8(buffer);
        //Check for N_LENGTH
        if (bhpbConfigVersionInPathBlockSize == bhpConfigVersion.noOfBytesApplicationData)
        {
         // Set the static BHPB Configuration version
         Kernel::ModeControl::instance().setStaticConfigurationVersion(bhpConfigVersion.configurationMajorVersion,
            bhpConfigVersion.configurationMinorVersion);
         retvalue = true;
        }
        else
        {
          invalidDataInTCCMessage.setDynamicText("BHPB_CONFIG_VERSION");
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
        }
        break;
      }
      default:
      {
        trace->write(ATC::detailedTrace, "Adaptation Block not defined");
        writeToLog(ATC::DetailedLog, "Adaptation Block not defined", __FILE__, __LINE__);
        invalidBlockTypeInTCCMessage.setDynamicText(static_cast<uint32_t>(adapBlockType));
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidBlockTypeInTCCMessage, __FILE__, __LINE__);
        break;
      }
      }

      return retvalue;
    }

    /******************************************************************************
    * getStaticConfigurationVersionInPath
    ******************************************************************************/
    bool RadioMessageInPathBHP::getStaticConfigurationVersionInPath(uint8_t & configMajorVersion, uint8_t & configMinorVersion) const
    {
      if (DataValidated == dataProcessState)
      {
        configMajorVersion = bhpConfigVersion.configurationMajorVersion;
        configMinorVersion = bhpConfigVersion.configurationMinorVersion;
      }
      else
      {
        configMajorVersion = 0U;
        configMinorVersion = 0U;
      }

      return (DataValidated == dataProcessState);
    }
   
  }
}
