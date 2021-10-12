/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (TCC->AOS) has an associated parser class inherited from AbstractRadioMessageIn.
* This file implements the parser for the ExternalData message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-02-28    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_mode_control.hpp"
#include "radio_message_in_external_data.hpp"

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
    * RadioMessageInExternalData constructor
    ******************************************************************************/
    RadioMessageInExternalData::RadioMessageInExternalData() : AbstractRadioMessageIn(MTypeExternalData)
    {
      implemented = true;
      memset(&externalData, 0, sizeof(externalData));
    }


    /******************************************************************************
    * RadioMessageInExternalData::validate
    ******************************************************************************/
    bool RadioMessageInExternalData::validate()
    {
      trace->write(ATC::briefTrace, "Validating ExternalData");

      bool ret;

      ret = AbstractRadioMessageIn::validate();

      if (ret)
      {
        ret = false;

        // Parse, validate and publish data
        if (DataAvailable == dataProcessState)
        {
          if (parseMessageData())
          {
            if (validateMode())
            {
              dataProcessState = DataValidated;
              ret = true;
            }
          }
        }
      }

      return ret;
    }

    /******************************************************************************
    * RadioMessageInExternalData::validateMode
    ******************************************************************************/
    bool RadioMessageInExternalData::validateMode() const
    {
      bool modeValid = false;

      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
      case ATPModeYard:
      case ATPModePossession:
      case ATPModeShunting:
      case ATPModeSleeping:
        // Discard the incoming message from TCC 
        invalidIncmgMsgTCC.setDynamicText("External Data");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
        break;

      case ATPModeSafetyHalt:
      case ATPModePowerUp:
      case ATPModePoweringDown:
        break;

      case ATPModeUnregistered:
      case ATPModeNormal:
      case ATPModeConfiguration:
      case ATPModeRegistration:
      case ATPModeBaliseSearch:
      case ATPModeLocation:
      case ATPModeStaffResponsible:
      case ATPModeSplit:
      case ATPModeJoin:
      case ATPModeShuntingRoute:
      case ATPModeSafeBrakeToStop:
        modeValid = true;
        break;

      case ATPModeUndefined:
      case ATPModesCount:
      default:
        {
          ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
          modeValid = false;
        }
        break;
      }

      traceValidateMode(modeValid);
      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInExternalData::parseMessageData
    ******************************************************************************/
    bool RadioMessageInExternalData::parseMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));
      vfwSetReadBuffer(&buffer, sizeof(messageData.message.data));

      // Read & validate NID_MESSAGE_TYPE
      if (vfwGetU8(&buffer) != static_cast<uint8_t>(messageType))
      {
        trace->write(ATC::detailedTrace, "NID_MESSAGE_TYPE invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("NID_MESSAGE_TYPE");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

      // Read & validate NID_BLOCK_TYPE (EXTERNAL_DATA)
      if (vfwGetU8(&buffer) != BTypeExternalData)
      {
        trace->write(ATC::detailedTrace, "EXTERNAL_DATA invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("EXTERNAL_DATA");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

      // Read NID_SYSTEM
      externalData.nidSystem = vfwGetU8(&buffer);
      // Read N_LENGTH
      const uint16_t appDataSize = vfwGetU16(&buffer);

      if ((appDataSize > 0U) && (appDataSize <= maxSizeAppData))
      {
        for (uint16_t i = 0U; i < appDataSize; i++)
        {
          externalData.appdata[i] = vfwGetU8(&buffer);
        }

        externalData.noOfAppData = appDataSize;

        // Find M_END_Of_MESSAGE or read adaptation blocks
        uint8_t nextMsgIdentifier = vfwGetU8(&buffer);
        while ((nextMsgIdentifier != M_END_OF_MESSAGE) && (parseDataValid))
        {
          parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
          nextMsgIdentifier = vfwGetU8(&buffer);
        }

        if ((!validateSizeOfParsedBytes(&buffer)) && (parseDataValid))
        {
          invalidDataInTCCMessage.setDynamicText("Msg size incorrect");
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
          parseDataValid = false;
        }
      }
      else
      {
        externalData.noOfAppData = 0U;
        parseDataValid = false;
      }

      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInExternalData::invalidate
    ******************************************************************************/
    void RadioMessageInExternalData::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      memset(&externalData, 0, sizeof(externalData));
      dataProcessState = NoDataAvailable;
    }
  }
}
