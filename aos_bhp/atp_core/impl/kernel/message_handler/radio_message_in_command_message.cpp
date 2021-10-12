/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
* Each messageType (TCC->AOS) has an associated parser class inherited from AbstractRadioMessageIn.
* This file implements the parser for the CommandMessage message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-26    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <algorithm>

#include "atc_base.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_message_handler.hpp"
#include "radio_message_in_command_message.hpp"

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
    * RadioMessageInCommandMessage constructor
    ******************************************************************************/
    RadioMessageInCommandMessage::RadioMessageInCommandMessage() :
      AbstractRadioMessageIn(MTypeCommandMessage),
      trainNameReceived(false),
      setTimeReceived(false),
      releaseBrakeReceived(false),
      textMessageReceived(false)
    {
      implemented = true;
      memset(&trainName, 0, sizeof(trainName));
      memset(&setTime, 0, sizeof(setTime));
      memset(&textMessage, 0, sizeof(textMessage));
    }

    /******************************************************************************
    * RadioMessageInCommandMessage::getTextMessage
    ******************************************************************************/
    const char_t* RadioMessageInCommandMessage::getTextMessage() const
    {
      const char_t* t = static_cast<char_t *>(NULL);

      // CommandMessage needs to be received and textMessage present to be valid for reading
      if ((DataValidated == dataProcessState)  && textMessageReceived)
      {
        t = textMessage.text;
      }

      return t;
    }

    /******************************************************************************
    * RadioMessageInCommandMessage::validate
    ******************************************************************************/
    bool RadioMessageInCommandMessage::validate()
    {
      trace->write(ATC::briefTrace, "Validating CommandMessage");

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
              ret = true;
            }
          }
        }
      }

      return ret;
    }

    /******************************************************************************
    * RadioMessageInCommandMessage::publishData
    ******************************************************************************/
    bool RadioMessageInCommandMessage::publishData()
    {
      bool publishDataValid = true;

      // New train name received?
      if (trainNameReceived)
      {
        // Set new train name
        if (!DS::AbstractTSetup::corePtr()->setTrainName(&trainName.trainName[0]))
        {
          publishDataValid = false;
        }
      }

      if (publishDataValid)
      {
        if (setTimeReceived)
        {
          /** Call setTimeOfDay application with the Time Extracted. **/
          AbstractMessageHandler::corePtr()->updateSystemTime(static_cast<uint32_t>(setTime.currentTime));
        }
      }
      tracePublishData(publishDataValid);

      return publishDataValid;
    }

    /******************************************************************************
    * RadioMessageInCommandMessage::validateMode
    ******************************************************************************/
    bool RadioMessageInCommandMessage::validateMode() const
    {
      bool modeValid = false;

      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
      case ATPModeYard:
      case ATPModeSleeping:
      case ATPModePossession:
      case ATPModeShunting:
        //Reject the incoming message from TCC 
        invalidIncmgMsgTCC.setDynamicText("Command Message");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
        break;
        // Accept message in these modes
      case ATPModePowerUp:
      case ATPModeConfiguration:
      case ATPModeRegistration:
      case ATPModeUnregistered:
      case ATPModePoweringDown:
      case ATPModeSafetyHalt:
      case ATPModeStaffResponsible:
      case ATPModeShuntingRoute:
      case ATPModeSplit:
      case ATPModeJoin:
      case ATPModeSafeBrakeToStop:
      case ATPModeBaliseSearch:
      case ATPModeNormal:
      case ATPModeLocation:
        modeValid = true;
        break;
      case ATPModeUndefined:
      case ATPModesCount:
      default:
        ATC::aosHalt(__FILE__, __LINE__, "IIlegal ATP Mode");
        break;
      }
      traceValidateMode(modeValid);
      return modeValid;
    }
    /******************************************************************************
    * RadioMessageInCommandMessage::parseMessageData
    ******************************************************************************/
    bool RadioMessageInCommandMessage::parseMessageData()
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
      
      // BlockData
      uint8_t nextMsgIdentifier =  vfwGetU8(&buffer);
      
      // Fetch next data-block until M_END_OF_MESSAGE
      while ((nextMsgIdentifier != M_END_OF_MESSAGE) && parseDataValid)
      {
        switch (nextMsgIdentifier)
        {
          case BTypeTrainName:
          {
            if (!trainNameReceived)
            {
              for (uint8_t i = 0U; i < (trainNameMaxLength / sizeof(trainName.trainName[0U])); i++)
              {
                trainName.trainName[i] = static_cast<char_t>(vfwGetI8(&buffer));
              }
              trainNameReceived = true;
            }
            else
            {
              trace->write(ATC::detailedTrace, "BTypeTrainName buffer overflow");
              parseDataValid = false;
              invalidDataInTCCMessage.setDynamicText("TRAIN_NAME > 1");
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
            }
            break;
          }
          case BTypeSetTime:
          {
            if (!setTimeReceived)
            {
              setTime.currentTime = vfwGetU64(&buffer);
              if (!validateT_CLOCK(setTime.currentTime))
              {
                parseDataValid = false;
                invalidDataInTCCMessage.setDynamicText("T_CLOCK");
                ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
              }

              setTimeReceived = true;
            }
            else
            {
              trace->write(ATC::detailedTrace, "BTypeSetTime buffer overflow");
              parseDataValid = false;
              invalidDataInTCCMessage.setDynamicText("SET_TIME > 1");
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
            }
            break;
          }
          case BTypeReleaseBrake:
          {
            if (!releaseBrakeReceived)
            {
              releaseBrakeReceived = true;
            }
            else
            {
              trace->write(ATC::detailedTrace, "BTypeReleaseBrake buffer overflow");
              parseDataValid = false;
              invalidDataInTCCMessage.setDynamicText("RELEASE_BRAKE > 1");
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
            }
            break;
          }
          case BTypeTextMessage:
          {
            if (!textMessageReceived)
            {
              for (uint8_t i = 0U; i < maxTextLength; i++)
              {
                textMessage.text[i] = static_cast<char_t>(vfwGetI8(&buffer));
              }

              textMessageReceived = true;
            }
            else
            {
              trace->write(ATC::detailedTrace, "BTypeTextMessage buffer overflow");
              parseDataValid = false;
              invalidDataInTCCMessage.setDynamicText("TEXT_MESSAGE > 1");
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
            }
            break;
          }
          default:
          {
            parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
            break;
          }
        }

        // Fetch next msg-type (or M_END_OF_MESSAGE)
        nextMsgIdentifier = vfwGetU8(&buffer);
      }

      if ((!validateSizeOfParsedBytes(&buffer)) && (parseDataValid))
      {
        invalidDataInTCCMessage.setDynamicText("Msg size incorrect");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
        parseDataValid = false;
      }

      traceParseData(parseDataValid);

      return parseDataValid;
    }

 
    /******************************************************************************
    * RadioMessageInCommandMessage::invalidate
    ******************************************************************************/
    void RadioMessageInCommandMessage::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      // Clear all data-block vectors
  
      memset(&trainName, 0, sizeof(trainName));
      trainNameReceived = false;

      memset(&setTime, 0, sizeof(setTime));
      setTimeReceived = false;

      releaseBrakeReceived = false;

      memset(&textMessage, 0, sizeof(textMessage));
      textMessageReceived = false;

      dataProcessState = NoDataAvailable;
    }
  }
}
