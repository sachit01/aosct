/****************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the parser for the MovementAuthority message.
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
#include "radio_message_in_command_message_bhp.hpp"
#include "tsetup.hpp"
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
  namespace Kernel
  {

    /******************************************************************************
    * constructor
    ******************************************************************************/
    RadioMessageInCommandMessageBHP::RadioMessageInCommandMessageBHP() : RadioMessageInCommandMessage()
    {
      // Setup Fixed Sizes for msg-block vectors.
      bhpCommandMessage.bhpSafeForBoardingReceived = false;
      bhpCommandMessage.bhpRadioChannelReceived = false;
      memset(&bhpCommandMessage.bhpRadioChannel.radioChannel[0], 0, sizeof(bhpCommandMessage.bhpRadioChannel.radioChannel));
      isRadioChannelEnable = false;
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void RadioMessageInCommandMessageBHP::invalidate()
    {
      RadioMessageInCommandMessage::invalidate();
      bhpCommandMessage.bhpSafeForBoardingReceived = false;
      bhpCommandMessage.bhpRadioChannelReceived = false;
      memset(&bhpCommandMessage.bhpRadioChannel.radioChannel[0], 0, sizeof(bhpCommandMessage.bhpRadioChannel.radioChannel));
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool RadioMessageInCommandMessageBHP::validate()
    {
      bool retValue = true;

      if (RadioMessageInCommandMessage::validate())
      {
        if (RadioMessageInCommandMessage::publishData())
        {
          dataProcessState = DataValidated;
        }
        else
        {
          retValue = false;
        }
      }
      else
      {
        retValue = false;
      }

      return retValue;
    }

    /******************************************************************************
    * parseAdditionalBlocks
    ******************************************************************************/
    bool RadioMessageInCommandMessageBHP::parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType)
    {
      bool parseDataValid = true;

      switch (adapBlockType)
      {
        case BTypeBHPBRadioChannel:
        {
          BHPBRadioChannel radioChannel;
          radioChannel.noOfBytesApplicationData = vfwGetU16(buffer);

          if (bhpbRadioChannelBlockSize == radioChannel.noOfBytesApplicationData)
          {
            if (!bhpCommandMessage.bhpRadioChannelReceived)
            {
              bhpCommandMessage.bhpRadioChannelReceived = true;
              //Copy the Radio channel Value
              for (uint8_t i = 0U; i < radioChannelNameLength; i++)
              {
                radioChannel.radioChannel[i] = static_cast<char_t>(vfwGetI8(buffer));
              }
              radioChannel.radioChannel[radioChannelNameLength] = '\0';

              bhpCommandMessage.bhpRadioChannel = radioChannel;
            }
            else
            {
              parseDataValid = false;
              setInvalidationReason("bhpRadioChannel overflow");
              invalidDataInTCCMessage.setDynamicText("BHPB_RADIO_CHANNEL");
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
            }
          }
          else
          {
            setInvalidationReason("bhpRadioChannel Invalid application length");
            parseDataValid = false;
            invalidDataInTCCMessage.setDynamicText("BHPB_RADIO_CHANNEL");
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
          }
          break;
        }
        case BTypeBHPBSafeForBoarding:
        {
          BHPBSafeForBoarding bhpbSafeForBoarding;

          bhpbSafeForBoarding.noOfBytesApplicationData = vfwGetU16(buffer);

          // Check for N_LENGTH
          if (bhpbSafeForBoardingBlockSize == bhpbSafeForBoarding.noOfBytesApplicationData)
          {
            // Max number of BHPB_SAFE_FOR_BOARDING blocks
            if (!bhpCommandMessage.bhpSafeForBoardingReceived)
            {
              bhpCommandMessage.bhpSafeForBoardingReceived = true;
              bhpCommandMessage.bhpSafeForBoarding = bhpbSafeForBoarding;
            }
            else
            {
              trace->write(ATC::detailedTrace, "bhpSafeForBoardingVec buffer overflow");
              parseDataValid = false;
              invalidDataInTCCMessage.setDynamicText("BHPB_SAFE_FOR_B...");
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
            }
          }
          else
          {
            setInvalidationReason("bhpSafeForBoardingVec Invalid application length");
            parseDataValid = false;
            invalidDataInTCCMessage.setDynamicText("BHPB_SAFE_FOR_B...");
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
          }
          break;
        }
        default:
        {
          trace->write(ATC::detailedTrace, "Adaptation Block not defined");
          writeToLog(ATC::DetailedLog, "Adaptation Block not defined", __FILE__, __LINE__);
          parseDataValid = false;
          invalidBlockTypeInTCCMessage.setDynamicText(static_cast<uint32_t>(adapBlockType));
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidBlockTypeInTCCMessage, __FILE__, __LINE__);
          break;
        }
      }

      return parseDataValid;
    }

    /******************************************************************************
    * getSafeForBoarding
    ******************************************************************************/
    bool RadioMessageInCommandMessageBHP::getSafeForBoarding(void) const
    {
      return (DataValidated == dataProcessState) && bhpCommandMessage.bhpSafeForBoardingReceived;
    }

    /******************************************************************************
    * getRadioChannelName
    ******************************************************************************/
    bool RadioMessageInCommandMessageBHP::getRadioChannelName(char_t * const radioChannel)
    {
      if (bhpCommandMessage.bhpRadioChannelReceived)
      {
        isRadioChannelEnable = true;
        static_cast<void>(vfw_strlcpy(radioChannel, &bhpCommandMessage.bhpRadioChannel.radioChannel[0], (Kernel::radioChannelNameLength + 1U)));
      }

      return bhpCommandMessage.bhpRadioChannelReceived;
    }

    /******************************************************************************
    * getRadioChannelEnable
    ******************************************************************************/
    bool RadioMessageInCommandMessageBHP::getRadioChannelEnable() const
    {
      return isRadioChannelEnable;
    }

  }
}
