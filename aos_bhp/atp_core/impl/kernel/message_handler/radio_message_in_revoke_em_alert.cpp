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
* This file implements the parser for the RevokeEmAlert message.
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
#include "radio_message_in_revoke_em_alert.hpp"
#include "abstract_mode_control.hpp"

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
    * RadioMessageInRevokeEmAlert constructor
    ******************************************************************************/
    RadioMessageInRevokeEmAlert::RadioMessageInRevokeEmAlert() : AbstractRadioMessageIn(MTypeRevokeEmergencyAlert)
    {
      implemented = true;
    }

    /******************************************************************************
    * RadioMessageInRevokeEmAlert getRevokeEmAlert
    ******************************************************************************/
    bool RadioMessageInRevokeEmAlert::getRevokeEmAlert() const
    {
      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageInRevokeEmAlert::validate
    ******************************************************************************/
    bool RadioMessageInRevokeEmAlert::validate()
    {
      trace->write(ATC::briefTrace, "Validating RevokeEmAlert");

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
    * RadioMessageInRevokeEmAlert::validateMode
    ******************************************************************************/
    bool RadioMessageInRevokeEmAlert::validateMode() const
    {
      bool modeValid = true;
        
      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
        case ATPModePoweringDown:
        case ATPModeSafetyHalt:
        case ATPModePowerUp:
          modeValid = false;
          break;
        case ATPModeYard:
        case ATPModeSafeBrakeToStop:
        case ATPModeShuntingRoute:
        case ATPModePossession:
        case ATPModeShunting:
        case ATPModeBaliseSearch:
        case ATPModeConfiguration:
        case ATPModeRegistration:
        case ATPModeNormal:
        case ATPModeLocation:
        case ATPModeStaffResponsible:
        case ATPModeSplit:
        case ATPModeJoin:
        case ATPModeUnregistered:
          break;
        case ATPModeSleeping:
          //Reject the incoming message from TCC 
          modeValid = false;
          invalidIncmgMsgTCC.setDynamicText("RevokeEmerAlert");
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
          break;
        case ATPModeUndefined:
        case ATPModesCount:
        default:
          ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
          break;
      }
      traceValidateMode(modeValid);
      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInRevokeEmAlert::parseMessageData
    ******************************************************************************/
    bool RadioMessageInRevokeEmAlert::parseMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));
      vfwSetReadBuffer(&buffer, sizeof(messageData.message.data));

      // Validate NID_MESSAGE_TYPE
      if (vfwGetU8(&buffer) != static_cast<uint8_t>(messageType))
      {
        trace->write(ATC::detailedTrace, "NID_MESSAGE_TYPE invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("NID_MESSAGE_TYPE");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

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

      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInRevokeEmAlert::invalidate
    ******************************************************************************/
    void RadioMessageInRevokeEmAlert::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      dataProcessState = NoDataAvailable;
    }

  }
}
