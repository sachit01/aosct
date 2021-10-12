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
* This file implements the parser for the JoinCommand message.
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
#include "radio_message_in_join_command.hpp"

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
    * RadioMessageInJoinCommand constructor
    ******************************************************************************/
    RadioMessageInJoinCommand::RadioMessageInJoinCommand() : AbstractRadioMessageIn(MTypeJoinCommand)
    {
      implemented = true; 
    }
    
    /******************************************************************************
    * RadioMessageInJoinCommand::validate
    ******************************************************************************/
    bool RadioMessageInJoinCommand::validate()
    {
      trace->write(ATC::briefTrace, "Validating JoinCommand");

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
    * RadioMessageInJoinCommand::validateMode
    ******************************************************************************/
    bool RadioMessageInJoinCommand::validateMode() const
    {
      bool modeValid = false;

      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
          case ATPModeNormal:
          modeValid = true;
          break;
          case ATPModeYard:
          case ATPModePossession:
          case ATPModeShunting:
          case ATPModeSleeping:
            //Reject the incoming message from TCC
            invalidIncmgMsgTCC.setDynamicText("Join Command");
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
            break;
          case ATPModePowerUp:
          case ATPModeConfiguration:
          case ATPModeRegistration:
          case ATPModeBaliseSearch:
          case ATPModeLocation:
          case ATPModeStaffResponsible:
          case ATPModeSplit:
          case ATPModeJoin:
          case ATPModeShuntingRoute:
          case ATPModePoweringDown:
          case ATPModeUnregistered:
          case ATPModeSafetyHalt:
          case ATPModeSafeBrakeToStop:
            break;
          case ATPModeUndefined:
          case ATPModesCount:
          default:
          {
            ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
            break;
          }
      }
      traceValidateMode(modeValid);
      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInJoinCommand::parseMessageData
    ******************************************************************************/
    bool RadioMessageInJoinCommand::parseMessageData()
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

      //TODO: The Field for join Command message is not yet defined in Interface Spec
      //Need to update as soon as fields are defined in Interface spec. 

      // Fetch next data-block until M_END_OF_MESSAGE
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
    * RadioMessageInJoinCommand::invalidate
    ******************************************************************************/
    void RadioMessageInJoinCommand::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      dataProcessState = NoDataAvailable;
    }

    /******************************************************************************
    * RadioMessageInJoinCommand::getJoinCommand
    ******************************************************************************/
    bool RadioMessageInJoinCommand::getJoinCommand() const
    {
      return (DataValidated == dataProcessState);
    }

  }
}
