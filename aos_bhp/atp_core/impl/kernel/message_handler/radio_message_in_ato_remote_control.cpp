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
* This file implements the parser for the ATORemoteControl message.
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
#include "radio_message_in_ato_remote_control.hpp"

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
    * RadioMessageInATORemoteControl constructor
    ******************************************************************************/
    RadioMessageInATORemoteControl::RadioMessageInATORemoteControl() : AbstractRadioMessageIn(MTypeATORemoteControl)
    {
      implemented = true;
      atoRemoteControlData.requestedSpeed = 0U;
      atoRemoteControlData.timeLimit = 0U;
      atoRemoteControlData.trainDirection = 0U;
      atoRemoteControlData.loadFinishedVec.reserve(loadFinishedDataSize);
    }

    /******************************************************************************
    * RadioMessageInATORemoteControl::validate
    ******************************************************************************/
    bool RadioMessageInATORemoteControl::validate()
    {
      trace->write(ATC::briefTrace, "Validating ATORemoteControl");

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
              if (publishData())
              {
                dataProcessState = DataValidated;
                ret = true;
              }
            }
          }
        }
      }

      return ret;
    }

    /******************************************************************************
    * RadioMessageInATORemoteControl::validateMode
    ******************************************************************************/
    bool RadioMessageInATORemoteControl::validateMode() const
    {
      bool modeValid = false;

      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
        case ATPModeYard:
        case ATPModeShunting:
        case ATPModePossession:
        case ATPModeSleeping:
          //Reject the incoming message from TCC 
          invalidIncmgMsgTCC.setDynamicText("ATO Remote Control");
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
          break;

        case ATPModeNormal:
        case ATPModePowerUp:
        case ATPModeConfiguration:
        case ATPModeRegistration:
        case ATPModeBaliseSearch:
        case ATPModeStaffResponsible:
        case ATPModeSplit:
        case ATPModeJoin:
        case ATPModeShuntingRoute:
        case ATPModePoweringDown:
        case ATPModeUnregistered:
        case ATPModeSafetyHalt:
        case ATPModeSafeBrakeToStop:
          break;

        case ATPModeLocation:
          modeValid = true;
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
    * RadioMessageInATORemoteControl::publishData
    ******************************************************************************/
    bool RadioMessageInATORemoteControl::publishData() const
    {
      bool publishDataValid = true;

      //TODO:
      // Set the Requested speed (0 for stop)
      // Set the Time limit for speed order
      // Set the train Direction (B_direction)
      // Set the Load_finished


      tracePublishData(publishDataValid);

      return publishDataValid;
    }

    /******************************************************************************
    * RadioMessageInATORemoteControl::parseMessageData
    ******************************************************************************/
    bool RadioMessageInATORemoteControl::parseMessageData()
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

      // Read V_SPEED
      atoRemoteControlData.requestedSpeed = vfwGetU16(&buffer);

      // Read T_REMOTE
      atoRemoteControlData.timeLimit = vfwGetU8(&buffer);

      // Read B_DIRECTION
      atoRemoteControlData.trainDirection = vfwGetU8(&buffer);
      if (atoRemoteControlData.trainDirection != 0U)
      {
        trace->write(ATC::detailedTrace, "B_DIRECTION invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("B_DIRECTION != 0");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

      // BlockData
      uint8_t nextMsgIdentifier = vfwGetU8(&buffer);

      // Fetch next data-block until M_END_OF_MESSAGE
      while (nextMsgIdentifier != M_END_OF_MESSAGE)
      {
        //TODO: The field for LOAD_FINISHED block is not yet defined in FFFIS AOS TCC Ver5.5
        //Needs to be implemented once defined
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
    * RadioMessageInATORemoteControl::invalidate
    ******************************************************************************/
    void RadioMessageInATORemoteControl::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      atoRemoteControlData.requestedSpeed = 0U;
      atoRemoteControlData.timeLimit = 0U;
      atoRemoteControlData.trainDirection = 0U;
      atoRemoteControlData.loadFinishedVec.clear();
      dataProcessState = NoDataAvailable;
    }
  }
}
