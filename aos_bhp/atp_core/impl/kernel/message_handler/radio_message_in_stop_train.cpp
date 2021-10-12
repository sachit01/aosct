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
* This file implements the parser for the StopTrain message.
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
#include "radio_message_in_stop_train.hpp"
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
    * RadioMessageInStopTrain constructor
    ******************************************************************************/
    RadioMessageInStopTrain::RadioMessageInStopTrain() :
      AbstractRadioMessageIn(MTypeStopTrain)
    {
      implemented = true;
    }

    /******************************************************************************
    * RadioMessageInStopTrain::getStopTrain
    ******************************************************************************/
    bool RadioMessageInStopTrain::getStopTrain() const
    {
      return (DataValidated == dataProcessState);
    }
    /******************************************************************************
    * RadioMessageInStopTrain::validate
    ******************************************************************************/
    bool RadioMessageInStopTrain::validate()
    {
      trace->write(ATC::briefTrace, "Validating StopTrain");

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
    * RadioMessageInStopTrain::validateMode
    ******************************************************************************/
    bool RadioMessageInStopTrain::validateMode() const
    {
      bool modeValid = false;

      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();

      switch (mode)
      {
      case ATPModeYard:
      case ATPModePossession:
      case ATPModeShunting:
      case ATPModeSleeping:
        //Reject the incoming message from TCC 
        invalidIncmgMsgTCC.setDynamicText("Stop Train");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
        break;
      case ATPModeNormal:
      case ATPModeStaffResponsible:
      case ATPModeShuntingRoute:
      case ATPModeLocation:
      case ATPModeSplit:
      case ATPModeJoin:
        modeValid = true;
        break;
      case ATPModePowerUp:
      case ATPModeConfiguration:
      case ATPModeRegistration:
      case ATPModeBaliseSearch:
      case ATPModeUnregistered:
      case ATPModePoweringDown:
      case ATPModeSafetyHalt:
      case ATPModeSafeBrakeToStop:
        break;
      case ATPModeUndefined:
      case ATPModesCount:
      default:
        {
        ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        }
        break;
      }

      traceValidateMode(modeValid);
      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInStopTrain::parseMessageData
    ******************************************************************************/
    bool RadioMessageInStopTrain::parseMessageData()
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
    * RadioMessageInStopTrain::invalidate
    ******************************************************************************/
    void RadioMessageInStopTrain::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      dataProcessState = NoDataAvailable;
    }
  }
}
