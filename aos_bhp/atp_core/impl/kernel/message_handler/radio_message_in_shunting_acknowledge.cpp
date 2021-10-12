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
* This file implements the parser for the ShuntingAcknowledge message.
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
#include "radio_message_in_shunting_acknowledge.hpp"
#include "abstract_odometry.hpp"

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
    * RadioMessageInShuntingAcknowledge constructor
    ******************************************************************************/
    RadioMessageInShuntingAcknowledge::RadioMessageInShuntingAcknowledge() : AbstractRadioMessageIn(MTypeShuntingAcknowledge)
    {
      implemented = true;
      shuntingAcknowledgeData.shuntingAcknowledge = RequestNotAcknowledged;
      shuntingAcknowledgeData.allowedSpeedInShunting = 0U;
    }

    /******************************************************************************
    * RadioMessageInShuntingAcknowledge::validate
    ******************************************************************************/
    bool RadioMessageInShuntingAcknowledge::validate()
    {
      trace->write(ATC::briefTrace, "Validating ShuntingAcknowledge");

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
    * RadioMessageInShuntingAcknowledge::validateMode
    ******************************************************************************/
    bool RadioMessageInShuntingAcknowledge::validateMode() const
    {
      bool modeValid = false;

      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
      case ATPModePowerUp:
      case ATPModeUnregistered:
      case ATPModeLocation:
      case ATPModeSafeBrakeToStop:
      case ATPModeBaliseSearch:
      case ATPModeNormal:
      case ATPModeStaffResponsible:
      case ATPModeShuntingRoute:
      case ATPModeSplit:
      case ATPModeJoin:
      case ATPModeConfiguration:
      case ATPModeRegistration:
      case ATPModeYard:
        if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
        {
          modeValid = true;
        }
        break;
      case ATPModePossession:
        modeValid = true;
        break;
      case ATPModeSleeping:
      {
        if (IO::AbstractLocoIO::corePtr()->getSleepingSignal())
        {
          //Reject the incoming message from TCC when the sleep signal is active
          modeValid = false;
          invalidIncmgMsgTCC.setDynamicText("Shunting Ack");
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
        }
        else
        {
          if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
          {
            // Accept the incoming message from TCC when the sleep signal is inactive
            modeValid = true;
          }
        }
        break;
      }
      case ATPModeShunting:
      case ATPModePoweringDown:
      case ATPModeSafetyHalt:
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
    * RadioMessageInShuntingAcknowledge::parseMessageData
    ******************************************************************************/
    bool RadioMessageInShuntingAcknowledge::parseMessageData()
    {
      bool parseDataValid = true;
      uint8_t tmpValU8;

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

      // Read & validate Q_ACKNOWLEDGE
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateQ_ACKNOWLEDGE(tmpValU8))
      {
        trace->write(ATC::detailedTrace, "Q_ACKNOWLEDGE invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("Q_ACKNOWLEDGE");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }
      else
      {
        shuntingAcknowledgeData.shuntingAcknowledge = static_cast<Acknowledge>(tmpValU8);
      }

      // Read V_SPEED
      shuntingAcknowledgeData.allowedSpeedInShunting = vfwGetU16(&buffer);

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
    * RadioMessageInShuntingAcknowledge::invalidate
    ******************************************************************************/
    void RadioMessageInShuntingAcknowledge::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      shuntingAcknowledgeData.shuntingAcknowledge = RequestNotAcknowledged;
      shuntingAcknowledgeData.allowedSpeedInShunting = 0U;
      dataProcessState = NoDataAvailable;
    }

    /******************************************************************************
    * RadioMessageInShuntingAcknowledge::getShuntingAcknowledge
    ******************************************************************************/
    bool RadioMessageInShuntingAcknowledge::getShuntingAcknowledge(ShuntingAcknowledge &shuntingAck) const
    {
      if (DataValidated == dataProcessState)
      {
        shuntingAck = shuntingAcknowledgeData;
      }

      return (DataValidated == dataProcessState);
    }
  }
}
