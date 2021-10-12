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
* This file implements the parser for the YardAcknowledge message.
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
#include "radio_message_in_yard_acknowledge.hpp"
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
    * RadioMessageInYardAcknowledge constructor
    ******************************************************************************/
    RadioMessageInYardAcknowledge::RadioMessageInYardAcknowledge()
      : AbstractRadioMessageIn(MTypeYardAcknowledge)
    {
      implemented = true;
      yardAcknowledgeData.yardAcknowledge = RequestNotAcknowledged;
      yardAcknowledgeData.allowedSpeedInYard = 0U;
    }

    /******************************************************************************
    * RadioMessageInYardAcknowledge::validate
    ******************************************************************************/
    bool RadioMessageInYardAcknowledge::validate()
    {
      trace->write(ATC::briefTrace, "Validating Yard Acknowledge");

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
    * RadioMessageInYardAcknowledge::validateMode
    ******************************************************************************/
    bool RadioMessageInYardAcknowledge::validateMode() const
    {
      bool modeValid = false;
      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();

      switch (mode)
      {
      case ATPModeSafeBrakeToStop:
      case ATPModeUnregistered:
      case ATPModeLocation:
      case ATPModeBaliseSearch:
      case ATPModeNormal:
      case ATPModeStaffResponsible:
      case ATPModeShuntingRoute:
      case ATPModeSplit:
      case ATPModeConfiguration:
      case ATPModeRegistration:
      case ATPModePowerUp:
      case ATPModeShunting:
      case ATPModeJoin:
      case ATPModePoweringDown:
      case ATPModeSafetyHalt:
      case ATPModePossession:
        modeValid = true;
        break;
      case ATPModeSleeping:
        if (!IO::AbstractLocoIO::corePtr()->getSleepingSignal())
        {
          modeValid = true;
        }
        break;
      case ATPModeYard:
        // Do nothing
        break;
      case ATPModesCount:
      case ATPModeUndefined:
      default:
        ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        break;
      }

      if (!modeValid)
      {
        invalidIncmgMsgTCC.setDynamicText("YardAcknowledge");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
      }

      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInYardAcknowledge::parseMessageData
    ******************************************************************************/
    bool RadioMessageInYardAcknowledge::parseMessageData()
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
        yardAcknowledgeData.yardAcknowledge = static_cast<Acknowledge>(tmpValU8);
      }

      // Read V_SPEED
      yardAcknowledgeData.allowedSpeedInYard = vfwGetU16(&buffer);

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
    * RadioMessageInYardAcknowledge::invalidate
    ******************************************************************************/
    void RadioMessageInYardAcknowledge::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      yardAcknowledgeData.yardAcknowledge = RequestNotAcknowledged;
      yardAcknowledgeData.allowedSpeedInYard = 0U;
      dataProcessState = NoDataAvailable;
    }

    /******************************************************************************
    * RadioMessageInYardAcknowledge::getYardAcknowledge
    ******************************************************************************/
    bool RadioMessageInYardAcknowledge::getYardAcknowledge(YardAcknowledge &yardAck) const
    {
      if (DataValidated == dataProcessState)
      {
        yardAck = yardAcknowledgeData;
      }

      return (DataValidated == dataProcessState);
    }
  }
}
