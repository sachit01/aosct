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
* This file implements the parser for the DriverLogonStatus message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-26    marlundg    Created
* 2017-04-11    skothiya    Updated for implementation of cabin handling and authorization requirements
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_mode_control.hpp"
#include "radio_message_in_driver_logon_status.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_message_handler_event_ids.hpp"

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
    * RadioMessageInDriverLogonStatus constructor
    ******************************************************************************/
    RadioMessageInDriverLogonStatus::RadioMessageInDriverLogonStatus() : AbstractRadioMessageIn(MTypeDriverLogonStatus),
      invalidDriverLogonStatusSeq(ATC::Event::createLogEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdInvalidDriverLogonStatusSeq,
        0U, "DriverLogonStatus message received from TCC while driver is already logged-in"))
    {
      implemented = true;

      driverLogonStatus.logonStatus = DriverLogonFailed;
      driverLogonStatus.setTime.currentTime = 0U;
    }

    /******************************************************************************
    * RadioMessageInDriverLogonStatus::getDriverLogonStatus
    ******************************************************************************/
    bool RadioMessageInDriverLogonStatus::getDriverLogonStatus(LogonStatus & status) const
    {
      if (DataValidated == dataProcessState)
      {
        status = driverLogonStatus.logonStatus;
      }

      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageInDriverLogonStatus::validate
    ******************************************************************************/
    bool RadioMessageInDriverLogonStatus::validate()
    {
      trace->write(ATC::briefTrace, "Validating DriverLogonStatus");

      bool ret;

      ret = AbstractRadioMessageIn::validate();

      if (ret)
      {
        ret = false;

        // Parse, validate and publish data
        if (DataAvailable == dataProcessState)
        {
          if ((AbstractModeControl::corePtr()->getDriverLoginSeqState()) != DriverLoginSeq::driverLoggedIn)
          {
            if (parseMessageData())
            {
              if (validateMode())
              {
                /** Call setTimeOfDay application with the Time Extracted. **/
                AbstractMessageHandler::corePtr()->updateSystemTime(static_cast<uint32_t>(driverLogonStatus.setTime.currentTime));

                dataProcessState = DataValidated;
                ret = true;
              }
            }
          }
          else
          {
            //Reporting event DriverLogonStatus message received while driver is already in login (authorized) state
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDriverLogonStatusSeq, __FILE__, __LINE__);
          }
        }
      }

      return ret;
    }

    /******************************************************************************
    * RadioMessageInDriverLogonStatus::validateMode
    ******************************************************************************/
    bool RadioMessageInDriverLogonStatus::validateMode() const
    {
      /*
      LogonStatus message has to be accepted in all the modes except Sleeping
      */
      bool modeValid = false;

      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
        case ATPModePowerUp:
        case ATPModeYard:
        case ATPModeShunting:
        case ATPModePossession:
        case ATPModeConfiguration:
        case ATPModeRegistration:
        case ATPModeBaliseSearch:
        case ATPModeNormal:
        case ATPModeLocation:
        case ATPModeStaffResponsible:
        case ATPModeSplit:
        case ATPModeJoin:
        case ATPModeShuntingRoute:
        case ATPModePoweringDown:
        case ATPModeUnregistered:
        case ATPModeSafetyHalt:
        case ATPModeSafeBrakeToStop:
          modeValid = true;
          break;
        case ATPModeSleeping:
        {
          if (IO::AbstractLocoIO::corePtr()->getSleepingSignal())
          {
            //Reject the incoming message from TCC when the sleep signal is active
            modeValid = false;
            invalidIncmgMsgTCC.setDynamicText("DriverLogonStatus");
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
          }
          else
          {
            // Accept the incoming message from TCC when the sleep signal is inactive
            modeValid = true;
          }
          break;
        }
        case ATPModesCount:
        case ATPModeUndefined:
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
    * RadioMessageInDriverLogonStatus::parseMessageData
    ******************************************************************************/
    bool RadioMessageInDriverLogonStatus::parseMessageData()
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

      // Read & validate Q_LOGON_STATUS
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateQ_LOGON_STATUS(tmpValU8))
      {
        trace->write(ATC::detailedTrace, "Q_LOGON_STATUS invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("Q_LOGON_STATUS");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }
      else
      {
        driverLogonStatus.logonStatus = static_cast<LogonStatus>(tmpValU8);
      }

      // Read & validate NID_BLOCK_TYPE (SET_TIME)
      if (vfwGetU8(&buffer) != BTypeSetTime)
      {
        trace->write(ATC::detailedTrace, "SET_TIME invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("SET_TIME");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

      // Read & validate T_CLOCK 
      driverLogonStatus.setTime.currentTime = vfwGetU64(&buffer);
      if (!validateT_CLOCK(driverLogonStatus.setTime.currentTime))
      {
        trace->write(ATC::detailedTrace, "T_CLOCK invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("T_CLOCK");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

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
    * RadioMessageInDriverLogonStatus::invalidate
    ******************************************************************************/
    void RadioMessageInDriverLogonStatus::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      dataProcessState = NoDataAvailable;
    }
  }
}
