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
* This file implements the parser for the EmAlert message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-20    bhermans    Created
* 2016-04-03    bhermans    File renamed
* 2016-08-26    marlundg    Updated for ATP-Limited
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "radio_message_in_em_alert.hpp"
#include "abstract_mode_control.hpp"
#include "dmi_event_codes.hpp"
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
    * RadioMessageInEmAlert constructor
    ******************************************************************************/
    RadioMessageInEmAlert::RadioMessageInEmAlert() : 
      AbstractRadioMessageIn(MTypeEmergencyAlert), 
      emAlertReason(static_cast<uint8_t>(EmAlertUndefined)),
      emAlertMsgUnknownReason(ATC::Event::createLogEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdUnknownReasonInEmAlertMsg,
        DMICom::noDmi, "Unknown reason EmAlert message received.", true))
    {
      implemented = true;
    }

    /******************************************************************************
    * RadioMessageInEmAlert::getEmAlertReason
    ******************************************************************************/
    bool RadioMessageInEmAlert::getEmAlertReason(EmAlertReasonInfo & reason) const
    {
      if (DataValidated == dataProcessState)
      {
        reason = emAlertReason;
      }

      return (DataValidated == dataProcessState);
    }
    /******************************************************************************
    * RadioMessageInEmAlert::validate
    ******************************************************************************/
    bool RadioMessageInEmAlert::validate()
    {
      trace->write(ATC::briefTrace, "Validating EmAlert");

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
    * RadioMessageInEmAlert::validateMode
    ******************************************************************************/
    bool RadioMessageInEmAlert::validateMode() const
    {
      bool modeValid = false;
        
      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
        case ATPModeConfiguration:
        case ATPModeRegistration:
        case ATPModeBaliseSearch:
        case ATPModeNormal:
        case ATPModeLocation:
        case ATPModeStaffResponsible:
        case ATPModeSplit:
        case ATPModeJoin:
        case ATPModeShuntingRoute:
        case ATPModeUnregistered:
        case ATPModeSafeBrakeToStop:
        case ATPModeYard:
        case ATPModePossession:
        case ATPModeShunting:
          modeValid = true;
          break;
        case ATPModeSleeping:
          //Reject the incoming message from TCC 
          invalidIncmgMsgTCC.setDynamicText("Emergency Alert");
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
          break;
        case ATPModePowerUp:
        case ATPModeSafetyHalt:
        case ATPModePoweringDown:
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
    * RadioMessageInEmAlert::parseMessageData
    ******************************************************************************/
    bool RadioMessageInEmAlert::parseMessageData()
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
      
      // Validate Q_ALERT
      emAlertReason = vfwGetU8(&buffer);
      if (isQ_ALERTUnknown(emAlertReason))
      {
        trace->write(ATC::detailedTrace, "Q_ALERT unknown reason");
        emAlertMsgUnknownReason.setDynamicText("Q_ALERT");
        ATC::AbstractEventHandler::corePtr()->reportEvent(emAlertMsgUnknownReason, __FILE__, __LINE__);
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

      // Log according to trace-level
      detailedLog();

      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInEmAlert::invalidate
    ******************************************************************************/
    void RadioMessageInEmAlert::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      emAlertReason = static_cast<uint8_t>(EmAlertUndefined);
      dataProcessState = NoDataAvailable;
    }

    /******************************************************************************
    * RadioMessageInEmAlert::detailedLog
    ******************************************************************************/
    void RadioMessageInEmAlert::detailedLog(void) const
    {
      uint8_t currentLevel;
      bool isEnabled;

      trace->getTraceDetails(currentLevel, isEnabled);

      if (isEnabled && (currentLevel >= ATC::detailedMessageTrace))
      {   // No reason to assemble logStr if trace not enabled
        char_t logStr[120];

        //lint -e{586} snprintf is needed here
        int32_t res = snprintf(&logStr[0], sizeof(logStr),
          "Q_ALERT=%u",
          static_cast<uint32_t> (emAlertReason));

        if ((res > 0) && (static_cast<size_t>(res) < sizeof(logStr)))
        {
          traceLog(ATC::detailedMessageTrace, ATC::DetailedLog, &logStr[0]);
        }
      }
    }

    /******************************************************************************
    * RadioMessageInEmAlert::isQ_ALERTUnknown
    ******************************************************************************/
    bool RadioMessageInEmAlert::isQ_ALERTUnknown(const EmAlertReasonInfo val) const
    {
      return val >= EmAlertUnknown;
    }
  }
}
