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
* This file implements the parser for the PositionReport message.
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
#include <cstdio>
#include "abstract_event_handler.hpp"
#include "abstract_message_handler.hpp"
#include "radio_message_in_position_report_request.hpp"

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
    * RadioMessageInPositionReportRequest constructor
    ******************************************************************************/
    RadioMessageInPositionReportRequest::RadioMessageInPositionReportRequest()
      : AbstractRadioMessageIn(MTypePositionReportRequest)
    {
      implemented = true;
      
      // Setup Fixed Sizes for msg-block vectors.
      positionReportRequest.waitingTimeReceived = false;
      positionReportRequest.initiateConfigReceived = false;
    }

    /******************************************************************************
    * RadioMessageInPositionReportRequest::validate
    ******************************************************************************/
    bool RadioMessageInPositionReportRequest::validate()
    {
      trace->write(ATC::briefTrace, "Validating PositionReportRequest");

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
            if (publishData())
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
    * RadioMessageInPositionReportRequest::publishData
    ******************************************************************************/
    bool RadioMessageInPositionReportRequest::publishData() const
    {
      if (positionReportRequest.initiateConfigReceived)
      {
        // Save channel where INITIATE_CONFIG was received (for StartUp message reply).
        AbstractMessageHandler::corePtr()->setReplyChannelInitConfig(messageData.channelId);
      }

      return true;
    }

    /******************************************************************************
    * RadioMessageInPositionReportRequest::getInitiateConfig
    ******************************************************************************/
    bool RadioMessageInPositionReportRequest::getInitiateConfig(InitiateConfigReason &initiateConfigValue) const
    {
      bool initiateConfigValid = false;

      // Data must be validated and the INITIATE_CONFIG block must be present.
      if (DataValidated == dataProcessState)
      {
        if (positionReportRequest.initiateConfigReceived)
        {
          initiateConfigValue = positionReportRequest.initiateConfig.statusOfTrainConfig;

          initiateConfigValid = true;
        }
      }
  
      return initiateConfigValid;
    }

    /******************************************************************************
    * RadioMessageInPositionReportRequest::parseMessageData
    ******************************************************************************/
    bool RadioMessageInPositionReportRequest::parseMessageData()
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

      // BlockData
      uint8_t nextMsgIdentifier = vfwGetU8(&buffer);

      // Fetch next data-block until M_END_OF_MESSAGE
      while ((nextMsgIdentifier != M_END_OF_MESSAGE) && (parseDataValid))
      {
        switch (nextMsgIdentifier)
        {
        case BTypeWaitingTime:
          {
            // Read the T_WAITING_TIME
            uint8_t tmpValU8 = vfwGetU8(&buffer);

            if (!positionReportRequest.waitingTimeReceived)
            {
              positionReportRequest.waitingTimeReceived = true;
              positionReportRequest.waitingTime.estimatedTimeToWait = tmpValU8;
            }
            else
            {
              trace->write(ATC::detailedTrace, "BTypeWaitingTime overflow");
              parseDataValid = false;
              invalidDataInTCCMessage.setDynamicText("WAITING_TIME");
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
            }
          }
          break;

        case BTypeInitiateConfig:
          {
            uint8_t tmpValU8 = vfwGetU8(&buffer);

            if (!validateQ_INITIATE(tmpValU8))
            {
              trace->write(ATC::detailedTrace, "Q_INITIATE invalid");
              parseDataValid = false;
              invalidDataInTCCMessage.setDynamicText("Q_INITIATE");
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
            }
            else
            {
              if (!positionReportRequest.initiateConfigReceived)
              {
                positionReportRequest.initiateConfigReceived = true;
                positionReportRequest.initiateConfig.statusOfTrainConfig = static_cast<InitiateConfigReason>(tmpValU8);
              }
              else
              {
                trace->write(ATC::detailedTrace, "BTypeInitiateConfig overflow");
                parseDataValid = false;
                invalidDataInTCCMessage.setDynamicText("INITIATE_CONFIG");
                ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
              }
            }
          }
          break;

        default:
          parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
          break;
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

      // Log according to trace-level
      detailedLog();

      traceParseData(parseDataValid);
      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInPositionReportRequest::invalidate
    ******************************************************************************/
    void RadioMessageInPositionReportRequest::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      // Clear all data-block 
      positionReportRequest.waitingTimeReceived = false;
      positionReportRequest.initiateConfigReceived = false;
    
      dataProcessState = NoDataAvailable;
    }
    /******************************************************************************
    * RadioMessageInPositionReportRequest::detailedLog
    ******************************************************************************/
    void RadioMessageInPositionReportRequest::detailedLog(void) const
    {
      uint8_t currentLevel;
      bool isEnabled;

      trace->getTraceDetails(currentLevel, isEnabled);

      if (isEnabled && (currentLevel >= ATC::detailedMessageTrace))
      {   // No reason to assemble logStr if trace not enabled

        // The only block logged so far in detailed log is the Q_INITIATE which is not always present
        if (positionReportRequest.initiateConfigReceived)
        {
          char_t logStr[120];

          //lint -e{586} snprintf is needed here
          int32_t res = snprintf(&logStr[0], sizeof(logStr),
            "Q_INITIATE=%u", static_cast<uint32_t> (positionReportRequest.initiateConfig.statusOfTrainConfig));

          if ((res > 0) && (static_cast<size_t>(res) < sizeof(logStr)))
          {
            traceLog(ATC::detailedMessageTrace, ATC::DetailedLog, &logStr[0]);
          }
        }
      }
    }
  }
}
