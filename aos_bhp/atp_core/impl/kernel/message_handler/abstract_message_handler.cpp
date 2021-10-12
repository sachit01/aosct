/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the AbstractMessageHandler class
* which contains the core functionality of the MessageHandler
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-09    bhermans    Created
* 2016-03-29    bhermans    Parsers and creators split into separate files
* 2016-04-03    bhermans    Include files renamed
* 2016-04-21    lantback    Implemented corePtr()
* 2016-04-22    lantback    Added component type
* 2016-06-16    akushwah    Implemented readMessage()
* 2016-08-04    adgupta     Updated to support TCC Sim Integration
* 2016-08-26    marlundg    Updated for ATP-Limited
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "atc_base.hpp"
#include "atc_util.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_radio_handler.hpp"
#include "radio_message_in_movement_authority.hpp"
#include "radio_message_in_em_alert.hpp"
#include "radio_message_in_revoke_em_alert.hpp"
#include "radio_message_in_stop_train.hpp"
#include "radio_message_in_unregistration.hpp"
#include "radio_message_in_driver_logon_status.hpp"
#include "radio_message_in_command_message.hpp"
#include "radio_message_in_train_setup.hpp"
#include "radio_message_in_area_request.hpp"
#include "radio_message_in_configuration_data.hpp"
#include "radio_message_in_join_command.hpp"
#include "radio_message_in_possession_acknowledge.hpp"
#include "radio_message_in_reject_configuration.hpp"
#include "radio_message_in_shunting_acknowledge.hpp"
#include "radio_message_in_yard_acknowledge.hpp"
#include "radio_message_in_protocol_version.hpp"
#include "radio_message_in_approximate_position.hpp"
#include "radio_message_in_path.hpp"
#include "radio_message_in_position_report_request.hpp"
#include "radio_message_in_unconditional_shortening.hpp"
#include "radio_message_out_driver_information.hpp"
#include "radio_message_out_position_report.hpp"
#include "radio_message_out_protocol_version.hpp"
#include "radio_message_out_startup_message.hpp"
#include "dmi_event_codes.hpp"
#include "dmi_message_out_time.hpp"
#include "abstract_tims.hpp"
#include "abstract_message_handler_event_ids.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_mode_control.hpp"
#include <vfw_checkpoints.h>

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
    /** Current FFFIS TCC-AOS Protocol Version
    */
    static ProtocolVersion currentProtocolVersion(1U, 0U);

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractMessageHandler::AbstractMessageHandler() : ATC::IOComponent(atpMessageHandlerId, "MessageHandler", "MH"),
      // creating different set of objects for different type of events
        receiveQueueFullError(ATC::Event::createSBReqEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdReceiveQueueFullError,
          ATC::NoSB, DMICom::msgHdlrQueueFullError, "Receive queue is full")),
        sendQueueFullError(ATC::Event::createSBReqEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdSendQueueFullError,
          ATC::NoSB, DMICom::msgHdlrQueueFullError, "Send queue is full")),
        validationIncomingMessageFailed(ATC::Event::createLogEvent(atpMessageHandlerId, ATC::CoreContainer,
          eventIdValidationIncomingMessageFailed, 0U, "Validation of incoming message failed, Message Type:", true)),
        trainSetupRejected(ATC::Event::createLogEvent(atpMessageHandlerId, ATC::CoreContainer, 
          eventIdTrainSetupRejectedByAOS, DMICom::msgHdlrTrainSetupRejectedByAOS, "Train Setup message rejected by AOS!")),
        validationOutgoingMessageFailed(ATC::Event::createSafetyHaltEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdValidationOutgoingMessageFailed,
          ATC::NoEB, DMICom::msgHdlrValidationOutgoingMessageFailed, "Validation of outgoing message failed, Message Type:", true)),
        invalidMessageType(ATC::Event::createSafetyHaltEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdInvalidMessageType,
          ATC::NoEB, DMICom::msgHdlrInvalidMessage, "Invalid message type received,Message Type:", true)),
        parserNotImplemented(ATC::Event::createSafetyHaltEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdParserNotImplemented,
          ATC::NoEB, DMICom::msgHdlrInvalidMessage, "Parser for this message type is not implemented, Message Type:", true)),
        parserNullPtr(ATC::Event::createSafetyHaltEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdParserNullPtr,
          ATC::NoEB, DMICom::msgHdlrParserError, "Parser for this message type is NULL, Message Type:", true)),
        noValidPositionreport(ATC::Event::createSBReqEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdNoValidPositionreport,
          ATC::NoSB, DMICom::msgHdlrNoValidPositionreport, "No valid position report available"))
    {
      if (coreMessageHandlerInstancePtr != 0)
      {
        ATC::aosHalt(__FILE__, __LINE__, "Message handler constructor already instantiated");
      }

      // Setup single instance pointer for core access
      coreMessageHandlerInstancePtr = this;

      // Reserve space for in/out message queues
      messageQueueIn.reserve(inQueueSize);
      messageQueueOut.reserve(outQueueSize);

      // Protocol Versions for all regions invalid to start with.
      // Position Report for all regions invalid to start with.
      for (uint8_t regionNr = 0U; regionNr < numberOfRegions; regionNr++)
      {
        protocolVersionValid[regionNr] = false;
        validPositionReport[regionNr] = false;
      }

      isSendRegAreaToTCC = false;
      updateDMITime = false;
      newTimeToSet = 0U;
      timeAtReq = 0U;
      approxPosReceivedEarlier = false;
      configurationDataRecv = false;
      isRegAreaSelectedByDriver = false;
      selectedRegArea = 0U;
      replyChInitConfig = RadioCom::radioChannelId0;
      regAreaMsgSendToCentralTCC = false;
      approxPosRejected = false;
    }

    /******************************************************************************
    * run_in
    ******************************************************************************/
    void AbstractMessageHandler::runIn(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "MH_runIn");
      approxPosRejected = false; 
      RadioMessage msg;
      uint16_t     channelId;

      // Invalidate old data for all parsers
      for (uint8_t messageType = static_cast<uint8_t>(MTypePositionReportRequest);
        messageType < static_cast<uint8_t>(MTypeTCCToAOSMax); messageType++)
      {
        messagesInParsers[messageType]->invalidate();
      }

      // Read and store all messages from RadioHandler
      while (RadioCom::AbstractRadioHandler::corePtr()->readMessage(msg, channelId))
      {
        // Space left in receiving queue?
        if (messageQueueIn.size() < messageQueueIn.capacity())
        {
          RadioMessageReceived messageReceived;

          messageReceived.message = msg;
          messageReceived.channelId = channelId;
          messageQueueIn.push_back(messageReceived);
        }
        else
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(receiveQueueFullError, __FILE__, __LINE__);
        }
      }

      // Any message in the receiving queue?
      if (messageQueueIn.size() > 0U)
      {
        // Trace the binary incoming message (level 3)
        traceBinaryData(&trace, ATC::veryDetailedTrace, &messageQueueIn.front().message.data[0], messageQueueIn.front().message.dataLength);

        // Fetch the NID_MESSAGE_TYPE
        uint8_t messageType = messageQueueIn.front().message.data[nidMessageTypePos];
        AbstractRadioMessageIn *ptrParser = static_cast<AbstractRadioMessageIn*>(NULL);

        if (((messageType >= static_cast<uint8_t>(MTypePositionReportRequest)) && 
            (messageType < static_cast<uint8_t>(MTypeTCCToAOSMax))) || (messageType == static_cast<uint8_t>(MTypeProtocolVersion)))
        {
          if (messageType == static_cast<uint8_t>(MTypeProtocolVersion))
          {
            ptrParser = messagesInParsers[MTypeProtocolVersionIncoming];
          }
          else
          {
            ptrParser = messagesInParsers[messageType];
          }
        }
        else
        {
          //prepare the dynamic text to be send while reporting event.
          validationIncomingMessageFailed.setDynamicText(static_cast<uint32_t>(messageType));
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidMessageType, __FILE__, __LINE__);
        }

        if (ptrParser != static_cast<AbstractRadioMessageIn*>(NULL))
        {
          // Check if a parser for this message type is implemented
          if (ptrParser->getImplemented())
          {           
            // Get next message in queue
            ptrParser->setMessageData(messageQueueIn.front());

            // Parse, validate and publish the data if valid
            if (!ptrParser->validate())
            {
              //Raise an event to inform driver when Train setup message is rejected by AOS
              if (messageType == static_cast<uint8_t>(MTypeTrainSetup))
              {
                ATC::AbstractEventHandler::corePtr()->reportEvent(trainSetupRejected, __FILE__, __LINE__);
              }
              else if (messageType == static_cast<uint8_t>(MTypeApproximatePosition))
              {
                //Approximate position message rejected
                approxPosRejected = true;
              }
              else
              {
                //Do nothing to avoid lint error
              }

              //prepare the dynamic text to be send while reporting event.
              validationIncomingMessageFailed.setDynamicText(static_cast<uint32_t>(messageType));
              ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
              trace.write(ATC::briefTrace, "Incoming message validated NOK");

              // Check if we have a description of the invalidation, then report it.
              const char_t* const description = ptrParser->getInvalidationReason();
              if ((description != static_cast<char_t*>(NULL)) && (*description != '\0'))
              {
                writeToLog(ATC::BriefLog, description,__FILE__, __LINE__);
              }
            }
            else
            {
              trace.write(ATC::briefTrace, "Incoming message validated OK");
            }
          }
          else
          {
            //prepare the dynamic text to be send while reporting event.
            parserNotImplemented.setDynamicText(static_cast<uint32_t>(messageType));
            ATC::AbstractEventHandler::corePtr()->reportEvent(parserNotImplemented, __FILE__, __LINE__);
          }
        }
        else
        {
          //prepare the dynamic text to be send while reporting event.
          parserNullPtr.setDynamicText(static_cast<uint32_t>(messageType));
          ATC::AbstractEventHandler::corePtr()->reportEvent(parserNullPtr, __FILE__, __LINE__);
        }

        //Check whether Approximate position is received earlier or not
        if (getApproximatePosition())
        {
          approxPosReceivedEarlier = true;
        }

        // Get the configuration message status
        if (getConfigDataReceived())
        {
          configurationDataRecv = true;
        }

        // Erase the processed message 
        static_cast<void>(messageQueueIn.erase(messageQueueIn.begin()));
      }
    }

    /******************************************************************************
    * run_out
    ******************************************************************************/
    void AbstractMessageHandler::runOut(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "MH_runOut");

      // Iterate through the creators to collect and push possible data
      for (uint8_t messageType = static_cast<uint8_t>(MTypeDriverInformation);
        messageType < static_cast<uint8_t>(MTypeAOSToTCCMax); messageType++)
      {
        AbstractRadioMessageOut *ptrCreator = static_cast<AbstractRadioMessageOut*>(NULL);

        if (messageType == static_cast<uint8_t>(MTypeProtocolVersionOutgoing))
        {
          ptrCreator = messagesOutCreators[static_cast<uint8_t>(MTypeProtocolVersionOutgoing) - static_cast<uint8_t>(MTypeDriverInformation)];
        }
        else
        {
          ptrCreator = messagesOutCreators[messageType - static_cast<uint8_t>(MTypeDriverInformation)];
        }

        if (static_cast<AbstractRadioMessageOut*>(NULL) != ptrCreator)
        {
          // Invalidate data
          ptrCreator->invalidate();

          // Collect data from components
          ptrCreator->collectData();

          // Validate and assemble data
          if (ptrCreator->validate())
          {

            // Special case, Default Position-Report
            if ((static_cast<uint8_t>(MTypePositionReportRegion1) == messageType) ||
              (static_cast<uint8_t>(MTypePositionReportRegion2) == messageType))
            {
              uint16_t regionIndex;

              RadioMessageOutPositionReport* positionReportCreator =
                ATC::dynamicCast<AbstractRadioMessageOut*, RadioMessageOutPositionReport*>(ptrCreator, __FILE__, __LINE__);

              if (static_cast<uint8_t>(MTypePositionReportRegion1) == messageType)
              {
                regionIndex = region1Pos;
              }
              else //MTypePositionReportRegion2
              {
                regionIndex = region2Pos;
              }

              if (regionIndex < numberOfRegions)
              {
                // A default position-report shall always be valid
                if (ptrCreator->getMessageData(defaultPositionReportMsg[regionIndex]))
                {
                  defaultPositionReport[regionIndex] = positionReportCreator->getPositionReport();
                  validPositionReport[regionIndex] = true;
                }
                else
                {
                  ATC::AbstractEventHandler::corePtr()->reportEvent(noValidPositionreport, __FILE__, __LINE__);
                }
              }
            }

            // Special case, Registration Area is stored as a response for next AreaRequest.
            else if (static_cast<uint8_t>(MTypeRegistrationAreaMessage) == messageType)
            {
              if (ptrCreator->getMessageData(registrationArea))
              {
                isRegAreaSelectedByDriver = true;
              }
            }
            // Other messages are pushed to out-queue.
            else if (messageQueueOut.size() < messageQueueOut.capacity())  // Space left in outgoing queue?
            {
              RadioMessageToSend messageToSend;

              // Get and push message to send
              if (ptrCreator->getMessageData(messageToSend))
              {
                trace.write(ATC::briefTrace, "Outgoing message validated OK");

                // Trace the binary outgoing message
                traceBinaryData(&trace, ATC::veryDetailedTrace, &messageToSend.message.data[0], messageToSend.message.dataLength);

                // Broadcast to all region channels? -> Replace channel ID and push one message for each region radio-channel. 
                if (messageToSend.channelId == RadioCom::radioChannelRegionBroadcast)
                {
                  for (uint16_t chId = RadioCom::radioChannelId2; chId <= RadioCom::radioChannelId3; chId++)
                  {
                    messageToSend.channelId = chId;
                    messageQueueOut.push_back(messageToSend);
                  }
                }
                else
                {
                  // No broadcast -> Just push this message
                  messageQueueOut.push_back(messageToSend);
                }
              }
              else
              {
                //prepare the dynamic text to be send while reporting event.
                validationOutgoingMessageFailed.setDynamicText(static_cast<uint32_t>(messageType));
                ATC::AbstractEventHandler::corePtr()->reportEvent(validationOutgoingMessageFailed, __FILE__, __LINE__);
              }
            }
            else
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(sendQueueFullError, __FILE__, __LINE__);
            }

            // Disable registrationAreaMessageSend flag, case1: TRI message is sent in normal Registration scenarios
            // Case2: Current Mode is Normal in Re-registration Scenarios.
            const ATPMode modeType = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
            if ((static_cast<uint8_t>(MTypeTrainRegistrationInformation) == messageType) || (ATPModeNormal == modeType))
            {
              regAreaMsgSendToCentralTCC = false;
            }
          }
          else if (ptrCreator->isDataAvailable())
          {
            //prepare the dynamic text to be send while reporting event.
            validationOutgoingMessageFailed.setDynamicText(static_cast<uint32_t>(messageType));
            ATC::AbstractEventHandler::corePtr()->reportEvent(validationOutgoingMessageFailed, __FILE__, __LINE__);
          }
          else
          {
            // Do nothing
          }
        }
      }
    }

    /******************************************************************************
    * readMessage
    ******************************************************************************/
    bool AbstractMessageHandler::readMessage(RadioMessageToSend& message, const uint16_t chId)
    {
      bool success = false;
      const bool isRegion = (chId >= RadioCom::radioChannelId2);

      std::vector<RadioMessageToSend>::iterator messageIt = messageQueueOut.begin();
      while (messageIt != messageQueueOut.end())
      {
        const uint16_t messageChId = messageIt->channelId;

        if ((messageChId == chId) || (isRegion && (messageChId == 0U)))
        {
          message = *messageIt;
          static_cast<void>(messageQueueOut.erase(messageIt));
          success = true;
          break;
        }

        ++messageIt;
      }

      return success;
    }

    /******************************************************************************
    * popMessages
    ******************************************************************************/
    void AbstractMessageHandler::popMessages(const uint16_t chId)
    {
      const bool isRegion = (chId >= RadioCom::radioChannelId2);

      std::vector<RadioMessageToSend>::iterator messageIt = messageQueueOut.begin();
      while (messageIt != messageQueueOut.end())
      {
        const uint16_t messageChId = messageIt->channelId;

        if ((messageChId == chId) || (isRegion && (messageChId == 0U)))
        {
          messageIt = messageQueueOut.erase(messageIt);
        }
        else
        {
          ++messageIt;
        }
      }
    }

    /******************************************************************************
    * validateAreaRequest
    ******************************************************************************/
    bool AbstractMessageHandler::validateAreaRequest(const RadioMessage& radioMessageToPeek, const uint16_t chId) const
    {
      return validateMsgType(MTypeAreaRequestMessage, radioMessageToPeek, chId);
    }

    /******************************************************************************
    * validateProtocolVersion
    ******************************************************************************/
    bool AbstractMessageHandler::validateProtocolVersion(const RadioMessage& radioMessageToPeek, const uint16_t chId) const
    {
      return validateMsgType(MTypeProtocolVersion, radioMessageToPeek, chId);
    }

    /******************************************************************************
    * validateMsgType
    ******************************************************************************/
    bool AbstractMessageHandler::validateMsgType(const RadioMessageType msgType, const RadioMessage& radioMessageToPeek, const uint16_t chId) const
    {
      bool retVal = false;

      // Fetch the NID_MESSAGE_TYPE
      uint8_t messageType = radioMessageToPeek.data[nidMessageTypePos];
      AbstractRadioMessageIn *ptrParser = static_cast<AbstractRadioMessageIn*>(NULL);

      // Proper message type?
      if (static_cast<uint8_t>(msgType) == messageType)
      {
        // Special case for the MTypeProtocolVersion which is both in and out.
        if (messageType == static_cast<uint8_t>(MTypeProtocolVersion))
        {
          ptrParser = messagesInParsers[MTypeProtocolVersionIncoming];
        }
        else
        {
          ptrParser = messagesInParsers[messageType];
        }

        // Do the parsing and validation
        if (ptrParser != static_cast<AbstractRadioMessageIn*>(NULL))
        {
          // Check if a parser for this message type is implemented
          if (ptrParser->getImplemented())
          {
            RadioMessageReceived messageReceived;

            messageReceived.message = radioMessageToPeek;
            messageReceived.channelId = chId;

            // Invalidate data
            ptrParser->invalidate();

            // Get data from message
            ptrParser->setMessageData(messageReceived);

            // Parse and validate message
            if (!ptrParser->validate())
            {
              // Prepare the dynamic text to be send while reporting event.
              validationIncomingMessageFailed.setDynamicText(static_cast<uint32_t>(messageType));
              ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
              trace.write(ATC::briefTrace, "Incoming message validated NOK");
            }
            else
            {
              trace.write(ATC::briefTrace, "Incoming message validated OK");
              retVal = true;
            }
          }
          else
          {
            // Prepare the dynamic text to be send while reporting event.
            parserNotImplemented.setDynamicText(static_cast<uint32_t>(messageType));
            ATC::AbstractEventHandler::corePtr()->reportEvent(parserNotImplemented, __FILE__, __LINE__);
          }
        }
        else
        {
          // Prepare the dynamic text to be send while reporting event.
          parserNullPtr.setDynamicText(static_cast<uint32_t>(messageType));
          ATC::AbstractEventHandler::corePtr()->reportEvent(parserNullPtr, __FILE__, __LINE__);
        }
      }
      else
      {
        // Do nothing, the message type wasn't correct.
      }

      return retVal;
    }

    /******************************************************************************
    * getRegistrationArea
    ******************************************************************************/
    bool AbstractMessageHandler::getRegistrationArea(RadioMessage& message) const
    {
      // Check if RegArea was selected by the driver
      if (isRegAreaSelectedByDriver)
      {
        // Get the pre-made complete message
        message = registrationArea.message;
      }

      return isRegAreaSelectedByDriver;
    }

    /******************************************************************************
    * getDefaultPositionReport
    ******************************************************************************/
    bool AbstractMessageHandler::getDefaultPositionReport(RadioMessage& message, const uint16_t chId) const
    {
      bool retVal = false;

      switch (chId)
      {
      case RadioCom::radioChannelId2:
        message = defaultPositionReportMsg[region1Pos].message;
        retVal = validPositionReport[region1Pos];
        break;

      case RadioCom::radioChannelId3:
        message = defaultPositionReportMsg[region2Pos].message;
        retVal = validPositionReport[region2Pos];
        break;

      default:
        trace.write(ATC::briefTrace, "No default position report for this Channel ID!");
        break;
      }

      return retVal;
    }

    /******************************************************************************
    * ackDefaultPositionReport
    ******************************************************************************/
    void AbstractMessageHandler::ackDefaultPositionReport(const uint16_t chId, const bool connected)
    {
      trace.write(ATC::detailedTrace, "Ack Default position report from radio handler received, channel ID = ", static_cast<uint32_t>(chId));

      // Valid region-channel ID?
      if ((chId >= RadioCom::radioChannelId2) && (chId <= RadioCom::radioChannelId3))
      {
        uint8_t positionReportMsgNr;
        uint16_t reportIndex;
        if (chId == RadioCom::radioChannelId2)
        {
          positionReportMsgNr = static_cast<uint8_t>(MTypePositionReportRegion1);
          reportIndex = region1Pos;
        }
        else // chId == RadioCom::radioChannelId3
        {
          positionReportMsgNr = static_cast<uint8_t>(MTypePositionReportRegion2);
          reportIndex = region2Pos;
        }

        RadioMessageOutPositionReport* const radioMessageOutPositionReport =
          ATC::dynamicCast<AbstractRadioMessageOut*, RadioMessageOutPositionReport*> 
          (messagesOutCreators[positionReportMsgNr - static_cast<uint8_t>(MTypeDriverInformation)], __FILE__, __LINE__);

        radioMessageOutPositionReport->ackDefaultPositionReport();

        // Also notify TIMS
        if (connected)
        {
          TG::AbstractTIMS::corePtr()->setPositionReport(
            defaultPositionReport[reportIndex].positionClassification,
            defaultPositionReport[reportIndex].leadingTrackAndPosition,
            defaultPositionReport[reportIndex].trailingTrackAndPosition);
        }
      }
      else
      {
        trace.write(ATC::briefTrace, "Unknown Channel ID!");
      }
    }

    /******************************************************************************
    * getEmAlertReason
    ******************************************************************************/
    bool AbstractMessageHandler::getEmAlertReason(EmAlertReasonInfo& reason) const
    {
      const RadioMessageInEmAlert* const radioMessageInEmAlert =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInEmAlert*> (messagesInParsers[MTypeEmergencyAlert], __FILE__, __LINE__);

      return radioMessageInEmAlert->getEmAlertReason(reason);
    }

    /******************************************************************************
    * isEmAlertStatusSetByUncondMsg
    ******************************************************************************/
    bool AbstractMessageHandler::isEmAlertStatusSetByUncondMsg() const
    {
      const RadioMessageInUnconditionalShortening* const radioMessageInUncodShortMsg =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInUnconditionalShortening*> 
        (messagesInParsers[MTypeUnconditionalShortening], __FILE__, __LINE__);

      return radioMessageInUncodShortMsg->getActiveEmergencyAlert();
    }

    /******************************************************************************
    * getApproximatePosition
    ******************************************************************************/
    bool AbstractMessageHandler::getApproximatePosition() const
    {
      const RadioMessageInApproximatePosition* const radioMessageInApproximatePosition =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInApproximatePosition*> 
        (messagesInParsers[MTypeApproximatePosition], __FILE__, __LINE__);

      return radioMessageInApproximatePosition->getApproximatePosition();
    }

    /******************************************************************************
    * getApproxFrontPos
    ******************************************************************************/
    bool AbstractMessageHandler::getApproxFrontPos(TrackAndPos& tnp) const
    {
      const RadioMessageInApproximatePosition* const radioMessageInFinalApproximatePosition =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInApproximatePosition*> 
        (messagesInParsers[MTypeApproximatePosition], __FILE__, __LINE__);

      return radioMessageInFinalApproximatePosition->getApproxFrontPos(tnp);
    }

    /******************************************************************************
    * isConfigDataReceived
    ******************************************************************************/
    bool AbstractMessageHandler::isConfigDataReceived() const
    {
      return configurationDataRecv;
    }

    /******************************************************************************
    * getAreaRequested
    ******************************************************************************/
    bool AbstractMessageHandler::getAreaRequested(TCCAreas& tccArea) const
    {
      const RadioMessageInAreaRequest* const radioMessageInAreaRequest =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInAreaRequest*> 
        (messagesInParsers[MTypeAreaRequestMessage], __FILE__, __LINE__);

      return radioMessageInAreaRequest->getAvailableAreasFromTCC(tccArea);
    }

    /******************************************************************************
    * getJoinCommand
    ******************************************************************************/
    bool AbstractMessageHandler::getJoinCommand() const
    {
      const RadioMessageInJoinCommand* const radioMessageInJoinCommand =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInJoinCommand*> 
        (messagesInParsers[MTypeJoinCommand], __FILE__, __LINE__);

      return radioMessageInJoinCommand->getJoinCommand();
    }

    /******************************************************************************
    * getPossessionAcknowledge
    ******************************************************************************/
    const PossessionAcknowledge* AbstractMessageHandler::getPossessionAcknowledge() const
    {
      const RadioMessageInPossessionAcknowledge* const radioMessageInPossessionAcknowledge =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInPossessionAcknowledge*> 
        (messagesInParsers[MTypePossessionAcknowledge], __FILE__, __LINE__);

      return radioMessageInPossessionAcknowledge->getPossessionAcknowledge();
    }

    /******************************************************************************
    * getShuntingAcknowledge
    ******************************************************************************/
    bool AbstractMessageHandler::getShuntingAcknowledge(ShuntingAcknowledge& shuntingAck) const
    {
      const RadioMessageInShuntingAcknowledge* const radioMessageInShuntingAcknowledge =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInShuntingAcknowledge*> 
        (messagesInParsers[MTypeShuntingAcknowledge], __FILE__, __LINE__);

      return radioMessageInShuntingAcknowledge->getShuntingAcknowledge(shuntingAck);
    }

    /******************************************************************************
    * getYardAcknowledge
    ******************************************************************************/
    bool AbstractMessageHandler::getYardAcknowledge(YardAcknowledge& yardAck) const
    {
      const RadioMessageInYardAcknowledge* const radioMessageInYardAcknowledge =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInYardAcknowledge*> 
        (messagesInParsers[MTypeYardAcknowledge], __FILE__, __LINE__);

      return radioMessageInYardAcknowledge->getYardAcknowledge(yardAck);
    }

    /******************************************************************************
    * getPath
    ******************************************************************************/
    const Path* AbstractMessageHandler::getPath() const
    {
      const RadioMessageInPath* const radioMessageInPath =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInPath*> (messagesInParsers[MTypePath], __FILE__, __LINE__);

      return radioMessageInPath->getPath();
    }

    /******************************************************************************
    * getRejectConfigurationInfo
    ******************************************************************************/
    bool AbstractMessageHandler::getRejectConfigurationInfo(RejectConfigInfo& info) const
    {
      const RadioMessageInRejectConfiguration* const radioMessageInRejectConfiguration =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInRejectConfiguration*> 
        (messagesInParsers[MTypeRejectConfiguration], __FILE__, __LINE__);

      return radioMessageInRejectConfiguration->getRejectConfigurationReason(info);
    }

    /******************************************************************************
    * getRevokeEmAlert
    ******************************************************************************/
    bool AbstractMessageHandler::getRevokeEmAlert() const
    {
      const RadioMessageInRevokeEmAlert* const radioMessageInRevokeEmAlert =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInRevokeEmAlert*>
        (messagesInParsers[MTypeRevokeEmergencyAlert], __FILE__, __LINE__);

      return radioMessageInRevokeEmAlert->getRevokeEmAlert();
    }

    /******************************************************************************
    * getStopTrain
    ******************************************************************************/
    bool AbstractMessageHandler::getStopTrain() const
    {
      const RadioMessageInStopTrain* const radioMessageInStopTrain =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInStopTrain*> (messagesInParsers[MTypeStopTrain], __FILE__, __LINE__);

      return radioMessageInStopTrain->getStopTrain();
    }

    /******************************************************************************
    * getUnregInfo
    ******************************************************************************/
    bool AbstractMessageHandler::getUnregInfo(UnregInfo& info) const
    {
      const RadioMessageInUnregistration* const radioMessageInUnregistration =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInUnregistration*> (messagesInParsers[MTypeUnregister], __FILE__, __LINE__);

      return radioMessageInUnregistration->getUnregInfo(info);
    }

    /******************************************************************************
    * getDriverLogonStatus
    ******************************************************************************/
    bool AbstractMessageHandler::getDriverLogonStatus(LogonStatus& status) const
    {
      const RadioMessageInDriverLogonStatus* const radioMessageInDriverLogonStatus =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInDriverLogonStatus*> 
        (messagesInParsers[MTypeDriverLogonStatus], __FILE__, __LINE__);

      return radioMessageInDriverLogonStatus->getDriverLogonStatus(status);
    }

    /******************************************************************************
    * getMAHead
    ******************************************************************************/
    bool AbstractMessageHandler::getMAHead(MAHead& head) const
    {
      const RadioMessageInMovementAuthority* const radioMessageInMovementAuthority =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInMovementAuthority*> 
        (messagesInParsers[MTypeMovementAuthority], __FILE__, __LINE__);

      return radioMessageInMovementAuthority->getMAHead(head);
    }

    /******************************************************************************
    * getMAReceived
    ******************************************************************************/
    bool AbstractMessageHandler::getMAReceived(uint8_t& id, uint16_t& replyChannelId) const
    {
      const RadioMessageInMovementAuthority* const radioMessageInMovementAuthority =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInMovementAuthority*> 
        (messagesInParsers[MTypeMovementAuthority], __FILE__, __LINE__);

      return radioMessageInMovementAuthority->getMAReceived(id, replyChannelId);
    }


    /******************************************************************************
    * isMAFromScratch
    ******************************************************************************/
    bool AbstractMessageHandler::isMAFromScratch(void) const
    {
      const RadioMessageInMovementAuthority* const radioMessageInMovementAuthority =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInMovementAuthority*> 
        (messagesInParsers[MTypeMovementAuthority], __FILE__, __LINE__);

      return radioMessageInMovementAuthority->isMAFromScratch();
    }

    /******************************************************************************
    * isValidMAReceived
    ******************************************************************************/
    bool AbstractMessageHandler::isValidMAReceived() const
    {
      MAHead  head;
      return getMAHead(head);
    }

    /******************************************************************************
    * getTrainSetupReceived
    ******************************************************************************/
    bool AbstractMessageHandler::getTrainSetupReceived(uint8_t& id, uint16_t& replyChannelId) const
    {
      const RadioMessageInTrainSetup* const radioMessageInTrainSetup =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInTrainSetup*> (messagesInParsers[MTypeTrainSetup], __FILE__, __LINE__);

      return radioMessageInTrainSetup->getTrainSetupReceived(id, replyChannelId);
    }

    /******************************************************************************
    * getTrainSetupMaxGradient
    ******************************************************************************/
    bool AbstractMessageHandler::getTrainSetupMaxGradient(int8_t& maxGradient)
    {
      RadioMessageInTrainSetup* const radioMessageInTrainSetup =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInTrainSetup*>(messagesInParsers[MTypeTrainSetup], __FILE__, __LINE__);

      return radioMessageInTrainSetup->getTrainSetupMaxGradient(maxGradient);
    }

    /******************************************************************************
    * getTrainSetupReceived
    ******************************************************************************/
    bool AbstractMessageHandler::getTrainSetupReceived(uint8_t& id) const
    {
      uint16_t replyChannelId = 0U;
      return getTrainSetupReceived(id, replyChannelId);
    }

    /******************************************************************************
    * getApproxPosReceived
    ******************************************************************************/
    bool AbstractMessageHandler::getApproxPosReceived(uint8_t& id, uint16_t& replyChannelId) const
    {
      const RadioMessageInApproximatePosition* const radioMessageInApproximatePosition =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInApproximatePosition*> (
          messagesInParsers[MTypeApproximatePosition], __FILE__, __LINE__);

      return radioMessageInApproximatePosition->getApproxPosReceived(id, replyChannelId);
    }

    /******************************************************************************
    * getApproxPosReceived
    ******************************************************************************/
    bool AbstractMessageHandler::getApproxPosReceived(uint8_t& id) const
    {
      uint16_t replyChannelId = 0U;
      return getApproxPosReceived(id, replyChannelId);
    }

    /******************************************************************************
    * getConfigDataReceived
    ******************************************************************************/
    bool AbstractMessageHandler::getConfigDataReceived(uint8_t& id, uint16_t& replyChannelId) const
    {
      const RadioMessageInConfigurationData* const radioMessageInConfigurationData =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInConfigurationData*> 
        (messagesInParsers[MTypeConfigurationData], __FILE__, __LINE__);

      return radioMessageInConfigurationData->getConfigDataReceived(id, replyChannelId);
    }

    /******************************************************************************
    * getTextMessage
    ******************************************************************************/
    const char_t* AbstractMessageHandler::getTextMessage() const
    {
      const RadioMessageInCommandMessage* const radioMessageInCommandMessage =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInCommandMessage*> 
        (messagesInParsers[MTypeCommandMessage], __FILE__, __LINE__);

      return radioMessageInCommandMessage->getTextMessage();
    }

    /******************************************************************************
    * getConfigDataReceived
    ******************************************************************************/
    bool AbstractMessageHandler::getConfigDataReceived() const
    {
      const RadioMessageInConfigurationData* const radioMessageInConfigurationData =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInConfigurationData*> 
        (messagesInParsers[MTypeConfigurationData], __FILE__, __LINE__);

      return radioMessageInConfigurationData->getConfigDataReceived();
    }

    /******************************************************************************
    * getQSetup
    ******************************************************************************/
    bool AbstractMessageHandler::getQSetup(TrainSetupReason& reason) const
    {
      const RadioMessageInTrainSetup* const radioMessageInTrainSetup =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInTrainSetup*> 
        (messagesInParsers[MTypeTrainSetup], __FILE__, __LINE__);

      return radioMessageInTrainSetup->getQSetup(reason);
    }

    /******************************************************************************
    * isTrainSetupRejectedByAOS
    ******************************************************************************/
    bool AbstractMessageHandler::isTrainSetupRejectedByAOS() const
    {
      const RadioMessageInTrainSetup* const radioMessageInTrainSetup =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInTrainSetup*> 
        (messagesInParsers[MTypeTrainSetup], __FILE__, __LINE__);

      return radioMessageInTrainSetup->isTrainSetupRejectedByAOS();
    }

    /******************************************************************************
    * getIncomingProtocolVersionFromTCC
    ******************************************************************************/
    bool AbstractMessageHandler::getIncomingProtocolVersionFromTCC(ProtocolVersion& protocolVersionFromTCC,
        ProtocolResponse& protocolVersionRequest) const
    {
      const RadioMessageInProtocolVersion* const radioMessageInProtocolVersion =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInProtocolVersion*> 
        (messagesInParsers[MTypeProtocolVersionIncoming],__FILE__, __LINE__);

      return radioMessageInProtocolVersion->getProtocolVersionFromTCC(protocolVersionFromTCC, protocolVersionRequest);
    }

    /******************************************************************************
    * isProtocolVersionMatching
    ******************************************************************************/
    bool AbstractMessageHandler::isProtocolVersionMatching(const uint16_t chId) const
    {
      bool retVal = false;

      if ((chId >= RadioCom::radioChannelId2) && (chId <= RadioCom::radioChannelId3))
      {
        retVal = protocolVersionValid[chId - RadioCom::radioChannelId2];
      }
      else
      {
        ATC::aosHalt(__FILE__, __LINE__, "isProtocolVersionMatching(), Array out of bounds for protocolVersionValid");
      }

      return retVal;
    }

    /******************************************************************************
    * isApproxPosReceivedEarlier()
    ******************************************************************************/
    bool AbstractMessageHandler::isApproxPosReceivedEarlier() const
    {
      return approxPosReceivedEarlier;
    }

    /******************************************************************************
    * isApproximatePositionMsgRejected()
    ******************************************************************************/
    bool AbstractMessageHandler::isApproximatePositionMsgRejected() const
    {
      return approxPosRejected;
    }

    /******************************************************************************
    * getDepartureSignal()
    ******************************************************************************/
    bool AbstractMessageHandler::getDepartureSignal(AcousticSignal& acousticSignal) const
    {
      const RadioMessageInMovementAuthority* const radioMessageInMA =  ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInMovementAuthority*>
        (messagesInParsers[MTypeMovementAuthority], __FILE__, __LINE__);

      return radioMessageInMA->getDepartureSignal(acousticSignal);
    }

    /******************************************************************************
    * setProtocolVersionValid
    ******************************************************************************/
    void AbstractMessageHandler::setProtocolVersionStatus(const bool versionValid, const uint16_t chId)
    {
      // Must be a valid region channel.
      if ((chId >= RadioCom::radioChannelId2) && (chId <= RadioCom::radioChannelId3))
      {
        protocolVersionValid[chId - RadioCom::radioChannelId2] = versionValid;
      }
      else
      {
        ATC::aosHalt(__FILE__, __LINE__, "isProtocolVersionMatching(), Array out of bounds for protocolVersionValid");
      }
    }

    /******************************************************************************
    * getProtocolVersionResponse
    ******************************************************************************/
    bool AbstractMessageHandler::getProtocolVersionResponse(RadioMessage& message) const
    {
      bool retVal = false;

      // Protocol Version Response to TCC.
      RadioMessageToSend protocolVersionOut;

      // Create response to TCC
      Kernel::RadioMessageOutProtocolVersion outMsgProtocolVersion;

      // Invalidate data
      outMsgProtocolVersion.invalidate();

      // Collect data from components
      outMsgProtocolVersion.collectData();

      // Validate and assemble data
      if (outMsgProtocolVersion.validate())
      {
        if (outMsgProtocolVersion.getMessageData(protocolVersionOut))
        {
          // Get the pre-made complete message
          message = protocolVersionOut.message;
          retVal = true;
        }
      }

      return retVal;
    }

    /******************************************************************************
    * getProtocolVersion
    ******************************************************************************/
    const ProtocolVersion& AbstractMessageHandler::getProtocolVersion() const
    {
      return currentProtocolVersion;
    }

    /******************************************************************************
    * getAckDefaultPositionReport
    ******************************************************************************/
    bool AbstractMessageHandler::getAckDefaultPositionReport(const uint16_t chId) const
    {
      bool retVal = false;

      // Valid region-channel ID?
      if ((chId >= RadioCom::radioChannelId2) && (chId <= RadioCom::radioChannelId3))
      {
        uint8_t positionReportMsgNr;
        
        if (RadioCom::radioChannelId2 == chId)
        {
          positionReportMsgNr = static_cast<uint8_t>(MTypePositionReportRegion1);
        }
        else
        {
          positionReportMsgNr = static_cast<uint8_t>(MTypePositionReportRegion2);
        }

        RadioMessageOutPositionReport* const radioMessageOutPositionReport =
          ATC::dynamicCast<AbstractRadioMessageOut*, RadioMessageOutPositionReport*> 
          (messagesOutCreators[positionReportMsgNr - static_cast<uint8_t>(MTypeDriverInformation)], __FILE__, __LINE__);

        retVal = radioMessageOutPositionReport->getAckDefaultPositionReport();

      }
      else
      {
        trace.write(ATC::briefTrace, "Unknown Region Channel ID!");
      }

      return retVal;
    }

    /******************************************************************************
    * isStartUpMessageSent
    ******************************************************************************/
    bool AbstractMessageHandler::isStartUpMessageSent() const
    {
      RadioMessageOutStartUpMessage* const radioMessageOutStartUpMessage =
        ATC::dynamicCast<AbstractRadioMessageOut*, RadioMessageOutStartUpMessage*>(messagesOutCreators[static_cast<uint8_t>(MTypeStartUpMessage)
          - static_cast<uint8_t>(MTypeDriverInformation)], __FILE__, __LINE__);

      return radioMessageOutStartUpMessage->isStartUpMessageSent();
    }

    /******************************************************************************
    * setReplyRegAreaToTCC
    ******************************************************************************/
    void AbstractMessageHandler::setReplyRegAreaToTCC(const bool isNeeded)
    {
      isSendRegAreaToTCC = isNeeded;
    }

    /******************************************************************************
    * getRegAreaSelectedByDriver
    ******************************************************************************/
    bool AbstractMessageHandler::getRegAreaSelectedByDriver(uint8_t& areaId) const
    {
      if (isRegAreaSelectedByDriver)
      {
        areaId = selectedRegArea;
      }

      return isRegAreaSelectedByDriver;
    }

    /******************************************************************************
    * setRegAreaSelectedByDriver
    ******************************************************************************/
    void AbstractMessageHandler::setRegAreaSelectedByDriver(const uint8_t areaId)
    {
      isRegAreaSelectedByDriver = true;

      selectedRegArea = areaId;
    }

    /******************************************************************************
    * clearAreaSelectedByDriver
    ******************************************************************************/
    void AbstractMessageHandler::clearAreaSelectedByDriver()
    {
      isRegAreaSelectedByDriver = false;

      selectedRegArea = 0U;
    }

    /******************************************************************************
    * setUpdateDMITime
    ******************************************************************************/
    void AbstractMessageHandler::setUpdateDMITime(const bool updateNeeded)
    {
      updateDMITime = updateNeeded;
    }


    /******************************************************************************
    * updateSystemTime
    ******************************************************************************/
    void AbstractMessageHandler::updateSystemTime(const uint32_t newTime)
    {
      if (0U != newTime) //Sanity check
      {
#ifdef  _SIL
        writeToLog(ATC::BriefLog, "Not possible to set ATP system time in SIL environment.",__FILE__, __LINE__);
#else
        // To be done in environments except SIL
        bool ret = false;

        uint32_t currentTime = 0U;
        ATC::getUTCTime(currentTime);

        if (currentTime != newTime)
        {
          ret = ATC::setUTCTime(newTime);

          this->timeAtReq = currentTime;
          this->newTimeToSet = newTime;

          // Compensate Time Calendar Calculations (if UTC time was properly set)
          if (ret)
          {
            const int32_t diffTime = static_cast<int32_t>(static_cast<int64_t>(newTime) - static_cast<int64_t>(currentTime));
            // Changing the timestamps for BTM and brake-test will cause problem at every startup when the time is adjusted after first login
            // We shall consider removing this compensation permanently
                //const int32_t diffTime = static_cast<int32_t>(static_cast<int64_t>(newTime) - static_cast<int64_t>(currentTime));
            //compensateCalendarCalculations(diffTime);
            writeToLog(ATC::BriefLog, "Calendar-time changed (sec): ", diffTime,__FILE__, __LINE__);
            // Update OPC calendar time
            IO::AbstractBTMHandler::corePtr()->updateBtmCalendarTime(newTime);

            if (VFW_A_SIDE == vfwGetSide())
            {
              trace.write(ATC::briefTrace, "UTC Time updated successfully on A side");
              writeToLog(ATC::BriefLog, "UTC Time updated successfully on A side",__FILE__, __LINE__);
            }
          }
          // To be done in environments except SIL
          else if (VFW_A_SIDE == vfwGetSide())
          {
            trace.write(ATC::briefTrace, "UTC Time could not be updated on A side");
            writeToLog(ATC::BriefLog, "UTC Time could not be updated on A side",__FILE__, __LINE__);
          }
          else
          {
            // Nothing
          }
        }
        else if (VFW_A_SIDE == vfwGetSide())
        {
          trace.write(ATC::briefTrace, "UTC Time same on A side, no need to update!");
          writeToLog(ATC::BriefLog, "UTC Time same on A side, no need to update!");
        }
        else
        {
          // Nothing
        }

        // Update DMI time as well.
        if (ret)
#endif // !_SIL
        {
          setUpdateDMITime(true);
        }
      }
    }

    /******************************************************************************
    * getTimeNew
    ******************************************************************************/
    uint32_t AbstractMessageHandler::getTimeNew() const
    {
      return newTimeToSet;
    }

    /******************************************************************************
    * getTimeAtReq
    ******************************************************************************/
    uint32_t AbstractMessageHandler::getTimeAtReq() const
    {
      return timeAtReq;
    }

    /******************************************************************************
    * getUpdateDMITime
    ******************************************************************************/
    bool AbstractMessageHandler::getUpdateDMITime() const
    {
      return updateDMITime;
    }

    /******************************************************************************
    * getInitiateConfig
    ******************************************************************************/
    bool AbstractMessageHandler::getInitiateConfig(InitiateConfigReason& initiateConfigValue) const
    {
      const RadioMessageInPositionReportRequest* const radioMessageInPositionReportRequest =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInPositionReportRequest*> 
        (messagesInParsers[MTypePositionReportRequest], __FILE__, __LINE__);

      return radioMessageInPositionReportRequest->getInitiateConfig(initiateConfigValue);
    }

    /******************************************************************************
    * getPartlyMaReceived
    ******************************************************************************/
    bool AbstractMessageHandler::getPartlyMaReceived() const
    {
      const RadioMessageInMovementAuthority* const radioMessageInMA =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInMovementAuthority*> 
        (messagesInParsers[MTypeMovementAuthority], __FILE__, __LINE__);

      return radioMessageInMA->getPartlyMaReceived();
    }

    /******************************************************************************
    * getMaxSearchDistInReReg
    ******************************************************************************/
    bool AbstractMessageHandler::getMaxSearchDistInReReg(uint32_t& maxDist) const
    {
      const RadioMessageInMovementAuthority* const radioMessageInMA =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInMovementAuthority*> 
        (messagesInParsers[MTypeMovementAuthority], __FILE__, __LINE__);

      return radioMessageInMA->getMaxSearchDistInReReg(maxDist);
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractMessageHandler* AbstractMessageHandler::corePtr(void)
    {
      return coreMessageHandlerInstancePtr;
    }

    /******************************************************************************
    * compensateCalendarCalculations
    ******************************************************************************/
    void AbstractMessageHandler::compensateCalendarCalculations(const int32_t diffTime) const
    {
      writeToLog(ATC::BriefLog, "Calendar-time changed (sec): ", diffTime,__FILE__, __LINE__);

      // Fetch last BTM Test Time
      uint32_t timeLastTestPassed = AbstractConfig::corePtr()->getLastBTMTestTime();

      // Write the updated BTM Routine Test time to NVSH
      if (!AbstractConfig::corePtr()->setLastBTMTestTime(timeLastTestPassed + static_cast<uint32_t>(diffTime)))
      {
        writeToLog(ATC::BriefLog, "Last BTM Test Time not set properly",__FILE__, __LINE__);
      }
      else
      {
        writeToLog(ATC::BriefLog, "Last BTM Test Time updated OK",__FILE__, __LINE__);
      }

      // Fetch last Brake Test Time
      timeLastTestPassed = AbstractConfig::corePtr()->getLastBrakeTestTime();

      // Write the updated Brake Routine Test time to NVSH
      if (!AbstractConfig::corePtr()->setLastBrakeTestTime(timeLastTestPassed + static_cast<uint32_t>(diffTime)))
      {
        writeToLog(ATC::BriefLog, "Last Brake Test Time not set properly",__FILE__, __LINE__);
      }
      else
      {
        writeToLog(ATC::BriefLog, "Last Brake Test Time updated OK",__FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * getReplyChannelInitConfig
    ******************************************************************************/
    void AbstractMessageHandler::getReplyChannelInitConfig(uint16_t& chId) const
    {
      chId = replyChInitConfig;
    }

    /******************************************************************************
    * setReplyChannelInitConfig
    ******************************************************************************/
    void AbstractMessageHandler::setReplyChannelInitConfig(const uint16_t chId)
    {
      // Must be a region channel
      if ((RadioCom::radioChannelId2 <= chId) && (RadioCom::radioChannelId3 >= chId))
      {
        replyChInitConfig = chId;
      }
      else
      {
        trace.write(ATC::briefTrace, "Unknown Channel ID!");
      }
    }

    /******************************************************************************
    * getRegistrationAreaMessageSentToCentralTCC
    ******************************************************************************/
    bool AbstractMessageHandler::getRegistrationAreaMessageSentToCentralTCC() const
    {
      return regAreaMsgSendToCentralTCC;
    }

    /******************************************************************************
    * setRegistrationAreaMessageSentToCentralTCC
    ******************************************************************************/
    void AbstractMessageHandler::setRegistrationAreaMessageSentToCentralTCC(const bool valueToSet)
    {
      regAreaMsgSendToCentralTCC = valueToSet;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractMessageHandler::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&receiveQueueFullError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&sendQueueFullError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&validationIncomingMessageFailed));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&validationOutgoingMessageFailed));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&trainSetupRejected));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidMessageType));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&parserNotImplemented));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&parserNullPtr));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&noValidPositionreport));

      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&approxPosReceivedEarlier));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&approxPosRejected));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&regAreaMsgSendToCentralTCC));

      // DefaultPositionReports and ProtocolVersionsValid for all regions
      for (uint8_t channelId = 0U; channelId < numberOfRegions; channelId++)
      {
        crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&defaultPositionReportMsg[channelId].message.id));
        crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&defaultPositionReportMsg[channelId].message.siteId));
        crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&defaultPositionReportMsg[channelId].message.regionId));
        crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&defaultPositionReportMsg[channelId].message.tSender));
        crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&defaultPositionReportMsg[channelId].message.tRef));
        crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&defaultPositionReportMsg[channelId].message.dataLength));

        crossCompare->addCrossCompareData(new Support::CrossCompareBool(&protocolVersionValid[channelId]));
        crossCompare->addCrossCompareData(new Support::CrossCompareBool(&validPositionReport[channelId]));
      }

      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&isSendRegAreaToTCC));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&updateDMITime));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&newTimeToSet));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&timeAtReq));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&configurationDataRecv));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&isRegAreaSelectedByDriver));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&selectedRegArea));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&replyChInitConfig));
    }
  }
}
