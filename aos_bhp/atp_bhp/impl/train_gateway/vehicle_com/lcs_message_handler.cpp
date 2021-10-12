/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
* This file implements the methods of the LCSMessageHandler class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-24    marlundg    Created

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_string.h>
#include "atc_base.hpp"
#include "atc_util.hpp"
#include "abstract_vehicle_com.hpp"
#include "abstract_event_handler.hpp"
#include "lcs_message_handler.hpp"
#include "abstract_cross_compare.hpp"
#include "lcs_message_in_train_status.hpp"
#include "lcs_message_in_rcl_status.hpp"
#include "lcs_message_out_rcl_information.hpp"
#include "lcs_message_in_ecpb_train_composition.hpp"
#include "lcs_message_out_status.hpp"
#include "lcs_message_out_ma.hpp"
#include "lcs_message_out_atp_command.hpp"
#include "lcs_message_out_path.hpp"
#include "channel_config.hpp"
#include "lcs_message_out_train_composition.hpp"
#include "abstract_console.hpp"
#include "lcs_message_out_warning_curve.hpp"
#include "vehicle_com_event_ids.hpp"
#include "dmi_bhp_event_codes.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_log_handler.hpp"
#include <cstdio>
#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#endif

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
  namespace TG
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    LCSMessageHandler::LCSMessageHandler(const char_t * const readChannelName, const char_t * const writeChannelName) :
      validationIncomingMessageFailed(ATC::Event::createLogEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdValidationIncomingMessageFailed, 0U, "Validation of incoming message failed, Message Type:",true)),
      validationOutgoingMessageFailed(ATC::Event::createSBReqEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdValidationOutgoingMessageFailed, ATC::NoSB,DMICom::validationOutgoingMessageFailed, "Validation of outgoing message failed")),
      invalidMessageType(ATC::Event::createSBReqEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdInvalidMessageType, ATC::NoSB, DMICom::invalidMessageType, "Invalid message type received, Message Type:",true)),
      parserNullPtr(ATC::Event::createSBReqEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdParserNullPtr, ATC::NoSB, DMICom::parserError, "Parser for this message type is NULL, Message Type:",true)),
      timeDeviationResponseFromLCS(ATC::Event::createLogEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdTimeDeviationResponseFromLCS, 0U, "Message(s) from LCS has too high time deviation.")),
      timeDeviationRecoveredFromLCS(ATC::Event::createLogEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdTimeDeviationRecoveredFromLCS, 0U, "Message(s) from LCS has recovered from time deviation.")),
      missedMessageFromLCS(ATC::Event::createLogEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdMissedMessageFromLCS, DMICom::missedMessageFromLCS, "Message sequence numbers not in order from LCS")),
      parserNotImplemented(ATC::Event::createSBReqEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdParserNotImplemented, ATC::NoSB, DMICom::parserNotImplemented,
        "Parser for this message type is not implemented, Message Type:", true)),
      lostConnectionWithLCSandECPB(ATC::Event::createSBReqEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdLostConnectionWithLCSandECPB, ATC::NoSB, DMICom::lostConnectionWithLCSandECPB,
        "Connection lost with LCS, and brake system in use is ECPB")),
      lostConnectionWithLCS(ATC::Event::createLogEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdLostConnectionWithLCS, 0U, "Connection lost with LCS")),
      establishedConnectionWithLCS(ATC::Event::createLogEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdEstablishedConnectionWithLCS, 0U, "Connection established with LCS")),
      validationCRCFailed(ATC::Event::createLogEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdValidationCRCFailed, 0U, "Validation of CRC failed, Message type:", true)),
      invalidMessageValue(ATC::Event::createLogEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdInvalidMessageValue, 0U, "Invalid message value received, Message Type:", true)),
      initDone(false),
      oldEmpStatus(IncorrectMsgNum),
      connectedToLcs(false),
      lcsWatchdogStartTime(0),
      incorrectMsgTime(false),
      lastSentUTCTimeToLCS(0U),
      lastReceivedUTCTimeFromLCS(0U)
    {

      trace = AbstractVehicleCom::corePtr()->getTrace();


      // Save the name of the channel.
      static_cast<void>(vfw_strlcpy(&(*lcsReadChannelName), readChannelName, sizeof(lcsReadChannelName)));
      static_cast<void>(vfw_strlcpy(&(*lcsWriteChannelName), writeChannelName, sizeof(lcsWriteChannelName)));

      syncChannelReadDesc = static_cast<VFW_SyncChannel>(NULL);

      for (uint8_t i = 0U; i < (static_cast<uint16_t>(LCSMTypeLCStoAOSMax) - static_cast<uint16_t>(LCSMTypeTrainStatusMessage)); i++)
      {
        messagesInParsers[i] = static_cast<AbstractLCSMessageIn*>(NULL);
      }

      for (uint8_t i = 0U; i < numOfAosLcsMessage; i++)
      {
        messagesOutCreators[i] = static_cast<AbstractLCSMessageOut*>(NULL);
      }

      memset(&chstat[0], 0, sizeof(chstat));
    }

    /******************************************************************************
    * preInit
    ******************************************************************************/
    void LCSMessageHandler::preInit()
    {
      /** Channel handle returned by vfwChannelOpenRead
      */
      VFW_ChannelDesc channelReadDesc;

      // Open a channel to be used when reading from dispatcher
      channelReadDesc = vfwChannelOpenRead(&(*lcsReadChannelName), ATC::lcsMessageInQueueSize,
        ATC::lcsMsgSize, &(*lcsReadChannelName));

      // Open a channel to be used when writing to dispatcher
      crossCompareWriteChannel.initChannel(&(*lcsWriteChannelName), ATC::lcsMessageOutQueueSize,
        ATC::lcsMsgSize, true);

      //lint --e{586} 'new' is acceptable during initialization

      const uint16_t startIndexInparser = static_cast<uint16_t>(LCSMTypeTrainStatusMessage);

      // Create and fill the container with parsers for incoming messages
      messagesInParsers[static_cast<uint16_t>(LCSMTypeTrainStatusMessage) - startIndexInparser] = new LCSMessageInTrainStatus();
      messagesInParsers[static_cast<uint16_t>(LCSMTypeECPBTrainCompositionMessage) - startIndexInparser] = new LCSMessageInECPBTrainComposition();
      messagesInParsers[static_cast<uint16_t>(LCSMTypeRCLStatusMessage) - startIndexInparser] = new LCSMessageInRclStatus();

      // Create and fill the container with creators for outgoing messages
      uint8_t messageOutCreatorIter = 0U;

      messagesOutCreators[messageOutCreatorIter] = new LCSMessageOutStatus();
      messageOutCreatorIter = incMessageOutCreatorIter(messageOutCreatorIter);

      messagesOutCreators[messageOutCreatorIter] = new LCSMessageOutRclInformation();
      messageOutCreatorIter = incMessageOutCreatorIter(messageOutCreatorIter);

      messagesOutCreators[messageOutCreatorIter] = new LCSMessageOutATPCommand();
      messageOutCreatorIter = incMessageOutCreatorIter(messageOutCreatorIter);

      messagesOutCreators[messageOutCreatorIter] = new LCSMessageOutMovementAuthority();
      messageOutCreatorIter = incMessageOutCreatorIter(messageOutCreatorIter);

      messagesOutCreators[messageOutCreatorIter] = new LCSMessageOutWarningCurve();
      messageOutCreatorIter = incMessageOutCreatorIter(messageOutCreatorIter);

      messagesOutCreators[messageOutCreatorIter] = new LCSMessageOutTrainComposition();
      messageOutCreatorIter = incMessageOutCreatorIter(messageOutCreatorIter);

      messagesOutCreators[messageOutCreatorIter] = new LCSMessageOutPath();

      if (NULL != channelReadDesc)
      {
        // Synchronize with diversified channel (A/B)
        syncChannelReadDesc = vfwSyncAddChannel(channelReadDesc, ATC::trueVfw);

        // Not event-driven, cyclic polled
        vfwSyncChannelDeactivate(syncChannelReadDesc);
      }
      else
      {
        ATC::aosHalt(__FILE__, __LINE__, "Failed to open channels");
      }
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool LCSMessageHandler::init()
    {
      // Only initialize once
      if (!initDone)
      {   
        initCrossCompare();

        //Update channel stats
        //Initialize the channel statistics for the component
        static_cast<void>(vfw_strlcpy(&(chstat[0].channelname[0]), &lcsReadChannelName[0], sizeof(chstat[0].channelname)));
        static_cast<void>(vfw_strlcpy(&(chstat[0].channelType[0]), "read", sizeof(chstat[0].channelType)));
        chstat[0].numMsgCnt = 0U;
        chstat[0].numMsgBytesCnt = 0U;

        static_cast<void>(vfw_strlcpy(&(chstat[1].channelname[0]), &lcsWriteChannelName[0], sizeof(chstat[1].channelname)));
        static_cast<void>(vfw_strlcpy(&(chstat[1].channelType[0]), "write", sizeof(chstat[0].channelType)));
        chstat[1].numMsgCnt = 0U;
        chstat[1].numMsgBytesCnt = 0U;

        initDone = true;
      }

      return initDone;
    }

    /******************************************************************************
    * runIn
    ******************************************************************************/
    void LCSMessageHandler::runIn(void)
    {
      const int64_t timeNow = vfwGetReferenceTime();

      // Invalidate old data for all parsers
      for(uint16_t messageType = static_cast<uint16_t>(LCSMTypeTrainStatusMessage);
        messageType < static_cast<uint16_t>(LCSMTypeLCStoAOSMax); messageType++)
      {
        messagesInParsers[messageType - static_cast<uint16_t>(LCSMTypeTrainStatusMessage)]->invalidate();
      }
 
      bool previousIncorrectMsgTime = incorrectMsgTime;

      // Check if there is any message in the channel queue
      while (vfwSyncChannelStat(syncChannelReadDesc) > 0U)
      {
        // Read one message into the EMPMsg buffer
        VFW_ChannelCheck check = { VFW_ChannelErrorNone, 0U };
        const int32_t noOfBytesRead = vfwSyncChannelReadCheck(
          syncChannelReadDesc, empMessage.getEMPBuffer(), empMessage.getEMPMessageMaxTotalLen(), &check);

        if (check.error != VFW_ChannelErrorNone)
        {
          ATC::AbstractLogHandler::corePtr()->writeToLog(
            ATC::BriefLog, "LCS channel error:", static_cast<uint32_t>(check.error), "VC", __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "LCS channel error");
        }
        if (check.timeSinceProduced > ATC::maxChannelTransmissionTime)
        {
          ATC::AbstractLogHandler::corePtr()->writeToLog(
            ATC::BriefLog, "DMI message too old:", check.timeSinceProduced, "VC", __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "LCS message too old");
        }

        // Read was successful?
        if (noOfBytesRead > 0)
        {  
          uint16_t messageType = 0U;
          uint8_t messageVersion = 0U;

          EMPParseError empStatus;

          // Update channel stats
          chstat[0].numMsgCnt++;
          chstat[0].numMsgBytesCnt += static_cast<uint32_t>(noOfBytesRead);

          // Cross compare incoming data
          Support::AbstractCrossCompare::corePtr()->addCrossCompareInputData(empMessage.getEMPBuffer(), static_cast<uint16_t>(noOfBytesRead));

          // Parse EMP Header/Footer and fetch message type and version
          empStatus = empMessage.parseEMPMessage(messageType, messageVersion);

          // Trace the binary incoming message
          trace->write(ATC::veryDetailedTrace, "Incoming message:");
          traceBinaryData(trace, ATC::veryDetailedTrace, empMessage.getEMPBuffer(), empMessage.getEMPMessageActualLen());

          // Store the last received UTC time from LCS
          lastReceivedUTCTimeFromLCS = empMessage.getEMPMsgHeader().msgTime;

          switch (empStatus)
          {
          case NoError:
            // Set no fault as default for the 'IncorrectMsgTime'.
            incorrectMsgTime = false;

            trace->write(ATC::briefTrace, "Incoming message EMP Parsing OK");
            break;
          case IncorrectProtocolVersion:
            trace->write(ATC::briefTrace, "Incoming message IncorrectProtocolVersion");
            ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
            break;
          case IncorrectFlag:
            trace->write(ATC::briefTrace, "Incoming message IncorrectFlag");
            ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
            break;
          case IncorrectMsgNum:
            trace->write(ATC::briefTrace, "Incoming message IncorrectMsgNum");
            ATC::AbstractEventHandler::corePtr()->reportEvent(missedMessageFromLCS, __FILE__, __LINE__);
             break;
          case IncorrectMsgTime:
            trace->write(ATC::veryDetailedTrace, "Incoming message IncorrectMsgTime");
            
            incorrectMsgTime = true;

            // TODO: Logging onto the U or Logging an event should be done after the requirement of time delay clear.
            // Which time should be compared(msh header time or the time in the Status message) and shall we use ATP 
            // local time or UTC time to compare time difference. 

            // For now, the RU-logging is done after this switch-case and will only produce one log when the incorrect time is
            // detected or recovered.

            break;
          case IncorrectVariableHeader:
            trace->write(ATC::briefTrace, "Incoming message IncorrectVariableHeader");
            ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
            break;
          case IncorrectCRC:
            validationCRCFailed.setDynamicText(static_cast<uint32_t>(messageType));
            ATC::AbstractEventHandler::corePtr()->reportEvent(validationCRCFailed, __FILE__, __LINE__);
            trace->write(ATC::briefTrace, "Incoming message IncorrectCRC");

            break;

          default:
            trace->write(ATC::briefTrace, "Incoming message Unknown fault");
            ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
          }                  
          
          // Detect only changes. Message with high time deviation received from LCS?
          if (incorrectMsgTime && (!previousIncorrectMsgTime))
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(timeDeviationResponseFromLCS, __FILE__, __LINE__);
          }
          else if ((!incorrectMsgTime) && previousIncorrectMsgTime)
          {
            // Recovered from time deviation
            ATC::AbstractEventHandler::corePtr()->reportEvent(timeDeviationRecoveredFromLCS, __FILE__, __LINE__);
          }
          else
          {
            // No action if no change.
          }
          
          // Remember current value for evaluation next time
          previousIncorrectMsgTime = incorrectMsgTime;
          
          // Continue if no error (IncorrectMsgTime are also allowed for now)
          // Error handling for IncorrectMsgNum/IncorrectMsgTime
          if ((NoError == empStatus) || (IncorrectMsgTime == empStatus))
          {
            AbstractLCSMessageIn *ptrParser = static_cast<AbstractLCSMessageIn*>(NULL);

            // Fetch the proper parser for received message type
            if ((messageType >= static_cast<uint16_t>(LCSMTypeTrainStatusMessage)) &&
              (messageType < static_cast<uint16_t>(LCSMTypeLCStoAOSMax)))
            {
              ptrParser = messagesInParsers[messageType - static_cast<uint16_t>(LCSMTypeTrainStatusMessage)];
            }
            else
            {
              //prepare the dynamic text to be send while reporting event.
              invalidMessageType.setDynamicText(static_cast<uint32_t>(messageType));
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidMessageType, __FILE__, __LINE__);
            }

            if (ptrParser != NULL)
            {
              // Check if a parser for this message type is implemented
              if (ptrParser->getImplemented())
              {
                // Is the message version valid?
                if (ptrParser->getVersion() == messageVersion)
                {
                  // Parse, validate and publish the data if valid
                  if (!ptrParser->validate(&empMessage))
                  {
                     //prepare the dynamic text to be send while reporting event.
                     invalidMessageValue.setDynamicText(static_cast<uint32_t>(messageType));
                     ATC::AbstractEventHandler::corePtr()->reportEvent(invalidMessageValue, __FILE__, __LINE__);
                    trace->write(ATC::briefTrace, "Incoming message validated NOK");
                  }
                  else
                  {
                    trace->write(ATC::briefTrace, "Incoming message validated OK");

                    ptrParser->logToRU(&empMessage);

                    // Two consecutive correct messages were received -> Set status to 'Connected'
                    if ((NoError == oldEmpStatus) || (IncorrectMsgTime == oldEmpStatus))
                    {
                      
                      // Notify that the connection has changed status if previous state was NOT connected.
                      if (!connectedToLcs)
                      {
                        connectedToLcs = true;
                        ATC::AbstractEventHandler::corePtr()->reportEvent(establishedConnectionWithLCS, __FILE__, __LINE__);
                      }

                      // Start the Watchdog timer
                      lcsWatchdogStartTime = timeNow;
                    }
                  }
                }
                else
                {
                  //prepare the dynamic text to be send while reporting event.
                  validationIncomingMessageFailed.setDynamicText(static_cast<uint32_t>(messageType));
                  ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
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
          }
  
          // Remember the empStatus for next message to detect if link is up (needs 2 successive messages in sequence).
          oldEmpStatus = empStatus;

        }
      }

      // Check timeout towards LCS
      if ((lcsWatchdogStartTime != 0) && ((timeNow - lcsWatchdogStartTime) >= lcsWatchdogTimeout))
      {
        const BrakeSystemType brakeSystem = DS::AbstractTSetup::corePtr()->getBrakeSystemInUse();

        // Brake-event if connection was lost when brake system in-use is ECPB.
        const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
        if ((BrakeSystemType2 == brakeSystem) && (!isStandStill))
        {
          // Apply brake until Train is not StandStill.
          ATC::AbstractEventHandler::corePtr()->reportEvent(lostConnectionWithLCSandECPB, __FILE__, __LINE__);
        }
        else
        {
          // Notify that the connection has changed status if previous state was connected.
          if (connectedToLcs)
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(lostConnectionWithLCS, __FILE__, __LINE__);
          }
        }

        connectedToLcs = false;
        lcsWatchdogStartTime = 0;
      }
    }

    /******************************************************************************
    * runOut
    ******************************************************************************/
    void LCSMessageHandler::runOut(void)
    {  
      // Iterate through the outgoing message creators to collect and push possible data
      for (uint16_t messageItr = 0U; messageItr < numOfAosLcsMessage; messageItr++)
      {
        AbstractLCSMessageOut *ptrCreator = messagesOutCreators[messageItr];
        uint16_t messageType = static_cast<uint16_t>(ptrCreator->getMessageType());
        uint16_t empBodyLength = 0U;

        // Invalidate data
        ptrCreator->invalidate();

        // Collect data from components
        ptrCreator->collectData();

        // Validate and assemble data -> Put network message and length in empMessage/empBodyLength
        if (ptrCreator->validate(&empMessage, empBodyLength))
        {
          trace->write(ATC::briefTrace, "Outgoing message validated OK");

          // Add EMP-Header
          if (empMessage.addEMPEnvelope(messageType, ptrCreator->getVersion(), empBodyLength))
          {
            // Trace the binary outgoing message
            trace->write(ATC::veryDetailedTrace, "Outgoing message:");
            traceBinaryData(trace, ATC::veryDetailedTrace, empMessage.getEMPBuffer(), empMessage.getEMPMessageActualLen());
            ptrCreator->logToRU(&empMessage);
            
            // Store the last sent UTC time to LCS
            lastSentUTCTimeToLCS = empMessage.getEMPMsgHeader().msgTime;

            // Cross compare outgoing data and write message to dispatcher for further processing in Class-D and transmitting to LCS
            crossCompareWriteChannel.putBuffer(empMessage.getEMPBuffer(), empMessage.getEMPMessageActualLen());
            crossCompareWriteChannel.useNextMessage();

            // Update channel stats
            chstat[1].numMsgCnt++;
            chstat[1].numMsgBytesCnt += empMessage.getEMPMessageActualLen();
          }
        }
      }
    }

    /******************************************************************************
    * getTrainStatus
    ******************************************************************************/
    bool LCSMessageHandler::getTrainStatus(LCSTrainStatusType& trainStatus) const
    {
      LCSMessageInTrainStatus *lcsMessageInTrainStatus = ATC::dynamicCast<AbstractLCSMessageIn*, LCSMessageInTrainStatus*>(
        messagesInParsers[static_cast<uint16_t>(LCSMTypeTrainStatusMessage) - static_cast<uint16_t>(LCSMTypeTrainStatusMessage)]
        , __FILE__, __LINE__); 

      const bool retVal = lcsMessageInTrainStatus->getTrainStatus(trainStatus);

      return retVal;
    }


    /******************************************************************************
    * getECPBTrainComposition
    ******************************************************************************/
    bool LCSMessageHandler::getECPBTrainComposition(ECPBTrainCompositionType& ecpbTrainComposition) const
    {
      LCSMessageInECPBTrainComposition *lcsMessageInECPBTrainComposition = 
        ATC::dynamicCast<AbstractLCSMessageIn*, LCSMessageInECPBTrainComposition*>(
          messagesInParsers[static_cast<uint16_t>(LCSMTypeECPBTrainCompositionMessage) - static_cast<uint16_t>(LCSMTypeTrainStatusMessage)],
          __FILE__, __LINE__); 

      const bool retVal = lcsMessageInECPBTrainComposition->getECPBTrainComposition(ecpbTrainComposition);

      return retVal;
    }

    /******************************************************************************
    * getHandlingDone
    ******************************************************************************/
    bool LCSMessageHandler::getHandlingDone(HandlingDoneType& handlingDone) const
    {
      LCSMessageInRclStatus *lcsMessageInRclStatus =
        ATC::dynamicCast<AbstractLCSMessageIn*, LCSMessageInRclStatus*>(
          messagesInParsers[static_cast<uint16_t>(LCSMTypeRCLStatusMessage) - static_cast<uint16_t>(LCSMTypeTrainStatusMessage)],
          __FILE__, __LINE__);

      const bool retVal = lcsMessageInRclStatus->getHandlingDone(handlingDone);

      return retVal;
    }


    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void LCSMessageHandler::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&incorrectMsgTime));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&lastSentUTCTimeToLCS));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&lastReceivedUTCTimeFromLCS));
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool LCSMessageHandler::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments and searches for component specific command 
      calls and handles it. Returns true if completely handled else returns false. 
      Returning false will let other components handle the call. Help always returns false.
      */
      bool retVal = false;
      char_t  buff[100];

      if (ATC::isTextMatch(&argv[0][0], "chstat", sizeof("chstat")) && (argc == 1U))
      {
        for (uint8_t cnt = 0U; cnt < numVfwChannelsLCSMessage; cnt++)
        {
          //lint -e{586} snprintf is needed here
          const int32_t ret = snprintf(&buff[0], sizeof(buff), "%-30s%-14s%-15u%-12u", chstat[cnt].channelname, chstat[cnt].channelType,
            chstat[cnt].numMsgCnt, chstat[cnt].numMsgBytesCnt);

          if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buff)))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buff[0]);
          }
        }
      }

      return retVal;
    }
    
    /******************************************************************************
    * connectedLCS
    ******************************************************************************/
    bool LCSMessageHandler::connectedLCS() const
    {
      return connectedToLcs;
    }

    /******************************************************************************
    * getLastSentUTCTimeToLCS
    ******************************************************************************/
    uint32_t LCSMessageHandler::getLastSentUTCTimeToLCS() const
    {
      return lastSentUTCTimeToLCS;
    }

    /******************************************************************************
    * getLastReceivedUTCTimeFromLCS
    ******************************************************************************/
    uint32_t LCSMessageHandler::getLastReceivedUTCTimeFromLCS() const
    {
      return lastReceivedUTCTimeFromLCS;
    }

    /******************************************************************************
    * incMessageOutCreatorIter
    ******************************************************************************/
    uint8_t LCSMessageHandler::incMessageOutCreatorIter(const uint8_t messageOutCreatorIter) const
    {
      const uint8_t nextIndex = messageOutCreatorIter + 1U;
      if (messageOutCreatorIter >= numOfAosLcsMessage)
      {
        ATC::aosHalt(__FILE__, __LINE__, "Not enough space in array for AOS-LCS Message creator");
      }
      return nextIndex;
    }
  }
}
