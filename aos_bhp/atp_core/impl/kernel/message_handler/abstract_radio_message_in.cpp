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
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-20    bhermans    Created
* 2016-03-28    bhermans    Split into separate file per parser
* 2016-04-03    bhermans    File renamed
* 2016-08-26    marlundg    Updated for ATP-Limited
* 2020-01-17    plonnber    Validate last byte of message is M_END_OF_MESSAGE
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"

#include "abstract_log_handler.hpp"
#include "abstract_radio_message_in.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_message_handler_event_ids.hpp"
#include "dmi_event_codes.hpp"

#include <vfw_string.h>
#include <cstdio>

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
    * AbstractRadioMessageIn destructor
    ******************************************************************************/
    AbstractRadioMessageIn::~AbstractRadioMessageIn()
    {
      trace = static_cast<ATC::TraceInterface*>(NULL);
    }

    /******************************************************************************
    * AbstractRadioMessageIn alternative constructor
    ******************************************************************************/
    AbstractRadioMessageIn::AbstractRadioMessageIn(const RadioMessageType mType) :
      messageType(mType), dataProcessState(NoDataAvailable), implemented(false),
      invalidIncmgMsgTCC(ATC::Event::createLogEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdInvalidIncomingMsg,
        0x0U, "Incoming message from TCC discarded, Message:", true)),
      invalidDataInTCCMessage(ATC::Event::createSafetyHaltEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdInvalidDataInMessage,
        ATC::NoEB, DMICom::msgHdlrInvalidDataInMessage, "Invalid data in message from TCC:", true)),
      invalidBlockTypeInTCCMessage(ATC::Event::createSafetyHaltEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdInvalidBlockTypeInMessage,
        ATC::NoEB, DMICom::msgHdlrInvalidDataInMessage, "Invalid NID_BLOCK_TYPE in message from TCC:", true)),
      invalidBlockTypeInRejectedTCCMessage(ATC::Event::createLogEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdInvalidBlockTypeInRejectedMessage,
        DMICom::msgHdlrInvalidDataInMessage, "Invalid NID_BLOCK_TYPE in rejected message from TCC:", true))
    {

      // Use the TraceInterface from Message Handler
      trace = AbstractMessageHandler::corePtr()->getTrace();

      memset(&invalidationReason[0], 0, sizeof(invalidationReason));
    }

    /******************************************************************************
    * getImplemented
    ******************************************************************************/
    bool AbstractRadioMessageIn::getImplemented() const
    {
      return implemented;
    }

    /******************************************************************************
    * setInvalidationReason
    ******************************************************************************/
    void AbstractRadioMessageIn::setInvalidationReason(const char_t* const description)
    {
      trace->write(ATC::detailedMessageTrace, description);
      static_cast<void>(vfw_strlcpy(&invalidationReason[0], description, sizeof(invalidationReason)));
    }

    /******************************************************************************
    * getInvalidationReason
    ******************************************************************************/
    const char_t* const AbstractRadioMessageIn::getInvalidationReason() const
    {
      return &invalidationReason[0];
    }

    /******************************************************************************
    * setMessageData
    ******************************************************************************/
    void AbstractRadioMessageIn::setMessageData(const RadioMessageReceived& mData)
    {
      messageData = mData;

      briefLog();

      // Data is now available to parse and validate
      dataProcessState = DataAvailable;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool AbstractRadioMessageIn::validate()
    {
      return true;
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void AbstractRadioMessageIn::invalidate()
    {
      invalidationReason[0] = '\0';
    }

    /******************************************************************************
    * validateSizeOfParsedBytes
    ******************************************************************************/
    bool AbstractRadioMessageIn::validateSizeOfParsedBytes(const VFW_Buffer* const buffer) const
    {
      return ((vfwGetMaxSize(buffer) - vfwGetFreeSpace(buffer)) == messageData.message.dataLength);
    }

    /******************************************************************************
    * tracePublishData
    ******************************************************************************/
    void AbstractRadioMessageIn::tracePublishData(const bool publishOk) const
    {
      if (publishOk)
      {
        trace->write(ATC::veryDetailedTrace, "Publishing incoming data Ok");
      }
      else
      {
        trace->write(ATC::veryDetailedTrace, "Publishing incoming data failed");
      }
    }

    /******************************************************************************
    * traceParseData
    ******************************************************************************/
    void AbstractRadioMessageIn::traceParseData(const bool parseOk) const
    {
      if (parseOk)
      {
        trace->write(ATC::veryDetailedTrace, "Parsing incoming data Ok");
      }
      else
      {
        trace->write(ATC::veryDetailedTrace, "Parsing incoming data failed");
      }
    }

    /******************************************************************************
    * traceValidateMode
    ******************************************************************************/
    void AbstractRadioMessageIn::traceValidateMode(const bool validateModeOk) const
    {
      if (validateModeOk)
      {
        trace->write(ATC::veryDetailedTrace, "Validating mode Ok");
      }
      else
      {
        trace->write(ATC::veryDetailedTrace, "Validating mode failed");
      }
    }

    /******************************************************************************
    * writeToLog
    ******************************************************************************/
    void AbstractRadioMessageIn::writeToLog(ATC::LogLevel const level, const char_t* const text,
      const char_t* const filepath, const int32_t line) const
    {
      ATC::AbstractLogHandler::corePtr()->writeToLog(level, text, "MH", filepath,line);
    }

    /******************************************************************************
    * briefLog
    ******************************************************************************/
    void AbstractRadioMessageIn::briefLog(void) const
    {
      uint8_t currentLevel;
      bool isEnabled;

      trace->getTraceDetails(currentLevel, isEnabled);

      if (isEnabled && (currentLevel >= ATC::briefMessageTrace))
      {   // No reason to assemble logStr if trace not enabled
        char_t logStr[80];
        //lint -e{586} snprintf is needed here
        const int32_t res = snprintf(&logStr[0], sizeof(logStr),
          "TCC->AOS MT:%d", static_cast<int32_t>(messageType));

        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
        {
          trace->write(ATC::briefMessageTrace, &logStr[0]);
          writeToLog(ATC::DetailedLog, &logStr[0]);
        }
      }
    }

    /******************************************************************************
    * traceLog
    ******************************************************************************/
    void AbstractRadioMessageIn::traceLog(uint8_t const traceLevel, ATC::LogLevel const logLevel, const char_t* const text) const
    {
      trace->write(traceLevel, text);
      writeToLog(logLevel, text);
    }

    /******************************************************************************
    * parseAdditionalBlocks
    ******************************************************************************/
    bool AbstractRadioMessageIn::parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType)
    {
      // Writing the below trace for removing warning
      trace->write(ATC::detailedTrace, "No adaptation block included in message", static_cast<uint32_t>(messageType));
      trace->write(ATC::detailedTrace, "Unknown adaptation block", static_cast<uint32_t>(adapBlockType));
      trace->write(ATC::detailedTrace, "Size of Buffer passed", buffer->b_s);
      invalidBlockTypeInTCCMessage.setDynamicText(static_cast<uint32_t>(adapBlockType));
      ATC::AbstractEventHandler::corePtr()->reportEvent(invalidBlockTypeInTCCMessage, __FILE__, __LINE__);

      return false;
    }
  }
}
