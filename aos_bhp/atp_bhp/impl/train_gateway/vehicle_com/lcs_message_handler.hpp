#ifndef LCSMessageHandler_hpp
#define LCSMessageHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines LCSMessageHandler class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-21    marlundg    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include <vfw_sync.h>
#include "event.hpp"
#include "lcs_message_common.hpp"
#include "abstract_lcs_message_in.hpp"
#include "abstract_lcs_message_out.hpp"
#include "lcs_message_in_train_status.hpp"
#include "lcs_message_out_status.hpp"
#include "cross_compare_output_channel.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace TG
  {
    /**
    * LCS Message Handler Class
    */
    class LCSMessageHandler
    {
    public:

      /** Constructor
      *
      *  @param[in] readChannelName  Name of vfw channel to read lcs-messages from
      *  @param[in] writeChannelName Name of vfw channel to write lcs-messages to
      */
      LCSMessageHandler(const char_t * const readChannelName, const char_t * const writeChannelName );

      /**
      * Register vfw channels in sync handler.
      *
      */
      void preInit(void);

      /**
      * Initializes the Creators/Parsers for the LCS Message Handler
      *
      */
      bool init(void);

      /**
      * Parse, validate and publish incoming messages
      *
      */
      void runIn(void);

      /**
      * Collect, validate and transmit outgoing messages
      */
      void runOut(void);

      /**
      * Access-function for LCS train-status
      *
      *  @param[out] status   The info associated with the LcsTrainStatus
      *
      *  @return true if any LcsTrainStatus is published
      */
      bool getTrainStatus(LCSTrainStatusType& trainStatus) const;

      /**
      * Access-function for LCS train-composition
      *
      *  @param[out] status   The info associated with the LcsTrainComposition
      *
      *  @return true if any LcsTrainComposition is published
      */
      bool getECPBTrainComposition(ECPBTrainCompositionType& ecpbTrainComposition) const;

      /**
      * Access-function for Rcl status
      *
      *  @param[out] handlingDone   The info associated with the Handling Done
      *
      *  @return true if any LcsRclStatus is published
      */
      bool getHandlingDone(HandlingDoneType& handlingDone) const;
      
      /**
      * Get connection status towards LCS
      *
      * @return true if connected to LCS
      */
      bool connectedLCS() const;

      /**
      * To handle console calls for the DMI channel.
      * This functions parses the arguments searches for the "help"  or any other Console
      * component specific command calls and handles it. Returns true if completely handled
      * else returns false. returning false will let other components handle the call. help always returns false.
      *
      * @param[in] argc - Number of arguments in the argument array argv
      *
      * @param[in] argv - Arguments array
      *
      * @return - returns true if handled successfully, except "help"(it always returns false)
      */
      bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

      /**
      * Access-function for last sent UTC time to LCS
      *   
      * @return last sent UTC time to LCS
      */
      uint32_t getLastSentUTCTimeToLCS() const;
      
      /**
      * Access-function for last received UTC time from LCS
      *
      * @return last received UTC time from LCS
      */
      uint32_t getLastReceivedUTCTimeFromLCS() const;

    private:

      /**
      * Default constructor, not to be used
      */
      LCSMessageHandler();

      /**
      * Initialize the Cross compare
      */
      void initCrossCompare() const;

      /**
      * To increment the index of outgoing Message.
      * @param[in] messageOutCreatorIter - current index of outogoing message. 
      *
      * @return - returns the next index of outgoing message.
      */
      uint8_t incMessageOutCreatorIter(const uint8_t messageOutCreatorIter) const;

      /**
      * Validation of incoming message failed
      */
      const ATC::Event validationIncomingMessageFailed;

      /**
      * Validation of outgoing message failed
      */
      const ATC::Event validationOutgoingMessageFailed;

      /**
      * Invalid messagetype received
      */
      const ATC::Event invalidMessageType;

      /**
      * Parser for this messagetype is NULL
      */
      const ATC::Event parserNullPtr;

      /**
      * Message(s) from LCS has time deviation
      */
      const ATC::Event timeDeviationResponseFromLCS;
      
      /**
      * Message(s) from LCS has recovered from time deviation
      */
      const ATC::Event timeDeviationRecoveredFromLCS;

      /**
      * Missing message
      */
      const ATC::Event missedMessageFromLCS;

      /**
      * Parser for this message type is not implemented
      */
      const ATC::Event parserNotImplemented;

      /**
      * Connection lost with LCS, current brakesystem in use is ECPB
      */
      const ATC::Event lostConnectionWithLCSandECPB;

      /**
      * Connection lost with LCS
      */
      const ATC::Event lostConnectionWithLCS;

      /**
      * Connection established with LCS
      */
      const ATC::Event establishedConnectionWithLCS;

      /**
      * Validation of CRC failed
      */
      const ATC::Event validationCRCFailed;

      /**
      * Invalid message value received
      */
      const ATC::Event invalidMessageValue;

      /**
      * Number of outgoing messages.
      */
      static const uint8_t numOfAosLcsMessage = 7U;

      /**
      * The container of the pointers to parsers for incoming messages
      */
      AbstractLCSMessageIn  *messagesInParsers[(static_cast<uint16_t>(LCSMTypeLCStoAOSMax) - static_cast<uint16_t>(LCSMTypeTrainStatusMessage))];

      /**
      * The container of the pointers to creators of outgoing messages
      */
      AbstractLCSMessageOut *messagesOutCreators[numOfAosLcsMessage];

      /** Name of channel to read messages from
      */
      char_t lcsReadChannelName[VFW_CH_NAME_MAX_Z];

      /** Name of channel to write messages to
      */
      char_t lcsWriteChannelName[VFW_CH_NAME_MAX_Z];

      /** Channel for reading EMP messages
      */
      VFW_SyncChannel syncChannelReadDesc;

      /** Cross compare channel for writing EMP messages
      */
      Support::CrossCompareOutputChannel crossCompareWriteChannel;

      /**
      * Trace Interface to be used
      */
      ATC::TraceInterface *trace;

      /**
      * EMP message to be sent or received
      */
      EmpMsg empMessage;

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * Number of VFW channels used in the LCS message handler.
      */
      static const uint8_t numVfwChannelsLCSMessage = 2U;

      /**
      * array of VFW channel statistics for LCS message handler.
      */
      ATC::ChannelStats chstat[numVfwChannelsLCSMessage];

      /**
      * EmpParsing status for previous message
      */
      EMPParseError oldEmpStatus;

      /**
      * Connection status towards LCS
      */
      bool connectedToLcs;

      /**
      * Starting time for LCS watchdog (reference time, ms)
      */
      int64_t lcsWatchdogStartTime;

      /**
      * Timeout for LCS watchdog (ms)
      */
      static const int64_t lcsWatchdogTimeout = 5000;

      /**
      * Flag if message from LCS has too high time deviation
      */
      bool incorrectMsgTime;
   
      /**
      * Last sent UTCTime to LCS in the EMP Message Header
      */
      uint32_t lastSentUTCTimeToLCS;
      
      /**
      * Last received UTCTime from LCS in the EMP Message Header
      */
      uint32_t lastReceivedUTCTimeFromLCS;
    };
  }
}

#endif
