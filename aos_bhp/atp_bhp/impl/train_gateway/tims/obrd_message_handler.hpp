#ifndef OBRD_Message_Handler_hpp
#define OBRD_Message_Handler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines the OBRD Interface class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-25    skothiya    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "vfw_sync.h"
#include "event.hpp"
#include "trace_interface.hpp"
#include "obrd_message_common.hpp"
#include "obrd_message_in_protocol_version.hpp"
#include "obrd_message_in_obrd_unit_status.hpp"
#include "obrd_message_out_protocol_version.hpp"
#include "obrd_message_out_reject_message.hpp"
#include "cross_compare_output_channel.hpp"

namespace ATP
{
  namespace TG
  {
    /**
    * Handles communication with the OBRD subsystem.
    */
    class OBRDMessageHandler
    {

    public:

      /** Constructor
      *
      *  @param[in] readChannelName  Name of vfw channel to read OBRD messages from dispatcher
      *  @param[in] writeChannelName Name of vfw channel to write OBRD messages to dispatcher
      *  @param[in] trace_           The component's trace object
      */
      OBRDMessageHandler(const char_t* const readChannelName, const char_t* const writeChannelName, ATC::TraceInterface* const trace_);

      /**
      * Register vfw channels in sync handler.
      *
      */
      void preInit();

      /**
      * Initializes the Creators/Parsers for the LCS Message Handler
      *
      */
      bool init();

      /**
      * Parse, validate and publish incoming messages
      *
      */
      void runIn();

      /**
      * Collect, validate and transmit outgoing messages
      */
      void runOut();

      /**
      * Fetch get OBRD Unit Status Report
      *
      *  @param[out] report   Reference to OBRD Unit Status Report
      *  @return Flag to indicate if OBRD Unit Status Report Received in current cycle
      */
      bool getStatusReport(OBRDUnitStatusReport &report) const;


    private:

      /**
      * Default constructor, not to be used
      */
      OBRDMessageHandler();

      /** Extract message from characters in @ref inBuf
      *
      *  @param[out] msg Reference to the extracted message
      *  @return true if message extracted and validated successfully
      */
      bool extractMessage(OBRDMessage& msg);

      /**
      * Set the Header fields in outgoing message
      *
      * @param[in] pCreator  OBRD Message Data
      * @param[out] msg   OBRD Message Reference
      *
      * @return true if the message was assembled successfully
      */
      bool assembleMessageData(OBRDMessageOut* const pCreator, OBRDMessage& msg) const;

      /**
      * Calculates the CRC of a message.
      *
      *  @param[in] start  Pointer to start of buffer to calculate CRC of
      *  @param[in] length Length of buffer
      *  @return the CRC
      */
      uint64_t calculateCRC(const uint8_t *start, const uint16_t length) const;

      /**
      *  Pack a RadioMessage to an outgoing buffer (in network order)
      *
      *  @param[in]  msg   Reference to the message
      *  @param[out] outBuffer Pointer to the outgoing buffer
      *  @param[in]  outBufferSize    Size of the outgoing buffer
      *  @return the valid length of outBuf
      */
      uint16_t packMessage(const OBRDMessage& msg, uint8_t * const outBuffer, uint32_t const outBufferSize) const;

      /** Find start of telegram.
      *  Scans @ref inBuf for the next occurrence of STX. Increases @ref inBufReadIndex until
      *  STX is found or until end of the buffer is reached (inBufIndex).
      *
      *  @param[out] availableBytes The number of bytes available in inBuf, including the STX
      *  @return true, if start of the telegram (STX) was found
      */
      bool findStartOfTelegram(uint16_t & availableBytes);


      /**
      * Validation of incoming message failed
      */
      const ATC::Event validationIncomingMessageFailed;

      /**
      * Connection established with OBRD BOS
      */
      const ATC::Event establishedConnectionWithOBRD;

      /**
      * Connection timeout with OBRD BOS
      */
      const ATC::Event lostConnectionWithOBRD;

      /**
      * Wrong type of message received in OBRD interface
      */
      const ATC::Event wrongOBRDTypeMessageRecvd;

      /**
      * Wrong protocol version received in OBRD interface
      */
      const ATC::Event wrongOBRDProtocolVersionRecvd;

      /**
      * Parser for this message type is NULL
      */
      const ATC::Event parserNullPtr;

      /**
      * Parser for incoming message
      */
      OBRDMessageInProtocolVersion protocolVersionParser;

      /**
      * Parser for incoming message
      */
      OBRDMessageInUnitStatus unitStatusParser;

      /**
      * Creator of outgoing message
      */
      OBRDMessageOutProtocolVersion protocolVersionCreator;

      /**
      * Creator of outgoing message
      */
      OBRDMessageOutRejectMessage rejectMessageCreator;

      /** Name of channel to read messages from
      */
      char_t obrdReadChannelName[VFW_CH_NAME_MAX_Z];

      /** Name of channel to write messages to
      */
      char_t obrdWriteChannelName[VFW_CH_NAME_MAX_Z];

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
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * Number of VFW channels used in the OBRD message handler.
      */
      static const uint8_t numVfwChannelsOBRDMessage = 2U;

      /**
      * Array of VFW channel statistics for OBRD message handler.
      */
      ATC::ChannelStats channelStatistics[numVfwChannelsOBRDMessage];

      /**
      * Time when last message was received, for detecting connection loss (reference time, ms)
      */
      int64_t lastMessageReceivedTime;

      /**
      * Timeout for detecting connection loss (ms)
      */
      int64_t connectionLossTimeout;

      /** Initial value for CRC64 calculation
      *   Target does not support 'ULL', therefore a workaround with shift operation.
      */
      static const uint64_t crcInitValue = (static_cast<uint64_t>(0x989626B1UL) << 32) + 0xE8F2D6F0UL;

      /**
      *  isProtocolVersionMatched boolean flag to indicate protocol matched
      */
      bool protocolVersionMatched;

      /**
      *  sendProtocolVersionMessage boolean flag to indicate send protocol version
      */
      bool sendProtocolVersionMessage;

      /**
      *  sendRejectMessage boolean flag to indicate send Message Rejection
      */
      bool sendRejectMessage;

      /**
      *  Rejection reason
      */
      RejectReason rejectionReason;

      /**
      * Flag to indicate if an OBRD Unit Status Report was received in current cycle (see @ref statusReport)
      */
      bool isUnitStatusReportRecv;

      /**
      * The OBRD Unit Status Report (see @ref isUnitStatusReportRecv)
      */
      OBRDUnitStatusReport statusReport;

      /**
      * Connection status towards OBRD
      */
      bool connectedToOBRD;

      /**
      * Connection Time out
      */
      bool connectionTimeoutWithOBRD;

      /** Buffer to store received bytes before extracting the message
      */
      uint8_t  inBuf[ATC::maxOBRDMsgSize];

      /** Index in @ref inBuf where to store the next received byte
      */
      uint16_t  inBufWriteIndex;

      /** Index in @ref inBuf from where to start searching for a new message
      */
      uint16_t  inBufReadIndex;

    };
  }
}

#endif
