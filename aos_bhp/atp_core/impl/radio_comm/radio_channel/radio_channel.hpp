#ifndef RadioChannel_hpp
#define RadioChannel_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*
*
******************************************************************************/


/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-01-07    bhermans    Created
* 2016-03-01    bhermans    Removed AdaptedRadioChannel
* 2016-03-03    bhermans    Introduced namespace RadioCom
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-06-09    akushwah    Radio Channel implementation
* 2016-06-16    akushwah    Added Access function getChannelId()
* 2016-08-04    adgupta     Updated to add init flag to prevent multiple initialization
* 2016-10-10    spandita    Updated the functional declaration for setting tsender time bug
* 2017-01-27    rquensel    Changed to use CrossCompareOutputChannel
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_sync.h>
#include <vfw_crc.h>
#include "atc_base.hpp"
#include <radio_message.hpp>
#include "cross_compare_output_channel.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace RadioCom
  {
    /* Radio Channel Ids
    */
    static const uint16_t radioChannelId0 = 0U; //!< Radio Channel Id for responding to first requesting TCC 
    static const uint16_t radioChannelId1 = 1U; //!< Radio Channel Id for TCC1 (Central)
    static const uint16_t radioChannelId2 = 2U; //!< Radio Channel Id for TCC2 (Region)
    static const uint16_t radioChannelId3 = 3U; //!< Radio Channel Id for TCC3 (Region)
    static const uint16_t radioChannelRegionBroadcast = 255U; //!< Radio Channel Broadcast to all connected region TCCs

    /** Maximum Length of Incoming/Outgoing Message Data Chunks
    */
    static const uint16_t maxLengthOfMessageChunks = 4000U;

    /** One instance of RadioChannel handles reception of radio-messages arriving on one
    *  vfw channel and sending of radio-messages on another vfw channel.
    */
    class RadioChannel : public ATC::IOComponent
    {
    private:

      /** Transmit mode enum
      */
      enum TxMode
      {
        txIdle,     //!< txIdle    - ready to send a new message 
        txSending,  //!< txSending - message sent, waiting for message from peer 
        txResend    //!<txResend  - message from peer received but timestamps indicate that a resend is necessary 
      };

      /** Maximum Number of Message chunks
      */
      static const uint8_t maxMessageChunks = 3U;

      /** Maximum number of Radio Channel Event
      */
      static const uint16_t maxRadioChannelEvent = 20U;

      /** Max  no of messages to read and queue in one cycle
      */
      static const uint32_t   radioMessageInQueueSize = 5U;

      /** Max  no of messages to write and queue in one cycle
      */
      static const uint32_t   radioMessageOutQueueSize = 5U;

      /** Default value of tRef, when no previous time-stamp is available
      *  E.g. at startup
      */
      static const uint16_t   defaultTRef = 0x5665U;

      /** Initial value for CRC64 calculation
      *   Target does not support 'ULL', therefore a workaround with shift operation.
      */
      static const uint64_t crcInitValue = (static_cast<uint64_t>(0x989626B1UL) << 32) + 0xE8F2D6F0UL;

      /**
      * Number of VFW channels used in the Radio channel component.
      */
      static const uint8_t numVfwChannelsRadioChannels = 2U;

      /** lastTimeMsgRecvFrmTCC variable to last store time at which radio channel has received the message.
      */
      int64_t lastTimeMsgRecvFrmTCC;

      /** lastTimeMsgSendToTCC variable to last store time at which radio channel has send the message.
      */
      int64_t lastTimeMsgSendToTCC;

      /** Unique radio Channel Id
      */
      const uint16_t radioChannelID;

      /**
      * array of VFW channel statistics for Radio Channel component
      */
      ATC::ChannelStats chstat[numVfwChannelsRadioChannels];

      /** Region Id
      *  Read regionId received from TCC per message
      */
      uint8_t   regionId;

      /**
      * Indicates whether this radio channel is connected
      */
      bool connected;

      /** Name of channel to read messages from
      */
      char_t radioReadChannelName[VFW_CH_NAME_MAX_Z];

      /** Name of channel to write messages to
      */
      char_t radioWriteChannelName[VFW_CH_NAME_MAX_Z];

      /** Channel handle returned by vfwSyncAddChannel
      */
      VFW_SyncChannel syncChannelReadDesc;

      /** Channel handle returned by vfwChannelOpenWrite
      */
      Support::CrossCompareOutputChannel crossCompareChannelWriteDesc;


      /** Buffer to store received bytes before extracting the message
      */
      uint8_t  inBuf[ATC::maxRadioMessageSize];

      /** Index in buffer where to store the next received byte
      */
      uint16_t  inBufIndex;

      /** Index in inbuf from where to start searching for a new message
      */
      uint16_t  inBufIndexStart;

      /** Queue to store received messages in
      */
      RadioMessage messageInQueue[radioMessageInQueueSize];

      /** Index to where in messageInQueue the next message will be pushed
      */
      uint8_t pushInQueueIndex;

      /** Index from where the next (the oldest) message will be popped from messageInQueue
      */
      uint8_t popInQueueIndex;

      /** Queue to store outgoing messages (waiting to be sent) in
      */
      RadioMessage messageOutQueue[radioMessageOutQueueSize];

      /** Index to where in OutQueue the next outgoing message will be pushed
      */
      uint8_t pushOutQueueIndex;

      /** Index from where the next (the oldest) message will be popped from the messageOutQueue
      */
      uint8_t popOutQueueIndex;

      /** Timestamp tSender that was last transmitted to TCC
      */
      uint16_t prevTx_tSender;

      /** Timestamp tSender that was 2nd last transmitted to TCC
      */
      uint16_t prevPrevTx_tSender;

      /** Timestamp tSender that was last received from TCC
      */
      uint16_t prevRx_tSender;

      /** Outbuf with last message sent
      *  Converted to network order
      */
      uint8_t  outBuf[ATC::maxRadioMessageSize];

      /** Length of outBuf with last message sent
      */
      uint16_t outBufLen;

      /** Too many messages in One Cycle Error
      */
      ATC::Event messageInQueueError;

      /** Message is available to write but something failed internally
      */
      ATC::Event writeMessageError;

      /** Illegal length in the header
     */
      ATC::Event incomingMessageLengthError;
      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /** Transmit mode
      */
      TxMode txMode;

      /** Push an incoming validated message to the inqueue
      *
      * @param[in] msg Message to push to InQueue
      * @return true if message pushed successfully
      */
      bool pushMessageToInQueue(const RadioMessage & msg);


      /** Validate message timestamp
      *
      * @param[in]  tRef              The value of tRef in the message to be validated
      * @param[out] resendLastMessage Will indicate whether last message needs to be resent
      * @param[in]  tRecSender        The value of tSender in the message to be validated
      *
      * @return true if time-stamp validated successfully
      */
      virtual bool isValidTimeStamp(uint16_t const tRef, bool & resendLastMessage, uint16_t const tRecSender);

      /** Validate message in buffer and extract as a RadioMessage
      *
      *  Note! The extracted message attributes are converted to host order except of the
      *  variable length data-part which is still in network byte order.
      *
      * @param[in] buffer             Pointer to where the raw message-buffer is located
      * @param[in] msg                Where the extracted message is stored
      * @param[in] stx                The byte expected to be equal to STX
      * @param[in] extractedCRC       Extracted CRC(Cyclic Redundancy Check code)
      * @param[in] numOfMessageChunks Number of message chunks included in message
      *
      * @return true if message validated successfully
      */
      bool validateMessage(const uint8_t * const buffer, const RadioMessage & msg, uint8_t const stx,
        const uint64_t extractedCRC[maxMessageChunks], const uint8_t numOfMessageChunks);

      /** Read incoming characters and append to inBuf
      *
      *  @return the number of characters received
      */
      int32_t readFromChannel();

      /** Write outgoing characters to channel
      *
      * @param[in] outBuffer    Where the outgoing message is stored
      * @param[in] outBufferLen Length of outBuf
      */
      void writeToChannel(const uint8_t * const outBuffer, uint16_t const outBufferLen);

      /** Find start of telegram.
      *  Scans inBuf for the next occurrence of STX. Increases inBufIndexStart until
      *  STX is found or until end of the buffer is reached (inBufIndex).
      *
      *  @param[out] availableBytes The number of bytes available in inBuf, including the STX
      *  @return true, if start of the telegram (STX) was found
      */
      bool findStartOfTelegram(uint16_t & availableBytes);

      /** Calculate Cyclic Redundancy Code
      *
      *  @param[in] start  Pointer to start of buffer to calculate CRC of
      *  @param[in] length Length of buffer
      *  @return the CRC
      */
      virtual uint64_t calculateCRC(const uint8_t* start, uint16_t const length);

      /** Extract message from characters in inBuf
      *
      *  @param[out] msg Reference to the extracted message
      *  @return true if message extracted and validated successfully
      */
      bool extractMessage(RadioMessage & msg);

      /** Pop message from outQueue
      *
      *  @param[out] msg Reference to the message
      *  @return true if any message was available
      */
      bool popMessageFromOutQueue(RadioMessage & msg);

      /** Pack message
      *  Pack a RadioMessage to an outgoing buffer (in network order)
      *
      *  @param[out] outBuffer Pointer to the outgoing buffer
      *  @param[in]  msg    Reference to the message
      *  @return the valid length of outBuf
      */
      uint16_t packMessage(uint8_t * const outBuffer, const RadioMessage & msg);

      /** Generate a new time-stamp to be used as tSender in outgoing message
      */
      uint16_t generateTimeStamp() const;

      /** Handle transmit time-stamps
      *  Create,assign and save time-stamps
      */
      void handleTxTimeStamps(RadioMessage & msg);

      /** Set the Header i.e assign Id, SiteId and regionId for the
      *  transmitted message
      */
      void setOutBufHeader(RadioMessage & msg) const;

      /**
      * Default Constructor
      * Declare constructor as private in order to prevent illegal use.
      */
      RadioChannel();

      /**
      * Function to write in the NJRU log ,the channel name being read from
      */
      void logChannelRead(const char_t* const channelName) const;

    public:

      /**
      * To handle console calls for the DMI channel.
      * This functions parses the arguments searches for the "help"  or any other Console
      * component specific command calls and handles it. Returns true if completeley handled
      * else returns false. returning false will let other componets handle the call. help always returns false.
      *
      * @param[in] argc - Number of arguments in the argument array argv
      *
      * @param[in] argv - Arguments array
      *
      * @return - returns true if handled successfully, except "help"(it always returns false)
      */
      virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

      /** RadioChannel constructor
      *
      *  @param[in] readChannelName  Name of vfw channel to read messages from
      *  @param[in] writeChannelName Name of vfw channel to write messages to
      *  @param[in] radChnlID Radio Channel ID
      *  @param[in] compShtName Short Name for this Channel
      */
      RadioChannel(const char_t * const readChannelName, const char_t * const writeChannelName, const uint16_t radChnlID, const char_t * const compShtName);

      /** Get Radio Channel Id
      *
      *  @return the unique Id of Radio Channel.
      */
      uint16_t getChannelId() const;

      /** Get status of channel
      *
      *  @return true if a radio-message has been received within a timeout
      */
      bool getConnected() const;

      /** Get no of messages in out-queue
      *  With the current protocol the size of the out-queue == 1
      *  @return 0 if out-queue is empty or 1 when it is occupied
      */
      uint8_t getTxPending() const;

      /** Read message
      *
      *  @param[out] msg The available radio-message in the channel::messageInQueue
      *  @return true if message available
      */
      bool readMessage(RadioMessage & msg);

      /** Peek message
      *
      *  Same as readMessage but leaves the message available for readMessage
      *
      *  @param[out] msg The available radio-message in the channel::messageInQueue
      *  @return true if message available
      */
      bool peekMessage(RadioMessage & msg) const;

      /** Append a radio-message to the messageOutQueue
      *
      *  @param[in] msg
      *  @return true if message successfully queued
      */
      bool writeMessage(const RadioMessage & msg);


      /** Check if radio-channel is associated with Central TCC
      *
      *  @return true if associated with central TCC
      */
      virtual bool isCentral() const;

      /**
      * Implements the virtual preInit function.
      *
      * Register vfw channels in sync handler.
      */
      virtual void preInit(void);

      /** Init Radio Channel resources
      *  Open vfw channels to read from/write to
      *  Init variables
      *
      * @return Returns true when initialization completed
      */
      virtual bool init();

      /** Owner or scheduler shall call runIn() once per activation.
      *  Reads all incoming bytes and extracts all messages
      */
      virtual void runIn();

      /** Owner or scheduler shall call runOut() once per activation.
      *  Packs all pending messages as a stream of bytes in network order and writes to the vfw channel
      */
      virtual void runOut();

      /**
      * Test the predefined crc specified in FFFIS TCC-AOS 4.2.2 for string "123456789"
      *
      * @return true if pass
      */
      virtual bool selfTest(void);

      /** Init cross compare registers internal persistent values for cross-compare
      *
      */
      virtual void initCrossCompare() const;
    };

    typedef RadioChannel* RadioChannelPtr;
  }

}

#endif
