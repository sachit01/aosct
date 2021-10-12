#ifndef DMIChannel_hpp
#define DMIChannel_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The DMI Channel deals in communicating with the DMI. It provides the interface
*  via VFW. DMI Channel deals only with the interface and treats all the messages
*  as data. It is the DMI Handler which should get the message out of this data
*  and interpret it further.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-12    adgupta     Created
* 2016-08-29    adgupta     Implemented the functionalities for DMI Channel
* 2017-02-03    rquensel    Changed DMI to use CrossCompareOutputChannel
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_sync.h>
#include <vfw_crc.h>
#include "atc_base.hpp"
#include "dmi_message.hpp"
#include "cross_compare_output_channel.hpp"
#include "atc_types.hpp"
#include "channel_config.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace DMICom
  {
    /**
    * To be used to handle errors while using GP list as Out queue.
    */
    class DMIErrorHandler
    {
    public:
      //lint -esym(1714,ATP::DMICom::DMIErrorHandler::report) Lint is wrong, member is referenced
      static void report(const char_t * const str)
      {
        //To report Write to the Console.
        ATC::TraceInterface trace("DMI Channel Out queue", 1U, str);
        trace.write(1U, str);
      }
    };

    /**
    * Types of message header
    */
    static const uint8_t dmiHeaderTypeUnAckMsg = 0x00U; //<! Message which does not need an Ack
    static const uint8_t dmiHeaderTypeAckMsg = 0x10U;   //<! Message which needs an Ack
    static const uint8_t dmiHeaderTypeAck = 0x80U;      //<! Ack message Itself
    
    /**
    * The type of container used for storing the DMI message Out queue for storing
    * messages which need an Ack as response from DMI. This is a GP list used as a queue.
    */
    typedef ATC::GPList<DMIMessage, ATC::dmiAckMsgOutQueueSize, DMIErrorHandler> DMIAckMsgOutQueue;

    /**
    * The class DMIChannel implements the interface defined by the IOComponent
    *
    */
    class DMIChannel : public ATC::IOComponent
    {
    public:

      /** 
      * DMIChannel constructor
      *  
      * @param[in] readChannelName  - Name of vfw channel to read messages from.
      * @param[in] writeChannelName - Name of vfw channel to write messages to.
      * @param[in] dmiChannelName   - Name of the DMI channel which is to be initialised.
      * @param[in] dmiChnlID        - Channel Id of the DMI channel that is to be initialised.
      * @param[in] compShtName      - Short Name for this DMI Channel component
      */  
      DMIChannel(const char_t * const readChanlName, const char_t * const writeChanlName, const char_t* const dmiChannelName, const uint16_t dmiChnlID, const char_t * const compShtName);

      /**
      * Implements the virtual preInit function.
      *
      */
      virtual void preInit(void);

      /**
      * Implements the virtual init function.
      *
      * @return - Returns true when initialization completed successfully
      */
      virtual bool init(void);

      /**
      * Implements the virtual runIn function.
      */
      virtual void runIn(void);

      /**
      * Implements the virtual runOut function.
      */
      virtual void runOut(void);

      /**
      * Read the message from the In queue.
      *
      * @param[out] msg - Message that was read from the In queue.
      * 
      * @return - Returns true if any message read.
      */
      bool readMessage(DMIMessage & msg);

      /**
      * Write the message to the Out queue.
      *
      * @param[in] msg - Message that was to be written to the Out queue to be sent out to DMI.
      * 
      * @return - Returns true if any message written.
      */
      bool writeMessage(DMIMessage const & msg);

      /**
      * Is the DMI connected
      *
      * @return - Returns true if connected.
      */
      bool isConnected() const;

      /**
      * Is the DMI channel active
      *
      * @return - Returns true if active.
      */
      bool isActive() const;

      /**
      * To activate/de-activate the DMI channel.
      *
      * @param[in] activeChannel - true: activate the channel, false: de-activate the channel
      */
      void setActive(const bool activeChannel);

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

    protected:

    private:

      /**
      * Default constructor (disabled).
      */
      DMIChannel();

      /**
      * Trace Level for Console. Only 1 trace level defined Here.
      */
      static const uint8_t traceLevelError = 1U;

      /**
      * DMI messages should start with 7A
      */
      static const uint8_t dmiMsgStart = 0x7AU;

      /**
      * The maximum number of resending of a message before stop attempting to receive an Ack
      */
      static const uint16_t maxRetryCnt = 2U;

      /**
      * Timeout time for Ack message (ms)
      */
      static const int64_t timeoutAckMessage = 2000;

      /**
      * Timeout time for connection to be said to be disconnected (ms)
      */
      static const int64_t timeoutConnected = 5000;

      /**
      * Number of VFW channels used in the DMI channel component.
      */
      static const uint8_t numVfwChannelsDmiChannels = 2U;

      /**
      * array of VFW channel statistics for DMI Channel component
      */
      ATC::ChannelStats chstat[numVfwChannelsDmiChannels];

      /** Maximum number of DMI Channel Event
      */
      static const uint16_t maxDmiChannelEvent = 20U;

      /** Log Event for Dmi Health Status Supervision
      */
      const ATC::Event dmiConnectionStatus;

      /**
      * Transmit mode enumeration
      */
      enum TxMode
      {
        txIdle,       //!< txIdle       - ready to send a new message 
        txWaitingAck, //!< txWaitingAck - message sent, waiting for the corresponding Ack
        txResend      //!< txResend     - No Ack for sent message received, need to resend
      };

      /**
      * read from the DMI channel
      *
      * @return - Returns true if read succeeded from the DMI read channel.
      */
      bool readFromChannel();

      /**
      * Reads all available data on the input channel and discards them.
      */
      void readFromChannelAndDiscard();

      /**
      * write the data from outbuf buffer to the DMI write channel
      */
      void writeToChannel();

      /**
      * push messages to In queue increase the in queue count
      *
      * @param[in] msg - message that needs to be pushed to In queue
      * 
      * @return - Returns true if push is successful.
      */
      bool pushMsgToInQueue(const DMIMessage & msg);

      /**
      * Pop the No Ack out message queue return the 1st message
      *
      * @param[out] msg - Message popped out of the Queue
      *
      * @return - Returns true if available message is returned.
      */
      bool popNoAckOutQueue(DMIMessage & msg);

      /**
      * pack the message to be sent out to the DMI.
      *
      * @param msg - DMI message containing required data to be packed to out buffer
      *
      * @param isAckMsg - Is the data to be packed is an Ack message: 
      *                   Ack message does not have a data field and message type
      *
      * @return - Returns the number of messages written to out buf while packing
      */
      uint16_t packMsg(DMIMessage const & msg, const bool isAckMsg);

      /**
      * Initialize the Cross compare
      */
      void initCrossCompare() const;

      /**
      * extract the message received from DMI onto the object member variables.
      *
      * @param msg[out] - message after extracting it from In buf buffer
      *
      * @return - Returns true if message was extracted and validated successfully
      */
      bool extractMessage(DMIMessage &msg);

      /**
      * Send the waiting Ack back to DMI
      *
      * @param AckNumber[in] - Acknowledgment Number of Ack that is sent to DMI.
      */
      void sendAck(const uint8_t AckNumber);

      /**
      * calculate the CRC over a block of message
      *
      * @param[in] start - Pointer from where the CRC calculation should start
      *
      * @param[in] length - The length of the buffer from start pointer location over which the CRC is to be calculated.
      *
      * @return - Returns the Calculated CRC value
      */
      uint32_t calculateCRC(const uint8_t* const start, uint16_t const length) const;

      /**
      * Transmit mode
      */
      TxMode txMode;

      /**
      * Unique DMI Channel ID
      */
      const uint16_t dmiChannelID;

      /**
      * Name of channel to read messages from
      */
      char_t readChannelName[VFW_CH_NAME_MAX_Z];

      /**
      * Name of channel to write messages to
      */
      char_t writeChannelName[VFW_CH_NAME_MAX_Z];

      /**
      * Channel handle returned by vfwChannelOpenWrite
      */
      Support::CrossCompareOutputChannel crossCompareChannelWriteDesc;

      /**
      * Synchronized channel to read via VFW
      */
      VFW_SyncChannel syncChannelReadDesc;

      /**
      * DMI message in queue
      */
      DMIMessage dmiMsgInQueue[ATC::dmiMsgInQueueSize];

      /**
      * DMI message out queue needing Ack.
      * This stores messages which "need" an Ack as response from DMI.
      * This is of GP list type. This is because there may be a case where not
      * all messages are written to DMI and thus cause congestion/message drop. Can't use array as queue.
      */
      DMIAckMsgOutQueue dmiAckMsgOutQueue;

      /**
      * DMI message out queue do not needing any Ack.
      * This stores messages which do not need an Ack as response from DMI.
      */
      DMIMessage dmiNoAckMsgOutQueue[ATC::dmiNoAckMsgOutQueueSize];

      /**
      * Initialize flag
      */
      bool initDone;

      /**
      * The Ack number for which the Ack message is to be sent out.
      * This will be 0 if no Ack is to be sent.
      */
      uint8_t sendAckNumber;

      /**
      * The index of Out queue for messages which do not need an Ack from where the message is to be popped out
      */
      uint8_t popNoAckOutQueueIdx;

      /**
      * The index of In queue from where the message is to be popped out
      */
      uint8_t popInQueueIdx;

      /**
      * The index of Out queue for messages which do not need an Ack from where the message is to be pushed In
      */
      uint8_t pushNoAckOutQueueIdx;

      /**
      * The index of In queue from where the message is to be pushed In
      */
      uint8_t pushInQueueIdx;

      /**
      * Ack number of the message whose Ack is to be received(waiting for this Ack number)
      */
      uint8_t waitingAckNumber;

      /**
      * This is the message that is resent when Ack is not received on time.
      */
      DMIMessage waitingResendMsg;

      /**
      * Number of retries(resends) attempted count before resending a new message
      */
      uint8_t retryCnt;

      /**
      * If non-zero, the time when we started waiting for an Ack message
      */
      int64_t waitForAckStartTime;

      /**
      * If non-zero, the time when we last received a message
      */
      int64_t lastMessageReceivedTime;

      /**
      * The current count of number of messages in the out buffer
      */
      uint16_t outBufCnt;

      /**
      * The current count of number of messages in the in buffer
      */
      uint16_t inBufCnt;

      /**
      * Is the DMI connected - if any valid message is received from the DMI.
      * We set this as true.
      * Any incorrect message(fails validity) should set this false.
      */
      bool connected;

      /**
      * DMI channel active flag - DMI in the inactive cabin should be inactive
      * thus, the DMI channel should also be inactive.
      * We set this(DMI channel active flag) as true when the connected DMI is active.
      */
      bool active;

      /**
      * The In buffer that receives data from DMI
      */
      uint8_t inBuf[ATC::maxDMIMessageSize + 2U]; // increased by 2 to avoid (erroneous) lint error

      /**
      * The Out buffer from where data is sent to DMI
      */
      uint8_t outBuf[ATC::maxDMIMessageSize];

      /**
      * Index count to trace through the buffer(while reading or writing)
      */
      uint16_t bufIdx;

    };
  }
}

#endif
