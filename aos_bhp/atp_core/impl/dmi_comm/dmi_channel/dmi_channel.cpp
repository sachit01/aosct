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
* 2017-01-27    rquensel    Changed DMI to use CrossCompareOutputChannel
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_identity.h>
#include <stdio.h>
#include "dmi_channel.hpp"
#include "abstract_log_handler.hpp"
#include "atc_util.hpp"
#include "abstract_console.hpp"
#include <vfw_string.h>
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include <vfw_checkpoints.h>
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
  namespace DMICom
  {
      static const uint32_t crcTable[256]  =
       {0x0U,
        0x1edc6f41U,        0x3db8de82U,        0x2364b1c3U,        0x7b71bd04U,        0x65add245U,
        0x46c96386U,        0x58150cc7U,        0xf6e37a08U,        0xe83f1549U,        0xcb5ba48aU,
        0xd587cbcbU,        0x8d92c70cU,        0x934ea84dU,        0xb02a198eU,        0xaef676cfU,
        0xf31a9b51U,        0xedc6f410U,        0xcea245d3U,        0xd07e2a92U,        0x886b2655U,
        0x96b74914U,        0xb5d3f8d7U,        0xab0f9796U,        0x05f9e159U,        0x1b258e18U,
        0x38413fdbU,        0x269d509aU,        0x7e885c5dU,        0x6054331cU,        0x433082dfU,
        0x5deced9eU,        0xf8e959e3U,        0xe63536a2U,        0xc5518761U,        0xdb8de820U,
        0x8398e4e7U,        0x9d448ba6U,        0xbe203a65U,        0xa0fc5524U,        0x0e0a23ebU,
        0x10d64caaU,        0x33b2fd69U,        0x2d6e9228U,        0x757b9eefU,        0x6ba7f1aeU,
        0x48c3406dU,        0x561f2f2cU,        0x0bf3c2b2U,        0x152fadf3U,        0x364b1c30U,
        0x28977371U,        0x70827fb6U,        0x6e5e10f7U,        0x4d3aa134U,        0x53e6ce75U,
        0xfd10b8baU,        0xe3ccd7fbU,        0xc0a86638U,        0xde740979U,        0x866105beU,
        0x98bd6affU,        0xbbd9db3cU,        0xa505b47dU,        0xef0edc87U,        0xf1d2b3c6U,
        0xd2b60205U,        0xcc6a6d44U,        0x947f6183U,        0x8aa30ec2U,        0xa9c7bf01U,
        0xb71bd040U,        0x19eda68fU,        0x0731c9ceU,        0x2455780dU,        0x3a89174cU,
        0x629c1b8bU,        0x7c4074caU,        0x5f24c509U,        0x41f8aa48U,        0x1c1447d6U,
        0x02c82897U,        0x21ac9954U,        0x3f70f615U,        0x6765fad2U,        0x79b99593U,
        0x5add2450U,        0x44014b11U,        0xeaf73ddeU,        0xf42b529fU,        0xd74fe35cU,
        0xc9938c1dU,        0x918680daU,        0x8f5aef9bU,        0xac3e5e58U,        0xb2e23119U,
        0x17e78564U,        0x093bea25U,        0x2a5f5be6U,        0x348334a7U,        0x6c963860U,
        0x724a5721U,        0x512ee6e2U,        0x4ff289a3U,        0xe104ff6cU,        0xffd8902dU,
        0xdcbc21eeU,        0xc2604eafU,        0x9a754268U,        0x84a92d29U,        0xa7cd9ceaU,
        0xb911f3abU,        0xe4fd1e35U,        0xfa217174U,        0xd945c0b7U,        0xc799aff6U,
        0x9f8ca331U,        0x8150cc70U,        0xa2347db3U,        0xbce812f2U,        0x121e643dU,
        0x0cc20b7cU,        0x2fa6babfU,        0x317ad5feU,        0x696fd939U,        0x77b3b678U,
        0x54d707bbU,        0x4a0b68faU,        0xc0c1d64fU,        0xde1db90eU,        0xfd7908cdU,
        0xe3a5678cU,        0xbbb06b4bU,        0xa56c040aU,        0x8608b5c9U,        0x98d4da88U,
        0x3622ac47U,        0x28fec306U,        0x0b9a72c5U,        0x15461d84U,        0x4d531143U,
        0x538f7e02U,        0x70ebcfc1U,        0x6e37a080U,        0x33db4d1eU,        0x2d07225fU,
        0x0e63939cU,        0x10bffcddU,        0x48aaf01aU,        0x56769f5bU,        0x75122e98U,
        0x6bce41d9U,        0xc5383716U,        0xdbe45857U,        0xf880e994U,        0xe65c86d5U,
        0xbe498a12U,        0xa095e553U,        0x83f15490U,        0x9d2d3bd1U,        0x38288facU,
        0x26f4e0edU,        0x0590512eU,        0x1b4c3e6fU,        0x435932a8U,        0x5d855de9U,
        0x7ee1ec2aU,        0x603d836bU,        0xcecbf5a4U,        0xd0179ae5U,        0xf3732b26U,
        0xedaf4467U,        0xb5ba48a0U,        0xab6627e1U,        0x88029622U,        0x96def963U,
        0xcb3214fdU,        0xd5ee7bbcU,        0xf68aca7fU,        0xe856a53eU,        0xb043a9f9U,
        0xae9fc6b8U,        0x8dfb777bU,        0x9327183aU,        0x3dd16ef5U,        0x230d01b4U,
        0x0069b077U,        0x1eb5df36U,        0x46a0d3f1U,        0x587cbcb0U,        0x7b180d73U,
        0x65c46232U,        0x2fcf0ac8U,        0x31136589U,        0x1277d44aU,        0x0cabbb0bU,
        0x54beb7ccU,        0x4a62d88dU,        0x6906694eU,        0x77da060fU,        0xd92c70c0U,
        0xc7f01f81U,        0xe494ae42U,        0xfa48c103U,        0xa25dcdc4U,        0xbc81a285U,
        0x9fe51346U,        0x81397c07U,        0xdcd59199U,        0xc209fed8U,        0xe16d4f1bU,
        0xffb1205aU,        0xa7a42c9dU,        0xb97843dcU,        0x9a1cf21fU,        0x84c09d5eU,
        0x2a36eb91U,        0x34ea84d0U,        0x178e3513U,        0x09525a52U,        0x51475695U,
        0x4f9b39d4U,        0x6cff8817U,        0x7223e756U,        0xd726532bU,        0xc9fa3c6aU,
        0xea9e8da9U,        0xf442e2e8U,        0xac57ee2fU,        0xb28b816eU,        0x91ef30adU,
        0x8f335fecU,        0x21c52923U,        0x3f194662U,        0x1c7df7a1U,        0x02a198e0U,
        0x5ab49427U,        0x4468fb66U,        0x670c4aa5U,        0x79d025e4U,        0x243cc87aU,
        0x3ae0a73bU,        0x198416f8U,        0x075879b9U,        0x5f4d757eU,        0x41911a3fU,
        0x62f5abfcU,        0x7c29c4bdU,        0xd2dfb272U,        0xcc03dd33U,        0xef676cf0U,
        0xf1bb03b1U,        0xa9ae0f76U,        0xb7726037U,        0x9416d1f4U,        0x8acabeb5U };

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    DMIChannel::DMIChannel(
      const char_t * const readChanlName, const char_t * const writeChanlName,
      const char_t* const dmiChannelName, const uint16_t dmiChnlID, const char_t * const compShtName) :
      ATC::IOComponent(atpDMIChannelId, dmiChannelName, compShtName),
      dmiConnectionStatus(ATC::Event::createLogEvent(atpDMIChannelId, ATC::CoreContainer,
        (static_cast<uint16_t>(dmiChnlID * maxDmiChannelEvent)) + 1U, 0x0U, "Health Supervision of DMI Channel: ", true)),
      dmiChannelID(dmiChnlID)
    {
      txMode = txIdle;
      static_cast<void>(vfw_strlcpy(&readChannelName[0], readChanlName, sizeof(readChannelName)));
      static_cast<void>(vfw_strlcpy(&writeChannelName[0], writeChanlName, sizeof(writeChannelName)));

      syncChannelReadDesc = static_cast<VFW_SyncChannel>(NULL);

      initDone = false;
      sendAckNumber = 0U;
      popNoAckOutQueueIdx = 0U;
      popInQueueIdx = 0U;
      pushNoAckOutQueueIdx = 0U;
      pushInQueueIdx = 0U;
      waitingAckNumber = 0U;
      retryCnt = 0U;
      waitForAckStartTime = 0;
      lastMessageReceivedTime = 0;
      outBufCnt = 0U;
      inBufCnt = 0U;
      bufIdx   = 0U;
      connected = false;
      active = false;
      memset(&inBuf[0], 0, sizeof(inBuf));
      memset(&outBuf[0], 0, sizeof(outBuf));
      memset(&chstat[0], 0, sizeof(chstat));
    }

    /******************************************************************************
    * preInit
    ******************************************************************************/
    void DMIChannel::preInit(void)
    {
      
      // Channel handle returned by vfwChannelOpenRead
      
      VFW_ChannelDesc channelReadDesc;

      // Open a vfw channel to be used when reading from dispatcher
      channelReadDesc = vfwChannelOpenRead(&readChannelName[0], ATC::dmiMsgInQueueSize,
                                           ATC::maxDMIMessageSize, &readChannelName[0]);      

      crossCompareChannelWriteDesc.initChannel(&writeChannelName[0], ATC::dmiMsgOutQueueSize, ATC::maxDMIMessageSize, true);

      if (NULL != channelReadDesc)
      {
        syncChannelReadDesc = vfwSyncAddChannel(channelReadDesc, ATC::trueVfw);
        //Deactivate the event driven callback functionality. Reading from channels are cyclic(runIn/runOut)
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
    bool DMIChannel::init(void)
    {
      //Should always remain true for init to be success
      bool retVal = true;

      if (!initDone)
      {
        //Initialize the channel statistics for the component
        static_cast<void>(vfw_strlcpy(&(chstat[0].channelname[0]), &readChannelName[0], sizeof(chstat[0].channelname)));
        static_cast<void>(vfw_strlcpy(&(chstat[0].channelType[0]), "read", sizeof(chstat[0].channelType)));
        chstat[0].numMsgCnt = 0U;
        chstat[0].numMsgBytesCnt = 0U;

        static_cast<void>(vfw_strlcpy(&(chstat[1].channelname[0]), &writeChannelName[0], sizeof(chstat[1].channelname)));
        static_cast<void>(vfw_strlcpy(&(chstat[1].channelType[0]), "write", sizeof(chstat[0].channelType)));
        chstat[1].numMsgCnt = 0U;
        chstat[1].numMsgBytesCnt = 0U;

        // Initialize all the events defined - No events are defined. Errors are handled by logging them via log handler
        
        //Initialize values for cross compare for DMI channels
        initCrossCompare();

        //Add the component to the Console Trace Vector
        ATC::AbstractConsole::corePtr()->addTraceObj(getTrace());

        initDone = retVal;
      }// End of if(!initDone)

      return initDone;
    }

    /******************************************************************************
    * runIn
    ******************************************************************************/
    void DMIChannel::runIn(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "DCH_runIn");

      // Is the DMI channel Active
      if (isActive())
      {
        const int64_t timeNow = vfwGetReferenceTime();

        while (readFromChannel())
        {

          DMIMessage msg;
          bufIdx = 0U;  //Reset for each read from channel. After reading from channel index should be 0.

          // Extract the received message into the local DMI message variable
          while (extractMessage(msg))
          {
            // Message is extracted successfully. Received message is valid.
            // Extract message ensures msg.headerType to be of only 3 types(validity check)
            // dmiHeaderTypeUnAckMsg, dmiHeaderTypeAckMsg, dmiHeaderTypeAck
            if (msg.headerType == dmiHeaderTypeAck)
            {
              // Received an Ack message. Check if this is the one we were waiting for.
              if (msg.msgNumber == waitingAckNumber)
              {
                // Correct ack received. Can send other Ack needed messages now.
                waitingAckNumber = 0U;
                txMode = txIdle;
              }
              else
              {
                // Unknown Ack message received. Report a log. Write to log and Console
                writeToLog(ATC::BriefLog, "Message error : Unknown Ack message received", __FILE__, __LINE__);
              }
            }
            else
            {
              // Message is not an Ack message but, may or may not need to respond with an Ack
              // (dmiHeaderTypeUnAckMsg, dmiHeaderTypeAckMsg). Either case push it to the In queue
              if (pushMsgToInQueue(msg))
              {
                // This message needs an Acknowledge
                if (msg.headerType == dmiHeaderTypeAckMsg)
                { 
                  // Update the sendAck Number. Ack will definitely be sent in runOut() using this value.
                  sendAckNumber = msg.msgNumber;
                }
              }
              else
              {
                // Message cannot be pushed to In queue
                writeToLog(ATC::DetailedLog, "DMI Channel error : Can't push to In queue", __FILE__, __LINE__);
              }
            }

            if (!connected)
            {
              connected = true;

              dmiConnectionStatus.setDynamicText("Connected");
              ATC::AbstractEventHandler::corePtr()->reportEvent(dmiConnectionStatus, __FILE__, __LINE__);
            }

            // Restart the Connected Status Timer
            lastMessageReceivedTime = timeNow;

          }// End of while(extractMessage())

        }// End of while(readFromChannel)

        if ((lastMessageReceivedTime != 0) && ((timeNow - lastMessageReceivedTime) >= timeoutConnected))
        {
          if (connected)
          {
            connected = false;
            txMode = txIdle;

            dmiConnectionStatus.setDynamicText("Disconnected");
            ATC::AbstractEventHandler::corePtr()->reportEvent(dmiConnectionStatus, __FILE__, __LINE__);
          }
        }

        // Reset inBufCnt after processing is complete
        inBufCnt = 0U;
        bufIdx = 0U;
        memset(&inBuf[0], 0, sizeof(inBuf));
      }
      else // if (!isActive())
      {
        readFromChannelAndDiscard();
      }
    }

    /******************************************************************************
    * runOut
    ******************************************************************************/
    void DMIChannel::runOut(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "DCH_runOut");

      // Is the DMI channel Active?
      if (isActive())
      {
        const int64_t timeNow = vfwGetReferenceTime();
        DMIMessage msg;

        // Handle the sending of Ack for a received message (needing acknowledge)
        if (sendAckNumber != 0U)
        {
          sendAck(sendAckNumber);
          sendAckNumber = 0U;
        }

        while (popNoAckOutQueue(msg))
        {
          // Take the message from Queue that needs no Ack from DMI and send it till queue is empty.

          // Update the outBufCnt after packing the message
          outBufCnt = packMsg(msg, false);

          if (outBufCnt > 0U)
          {
            writeToChannel();
          }
          else
          {
            //Report issue with wfw-buffer.
            writeToLog(ATC::BriefLog, "VFW error : Can't write to buffer. VFW buffer not valid", __FILE__, __LINE__);
          }
        }

        switch (txMode)
        {
        case txIdle:
        {
          // txIdle means that a new message (needing acknowledge) can be sent to DMI via Dispatcher
          if (!dmiAckMsgOutQueue.empty())
          {
            // pop the oldest message from the queue and send it
            msg = dmiAckMsgOutQueue.front();

            // Update the outBufCnt after packing the message
            outBufCnt = packMsg(msg, false);

            if (outBufCnt > 0U)
            {
              writeToChannel();
              // Store this message, to be resent when required!
              waitingResendMsg = msg;
              waitingAckNumber = msg.msgNumber;
              txMode = txWaitingAck;

              // Start the Ack timer to keep track
              waitForAckStartTime = timeNow;
            }
            else
            {
              // Log issue while writing to vfw-buffer
              writeToLog(ATC::BriefLog, "VFW error : Can't write to buffer. VFW buffer not valid", __FILE__, __LINE__);
            }
            // Remove the sent message from the queue
            dmiAckMsgOutQueue.popFront();
          }
          break;
        }

        case txWaitingAck:
        {
          // A message has already been sent. Expecting an Ack within timeout
          // Check timer and handle accordingly
          if ((waitForAckStartTime != 0) && ((timeNow - waitForAckStartTime) >= timeoutAckMessage))
          {
            if (retryCnt < maxRetryCnt)
            {
              // Increment the retry count. Set Mode to resend. The message will be sent in the next cycle.
              retryCnt++;
              txMode = txResend;  // Will be resent next cycle.
            }
            else
            {
              // Giving up to retry! Setting mode to Idle.
              retryCnt = 0U;
              txMode = txIdle;

              // Log stopping of retry via log handler and console
              writeToLog(ATC::BriefLog, "DMI message error : Ending retry of message number",
                static_cast<int32_t>(msg.msgNumber), __FILE__, __LINE__);
            }
          }
          else
          {
            //Timer has not yet expired. Keep waiting....
          }

          break;
        }

        case txResend:
        {
          // The acknowledge did not arrive within timeout
          // Resend the last (saved) message
          // Update the outBufCnt with the saved message
          outBufCnt = packMsg(waitingResendMsg, false);

          if (outBufCnt > 0U)
          {
            // Write it on the channel.
            writeToChannel();

            // In Resend mode, Message will need an Ack.
            // Will never enter txResend mode if Ack is not needed. State changes to txIdle in such case.
            waitingAckNumber = waitingResendMsg.msgNumber;
            txMode = txWaitingAck;

            // Start the Ack timer to keep track
            waitForAckStartTime = vfwGetReferenceTime();
          }
          else
          {
            // Log VFW error
            writeToLog(ATC::BriefLog, "VFW error : Can't write to buffer. VFW buffer not valid", __FILE__, __LINE__);
          }

          break;
        }

        default:
        {
          // Invalid case. Should never occur!
          txMode = txIdle;
          break;
        }
        }// End of switch(txMode)

        // Reset the buffer count after processing is done.
        outBufCnt = 0U;
        bufIdx = 0U;
      }// End of if(isActive)
    }

    /******************************************************************************
    * readMessage
    ******************************************************************************/
    bool DMIChannel::readMessage(DMIMessage & msg)
    {
      // This function should be called "repeatedly" till it returns false in order to get all
      // the messages from the queue. This will return false when all the read is done.

      bool retVal;

      if (popInQueueIdx < pushInQueueIdx)
      {
        msg = dmiMsgInQueue[popInQueueIdx];
        popInQueueIdx++;
        retVal = true;
      }
      else
      {
        // All items read from the queue, reset indices!
        pushInQueueIdx = 0U;
        popInQueueIdx = 0U;
        retVal = false;
      }

      return retVal;
    }

    /******************************************************************************
    * writeMessage
    ******************************************************************************/
    bool DMIChannel::writeMessage(DMIMessage const & msg)
    {
      bool retVal;

      if((dmiHeaderTypeUnAckMsg == msg.headerType) && (pushNoAckOutQueueIdx < (ATC::dmiNoAckMsgOutQueueSize - 1U)))
      {
        //Push to No Ack queue.
        dmiNoAckMsgOutQueue[pushNoAckOutQueueIdx] = msg;
        pushNoAckOutQueueIdx++;
        retVal = true;
      }
      else if( (!dmiAckMsgOutQueue.full()) && (dmiHeaderTypeAckMsg == msg.headerType))
      {
        //Push to Queue of messages which need ACK. Index handling done by GP list.
        dmiAckMsgOutQueue.pushBack(msg);
        retVal = true;
      }
      else
      {
        retVal = false;
      }

      return retVal;
    }

    /******************************************************************************
    * isConnected
    ******************************************************************************/
    bool DMIChannel::isConnected() const
    {
      return connected;
    }

    /******************************************************************************
    * isActive
    ******************************************************************************/
    bool DMIChannel::isActive() const
    {
      return active;
    }

    /******************************************************************************
    * setActive
    ******************************************************************************/
    void DMIChannel::setActive(const bool activeChannel)
    {
      active = activeChannel;

      if (!active)
      {
        if (connected)
        {
          connected = false;
          txMode = txIdle;

          dmiConnectionStatus.setDynamicText("Disconnected");
          ATC::AbstractEventHandler::corePtr()->reportEvent(dmiConnectionStatus, __FILE__, __LINE__);
        }

        waitForAckStartTime = 0;
        lastMessageReceivedTime = 0;
      }
    }

    /******************************************************************************
    * readFromChannel
    ******************************************************************************/
    bool DMIChannel::readFromChannel()
    {
      bool readSucceeded = false;
      // If there is any message in the channel queue
      if (vfwSyncChannelStat(syncChannelReadDesc) > 0U)
      {
        // Read one message
        VFW_ChannelCheck check = { VFW_ChannelErrorNone, 0U };

        const uint16_t inBufferCountLeft = ATC::maxDMIMessageSize - inBufCnt;
        //lint -e{826} vfwSyncChannelReadCheck takes a void* to a byte array
        const int32_t noOfSignedBytesRead = vfwSyncChannelReadCheck(
          syncChannelReadDesc, &inBuf[inBufCnt], inBufferCountLeft, &check);

        if (check.error != VFW_ChannelErrorNone)
        {
          writeToLog(ATC::BriefLog, "DMI channel error:", static_cast<uint32_t>(check.error), __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "DMI channel error");
        }

        if (check.timeSinceProduced > ATC::maxChannelTransmissionTime)
        {
          writeToLog(ATC::BriefLog, "DMI message too old:", check.timeSinceProduced, __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "DMI message too old");
        }

        // Read was successful
        if (noOfSignedBytesRead > 0)
        {
          const uint16_t noOfBytesRead = static_cast<uint16_t>(noOfSignedBytesRead);

          Support::AbstractCrossCompare::corePtr()->addCrossCompareInputData(&inBuf[inBufCnt], noOfBytesRead);

          // Check if buffer not full
          if ((inBufCnt + noOfBytesRead) < ATC::maxDMIMessageSize)
          {
            inBufCnt += noOfBytesRead;

            //Update channel statistics
            chstat[0].numMsgCnt++;
            chstat[0].numMsgBytesCnt += noOfBytesRead;

            readSucceeded = true;
          }
          else
          {
            // Buffer is full now
            inBufCnt = static_cast<uint16_t>(ATC::maxDMIMessageSize);

            // Log that in buffer is full
            writeToLog(ATC::DetailedLog, "DMI Channel Error: In buffer full", __FILE__, __LINE__);
            trace.write(traceLevelError, "DMI Channel Error: In buffer full");
          }
        }
      }

      return readSucceeded;
    }

    /******************************************************************************
    * readFromChannelAndDiscard
    ******************************************************************************/
    void DMIChannel::readFromChannelAndDiscard()
    {
      bool keepReading = true;

      while (keepReading)
      {
        if (vfwSyncChannelStat(syncChannelReadDesc) > 0U)
        {
          const uint16_t inBufferCountLeft = ATC::maxDMIMessageSize - inBufCnt;
          //lint -e{826} vfwSyncChannelReadCheck takes a void* to a byte array
          const int32_t noOfSignedBytesRead = vfwSyncChannelRead(
            syncChannelReadDesc, &inBuf[inBufCnt], inBufferCountLeft);

          if (noOfSignedBytesRead <= 0)
          {
            keepReading = false;
          }
        }
        else
        {
          keepReading = false;
        }
      }
    }

    /******************************************************************************
    * writeToChannel
    ******************************************************************************/
    void DMIChannel::writeToChannel()
    {
      crossCompareChannelWriteDesc.putBuffer(&outBuf[0], outBufCnt);
      crossCompareChannelWriteDesc.useNextMessage();

      //Update channel statistics
      chstat[1].numMsgCnt++;
      chstat[1].numMsgBytesCnt += static_cast<uint32_t>(outBufCnt);
    }

    /******************************************************************************
    * pushMsgToInQueue
    ******************************************************************************/
    bool DMIChannel::pushMsgToInQueue(const DMIMessage & msg)
    {
      bool retVal;

      //Is the queue having space to get new values?
      if(pushInQueueIdx < (ATC::dmiMsgInQueueSize - 1U))
      {
        dmiMsgInQueue[pushInQueueIdx] = msg;
        pushInQueueIdx++;
        retVal = true;
      }
      else
      {
        retVal = false;

        //Log that the In queue is full
        writeToLog(ATC::DetailedLog, "DMI Channel Error: In Queue full", __FILE__, __LINE__);
        trace.write(traceLevelError, "DMI Channel Error: In Queue full");
      }

      return retVal;
    }

    /******************************************************************************
    * popNoAckOutQueue
    ******************************************************************************/
    bool DMIChannel::popNoAckOutQueue(DMIMessage & msg)
    {
      bool retVal;

      if (popNoAckOutQueueIdx < pushNoAckOutQueueIdx)
      {
        msg = dmiNoAckMsgOutQueue[popNoAckOutQueueIdx];
        popNoAckOutQueueIdx++;
        retVal = true;
      }
      else
      {
        popNoAckOutQueueIdx = 0U;
        pushNoAckOutQueueIdx = 0U;
        retVal = false;
      }

      return retVal;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void DMIChannel::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TxMode>(&txMode));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&dmiChannelID));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&initDone));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&sendAckNumber));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&popNoAckOutQueueIdx));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&popInQueueIdx));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&pushNoAckOutQueueIdx));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&pushInQueueIdx));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&waitingAckNumber));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&waitingResendMsg.headerType));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&waitingResendMsg.msgNumber));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&waitingResendMsg.msgLen));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&waitingResendMsg.dmiData.msgType));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&waitingResendMsg.crc));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&waitForAckStartTime));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&lastMessageReceivedTime));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&retryCnt));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&outBufCnt));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&inBufCnt));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&connected));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&active));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&bufIdx));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&dmiConnectionStatus));
    }

    /******************************************************************************
    * extractMessage
    ******************************************************************************/
    bool DMIChannel::extractMessage(DMIMessage &msg)
    {
      bool      retVal = true;

      //Find the start of message field in the received buffer
      while((bufIdx < inBufCnt) && (inBuf[bufIdx] != dmiMsgStart))
      {
        //Keep incrementing the index till the start of message is found
        bufIdx++;
      }
      
      const uint16_t remainingBuffCnt = inBufCnt - bufIdx;
      //bufIdx now stores the index having dmi start character

      if(remainingBuffCnt > 0U)
      {
        //VFW functions to be used while reading for byte order conversion
        VFW_Buffer buffer;
        //Initialize the inBuf to be raw buffer for VFW
        vfwInitBuffer(&buffer, &inBuf[bufIdx], remainingBuffCnt);

        //Need to read from buffer
        vfwSetReadBuffer(&buffer, remainingBuffCnt);

        const uint8_t msgStart = vfwGetU8(&buffer);
        if(dmiMsgStart != msgStart)
        {
          retVal = false;
        }

        if(true == retVal)
        {
          msg.headerType      = vfwGetU8(&buffer);

          //Header files should be only of these 3 types. Else Msg is invalid
          if(!((msg.headerType == dmiHeaderTypeUnAckMsg) ||
              (msg.headerType == dmiHeaderTypeAckMsg) ||
              (msg.headerType == dmiHeaderTypeAck)))
          {
            retVal = false;
          }
        }

        if(true == retVal)
        {
          msg.msgNumber       = vfwGetU8(&buffer);  //Msg number should be 0-255. This will always be tha case as vfwGetU8returns uint8.
          msg.msgLen          = vfwGetU16(&buffer);

          //Validate the length value received. Length has length of the data field(dmiData)
          if(msg.msgLen <= (remainingBuffCnt - (DMIMessage::headerSize - (DMIMessage::trailerSize))))
          {
            //Read only for Non-Ack message
            if (msg.headerType != dmiHeaderTypeAck)
            {
              //Get data values
              msg.dmiData.msgType = vfwGetU8(&buffer);
              vfwCpyToRawBuffer(&msg.dmiData.msgData[0], &buffer,
                (static_cast<uint32_t>(msg.msgLen) - static_cast<uint32_t>(sizeof(msg.dmiData.msgType))));
            }
            else
            {
              //Do nothing. ACk msg does not have data.
            }
          }
          else
          {
            retVal = false;
          }
        }

        //All good till now. Validate and check the CRC acquired
        if(true == retVal)
        {
          msg.crc = vfwGetU32(&buffer);

          // Calculate CRC for all bytes in header and data. (Sec-4.1.5 IF spec ATP_MMI)
          const uint16_t crcCalcLen = DMIMessage::headerSize + msg.msgLen;
          if ((bufIdx + crcCalcLen) <= static_cast<uint16_t>(sizeof(inBuf)))
          {
            uint32_t calcCRC = calculateCRC(&inBuf[bufIdx], crcCalcLen);
            if(calcCRC != msg.crc)
            {
              retVal = false;
            }
          }
          else
          {
            retVal = false;
          }
        }

        //Update bufIdx value to read the next message if available
        bufIdx += (DMIMessage::headerSize + msg.msgLen + DMIMessage::trailerSize);
      }
      else
      {
        retVal = false;
      }

      // If all validation are passed this will remain true. Else false.
      return retVal;
    }

    /******************************************************************************
    * sendAck
    ******************************************************************************/
    void DMIChannel::sendAck(const uint8_t AckNumber)
    {
      //Create and send an Ack message
      DMIMessage msg;

      msg.headerType = 0x80U;
      msg.msgNumber  = AckNumber;
      msg.msgLen     = 0x0U;

      //Pack this message to the out Buffer
      outBufCnt = packMsg(msg, true);

      if(outBufCnt > 0U)
      {
        writeToChannel();
      }
      else
      {
        //Log error for VFW
        writeToLog(ATC::BriefLog, "VFW error : Can't write to buffer. VFW buffer not valid", __FILE__, __LINE__);
        trace.write(traceLevelError, "VFW error : Can't write to buffer. VFW buffer not valid");
      }
    }

    /******************************************************************************
    * sendAck
    ******************************************************************************/
    uint16_t DMIChannel::packMsg(DMIMessage const & msg, const bool isAckMsg)
    {
      // Use vfw-functions to convert from host order to network order
      VFW_Buffer buffer;

      vfwInitBuffer(&buffer, &outBuf[0], sizeof(outBuf));

      //Save this pointer as start location for CRC calculation
      const uint8_t* const crcStartPtr = vfwGetPointer(&buffer);
      
      // Pack Start of message character
      vfwPutU8(&buffer, dmiMsgStart);

      vfwPutU8(&buffer, msg.headerType);
      vfwPutU8(&buffer, msg.msgNumber);
      vfwPutU16(&buffer, msg.msgLen);

      //Data field is populated only in case of the message which is not Ack
      if(!isAckMsg)
      {
        // Pack message type
        vfwPutU8(&buffer, msg.dmiData.msgType);
        // Pack data(excluding msg type, it is already packed)
        vfwCpyFromRawBuffer(&buffer, &msg.dmiData.msgData[0], static_cast<uint32_t>((msg.msgLen - sizeof(msg.dmiData.msgType))));
      }

      const uint32_t crc = calculateCRC(crcStartPtr, (DMIMessage::headerSize + msg.msgLen));

      // Pack crc
      vfwPutU32(&buffer, crc);

      // Get valid length of buffer
      return static_cast<uint16_t> (vfwGetValidSize(&buffer));
    }

    /******************************************************************************
    * calculateCRC
    ******************************************************************************/
    uint32_t DMIChannel::calculateCRC(const uint8_t* const start, uint16_t const length) const
    {
      uint32_t counter;
      uint32_t crcAccum = 0xFFFFFFFFU;  //Init value set as per the previous project.

      const uint8_t* p = start;

      for(counter = 0U; counter < length; counter++)
      {
        const uint8_t temp = static_cast<uint8_t>((crcAccum >> 24U) ^ *p);
        ++p;
        crcAccum = ((crcAccum << 8U) ^ (crcTable[temp]));
      }

     return crcAccum;
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool DMIChannel::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments searches for the "help" or any other DMI Channel
      component specific command calls and handles it. Returns true if completely handled
      else returns false. returning false will let other components handle the call. help always returns false.
      */
      bool retVal = false;
      char_t  buff[100];

      if(ATC::isTextMatch(&argv[0][0], "dmich", sizeof("dmich")))
      {
        switch (argc)
        {
          case 1:
          {
            char_t buffer[100] = { '\0' };/// to ensure null terminated buffer

            const char_t* const buffActv = isActive() ? "active" : "not active";
            const char_t* const buffConn = isConnected() ? "connected" : "not connected";

            //lint -e{586} snprintf is needed here
            const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "The DMI Channel %u is %s and %s", dmiChannelID, buffActv, buffConn);

            if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
            }

            break;
          }

          default:
          {
            char_t toWrite[] = "Illegal Arguments: dmich takes 0 arguments";

            ATC::AbstractConsole::corePtr()->writeWithNewline(&toWrite[0]);

            break;
          }
        }

        retVal = false; //console call "dmich" should return false so that other channels' console calls are called.
      }
      else if (ATC::isTextMatch(&argv[0][0], "chstat", sizeof("chstat")))
      {
        for (uint8_t cnt = 0U; cnt < numVfwChannelsDmiChannels; cnt++)
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
      else
      {
        //Do Nothing
      }

      return retVal;
    }
  }
}
