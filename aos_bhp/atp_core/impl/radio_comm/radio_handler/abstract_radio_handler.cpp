/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2015
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
* 2015-11-13    bhermans    Created
* 2016-03-03    bhermans    Introduced namespace RadioCom
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-04-21    lantback    Implemented corePtr()
* 2016-04-22    lantback    Added component type
* 2016-06-15    akushwah    Radio Handler Implementation
* 2016-06-17    akushwah    Incorporated the Review Comments
* 2016-09-23    bhermans    Removed event "no message available in radio-channel"
* 2016-09-23    bhermans    Removed unused event definition.
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_config.hpp"
#include "abstract_odometry.hpp"
#include "abstract_radio_handler.hpp"
#include "radio_message_types.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_cross_compare.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_targets.hpp"
#include <vfw_checkpoints.h>
#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <cstdio>
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
  namespace RadioCom
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractRadioHandler::AbstractRadioHandler() : ATC::ProcComponent(atpRadioHandlerId, "RadioHandler", "RH")
    {
      if (coreRadioHandlerInstancePtr != 0)
      {
        // Error handler
        ATC::aosHalt(__FILE__, __LINE__, "Radio handler constructor already instantiated");
      }
      // Set up single instance pointer for core access
      coreRadioHandlerInstancePtr = this;

      isYardModeCounterExpired = false;
      yardModeCounter = 0U;
      numPositionMessages = 0U;
      tccTimeoutStatus = false;
      lastRefTimeTCCTimeout = 0;
    }

    /******************************************************************************
    * Function: run()
    * Description: 1. Calls RunIn() function for each Radio Channel
    *              2. Write the Messages to Radio Channel based on the availability Message Handler messages
    *              3. calls RunOut() function for each Radio Channel
    ******************************************************************************/
    void AbstractRadioHandler::run(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "RH_run");

      bool anyRadioChlConnected = false;
      bool writeMessageStatus = false;
      RadioChannelVector::iterator iter = radioChannels.begin();
      while (iter != radioChannels.end())
      {
        RadioChannel *radioChannel = (*iter);
        // schedule each radio-channel
        radioChannel->runIn();
        //check if radio channel is connected
        if (radioChannel->getConnected())
        {
          anyRadioChlConnected = true;

          // Read message from radio channel
          RadioMessage radioMessageToPeek;
          if (radioChannel->peekMessage(radioMessageToPeek))
          {

            // Separate processing for Central/Regional TCC
            if (radioChannel->isCentral())
            {
              handleCentral(radioChannel, radioMessageToPeek);
            }
            else
            {
              handleRegion(radioChannel, radioMessageToPeek, writeMessageStatus);
            }
          }

          // Send outgoing messages 
          radioChannel->runOut();
        }

        ++iter;
      }//End of while()
      
      if(anyRadioChlConnected)
      {
        // Reset the TCC timeout
        tccTimeoutStatus = false;

        //Reset the Yard Mode Counter 
        yardModeCounter = 0U;
        isYardModeCounterExpired = false;
      }
      else
      {
        if (tccTimeoutStatus)
        {
          /**
          * Calculate the maximum time allowed to enter yard mode from Config params
          */
          const uint16_t MaxCountReqForYardMode =
            (static_cast<uint16_t>(AbstractConfig::corePtr()->getRadioTimeOutYardMode()) * ATC::secToMSec) /
            ATC::cycleCntToMsec; //lint !e734 no loss of precision

                   // Counter to enter yard mode
          if (yardModeCounter <= MaxCountReqForYardMode)
          {
            yardModeCounter++;
          }
          else
          {
            // Set the yard mode flag as true
            isYardModeCounterExpired = true;
          }
        }
        else
        {//Set the TCC timeout
          tccTimeoutStatus = true;
          //set the time for tcc timeout
          lastRefTimeTCCTimeout = vfwGetReferenceTime();
        }
      }
    }

    /******************************************************************************
    * handleCentral
    ******************************************************************************/
    void AbstractRadioHandler::handleCentral(RadioChannel* const radioChannel, const RadioMessage& radioMessageToPeek) const
    {
      RadioMessage radioMsgToSend;

      // Is message a valid AreaRequest?
      if (Kernel::AbstractMessageHandler::corePtr()->validateAreaRequest(radioMessageToPeek, radioChannel->getChannelId()))
      {
        RadioMessage processedAreaReqMsg;
        Kernel::AbstractMessageHandler::corePtr()->setRegistrationAreaMessageSentToCentralTCC(false);

        // Yes -> Get response (RegistrationArea) if available.
        if (Kernel::AbstractMessageHandler::corePtr()->getRegistrationArea(radioMsgToSend))
        {
          // Write Registration Area in response to AreaRequest
          static_cast<void>(radioChannel->writeMessage(radioMsgToSend));

          //Set the flag to true, indicating whether RegistrationArea Message has been sent to Central TCC
          Kernel::AbstractMessageHandler::corePtr()->setRegistrationAreaMessageSentToCentralTCC(true);
          Kernel::AbstractMessageHandler::corePtr()->clearAreaSelectedByDriver();

          // Discard the message so that it is not read by MessageHandler
          static_cast<void>(radioChannel->readMessage(processedAreaReqMsg));
        }

        // If the Area Request Message was the first one, let message-handler process it in
        // order to set condition for DMI to prompt for RegArea (I e don't readMessage() here)
      }
    }

    /******************************************************************************
    * handleRegion
    ******************************************************************************/
    void AbstractRadioHandler::handleRegion(RadioChannel* const radioChannel, const RadioMessage& radioMessageToPeek, bool& writeMessageStatus)
    {
      RadioMessageToSend messageDataToSend;

      // Check, process and reply to ProtocolVersion immediately
      if (Kernel::AbstractMessageHandler::corePtr()->validateProtocolVersion(radioMessageToPeek, radioChannel->getChannelId()))
      {
        RadioMessage protocolVersionOut;
        RadioMessage processedProtocolVersionIn;

        // Any ProtocolVersion-response to send to TCC?
        if (Kernel::AbstractMessageHandler::corePtr()->getProtocolVersionResponse(protocolVersionOut))
        {
          // Write an outgoing ProtocolVersion in response to incoming ProtocolVersion
          static_cast<void>(radioChannel->writeMessage(protocolVersionOut));
        }

        // Discard the incoming ProtocolVersion message so that it is not processed again by MessageHandler.
        static_cast<void>(radioChannel->readMessage(processedProtocolVersionIn));

        // Clear any pending outgoing messages
        Kernel::AbstractMessageHandler::corePtr()->popMessages(radioChannel->getChannelId());
      }
      else if (Kernel::AbstractMessageHandler::corePtr()->readMessage(messageDataToSend, radioChannel->getChannelId()))
      {
        // Write the pending outgoing message from MessageHandler to Radio Channel
        writeMessageStatus = radioChannel->writeMessage(messageDataToSend.message);
      }
      else // Default is to send a position report
      {
        RadioMessage radioMsgToSend;
        if (Kernel::AbstractMessageHandler::corePtr()->getDefaultPositionReport(radioMsgToSend, radioChannel->getChannelId()))
        {
          // Write the Default Position Report Message to the Radio Channel
          writeMessageStatus = radioChannel->writeMessage(radioMsgToSend);

          // Tell message-handler that default position report was successfully sent on this channel.
          Kernel::AbstractMessageHandler::corePtr()->ackDefaultPositionReport(radioChannel->getChannelId(), true);

          /* Keep track of the number of position messages sent */
          numPositionMessages++;
        }
      }
    }

    /******************************************************************************
    * Function:    readMessage
    * Description: Reads any available message from any radio-channel
    *
    * Return true if any message is available for reading from radio channel
    ******************************************************************************/
    bool AbstractRadioHandler::readMessage(RadioMessage & msg, uint16_t & channelId)
    {
      bool messageAvailable = false;
      RadioChannelVector::iterator iter = radioChannels.begin();

      //read any valid incoming message from any RadioChannel
      while ((iter != radioChannels.end()) && (messageAvailable == false))
      {
        RadioChannel* radioChannel = (*iter);

        // Read message from radio channel
        if (radioChannel->readMessage(msg))
        {
          messageAvailable = true;
          //get the Unique Radio Channel Id
          channelId = radioChannel->getChannelId();
        }
        else
        {
          ++iter;
        }
      }// end of while()

      return messageAvailable;
    }

    /******************************************************************************
    * getConnected
    ******************************************************************************/
    bool AbstractRadioHandler::getConnected(void) const
    {
      bool anyChannelConnected = false;

      RadioChannelVector::const_iterator iter = radioChannels.begin();

      while ((iter != radioChannels.end()) && (!anyChannelConnected))
      {
        RadioChannel* radioChannel = (*iter);

        // Read radio channel connection status
        if (radioChannel->getConnected())
        {
          anyChannelConnected = true;
        }
        else
        {
          ++iter;
        }
      }

      return anyChannelConnected;
    }

    /******************************************************************************
    * getConnected
    ******************************************************************************/
    bool AbstractRadioHandler::getConnected(const uint16_t chId) const
    {
      bool chIdConnected = false;

      RadioChannelVector::const_iterator iter = radioChannels.begin();

      while ((iter != radioChannels.end()) && (!chIdConnected))
      {
        RadioChannel* radioChannel = (*iter);

        // Read radio channel connection status and channel ID.
        bool connected = radioChannel->getConnected();
        bool correctChannelId = (radioChannel->getChannelId() == chId);

        if (connected && correctChannelId)
        {
          chIdConnected = true;
        }
        else
        {
          ++iter;
        }
      }

      return chIdConnected;
    }

    /******************************************************************************
    * isYardModeTimerExpired
    ******************************************************************************/
    bool AbstractRadioHandler::isYardModeTimerExpired() const
    {
      return isYardModeCounterExpired;
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractRadioHandler* AbstractRadioHandler::corePtr(void)
    {
      return coreRadioHandlerInstancePtr;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractRadioHandler::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      // radioChannels : Complex type
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&numPositionMessages));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&yardModeCounter));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&lastRefTimeTCCTimeout));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&tccTimeoutStatus));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&isYardModeCounterExpired));

    }

    /******************************************************************************
    * getNumOfTCCConnected
    ******************************************************************************/
    uint8_t AbstractRadioHandler::getNumOfTCCConnected()
    {
      uint8_t numOfTCCConnected = 0U;

      RadioChannelVector::iterator iter = radioChannels.begin();

      while ((iter != radioChannels.end()))
      {
        // Read radio channel connection status
        if ((*iter)->getConnected())
        {
          ++numOfTCCConnected;
          ++iter;
        }
        else
        {
          ++iter;
        }
      }

      return numOfTCCConnected;

    }

    /******************************************************************************
    * getNumPositionMessages
    ******************************************************************************/
    uint32_t AbstractRadioHandler::getNumPositionMessages() const
    {
      return numPositionMessages;
    }

    /******************************************************************************
    * getTCCTimeoutVal
    ******************************************************************************/
    int64_t AbstractRadioHandler::getTCCTimeoutVal() const
    {
      return lastRefTimeTCCTimeout;
    }

    /******************************************************************************
    * getTCCTimeoutStatus
    ******************************************************************************/
    bool AbstractRadioHandler::getTCCTimeoutStatus() const
    {
      return tccTimeoutStatus;
    }

  }
}
