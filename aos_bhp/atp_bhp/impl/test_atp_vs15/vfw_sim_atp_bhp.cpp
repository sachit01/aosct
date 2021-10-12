/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file implements the ATP/BHP specific part of the vfw-sim.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-02-23    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_sync.h>

#include "basic_ip.hpp"
#include "vfw_sim.hpp"

#include "class_d_transceiver.hpp"

static ClassD::TransceiverClassD classDMsg;

/******************************************************************************
* vfwSimPreProcessIncomingMessage
******************************************************************************/
bool vfwSimPreProcessIncomingMessage(uint8_t channelDescr, uint8_t *inBuf, uint32_t &nrOfBytesToProcess, uint8_t *outBuf, uint32_t &outBufLen)
{
  bool retVal = false;

  // Simulate behavior for Class-D in Dispatcher, i e parse Class-D Header
  if ((ATP::BasicIP::connectionSimLCS == channelDescr) && (nrOfBytesToProcess > 0))
  {
    const ClassD::ErrorType errorType = classDMsg.parseMessage(inBuf, nrOfBytesToProcess);

    // Simulate error-handling from real Dispatcher.
    // If parsing was OK, then return the EMP-message (Body of Class-D).
    // All parsing-errors will result in  a closeReopen() of connection towards the LCS, and a reset of sequence IDs.
    if (ClassD::NoError == errorType)
    {
      // outBufLen shall return the length of EMP Message
      outBufLen = classDMsg.getActualMsgLen();

      // nrOfBytesToProcess returns the number of bytes left to process in inBuf
      nrOfBytesToProcess -= classDMsg.getClassDMessageActualLen();
      
      // Copy only the Class-D body (EMP Message) back to buffer to simulate the behavior of dispatcher.
      memcpy(outBuf, classDMsg.getClassDBodyPtr(), outBufLen);

      // Move down the rest of the bytes to start
      if (nrOfBytesToProcess > 0)
      {
        memmove(&inBuf[0], &inBuf[classDMsg.getClassDMessageActualLen()], nrOfBytesToProcess);
      }
    }
    else if (ClassD::MessageIncomplete == errorType)
    {
      // Do nothing - wait for the next read
    }
    else
    {
      ATC::AbstractBasicIP::corePtr()->closeReopen(ATP::BasicIP::connectionSimLCS);
      classDMsg.resetCommID();
      nrOfBytesToProcess = 0;
    }

    // Has been processed
    retVal = true;
  }
  else
  {

    // Has not been processed
    retVal =  false;
  }

  return retVal;
}

/******************************************************************************
* vfwSimPostProcessOutgoingMessage
******************************************************************************/
bool vfwSimPostProcessOutgoingMessage(uint8_t channelDescr, uint8_t *buf, uint32_t &len)
{
  bool retVal = false;

  // Simulate behavior for Class-D in Dispatcher, i e add Class-D Header
  if (ATP::BasicIP::connectionSimLCS == channelDescr)
  {
     
    if (ATC::AbstractBasicIP::corePtr()->getConnectionStatus(ATP::BasicIP::connectionSimLCS) == ATP::BasicIP::ConnectionStatusConnected)
    {
      // Build a Class-D Message from the outgoing buffer (EMP Message)
      classDMsg.buildMessage(buf, len);

      // Length will now also include Class-D Header
      len = classDMsg.getClassDMessageActualLen();

      // Copy whole Class-D message back to buffer to simulate the behavior of dispatcher.
      memcpy(buf, classDMsg.getClassDBufferPtr(), len);
    }
    else
    {
      classDMsg.resetCommID();
    }

    retVal = true;

  }
  return retVal;
}

