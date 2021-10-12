/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines CrossCompareOutputChannel class
* used by the AOS.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-01-16    rquensel    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "cross_compare_output_channel.hpp"
#include <vfw_string.h>
#include "atc_util.hpp"

#ifndef __VSIM
#include <string.h>
#endif

/******************************************************************************
* commit
******************************************************************************/
void ATP::Support::CrossCompareOutputChannel::commit()
{
  uint8_t bufferIndex = 0U;

  VFW_Buffer* buffer = getBuffer_(bufferIndex);

  while (buffer != static_cast<VFW_Buffer*>(NULL))
  {
    const uint32_t numberOfBytesToSend = vfwGetValidSize(buffer);
    
    if (numberOfBytesToSend > 0U)
    {
      if (!sendDividedMessage)
      {
        // Do not divide, send whole message...
        vfwChannelWrite(channelWriteDesc, vfwGetStart(buffer), numberOfBytesToSend);
      }
      else
      {
        // Send first half from CPU-A, add one if odd
#ifndef __EMD
        const uint32_t numberOfBytesToSendFromA = (numberOfBytesToSend / 2U) + (numberOfBytesToSend & 1U);
#else
        const uint32_t numberOfBytesToSendFromA = numberOfBytesToSend - 1U; // In EMD in order to test the load, only send one from B
#endif
        if (vfwGetSide() == VFW_A_SIDE)
        {
          // Only send first part from ATP-A
          vfwChannelWrite(channelWriteDesc, vfwGetStart(buffer), numberOfBytesToSendFromA);
        }
        else // VFW_B_SIDE
        {
          const uint32_t numberOfBytesToSendFromB = numberOfBytesToSend - numberOfBytesToSendFromA;
          // Remove part from ATP-A
          vfwConsumeBuffer(buffer, numberOfBytesToSendFromA);
          // Send second half from ATP-B
          vfwChannelWrite(channelWriteDesc, vfwGetPointer(buffer), numberOfBytesToSendFromB);
        }
      }
    }
    ++bufferIndex;
    buffer = getBuffer_(bufferIndex);
  }

  clearBuffers();
}


/******************************************************************************
* getDescription
******************************************************************************/
const char_t* ATP::Support::CrossCompareOutputChannel::getDescription() const
{
  return chName;
}

/******************************************************************************
* CrossCompareOutputChannel destructor
******************************************************************************/
ATP::Support::CrossCompareOutputChannel::~CrossCompareOutputChannel()
{
  channelWriteDesc = static_cast<VFW_ChannelDesc>(NULL);
}

/******************************************************************************
* CrossCompareOutputChannel constructor
******************************************************************************/
ATP::Support::CrossCompareOutputChannel::CrossCompareOutputChannel()
  : 
  CrossCompareOutput(),
  channelWriteDesc(static_cast<VFW_ChannelDesc>(NULL)),
  sendDividedMessage(true)
{
  chName[0] = '\0';
}

/******************************************************************************
* initChannel
******************************************************************************/
void ATP::Support::CrossCompareOutputChannel::initChannel(const char_t * const channelName, const uint32_t nMessages, const uint32_t sizeMessages,
#ifndef _SIL
  const bool divideMessage)
#else
  const bool /*divideMessage*/)
#endif

{
  CrossCompareOutput::init_(nMessages, sizeMessages);

#ifndef _SIL
  sendDividedMessage = divideMessage;
#else
  // Don't divide the message in SIL
  sendDividedMessage = false;
#endif

  channelWriteDesc = vfwChannelOpenWrite(channelName, nMessages, sizeMessages);

  if (channelWriteDesc == static_cast<VFW_ChannelDesc>(NULL))
  {
    ATC::aosHalt(__FILE__, __LINE__, "Could not create channel!");
  }
  else
  {
#ifndef __VSIM
    vfwChannelSetOverwritable(channelWriteDesc);
#endif
    static_cast<void>(vfw_strlcpy(&chName[0], channelName, VFW_CH_NAME_MAX_Z));
  }
}
