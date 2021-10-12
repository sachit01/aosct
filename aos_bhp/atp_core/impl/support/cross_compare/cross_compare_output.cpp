/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines CrossCompareOutput class
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

#include "cross_compare_output.hpp"
#include "abstract_cross_compare.hpp"
#include "atc_util.hpp"
#include "abstract_console.hpp"
#include <stdio.h>

namespace ATP
{

  /******************************************************************************
  * useNextMessage
  ******************************************************************************/
  void Support::CrossCompareOutput::useNextMessage()
  {
    currentMessageIndex_ = currentMessageCount_;
  }



  /******************************************************************************
  * getNext
  ******************************************************************************/
  Support::CrossCompareOutput * Support::CrossCompareOutput::getNext()
  {
    return next_;
  }

  /******************************************************************************
  * putBoolean
  ******************************************************************************/
  void Support::CrossCompareOutput::putBoolean(const bool val)
  {
    vfwPutU8(vfwBuffers_[calculateCurrentMessageIndex_()], val ? 0xFFU : 0x00U);
  }

  /******************************************************************************
  * putUint64
  ******************************************************************************/
  void Support::CrossCompareOutput::putUint64(const uint64_t val)
  {
    vfwPutU64(vfwBuffers_[calculateCurrentMessageIndex_()], val);
  }

  /******************************************************************************
  * putUint32
  ******************************************************************************/
  void Support::CrossCompareOutput::putUint32(const uint32_t val)
  {
    vfwPutU32(vfwBuffers_[calculateCurrentMessageIndex_()], val);
  }

  /******************************************************************************
  * putUint16
  ******************************************************************************/
  void Support::CrossCompareOutput::putUint16(const uint16_t val)
  {
    vfwPutU16(vfwBuffers_[calculateCurrentMessageIndex_()], val);
  }

  /******************************************************************************
  * useNextMessage
  ******************************************************************************/
  void Support::CrossCompareOutput::putUint8(const uint8_t val)
  {
    vfwPutU8(vfwBuffers_[calculateCurrentMessageIndex_()], val);
  }

  /******************************************************************************
  * putInt32
  ******************************************************************************/
  void Support::CrossCompareOutput::putInt32(const int32_t val)
  {
    vfwPutI32(vfwBuffers_[calculateCurrentMessageIndex_()], val);
  }

  /******************************************************************************
  * putInt16
  ******************************************************************************/
  void Support::CrossCompareOutput::putInt16(const int16_t val)
  {
    vfwPutI16(vfwBuffers_[calculateCurrentMessageIndex_()], val);
  }

  /******************************************************************************
  * putInt8
  ******************************************************************************/
  void Support::CrossCompareOutput::putInt8(const int8_t val)
  {
    vfwPutI8(vfwBuffers_[calculateCurrentMessageIndex_()], val);
  }

  /******************************************************************************
  * putBuffer
  ******************************************************************************/
  void Support::CrossCompareOutput::putBuffer(const uint8_t * const buffer, const uint32_t length)
  {
    vfwCpyFromRawBuffer(vfwBuffers_[calculateCurrentMessageIndex_()], buffer, length);
  }

  /******************************************************************************
  * CrossCompareOutput destructor
  ******************************************************************************/
  Support::CrossCompareOutput::~CrossCompareOutput()
  {
    next_ = static_cast<CrossCompareOutput*>(NULL);
  }

  /******************************************************************************
  * setNext
  ******************************************************************************/
  void Support::CrossCompareOutput::setNext(CrossCompareOutput * const endObject)
  {
    if (endObject == NULL)
    {
      next_ = endObject;
    }
    else if (next_ != NULL)
    {
      ATC::aosHalt(__FILE__, __LINE__, "Object added must not already be in a list");
    }
    else
    {
      next_ = endObject;
    }
  }

  /******************************************************************************
  * writeDataBinary
  ******************************************************************************/
  void Support::CrossCompareOutput::writeDataBinary(VFW_Buffer* const ownBuffer) const
  {
    uint8_t bufferIndex = 0U;

    VFW_Buffer* buffer = getBuffer_(bufferIndex);

    while (buffer != static_cast<VFW_Buffer*>(NULL))
    {
      const uint32_t numberOfBytesToWrite = vfwGetValidSize(buffer);
      vfwCpyFromRawBuffer(ownBuffer, vfwGetStart(buffer), numberOfBytesToWrite);
      vfwSetFullBuffer(buffer);

      ++bufferIndex;
      buffer = getBuffer_(bufferIndex);
    }

  }

  /******************************************************************************
  * CrossCompareOutput constructor
  ******************************************************************************/
  Support::CrossCompareOutput::CrossCompareOutput()
    :
    next_(static_cast<CrossCompareOutput*>(NULL)),
    bufferMaxSize_(0U),
    nMessages_(0U),
    currentMessageCount_(0U),
    currentMessageIndex_(0U)
  {
    for (uint8_t i = 0U; i < maxNumberOfMessages; ++i)
    {
      vfwBuffers_[i] = static_cast<VFW_Buffer*>(NULL);
      bufferData_[i] = static_cast<uint8_t*>(NULL);
    }
  }

  /******************************************************************************
  * init_
  ******************************************************************************/
  void Support::CrossCompareOutput::init_(const uint32_t nMessages, const uint32_t sizeMessages)
  {
    bufferMaxSize_ = sizeMessages;
    nMessages_ = nMessages;

    if (nMessages <= maxNumberOfMessages)
    {

      for (uint8_t i = 0U; i < nMessages; ++i)
      {
        vfwBuffers_[i] = new VFW_Buffer;
        bufferData_[i] = new uint8_t[sizeMessages];

        vfwInitBuffer(vfwBuffers_[i], bufferData_[i], sizeMessages);
      }

      // Add yourself to cross compare...
      Support::AbstractCrossCompare::corePtr()->addCrossCompareOutputData(this);
    }
    else
    {
      ATC::aosHalt(__FILE__, __LINE__, "Not enough number of messages...");
    }
  }

  /******************************************************************************
  * getBuffer_
  ******************************************************************************/
  VFW_Buffer* Support::CrossCompareOutput::getBuffer_(const uint8_t bufferIndex) const
  {
    VFW_Buffer* buffer = static_cast<VFW_Buffer*>(NULL);

    if (bufferIndex < currentMessageCount_)
    {
      VFW_Buffer* tmpBuffer = vfwBuffers_[bufferIndex];
      if (vfwGetValidSize(tmpBuffer) != 0U) // Check if it is zero or not
      {
        buffer = tmpBuffer;
      }
    }

    return buffer;
  }


  /******************************************************************************
  * clearBuffers
  ******************************************************************************/
  void Support::CrossCompareOutput::clearBuffers()
  {
    for (uint8_t i = 0U; i < currentMessageCount_; ++i)
    {
      vfwClearBuffer(vfwBuffers_[i]);
    }

    currentMessageCount_ = 0U;
    currentMessageIndex_ = 0U;
  }

  /******************************************************************************
  * calculateCurrentMessageIndex_
  ******************************************************************************/
  uint8_t Support::CrossCompareOutput::calculateCurrentMessageIndex_()
  {
    if (currentMessageIndex_ == currentMessageCount_) // Shell we get to the next message?
    {
      ++currentMessageCount_;
      if (currentMessageCount_ > nMessages_)
      {
        ATC::aosHalt(__FILE__, __LINE__, "Creating too many messages...");
      }
    }

    return currentMessageIndex_;
  }
}

