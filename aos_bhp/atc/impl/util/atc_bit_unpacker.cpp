/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class implements the atc common utility function.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-03-28    rquensel     Created from P8
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_bit_unpacker.hpp"

/******************************************************************************
 * Local data for this file
 ******************************************************************************/
namespace
{
  /******************************************************************************
  * Bit masks used for packing
  ******************************************************************************/

  static uint8_t byteMaskLow[] =
  {
    0x00U, // 0
    0x01U, // 1
    0x03U, // 2
    0x07U, // 3
    0x0FU, // 4
    0x1FU, // 5
    0x3FU, // 6
    0x7FU, // 7
    0xFFU  // 8
  };
}

namespace ATC
{

  /******************************************************************************
  * skip
  ******************************************************************************/
  void
  BitUnpacker::skip(const uint16_t nBits)
  {
    if ((readPos_ + nBits) > dataSize_)
    {
      status_ = OUT_OF_DATA;
      readPos_ = dataSize_;
    }

    readPos_ += nBits;
  }

  /******************************************************************************
  * getStatus
  ******************************************************************************/
  BitUnpacker::PackStatus
  BitUnpacker::getStatus() const
  {
    return status_;
  }

  /******************************************************************************
  * getNumberOfRemainingBits
  ******************************************************************************/
  uint32_t
  BitUnpacker::getNumberOfRemainingBits() const
  {
    return dataSize_ - readPos_;
  }

  /******************************************************************************
  * Constructor
  ******************************************************************************/
  BitUnpacker::BitUnpacker(const uint8_t* const rawBuffer, const uint32_t bufferLength) :
    rawBuffer_(rawBuffer),
    dataSize_(bufferLength * 8U),  // *8 Size is in bytes, dataSize_ is in bits.
    readPos_(0U),
    status_(BitUnpacker::OK_STATUS)
  {
  }

  /******************************************************************************
  * unpack32
  ******************************************************************************/
  uint32_t
  BitUnpacker::unpack32(const uint8_t numberOfBitsToUnpack)
  {
    uint32_t d = 0U;

    const uint32_t readBytePos = readPos_ >> 3U;
    const uint8_t* bytePos = &rawBuffer_[readBytePos];
    const uint8_t r = 8U - static_cast<uint8_t>(readPos_ & 7U);   // Number of bits left in last byte in buffer

    readPos_ += numberOfBitsToUnpack;

    if ((readPos_ > dataSize_)  ||  (numberOfBitsToUnpack > 32U))
    {
      status_ = OUT_OF_DATA;
      readPos_ = dataSize_;
    }
    else
    {
      if ((r == 8U) && ((numberOfBitsToUnpack & 7U) == 0U))
      {
        // One or mode bytes are wanted and a full byte is found in the beginning of 
        // the bit buffer so we can copy bytewise.
        switch (numberOfBitsToUnpack >> 3U)
        {
        case 4U:
          d = *bytePos;
          ++bytePos;
          d = (d << 8) | *bytePos;
          ++bytePos;
          d = (d << 8) | *bytePos;
          ++bytePos;
          d = (d << 8) | *bytePos;
          break;

        case 3U:
          d = *bytePos;
          ++bytePos;
          d = (d << 8) | *bytePos;
          ++bytePos;
          d = (d << 8) | *bytePos;
          break;

        case 2U:
          d = *bytePos;
          ++bytePos;
          d = (d << 8) | *bytePos;
          break;

        case 1U:
          d = *bytePos;
          break;

        default:
          // nop
          break;
        }
        //size -= 8;
      }
      else if (numberOfBitsToUnpack <= r)
      {
        // There are more bits left in the first byte in the bit buffer than
        // what is asked for so only bits from that bytes need to be copied.
        d = (static_cast<uint32_t>(*bytePos) >> (r - numberOfBitsToUnpack)) & static_cast<uint32_t>(byteMaskLow[numberOfBitsToUnpack]);
        //size -= r;                     // ((1 << nBits) - 1)
      }
      else
      {
        // First take all bits remaining in first byte in bit buffer.
        d = static_cast<uint32_t>(*bytePos) & static_cast<uint32_t>(byteMaskLow[r]);

        uint8_t numberOfBitsLeft = numberOfBitsToUnpack - r;

        // Take as many full bytes as needed.
        switch (numberOfBitsLeft >> 3U)
        {
        case 4U:
          ++bytePos;
          d = (d << 8) | *bytePos;
          ++bytePos;
          d = (d << 8) | *bytePos;
          ++bytePos;
          d = (d << 8) | *bytePos;
          ++bytePos;
          d = (d << 8) | *bytePos;
          numberOfBitsLeft -= 32U;
          break;

        case 3U:
          ++bytePos;
          d = (d << 8) | *bytePos;
          ++bytePos;
          d = (d << 8) | *bytePos;
          ++bytePos;
          d = (d << 8) | *bytePos;
          numberOfBitsLeft -= 24U;
          break;

        case 2U:
          ++bytePos;
          d = (d << 8) | *bytePos;
          ++bytePos;
          d = (d << 8) | *bytePos;
          numberOfBitsLeft -= 16U;
          break;

        case 1U:
          ++bytePos;
          d = (d << 8) | *bytePos;
          numberOfBitsLeft -= 8U;
          break;

        default:
          // nop
          break;
        }

        // If still a few bits (now less than 8) is needed take it from the
        // byte now first in the bit buffer.
        if (numberOfBitsLeft > 0U)
        {
          ++bytePos;
          d = (d << numberOfBitsLeft) | (static_cast<uint32_t>(*bytePos) >> (8U - numberOfBitsLeft));
        }
      }
    }
    return d;
  }

  /******************************************************************************
  * unpack8
  ******************************************************************************/
  uint8_t
  BitUnpacker::unpack8(const uint8_t numberOfBitsToUnpack)
  {
    uint8_t d = 0U;

    const uint32_t readBytePos = readPos_ >> 3U;
    const uint8_t* bytePos = &rawBuffer_[readBytePos];
    const uint8_t r = 8U - static_cast<uint8_t>(readPos_ & 7U);   // Number of bits left in last byte in buffer

    readPos_ += numberOfBitsToUnpack;

    if ((readPos_ > dataSize_)  ||  (numberOfBitsToUnpack > 8U))
    {
      status_ = OUT_OF_DATA;
      readPos_ = dataSize_;
    }
    else
    {
      if ((r == 8U) && ((numberOfBitsToUnpack & 7U) == 0U))
      {
        // One or mode bytes are wanted and a full byte is found in the beginning of 
        // the bit buffer so we can copy bytewise.
        d = *bytePos;
      }
      else if (numberOfBitsToUnpack <= r)
      {
        // There are more bits left in the first byte in the bit buffer than
        // what is asked for so only bits from that bytes need to be copied.
        d = (*bytePos >> (r - numberOfBitsToUnpack)) & byteMaskLow[numberOfBitsToUnpack];
        //size -= r;                     // ((1 << nBits) - 1)
      }
      else
      {
        // First take all bits remaining in first byte in bit buffer.
        d = (*bytePos) & byteMaskLow[r];

        uint8_t numberOfBitsLeft = numberOfBitsToUnpack - r;

        // If still a few bits (now less than 8) is needed take it from the
        // byte now first in the bit buffer.
        if (numberOfBitsLeft > 0U)
        {
          ++bytePos;
          //lint -e{701} A uint8_t (unsigned) is shifted left
          d = static_cast<uint8_t>(d << numberOfBitsLeft) | (*bytePos >> (8U - numberOfBitsLeft));
        }
      }
    }
    return d;
  }

}

