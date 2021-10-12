#ifndef ATCBitUnpacker_hpp
#define ATCBitUnpacker_hpp
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
* 2017-03-21    rquensel     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "atc_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATC
{
  class BitUnpacker
  {
  public:
    
    
    /**
    * Status of the packing
    */
    enum PackStatus
    {
      OK_STATUS = 0,
      OUT_OF_DATA = 3
    };

    /**
    * Constructor
    *
    * @param[in] rawBuffer - Data buffer to pack data into
    * @param[in] bufferLength - Length of the data buffer
    *
    */
    BitUnpacker(const uint8_t* const rawBuffer, const uint32_t bufferLength);

    /**
    * Unpack function to unpack up to 8 bits of a variable
    *
    * @param[in] numberOfBitsToPack - Number of bits to unpack
    *
    */
    uint8_t unpack8(const uint8_t numberOfBitsToUnpack);

    /**
    * Unpack function to unpack up to 32 bits of a variable
    *
    * @param[in] numberOfBitsToPack - Number of bits to unpack
    *
    */
    uint32_t unpack32(const uint8_t numberOfBitsToUnpack);

    /**
    * Skip nBits number of bits
    */
    void skip(const uint16_t nBits);

    /**
    * Get the unpacker status
    * @return the status of the unpacker
    */
    PackStatus getStatus() const;

    /**
    * Get the number of remaining bits
    * @return the number of remaining bits
    */
    uint32_t getNumberOfRemainingBits() const;


  private:

    /**
    * Default constructor, disabled.
    */
    BitUnpacker();

    /**
    * Assignment operator, disabled. Use copy constructor instead.
    *
    * @param[in] otherBitUnpacker - BitUnpacker object to copy
    *
    */
    const BitUnpacker& operator = (const BitUnpacker& otherBitUnpacker);

    /**
    * Raw buffer to pack data into.
    */
    const uint8_t* const rawBuffer_;

    /**
    * Data size in bits.
    */
    uint32_t dataSize_;

    /**
    * Current offset of bits read.
    */
    uint32_t readPos_;

    /**
    * Pack status, must be checked after packing to see if overflow has occurred.
    */
    PackStatus status_;
  };
}

#endif
