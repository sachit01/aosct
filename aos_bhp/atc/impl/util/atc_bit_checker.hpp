#ifndef AtcBitChecker_hpp
#define AtcBitChecker_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2020
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  This is a utility class to set bits in a bitfield
*
******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"
#include "atc_util.hpp"
#include <string.h>

/******************************************************************************
* DECLARATIONS    
******************************************************************************/
namespace ATC
{
  /**
  * The class BitChecker can handle the maximum number of bits that the required type can handle.
  * maxBitToSet must be less or equal to the maximum value for the provided type.
  *
  */
  template<class T, const T maxBitToSet>
  class BitChecker
  {
  public:

    /**
    * Constructor
    */
    BitChecker();

    //lint -sem(ATC::BitChecker::clear,initializer)

    /**
    * Resets all bits
    */
    void clear();

    /**
    * Will check and set the bit
    *
    * @param[in] bitToSet bit to set
    * Returns true if the bitToSet is not set.
    */
    bool setBit(const T bitToSet);

    /**
    * Will check if a bit is set
    *
    * @param[in] bitToGet bit to get
    * Returns true if the bitToGet is set.
    */
    bool getBit(const T bitToGet) const;

  protected:

  private:

    /**
    * Arraysize
    */
    static const T storageSize = (maxBitToSet + 31U) / 32U;

    /**
    * Array to hold the number of bits provided
    */
    uint32_t occupiedBitsArray[storageSize];

  };

  /******************************************************************************
  * Constructor
  ******************************************************************************/
  template<class T, const T maxBitToSet>
  BitChecker<T, maxBitToSet>::BitChecker()
  {
    clear();
  }

  /******************************************************************************
  * setBit
  ******************************************************************************/
  template<class T, const T maxBitToSet>
  bool BitChecker<T, maxBitToSet>::setBit(const T bitToSet)
  {
    const T i = bitToSet / 32U;
    const uint32_t bit = bitToSet & static_cast<uint32_t>(31U); // Same as % 32
    bool retValue = false;

    if (i < storageSize)
    {
      uint32_t& val = occupiedBitsArray[i];
      const uint32_t maskValue = static_cast<uint32_t>(1U) << bit;
      retValue = ((maskValue & val) == 0U);
      val |= maskValue;
    }
    else
    {
      aosHalt(__FILE__, __LINE__, "BitChecker bitToSet outside range");
    }

    return retValue;
  }

  /******************************************************************************
  * getBit
  ******************************************************************************/
  template<class T, const T maxBitToSet>
  bool BitChecker<T, maxBitToSet>::getBit(const T bitToGet) const
  {
    const T i = bitToGet / 32U;
    const uint32_t bit = bitToGet & static_cast<uint32_t>(31U); // Same as % 32
    bool retValue = false;

    if (i < storageSize)
    {
      const uint32_t& val = occupiedBitsArray[i];
      const uint32_t maskValue = static_cast<uint32_t>(1U) << bit;
      retValue = ((maskValue & val) != 0U);
    }
    else
    {
      aosHalt(__FILE__, __LINE__, "BitChecker bitToGet outside range");
    }

    return retValue;
  }

  /******************************************************************************
  * clear
  ******************************************************************************/
  template<class T, const T maxBitToSet>
  void BitChecker<T, maxBitToSet>::clear()
  {
    memset(&occupiedBitsArray[0], 0, sizeof(occupiedBitsArray));
  }

}

#endif
