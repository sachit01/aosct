/**********************************************************************
*
* Filename:    crc.c
* 
* Description: Slow and fast implementations of the CRC standards.
*
* Notes:       The parameters for each supported CRC standard are
*        defined in the header file crc.h.  The implementations
*        here should stand up to further additions to that list.
*
* 
* Copyright (c) 2000 by Michael Barr.  This software is placed into
* the public domain and may be used for any purpose.  However, this
* notice must not be changed or removed and no warranty is either
* expressed or implied by its publication or distribution.
**********************************************************************/
/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
* 1.0     130222  wahmad   Reused
* 1.1     270222  wahmad   Few descriptions added
* 1.2     130604  wahmad   Lint corrections have been made
*******************************************************************************/
#include "crc.h"


/*
* Derive parameters from the standard-specific parameters in crc.h.
*/
#define WIDTH    (8 * sizeof(crc))
#define TOPBIT   (1 << (WIDTH - 1))

#if (REFLECT_DATA == TRUE)
#undef  REFLECT_DATA
#define REFLECT_DATA(X)     ((US_CHAR) reflect((X), 8))
#else
#undef  REFLECT_DATA
#define REFLECT_DATA(X)     (X)
#endif

#if (REFLECT_REMAINDER == TRUE)
#undef  REFLECT_REMAINDER
#define REFLECT_REMAINDER(X)  ((crc) reflect((X), WIDTH))
#else
#undef  REFLECT_REMAINDER
#define REFLECT_REMAINDER(X)  (X)
#endif


/*********************************************************************
*
* Function:    reflect()
* 
* Description: Reorder the bits of a binary sequence, by reflecting
*        them about the middle position.
*
* Notes:   No checking is done that nBits <= 32.
*
* Returns:   The reflection of the original data.
*
*********************************************************************/
static US_LONG
reflect(US_LONG data, US_CHAR nBits)
{
  US_LONG  reflection = 0x00000000;
  US_CHAR  bit;
  
  /*
  * Reflect the data about the center bit.
  */
  for (bit = 0; bit < nBits; ++bit)
  {
  /*
  * If the LSB bit is set, set the reflection of it.
    */
    if (data & 0x01)
    {
      reflection |= (1 << ((nBits - 1) - bit));
    }
    
    data = (data >> 1);
  }
  
  return (reflection);
  
} /* reflect() */

#if 0
  /*********************************************************************
  *
  * Function:    crcSlow()
  * 
  * Description: Compute the CRC of a given message.
  *
  * Notes:   
  *
  * Returns:   The CRC of the message.
  *
*********************************************************************/
crc
crcSlow(US_CHAR const message[], S_INT nBytes)
{
  crc            remainder = INITIAL_REMAINDER;
  S_INT            byte;
  US_CHAR  bit;
  
  
  /*
  * Perform modulo-2 division, a byte at a time.
  */
  for (byte = 0; byte < nBytes; ++byte)
  {
  /*
  * Bring the next byte into the remainder.
    */
    remainder ^= (REFLECT_DATA(message[byte]) << (WIDTH - 8));
    
    /*
    * Perform modulo-2 division, a bit at a time.
    */
    for (bit = 8; bit > 0; --bit)
    {
    /*
    * Try to divide the current data bit.
      */
      if (remainder & TOPBIT)
      {
        remainder = (remainder << 1) ^ POLYNOMIAL;
      }
      else
      {
        remainder = (remainder << 1);
      }
    }
  }
  
  /*
  * The final remainder is the CRC result.
  */
  return (REFLECT_REMAINDER(remainder) ^ FINAL_XOR_VALUE);
  
}   /* crcSlow() */

#endif
crc  crcTable[256];


/*********************************************************************
*
* Function:    crcInit()
* 
* Description: Populate the partial CRC lookup table.
*
* Notes:   This function must be rerun any time the CRC standard
*        is changed.  If desired, it can be run "offline" and
*        the table results stored in an embedded system's ROM.
*
* Returns:   None defined.
*
*********************************************************************/
void
crcInit(void)
{
  crc        remainder;
  S_INT        dividend;
  US_CHAR  bit;
  
  
  /*
  * Compute the remainder of each possible dividend.
  */
  for (dividend = 0; dividend < 256; ++dividend)
  {
  /*
  * Start with the dividend followed by zeros.
    */
    remainder = (crc) dividend << (WIDTH - 8);
    
    /*
    * Perform modulo-2 division, a bit at a time.
    */
    for (bit = 8; bit > 0; --bit)
    {
    /*
    * Try to divide the current data bit.
      */     
      if (remainder > 0x7FFFFFFF)/*& TOPBIT)*/
      {
        remainder = (remainder << 1) ^ POLYNOMIAL;
      }
      else
      {
        remainder = (remainder << 1);
      }
    }
    
    /*
    * Store the result into the table.
    */
    crcTable[dividend] = remainder;
  }
  
}   /* crcInit() */


    /*********************************************************************
    *
    * Function:    crcFast()
    * 
    * Description: Compute the CRC of a given message.
    *
    * Notes:   crcInit() must be called first.
    *
    * Returns:   The CRC of the message.
    *
*********************************************************************/
crc
crcFast(US_CHAR const message[], S_INT nBytes)
{
  crc            remainder = INITIAL_REMAINDER;
  US_CHAR  data;
  S_INT            byte;
  
  
  /*
  * Divide the message by the polynomial, a byte at a time.
  */
  for (byte = 0; byte < nBytes; ++byte)
  {
    data = (US_CHAR)(REFLECT_DATA(message[byte]) ^ (remainder >> (WIDTH - 8)));
    remainder = crcTable[data] ^ (remainder << 8);
  }
  
  /*
  * The final remainder is the CRC.
  */
  return (REFLECT_REMAINDER(remainder) ^ FINAL_XOR_VALUE);
  
}   /* crcFast() */
