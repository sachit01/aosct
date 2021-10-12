#ifndef SERIALIZATION_HPP
#define SERIALIZATION_HPP
/** \internal
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without express authority is strictly forbidden.
* 
* DESCRIPTION: 
* Implementation of the Coversion of endianness
*
*******************************************************************************/
/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-20    spandita    updated 64,8 bit swapping function with modification of
                            old function with the comments.
* 2016-07-04    spandita    updated with review comments
                           			
*******************************************************************************/

#include <iostream>
#include <iomanip>

enum endianness_t {
  BIG,         // 0x44332211  => 0x44 0x33 0x22 0x11
  LITTLE,      // 0x44332211  => 0x11 0x22 0x33 0x44
  UNKNOWN
};



const uint32_t test_value    = 0x44332211;
const bool is_little_endian  = (((char *)&test_value)[0] == 0x11) && (((char *)&test_value)[1] == 0x22);
const bool is_big_endian     = (((char *)&test_value)[0] == 0x44) && (((char *)&test_value)[1] == 0x33);

const endianness_t endianness = 
   is_big_endian ? BIG: 
 (is_little_endian ? LITTLE : UNKNOWN);



/********************************************************************************
 * \brief       Feature currently not in use
 * 
 * \param [in]  Value need to change format.
 *******************************************************************************/
template <typename T>
T identity(T v){
  return v;
}

/********************************************************************************
 * \brief       Swap the 16 bit  
 * 
 * \param [in]  Value to be Swapped.
 * \param [out] Swapped Value
 *******************************************************************************/

uint16_t swap_(uint16_t v){
  return ((v & 0xFF) << 8) | ((v & 0xFF00) >> 8);
}

/********************************************************************************
 * \brief       Swap the 32 bit  
 * 
 * \param [in]  Value to be Swapped.
 * \param [out] Swapped Value
 *******************************************************************************/

uint32_t swap_(uint32_t v){
  return ((v & 0xFF) << 24) | ((v & 0xFF00) << 8) | ((v & 0xFF0000) >> 8) | ((v & 0xFF000000) >> 24);
}
/********************************************************************************
 * \brief       Swap the 64 bit  
 * 
 * \param [in]  Value to be Swapped.
 * \param [out] Swapped Value
 *******************************************************************************/
 
uint64_t swap_(uint64_t v){
  return  (((v & 0xFF00000000000000ull) >> 56)
          |((v & 0x00FF000000000000ull) >> 40)
          |((v & 0x0000FF0000000000ull) >> 24)
          |((v & 0x000000FF00000000ull) >>  8)
          |((v & 0x00000000FF000000ull) <<  8)
          |((v & 0x0000000000FF0000ull) << 24)
          |((v & 0x000000000000FF00ull) << 40)
          |((v & 0x00000000000000FFull) << 56));
}

/********************************************************************************
 * \brief       Swap the 8 bit  
 * 
 * \param [in]  Value to be Swapped.
 * \param [out] Swapped Value
 *******************************************************************************/
 
uint8_t swap_(uint8_t v){
  return v;
}
/********************************************************************************
 * \brief        Swapping function for different endianess format
 * 
 * \param [in]   Value need to change format.
 * \param [out]  destination format value
 *******************************************************************************/

template <typename T, endianness_t HOST, endianness_t REMOTE>
struct en_swap{
  static T conv(T v){
    return swap_(v);
  }
};
/********************************************************************************
 * \brief        Swapping function for same endianess format(BIG)
 * 
 * \param [in]   Value need to change format.
 * \param [out]  unchanged value
 *******************************************************************************/
template <typename T>
struct en_swap<T, BIG, BIG>{
  static T conv(T v){
    return v;
  }
};
/********************************************************************************
 * \brief        Swapping function for same endianess format(LITTLE)
 * 
 * \param [in]   Value need to change format.
 * \param [out]  unchanged value
 *******************************************************************************/
template <typename T>
struct en_swap<T, LITTLE, LITTLE> {
  static T conv(T v){
    return v;
  }
};
/********************************************************************************
 * \brief        Function to convert value to big endian format
 * 
 * \param [in]   Value .
 * \param [out]  Big endian format value
 *******************************************************************************/
template <typename T>
T to_big(T v) {
  switch (endianness){
  case LITTLE :
    return en_swap<T,LITTLE,BIG>::conv(v);
  case BIG :
  default:
    return en_swap<T,BIG,BIG>::conv(v);
  }
}

/********************************************************************************
 * \brief        Function to convert value to little endian format
 * 
 * \param [in]   Value .
 * \param [out]  lillte endian format value
 *******************************************************************************/
template <typename T>
T to_little(T v) {
   switch (endianness){
   case LITTLE :
      return en_swap<T,LITTLE,LITTLE>::conv(v);
   case BIG :
   default:
     return en_swap<T,BIG,BIG>::conv(v);
  }
}
/********************************************************************************
 * \brief        Function to convert network byte order value to host byte order
 * 
 * \param [in]   Value .
 * \param [out]  Host byte order format
 *******************************************************************************/
template <typename T>
T from_big(T v) {
   switch (endianness){
   case LITTLE :
      return en_swap<T,BIG,LITTLE>::conv(v);
   case BIG :
   default:
     return en_swap<T,BIG,BIG>::conv(v);
  }
}

#endif
