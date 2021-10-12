#ifndef ATCUtil_hpp
#define ATCUtil_hpp
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
* 2016-08-23    akushwah     Created
* 2016-09-26    spandita     updated the vfwSideStr()
* 2016-11-10    adgupta     Added function to get short name with Component id
* 2016-12-21    spandita    Added the halt function for constructor
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include <vfw_identity.h>
#include "atc_types.hpp"

/******************************************************************************
* LINT SUPPRESSIONS:
*
* Error 929: "Note -- cast from pointer to pointer [MISRA C++ Rule 5-2-7]"
* is suppressed locally when using dynamic_cast. Motivation: dynamic_cast
* can only be used between related classes and therefore MISRA rule 5-2-7
* does not apply.
******************************************************************************/

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATC
{

  static const uint16_t secToMSec = 1000U;      //!< Conversion factor from second to milisecond.

  static const uint16_t cycleCntToMsec = 100U;  //!< Conversion factor from milisecond to number of counts i.e.- ms per cycle.

  /**
  * Matches the Strings passed
  *
  * @param[in] str1 - String to be compared
  * @param[in] str2 - String to be compared
  *
  * @return return true, if String is matching.
  */
  bool isTextMatch(const char_t* const str1, const char_t* const str2, const size_t maxNumberOfCharacters);

  /**
  * Returns the Side of VFW in string format
  *
  * @param[in] VfwSide - Id of VFW Side
  *
  * @return return VFW Side in String.
  */
  const char_t* getVfwSideString(const VFW_Side side);

  /*
  * Returns a pointer to the last occurrence of 'c' in 'string' or NULL if 'c' does not occur
  *
  * @param[in] string  the string to search in
  * @param[in] length  the max expected length of the string
  * @param[in] c       the character to search for
  *
  * @return a pointer to the last occurrence of 'c' in 'string' or NULL if 'c' does not occur
  */
  const char_t* findLast(const char_t* const string, const size_t length, const char_t c);

  /**
  * Extract the filename from the path.
  *
  * @param[in] path    pointer to the path of the file
  * @param[in] length  the max expected length of the string
  *
  * @return pointer to the filename.
  */
  const char_t* getFileNameFromPath(const char_t* const path, const size_t length);

  /**
  * Raise the VFW Halt for already instantiated constructor
  *
  * @param[in] Module_C -Module C of VFW halt
  * @param[in] lineNumber -line number
  * @param[in] String to Halt call
  *
  */
  void aosHalt(const char_t* const, const int32_t, const char_t * const);

  /**
  * Returns the highest of two unsigned 32-bit values.
  *
  * @param[in] a  the first value
  * @param[in] b  the second value
  *
  * @return The highest number of a and b
  */
  uint32_t maxU32(const uint32_t a, const uint32_t b);

  /**
  * Returns the lowest of two unsigned 32-bit values.
  *
  * @param[in] a  the first value
  * @param[in] b  the second value
  *
  * @return The lowest number of a and b
  */
  uint32_t minU32(const uint32_t a, const uint32_t b);

  /**
  * Function to get the UTC Time as a 32-bit integer.
  *
  * @param[out] timeUTC - Current UTC time
  */
  void getUTCTime(uint32_t &timeUTC);

  /**
  * Function to get the UTC Time as a 64-bit integer.
  *
  * @param[out] timeUTC - Current UTC time
  */
  void getUTCTime(uint64_t &timeUTC);

  /**
  * Function to get the UTC Time in milliseconds.
  *
  * @return UTC Time in milliseconds
  */
  uint64_t getUTCTimeInMs();

  /**
  * Function to set the updated UTC Time as system Time.
  *
  * @param[in] timeUTC - Time to set as UTC time
  *
  * @return true if UTC time set successfully
  */
  bool setUTCTime(const uint32_t timeUTC);

  /**
  * debugInfo
  *
  * Writes the Debug Information onto standard output with buffer flush.
  *
  * @param[in]   text   The text to be flushed to standard output.
  */
  void debugInfo(const char_t* const text);

  /**
  * Template function for dynamic cast. Issuing Safety halt if the cast fails
  * This will eliminate the need to check if the result is valid from the calling function
  * From: is the type that will be casted
  * To: is the type that the function will cast to
  *
  * @param[in] base   Instance of the type that shall be casted
  * @param[in] file   Pointer to the filename from where the function is called.
  * @param[in] line   Line number from where the function is called.
  */
  template <class From, class To>
  To dynamicCast(From base, const char_t* const file, const int32_t line)
  {
    To result = dynamic_cast<To>(base); //lint !e929 dynamic_cast is ok
    if (result == static_cast<To>(NULL))
    {
      ATC::aosHalt(file, line, "Dynamic cast error!");
    }
    return result;
  }

}

#endif
