#ifndef BsWrapper_hpp
#define BsWrapper_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines BS Wrapper functions used by SPL-library.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-12-06    rquensel    Created
*
*******************************************************************************/

extern "C"
{
  /**
  * time structure for use with bs_getClock()
  * Stored value shall be the number of seconds and nanoseconds
  * from January 1st 1970, 00:00.00.
  */
  typedef struct posix_time_str
  {
    /* interval = sec*10**9 + nanosec */
    uint32_t sec;     /**<PosixTimeStruct::sec*/
    uint32_t nanosec; /**<PosixTimeStruct::nanosec*/

  } PosixTimeStruct;

  /**
  * FailureMsgType, 52 characters + 0-byte
  */
  typedef char_t FailureMsgType[53U];

  /**
  * FailureCode, 8 characters + 0-byte
  */
  typedef char_t FailureCode[9U];



  /**
  * bs_safe_sprintf is a restricted implementation of the C-standard function sprintf().
  * It does not provide all of the conversions in the C-standard, only a subset
  * of these. However, the implementation is much safer, as the number of characters
  * is monitored.
  *
  * @param [out] dst_buf    - The buffer used for output.
  * @param [in] dst_buf_len - Max no of characters to write (including the terminating NULL-character).
  * @param [in] fmt         - A format string.
  * @param [in] ...         - Any number/any type of arguments (specified by field tokens in the format string).
  * @return Number of characters written to 'dst_buf' (excluding the terminating NULL-character)
  * or a negative value if an error occurred.
  */
  int32_t
  bs_safe_sprintf(char_t* const dst_buf,
      const uint32_t dst_buf_len,
      const char_t* const fmt, ...); //lint !e1916 !e1960 Needed as glue towards the SPL library

  /******************************************************************************
  * bs_psCyclesGet
  * Get the global reference time value. The value received
  * here may be different from the time value provided when a vital
  * task function is called.
  * The provided PlatformTime parameters are assigned the reference time
  * value, in A- and B-format (inverted).
  * @param [out] timeA    - Time variable to store the output
  * @param [out] timeB    - Time variable to store the output (inverted value)
  ******************************************************************************/
  void
  bs_psCyclesGet(
    uint32_t* const timeA,
    uint32_t* const timeB);


  /******************************************************************************
  * bs_getClock
  * Get the current calendar time, using seconds/nanoseconds format
  * @param [out] posixTime - The current calendar time, using seconds/nanoseconds format
  * @return 0 if successful
  ******************************************************************************/
  int16_t
  bs_getClock(PosixTimeStruct* const posixTime);


  /******************************************************************************
  * bs_minorFailureA
  *
  * The failure information is written into the failure log and execution continues.
  * @param [in] fileName - File name
  * @param [in] lineNo   - Line number
  * @param [in] code     - Error code string
  * @param [in] msg      - Error message string
  *
  ******************************************************************************/
  void
  bs_minorFailureA(
    const char_t fileName[],
    const int32_t lineNo,
    const FailureCode code,
    const FailureMsgType msg);


  /******************************************************************************
  * bs_fatalFailureA
  *
  * This function is the final exit point for any severe error in an vital application
  * The function calls to log to send an error message, sleeps for 10 milliseconds
  * and then calls AOS halt function causing a final terminate of the application.
  * @param [in] fileName - File name
  * @param [in] lineNo   - Line number
  * @param [in] code     - Error code string
  * @param [in] msg      - Error message string
  *
  ******************************************************************************/
  void
  bs_fatalFailureA(
    const char_t fileName[],
    const int32_t lineNo,
    const FailureCode code,
    const FailureMsgType msg);


  /******************************************************************************
  * bs_memcopyA
  *
  * The function copies the given number of bytes from memory source area destination area.
  * @param [out] destPtr  - Destination area
  * @param [in] srcPtr    - Source area
  * @param [in] noOfBytes - Number of bytes to copy
  *
  ******************************************************************************/
  void
  bs_memcopyA(void* const destPtr, const void* const srcPtr, const uint32_t noOfBytes);


  /**************************************************************************
  * bs_memset8A
  *
  * Fills the destination memory area with a constant byte value provided.
  *
  * @param [out] destPtr       - Destination area
  * @param [in]  val           - Value to fill
  * @param [in]  noOf8bitWords - Number of bytes to fill
  *
  *************************************************************************/
  void
  bs_memset8A(void* const destPtr, const uint8_t val, const uint32_t noOf8bitWords);

  /**************************************************************************
  *
  * bs_memcompA
  *
  * This function compare two memory areas for equality
  *
  * @param [in] data1Ptr - Area of at least noOfBytes bytes
  * @param [in] data2Ptr - Area of at least noOfBytes bytes
  * @param [in] noOfBytes - Number of bytes to compare
  *
  * @return If the memory areas are equal the function returns zero else
  *         the return value is different from zero.
  *************************************************************************/
  int32_t
  bs_memcompA(const void* const data1Ptr, const void* const data2Ptr, const uint32_t noOfBytes);


  /**************************************************************************
  *
  * bs_strncmpA
  *
  * This functions compare two strings for equality
  * @param [in] data1Ptr - Area of at least noOfBytes bytes
  * @param [in] data2Ptr - Area of at least noOfBytes bytes
  * @param [in] maxLen   - Number of characters to compare
  * @return < 0 if string1 is less than string2,
  *         = 0 if string1 is equal to string2,
  *         > 0 if string1 is greater than string2
  *
  *************************************************************************/
  int32_t
  bs_strncmpA(const char_t* const data1Ptr, const char_t* const data2Ptr, const uint32_t maxLen);

  /**************************************************************************
  *
  * bs_strncpyA
  *
  * Copy a string at most maxLen number of bytes. Guarantees to NUL-terminate
  * the destination string for all strings where the given size is non-zero.
  *
  * @param [out]  destPtr - The destination.
  * @param [in]   srcPtr -  The source.
  * @param [in]   maxLen -  The size of dst.
  * @return       The destination string.
  *************************************************************************/
  char_t*
  bs_strncpyA(char_t* const destPtr, const char_t* const srcPtr, const uint32_t maxLen);
}
#endif
