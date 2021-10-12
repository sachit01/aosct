/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the BS Wrapper functions used by SPL-library.
*
*******************************************************************************
* Revision History
*
* Date      Sign      Change description
*
* 170224    rquensel  Created
*******************************************************************************/

/*******************************************************************************
* Lint directives
*******************************************************************************/
//lint -esym(714,bs_*) Needed as glue towards the SPL library
//lint -esym(759,bs_*) Needed as glue towards the SPL library
//lint -esym(765,bs_*) Needed as glue towards the SPL library

/*******************************************************************************
* Includes
*******************************************************************************/

#include "atc_util.hpp"
#include "abstract_log_handler.hpp"

#include "bs_wrapper.hpp"

#ifdef WIN32
#include <time.h>
#else
#include <vfw_time.h>
#endif
#include <vfw_types.h>
#include <vfw_identity.h>
#include <vfw_string.h>

#ifndef WIN32
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/time.h>
#endif
#include <stdio.h>

#ifndef WIN32
#include <unistd.h>
#endif

#include <stdarg.h> //lint !e829 Needed as glue towards the SPL library

/*******************************************************************************
* Macros
*******************************************************************************/

/*******************************************************************************
* Declarations and Definitions
*******************************************************************************/

#ifdef WIN32
extern "C" int64_t vfwGetReferenceTime(void);
extern "C" void vfwGetTimeOfDay(struct timespec * const timespec_p, struct tm * const tm_p);
#endif

namespace
{
  const int16_t OK_16 = 0;
  const int16_t ERROR_16 = -1;
  const uint8_t maxLogLength = 200U;
  //lint -esym(551,*::maxLogLength) Lint is wrong, this constant *is* used

  const char_t bswErrorCodePtrNullStr[] = "0xC307";

  void
  wrapperOutputFailureInfo(
    const char_t fileName[],
    const int32_t lineNo,
    const FailureCode code,
    const FailureMsgType msg);

}

namespace
{

  /******************************************************************************
  * wrapperOutputFailureInfo
  *
  ******************************************************************************/
  void
  wrapperOutputFailureInfo(
    const char_t fileName[],
    const int32_t lineNo,
    const FailureCode code,
    const FailureMsgType msg)
  {

    if ((code == NULL) ||
      (msg == NULL))
    {
      // Should be handled in bs_minorFailure and bs_fatalFailure functions. If we get here anyway, call VFW_HALT.
      // This is a defensive check that should not occur, so it is okay to call VFW_HALT also in WIN32 builds in this location
      ATC::aosHalt(__FILE__, __LINE__, "Null pointer in wrapperOutputFailureInfo()");
    }
    else
    {
      //M_MSG
      char_t send_data[maxLogLength];
      (void)bs_safe_sprintf(&send_data[0], sizeof(send_data), " Failure code: %s fileName: %s  fileLine: %d msg: %s", code, fileName, lineNo, msg);

      ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, &send_data[0], "SPL", __FILE__, __LINE__);
    }
  }
}

extern "C"
{
  /******************************************************************************
  * bs_safe_sprintf
  *  ABSTRACT : bs_safe_sprintf This is a function for backwards compatibility
  *             that is inherited from TBSW. The vsnprintf POSIX function is
  *             used with implementation respecting the usage rules from GSP
  *             Coding Conventions (3NSS012264D0048).
  *
  *             vsnprintf() is a category 3 function according to 3NSS012264D0048. It is used
  *             in order to provide flexible string formatting functionality for logging purposes.
  *             bs_safe_sprintf() enables backwards compatibility without having to change
  *             all calls to the function, as well as checking of some of the usage rules
  *             for vsnprintf() as specified in 3NSS012264D0048. Mostly these are the same as
  *             for fprintf, as demanded by vsnprinft() usage rule number 1.
  *----------------------------------------------------------------------------
  *  USAGE:   : The caller of bs_safe_sprintf needs to ensure Usage rules 2, 3 and 4 for fprintf,
  *             that are referenced from vsnprintf() Usage rule 1.
  *
  *               2. The format string shall be a string constant, so compilers may be able to perform
  *                  additional parameter checks, if available. If it would be any kind of non-string
  *                  constant, the call would not be predictable, as it's not known, how many % or * are
  *                  contained in the format string.
  *
  *               3. The format string shall only contain simple formatting entities. Especially forbidden
  *                  are %n (write- accesses an int32_t *), %m$ and *m$ (there must be no gaps and the
  *                  order of % entities is not 1:1 matchable with the order of parameters). Furthermore,
  *                  the use of * is not allowed.
  *
  *               4. Those functions shall only be used for logging purposes that can be deactivated
  *                  using either a define or a runtime switch from a configuration, for instance.
  *----------------------------------------------------------------------------
  ******************************************************************************/

  int32_t
  bs_safe_sprintf(char_t* const dst_buf,
    const uint32_t dst_buf_len,
    const char_t* const fmt, ...) //lint !e1916 !e1960 Needed as glue towards the SPL library
  {
    int32_t retVal = 0;

    //Checking fprintf() Usage rule 1 (referenced by vsnprintf Usage rule 1): No NULL pointers allowed
    if ((dst_buf == NULL) ||
      (fmt == NULL))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Null pointer in bs_safe_sprintf");
    }
    else
    {
      va_list args;
      (void)va_start(args, fmt); //lint !e530 'args' is initialized by va_start()

      //lint -e{586} vsnprintf() is used according to 3NSS012264D0048
      retVal = vsnprintf(dst_buf, dst_buf_len, fmt, args);

      // Applying vsnprintf() Usage rule 2: Clean up using va_end(args)
      va_end(args);
    }

    return retVal;
  }


  /******************************************************************************
  * bs_psCyclesGet
  *
  ******************************************************************************/
  void
  bs_psCyclesGet(
      uint32_t* const timeA,
      uint32_t* const timeB)
  {
    if ((timeA == NULL) ||
      (timeB == NULL))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Null pointer in bs_psCyclesGet");
    }
    else
    {
      /* We just need a millisecond count.  On PC this is no problem */
      /* The real TBSW implementation is obviously different. */
      const uint32_t tmpTime = static_cast<uint32_t>(vfwGetReferenceTime());
      *timeA = tmpTime;
      *timeB = ~tmpTime;
    }
  }



  /******************************************************************************
  * bs_getClock
  *
  ******************************************************************************/
  int16_t
  bs_getClock(PosixTimeStruct* const posixTime)
  {
    int16_t retValue = OK_16;

    if (posixTime == NULL)
    {
      ATC::aosHalt(__FILE__, __LINE__, "Null pointer in bs_getClock");
      retValue = ERROR_16;
    }
    else
    {
      struct timespec tspec;
      struct tm       mytm;

      vfwGetTimeOfDay(&tspec, &mytm);

      posixTime->sec = static_cast<uint32_t>(tspec.tv_sec);
      posixTime->nanosec = static_cast<uint32_t>(tspec.tv_nsec);
    }

    return retValue;
  }


  /******************************************************************************
  * bs_minorFailureA
  *
  ******************************************************************************/
  void
    bs_minorFailureA(
      const char_t fileName[],
      const int32_t lineNo,
      const FailureCode code,
      const FailureMsgType msg)
  {
    if ((code == NULL) &&
      (msg == NULL))
    {
      ATC::aosHalt(fileName, lineNo, "Failure msg ptr is null");
    }
    else if (msg == NULL)
    {
      ATC::aosHalt(fileName, lineNo, "Failure msg ptr is null");
    }
    else if (code == NULL)
    {
      wrapperOutputFailureInfo(&fileName[0], lineNo, &bswErrorCodePtrNullStr[0], msg);
    }
    else
    {
      wrapperOutputFailureInfo(&fileName[0], lineNo, code, msg);
    }

  }



  /******************************************************************************
  * bs_fatalFailureA
  *
  ******************************************************************************/
  void
  bs_fatalFailureA(
    const char_t fileName[],
    const int32_t lineNo,
    const FailureCode code,
    const FailureMsgType msg)
  {

    if ((code == NULL) &&
      (msg == NULL))
    {
      wrapperOutputFailureInfo(fileName, lineNo, &bswErrorCodePtrNullStr[0], "Failure msg ptr is null");
    }
    else if (msg == NULL)
    {
      wrapperOutputFailureInfo(fileName, lineNo, code, "Failure msg ptr is null");
    }
    else if (code == NULL)
    {
      wrapperOutputFailureInfo(fileName, lineNo, &bswErrorCodePtrNullStr[0], msg);
    }
    else
    {
      wrapperOutputFailureInfo(fileName, lineNo, code, msg);
    }

#ifndef WIN32
    // usleep() is a category 3 POSIX function
    // Usage of usleep is justified by the fact that system is about to be shut down. The delay before shut down is done in
    // order to increase the chances of diagnostic messages to be recorded before shutting down. The additional delay of 10ms
    // before VFW_HALT is called is deemed negligible.
    // There are no specific usage rules for usleep()
    //lint -e{586} usleep is needed here
    static_cast<void>(usleep(10U * 1000U));

#endif
    ATC::aosHalt(__FILE__, __LINE__, msg);
  }


  /**************************************************************************
  *
  * bs_memcopyA
  *
  *-------------------------------------------------------------------------
  * Usage Rules originating from 3NSS012264D0048 to be respected by caller:
  *
  * 3. The caller must ensure that destPtr and srcPtr point at strings of at least noOfBytes bytes
  *
  *-------------------------------------------------------------------------
  * Ex.
  * bs_memcopyA( destPtr, srcPtr, noOfBytes);
  *                 /       |            \
  * Ptr to dest. area  Ptr to src. area  Number of bytes to copy
  *************************************************************************/
  void
  bs_memcopyA(void* const destPtr, const void* const srcPtr, const uint32_t noOfBytes)
  {
    if ((destPtr == NULL) ||
      (srcPtr == NULL))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Null pointer in bs_memcopyA");
    }
    else
    {
      if (noOfBytes != 0U)
      {
        // Usage rules for Category 1 function memmove (rules 4 and 5 are not relevant here):
        // 1. No NULL-pointer may be passed to memmove() - verified above
        // 2. Return code is ignored
        // 3. The caller must ensure, src and dest point to at least n bytes
        //    - To be verified by the caller of bs_memcopyA, see notes in header and in SMDS
        memmove(destPtr, srcPtr, noOfBytes);
      }
    }
  }

  /**************************************************************************
  * bs_memset8A
  *
  *************************************************************************/
  void
  bs_memset8A(void* const destPtr, const uint8_t val, const uint32_t noOf8bitWords)
  {
    if (destPtr == NULL)
    {
      ATC::aosHalt(__FILE__, __LINE__, "Null pointer in bs_memset8A");
    }
    else
    {
      memset(destPtr, static_cast<int32_t>(val), noOf8bitWords);
    }
  }


  /**************************************************************************
  * bs_memcompA
  *
  *************************************************************************/
  int32_t
  bs_memcompA(const void* const data1Ptr, const void* const data2Ptr, const uint32_t noOfBytes)
  {
    int32_t retVal = 0;
    if ((data1Ptr == NULL) ||
      (data2Ptr == NULL))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Null pointer in bs_memcompA");
      retVal = -1;
    }
    else
    {
      if (noOfBytes != 0U)
      {
        // Usage rules for Category 1 memcmp:
        // 1. No NULL-pointer may be passed to memcmp() - verified above
        // 2. Return value is to be checked against < 0, == 0 or > 0 - handled below
        // 3. To be verified by the caller, see comment in header and in SMDS
        retVal = memcmp(data1Ptr, data2Ptr, noOfBytes);
      }
      if (retVal != 0)
      {
        retVal = 1;
      }
    }
    return retVal;
  }


  /**************************************************************************
  * bs_strncmpA
  *
  *************************************************************************/
  int32_t
  bs_strncmpA(const char_t* const data1Ptr, const char_t* const data2Ptr, const uint32_t maxLen)
  {
    int32_t retVal = -2; // This value shall never be returned
    if ((data1Ptr == NULL) ||
      (data2Ptr == NULL))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Null pointer in bs_strncmpA");
      retVal = -1;
    }
    else
    {
      if (maxLen == 0U)
      {
        retVal = 0;
      }
      else
      {
        retVal = strncmp(data1Ptr, data2Ptr, maxLen);
      }
    }
    return retVal;
  }


  /**************************************************************************
  *
  * bs_strncpyA
  *
  *************************************************************************/
  char_t*
  bs_strncpyA(char_t* const destPtr, const char_t* const srcPtr, const uint32_t maxLen)
  {
    if ((destPtr == NULL) ||
      (srcPtr == NULL))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Null pointer in bs_strncpyA");
    }
    else
    {
      if (maxLen != 0U)
      {
        static_cast<void>(vfw_strlcpy(destPtr, srcPtr, maxLen));
      }
    }
    return destPtr;
  }
}

/*************************** end of file **************************************/
