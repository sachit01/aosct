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
* 2016-08-23    akushwah    Created
* 2016-09-26    spandita    updated the vfwSideStr()
* 2016-11-10    adgupta     Added function to get short name with Component id
* 2016-12-21    spandita    Added the halt function for constructor
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_util.hpp"
#include <cstring>
#ifndef __GNUG__
#include "time.h"
extern "C" void vfwGetTimeOfDay(struct timespec * const timespec_p, struct tm * const tm_p);
#define __attribute__(A) /* If not GNU, do nothing. This must be defined before including vfw_halt.h */
#else
#include <vfw_time.h>
#endif
#include <vfw_halt.h>




/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATC
{
  /******************************************************************************
  * isTextMatch
  ******************************************************************************/
  bool isTextMatch(const char_t* const str1, const char_t* const str2, const size_t maxNumberOfCharacters)
  {
#ifdef __GNUG__
    return strncasecmp(str1, str2, maxNumberOfCharacters) == 0;
#else
    return _strnicmp(str1, str2, maxNumberOfCharacters) == 0;
#endif
  }

  /******************************************************************************
  * getVfwSideString
  ******************************************************************************/
  const char_t* getVfwSideString(const VFW_Side side)
  {
    const char_t* retVal;
    switch (side)
    {
    case VFW_NO_SIDE:
      retVal = "No Side";
      break;
    case VFW_A_SIDE:
      retVal = "A";
      break;
    case VFW_B_SIDE:
      retVal = "B";
      break;
    case VFW_C_SIDE:
      retVal = "C";
      break;
    default:
      retVal = "?";
      break;
    }
    return retVal;
  }

  /******************************************************************************
  * findLast
  ******************************************************************************/
  const char_t* findLast(const char_t* const string, const size_t length, const char_t c)
  {
    const char_t* result = static_cast<const char_t*>(NULL);
    const char_t* p = string;

    for (size_t i = 0U; i < length; ++i)
    {
      if (*p == '\0')
      {
        break;
      }

      if (*p == c)
      {
        result = p;
      }

      ++p;
    }

    return result;
  }

  /******************************************************************************
  * getFileNameFromPath
  *
  ******************************************************************************/
  const char_t* getFileNameFromPath(const char_t* const path, const size_t length)
  {
    //extract the filename from the full path
    const char_t* filename = static_cast<char_t*>(NULL);

    if (path != static_cast<char_t*>(NULL))
    {
      //find the position of the delimiter.. can be \\ or /
      const char_t* pDelimeter = findLast(path, length, '\\');
      if (pDelimeter != static_cast<char_t*>(NULL))
      {
        filename = pDelimeter + 1;
      }
      else
      {
        pDelimeter = findLast(path, length, '/');
        if (pDelimeter != static_cast<char_t*>(NULL))
        {
          filename = pDelimeter + 1;
        }
        //delimiter not found.. set filename to the full path.
        else
        {
          filename = path;
        }
      }
    }

    return filename;
  }

  /******************************************************************************
  * aosHalt
  *******************************************************************************/
  void aosHalt(const char_t* const module_C, const int32_t lineNumber, const char_t * const errStr)
  {
    // Please note that although VFW_HALT adds file name and line number
    // we add them here as well, because VFW_HALT shows the wrong line number

    //lint -e{717} -e{909} -e{970} Side effect of external macro
    VFW_HALT(("at line %d in %s: %s", lineNumber, module_C, errStr));
  }

  /******************************************************************************
  * maxU32
  ******************************************************************************/
  uint32_t maxU32(const uint32_t a, const uint32_t b)
  {
    uint32_t result;

    if (a > b)
    {
      result = a;
    }
    else
    {
      result = b;
    }

    return result;
  }

  /******************************************************************************
  * minU32
  ******************************************************************************/
  uint32_t minU32(const uint32_t a, const uint32_t b)
  {
    uint32_t result;

    if (a > b)
    {
      result = b;
    }
    else
    {
      result = a;
    }

    return result;
  }

  /******************************************************************************
  * getUTCTime
  ******************************************************************************/
  void getUTCTime(uint32_t &timeUTC)
  {
    struct timespec utcTimeStamp;

    vfwGetTimeOfDay(&utcTimeStamp, static_cast<tm *>(NULL));

    timeUTC = static_cast<uint32_t>(utcTimeStamp.tv_sec);
  }

  /******************************************************************************
  * timeToUInt64
  ******************************************************************************/
  uint64_t timeToUInt64(const time_t timeT)
  {
    uint64_t time64;
    volatile size_t sizeOfTime = sizeof(timeT);

    if (sizeOfTime == 4U)
    {
      // Cast to uint32_t first, to avoid sign extension
      time64 = static_cast<uint64_t>(static_cast<uint32_t>(timeT));
    }
    else
    {
      //lint -e{571} Not suspicious, since time_t is 64 bit in this case
      time64 = static_cast<uint64_t>(timeT);
    }

    return time64;
  }

  /******************************************************************************
  * getUTCTime
  ******************************************************************************/
  void getUTCTime(uint64_t &timeUTC)
  {
    struct timespec utcTimeStamp;

    vfwGetTimeOfDay(&utcTimeStamp, static_cast<tm *>(NULL));

    timeUTC = timeToUInt64(utcTimeStamp.tv_sec);
  }

  /******************************************************************************
  * getUTCTimeInMs
  ******************************************************************************/
  uint64_t getUTCTimeInMs()
  {
    struct timespec utcTimeStamp;

    vfwGetTimeOfDay(&utcTimeStamp, static_cast<tm *>(NULL));

    uint32_t milliSeconds = static_cast<uint32_t>(utcTimeStamp.tv_nsec) / 1000000U;

    return (timeToUInt64(utcTimeStamp.tv_sec) * 1000U) + milliSeconds;
  }

  /******************************************************************************
  * setUTCTime
  ******************************************************************************/
  bool setUTCTime(const uint32_t timeUTC)
  {
    bool retVal = false;

    if (VFW_A_SIDE == vfwGetSide())
    {
      char_t commandStr[128] = { '\0' };
      size_t commandStrSafeSize = sizeof(commandStr) - 1U;

      //lint -e{586} snprintf is needed here
      int32_t ret = snprintf(&commandStr[0], commandStrSafeSize, "/opt/bin/aos/settimeofday -t %u", timeUTC);

      if ((ret > 0) && (static_cast<size_t>(ret) < commandStrSafeSize))
      {
        //lint -e{586} system() is used according to 1DOC-1015361
        ret = system(&commandStr[0]);
        if (0 == ret)
        {
          retVal = true;
        }
      }
    }
    else
    {
      // Always true on B-side
      retVal = true;
    }

    return retVal;
  }

  /******************************************************************************
  * traceInfo
  ******************************************************************************/
  void debugInfo(const char_t* const text)
  {
#ifndef DISABLE_DEBUG_INFO // Needed for disabling the printout, as specified in 3NSS012264D0048
    if (text != NULL)
    {
      struct tm utcTime;
      vfwGetTimeOfDay(static_cast<timespec *>(NULL), &utcTime);

      //lint -e{586} fprintf() is used according to 3NSS012264D0048
      fprintf(stdout, "%04d-%02d-%02d %02d:%02d:%02d %s",
        utcTime.tm_year + 1900, utcTime.tm_mon + 1, utcTime.tm_mday, utcTime.tm_hour, utcTime.tm_min, utcTime.tm_sec, text);

      //lint -e{586} fflush() is used according to 3NSS012264D0048
      static_cast<void>(fflush(stdout));
    }
#endif
  }
}
