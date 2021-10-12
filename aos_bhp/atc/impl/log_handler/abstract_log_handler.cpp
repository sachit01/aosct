/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Replace this text with a short description of the classes etc implemented.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-18    akushwah    Updated after review
* 2016-08-09    akushwah    Initial Implementation
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-20    akushwah    Added the addHeaderToNJRUBuffer function
* 2016-09-23    arastogi    Removed ATC::
* 2016-09-23    bhermans    Add header for NJRU also in logEvent()
* 2016-10-24    nsyed       Use access functions to get the config parameters
* 2016-11-03    adgupta     Updated after Log Handler Redesign
* 2016-11-03    adgupta     Improved Console call for logstat
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include <time.h>
#include "abstract_log_handler.hpp"
#include "abstract_basic_ip.hpp"
#include "abstract_console.hpp"
#include "abstract_config_base.hpp"
#include "dia_message.h"
#include "dia_send.h"
#include "atc_util.hpp"
#include <vfw_string.h>
#include <cstdio>
#ifndef _DISPATCHER
#include <vfw_checkpoints.h>
#endif

#ifndef WIN32
#include <vfw_time.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#else
#include <ws2tcpip.h>

extern "C" int64_t vfwGetReferenceTime(void);
extern "C" void vfwGetTimeOfDay(struct timespec * const timespec_p, struct tm * const tm_p);
#endif

/******************************************************************************
* LINT SUPPRESSIONS
******************************************************************************/
//lint -esym(586,snprintf) snprintf is needed here

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/

/******************************************************************************
* Basic implementation of inet_ntop() (which is missing in mingw32)
******************************************************************************/
#if defined(WIN32) && defined(__GNUG__) && defined(_SIL)
static const char* inet_ntop(int family, const void* pAddr, char* pStringBuf, size_t stringBufSize)
{
  const char* result = NULL;

  if (family == AF_INET)
  {
    const uint8_t* address = static_cast<const uint8_t*>(pAddr);

    if (snprintf(pStringBuf, stringBufSize, "%d.%d.%d.%d", address[0], address[1], address[2], address[3]) > 0)
    {
      result = pStringBuf;
    }
  }

  return result;
}
#endif

namespace ATC
{

  /******************************************************************************
  * Constructor
  ******************************************************************************/
  AbstractLogHandler::AbstractLogHandler() : ProcComponent(atcLogHandlerId,"LogHandler", "LH"),
    initDone(false),
    initNjruConnectionDone(false),
#ifndef _DISPATCHER
    initRuConnectionDone(false),
#endif
    initBdsDone(false)
  {
    if (coreLogHandlerInstancePtr != 0)
    {
      // Invalid Constructor Error 
      aosHalt(__FILE__, __LINE__, "Log handler constructor already instantiated");
    }
    // Setup single instance pointer for core access
    coreLogHandlerInstancePtr = this;
  }

  /******************************************************************************
  * init
  ******************************************************************************/
  bool AbstractLogHandler::init(void)
  {

    if (!initDone)
    {
      messageSentNjruCounter = 0U;
      messageNotSentNjruCounter = 0U;
      messageSentRuCounter = 0U;
      messageNotSentRuCounter = 0U;
      messageSentBdsCounter = 0U;
      messageNotSentBdsCounter = 0U;
      bdsLevel = AbstractConfigBase::basePtr()->getBdsLevel();
      njruLevel = AbstractConfigBase::basePtr()->getNjruLevel();
      bdsSeqNumber = 0U;
      njruBuffer.init();
      ruBuffer.init();

      static_cast<void>(vfw_strlcpy(&msgSeparator[0], "\n", maxMsgSeparatorSize));
      msgSeparatorLength = static_cast<uint8_t>(strnlen(&msgSeparator[0], static_cast<size_t>(maxMsgSeparatorSize)));

      // Initialize a connection via BasicIp. IP address doesn't need to be populated as 
      // it is a host connection itself.      
      if (!initNjruConnectionDone)
      {
#ifndef _DISPATCHER
        if (vfwGetSide() == VFW_A_SIDE)
        {
#endif
          const uint16_t njruPortNum = AbstractConfigBase::basePtr()->getNjruPort();
          const char_t* njruIP = AbstractConfigBase::basePtr()->getNjruIp();

          initNjruConnectionDone = AbstractBasicIP::corePtr()->initConnection(AbstractBasicIP::connectionNJRU,
            AbstractBasicIP::ConnectionTypeTcpClient, njruIP, njruPortNum, maxNJRUBufferSize, maxNJRUBufferSize);
#ifndef _DISPATCHER
        }
        else
        {
          initNjruConnectionDone = true;
        }
#endif
      }


#ifndef _DISPATCHER
      if (!initRuConnectionDone)
      {
        if (vfwGetSide() == VFW_A_SIDE)
        {
          const uint16_t ruPortNum = AbstractConfigBase::basePtr()->getRuPort();
          const char_t* ruIP = AbstractConfigBase::basePtr()->getRuIp();

          initRuConnectionDone = AbstractBasicIP::corePtr()->initConnection(AbstractBasicIP::connectionRU,
            AbstractBasicIP::ConnectionTypeTcpClient, ruIP, ruPortNum, maxRUBufferSize, maxRUBufferSize);
        }
        else
        {
          initRuConnectionDone = true;
        }
      }
#endif

      if (!initBdsDone)
      {
        const uint16_t bdsPortNum = AbstractConfigBase::basePtr()->getBdsPort();

        struct addrinfo hints;
        struct addrinfo *res = static_cast<struct addrinfo*>(NULL);

        memset(&hints, 0, sizeof(hints));
        hints.ai_family = AF_INET;
        hints.ai_socktype = static_cast<int8_t>(SOCK_STREAM);
        hints.ai_flags = AI_CANONNAME;
#ifndef _SIL
        const char_t* const diaHostname = DIA_HOSTNAME;
#else
        const char_t* const diaHostname = "localhost";
#endif
        //lint -e{586} getaddrinfo() is used according to 3NSS012264D0048
        //lint -e{970} getaddrinfo() uses char, so we must use char
        const int32_t addrInfoErrorCode = getaddrinfo(diaHostname, static_cast<const char*>(NULL), &hints, &res);

        if (addrInfoErrorCode == 0)
        {
          sockaddr_in socketAddress;
          memmove(&socketAddress, res->ai_addr, sizeof(sockaddr_in));

          char_t bdsString[INET_ADDRSTRLEN];
          if (inet_ntop(AF_INET, &(socketAddress.sin_addr), &bdsString[0], static_cast<size_t>(INET_ADDRSTRLEN))
            != static_cast<char_t *>(NULL))
          {
            int16_t bdsInitStatus = diaInit(&bdsString[0], bdsPortNum);
            initBdsDone = (static_cast<int16_t>(DIA_SEND_OK) == bdsInitStatus);
          }
        }

        //lint -e{586} freeaddrinfo() is used according to 3NSS012264D0048
        freeaddrinfo(res);
      }

      initDone =
        initNjruConnectionDone &&
#ifndef _DISPATCHER
        initRuConnectionDone &&
#endif
        initBdsDone;
    }

    return initDone;
  }

  /******************************************************************************
  * run
  ******************************************************************************/
  void AbstractLogHandler::run(void)
  {
#ifndef _DISPATCHER
    static uint32_t cp = 0U; // Must be initialized to 0
    vfwVisitCheckPoint(&cp, "LH_run");
#endif

    //Write the collected log from all the components in N-JRU buffer onto the N-JRU.
    writeToNJRU();

    //Write the collected log from all the components in RU buffer onto the RU.
    writeToRU();
  }

  /******************************************************************************
  * getATPCompShortName
  ******************************************************************************/
  const char_t* AbstractLogHandler::getCompShortName(const ComponentIDType id) const
  {
    CompPtrIter itr = AbstractApplicationBase::corePtr()->getCompIterator();
    CompPtrIter itrEnd = AbstractApplicationBase::corePtr()->getCompIteratorEnd();
    const char_t* compName = "";

    while (itr != itrEnd)
    {
      if (id == (*itr)->getId())
      {
        compName = (*itr)->getShortName();
        break;
      }
      ++itr;
    }

    return compName;
  }

  /******************************************************************************
  * extractFileName
  ******************************************************************************/
  const char_t* AbstractLogHandler::extractFileName(const char_t* const str, const size_t length) const
  {
    const char_t* ptr = static_cast<char_t*>(NULL);

    if (NULL == str)
    {
      trace.write(1U, "Event handler sent a NULL file name pointer");
    }
    else
    {
      ptr = ATC::getFileNameFromPath(str, length);
    }

    return ptr;
  }
  /******************************************************************************
  * getEventTypeStr
  ******************************************************************************/
  const char_t* AbstractLogHandler::getEventTypeStr(const Event::EventType eventType) const
  {
    const char_t* eventTypeStr = "";
    switch (eventType)
    {
    case Event::EventTypeLog:
      eventTypeStr = "LOG";
      break;
    case Event::EventTypeSBReq:
      eventTypeStr = "SB";
      break;
    case Event::EventTypeEBReq:
      eventTypeStr = "EB";
      break;
    case Event::EventTypeStandstill:
      eventTypeStr = "SS";
      break;
    case Event::EventTypeSafeBrakeSB:
      eventTypeStr = "SBSB";
      break;
    case Event::EventTypeSafeBrakeEB:
      eventTypeStr = "SBEB";
      break;
    case Event::EventTypeSafetyHalt:
      eventTypeStr = "SH";
      break;
    default:
      eventTypeStr = "";
      break;
    }
    return eventTypeStr;
  }

  /******************************************************************************
  * writeEventToLogger
  ******************************************************************************/
  void AbstractLogHandler::writeEventToLogger(const Event& eventToLog, const char_t* const filepath, int32_t const line)
  {
    ComponentIDType id = eventToLog.getComponentId();

    const char_t* compShortName = getCompShortName(id);

    if (NULL != compShortName)
    {
      LineBuffer lineBuffer;

      const char_t *eventTypeStr = getEventTypeStr(eventToLog.getType());

      bool textAppended = addNJRUHeaderToBuffer(compShortName, eventTypeStr, lineBuffer);

      if (textAppended)
      {
        textAppended = lineBuffer.appendToBuffer(eventToLog.getText());

        if (textAppended)
        {
          //Append the Dynamic Text, if it is not NULL
          const char_t* dynamicText = eventToLog.getDynamicText();
          
          if (NULL != dynamicText)
          {
            textAppended = lineBuffer.appendToBuffer(dynamicText);
          }

          if (textAppended)
          {
            textAppended = lineBuffer.appendToBuffer(" File:");

            if (textAppended)
            {
              const char_t* fileText = extractFileName(filepath, maxSourceFilePathLength);

              if (fileText != NULL)
              {
                textAppended = lineBuffer.appendToBuffer(fileText);
              }
              else
              {
                textAppended = lineBuffer.appendToBuffer("NULL");
              }

              if (textAppended)
              {
                char_t lineText[32];
                const int32_t res = snprintf(&lineText[0], sizeof(lineText), " Line:%d%s", line, msgSeparator);

                if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(lineText)))
                {
                  textAppended = lineBuffer.appendToBuffer(&lineText[0]);
                }
                else
                {
                  textAppended = false;
                }

                if (textAppended)
                {
                  njruBuffer.appendToBuffer(lineBuffer.buffer());

                  //All good, increment counter.
                  messageSentNjruCounter++;

                  if (bdsLevel >= detailedLevel)
                  {
                    writeToBDS(lineBuffer.buffer());
                  }
                }
              }
            }
          }
        }
      }
    }
    else
    {
      trace.write(1U, "Component name calling Event does not exist.");
    }
  }

  /******************************************************************************
  * addNJRUHeaderToBuffer
  ******************************************************************************/
  bool AbstractLogHandler::addNJRUHeaderToBuffer(const char_t* const compName, const char_t* const eventTypeStr, LineBuffer& buffer) const
  {
    bool success = false;

    struct tm utcTime;
    struct timespec timeSpec;
    vfwGetTimeOfDay(&timeSpec, &utcTime);
    int32_t timeMillis = static_cast<int32_t>(timeSpec.tv_nsec / 1000000);

    LineBuffer positionBuffer;
    addPositionAndSpeedToBuffer(positionBuffer);

    LineBuffer appIdBuffer;
    addApplicationIdToBuffer(appIdBuffer);

    const char_t sideChar = getSideChar();

    //lint -e{586} snprintf is needed here
    char_t stringBuffer[80];
    const int32_t charsWritten = snprintf(&stringBuffer[0], sizeof(stringBuffer), "%02d:%02d:%02d.%03d %19s %4s %c %-4s %-4s: ",
      utcTime.tm_hour, utcTime.tm_min, utcTime.tm_sec, timeMillis, positionBuffer.buffer(),
      appIdBuffer.buffer(), sideChar, (compName == NULL) ? "" : compName, eventTypeStr);

    if ((charsWritten > 0) && (static_cast<size_t>(charsWritten) < sizeof(stringBuffer)))
    {
      success = buffer.appendToBuffer(&stringBuffer[0]);
    }
    else
    {
      trace.write(ATC::briefTrace, "N-JRU Buffer Error");
    }

    return success;
  }

  /******************************************************************************
  * addPositionAndSpeedToBuffer
  ******************************************************************************/
  void AbstractLogHandler::addPositionAndSpeedToBuffer(LineBuffer& /*buffer*/) const
  {
    // The default implementation does nothing
  }

  /******************************************************************************
  * interfaceIdToString
  ******************************************************************************/
  const char_t* AbstractLogHandler::interfaceIdToString(InterfaceId const ifc)
  {
    const char_t* ifcString;

    switch (ifc)
    {
    case Ifc_DMI:
      ifcString = "DMI";
      break;
    case Ifc_IO:
      ifcString = "IO";
      break;
    default:
      ifcString = "?";
    }

    return ifcString;
  }

  /******************************************************************************
  * addHeaderToRUBuffer
  ******************************************************************************/
  bool AbstractLogHandler::addHeaderToRUBuffer(InterfaceId const ifc, InterfaceDir const dir, char_t const fragmentType)
  {
    struct timespec timeSpec;
    struct tm utcTime;
    vfwGetTimeOfDay(&timeSpec, &utcTime);
    int32_t timeMillis = static_cast<int32_t>(timeSpec.tv_nsec) / 1000000;

    char_t dateTime[32];
    int32_t res = snprintf(&dateTime[0], sizeof(dateTime), "%04d-%02d-%02d %02d:%02d:%02d.%03d",
      utcTime.tm_year + 1900, utcTime.tm_mon + 1, utcTime.tm_mday, utcTime.tm_hour, utcTime.tm_min, utcTime.tm_sec, timeMillis);

    bool ok = false;

    if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(dateTime)))
    {
      const char_t* ifcString = interfaceIdToString(ifc);
      const char_t* dirString = (dir == Ifc_In) ? "IN" : "OUT";

      char_t buf[128];
      res = snprintf(&buf[0], sizeof(buf), "ATP main[0]: [ID 0 user.notice] ,0,A,%s,JRU_LOG,ATP,%s-%s,1,%c,",
        dateTime, ifcString, dirString, fragmentType);

      if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(buf)))
      {
        ruBuffer.appendToBuffer(&buf[0]);
        ok = true;
      }
    }

    return ok;
  }

  /******************************************************************************
  * writeToLog
  ******************************************************************************/
  void AbstractLogHandler::writeToLog(LogLevel const level, const char_t* const text, const char_t* const compName,
    const char_t* const filepath, const int32_t line)
  {
    if (static_cast<uint8_t>(level) <= njruLevel)
    {
      LineBuffer lineBuffer;

      bool textAppended = addNJRUHeaderToBuffer(compName, "", lineBuffer);

      if (textAppended)
      {
        textAppended = lineBuffer.appendToBuffer(text);

        if (textAppended)
        {
          writeToLogInternal(lineBuffer, filepath, line, static_cast<uint8_t>(level) <= bdsLevel);
        }
      }
    }
  }

  /******************************************************************************
  * writeToLog
  ******************************************************************************/
  void AbstractLogHandler::writeToLog(LogLevel const level, const char_t* const text, const uint32_t val, const char_t* const compName,
    const char_t* const filepath, const int32_t line)
  {
    if (static_cast<uint8_t>(level) <= njruLevel)
    {
      LineBuffer lineBuffer;

      bool textAppended = addNJRUHeaderToBuffer(compName, "", lineBuffer);

      if (textAppended)
      {
        textAppended = lineBuffer.appendToBuffer(text);

        char_t valStr[13];
        int32_t ret = snprintf(&valStr[0], sizeof(valStr), " %u", val);

        if (textAppended  &&  (ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(valStr)))
        {
          textAppended = lineBuffer.appendToBuffer(&valStr[0]);

          if (textAppended)
          {
            writeToLogInternal(lineBuffer, filepath, line, static_cast<uint8_t>(level) <= bdsLevel);
          }
        }
      }
    }
  }

  /******************************************************************************
  * writeToLog
  ******************************************************************************/
  void AbstractLogHandler::writeToLog(LogLevel const level, const char_t* const text, const int32_t val, const char_t* const compName,
    const char_t* const filepath, const int32_t line)
  {
    if (static_cast<uint8_t>(level) <= njruLevel)
    {
      LineBuffer lineBuffer;

      bool textAppended = addNJRUHeaderToBuffer(compName, "", lineBuffer);

      if (textAppended)
      {
        textAppended = lineBuffer.appendToBuffer(text);

        char_t valStr[12];
        int32_t ret = snprintf(&valStr[0], sizeof(valStr), " %d", val);

        if (textAppended  &&  (ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(valStr)))
        {
          textAppended = lineBuffer.appendToBuffer(&valStr[0]);

          if (textAppended)
          {
            writeToLogInternal(lineBuffer, filepath, line, static_cast<uint8_t>(level) <= bdsLevel);
          }
        }
      }
    }
  }

  /******************************************************************************
  * writeToLog
  ******************************************************************************/
  void AbstractLogHandler::writeToLog(LogLevel const level, const char_t* const text, const bool val, const char_t* const compName,
    const char_t* const filepath, const int32_t line)
  {
    if (static_cast<uint8_t>(level) <= njruLevel)
    {
      LineBuffer lineBuffer;

      bool textAppended = addNJRUHeaderToBuffer(compName, "", lineBuffer);

      if (textAppended)
      {
        textAppended = lineBuffer.appendToBuffer(text);

        if (textAppended)
        {
          const char_t* valStr = val ? "TRUE" : "FALSE";
          textAppended = lineBuffer.appendToBuffer(valStr);

          if (textAppended)
          {
            writeToLogInternal(lineBuffer, filepath, line, static_cast<uint8_t>(level) <= bdsLevel);
          }
        }
      }
    }
  }

  /******************************************************************************
  * writeToLogInternal
  ******************************************************************************/
  void AbstractLogHandler::writeToLogInternal(LineBuffer& lineBuffer, const char_t* const filepath, const int32_t line, const bool logToBDS)
  {
    bool textAppended;

    // Add Filename and linenumber?
    if (filepath != NULL)
    {
      textAppended = lineBuffer.appendToBuffer(" File:");

      if (textAppended)
      {
        const char_t* fileName = extractFileName(filepath, maxSourceFilePathLength);

        if (fileName != static_cast<const char_t*>(NULL))
        {
          textAppended = lineBuffer.appendToBuffer(fileName);
        }
        else
        {
          textAppended = lineBuffer.appendToBuffer("NULL");
        }

        if (textAppended)
        {
          char_t lineText[32];
          const int32_t res = snprintf(&lineText[0], sizeof(lineText), " Line:%d%s", line, msgSeparator);

          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(lineText)))
          {
            textAppended = lineBuffer.appendToBuffer(&lineText[0]);
          }
          else
          {
            textAppended = false;
          }
        }
      }
    }
    else
    {
      char_t lineText[5];
      const int32_t res = snprintf(&lineText[0], sizeof(lineText), "%s", msgSeparator);

      if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(lineText)))
      {
        textAppended = lineBuffer.appendToBuffer(&lineText[0]);
      }
      else
      {
        textAppended = false;
      }
    }

    // Add the log-text (with or without Filename and Linenumber)
    if (textAppended)
    {
      njruBuffer.appendToBuffer(lineBuffer.buffer());

      // All good, increment counter.
      messageSentNjruCounter++;

      if (logToBDS)
      {
        writeToBDS(lineBuffer.buffer());
      }
    }
  }

  /******************************************************************************
  * logFragmentToRU
  ******************************************************************************/
  bool AbstractLogHandler::logFragmentToRU(
    InterfaceId const ifc, InterfaceDir const dir, const uint8_t fragment[], uint32_t const length, char_t const fragmentType)
  {
    uint32_t i = 0U;

    bool ok = addHeaderToRUBuffer(ifc, dir, fragmentType);

    while (ok && (i < length))
    {
      char_t buf[40];

      if ((length - i) >= 16U)
      {
        const int32_t res = snprintf(&buf[0], sizeof(buf),
          "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x",
          fragment[i], fragment[i + 1U], fragment[i + 2U], fragment[i + 3U],
          fragment[i + 4U], fragment[i + 5U], fragment[i + 6U], fragment[i + 7U],
          fragment[i + 8U], fragment[i + 9U], fragment[i + 10U], fragment[i + 11U],
          fragment[i + 12U], fragment[i + 13U], fragment[i + 14U], fragment[i + 15U]);
        
        ok = ((res > 0) && (static_cast<size_t>(res) < sizeof(buf)));
        i += 16U;
      }
      else
      {
        const int32_t res = snprintf(&buf[0], sizeof(buf), "%02x", fragment[i]);
        ok = ((res > 0) && (static_cast<size_t>(res) < sizeof(buf)));
        i++;
      }

      if (ok)
      {
        ruBuffer.appendToBuffer(&buf[0]);
      }
    }

    if (ok)
    {
      ruBuffer.appendToBuffer(&msgSeparator[0]);
    }

    return ok;
  }

  /******************************************************************************
  * logRU
  ******************************************************************************/
  void AbstractLogHandler::logRU(
    InterfaceId const ifc, const InterfaceDir dir, const uint8_t message[], uint32_t const length)
  {
    bool ok = false;

    if (AbstractBasicIP::corePtr()->getConnectionStatus(AbstractBasicIP::connectionRU) ==
      AbstractBasicIP::ConnectionStatusConnected)
    {
      for (uint32_t startPos = 0U; startPos < length; startPos += maxRUFragmentSize)
      {
        const uint32_t fragmentSize = minU32(length - startPos, maxRUFragmentSize);

        char_t fragmentType;
        if (fragmentSize == length)
        {
          fragmentType = 'S';
        }
        else if (startPos == 0U)
        {
          fragmentType = 'F';
        }
        else if ((startPos + fragmentSize) < length)
        {
          fragmentType = 'C';
        }
        else
        {
          fragmentType = 'L';
        }

        ok = logFragmentToRU(ifc, dir, &message[startPos], fragmentSize, fragmentType);

        if (!ok)
        {
          break;
        }
      }

      if (!ok)
      {
        trace.write(1U, "Text Length is more than RU Buffer Empty Size");
      }
    }

    if (ok)
    {
      messageSentRuCounter++;
    }
    else
    {
      messageNotSentRuCounter++;
    }
  }

  /******************************************************************************
  * logRU
  ******************************************************************************/
  void AbstractLogHandler::logRU(
    InterfaceId const ifc, const InterfaceDir dir, uint8_t const channelId, bool const digitalValue)
  {
    bool ok = false;

    if (AbstractBasicIP::corePtr()->getConnectionStatus(AbstractBasicIP::connectionRU) ==
      AbstractBasicIP::ConnectionStatusConnected)
    {
      ok = addHeaderToRUBuffer(ifc, dir, 'S');

      if (ok)
      {
        char_t buf[32];
        const int32_t res = snprintf(&buf[0], sizeof(buf),
          "digital channel %u value %d", channelId, static_cast<int32_t>(digitalValue));

        ok = ((res > 0) && (static_cast<size_t>(res) < sizeof(buf)));

        if (ok)
        {
          ruBuffer.appendToBuffer(&buf[0]);
          ruBuffer.appendToBuffer(&msgSeparator[0]);
        }
      }

      if (!ok)
      {
        trace.write(1U, "Text Length is more than RU Buffer Empty Size");
      }
    }

    if (ok)
    {
      messageSentRuCounter++;
    }
    else
    {
      messageNotSentRuCounter++;
    }
  }

  /******************************************************************************
  * logRU
  ******************************************************************************/
  void AbstractLogHandler::logRU(
    InterfaceId const ifc, const InterfaceDir dir, uint8_t const channelId, uint16_t const analogValue)
  {
    bool ok = false;

    if (AbstractBasicIP::corePtr()->getConnectionStatus(AbstractBasicIP::connectionRU) ==
      AbstractBasicIP::ConnectionStatusConnected)
    {
      ok = addHeaderToRUBuffer(ifc, dir, 'S');

      if (ok)
      {
        char_t buf[32];
        const int32_t res = snprintf(&buf[0], sizeof(buf),
          "analog channel %u value %u", channelId, analogValue) != -1;

        ok = ((res > 0) && (static_cast<size_t>(res) < sizeof(buf)));

        if (ok)
        {
          ruBuffer.appendToBuffer(&buf[0]);
          ruBuffer.appendToBuffer(&msgSeparator[0]);
        }
      }

      if (!ok)
      {
        trace.write(1U, "Text Length is more than RU Buffer Empty Size");
      }
    }

    if (ok)
    {
      messageSentRuCounter++;
    }
    else
    {
      messageNotSentRuCounter++;
    }
  }

  /******************************************************************************
  * consoleCall
  ******************************************************************************/
  bool AbstractLogHandler::consoleCall(const uint32_t argc, const ConsoleArguments argv)
  {
    bool retFlag = false;

    //tempBufferSize size is 30 as it contain the Concatenated string with value which will be displayed on console 
    static const uint8_t tempBufferSize = 30U;

    char_t valueTextSentNJRU[sizeOfDigitsInUnSignedValue + 1U];
    char_t valueTextSentRU[sizeOfDigitsInUnSignedValue + 1U];
    char_t valueTextSentBDS[sizeOfDigitsInUnSignedValue + 1U];

    char_t njruLevelBuffer[tempBufferSize] = "N-JRU Level =";
    char_t bdsLevelBuffer[tempBufferSize] = "BDS Level =";
    char_t valueTextNJRULevel[sizeOfDigitsInUnSignedValue];
    char_t valueTextBDSLevel[sizeOfDigitsInUnSignedValue];

    // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
    if (isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
    {
      const char_t* const toWrite =
        "logstat       To get information about all the messages received and send from\n"
        "              Log Handler Component\n"
        "loglevel      To get the current NJRU Level & BDS Level\n"
        "loglevel X Y  To set the NJRU Level & BDS Level, where X = NJRU Level and \n"
        "              Y = BDS Level";

      AbstractConsole::corePtr()->writeWithNewline(toWrite);
      retFlag = false;
    }
    else
    {
      bool textMatch = isTextMatch(&argv[0][0], "logstat", sizeof("logstat"));

      if (textMatch && (argc == 1U))
      {
        const char_t* const toWrite = "Messages(count) sent to Destinations:\n"
                                      "NJRU       RU         BDS";
        ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);

        int32_t res = snprintf(&valueTextSentNJRU[0], sizeof(valueTextSentNJRU), "%-10u ", messageSentNjruCounter);
        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(valueTextSentNJRU)))
        {
          ATC::AbstractConsole::corePtr()->write(&valueTextSentNJRU[0]);
        }
        
        res = snprintf(&valueTextSentRU[0], sizeof(valueTextSentRU), "%-10u ", messageSentRuCounter);
        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(valueTextSentRU)))
        {
          ATC::AbstractConsole::corePtr()->write(&valueTextSentRU[0]);
        }

        res = snprintf(&valueTextSentBDS[0], sizeof(valueTextSentBDS), "%-10u\n", messageSentBdsCounter);
        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(valueTextSentBDS)))
        {
          ATC::AbstractConsole::corePtr()->write(&valueTextSentBDS[0]);
        }

        const char_t* const toWrite2 = "Messages(count) not sent to Destinations:\n"
                                       "NJRU       RU         BDS";
        ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite2);

        res = snprintf(&valueTextSentNJRU[0], sizeof(valueTextSentNJRU), "%-10u ", messageNotSentNjruCounter);
        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(valueTextSentNJRU)))
        {
          ATC::AbstractConsole::corePtr()->write(&valueTextSentNJRU[0]);
        }

        res = snprintf(&valueTextSentRU[0], sizeof(valueTextSentRU), "%-10u ", messageNotSentRuCounter);
        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(valueTextSentRU)))
        {
          ATC::AbstractConsole::corePtr()->write(&valueTextSentRU[0]);
        }

        res = snprintf(&valueTextSentBDS[0], sizeof(valueTextSentBDS), "%-10u\n", messageNotSentBdsCounter);
        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(valueTextSentBDS)))
        {
          ATC::AbstractConsole::corePtr()->write(&valueTextSentBDS[0]);
        }

        retFlag = true;
      }
      else if (isTextMatch(&argv[0][0], "loglevel", sizeof("loglevel")))
      {
        if (argc == 3U)
        {
          const bool legalNJRULoglevel = isLegalLogLevel(&argv[1][0]);
          const bool legalBDSLoglevel = isLegalLogLevel(&argv[2][0]);

          if (legalNJRULoglevel && legalBDSLoglevel)
          {
            //Extracting the njruLevel and bdsLevel from the command passed
            //Subtracting 48 to get the integer value from ASCII value extracted from String
            uint8_t newNJRULevel = static_cast<uint8_t>(argv[1][0]) - static_cast<uint8_t>('0');

            uint8_t newBDSLevel = static_cast<uint8_t>(argv[2][0]) - static_cast<uint8_t>('0');

            if ((newNJRULevel != njruLevel) || (newBDSLevel != bdsLevel))
            {
              trace.write(1U, "New NJRU Level =", static_cast<int32_t>(newNJRULevel), true);
              trace.write(1U, "New BDS Level =", static_cast<int32_t>(newBDSLevel), true);
            }

            njruLevel = newNJRULevel;
            bdsLevel = newBDSLevel;

            int32_t res = snprintf(&valueTextNJRULevel[0], sizeOfDigitsInUnSignedValue, " %d ", njruLevel);

            if ((res > 0) && (static_cast<size_t>(res) < sizeOfDigitsInUnSignedValue))
            {
              static_cast<void>(vfw_strlcat(&njruLevelBuffer[0], &valueTextNJRULevel[0], sizeof(njruLevelBuffer)));
            }

            res = snprintf(&valueTextBDSLevel[0], sizeOfDigitsInUnSignedValue, " %d ", bdsLevel);

            if ((res > 0) && (static_cast<size_t>(res) < sizeOfDigitsInUnSignedValue))
            {
              static_cast<void>(vfw_strlcat(&bdsLevelBuffer[0], &valueTextBDSLevel[0], sizeof(bdsLevelBuffer)));
            }

            //Write the current Level of BDS and NJRU
            AbstractConsole::corePtr()->writeWithNewline(&njruLevelBuffer[0]);
            AbstractConsole::corePtr()->writeWithNewline(&bdsLevelBuffer[0]);

            retFlag = true;
          }
          else
          {
            //Illegal Log Command
            // Size of logErrorOnConsole = 58
            const char_t* const logErrorOnConsole = "Incorrect Log level, correct log Level 0 to 9";
            AbstractConsole::corePtr()->writeWithNewline(logErrorOnConsole);
          }
        }
        else if (argc == 1U)
        {
          int32_t res = snprintf(&valueTextNJRULevel[0], sizeOfDigitsInUnSignedValue, " %d ", njruLevel);

          if ((res > 0) && (static_cast<size_t>(res) < sizeof(njruLevelBuffer)))
          {
            static_cast<void>(vfw_strlcat(&njruLevelBuffer[0], &valueTextNJRULevel[0], sizeof(njruLevelBuffer)));
          }

          res = snprintf(&valueTextBDSLevel[0], sizeOfDigitsInUnSignedValue, " %d ", bdsLevel);

          if ((res > 0) && (static_cast<size_t>(res) < sizeof(bdsLevelBuffer)))
          {
            static_cast<void>(vfw_strlcat(&bdsLevelBuffer[0], &valueTextBDSLevel[0], sizeof(bdsLevelBuffer)));
          }

          //Write the current Level of BDS and NJRU
          AbstractConsole::corePtr()->writeWithNewline(&njruLevelBuffer[0]);

          AbstractConsole::corePtr()->writeWithNewline(&bdsLevelBuffer[0]);

          retFlag = true;
        }
        else
        {
          //Illegal Log Command
          //Size of logErrorOnConsole = 33
          const char_t* const logLevelErrorOnConsole = "Wrongly written logLevel Command";
          AbstractConsole::corePtr()->writeWithNewline(logLevelErrorOnConsole);
        }
      }
      else
      {
        //Do Nothing
      }
    }

    return retFlag;
  }

  /******************************************************************************
  * writeToNJRU
  ******************************************************************************/
  void AbstractLogHandler::writeToNJRU()
  {
    njruBuffer.writeBufferTo(AbstractBasicIP::connectionNJRU);
  }

  /******************************************************************************
  * writeToRU
  ******************************************************************************/
  void AbstractLogHandler::writeToRU()
  {
    ruBuffer.writeBufferTo(AbstractBasicIP::connectionRU);
  }

  /******************************************************************************
  * writeToBDS
  ******************************************************************************/
  void AbstractLogHandler::writeToBDS(char_t * const str)
  {
    int64_t  timeRefInSec = vfwGetReferenceTime() / 1000; // vfwGetReferenceTime(): UTC time in millisec 
    int64_t  timeModuloSec = vfwGetReferenceTime() % 1000;
    uint16_t timeTicks = static_cast<uint16_t>(timeModuloSec) / 10U; //timeTicks : unit 100 microseconds
    int8_t   utcOffset = 0;
    uint32_t nidMsg = 0U;
    uint16_t DeviceID_ATP = 210U;           //ATP Device ID: 210
    uint8_t  serviceID = static_cast<uint8_t>(ServiceID_FREE_TEXT_FORMAT);
    uint16_t upByteNidSender = static_cast<uint16_t>(DeviceID_ATP << 8U);
    uint16_t nidSender = (upByteNidSender + static_cast<uint16_t>(serviceID));

    //lint -e{926} Cast is unavoidable here
    int16_t writeBDSStatus = diaSendString(static_cast<uint8_t>(N_CMD_DATA_TEXT), nidSender, bdsSeqNumber,
      static_cast<uint32_t>(timeRefInSec), timeTicks, utcOffset, static_cast<uint64_t>(timeRefInSec), nidMsg,
      static_cast<uint8_t>(NC_SEVERITY_LOG), reinterpret_cast<uint8_t*>(str));
    bdsSeqNumber++;

    if (0 != writeBDSStatus)
    {
      trace.write(1U, "Error in BDS Write");
      messageNotSentBdsCounter++;
    }
    else
    {
      messageSentBdsCounter++;
    }
  }

  /******************************************************************************
  * isLegalLogLevel
  ******************************************************************************/
  bool AbstractLogHandler::isLegalLogLevel(const char_t logLevelStr[]) const
  {
    bool legalLogLevel = false;
    //Maxcount parameter in strnlen is 3 just to check the string passed is single digit or not 
    //check 2 digit and null character 
    if (strnlen(logLevelStr, 3U) == 1U)
    {
      // One character expected
      switch (logLevelStr[0])
      {
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        // digit in the range 0..9
        legalLogLevel = true;
        break;
      default:
        legalLogLevel = false;
      }
    }
    else
    {
      legalLogLevel = false;
    }
    return legalLogLevel;
  }

  /******************************************************************************
  * getSideChar
  ******************************************************************************/
  char_t AbstractLogHandler::getSideChar() const
  {
    VFW_Side side = vfwGetSide();
    char_t sideChar = '\0';
    switch (side)
    {
    case VFW_A_SIDE:
      sideChar = 'A';
      break;

    case VFW_B_SIDE:
      sideChar = 'B';
      break;

    case VFW_C_SIDE:
      sideChar = 'C';
      break;

    case VFW_NO_SIDE:
    default:
      sideChar = ' ';
      break;
    }
    return sideChar;
  }

  /******************************************************************************
  * corePtr
  ******************************************************************************/
  AbstractLogHandler* AbstractLogHandler::corePtr(void)
  {
    return coreLogHandlerInstancePtr;
  }

  /******************************************************************************
  * CharBuffer::CharBuffer
  ******************************************************************************/
  template <uint32_t maxSize>
  AbstractLogHandler::CharBuffer<maxSize>::CharBuffer()
  {
    init();
  }

  /******************************************************************************
  * CharBuffer::init
  ******************************************************************************/
  template <uint32_t maxSize>
  void AbstractLogHandler::CharBuffer<maxSize>::init()
  {
    buffer_[0] = '\0';
    bufferSize_ = 0U;
  }

  /******************************************************************************
  * CharBuffer::appendToBuffer
  ******************************************************************************/
  template <uint32_t maxSize>
  bool AbstractLogHandler::CharBuffer<maxSize>::appendToBuffer(const char_t* const textToAppend)
  {
    bool strAppendedFlag = false;
    uint32_t textLength = static_cast<uint32_t>(strnlen(textToAppend, (sizeof(buffer_) - bufferSize_)));

    if ((bufferSize_ + textLength + nullTerminationLength) < sizeof(buffer_))
    {
      static_cast<void>(vfw_strlcpy(&buffer_[bufferSize_], textToAppend, sizeof(buffer_) - bufferSize_));
      bufferSize_ += textLength;
      strAppendedFlag = true;
    }
    else
    {
      strAppendedFlag = false;
    }
    return strAppendedFlag;
  }

  /******************************************************************************
  * CharBuffer::size
  ******************************************************************************/
  template <uint32_t maxSize>
  uint32_t AbstractLogHandler::CharBuffer<maxSize>::size() const
  {
    return bufferSize_;
  }

  /******************************************************************************
  * CharBuffer::buffer
  ******************************************************************************/
  template <uint32_t maxSize>
  char_t* AbstractLogHandler::CharBuffer<maxSize>::buffer()
  {
    return buffer_;
  }

  /******************************************************************************
  * CharBuffer::buffer
  ******************************************************************************/
  template <uint32_t maxSize>
  const char_t* AbstractLogHandler::CharBuffer<maxSize>::buffer() const
  {
    return buffer_;
  }

  /******************************************************************************
  * CircularCharBuffer::CircularCharBuffer
  ******************************************************************************/
  template <uint32_t maxSize>
  AbstractLogHandler::CircularCharBuffer<maxSize>::CircularCharBuffer()
  {
    init();
  }

  /******************************************************************************
  * CircularCharBuffer::init
  ******************************************************************************/
  template <uint32_t maxSize>
  void AbstractLogHandler::CircularCharBuffer<maxSize>::init()
  {
    buffer_[0] = '\0';
    posToWrite_ = 0U;
    posToRead_ = 0U;
  }

  /******************************************************************************
  * CircularCharBuffer::appendToBuffer
  ******************************************************************************/
  template <uint32_t maxSize>
  void AbstractLogHandler::CircularCharBuffer<maxSize>::appendToBuffer(const char_t* const textToAppend)
  {
    uint32_t textLength = static_cast<uint32_t>(strnlen(textToAppend, sizeof(buffer_)));

    // If needed, move posToRead_ forwards to make space for the new string
    while (available() < textLength)
    {
      // Move posToRead_ forwards to the start of the next line
      while ((posToRead_ != posToWrite_) && (buffer_[posToRead_] != '\n'))
      {
        ++posToRead_;
        if (posToRead_ == sizeof(buffer_))
        {
          posToRead_ = 0U;
        }
      }

      while ((posToRead_ != posToWrite_) && (buffer_[posToRead_] == '\n'))
      {
        ++posToRead_;
        if (posToRead_ == sizeof(buffer_))
        {
          posToRead_ = 0U;
        }
      }
    }

    if ((posToWrite_ + textLength) <= sizeof(buffer_))
    {
      memmove(&buffer_[posToWrite_], &textToAppend[0U], textLength);
      posToWrite_ += textLength;
    }
    else
    {
      const uint32_t firstPart = sizeof(buffer_) - posToWrite_;
      const uint32_t secondPart = textLength - firstPart;
      memmove(&buffer_[posToWrite_], &textToAppend[0U], firstPart);
      memmove(&buffer_[0U], &textToAppend[firstPart], secondPart);
      posToWrite_ = secondPart;
    }

    if (posToWrite_ == sizeof(buffer_))
    {
      posToWrite_ = 0U;
    }
  }

  /******************************************************************************
  * CircularCharBuffer::writeBufferTo
  ******************************************************************************/
  template <uint32_t maxSize>
  void AbstractLogHandler::CircularCharBuffer<maxSize>::writeBufferTo(const uint8_t connectionId)
  {
    while (size() > 0U)
    {
      uint32_t numCharsToSend = size();

      if (numCharsToSend > (sizeof(buffer_) - posToRead_))
      {
        numCharsToSend = sizeof(buffer_) - posToRead_;
      }

      const uint32_t noOfCharactersSent = AbstractBasicIP::corePtr()->writeBuf(
        connectionId, reinterpret_cast<uint8_t*>(&buffer_[posToRead_]), numCharsToSend);

      if (noOfCharactersSent > 0U)
      {
        posToRead_ += noOfCharactersSent;
        if (posToRead_ == sizeof(buffer_))
        {
          posToRead_ = 0U;
        }
      }
      else
      {
        break;
      }
    }
  }

  /******************************************************************************
  * CircularCharBuffer::size
  ******************************************************************************/
  template <uint32_t maxSize>
  uint32_t AbstractLogHandler::CircularCharBuffer<maxSize>::size() const
  {
    uint32_t numChars;

    if (posToWrite_ >= posToRead_)
    {
      numChars = posToWrite_ - posToRead_;
    }
    else
    {
      numChars = posToWrite_ + (sizeof(buffer_) - posToRead_);
    }

    return numChars;
  }

  /******************************************************************************
  * CircularCharBuffer::available
  ******************************************************************************/
  template <uint32_t maxSize>
  uint32_t AbstractLogHandler::CircularCharBuffer<maxSize>::available() const
  {
    uint32_t numChars;

    if (posToWrite_ >= posToRead_)
    {
      numChars = posToRead_ + (sizeof(buffer_) - posToWrite_);
    }
    else
    {
      numChars = posToRead_ - posToWrite_;
    }

    return numChars;
  }
}

//lint +esym(586,snprintf)
