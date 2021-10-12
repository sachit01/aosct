/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This abstract class implements the core functionality of Console Component.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2015-07-01    adgupta     Created
* 2016-07-09    adgupta     Implementation of Console functionality
* 2016-08-22    akushwah    Implementated isLegalLogLevel()
* 2016-09-08    adgupta     Updated Console call for write() and to have headers
* 2016-09-20    akushwah    Formatted the string for help command
* 2016-09-28    adgupta     Updated the console to display prompt even after blank input
* 2016-10-21    nsyed       Use access functions for the config paramets
* 2016-11-07    saprasad    Implemented Version console command
* 2016-11-28    saprasad    Change string syntax for version(added dispatcher/ato)

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include <cstdio>
#include <vfw_string.h>

#include "abstract_basic_ip.hpp"
#include "abstract_config_base.hpp"
#include "abstract_console.hpp"
#include "atc_util.hpp"


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
namespace ATC
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  AbstractConsole::AbstractConsole(const char_t* const promptInput) : ProcComponent(atcConsoleId, "Console", "CON")
  {
    if (coreConsoleInstancePtr != 0)
    {
      aosHalt(__FILE__, __LINE__,"Console Constructor already instantiated");
    }

    // Setup single instance pointer for core access
    coreConsoleInstancePtr = this;

    //Prompt to be displayed on console to denote it's connected.
    static_cast<void>(vfw_strlcpy(&prompt[0], promptInput, sizeof(prompt)));

    initDone = false;
    connected = false;
    outputBuffCount = 0U;
  }

  /******************************************************************************
  * corePtr
  ******************************************************************************/
  AbstractConsole* AbstractConsole::corePtr(void)
  {
    return coreConsoleInstancePtr;
  }

  /******************************************************************************
  * init
  ******************************************************************************/
  bool AbstractConsole::init(void)
  {
    if (!initDone) // init should be done only once.
    {
#ifndef _DISPATCHER
      if (vfwGetSide() == VFW_A_SIDE)
      {
#endif
        ATC::CompPtrIter itr;
        uint16_t consolePortNum = AbstractConfigBase::basePtr()->getConsolePort();

        // 1. Read configuration file to get the value of the port number on Local Host to listen to.
        // 2. Initialize a connection via BasicIp. IP address doesn't need to be populated as it is a host connection itself.

        initDone = AbstractBasicIP::corePtr()->initConnection(AbstractBasicIP::connectionConsole, AbstractBasicIP::ConnectionTypeTcpHost,
          static_cast<char_t*>(NULL), consolePortNum, maxOutputBufferSize, static_cast<uint32_t>(maxInputBufferSize));

        if (initDone)  // Do only once, only id init is successful.
        {
          // 3. Initialize the trace object vector list. Add all the components' trace object to this vector.
          for (itr = getComponentIter(); itr != getComponentIterEnd(); ++itr)
          {
            // Acquire the trace from complist via iterators and push it to trace object pointer list.
            addTraceObj((*itr)->getTrace());
          }
        }
#ifndef _DISPATCHER
      }
      else
      {
        initDone = true;
      }
#endif
    }

    return initDone;
  }

  /******************************************************************************
  * run
  ******************************************************************************/
  void AbstractConsole::run(void)
  {
    // Continue only if connected!
    if (AbstractBasicIP::ConnectionStatusConnected == AbstractBasicIP::corePtr()->getConnectionStatus(AbstractBasicIP::connectionConsole))
    {
      if (!connected)
      {
        ////If connection is established.Write version information 
        writeVersion();
        //If got connected now. Print the prompt. Update connected flag.
        writePrompt();
        connected = true;
      }

      //lint -e{926} Cast is unavoidable here
      const uint32_t bytesRead = AbstractBasicIP::corePtr()->readBuf(
        AbstractBasicIP::connectionConsole, reinterpret_cast<uint8_t *>(&inputBuffer[0]), maxInputBufferSize - 1U);
      //null terminate the input buffer.
      inputBuffer[bytesRead] = '\0';

      if (bytesRead > 0U)
      {
        // Scan and populate the inputbuffer and fill the argv buffer
        // It is OK to use sscanf as the input buffer is null terminated, format string has length specified
        // the argument pointers are not null and the return value is checked.
        ConsoleArguments consoleArgv;
        const int32_t numScanned = sscanf(&inputBuffer[0], "%8s%8s%8s%8s%8s",
          &consoleArgv[0][0], &consoleArgv[1][0], &consoleArgv[2][0], &consoleArgv[3][0], &consoleArgv[4][0]);

        if (numScanned != -1)
        {
          if (numScanned > 0)
          {
            const uint32_t argc = static_cast<uint32_t>(numScanned);
            ATC::CompPtrIter itr;
            bool retVal = false;

            if (isTextMatch(&consoleArgv[0][0], "help", sizeof("help")) && (argc == 1U))
            {
              const char_t* const toWrite =
                "Command       Brief description\n"
                "-------------------------------------------------------------------------------\n";

              write(toWrite);
            }

            const bool isDistributed = isDistributedConsoleCall(&consoleArgv[0][0]);

            // Now we have the argc, argv available to be able to call consoleCall() function
            for (itr = getComponentIter(); itr != getComponentIterEnd(); ++itr)
            {
              retVal = (*itr)->consoleCall(argc, consoleArgv); //lint !e1960 Function expects ConsoleArguments and gets ConsoleArguments

              // Acquire the trace from complist via iterators and do consoleCall()
              if (retVal && (!isDistributed))
              {
                break; // break if true. Console Call was handled successfully!
              }
            }

            // Only non-distributed commands can return false to signal error
            if ((!retVal) && (!isDistributed))
            {
              char_t toWrite[] = "Unrecognized command/Can't Handle Command";

              writeWithNewline(&toWrite[0]);
            }

            writePrompt();
          }

          //Reset the buffer after reading.
          memset(&inputBuffer[0], 0, maxInputBufferSize);
        }
        else
        {
          //numScanned == -1: No values is entered before carriage return Key. Display prompt again.
          writePrompt();
        }
      }// End of if(bytesRead > 0)

      // Now write the output Buffer
      if (outputBuffCount > 0U)
      {
        //lint -e{926} Cast is unavoidable here
        const uint32_t bytesWritten = AbstractBasicIP::corePtr()->writeBuf(
          AbstractBasicIP::connectionConsole, reinterpret_cast<uint8_t *>(&outputBuffer[0]), outputBuffCount);

        if (bytesWritten == 0U)
        {
          // Do nothing
        }
        else if (bytesWritten < outputBuffCount)
        {
          // Flush the written bytes and move the unwritten ones to the top
          const uint32_t bytesNotWritten = outputBuffCount - bytesWritten;
          memmove(&outputBuffer[0], &outputBuffer[bytesWritten], bytesNotWritten);
          outputBuffCount = bytesNotWritten;
        }
        else
        {
          // Clear the output buffer count for new outputs
          outputBuffCount = 0U;
        }
      }
    }//End of if(connected)
    else
    {
      connected = false;
    }
  }

  /******************************************************************************
  * write
  ******************************************************************************/
  void AbstractConsole::write(const char_t* const str)
  {
    //Add the string to the output buffer

    const uint32_t len = static_cast<uint32_t>(strnlen(str, static_cast<size_t>(maxOutputBufferSize - outputBuffCount)));

    // Check if buffer still has space to copy
    if ((outputBuffCount + len) < maxOutputBufferSize)
    {
      memmove(&outputBuffer[outputBuffCount], str, len);
      outputBuffCount += len;
    }
    else
    {
      //Log output buffer overflow?
    }
  }

  /******************************************************************************
  * writeWithNewline
  ******************************************************************************/
  void AbstractConsole::writeWithNewline(const char_t* const str)
  {
    //Add the string to the output buffer

    const uint32_t len = static_cast<uint32_t>(strnlen(str, static_cast<size_t>(maxOutputBufferSize - outputBuffCount)));
    const char_t newLine = '\n';

    // Check if buffer still has space to copy
    if ((outputBuffCount + len + 1U) < maxOutputBufferSize)
    {
      memmove(&outputBuffer[outputBuffCount], str, len);
      outputBuffCount += len;
      memmove(&outputBuffer[outputBuffCount], &newLine, 1U);
      outputBuffCount += 1U;
    }
    else
    {
      //Log output buffer overflow?
    }
  }

  /******************************************************************************
  * writePrompt
  ******************************************************************************/
  void AbstractConsole::writePrompt()
  {
    if ((outputBuffCount + maxPromptSize) < maxOutputBufferSize)
    {
      memmove(&outputBuffer[outputBuffCount], &prompt[0], static_cast<uint32_t>(strnlen(&prompt[0], maxPromptSize)));
      outputBuffCount += static_cast<uint32_t>(strnlen(&prompt[0], maxPromptSize));
    }
  }

  /******************************************************************************
  * addTraceObj
  ******************************************************************************/
  void AbstractConsole::addTraceObj(TraceInterface* const obj)
  {
    if (static_cast<TraceInterface*>(NULL) != obj)
    {
      traceObjList.push_back(obj);
    }
  }

  /******************************************************************************
  * consoleCall
  ******************************************************************************/
  bool AbstractConsole::consoleCall(const uint32_t argc, const ConsoleArguments argv)
  {
    /*
    This functions parses the arguments searches for the "help", "trace" or any other Console
    component specific command calls and handles it. Returns true if completely handled
    else returns false. returning false will let other components handle the call. help always returns false.
    */

    bool retVal = false;
    char_t  buffer[512];

    // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
    if (isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
    {
      const char_t* const toWrite =
        "help          To get information about all the commands supported by components\n"
        "trace         To enable/disable the component trace and to change the trace levels\n";

      write(toWrite);
      retVal = false;
    }
    else if (isTextMatch(&argv[0][0], "chstat", sizeof("chstat")) && (argc == 1U))
    {
      const char_t* const toWrite =
        "ChannelName                   ChannelType   NoOfMessages   NoOfBytes\n"
        "--------------------------------------------------------------------\n";

      write(toWrite);
      retVal = false;
    }
    else if (isTextMatch(&argv[0][0], "trace", sizeof("trace")))
    {
      TracePtrIterator itr;

      switch (argc)
      {
      case 1:
      {
        bool    traceEnabled;
        uint8_t traceLevel;

        int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%-22s%-14s%-12s", "Component", "Enabled(0-1)", "Level(0-9)");

        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          writeWithNewline(&buffer[0]);
        }

        // only 1 argument i.e.- "trace" received - List all the trace objects.
        for (itr = traceObjList.begin(); itr != traceObjList.end(); ++itr)
        {
          (*itr)->getTraceDetails(traceLevel, traceEnabled);

          ret = snprintf(&buffer[0], sizeof(buffer), "%-22s%-14d%-12d", (*itr)->trcName(), static_cast<uint8_t>(traceEnabled), traceLevel);

          if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
          {
            writeWithNewline(&buffer[0]);
          }
        }

        break;
      }

      case 2:
      {
        //find the trace in the trace object list.
        for (itr = traceObjList.begin(); itr != traceObjList.end(); ++itr)
        {
          if (isTextMatch((*itr)->trcName(), &argv[1][0], maxTraceNameLen))
          {
            bool    traceEnabled;
            uint8_t traceLevel;
            int32_t ret;

            //Toggle the trace enable for this trace
            (*itr)->getTraceDetails(traceLevel, traceEnabled);
            traceEnabled = !traceEnabled;
            (*itr)->setTraceDetails(traceLevel, traceEnabled);

            const char_t* buffEnabled = traceEnabled ? "Enabled" : "Disabled";

            ret = snprintf(&buffer[0], sizeof(buffer), "%s %s", "Trace is now", buffEnabled);
            if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              writeWithNewline(&buffer[0]);
            }

            break;
          }
        }

        // iterator reached the end no appropriate trace found. Report!
        if (itr == traceObjList.end())
        {
          const char_t* const toWrite = "No matching trace object found";
          writeWithNewline(toWrite);
        }

        break;
      }

      case 3:
      {

        //find the trace in the trace object list.
        for (itr = traceObjList.begin(); itr != traceObjList.end(); ++itr)
        {
          if (isTextMatch((*itr)->trcName(), &argv[1][0], maxTraceNameLen))
          {
            uint32_t cnt;

            int32_t ret = sscanf(&argv[2][0], "%u", &cnt);  // not unsafe as argv[2] is of size 8 while sscanf will read maximum 4 bytes

            if (-1 == ret)
            {
              //Argument is not all digits. Illegal argument
              char_t toWrite[] = "Illegal argument found";
              writeWithNewline(&toWrite[0]);
            }
            else
            {
              int32_t val;
              bool    traceEnabled;
              uint8_t traceLevel;

              //update the trace values for this trace
              (*itr)->getTraceDetails(traceLevel, traceEnabled);

              val = static_cast<int32_t>(cnt);

              // For all illegal values.
              if ((val > 9) || (val < 0))
              {
                char_t toWrite[] = "Illegal trace level entered. Valid trace level 0-9";
                writeWithNewline(&toWrite[0]);
              }
              else
              {
                traceLevel = static_cast<uint8_t>(val);

                (*itr)->setTraceDetails(traceLevel, traceEnabled);

                ret = snprintf(&buffer[0], sizeof(buffer), "%s %d", "Trace level Set! Trace level =", traceLevel);
                if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
                {
                  writeWithNewline(&buffer[0]);
                }

              }
            }

            break;
          }//end of if(isTextMatch)
        }//end of for(itr)


        // iterator reached the end no appropriate trace found. Report!
        if (itr == traceObjList.end())
        {
          const char_t* const toWrite = "No matching trace object found";
          writeWithNewline(toWrite);
        }

        break;
      }//End of Case 3

      default:
      {
        const char_t* const toWrite = "Illegal Arguments";
        writeWithNewline(toWrite);

        break;
      }
      }//End of Switch()

      //"trace" command should return true even if not successful. Handled only in Console component, don't want other components to get it.
      retVal = true;
    }
    else
    {
      // for lint
    }

    return retVal;
  }

  /******************************************************************************
  * isDistributedConsoleCall
  ******************************************************************************/
  bool AbstractConsole::isDistributedConsoleCall(const char_t* const callString)
  {
    const char_t helpCommandStr[] = "help";
    const char_t dmiCommandStr[] = "dmich";
    const char_t chanCommandStr[] = "chstat";

    const bool match1 = isTextMatch(callString, &helpCommandStr[0], sizeof(helpCommandStr));
    const bool match2 = ATC::isTextMatch(callString, &dmiCommandStr[0], sizeof(dmiCommandStr));
    const bool match3 = ATC::isTextMatch(callString, &chanCommandStr[0], sizeof(chanCommandStr));

    return match1 || match2 || match3;
  }
}

//lint +esym(586,snprintf)
