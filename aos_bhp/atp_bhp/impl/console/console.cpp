/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class implements the adaptation functionality of Console Component.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2015-07-04    adgupta     Created
* 2015-11-04    saprasad    Implemented ATP version functionality
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "console.hpp"
#include "abstract_atp_application.hpp"
#include "atp_application.hpp"
#include "atc_util.hpp"
#include <stdio.h>
#include "abstract_message_handler.hpp"
#include "abstract_dmi_handler.hpp"
#include "config.hpp"

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
namespace ATP
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  Console::Console() : AbstractConsole("ATP>")
  {
  }

  /******************************************************************************
  * instance
  ******************************************************************************/
  Console& Console::instance(void)
  {
    static Console theOnlyConsoleInstance;

    return theOnlyConsoleInstance;
  }

  /******************************************************************************
  * getComponentIter
  ******************************************************************************/
  ATC::CompPtrIter Console::getComponentIter()
  {
    return Kernel::AbstractATPApplication::corePtr()->getCompIterator();
  }

  /******************************************************************************
  * getLastComponentIter
  ******************************************************************************/
  ATC::CompPtrIter Console::getComponentIterEnd()
  {
    return Kernel::AbstractATPApplication::corePtr()->getCompIteratorEnd();
  }

  /******************************************************************************
  * writeVersion
  ******************************************************************************/
  void Console::writeVersion()
  {
    // Buffer use to store the ATP version information
    char_t  buffer[100];
    int32_t ret = snprintf(&buffer[0],
      sizeof(buffer), 
      "ATP Version    :    %s\nATP Build Time :    %s %s",
      ATC::AbstractApplicationBase::corePtr()->getApplicationVersionString(),
      __TIME__,
      __DATE__);

    if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
    {
      // write the ATP information on the console 
      writeWithNewline(&buffer[0]);
    }
  }

  /******************************************************************************
  * consoleCall
  ******************************************************************************/
  bool Console::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
  {
    bool retVal;
    char_t buffer[consoleBufferSize];

    // Handle the Abstract Console calls first
    retVal = AbstractConsole::consoleCall(argc, argv);

    if (false == retVal)
    {
      // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        const char_t* const toWrite =
          "chstat        To print the statistics of all the ATP channels\n"
          "cyctime       To print the last/min/max execution time for one cycle\n"
          "cyctime clear To clear the min/max execution time values\n"
          "version       To print ATP/Dispatcher/ATO version information";
        writeWithNewline(toWrite);
      }
      // Print ATP Version  
      else if (ATC::isTextMatch(&argv[0][0], "version", sizeof("version")) && (argc == 1U))
      {
        const char_t* const toWrite = "Binary              |  Version    \n"
          "------------------------------------------------------------";

        writeWithNewline(toWrite);

        //ATP Version
        int32_t ret = snprintf(
          &buffer[0],
          sizeof(buffer),
          "%s                 |  %s",
          ATC::AbstractApplicationBase::corePtr()->getApplicationName(),
          ATC::AbstractApplicationBase::corePtr()->getApplicationVersionString());

        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Common              |  %u.%u", commonVersionMajor, commonVersionMinor);
        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Type                |  %u.%u", typeVersionMajor, typeVersionMinor);
        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Instance            |  %u.%u", instVersionMajor, instVersionMinor);
        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Maintenance         |  %u.%u", mntVersionMajor, mntVersionMinor);
        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Runtime             |  %u.%u", rtVersionMajor, rtVersionMinor);
        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          writeWithNewline(&buffer[0]);
        }

        //Get the protocol TCC-AOS version
        const Kernel::ProtocolVersion& tccProtocolVersion = Kernel::AbstractMessageHandler::corePtr()->getProtocolVersion();

        ret = snprintf(&buffer[0], sizeof(buffer), "TCC Protocol        |  %u.%u",
          tccProtocolVersion.majorVersion, tccProtocolVersion.minorVersion);

        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          writeWithNewline(&buffer[0]);
        }

        //DMI SW Version
        ret = snprintf(&buffer[0], sizeof(buffer), "DMI Compatibility   |  %u",
          DMICom::AbstractDMIHandler::corePtr()->getCompatibilityVersion());

        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          writeWithNewline(&buffer[0]);
        }

        retVal = true;
      }
      // Cycle command is included here, since ATPApplication is not a component itself (and thus have no consoleCall()).
      else if (ATC::isTextMatch(&argv[0][0], "cyctime", sizeof("cyctime")))
      {
        switch (argc)
        {
        case 1:
        {
          const int32_t ret = snprintf(&buffer[0], sizeof(buffer),
            "%-28s: %-3dms\n"
            "%-28s: %-3dms\n"
            "%-28s: %-3dms\n",
            "Last cycle execution time", ATP::Kernel::ATPApplication::instance().getLastExecTime(),
            "Min cycle execution time", ATP::Kernel::ATPApplication::instance().getMinExecTime(),
            "Max cycle execution time", ATP::Kernel::ATPApplication::instance().getMaxExecTime());

          if ((ret > 0) && (static_cast<size_t>(ret) < consoleBufferSize))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
            retVal = true;
          }
          break;
        }
        case 2:
        {
          // Check if 2nd argument passed is "clear"
          if (ATC::isTextMatch(&argv[1][0], "clear", sizeof("clear")))
          {
            // Clear the execution time values
            ATP::Kernel::ATPApplication::instance().clearExecTimes();
            retVal = true;
          }
          break;
        }
        default:
          break;
        }
      }
      else
      {
        // Do nothing 
      }
    }

    return retVal;
  }
}

//lint +esym(586,snprintf)
