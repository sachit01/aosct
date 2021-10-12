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
* 2016-11-11    saprasad    Created
* 2016-11-17    saprasad    Added Console Dispatcher functionality
* 2016-11-26    saprasad    Added stdio.h header to remove snprintf error in linux
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "console.hpp"
#include "abstract_application_base.hpp"
#include "atc_util.hpp"
#include "config.hpp"
#include <stdio.h>

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace Dispatcher
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  Console::Console() : AbstractConsole("Disp>")
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
    // Get the begin iterator of all the component vector specified in Dispatcher
    ATC::CompPtrIter  beginIterator = ATC::AbstractApplicationBase::corePtr()->getCompIterator();
    return beginIterator;
  }

  /******************************************************************************
  * getLastComponentIter
  ******************************************************************************/
  ATC::CompPtrIter Console::getComponentIterEnd()
  {
    // Get the end iterator of all the component vector specified in Dispatcher
    ATC::CompPtrIter  endIterator = ATC::AbstractApplicationBase::corePtr()->getCompIteratorEnd();
    return endIterator;
  }

  /******************************************************************************
  * writeVersion
  ******************************************************************************/
  void Console::writeVersion()
  {
    // Buffer use to store the Dispatcher version information
    char_t  buffer[200];

    //lint -e{586} snprintf is needed here
    const int32_t ret = snprintf(&buffer[0], sizeof(buffer),
      "Dispatcher Version    :    %s\nDispatcher Build Time :    %s %s",
      ATC::AbstractApplicationBase::corePtr()->getApplicationVersionString(), __TIME__, __DATE__);

    if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
    {
      // write the Dispatcher information on the console 
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
      if (ATC::isTextMatch(&argv[0][0], "version", sizeof("version")) && (argc == 1U))
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

        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Disp                |  %u.%u", dispVersionMajor, dispVersionMinor);

        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          writeWithNewline(&buffer[0]);
        }

        retVal = true;
      }
    }
    return retVal;
  }
}
