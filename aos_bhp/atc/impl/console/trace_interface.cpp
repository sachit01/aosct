/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
* To print the Trace commands on Console each of the component will have
* a trace object of TraceInterface. This class defines the required
* infrastructure and details of the same.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-01    adgupta     Created
* 2016-07-12    adgupta     Implemented functionality
* 2016-09-08    adgupta     Added Write to have newline and without newlines
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "trace_interface.hpp"
#include "abstract_console.hpp"
#include <vfw_string.h>
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
namespace ATC
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/

  TraceInterface::TraceInterface(const char_t* const nameTrc, const uint8_t lev, const char_t* const helpStr)
  {
    // Constructor for explicit(apart from one created by default of the same name
    // as component short name) creation of the Trace object by any component.

    static_cast<void>(vfw_strlcpy(&traceName[0], nameTrc, sizeof(traceName)));

    level = lev;
    static_cast<void>(vfw_strlcpy(&help[0], helpStr, sizeof(help)));

    traceEnabled = true;
  }

  /******************************************************************************
  * traceHelp
  ******************************************************************************/
  const char_t* TraceInterface::traceHelp() const
  {
    return &help[0];
  }

  /******************************************************************************
  * traceName
  ******************************************************************************/
  const char_t* TraceInterface::trcName() const
  {
    return &traceName[0];
  }

  /******************************************************************************
  * setTraceDetails
  ******************************************************************************/
  void TraceInterface::setTraceDetails(const uint8_t lev, const bool enable)
  {
    level = lev;
    traceEnabled = enable;
  }

  /******************************************************************************
  * getTraceDetails
  ******************************************************************************/
  void TraceInterface::getTraceDetails(uint8_t &lev, bool &isEnabled) const
  {
    lev = level;
    isEnabled = traceEnabled;
  }

  /******************************************************************************
  * write
  ******************************************************************************/
  void TraceInterface::write(const uint8_t lev, const char_t* const str, const bool newLine) const
  {
    if (traceEnabled && (lev <= level) && (str != NULL))
    {
      if (newLine)
      {
        AbstractConsole::corePtr()->writeWithNewline(str);
      }
      else
      {
        AbstractConsole::corePtr()->write(str);
      }
    }
  }

  /******************************************************************************
  * write
  ******************************************************************************/
  void TraceInterface::write(const uint8_t lev, const char_t* const str, const uint32_t val, const bool newLine) const
  {
    char_t temp[11] = { '\0' }; // To store uint32 value. Max integer value can take upto 10 characters

    if (traceEnabled && (lev <= level) && (str != NULL))
    {
      const int32_t retVal = snprintf(&temp[0], sizeof(temp), "%u", val);

      if ((retVal > 0)  &&  (static_cast<size_t>(retVal) < sizeof(temp)))
      {
        //Cannot write more than what is left to be written in the output buffer
        AbstractConsole::corePtr()->write(str);
        AbstractConsole::corePtr()->write(" ");

        if (newLine)
        {
          AbstractConsole::corePtr()->writeWithNewline(&temp[0]);
        }
        else
        {
          AbstractConsole::corePtr()->write(&temp[0]);
        }
      }
    }
  }

  /******************************************************************************
  * write
  ******************************************************************************/
  void TraceInterface::write(const uint8_t lev, const char_t* const str, const int32_t val, const bool newLine) const
  {
    char_t temp[12] = { '\0' }; // To store uint32 value. Max integer value can take upto 11 characters

    if (traceEnabled && (lev <= level) && (str != NULL))
    {
      const int32_t retVal = snprintf(&temp[0], sizeof(temp), "%d", val);

      if ((retVal > 0) && (static_cast<size_t>(retVal) < sizeof(temp)))
      {
        AbstractConsole::corePtr()->write(str);
        AbstractConsole::corePtr()->write(" ");
        if (newLine)
        {
          AbstractConsole::corePtr()->writeWithNewline(&temp[0]);
        }
        else
        {
          AbstractConsole::corePtr()->write(&temp[0]);
        }
      }
    }
  }

}
