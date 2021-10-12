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
* Date          Name           Changes
* ---------------------------------------------------------------------------
* 2016-11-03    nsyed          Created
* 2016-11-17    akushwah       updated Prel design
* 2016-11-26    saprasad       Added stdio.h header to remove snprintf error in linux
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <time.h>
#include "log_handler.hpp"
#ifdef WIN32
extern "C"  int64_t vfwGetReferenceTime(void);
extern "C"  void vfwGetTimeOfDay(struct timespec * const timespec_p, struct tm * const tm_p);
#else
#include <vfw_time.h>
#endif
#include<stdio.h>

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
  LogHandler::LogHandler(void) : AbstractLogHandler()
  {
  }

  /******************************************************************************
  * instance
  *
  * Add additional functional description here if needed.
  * (This info is not included in doxygen documentation but may be usefull)
  *
  ******************************************************************************/
  LogHandler& LogHandler::instance(void)
  {
    static LogHandler theOnlyLogHandlerInstance;

    return theOnlyLogHandlerInstance;
  }

  /******************************************************************************
  * addApplicationIdToBuffer
  ******************************************************************************/
  void LogHandler::addApplicationIdToBuffer(LineBuffer& buffer) const
  {
    static_cast<void>(buffer.appendToBuffer("DISP"));
  }
}
