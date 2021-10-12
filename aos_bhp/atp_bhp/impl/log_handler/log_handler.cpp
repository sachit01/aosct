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

/*****************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-04    akushwah    Created
* 2016-07-18    akushwah    Updated after review
* 2016-08-09    akushwah    Initial Implementation
* 2016-09-19    akushwah    Corrected Init function
* 2016-11-03    adgupta     Updated after Log Handler Redesign   
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "abstract_odometry.hpp"
#include "abstract_position.hpp"
#include "log_handler.hpp"
#ifdef _MSC_VER
#include <time.h>
extern "C" void vfwGetTimeOfDay(struct timespec * const timespec_p, struct tm * const tm_p);
#else
#include <vfw_time.h>
#endif

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
    static_cast<void>(buffer.appendToBuffer("ATP"));
  }

  /******************************************************************************
  * addPositionAndSpeedToBuffer
  ******************************************************************************/
  void LogHandler::addPositionAndSpeedToBuffer(LineBuffer& buffer) const
  {
    TrackAndPos trackAndPos;
    if (Pos::AbstractPosition::corePtr()->getAccuracyState() == Pos::PosUnknown)
    {
      trackAndPos.track = 0U;
      trackAndPos.position = 0U;
    }
    else
    {
      trackAndPos = Pos::AbstractPosition::corePtr()->getLeadingPos();
    }

    uint16_t speed = Pos::AbstractOdometry::corePtr()->getSpeed();

    //lint -e{586} snprintf is needed here
    char_t stringBuffer[80];
    const int32_t charsWritten = snprintf(&stringBuffer[0], sizeof(stringBuffer), "%05u:+%07u %04u",
      trackAndPos.track, trackAndPos.position, speed);

    if ((charsWritten > 0) && (static_cast<size_t>(charsWritten) < sizeof(stringBuffer)))
    {
      static_cast<void>(buffer.appendToBuffer(&stringBuffer[0]));
    }
    else
    {
      trace.write(ATC::briefTrace, "N-JRU Buffer Error");
    }
  }

  /******************************************************************************
  * interfaceIdToString
  ******************************************************************************/
  const char_t* LogHandler::interfaceIdToString(InterfaceId const ifc)
  {
    const char_t* ifcString;

    switch (ifc)
    {
    case Ifc_LCS:
      ifcString = "LCS";
      break;
    case Ifc_OBRD:
      ifcString = "OBRD";
      break;
    default:
      ifcString = AbstractLogHandler::interfaceIdToString(ifc);
    }

    return ifcString;
  }

  /******************************************************************************
  * writeToBDS
  ******************************************************************************/
  void ATP::LogHandler::writeToBDS(char_t * const str)
  {
    if (vfwGetSide() == VFW_A_SIDE)
    {
      AbstractLogHandler::writeToBDS(str);
    }
  }
}
