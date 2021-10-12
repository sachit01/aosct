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
* 16-11-2016    adgupta     Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "event_handler.hpp"
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
    EventHandler::EventHandler() : AbstractEventHandler(maxActiveEventCount, maxTCCEventCount,
                                   maxDMIEventCount)
    {

    }

    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    * (This info is not included in doxygen documentation but may be useful)
    *
    ******************************************************************************/
    EventHandler& EventHandler::instance(void)
    {
      static EventHandler theOnlyEventHandlerInstance;

      return theOnlyEventHandlerInstance;
    }

    /******************************************************************************
     * getMaxEventActiveTime
     ******************************************************************************/
    int64_t EventHandler::getMaxEventActiveTime(const ATC::Event::EventType eventType) const
    {
      uint8_t activeTime;

      if (eventType == ATC::Event::EventTypeStandstill)
      {
        activeTime = maxStandstillEventActiveTime;
      }
      else
      {
        activeTime = maxEventActiveTime;
      }

      return static_cast<int64_t>(activeTime) * 1000;
    }
}
