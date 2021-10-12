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
* 2015-04-05    arastogi    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-06-09    arastogi    Updated based on the new design
* 2016-09-19    akushwah    Corrected Init function
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "event_handler.hpp"
#include "abstract_config.hpp"
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
    EventHandler::EventHandler() : AbstractEventHandler(maxActiveEventCount, maxTCCEventCount,
                                   maxDMIEventCount)
    {

    }

    /******************************************************************************
    *  instance
    ******************************************************************************/
    EventHandler& EventHandler::instance(void)
    {
      static EventHandler theOnlyEventHandlerInstance;

      return theOnlyEventHandlerInstance;
    }

    /******************************************************************************
    *  getMaxEventActiveTime
    ******************************************************************************/
    int64_t EventHandler::getMaxEventActiveTime(const Event::EventType eventType) const
    {
      uint8_t activeTime;

      if (eventType == Event::EventTypeStandstill)
      {
        activeTime = ATP::AbstractConfig::corePtr()->getStandstillEventKeepActiveTime();
      }
      else
      {
        activeTime = ATP::AbstractConfig::corePtr()->getEventKeepActiveTime();
      }

      return static_cast<int64_t>(activeTime) * 1000;
    }
}
