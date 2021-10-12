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
* 2016-04-21    lantback    Implemented corePtr()
* 2016-04-22    lantback    Added component type
* 2016-06-09    arastogi    Updated based on the new design
* 2016-06-20    saprasad    Implemented init,run,reportevent functions,lint fixes
* 2016-06-22    saprasad    Incoroparated Event Handler Review Comments
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-19    arastogi    Added functionality to apply SB/EB when reportEvent is called.
*                           Call AbstractLogHandler::logEvent in reportEvent.
*                           Added console call
* 2016-09-23    arastogi    Removed ATC::
* 2016-10-12    arastogi    Fixed event active time
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_event_handler.hpp"
#include "abstract_log_handler.hpp"
#include "abstract_console.hpp"
#include "atc_util.hpp"
#include <stdio.h>
#ifndef _DISPATCHER
#include <vfw_checkpoints.h>
#endif
#ifdef WIN32
extern "C" int64_t vfwGetReferenceTime(void);
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
namespace ATC
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  AbstractEventHandler::AbstractEventHandler(const uint16_t maxEvents, const uint16_t maxTCCEventsCount,
    const uint16_t maxDmiEventsCount) : initDone(false),
    maxActiveEvents(maxEvents), maxTCCEvents(maxTCCEventsCount), maxDMIEvents(maxDmiEventsCount),
    ProcComponent(atcEventHandlerId, "EventHandler", "EH")
  {
    if (coreEventHandlerInstancePtr != 0)
    {
        aosHalt(__FILE__, __LINE__, "Event Handler constructor already instantiated");
    }

    // Reserve vector sizes here
    activeEventList.reserve(maxActiveEvents);
    dmiEvents.reserve(maxDMIEvents);
    tcc2Events.reserve(maxTCCEvents);
    tcc3Events.reserve(maxTCCEvents);
    // Setup single instance pointer for core access
    coreEventHandlerInstancePtr = this;
  }

  /******************************************************************************
  * init
  ******************************************************************************/
  bool AbstractEventHandler::init(void)
  {
    if (!initDone)
    {
      //initialize all variable with default value
      activeEventList.clear();
      tcc2Events.clear();
      tcc3Events.clear();
      dmiEvents.clear();
      applyEB = false;
      applySB = false;
      modeChangeToSBS = false;
      modeChangeToSHT = false;
      ebReleaser = NoEB;
      sbReleaser = NoSB;
      initDone = true;
    }

    return initDone;
  }

  /******************************************************************************
  * run
  ******************************************************************************/
  void AbstractEventHandler::run(void)
  {
  #ifndef _DISPATCHER
    static uint32_t cp = 0U; // Must be initialized to 0
    vfwVisitCheckPoint(&cp, "EH_run");
  #endif

    //set default value to parameter
    applySB = false;
    applyEB = false;
    modeChangeToSBS = false;
    modeChangeToSHT = false;
    standstillEventActiveCounter = 0U;
    ebReleaser = NoEB;
    sbReleaser = NoSB;
    //get reference time
    int64_t currentTimestamp = vfwGetReferenceTime();
    std::vector<EventListItem>::iterator iteEventItem = activeEventList.begin();

    while (iteEventItem != activeEventList.end())
    {
      const Event::EventType eventType = iteEventItem->event->getType();
      bool bItemDeleted = false;
      if ((currentTimestamp > static_cast<int64_t>(iteEventItem->endTime)) && (Event::EventTypeSafetyHalt != eventType))
      {
        iteEventItem = activeEventList.erase(iteEventItem);
        //iteEventItem will point to followed element of erase element
        bItemDeleted = true;

      }
      else if ((Event::EventTypeEBReq == eventType) || (Event::EventTypeSafeBrakeEB == eventType)
        || (Event::EventTypeSafetyHalt == eventType))
      {
        applyEB = true;
        if (Event::EventTypeSafeBrakeEB == eventType)
        {
          //Event requested mode change to SafeBrakeToStop
          modeChangeToSBS = true;
        }
            
        if(Event::EventTypeSafetyHalt == eventType)
        {
          //Event requested mode change to SafetyHalt
          modeChangeToSHT = true;
        }

        if (ebReleaser < static_cast<EBReleaser>(iteEventItem->event->getReleaser()))
        {
          ebReleaser = static_cast<EBReleaser>(iteEventItem->event->getReleaser());
        }
      }
      else if ((Event::EventTypeSBReq == eventType) || (Event::EventTypeSafeBrakeSB == eventType))
      {
        applySB = true;

        if (Event::EventTypeSafeBrakeSB == eventType)
        {
          //event requested mode change to SafeBrakeToStop
          modeChangeToSBS = true;
        }

        if (sbReleaser < static_cast<SBReleaser>(iteEventItem->event->getReleaser()))
        {
          sbReleaser = static_cast<SBReleaser>(iteEventItem->event->getReleaser());
        }
      }
      else if (Event::EventTypeStandstill == eventType)
      {
        standstillEventActiveCounter += 1U;
      }
      else
      {
        //do nothing ,for satisfy lint
      }

      if (!bItemDeleted)
      {
        ++iteEventItem;
      }
    }
  }

  /******************************************************************************
  * isApplyEb
  ******************************************************************************/
  bool AbstractEventHandler::isApplyEb() const
  {
    return applyEB;
  }

  /******************************************************************************
  * isApplySb
  ******************************************************************************/
  bool AbstractEventHandler::isApplySb() const
  {
    return applySB;
  }

  /******************************************************************************
  * isModeChangeToSBS
  ******************************************************************************/
  bool AbstractEventHandler::isModeChangeToSafeBrakeToStop() const
  {
    return modeChangeToSBS;
  }

  /******************************************************************************
  * isModeChangeToSHT
  ******************************************************************************/
  bool AbstractEventHandler::isModeChangeToSafetyHalt() const
  {
    return modeChangeToSHT;
  }

  /******************************************************************************
  * isStandstillEventActive
  ******************************************************************************/
  bool AbstractEventHandler::isStandstillEventActive() const
  {
    return (standstillEventActiveCounter > 0U);
  }

  /******************************************************************************
  * getEbReleaser
  ******************************************************************************/
  EBReleaser AbstractEventHandler::getEbReleaser() const
  {
    return ebReleaser;
  }

  /******************************************************************************
  * getSbReleaser
  ******************************************************************************/
  SBReleaser AbstractEventHandler::getSbReleaser() const
  {
    return sbReleaser;
  }

  /******************************************************************************
  * reportEvent
  ******************************************************************************/
  void AbstractEventHandler::reportEvent(const Event& event, const char_t* const file, const int32_t line)
  {
    /* Check if the event is in the active event list.
    * If yes update the end time otherwise add the event.
    * Add the event to tccEvents list and dmiEvents list depending on the event properties.
    */

    bool isReportedEventFound = false;
    const Event::EventType eventType = event.getType();

    if (Event::EventTypeLog == eventType)
    {
      logEvent(event);
      //log in the event in log handler
      AbstractLogHandler::corePtr()->writeEventToLogger(event, file, line);
    }
    else
    {
      int64_t currentTime = vfwGetReferenceTime();
      //update end time
      for (std::vector<EventListItem>::iterator iteEvent = activeEventList.begin();
        iteEvent != activeEventList.end(); ++iteEvent)
      {
        if (event.getId() == iteEvent->event->getId())
        {
          // Event EndTime is sum of current reference time  and event active time
          iteEvent->endTime = currentTime + getMaxEventActiveTime(eventType);
          iteEvent->file = file;
          iteEvent->line = line;
          isReportedEventFound = true;
          break;
        }
      }

      if (!isReportedEventFound)
      {
        //if reported event is not in active event list
        //create a new event list item
        const int64_t totalTime = currentTime + getMaxEventActiveTime(eventType);
        const EventListItem eventListItem(&event, totalTime, line, file);

        if (activeEventList.size() < maxActiveEvents)
        {
          activeEventList.push_back(eventListItem);
          if ((Event::EventTypeEBReq == eventType) ||
            (Event::EventTypeSafeBrakeEB == eventType) ||
            (Event::EventTypeSafetyHalt == eventType))
          {
            applyEB = true;
            if (ebReleaser < static_cast<EBReleaser>(event.getReleaser()))
            {
              ebReleaser = static_cast<EBReleaser>(event.getReleaser());
            }
          }
          else if ((Event::EventTypeSBReq == eventType) ||
            (Event::EventTypeSafeBrakeSB == eventType))
          {
            applySB = true;
            if (sbReleaser < static_cast<SBReleaser>(event.getReleaser()))
            {
              sbReleaser = static_cast<SBReleaser>(event.getReleaser());
            }
          }
          else
          {
            //Do Nothing
          }
        }

        //put the event in tcc and dmi buffer
        logEvent(event);

        //log in the event in log handler
        AbstractLogHandler::corePtr()->writeEventToLogger(event, file, line);
      }
    }
  }

  /******************************************************************************
  * logEvent
  ******************************************************************************/
  void AbstractEventHandler::logEvent(const Event& eventObj)
  {
    //log the event
    const bool hasRoomForTcc2Event = tcc2Events.size() < maxTCCEvents;
    const bool hasRoomForTcc3Event = tcc3Events.size() < maxTCCEvents;

    if (hasRoomForTcc2Event && hasRoomForTcc3Event)
    {
      tcc2Events.push_back(&eventObj);
      tcc3Events.push_back(&eventObj);
    }
 
    if (eventObj.getDmiEventCode() > 0x0U)
    {
      if (dmiEvents.size() < maxDMIEvents)
      {
        dmiEvents.push_back(&eventObj);
      }
    }
  }

  /******************************************************************************
  * corePtr
  ******************************************************************************/
  AbstractEventHandler* AbstractEventHandler::corePtr(void)
  {
    return coreEventHandlerInstancePtr;
  }

  /******************************************************************************
  * EventListItem constructor
  ******************************************************************************/
  AbstractEventHandler::EventListItem::EventListItem(const Event* const eventObj, const int64_t endTimeVal,
    const int32_t lineVal, const char_t* const pText) :
    event(eventObj), endTime(endTimeVal), file(pText), line(lineVal)
  {
    //do nothing 
  }

  /******************************************************************************
  * EventListItem destructor
  ******************************************************************************/
  AbstractEventHandler::EventListItem::~EventListItem()
  {
    file = static_cast<char_t*>(NULL);
    event = static_cast<ATC::Event*>(NULL);
  }

  /******************************************************************************
  * EventListItem operator
  ******************************************************************************/
  AbstractEventHandler::EventListItem& AbstractEventHandler::EventListItem::operator=(const AbstractEventHandler::EventListItem& eventListItem)
  {
    bool bSelfAssignment = false;
    if (this == &eventListItem)
    {
      bSelfAssignment = true;
    }

    if (!bSelfAssignment)
    {
      line = eventListItem.line;
      event = eventListItem.event; //lint !e1555 Ok to copy pointer because object was instantiated outside this class
      file = eventListItem.file; //lint !e1555 Ok to copy pointer because object was instantiated outside this class
      endTime = eventListItem.endTime;
    }
    return *this;
  }

  /******************************************************************************
  * getNextTcc2Event
  ******************************************************************************/
  const Event* AbstractEventHandler::getNextTcc2Event(void)
  {
    const Event* event = static_cast<Event*>(NULL);
    if (tcc2Events.size() > 0U)
    {
      event = tcc2Events.front();
    }
    return event;
  }

  /******************************************************************************
  * getNextTcc3Event
  ******************************************************************************/
  const Event* AbstractEventHandler::getNextTcc3Event(void)
  {
    const Event* event = static_cast<Event*>(NULL);
    if (tcc3Events.size() > 0U)
    {
      event = tcc3Events.front();
    }
    return event;
  }


  /******************************************************************************
  * deleteTcc2Event
  ******************************************************************************/
  void AbstractEventHandler::deleteTcc2Event(void)
  {
    if (tcc2Events.size() > 0U)
    {
      //Deleting the processed event
      static_cast<void>(tcc2Events.erase(tcc2Events.begin()));
    }     
  }

  /******************************************************************************
  * deleteTcc3Event
  ******************************************************************************/
  void AbstractEventHandler::deleteTcc3Event(void)
  {
    if (tcc3Events.size() > 0U)
    {
      //Deleting the processed event
      static_cast<void>(tcc3Events.erase(tcc3Events.begin()));
    }
  }

  /******************************************************************************
  * getNextDmiEvent
  ******************************************************************************/
  const Event* AbstractEventHandler::getNextDmiEvent(void)
  {
    const Event* event = static_cast<Event*>(NULL);
    if (dmiEvents.size() > 0U)
    {
      event = dmiEvents.front();
      static_cast<void>(dmiEvents.erase(dmiEvents.begin()));
    }
    return event;
  }

  /******************************************************************************
  * getNumberOfStandstillEvents
  ******************************************************************************/
  uint16_t AbstractEventHandler::getNumberOfStandstillEvents(void) const
  {
    return standstillEventActiveCounter;
  }

  /******************************************************************************
  * consoleCall
  ******************************************************************************/
  bool AbstractEventHandler::consoleCall(const uint32_t argc, const ConsoleArguments argv)
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
      const char_t* const toWrite = "events        To print all the active events";

      AbstractConsole::corePtr()->writeWithNewline(toWrite);
      retVal = false;
    }
    else if (isTextMatch(&argv[0][0], "events", sizeof("events")) && (argc == 1U))
    {
      if (activeEventList.size() > 0U)
      {
        int32_t ret;

        //lint -e{586} snprintf is needed here
        ret = snprintf(&buffer[0], sizeof(buffer), "%-8s%-8s%-8s%-8s%-10s%-30s%-8s", "Cntnr", "Cmpt", "Num", "Type", "EndTime", "File", "Line");

        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        for (std::vector<EventListItem>::iterator iteEventItem = activeEventList.begin();
            iteEventItem != activeEventList.end(); ++iteEventItem)
        {
          const char_t* filename = getFileNameFromPath(iteEventItem->file, maxSourceFilePathLength);

          //lint -e{586} snprintf is needed here
          ret = snprintf(&buffer[0], sizeof(buffer), "%-8u%-8u%-8u%-8u%-10lld%-30s%-8u",
            iteEventItem->event->getComponentContainer(), iteEventItem->event->getComponentId(),
            iteEventItem->event->getEventNum(), iteEventItem->event->getType(),
            iteEventItem->endTime, filename, iteEventItem->line);

          if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
          {
            AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          }
        }
      }
      else
      {
        AbstractConsole::corePtr()->writeWithNewline("No events in event list");
      }
      retVal = true;
    }
    else
    {
      //Do nothing
    }

    return retVal;
  }
}
