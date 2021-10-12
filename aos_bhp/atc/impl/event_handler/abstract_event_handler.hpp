#ifndef AbstractEventHandler_hpp
#define AbstractEventHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  This abstract class defines the EventHandler functionality.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-05    arastogi    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-04-21    lantback    Implemented corePtr()
* 2016-05-04    lantback    Updated documentation issues after first review
* 2016-06-09    arastogi    Updated based on the new design
* 2016-06-20    saprasad    Added function prototype for logEvent and 
*                           EventListItem and Lint fixes  
* 2016-06-22    saprasad    Incorporated Event Handler Review Comments
* 2016-07-05    saprasad    Added testEventHandle function for testing.
* 2016-09-19    akushwah    Corrected Init function
* 2016-10-12    nsyed       Store events as pointers in eventList
* 2016-10-14    nsyed       Modify reportEvent function to accept 32bit line param
* 2016-10-25    nsyed       Change line parameter from uint32_t to int32_t
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "event.hpp"
#include <vector>

/******************************************************************************
* DECLARATIONS    
******************************************************************************/
namespace ATC
{
  class AbstractEventHandler;
  /** 
  *Static variable to store the single instance of AbstractEventHandler
  * Variable shall be setup during construction of the single instance used within ATP.
  * The variable is returned by corePtr() and used by the core ATP logic to access
  * adaptation objects through the core class.
  *
  * Note: During construction the variable shall be checked to guarantee that only 
  *       one instance is created. Should the variable be set to non-zero the execution shall
  *       be immediately interrupted and a safe state issued.
  */
  static AbstractEventHandler* coreEventHandlerInstancePtr = static_cast<AbstractEventHandler*>(NULL);
  
  /**
  * The class AbstractEventHandler implements the interface defined by the ComponentBase class.
  *
  */
  class AbstractEventHandler : public ProcComponent
  {
  public:
    /*** The structure defining entries to the activeEventList.**/
    struct EventListItem
    {
      /** Reference to active event object */
      const Event* event;
      /** Time when active event object is to be released */
      int64_t endTime;
      /** Source code file name reporting this event object */
      const char_t* file;
      /** Source code line reporting this event object */
      int32_t line;
      /*** Parameterized EventListItem constructor ****/
      EventListItem(const Event* eventObj, const int64_t endTimeVal, const int32_t lineVal, const char_t* const pText);
      /*** Default EventListItem constructor ****/
      EventListItem();
      /*** Default EventListItem destructor ****/
      ~EventListItem();
      /*** EventListItem assignment operator ****/
      EventListItem& operator=(const EventListItem& eventListItem);
    };

    /**
    * Implements the virtual init function.
    *
    * @return Returns true when initialization completed
    */
    virtual bool init(void);
    
    /** 
    *   Implements the virtual run function
    *
    */
    virtual void run(void);
    
    /**
    * Interface to call different level of Console Command
    *
    * @param[in] argc  Number of arguments in the argument array argv
    * @param[in] argv  Arguments array
    *
    * @return true if the Call is successful.
    */
    virtual bool consoleCall(const uint32_t argc, const ConsoleArguments argv);

    /**
    * Getter function for applyEB.
    *
    * Indicates if any event requested EB in last cycle.
    *
    * @return Returns true if EB shall be applied.
    */
    bool isApplyEb() const;
    
    /**
    * Getter function for applySB.
    *
    * Indicates if any event requested SB in last cycle.
    *
    * @return Returns true if SB shall be applied.
    */
    bool isApplySb() const;

    /**
    * Getter function for SafeBrakeToStop mode change Request.
    *
    * Indicates if any event requested SafeBrakeToStop in last cycle.
    *
    * @return Returns true if mode change to SafeBrakeToStop is required.
    */
    bool isModeChangeToSafeBrakeToStop() const;

    /**
    * Getter function for SafetyHalt mode change Request.
    *
    * Indicates if any event requested SafetyHalt in last cycle.
    *
    * @return Returns true if mode change to SafetyHalt is required.
    */
    bool isModeChangeToSafetyHalt() const;

    /**
    * getter function to know if the Standstill Event is active or not.
    *
    * @return Returns true if Standstill event is active.
    */
    bool isStandstillEventActive() const;

    /**
    * Getter function for ebReleaser.
    *
    * Indicates the highest EB release authority in last cycle.
    *
    * @return Returns required authority level to release EB
    */
    EBReleaser getEbReleaser() const;
    
    /**
    * Getter function for sbReleaser.
    *
    * Indicates the highest SB release authority in last cycle.
    *
    * @return Returns required authority level to release SB
    */
    SBReleaser getSbReleaser() const;
    
    /**
    * External interface to report events.
    *
    * The function takes the event reported and checks if the event is in the active event list.
    * If yes update the end time otherwise add the event.
    * Add the event to tccEvents list and dmiEvents list depending on the event properties.
    *
    * @param[in] event  Reference to the event being reported.
    * @param[in] file   Pointer to the filename from where the event is reported.
    * @param[in] line   Line number from where the event is reported.
    *
    */
    void reportEvent(const Event& event, const char_t* const file, const int32_t line);
    
    /**
    * Get core instance pointer
    *
    * @return Pointer to single instance core object.
    */
    static AbstractEventHandler* corePtr();
    
    /**
    * Get the next event in Tcc2 event list.
    * It will return NULL if the list is empty.
    *
    * @return Pointer to next event in the list, NULL if empty.
    */
    const Event* getNextTcc2Event();

    /**
    * Get the next event in Tcc3 event list.
    * It will return NULL if the list is empty.
    *
    * @return Pointer to next event in the list, NULL if empty.
    */
    const Event* getNextTcc3Event();


    /**
    *The function to delete the next TCC2 event from the list.
    */
    void deleteTcc2Event(void);

    /**
    *The function to delete the next TCC3 event from the list.
    */
    void deleteTcc3Event(void);
    
    /**
    * Get the next event in DMI event list.
    *
    * The returned event is removed from the list.
    * It will return NULL if the list is empty.
    *
    * @return Pointer to next event in the list, NULL if empty.
    */
    const Event* getNextDmiEvent();

    /**
    * Get the number of standstill events of ActiveEventList
    *
    * It will return number of standstill events
    *
    */
    uint16_t getNumberOfStandstillEvents(void) const;

 
  protected:
    /**
    * Constructor
    */
    AbstractEventHandler(const uint16_t maxEvents, const uint16_t maxTCCEventsCount,
      const uint16_t maxDmiEventsCount);

    /**
    * Get the keep active time for events with the given type
    *
    * @param[in] eventType the type of event for which to return the active time
    *
    * @return keep active time, in milliseconds.
    */
    virtual int64_t getMaxEventActiveTime(const Event::EventType eventType) const = 0;

  private:

    /**
    * Flag to prevent multiple initialization.
    */
    bool initDone;

    /**
    * Default AbstractEventHandler Constructor.
    *
    */
    AbstractEventHandler();
    /**
    * logEvent Function.
    *
    */
    void logEvent(const Event& eventObj);
    
    /**
    * The maximum number of events that can be active at a given time.
    *
    */
    const uint16_t maxActiveEvents;
    
    /**
    * The maximum number of events that have to be sent to TCC at a given time.
    *
    */
    const uint16_t maxTCCEvents;
    
    /**
    * The maximum number of events that have to be sent to DMI at a given time.
    *
    */
    const uint16_t maxDMIEvents;
    
    /**
    * The vector of events to be sent to TCC2.
    *
    */
    std::vector<const Event*> tcc2Events;

    /**
    * The vector of events to be sent to TCC3.
    *
    */
    std::vector<const Event*> tcc3Events;

    
    /**
    * The array of events to be sent to DMI.
    *
    */
    std::vector<const Event*> dmiEvents;

    /**
    * The boolean to indicate if any event requested SB in last cycle.
    *
    */
    bool applySB;
    
    /**
    * The boolean to indicate if any event requested EB in last cycle.
    *
    */
    bool applyEB;

    /**
    * The boolean to indicate if any event requested mode change to SafeBrakeToStop last cycle.
    *
    */
    bool modeChangeToSBS;

    /**
    * The boolean to indicate if any event requested mode change to SafetyHalt last cycle.
    *
    */
    bool modeChangeToSHT;

    /**
    * The number of the active Standstill events. This will be used in the Supervise for Standstill supervision.
    */
    uint16_t standstillEventActiveCounter;

    /**
    * The highest authority releaser of EB in last cycle.
    *
    */
    EBReleaser ebReleaser;
    
    /**
    * The highest authority releaser of SB in last cycle.
    *
    */
    SBReleaser sbReleaser;
    
    /**
    * The vector of all active events.
    *
    */
    std::vector<EventListItem> activeEventList;

  };

}

#endif
