#ifndef EventHandler_hpp
#define EventHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The class is the adaptation implementation for EventHandler component
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
* 2016-06-09    arastogi    Updated based on the new design
* 2016-09-19    akushwah    Corrected Init function
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_event_handler.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATC
{
    /**
    * The class EventHandler instantiates the abstract class and implements
    * the interfaces needed for both inherited classes and component.
    *
    */
    class EventHandler : public AbstractEventHandler
    {
    public:
        /**
        * Singleton instance.
        * Only one instance of this class is allowed.
        * @return the one and only instance.
        *
        * NOTE: Singleton handling shall only be used in Adaptation, not Core!
        */
        static EventHandler& instance(void);
      
    private:

        /**
        * Constructor.
        * Singleton instance.
        * Declare constructor as private in order to prevent illegal use.
        */
        EventHandler();

        /**
        * Declare copy-constructor as private in order to prevent illegal use.
        */
        EventHandler(const EventHandler&);

        /**
        * Declare assignment-operator as private in order to prevent illegal use.
        */
        EventHandler& operator = (const EventHandler&);

        /**
        * The maximum number of events that can be active at a given time.
        *
        */
        static const uint16_t maxActiveEventCount = 100U;

        /**
        * The maximum number of events for TCC at a given time.
        *
        */
        static const uint16_t maxTCCEventCount = 10U;

        /**
        * The maximum number of events for DMI at a given time.
        *
        */
        static const uint16_t maxDMIEventCount = 10U;

    protected:

      /**
      * Get the keep active time for events with the given type
      *
      * @param[in] eventType the type of event for which to return the active time
      *
      * @return keep active time, in milliseconds.
      */
      virtual int64_t getMaxEventActiveTime(const Event::EventType eventType) const;
    };
}

#endif
