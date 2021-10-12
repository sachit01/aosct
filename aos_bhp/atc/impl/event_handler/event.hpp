#ifndef Event_hpp
#define Event_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  This class defines the event structure with creators for events.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-05    arastogi    Created.
* 2016-05-24    adgupta     Updated create event functions to be static.
* 2016-06-09    arastogi    Updated based on the new design.
* 2016-06-20    saprasad    Lint Fixes.
* 2016-06-22    saprasad    Incoroparated Event Handler Review Comments.
* 2016-09-23    bhermans    maxTextLen increased 50->80.
* 2016-10-12    arastogi    Removed the = operator for event.
* 2016-10-14    nsyed       Re-design event id to 32 bits.
* 2016-10-19    nsyed       Re-design Event Types/ Levels
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "atc_types.hpp"

#ifdef GENERATE_TCC_EVENTLIST_XML_PATH
#include <fstream>
#endif

/******************************************************************************
* DECLARATIONS    
******************************************************************************/
namespace ATC
{
  /**
  * The class AbstractEvent defines the events.
  *
  */
  class Event
  {
  public:

    /**
    * The maximum length of the text describing the event..
    *
    */
    static const uint8_t maxTextLen = 80U;

    /**
    * The maximum length of the dynamic Text.
    *
    */
    static const uint8_t maxDynamicTextLen = 35U;

    /**
    * The Event types supported.
    * SafeBrake events will trigger mode change to SafeBrakeToStop.
    * SafetyHalt event will trigger mode change to SafetyHalt.
    *
    */
    enum EventType
    {
      EventTypeLog,
      EventTypeSBReq,
      EventTypeEBReq,
      EventTypeStandstill,
      EventTypeSafeBrakeSB,
      EventTypeSafeBrakeEB,
      EventTypeSafetyHalt
    };

    /**
    *  Create a Log Event.
    *
    *  @param[in] cmpt           The component defining the event.
    *  @param[in] cntnr          The container of the component defining the event.
    *  @param[in] eventNr        The event number for the component.
    *  @param[in] dmi            The dmi message for this event.
    *  @param[in] txt            The text describing the event.
    *  @param[in] useDynamicText flag to indicate if event should use Dynamic text while reporting.
    *
    *  @return Event object created using the passed parameters.
    */
    static Event createLogEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr,
       const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText = false);

    /**
    *  Create a Service Brake Request Event.
    *
    *  @param[in] cmpt           The component defining the event.
    *  @param[in] cntnr          The container of the component defining the event.
    *  @param[in] eventNr        The event number for the component.
    *  @param[in] sbRel          The SB releaser for this event.
    *  @param[in] dmi            The dmi message for this event.
    *  @param[in] txt            The text describing the event.
    *  @param[in] useDynamicText flag to indicate if event should use Dynamic text while reporting.
    *
    *  @return Event object created using the passed parameters.
    */
    static Event createSBReqEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr, const SBReleaser sbRel,
       const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText = false);

    /**
    *  Create an Emergency Brake Request Event.
    *
    *  @param[in] cmpt           The component defining the event.
    *  @param[in] cntnr          The container of the component defining the event.
    *  @param[in] eventNr        The event number for the component.
    *  @param[in] ebRel          The EB releaser for this event.

    *  @param[in] dmi            The dmi message for this event.
    *  @param[in] txt            The text describing the event.
    *  @param[in] useDynamicText flag to indicate if event should use Dynamic text while reporting.
    *
    *  @return Event object created using the passed parameters.
    */
    static Event createEBReqEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr, const EBReleaser ebRel,
       const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText = false);

    /**
    *  Create a Standstill Event to place the train at standstill without brake intervention till configured values of speed and distance are exceeded by the train.
    *
    *  @param[in] cmpt           The component defining the event.
    *  @param[in] cntnr          The container of the component defining the event.
    *  @param[in] eventNr        The event number for the component.
    *  @param[in] sbRel          The SB releaser for this event.
    *  @param[in] dmi            The dmi message for this event.
    *  @param[in] txt            The text describing the event.
    *  @param[in] useDynamicText flag to indicate if event should use Dynamic text while reporting.
    *
    *  @return Event object created using the passed parameters.
    */
    static Event createStandstillEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr, const SBReleaser sbRel,
       const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText = false);

    /**
    *  Create a SafeBrakeToStop mode change with Service Brake Event.
    *
    *  @param[in] cmpt           The component defining the event.
    *  @param[in] cntnr          The container of the component defining the event.
    *  @param[in] eventNr        The event number for the component.
    *  @param[in] sbRel          The SB releaser for this event.
    *  @param[in] dmi            The dmi message for this event.
    *  @param[in] txt            The text describing the event.
    *  @param[in] useDynamicText flag to indicate if event should use Dynamic text while reporting.
    *
    *  @return Event object created using the passed parameters.
    */
    static Event createSafeBrakeSBEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr, const SBReleaser sbRel,
       const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText = false);

    /**
    *  Create a SafeBrakeToStop mode change with Emergency Brake Event.
    *
    *  @param[in] cmpt           The component defining the event.
    *  @param[in] cntnr          The container of the component defining the event.
    *  @param[in] eventNr        The event number for the component.
    *  @param[in] ebRel          The EB releaser for this event.
    *  @param[in] dmi            The dmi message for this event.
    *  @param[in] txt            The text describing the event.
    *  @param[in] useDynamicText flag to indicate if event should use Dynamic text while reporting.
    *
    *  @return Event object created using the passed parameters.
    */
    static Event createSafeBrakeEBEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr, EBReleaser ebRel,
       const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText = false);

    /**
    *  Create a Safety Halt Event.
    *
    *  @param[in] cmpt           The component defining the event.
    *  @param[in] cntnr          The container of the component defining the event.
    *  @param[in] eventNr        The event number for the component.
    *  @param[in] ebRel          The EB releaser for this event.
    *  @param[in] dmi            The dmi message for this event.
    *  @param[in] txt            The text describing the event.
    *  @param[in] useDynamicText flag to indicate if event should use Dynamic text while reporting.
    *
    *  @return Event object created using the passed parameters.
    */
    static Event createSafetyHaltEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr, const EBReleaser ebRel,
       const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText = false);

    /**
    *  Get the id of the event.
    *
    *  @return Event id created using the passed parameters.
    */
    uint32_t getId() const;

    /**
    *  Get the id of the component defining the event.
    *
    *  @return componentID value.
    */
    ComponentIDType getComponentId() const;

    /**
    *  Get the container of the component defining the event.
    *
    *  @return container value.
    */
    ComponentContainers getComponentContainer() const;

    /**
    *  Get the DMI event code for the event.
    *
    *  @return dmiEventCode value.
    */
    DMIEventCode getDmiEventCode() const;

    /**
    *  Get the event number defined by the component creating it.
    *
    *  @return eventNum value.
    */
    uint16_t getEventNum() const;

    /**
    *  Get the Releaser value in uint8_t.
    *  Can be SBReleaser or EB releaser based on the event type.
    *
    *  @return releaser value.
    */
    uint8_t getReleaser() const;

    /**
    *  Get the text describing the event.
    *
    *  @return text value.
    */
    const char_t* getText() const;

    /**
    *  Get the type of the event.
    *
    *  @return type value.
    */
    EventType getType() const;

    /**
    * Default Constructor.
    *
    */
    Event();

    /**
    * The maximum size for the cross compare data
    *
    * @return Returns the maximum data size for this item
    */
    uint32_t getWriteCrossCompareMaxSize() const;

    /**
    * Add attributes to cross compare
    */
    void writeCrossCompare(VFW_Buffer* const buffer) const;

    /**
    *  Access-function to get the dynamic text 
    *
    *  @return Dynamic text value, NULL if no dynamic text is available.
    */
    const char_t* getDynamicText() const;

    /**
    *  Access-function to set the string as dynamic text
    *
    *  @param[in] dynamicTextToBeSet  Dynamic Text to be set. Maximum @ref maxDynamicTextLen characters will be copied.
    */
    void setDynamicText(const char_t* const dynamicTextToBeSet) const;

    /**
    *  Access-function to set the some value as dynamic text
    *
    *  @param[in] valueToSet  Dynamic Text to be set
    */
    void setDynamicText(const uint32_t valueToSet) const;

  protected:

    /**
    * Constructor.
    *
    *  @param[in] cmpt           The component defining the event.
    *  @param[in] cntnr          The container of the component defining the event.
    *  @param[in] eventNr        The event number for the component.
    *  @param[in] eventType      The type or severity of the event.
    *  @param[in] rel            The EB or SB releaser for this event.
    *  @param[in] dmi            The dmi message for this event.
    *  @param[in] txt            The text describing the event.
    *  @param[in] useDynamicText flag to indicate if event should use Dynamic text while reporting.
    */
    Event(ComponentIDType cmpt, const ComponentContainers cntnr, uint16_t eventNr, EventType eventType, uint8_t rel,
          DMIEventCode dmi, const char_t* const txt, const bool useDynamicText);



  private:

    /**
    * The component defining the event.
    *
    */
    ComponentIDType componentID;

    /**
    * The container of the component defining the event.
    *
    */
    ComponentContainers container;

    /**
    *  The event number for the component
    *
    */
    uint16_t eventNum;

    /**
    * The type or severity of the event.
    *
    */
    EventType type;

    /**
    * The EB or SB releaser for this event.
    *
    */
    uint8_t releaser;

    /**
    * The boolean to indicate if event should send dynamic text while reporting or not.
    *
    */
    bool isDynamicTextSet;

    /**
    * The dmi message for this event.
    *
    */
    DMIEventCode dmiEventCode;

    /**
    * The  constant factor for Block to generate Unique Event Id
    * Two raise to power 22 ,pow(2,22) = 4194304 = 2^22
    */
    static const uint32_t blockOffsetMultiplier = 4194304U;

    /**
    * The  constant factor for Container to generate Unique Event Id
    * Two raise to power 20 ,pow(2,20) = 1048576 = 2^20
    */
    static const uint32_t containerOffsetMultiplier = 1048576U;

    /**
    * The  constant factor for Component  to generate Unique Event Id
    * Two raise to power 12 ,pow(2,12) = 4096 = 2^12
    */
    static const uint32_t componentOffsetMultiplier = 4096U;

    /**
    * The text describing the event.
    *
    */
    char_t text[maxTextLen];

    /**
    * The pointer for the dynamic text describing the event.
    *
    * Note: This is made mutable so that setDynamicText can be const - which
    * in turn makes it possible to call setDynamicText from a const method)
    */
    mutable char_t dynamicText[maxDynamicTextLen + 1U];

    /**
    * The unique id for the event.
    *
    */
    uint32_t id;

#ifdef GENERATE_TCC_EVENTLIST_XML_PATH
    /**
    * Method to append event-information to the off-line file describing the AOS events.
    *
    */
    void registerEvent() const;

#endif

  };
}

#endif
