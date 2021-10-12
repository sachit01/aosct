namespace ATC
{
  /**
  \if AsMainPage
  \mainpage Event Handler Component Specification
  @anchor eh
  \endif

  \ifnot AsMainPage
  \class AbstractEventHandler
  \endif

  \section Purpose Purpose
  This document specifies the software design for the class AbstractEventHandler, the atc part
  of the Event Handler component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview
  \subsection GeneralFunctionality General Functionality
  The AbstractEventHandler handles the reported events and sends them to TCC events list. However, reported events with DMI event codes are sent to DMI. 
  It is responsible for the event handling mechanism in ATP and other AOS applications such as ATO
  and Dispatcher. In general, the event handling supports a collection of
  reported events, which are processed with in the required AOS application and provides access of Brake flags and ATP mode change flags to other components.
  The AbstractEventHandler supports:
  - Reporting active events from various components.
  - Forwarding reported events to the Log Handler component.
  - Internal states to determine active event status and provides brake flags to the other components accordingly.
  - Timeout handling to deactivate events when no longer reported by a component.
  - Storing the active events for TCC and DMI and give access functions to read them.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  This component has dependencies to the following component:
  - Log Handler - To log the events on RU/N-JRU.

  Events are raised by other components, these components comply with the following rules:
  - Use the available interface to report active event codes
  - Log events are not put in the active event list and not processed every cycle.
  - Repeat the event code of event of active event list in each execution cycle, otherwise it will be deleted from active event list. 
  - Use unique event code for each purpose

  Other components have dependencies to this component because they use its public types
  and methods, see \ref AbstractEventHandler Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design
  \subsection Initialization Initialization
  AbstractEventHandler::init() function clears all the events lists, and initializes variables with default values.

  \subsection ModeDependentOperation Mode dependent operation
  Event Handler component is independent of ATP modes.

  \subsection Scheduling Scheduling
  AbstractEventHandler::run() function is called in each execution cycle.

  \subsubsection run run()
  AbstractEventHandler::run() function is called by ATP Application component to perform background processing of required AOS application.
  It performs the following:
  - Set default value to following parameters:
   - flag to indicate if any event requested SB in last cycle.
   - flag to indicate if any event requested EB in last cycle.
   - flag to indicate if any event requested mode change to SafetyHalt last cycle.
   - flag to indicate if any event requested mode change to SafeBrakeToStop last cycle.
   - number of the active Standstill events
   - flag to indicate highest authority releaser of EB in last cycle.
   - flag to indicate highest authority releaser of SB in last cycle.
  - If end time is more than current reference time or event type is \a SafetyHalt, event type and end time are fetched from active Event list.
  Otherwise, event will be deleted from active event list.
  - Update following  based on event type, which is defined in \ref DefinedEventCodes.
   - flag to indicate if any event requested SB in last cycle.
   - flag to indicate if any event requested EB in last cycle.
   - flag to indicate if any event requested mode change to SafetyHalt last cycle.
   - flag to indicate if any event requested mode change to SafeBrakeToStop last cycle.
  - Update service and emergency brake releasers flags based on \ref DefinedBrakeReleasorLevels.

  \subsection EventHandling Event Handling
  The AbstractEventHandler defines the following principles to implement events within AOS. These similar principles are
  implemented for Dispatcher and ATO processes. <br>
  - Event code structure
  - Event reporting
  - Event life cycle
  - Event response triggering
  - Forwarding events to log devices

  An event is defined by a collection of data described by source part and reaction part. The source part
  includes user process, reporting component, component unique event id, the source code filename
  and line number. The reaction part specifies that respective flag of brake, highest authority releaser, mode change 
  will be set according to the severity of event and these flags will be used by Brake and Mode control components.<br>

  The Event codes are further described in sub chapters later in this document.
  These Event codes are not shared between Abstract part and Adaptation part.
  This is a built in requirement forwarded to all components using the
  AbstractEventHandler.

  The event life cycle depends on the type of event. If brake response is involved with events, these events are assigned
  a life cycle with the \a end \a time. Once \a end \a time expires, events are reported to the Log Handler component.
  After passing \a end \a time of event, it is removed from the active event list. Every time an event with brake reaction is reported that is already in the active event list, the \a end \a time is renewed based on the event type.
  Whereas \a end \a time is sum of event active time and configured maximum event time based on event type. 
  However reported Log event is not added in the active event list. But it is reported to Log Handler component in the same execution cycle.


  The Event brake response triggering will scan the active event list, identify the event
  with defined brake reaction and its brake release authority level for both emergency brake
  and service brake in each cycle.However event with emergency brake will get highest priority.
  Other components and the other AOS applications access this information by using interface methods.


  \subsubsection DefinedEventCodes Defined Event Codes
  The following event levels are defined:

  Eventlevel         | Brake reaction  | Definition
  ------------------- | --------------- | --------------------------------------------
  Log                 | -               | Forwarded to log component.
  SBReq               | Service         | Service Brake application when reported, allows resumed operation once at standstill.
  EBReq               | Emergency       | Emergency Brake application when reported, allows resumed operation once at standstill.
  Standstill          | Service         | Standstill event when reported, protects the train movement by applying SB.
  SafeBrakeSB         | Service         | Service Brake application when reported, change mode to SafeBrakeToStop.
  SafeBrakeEB         | Emergency       | Emergency Brake application when reported, change mode to SafeBrakeToStop.
  SafetyHalt          | Emergency       | Emergency Brake application when reported, change mode to SafetyHalt.

  \subsubsection DefinedBrakeReleasorLevels Defined Brake Releaser Levels
  The following brake releaser levels are defined:
  Brake releaser      | Authority level | Definition
  ------------------- | --------------- | --------------------------------------------
  DMI / Driver        | Highest         | Driver in cab.
  Dispatcher / Remote |                 | Brake release from train control room allowed/requested.
  ATO                 | Lowest          | ATO may release reaction.
  However, Emergency brake can be released by DMI/driver and Service brake can be release by any of three levels described as above.

  \subsubsection ActiveEventManagement Active Event Management
  The AbstractEventHandler manages event list and implements an active events list as below:
  - All the events have unique id.
  - The event list stores all reported events with brake response and considered as active.
  - The event list stores the time of last event report for each event.
  - Events are removed from the list, when the configured time is expired. This is being performed in the AbstractEventHandler::run method.

  The AbstractEventHandler manages 2 event lists as below:
  - Events to be sent to TCC.
  - Events to be sent to DMI indicated by the DMI Event code in Event structure.

  When a new event received is not available in the active event list, then event is added to above defined lists from the report event function. The maximum size of these lists is passed in the constructor by
  the Adaptation part. If the list is full, any additional reported events are discarded. The AbstractEventHandler provides interfaces to get event from the lists (to be sent to TCC or DMI or both). The events are
  returned in first in first out order by the interface. Once the event is read through the interface
  it is removed from the corresponding list.

  \subsubsection EventHandlerOutputSelection Event Handler Output Selection
  The Event Handler component sets brake reaction for other components based on severity level of requested event.\n
  The following brake reactions are defined:
  Brake reaction  | Definition
  --------------- | --------------------------------------------
  Emergency       | Requests application of emergency brake.
  Service         | Requests application of service brake.

  \subsubsection ReportEvent reportEvent function
  Other components call the AbstractEventHandler::reportEvent() function to raise events independently, i.e. at the code
  line where it occurs. However log events are logged in each call of this function in Log Handler component. Therefore, it should be handled by the user component.
  The AbstractEventHandler processes and stores the event codes. This method performs as below:
  - Log the reported event in Log Handler component and add the reported event in TCCs and DMI buffer list, if event type is \a Event::EventTypeLog.
  - Check if the active event list has the reported events.
  - Perform as below, if the Unique id of active events match with active event list.
  - Calculate end time of each active event based on given active time of event object.
  - Update file name and line fields of each active event based on source code file name and line number of reporting this event object
  - Perform as below, if the Unique id of active events do not match with active event list.
  - Add active event with its file, line number and EndTime in active event list.
  - Apply emergency brake and update the emergency brake releaser for below event types.
  - \a EBReq
  - \a SafeBrakeEB
  - \a SafetyHalt
  - Apply service brake and update the service brake releaser for below event types
  - \a SBReq
  - \a SafeBrakeSB
  - Log the reported event in Log Handler component and add the reported event in TCCs and DMI buffer list. \n

  \section ClassDiagram Class Diagram

  @image html abstract_event_handler_class.png "AbstractEventHandler Class diagram"
  @image latex abstract_event_handler_class.png "AbstractEventHandler Class diagram"

  Event class defines the event structure with creators for events. It generates unique event id using \a Block, \a Container, \a Component and \a eventNumber.
  This class is being used by Event Handler component and other components, which raise events.

  @image html event_class.png "Event Class Diagram"
  @image latex event_class.png "Event Class Diagram"

  \section Diagnostics Diagnostics
  \subsection Console Console-commands
  The following component-specific console-commands are implemented:
  - events : Print container, component, event number, event Type, EndTime, source file and Line number.

  \subsection Analyze Analyze
  - No values are registered for analysis for Event Handler component.

  \section CoreAdaptation Core / Adaptation
  The Event Handler component is split in atc and Adaptation parts. Atc part implements all functionality of Event Handler component.
  However, the Adaptation part overrides pure virtual function AbstractEventHandler::getMaxEventActiveTime().

  \section PreProcessor Pre-Processor Directives
  - _DISPATCHER directive available for this component

  \section Traceability Traceability
  \subsection SSRS Functional Requirements
  The functional requirements are defined in [SSRS].
  Common functional requirements are described in SCDS ATC.
  The requirements relevant for this component are:

  Req        | Chapter          | Function
  ---------- |  ----------------| -----------
  AOS 2098   | \ref Scheduling  | AbstractEventHandler::run()
  AOS 2099   | \ref Scheduling  | AbstractEventHandler::run()
  AOS 2071   | \ref Scheduling  | AbstractEventHandler::run()
  AOS 2072 S | \ref Scheduling  | AbstractEventHandler::run()
  AOS 2074   | \ref Scheduling  | AbstractEventHandler::reportEvent()
  AOS 2075   | \ref Scheduling  | AbstractEventHandler::reportEvent()
  AOS 2708 S | \ref Scheduling  | AbstractEventHandler::reportEvent()
  AOS 173    | \ref Scheduling  | AbstractEventHandler::reportEvent()
  AOS 1977   | \ref ActiveEventManagement | AbstractEventHandler::run()
  AOS 1978   | \ref ActiveEventManagement | AbstractEventHandler::run()
  AOS 1980   | \ref ActiveEventManagement | AbstractEventHandler::reportEvent()
  AOS 1995   | \ref Scheduling  | AbstractEventHandler::reportEvent()
  AOS 2015   | \ref Scheduling  | AbstractEventHandler::run()
  AOS 2021   | \ref Scheduling  | AbstractEventHandler::run()
  AOS 2597   | \ref Scheduling  | AbstractEventHandler::run()
  AOS 2027   | \ref ActiveEventManagement | AbstractEventHandler::reportEvent()
  AOS 1710   | \ref Scheduling  | AbstractEventHandler::run()

  \subsection SSAS SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATC.

  */
}

