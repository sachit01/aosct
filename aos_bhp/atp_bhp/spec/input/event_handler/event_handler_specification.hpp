namespace ATC
{
  /**
  \if AsMainPage
  \mainpage Event Handler Component Specification (Adaptation)
  @anchor eh
  \endif

  \ifnot AsMainPage
  \class EventHandler
  \endif

  \section Purpose Purpose
  This document specifies the software design for the class EventHandler, the Adaptation part
  of the Event Handler component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview
  \subsection GeneralFunctionality General Functionality
  The EventHandler class instantiates the AbstractEventHandler, generates a singleton object
  and provides active time for events.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  N/A

  Other components have dependencies to this component because they use its public types
  and methods, see \ref EventHandler Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design
  \subsection Initialization Initialization
  The virtual init() function is handled in the Core part

  \subsection ModeDependentOperation Mode dependent operation
  Event Handler component is independent of ATP modes.

  \subsection Scheduling Scheduling
  The EventHandler class is entirely scheduled inside the AbstractEventHandler, no Adaptation specific
  implementation is added.

  \subsection run run()
  It is implemented in AbstractEventHandler class of Event Handler component.

  \subsection EventActiveTime Event Active Time
  EventHandler::getMaxEventActiveTime() function is implemented as a virtual function of the base class and it returns the active time of the Event.
  Where, \a activeTime is set to \a "Keep Standstill Active event time" if event type is \a standstill, otherwise \a "Keep Active event time".

  \section ClassDiagram Class Diagram
  @image html event_handler_class.png  "Class diagram for EventHandler"
  @image latex event_handler_class.png  "Class diagram for EventHandler"

  \section Diagnostics Diagnostics
  \subsection Console Console-commands
  No console commands has been added for analysis for Event Handler component.

  \subsection Analyze Analyze
  No values are registered for analysis for Event Handler component.

  \section CoreAdaptation Core / Adaptation
  - All the major functionality of the Event Handler component is handled in the atc part.
  - The Adaptation of Event Handler component generates a singleton object of AbstractEventHandler
  - The EventHandler class overrides pure virtual function EventHandler::getMaxEventActiveTime() to get the active time for event.

  \section PreProcessor Pre-Processor Directives
  - _DISPATCHER directive available for this component

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].
  Common requirements are specified in SCDS ATP BHP.

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].
  Common requirements are specified in SCDS ATP BHP.

  */
}