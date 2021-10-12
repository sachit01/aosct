/**
\if AsMainPage
\mainpage Event Handling(section (6.3.21))
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-05-08 |Proposed changes for Event Handling Requirements | spandita


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
EB             | Emergency Brake
SB             | Service Brake
DMI            | Driver Machine Interface
TODO           | To be discussed
TCC            | Train Control Center

\section Introduction Introduction
Below changes are proposed as per the doors requirements defined in section 6.3.21 for event handling.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
Req     | Short requirement description                                                         | Description 
------- | --------------------------------------------------------------------------------------| ------------
AOS 1977 | All Events in AOS except platform halt event shall have a unique Id. | Implemented
AOS 1978 | The AOS shall prioritize active events in following order: PlatformHalt, SafetyHalt, SafeBrakeToStop, Brake, Log. | Partially Implemented  
AOS 1980 | The AOS shall, except for Platform Halt and Log event, keep the event active for a configurable amount of time after the trigger is removed. | Partially Implemented
AOS 1989 | If the platform Halt event is issued, AOS shall call VFW_HALT macro with the cause and source of the error. | Partially Implemented
AOS 1995 | If the safety Halt event is issued AOS shall apply EB and enter into Safety Halt Mode if mode transition is allowed, event shall be logged in NJRU, BDS and shall Inform to TCC and driver.  | Partially Implemented   
AOS 2015 | If the safe brake to stop event is issued the AOS shall apply SB(EB if explicitly stated in the trigger condition) and enter into Safe brake to stop mode if mode transition is allowed, The event shall be logged to NJRU, BDS and shall inform to TCC and Driver. |Partially Implemented 
AOS 2021 | If the Brake event is issued the AOS shall apply SB (EB if explicitly stated in the trigger condition), events shall be logged to NJRU, BDS and shall inform to TCC. | Partially Implemented
AOS 2027 | If the Log event is issued, The event shall be logged to NJRU, BDS and shall be informed to TCC. | Partially Implemented

 \subsection ExternalInterfaceDescription External Interface Description
\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection AOS1978 AOS1978
Affected component  : Event Handler \n
No need to implement as it is already implemented.


\subsection  AOS1980 AOS1980
Affected component  : Event Handler and Config \n
- Config : Create a new parameter eventKeepActiveTime of type uint16_t and its getter function in config (unit of parameter should be in seconds). \n
           Assign a numeric value 5 to it(TODO).\n
- Event Handler : Remove the class member variable maxEventActiveTime from the event handler class and assign the newly created config parameter(eventKeepActiveTime) value to eventActiveTime variable \n
of abstract event handler class by using getter function for parameter eventKeepActiveTime in config component.

\subsection  AOS1989 AOS1989
After confirmation with Bo there is no need to implement this requirement as it is important to halt the system as quickly and safe as possible \n
without any complicated code in between. 

\subsection  AOS1995 AOS1995
Affected  component  : All \n
Need to update the TCC flag to true and DMI event codes at the time of initialization of safety halt events in the respective components.\n
and pass the same event text as string to BDS. \n
Also english.ini file of DMI need to be update as per the provided DMI event codes in ATP.

\subsection  AOS2015 AOS2015
Affected  component  : All \n
Need to update the TCC flag to true and DMI event codes at the time of initialization of safe brake to stop events(SB/EB) in the respective components.\n
and pass same event text as string to BDS. \n
Also english.ini file of DMI need to be update as per the provided DMi event codes in ATP.


\subsection  AOS2021 AOS2021
Affected  component  : All \n
Need to update the TCC flag to true at the time of initialization of brake events(EB/SB) in the respective components.\n

\subsection  AOS2027 AOS2027
Affected component:  Message Handler and all \n
- Message Handler: In radio_message_out_position_report.cpp reserve the vector size for TCC event code to maxTCCEventCount member of event_handler class.\n
Get the TCC event ID and event text in collectData function of position report class by using getNextTccEvent function of event handler.\n
Store the event ID in vector of  position report class (ATP event text will not sent to TCC until it is confirmed by requirement team).
- All : Need to pass TCC event flag as true while initialization of log events in respective components.

\section AdditionalMaterial Additional Material

*/