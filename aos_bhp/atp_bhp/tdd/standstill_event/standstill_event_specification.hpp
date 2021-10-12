/**
\if AsMainPage
\mainpage Standstill event and standstill supervision
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-10-04 | Creation of Standstill event and standstill supervision  | adgupta


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description

\section Introduction Introduction
Standstill event is raised when there is a need to have the Train at Standstill without the use of any brakes. It should be the responsibility of the driver/ATO to stay on Standstill.
In case the AOS detects any kind of movement, it should apply brakes to prevent it.

This event shall be logged to the N-JRU. TCC shall be informed and train movement should be supervised by the AOS. AOS shall apply Brake intervention in case of any movement is detected.

Standstill supervision prevents the vehicle from moving when a standstill event is active. Also, the standstill supervision should allowDriver or ATO to revoke the brakes when the train
comes back to a standstill. In the ATO mode ATO Manual, the AOS shall inform the Driver when Brake event is issued because of standstill event.

This TDD also involves the design to update the implementation wherever the brake event is replaced by the standstill event in the requirements.

\subsection Design Design Overview
This document deals with the design to implement the Standstill event and its supervision.

\subsubsection CreationOfStandstillEvent Creation of Standstill event:Event Handler
- A new event type: standstill event is to be created.
- createStandstillEvent() function is to be created to create an object of Event type EventTypeStandstill with parameters as in other createEvent functions.

\subsubsection StandstillSupervision Standstill Supervision
- In Config component, Create 2 configuration parameters for limiting speed at standstill and limiting distance at standstill.
- In EventHandler component, create an API which returns if Standstill event is active or not.to be used in Supervise, for standstill supervision.
- In supervise component, create an SB event to apply brakes while standstill supervision is enabled and movement is detected.
- In AbstractSupervise::run(), check if the standstill event is active. If yes: do the following.
  + get the current Front Odo Position and save this as standstill odo Position.
  + In each of the coressponding cycle, get the current Front Odo Position and check if displacement(standstill odo position - current odo position) this is 
  more than configured value for standstill movement.
  + get the train speed from odometry component, check if speed is more than configured value for standstill movement.
  + apply service brake(trigger StandstillSB service brake event) if any of the above 2 conditions gets satisfied with releaser as NoSB.
  + to remove the brake condition check if train is at standstill: if yes, update the standstillodo position with the current odo position. This will nullify the brake condition.

\subsubsection TriggerStandstillEvent Triggering Standstill Event
- Check for the following modes to trigger Standstill event. i.e.- issue standstill event when in following modes.
  + Power up
  + Configuration
  + Registration
  + Uregistrered
  + Powering Down
- Trigger standstill events in the following cases:
  + While waiting for TCC response for a mode change.
  + If LCSRDY is inactive.
  + When No cabin is active and ATP mode is other than Sleeping.
  + BTM routine is in progress
  + Brake test is in progess
  + Brake test fails
  + Driver Login status is not authorized except when in Sleeping mode.
  + Train state has Idle status
  + When ATP mode changes to Split while waiting for driver confirmation.
  + When ATP mode changes to Join while waiting for driver confirmation.
  + Driver Login Status is Not Authorized
  + Idle state is set.
- When a standstill event is issued, log the event to the N-JRU, inform the Driver and send the log to TCC.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
AOS 2218   | AOS shall issue Standstill event while waiting for TCC response of mode transition. | Not Implemented
AOS 2600 S | The AOS shall while in the following ATP modes issue a Standstill event:Power up, Configuration, Registration, Unregistered, Powering Down. |Not Implemented
AOS 1695   | The AOS shall raise a Standstill event while waiting for the Driver to confirm the Split mode. | Not Implemented
AOS 1696   | The AOS shall raise a Standstill event while waiting for the Driver to confirm the Join mode. | Not implemented
AOS 1298   | If ·	No cabin is active AND ·	ATP mode is other than Sleeping issue a Standstill event. | Not implemented
AOS 2511   | The AOS shall issue a Standstill event when the BTM routine test is in progress. | Not implemented.
AOS 2612 S | The AOS shall issue a standstill event during the brake test. | Not implemented
AOS 2615 S, 2629 S | Conditions to trigger Brake test and hence, triggering of Standstill event | Not implemented
AOS 2636 S | If the brake test fails the AOS shall continue to issue a standstill event  | Not implemented
AOS 154 S  | The AOS shall issue a Standstill event while Driver Login status is Not Authorized | Not implemented
AOS 1723 S | Issue a Standstill event when the Idle status is set | Not implemented
AOS 2608   | AOS shall inform the Driver when a Standstill event is active. | Not implemented
AOS 2597   | If a Standstill event is issued:The event shall be logged to the RU, TCC shall be informed, AOS shall supervise the standstill. | Not implemented
AOS 1707   | When a Standstill event is active, the AOS shall issue a Brake event if: a movement exceeding a configured distance is detected, OR the current speed is more than a configurable value. | Not implemented
AOS 2427   | If the signal LCSRDY is inactive the AOS shall issue a Standstill event. | Not implemented

\section SystemArchitecturalDesign System Architectural Design

\subsection ChosenSystemArchitecture Chosen System Architecture

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

\subsection ExternalInterfaceDescription External Interface Description

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection Component-n Component-n

\subsection AOS2218 AOS 2218
Affected component  : message Handler \n
In the Modes: before sending out the TCC request for Mode change to Yard, Shunting or possession Mode, trigger the standstill event. This is meant to be implemented in Future.

\subsection AOS2600S AOS 2600 S
Affected component  : Mode Control \n
In the Modes: Power up, Configuration, Registration, Unregistered, Powering Down - handle Mode trigger a Standstill event.

\subsection AOS1695 AOS 1695
Affected component  : Mode Control \n
In the Split Mode: handleMode() - check for the button status from the DMI. Trigger the Standstill event in the else case of the check. This can be skipped till Split Mode is implemented.

\subsection AOS1696 AOS 1696
Affected component  : Mode Control \n
In the join Mode: handleMode() - check for the button status from the DMI. Trigger the Standstill event in the else case of the check. This can be skipped till Join Mode is implemented

\subsection AOS1298 AOS 1298
Affected component  : Mode Control \n
In the AbstractModeControl::manageCabActiveStatus() - check if no cabin is Active AND Mode is not Sleeping. Trigger the standstill event in such case.

\subsection AOS2511 AOS 2511
Affected component  : BTM Handler \n
In the AbstractBTMHandler::runBTMRoutineTestStateMachine()-switch (routineTestState) - Case  RoutineTestInProgress: Trigger the standstill event in such case.

\subsection AOS2612S AOS 2612 S
Affected component  : Brake \n
Not yet implemented. This should be implemented along with the implementation of Brake Test code.

\subsection AOS2615S2629S AOS 2615 S, 2629 S
Affected component  : Brake \n
Not yet implemented. This should be implemented along with the implementation of Brake Test code.

\subsection AOS2636S AOS 2636 S
Affected component  : Brake \n
Not yet implemented. This should be implemented along with the implementation of Brake Test code.

\subsection AOS154S AOS 154 S
Affected component  : Mode Control \n
In DriverLoginSeq::runDriverLoginVerification()-if (lStatus == DriverLogonSuccesful): Trigger the Standstill in the else case.

\subsection AOS1723S AOS 1723 S
Affected component  : Mode Control \n
In AbstractMode::manageTrainIdling() - Check if the 'trainState.isIdling' is true. If Yes, trigger Standstill event.

\subsection AOS2608 AOS 2608
Affected component  : Event Handler, DMI Handler \n
In DMI Handler, add a new DMIEventCode to send message to the DMI in order to display on DMI the "Standstill event active" icon.

\subsection AOS2597 AOS 2597
Affected component  : Event Handler, Supervise \n
In Event Handler, while creating the Event, provide the text to be displayed on the N-JRU as appropriate parameter.
Supervision should be performed as described above. Section:Standstill Supervision

\subsection AOS1707 AOS 1707
Affected component  : Supervise \n
Supervision should be performed as described above. Section:Standstill Supervision

\subsection AOS2427 AOS 2427
Affected component  : LocoIO \n
In the runIn(), get the value of the LCS Ready, if it is inactive trigger the standstill event.

\section AdditionalMaterial Additional Material

*/