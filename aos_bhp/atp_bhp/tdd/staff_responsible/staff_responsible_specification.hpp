/**
\if AsMainPage
\mainpage Technical Design Description for Staff Responsible
\endif

\section VersionLog Version Log
Version | Date       | Description                                            | Signature
------- | --------   | -------------------------------------------------------|---------------
1.0     | 2017-08-08 | Document creation                                      | akushwah


\section Abbreviations Abbreviation
Abbreviation   | Definition
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
DMI            | Driver Machine Interface

\section Introduction Introduction
This document describes the technical design description of Staff Responsible Mode changes in DMI Software for AOS BHP.


\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix
The below mentioned requirements need to be considered while implementing the staff Responsible mode changes in DMI.

Req       | Short requirement description                                                                                         | Implementation Status
------ -- | -------------------------------------------------------------------------------------- -------------------------------| ----------------------
AOS 1244  | The AOS shall change the ATP mode to Normal when an MA with Q_ROUTE_TYPE equal to Normal is accepted.                 | Not Implemented
AOS 1245 S| When the ATP mode changes to Staff Responsible the AOS shall request the Driver to confirm the Staff Responsible mode.| Partially Implemented
AOS 1250 S| The AOS shall allow a brake release only after the Driver has confirmed the Staff Responsible mode.                   | Partially Implemented
AOS 1830  | AOS shall evaluate a MovementAuthority message accepted from TCC, If an ApproximatePosition message was not previously accepted from TCC, or the movement authority is not valid for entering mode Staff Responsible, AOS shall enter mode Safe Brake to Stop, If an approximate position is accepted from the TCC and the movement authority is valid AOS shall enter mode Staff Responsible | Implemented
AOS 1243  | AOS shall change the ATP mode to Staff Responsible when an MA with Q_ROUTE_TYPE equal to Staff Responsible is accepted | Implemented
AOS 2270  | AOS shall change the ATP Mode to Staff Responsible if An ApproximatePosition message from TCC is accepted, AND No SafeBrakeToStop Event is active. | Not Implemented
AOS 1246  | AOS shall send to the TCC Position report information on last passed balise when ATP mode is Yard OR Staff Responsible OR Current position is Doubtful. | Not Implemented (It will be implemented in task#5316 of KB Board)


\subsection AdditionalInformation Additional Information
Based on the information in the Q_SETUP field in the TrainSetup message received from TCC in mode Configuration and the AOS information about train position,
the AOS will decide one of the following procedures for train registration:
- Reposition (Leading to mode Staff Responsible)

\subsection ExternalInterfaceDescription External Interface Description
Following external interface description will be use while implementing this TDD .
Seq No | Document Name                                      | Version
-------|----------------------------------------------------|----------
1.     | Interface Specification ATP - MMI                  | 2.4
2      | FFFIS TCC-AOS                                      | 5.8

DMI Interface spec for Staff responsible changes is already been done in IF Spec ATP-DMI.docx v2.4.

\section SystemArchitecturalDesign System Architectural Design

\subsection ChosenSystemArchitecture Chosen System Architecture
This TDD needs to be updated in:
- ATP Component: In this part of TDD, the ATP component DMI handler and Mode control will be updated. The detail description of the change is mentioned in \ref ATPChange.

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

<Describe if there are any alternatives to the chosen design.>

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection ATPChange ATP Component Change
For the Staff Responsible changes, ATP component Mode_control and DMI_Handler will be updated as mentioned below:
- Mode_control : The Function StaffResponsibleMode::handleMode() in class StaffResponsibleMode will be updated to set the ATP mode to normal mode once Q_ROUTE_TYPE as Normal is received.

- DMI_Handler : The class DMIMessageOutATPModesAndStatus will be updated to handle the processing Data 13: bit 2 for Staff responsible in the function DMIMessageOutATPModesAndStatus::collectData().    

\subsubsection ScreenImages Screen Images

\section AdditionalMaterial Additional Material

*/