/**
\if AsMainPage
\mainpage Message Handler update according to FFFIS ver5.5 
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-02-27 | First version                                 | akushwah


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
TCC            | Train Control Center


\section Introduction Introduction

\subsection Design Design Overview

This purpose of this TDD is to explains the different updates required for the message handler component according to FFFIS AOS TCC Ver5.5, which should be compatible with current project status. 

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

The below requirement ID should be taken care while implementing the Message Handler Changes.
AOS 1738,AOS 1735,AOS 1741,AOS 1674,AOS 1085,AOS 52,AOS 1203,AOS 1705,AOS 34,AOS 2056,AOS 40,AOS 1822,AOS 1823,AOS 1830,AOS 1832,AOS 1212,AOS 1213,AOS 769,AOS 144,AOS 149,AOS 150,
AOS 145,AOS 148,AOS 757,AOS 1246,AOS 1157,AOS 173,AOS 1156,AOS 175,AOS 2081,AOS 158,AOS 2031,AOS 2032,AOS 2033,AOS 2034,AOS 2035,AOS 2036,AOS 2037,AOS 2038,AOS 2039,AOS 2040,AOS 2041.


\section SystemArchitecturalDesign System Architectural Design

\subsection ChosenSystemArchitecture Chosen System Architecture

The updates required in Message Handler should be done in 3 phases
1. create the new/Not implemented Messages - This will include all the message which are newly introduced or not yet implemented according to FFFIS. 
2. Minor Updates in implemented messages - This will include all the message which all are changes nominally in the latest FFFIS AOS TCC between ver5.0 and Ver5.5.
Generally the function parse message() for the message falling under this category will be impacted as there is a inclusion of END_OF_MESSAGE for these message only. 
3. Major Updates in implemented messages - This will include all the message which all are changes drastically in the latest FFFIS AOS TCC between ver5.0 and Ver5.5.
The message who are not falling in Category 1 and 2 will fall under this and there will be a lot of update required for this. 

Note: Its not necessary to follow the sequence of update mentioned above and It can be done in any order.


\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

<Describe if there are any alternatives to the chosen design.>

\subsection ExternalInterfaceDescription External Interface Description

These changes will have a impact on the TCCSim updates. TCCSim need to be updated according to the FFFIS AOS TCC Ver5.5 and corresponding XMl file(Message, Block & Field) needs to be updated.
The update required is already been done in TCCSim. Only when implementation of these changed messages are done, the new XML files needs to be replaced with the current one and it should work properly.

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection ImplementationofMessage Implementation of Message(New/previously not implemented)

The below message will be created along with their functionality. Hence, the below message will be there in project along with their functionality published for other component to use.
but will not affect current functionality of project.
- ApproximatePosition
- AreaRequest
- ATORemoteControl
- ConfigurationData
- ExternalData
- JoinCommand
- Path
- PossessionAcknowledge
- RejectConfiguration
- ShuntingAcknowledge
- YardAcknowledge
- ProtocolVersion

\subsection MinorImplementationofMessage Minor Updates in implemented messages
The below message needs to be updated according to in FFFIS spec. It need to extract/add "M_END_OF_MESSAGE" while parsing the data depending the incoming or outgoing message.
- EmergencyAlert
- RevokeEmergencyAlert
- StopTrain
- Unregistration
- AbortSetup
- DriverInformation
- MessageAcknowledge
- RegistrationArea
- TrainRegistrationInformation


\subsection MajorImplementationofMessage Major Updates in implemented messages
The below message needs to be updated according to in FFFIS spec.
- CommandMessage 
  + Remove the REQ_CAR_STATUS block 
  + Remove the LOCOMOTIVE_POWER parameter 
  + Add the SAFE_FOR_BOARDING_ACTIVATE parameter

  Component needs to update: Message Handler

- MovementAuthority
  + Remove the parameter A_BRAKEABILITY 
  + Add the parameter M_LOADED
  + Add N_ADHESION parameter
  + Add NID_TRACK (Start of MA position, Track)
  + Add D_POSITION (Start of MA position, Distance)
  + Remove the parameter D_SAFETY_MARGIN 
  + Add the parameter D_OVERLAP
  + Add DEPARTURE_WARNING parameter
  + Remove PANTO_START_POSITION
  + Remove CAR_UNLOAD_DATA
  + Remove PANTOGRAPH_SHIFT

  Component needs to update: Message Handler, Position, Tsetup

- PositionReportRequest
  + Remove the parameter PROTOCOL_VERSION
  + Add the parameter CONFIRM_CONFIG 

  Component needs to update: Message Handler, Mode control

- TrainSetup
  + Remove the parameter NID_VEHICLE_TYPE 
  + Add the parameter NID_MSG
  + Add Q_TS_STATE parameter
  + Remove T_BRAKE_RESPONSE parameter
  + Remove A_BRAKEABILITY
  + Add M_BRAKE_SYSTEM
  + Remove VEHICLE_ID_DATA, CAR1_CONFIG_DATA, CAR2_CONFIG_DATA and CAR3_CONFIG_DATA
  + Add VEHICLE_TYPE_DATA, VEHICLE_ID_DATA and VEHICLE_LIST_DATA

  Component needs to update: Message Handler, Tsetup, Brake

- PositionReport
  + Remove the parameter B_TRAIN_STATUS 
  + Add the parameter B_TRAIN_CORE_STATUS
  + Remove PROTOCOL_VERSION parameter
  + Add M_BRAKE_SYSTEM parameter
  + Add D_BRAKE_DISTANCE parameter
  + Add YARD_REQUEST block
  + Add TRAIN_NAME
  + Remove ERROR_MESSAGE_DATA block
  + Remove CAR_STATUS_DATA block

  Component needs to update: Message Handler, Brake, TSetup, Supervise
  
- StartUpMessage
  + Remove the parameter B_TRAIN_STATUS 
  + Add the parameter B_TRAIN_CORE_STATUS
  + Add M_BRAKE_SYSTEM parameter
  + Add CONFIG_CONFIRMATION block
  + Add VEHICLE_LIST_DATA

  Component needs to update: Message Handler, Mode Control, Brake, Tsetup


\section UserInterfaceDesign User Interface Design

\subsection DescriptionOfTheUserInterface Description of the User Interface

<If applicable, describe the design/re-design of the UI.>

\subsubsection ScreenImages Screen Images

<If applicable, include screen-shoots or other images to better describe the UI.>

\subsubsection ObjectsAndActions Objects and Actions

<If applicable, describe the actions trigged by certain UI events.>

\section AdditionalMaterial Additional Material

<Anything not covered in other sections can be added here.>

*/