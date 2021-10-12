/**
\if AsMainPage
\mainpage Location Mode(section (5.1.10))
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-07-06 | Proposed Changes for location mode requirements | spandita


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
MA             | Movement Authority
DMI            | Driver Machine Interface
TODO           | To be discussed

\section Introduction Introduction
Below changes are proposed as per the doors requirements defined in section 5.1.10 for location mode.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
Req     | Short requirement description                                                         | Description 
------- | --------------------------------------------------------------------------------------| ------------
AOS 1008 | The AOS shall change the ATP mode to Location when the train footprint is within the Location End and Location Start target positions. | Not Implemented
AOS 2415 | The AOS shall change ATP mode to Normal if there is no target of type Location End OR Location Start. | Not Implemented
AOS 120 S | The AOS shall issue a Brake Event if the train front OR rear position is outside the location boundary. | Not Implemented
AOS 122 | The AOS shall offer the Driver the possibility to request Handling Done when the ATO mode is ATO Manual and train is at standstill. | Partly Implemented


 \subsection ExternalInterfaceDescription External Interface Description

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection AOS1008 AOS1008
Affected component  : Mode Control \n
Affected Function : run() of  AbstractModeControl class \n
- Create new function named as isLocationMode() in run() function of abstract mode control.
- In the isLocationMode() function get the front and rear end of train via position component API's.
- Get the location end and start position by iterating the target list .
- Check if the front and rear end of train is in between the location end and start position.
- If yes, Change the mode to location mode.


\subsection  AOS2415 AOS2415
Affected components  : Mode Control  \n
Affected Function : handleMode() of LocationMode class. \n
- In locationFinishOk submode, Check if there is any target of type location End or location start present in target list.
- If not, Set the mode to Normal Mode via setNextMode() function call.


\subsection  AOS120 AOS120
Affected component  : Mode Control \n
Affected Function : run() of  AbstractModeControl class \n
- Create new function named as isLocationMode() in run() function of abstract mode control.
- In the isLocationMode() function get the front and rear end of train via position component API's.
- Get the location end and start position by iterating the target list .
- Check if the front and rear end of train is in between the location end and start position.
- If not, Raise the SB brake event.



\subsection  AOS122 AOS122
Affected  component  : DMI Handler \n
Affected Function : collectData() of DMIMessageOutATPModesAndStatus class .
- Need to set Data 14 field of atp mode and status message for Handling done status in location mode.
- Check if train is standstill and ATO mode is manual.
- Standstill status will be received via isTrainStandStill() function of odometry.
- If yes, set the data 14 field with 4 (will be confirmed after discussion with BO).

\
\section AdditionalMaterial Additional Material
- Need to update the DMI for data 14 field in ATP modes and status message for handling done info refer \ref AOS122 .
*/