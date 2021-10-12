/**
\if AsMainPage
\mainpage Registration Area and Protocol Version Changes(6.3.5 & 6.3.22.1)
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 13-04-2017 | Purposed Changes for Registration Area and Protocol Version check | adgupta


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description

\section Introduction Introduction
Below changes are purposed for Area Request, Registration Area and Protocol Version check as per the doors requirements defined in section 6.3.5 and 6.3.22.1.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
Req     | Short requirement description                                                         | Description 
------- | --------------------------------------------------------------------------------------| ------------
AOS1212 | The AOS shall request a selection of "Registration area" by the Driver if requested by TCC.  |Not Implemented
AOS2201 | When AreaRequest message is received in Yard mode, AOS shall suspend the driver request until mode has changed to Configuration. | Not Implemented
AOS1213 | The AOS shall send a "RegistrationArea" message to TCC when "RegistrationArea" is submitted by the Driver. | Not Implemented
AOS1311 | For each new TCC connection established AOS shall allow processing of AreaRequest message only and reject other messages from TCC and suspend authorization until a ProtocolVersion message matching AOS is received. | Partially Implemented
AOS1247 | When ProtocolVersion message with protocol check is received from TCC it should compare the received version with that of AOS and reply with ProtocolVersion message having compare result. | Implemented
AOS1248 | When comparision in AOS1247 matches, the AOS shall allow processing of all TCC messages and allow authorization. | Partially Implemented
AOS1249 | When ProtocolVersion message with an unrecoverable mismatch is received from TCC, the AOS shall issue a Log Event. | Not Implemented

\subsection ExternalInterfaceDescription External Interface Description
Interface to TCC shall be according to the FFFIS TCC-AOS version 5.8

\section DetailedDescriptionOfComponents Detailed Description of Components

Following are the list of affected components:
- Message Handler
- DMI handler
- Mode Control

The set of requirements to be implemented in this TDD is classified in 2 parts:
- Registraion Area request Handling
- Protocol Version Handling and Authorization

\subsection RegistrationArea Registration Area request Handling
This sub-section covers the requirements under the section 6.3.5 in the SSRS_AOS_IF150_BHPB registration Area\n

@image html registration_area.png
@image latex registration_area.png

\subsection ProtocolVersion Protocol version Handling and Authorization
This sub-section covers the requirements under the section 6.3.22.1 in the SSRS_AOS_IF150_BHPB Protocol Version\n

@image html protocol_version.png
@image latex protocol_version.png

NOTE: After this requirement implementation there will be a change in the sequence of Position Report message handling. ATP will receive the position report request 
and send position reports only after verifying the Protocol version.

\section RequirementsImplementation Requirements Implementation
\subsection AOS1212 AOS1212
Affected component  : Message Handler, DMI handler \n
- In Message Handler, create a Flag that denotes the need to send Registration Area to the DMI and also an API to get this flag value.
- In DMI Handler, create a new class according to the new interface(TBD) to send the TCC Areas from ATP to DMI.
- In the assembleDMIMessageData() of this class, add the TCC Areas into the buffer to be sent to the DMI.
- In the validate() of this class check if AreaRequest message Flag is True

\subsection AOS2201 AOS2201
Affected component  : Message Handler \n
- In Message Handler, RadioMessageInAreaRequest::validate(), check if the ATP mode is Not Yard mode: if not, after all the validation is 
successful, also set the request Area Registration Flag to True(to be used by DMI Handler).

\subsection AOS1213 AOS1213
Affected component  : DMI Handler, Message Handler \n
- In DMI Handler, create a Flag that denotes the need to send Registration Area Response to the TCC and also an API to get this flag value.
- In the RadioMessageOutRegistrationArea::validate() check if Area Request Response message Flag is True

\subsection AOS1311 AOS1311
Affected component  : Message Handler \n
- RadioMessageInProtocolVersion message create a flag and an access function that denotes if ProtocolVersion is verfied or not.
- In the validate() of all the AbstractRadioMessageIn derived classes, use this flag to validate the received message.

\subsection AOS1247 AOS1247
Affected component  : Message Handler \n
- In RadioMessageInProtocolVersion::validate() check if the received Protocol version is matching the Protocol version of the ATP.
- In RadioMessageOutPositionReport::collectdata() check if RadioMessageInProtocolVersion is validated and then only collect the data to be sent.

\subsection AOS1248 AOS1248
Affected component  : Message Handler, Radio Handler \n
- In RadioMessageInProtocolVersion::validate() update the return value of the function to be true only if Protocol version validation is passed.
- The ATP should not send the Default position report unless the protocol version negotiation is complete.
	+ In AbstractMessageHandler::getDefaultPositionReport(), add a check to return false when protocol version negotiation is not complete.
	+ AbstractRadioHandler::run(), check for the return from getDefaultPositionReport() and write the message only if the return values is true.

\subsection AOS1249 AOS1249
Affected component  : Message Handler \n
- In RadioMessageInProtocolVersion::validate() check if the received Protocol version is not matching the Protocol version of the ATP issue a log event.


\section AdditionalMaterial Additional Material
- There is a need to implement a new message to the DMI having "Areas" received form the TCC.
- There is a need to implement a new message/upgrade existing message to accept one of the "Area" by the Driver on DMI.
- DMI needs to be updated.


*/