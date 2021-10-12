/**
\if AsMainPage
\mainpage Brake Application and Brake Release
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 31-01-2018 | Purposed Changes for Brake application and Brake Release | adgupta


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description

\section Introduction Introduction
Below changes are purposed for Brake Application, and Brake Release as per the doors requirements defined in section 5.3.4.2 and 5.3.4.3.

\subsection RequirementsTraceabilityMatrixApplication Requirements Traceability Matrix  Brake Application
Req     | Short requirement description                                                         | Description 
------- | --------------------------------------------------------------------------------------| ------------
AOS 2099 | The AOS shall apply emergency brake if any event requesting emergency brake application is active.  | Implemented
AOS 2100 | If configured for SB access, the AOS shall apply service brake when the emergency brake is applied. | Partially Implemented
AOS 2688 | The AOS shall set the Emergency Brake Applied output to Applied when the Emergency brake application is applied. | Partially Implemented
AOS_BHPB 2814 S | The AOS shall apply Traction Cut-off when the emergency brake is applied. | Partially Implemented
AOS 2586 | The AOS shall inhibit brake application in ATP mode Sleeping if the Sleeping signal is active. | Not Implemented
AOS 2588 | The AOS shall allow Brake application in ATP mode Sleeping when:	The Sleeping signal is inactive, AND The train is at standstill. The Brake application shall be allowed as long as the Sleeping signal is inactive. | Not Implemented


\subsection RequirementsTraceabilityMatrixRelease Requirements Traceability Matrix Brake Release
Req     | Short requirement description                                                         | Description 
------- | --------------------------------------------------------------------------------------| ------------
AOS 2071 | The AOS shall allow release of the service brake if there are no active events requesting service brake application. | Implemented
AOS 2072 S | The AOS shall allow release of the emergency brake if there are no active events requesting emergency brake application and the train is at standstill. | Implemented
AOS 2073 | The AOS shall for each service brake application calculate the authority required to release the brake based on: The events requesting the brake application AND The ATO mode switch state. | Partially Implemented
AOS 2074 | In the ATO mode switch state ‘ATO Manual’ or ‘ATO Supervised Automatic’ the AOS shall only accept brake release requests by the Driver. | Not Implemented
AOS 2075 | The AOS shall allow emergency brake release only by the Driver. | Implemented

\subsection ExternalInterfaceDescription External Interface Description
Interface to TCC shall be according to the FFFIS TCC-AOS version 5.11

\section DetailedDescriptionOfComponents Detailed Description of Components

Following are the list of affected components:
- Brake
- Event handler
- Loco IO
- DMI Handler

The set of requirements to be implemented in this TDD is classified in 2 parts:
- Brake Application
- Brake release

\subsection BrakeApplication Brake Application
This sub-section covers the requirements under the section 5.3.4.2 in the SSRS_AOS_IF150_BHPB Brake Application\n

\subsection BrakeRelease Brake Release
This sub-section covers the requirements under the section 5.3.4.3 in the SSRS_AOS_IF150_BHPB Brake Release\n

\section RequirementsImplementation Requirements Implementation
\subsection AOS2100 AOS 2100
Affected component  : Event handler \n
- In AbstractBrake::checkAndApplySBrakeReq(), Check, if the SB is available via getSbAvailable(). Check if ebApplied is true. Set sbApplied to 'true'.

\subsection AOS_BHPB2814 AOS_BHPB 2814
Affected component  : Loco IO \n
- In LocoIO::runOut(), Check if Eb applied flag in Brake component is true.
- Check, if configuration parameter to use TCO is set or not.
- If the getEbApplied() and configuration parameter to use TCO is set, set TCO digital output as true and return true.

\subsection AOS2586 AOS 2586
Affected component  : Brake, Mode control \n
- In Mode control, create an object brakeInhibit and a corresponding function to access it.
- If the current mode is Sleeping and the LocoIO sleep signal is active set this flag to true.
- In brake, checkAndApplyEBrakeReq and checkAndApplySBrakeReq before enabling 'ebApplied' and 'sbApplied' check for the access function from Mode control.

\subsection AOS2588 AOS 2588
Affected component  : Brake, Mode control \n
- In mode control, check if the sleep signal is inactive and train is at standstill: reset the brakeInhibit flag.

\subsection AOS2071 AOS 2071
Affected component  : Brake \n
- In AbstractBrake::performSBrakeReleaseControl() replace, in ((getSbApplied() && (trainStopVelocity == currentSpeed)), remove the check for stop train velocity with current speed.

\subsection AOS2073 AOS 2073
Affected component  : Brake \n
- In AbstractBrake::performSBrakeReleaseControl(), acquire the required authority to release brake, and also the current authority level. If current authority is higher than the expected authority level, allow release of brakes.

\subsection AOS2074 AOS 2074
Affected component  : Brake \n
- In AbstractBrake::performSBrakeReleaseControl(), add if the ATO mode switch state is 'ATO Manual' OR 'ATO Supervised Automatic'. update the required authority to 'Driver'.

\subsection AOS2075 AOS 2075
Affected component  : Brake \n
- In AbstractBrake::performEBrakeReleaseControl(), use only brakeReleaseReqDriverEB to compare with the required authority.

*/