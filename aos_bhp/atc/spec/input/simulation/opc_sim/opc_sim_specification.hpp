/** 
\if AsMainPage
\mainpage OPCSim Component Specification
\endif

\ifnot AsMainPage
\class OPCSim
\endif

\section VersionLog Version Log

Version | Date       | Description                   | Signature
------- | --------   | ----------------------------- | ---------
1.0     | 2016-09-06 |  initial Draft                | spandita
1.1     | 2016-09-12 | Upated with review comments   | spandita
1.2     | 2016-09-23 | Updated as per the Code Changes| spandita

\section Summary Executive Summary

\subsection Purpose Purpose
This document specifies the software design for the OPCSim component. 
Sufficient details provided for the module to be implemented.

\subsection IntendedAudience Intended Audience
This software component design specification is intended to be read by:
-   SW-Designers 
-   Developers
-   Component Testers
-   Sub System Testers
-   Valida-tors
-   Assessors
-   Verifiers

The reader of this document is assumed to be knowledgeable up to the state of the art of the following items:
-   Software Development Skills
-   The C/C++ programming language
-   Common SW data structures & algorithms

All of these items may be used without further references to their definitions

\subsection HowToRead How to read this document
For a reader who just wants to get a first overview of the AOS_IF150 ATP SW environment it should be enough to read the chapter "Overview".
This chapter shall be consumed anyway by every reader since it might contain additional hints on where important information is to be found.
 
\subsection References References
\subsubsection Project Documents Project  
Ref        | Document Name                            | Document Id       | Version
---------- | ---------------------------------------- | ----------------- | ---------
[SWAS]     | ATP Software Architecture Specification  | 3NSS?             | 1.0
?          | AOS Simulator Interface                  | ?                 | 1.2
?          | OPC Agent Interface                      | 1DOC-1015422      | 1.1
?          | Interface ‘B’ Specification              | 3NSS010889D0108   | 2.0
[SYSREQ]   | EEIG ERTMS USERS GROUP                   | Subset-026-7 V340 |3.4.0


\subsection DefinitionsAndABbreviations Definitions and Abbreviations
\subsubsection Definitions Definitions 
Term           | Definition                                   
-------------- | ------------------------------------------------------------------
Message        | Data organised in a predefined specific order
Telegram       | A message which has information added to it in order to be able to send or receive data over a communications link

\subsubsection Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
OPC            | OnBoard Protocol Converter
VFW  		   | CoHP-2 Vital Framework
AOS            | ATP Onboard System


\subsection EN50128 EN50128 Requirements
ID         | Technique/Measure           | Justification
---------- | --------------------------- | -----------------------------------------------
A4.2       | Modelling                   | Class diagrams and flowcharts
A4.3       | Structured methodology      | ...
A4.4       | Modular Approach            | ...
A4.5       | Components                  | ...

\subsection Deviations Deviations and questions

\latexonly \newpage \endlatexonly
\section Overview Overview
Basic purpose of OPC sim to simulate all the functionalities which is provided by OPC/OPC agent 
- Provide the simulated balise passage and sends the information to ATP. 
- Respond with status message in regards of Command from ATP

\n Note: Handling of balise telegram using real site data has not been covered/postponed in Prel SCDS.

\subsection GeneralFunctionality General Functionality

OPC sim replaces the OPC agent in the SIL and HIL test environment and provide the following functionality:
- Receive  “BTM Command”/"BTM Odo Command"-telegrams from the ATP Component BTM Handler via “ATP_To_OPCAgent_A/B” channel.
- Send simulated „BTM Status” telegrams to the BTM Handler on every cycle via “OPCAgent_To_ATP_A/B” channel.
- Send simulated „BTM” telegrams to the BTM Handler when a simulated balise detection occurs via “OPCAgent_BTMTelegram_To_ATP_A/B” channel.

\subsection Dependencies Dependencies
- Btm Handler Component to receive the simulated balise passage from OPC component.

 
\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection BehaviouralDescription Behavioural description
\subsubsection Initialisation Initialisation 

The init() method performs all required initialisation. 
\n init() will be called at startup and provides the following functionalities
- Opens the channels for reading BTM Command as well as for BTM odo command message sent by ATP BTM handler.
- Opens the channels for writing BTM status,BTM telegram and MVB time reference message to ATP .
- Initialize the simulated status message with init values as follows

\paragraph  Status Status Message with init values
Variable                    | Value
----------------------------|------------------------------------------------------
Enabled Telegram Format 	|3
Safety mode             	|0
unused                      |0
Euroloop Ser-vice Available |0
Balise Service Available    |0
BSA Counter                 |0
Preliminary Availability Test Status|0 
IF K Active                  |0
Used IF K BTM ID             |0
Tele-powering Status         |0
Status and Op-tions          |0
Used Protocol Version        |8
Used Euroloop code           |0
unused                       |0

- Intialize the simulated Telegram message with init values as follows 

\paragraph  BTM BTM telegram Message with init values

Variable                    | Value
----------------------------|------------------------------------------------------
Sequence number         	|0
Class                   	|4
Begin time stamp            |0	
End time stamp              |0
Telegram time stamp         |0
Balise number               |0
Number of ASK frames received|0 
Number of ASK frames with valid CRC   |0
Final report                 |0
UNUSED                       |0
Balise information           |0

- Intialize all member of class to init value
 
\subsubsection ModeDependentOperation Mode dependent operation

\subsubsection Scheduling Scheduling

The OPCSim has following functions that must be called in each execution-cycle:

\paragraph runIn runIn()
- Read the packets from VFW channel "ATP_To_OPCAgent_A/B" until no more data available and valid .
- Check if the message type is of "BTM command" type
   \n -Store/Extract the optionandatatus field 
   \n -Store/extract the tele powering field 
- Otherwise if the message type is of "BTM Odo Command" 
  \n - Recieve it and dont do any operation.


@image html opcsim_runin.png
@image latex opcsim_runin.png

\paragraph verifyBalisePassage verifyBalisePassage()

- Read the current antenna Odo
- check if ATP Mode is equal to "Balise Search Mode" and submode is equal to "baliseSearchWaitForBalise"
 	- Set the variable nextExpectedBaliseID to predefined Balise Id 
	- Set the Variable nextExpectedBaliseOdo to predefined balise Odo
 	- Update the odoAtStartDistance and odoAtEndDistance 
	\n-If travel direction is forward
		\n odoAtStartDistance = odoatBalise - baliseDetectionRange 
		\n odoAtEndDistance  = odoatBalise + baliseDetectionRange
	\n-If travel direction is reverse
		\n odoAtStartDistance = odoatBalise + baliseDetectionRange 
		\n odoAtEndDistance  = odoatBalise - baliseDetectionRange
- Otherwise call getNextBaliseToDetect()
- Check if current odo is greater and equal to Odo value at start of balise detection range
   \n -Set the start timestamp of "BTM telegram Message"
	\n-Set the "startOdoFlag"
- Check if the current Odo is greater and equal to Odo value at end of balise detection range
   \n  -Set the end timestamp of "BTM telegram Message"
	\n -Set the "stopOdoFlag"
- Upon setting up the fields of start and end timestamp update the sequence/balise number and balise information of "BTM telegram Message" 
- and reset the flags for start and end time stamp 

@image html opcsim_verifybalise.png
@image latex opcsim_verifybalise.png

\paragraph getNextBalisetoDetect getNextBalisetoDetect()
- Search the balise in baliseList as per the travel direction 
- Save the found balise in nextExpectedBaliseId 
- Check if it is equal to previous expected balise 
- If not - update the BaliseInfo structure with Odo at limits of  balise detection range refer section 1.4.1.3.2
- Update the previousExpectedBalise.
- In case previous expected balise is same as next expected balise then log the trace with message.

@image html opcsim_getNextBaliseToDetect.png
@image latex opcsim_getNextBaliseToDetect.png

\paragraph runOut runOut()
- call verifyBalisePassage() function
- Call Pack() method
- update the telegram timestamp for "BTM telegram" message 
- send the MVB time Reference Via VFW channel "OPCAgent_To_ATP_A/B"
- send the BTM status message via VFW Channel "OPCAgent_To_ATP_A/B"
- send the BTM telegram message vis VFW Channel "OPCAgent_BTMTelegram_To_ATP_A/B"
- Reset the Bit 6 of Status and Option field in Status message.

\paragraph Pack Pack()
- Output data will be packed as per the structure defined in OPC Agent Interface document.

@image html opcsim_runout.png
@image latex opcsim_runout.png


\subsection ClassDiagram Class Diagram

@image html opcsimdiagram.png
@image latex opcsimdiagram.png

\subsection ExternalInterfaces External Interfaces

\subsection Diagnostics Diagnostics

\subsubsection Trace Trace-commands

The following component-specific trace-commands shall be implemented

#####Trace Level 1
Display brief information about message received/sent by OPC Sim.

#####Trace Level 2
Display detailed information about message received/sent by OPC Sim.

\paragraph Analyze Analyze
OPCSim has no values to be registered for Analyzer.

\subsection FaultReporting Fault Reporting
All errors/events shall be reported using a standardized ATP Error/Event handling.
The list of errors/events and their severity and categories remain to be defined.

\subsection TestFeatures Test Features
To be defined during implementation.

\subsubsection CrossCompare Cross-compare
Cross-compare is not applicable for this component

\subsection CoreAdaptation Core / Adaptation
No adaptation

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component

\section DesignSafety Design Safety

\subsection SIR SIR Requirement tracing
Not required as this is a Simulation component.

\section DesignReliability Design Reliability
Not required as this is a Simulation component.

\section Traceability Traceability

\subsection Requirements Requirements
Not required as there is no requiremnet available for a Simulation component.

*/

