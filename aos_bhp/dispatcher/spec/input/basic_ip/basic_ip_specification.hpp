/**
\if AsMainPage
\mainpage Basic IP Component Specification
@anchor bip
\endif

\ifnot AsMainPage
\class BasicIP
\endif

\section VersionLog Version Log

Version | Date       | Description                   | Signature
------- | --------   | ----------------------------- | ---------
1.0     | 18-11-2016 | Start                         | adgupta

\section Summary Executive Summary

\subsection Purpose Purpose
This document specifies the software design for the BasicIP component in Dispatcher.
Sufficient details provided for the module to be implemented.

\subsection IntendedAudience Intended Audience
This system design specification is intended to be read by:
-   SW-Designers
-   Developers
-   Component Testers
-   Sub System Testers
-   Validators
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
\subsubsection ProjectDocuments Project Documents
Ref        | Document Name                            | Document Id       | Version
---------- | ---------------------------------------- | ----------------- | ---------
[SWAS]     | ATP Software Architecture Specification  | 3NSS?             | 1.0
[SYSREQ]   | ATP (Sub)System Requirements             | DOORS             | Baseline ?
[SIR]      | Safety Integrity Requirements            | ?                 | ?

\subsection DefinitionsAndABbreviations Definitions and Abbreviations
\subsubsection Definitions Definitions
Term           | Definition
-------------- | ------------------------------------------------------------------
X              | Y

\subsubsection Abbreviations Abbreviations
Abbreviation   | Definition
-------------- | ------------------------------------------------------------------
ATP            | Automatic Train Protection
TCC            | Train Control Center
DMI            | Driver Machine Interface
AOS            | IF-150 Onboard System
VFW            | CoHP-2 Vital Framework
IP             | Internet Protocol
TCP            | Transmission Control Protocol
UDP            | User Datagram Protocol
API            | Application Program Interface

\subsection EN50128 EN50128 Requirements
ID         | Technique/Measure           | Justification
---------- | --------------------------- | -----------------------------------------------
A4.2       | Modeling                    | Class diagrams and flowcharts
A4.3       | Structured methodology      | ...
A4.4       | Modular Approach            | ...
A4.5       | Components                  | ...

\subsection Deviations Deviations and questions

\latexonly \newpage \endlatexonly
\section Overview Overview
BasicIP component deals with handling of all the communication with external interfaces viz.- DMI, TCC and AOS-PC.
It creates and maintains IP connection(TCP/UDP) and provides APIs to initialize, read and write from each connections.
The major purpose is to simplify and standardize the usage of IP-connections across AOS. While most of the functionality 
implementation is done in the Abstract part of the component, adaptation part of BasicIp instantiates the instance, 
creates the connection control blocks and define the connection Ids.

\subsection GeneralFunctionality General Functionality
Major functionalities provided by BasicIP are:-
- initialize the connection at startup. BasicIP will after that take care of the connection-handling. Currently handled in Abstract part of the Component.
- provide access to read and write from the initialized connections via.readBuf()/writeBuf() to receive/send data on the IP channels. Currently handled in Abstract part of the Component.
- (optional)get info about the connection status via getConnectionStatus(). Currently handled in Abstract part of the Component.

The adaptation of the BasicIP component:-
+ creates as many ConnectionControlBlocks as there are connections to support.
+ defines the valid connectionId(s) for the adaptation.

\subsection ObjectDiagram Object Diagram

\subsection Dependencies Dependencies
The scheduler must:
- call BasicIP::init() at initialization
- call BasicIP::run() each cycle

Each component who needs to use BasicIP must:
- call BasicIP::initConnection with the required connection Id.
- call BasicIP::readBuf(<connection Id>) to read from the connection.
- call BasicIP::writeBuf(<connection Id>) to write to the connection.
- (optional)call BasicIP::getConnectionStatus(<connection Id>) to get the connection status.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection BehaviouralDescription Behavioral description
\subsubsection Initialization Initialization
The initialization in BasicIP is done in the Abstract part of the component. The BasicIP Dispatcher adaptation does not involve in any initialization activity.

\subsubsection ModeDependentOperation Mode dependent operation
BasicIP will be Mode independent and should be scheduled in all Modes.

\subsubsection Scheduling Scheduling
The scheduling of Dispatcher BasicIP is done in the Abstract part of the component. Dispatcher Basic IP adaptation does not involve any scheduling activity.

\subsection ClassDiagram Class Diagram
@image html basic_ip_class_diagram.png
@image latex basic_ip_class_diagram.png

\subsection ExternalInterfaces External Interfaces
All the external interfaces in BasicIP is provided by the Abstract part of the component.

See chapter 2.2 "Public Member Functions"


\subsection Diagnostics Diagnostics

\subsubsection Trace Trace-commands
Trace commands are implemented in the Abstract part of the component. In case any implementation is done in the 
Adaptation run() or init() functions, it will lead to use of Trace commands in the dispatcher adaptation.

\paragraph Analyze Analyze
References to variables to be accessed by an external Analyzer tool shall be prepared at initialization.
Some of the statistics values may be of such interest. Till now no such statistics are identified.

\subsection FaultReporting Fault Reporting
All errors/events shall be reported using a standardized ATP Error/Event handling.
The list of errors/events and their severity and categories remain to be defined.

\subsection TestFeatures Test Features

\subsection Miscellaneous Miscellaneous functions

\subsubsection CrossCompare Cross-compare
As BasicIP is only used by the non-vital dispatcher environment, it is not necessary to implement any cross-compare.

\subsection CoreAdaptation Core / Adaptation

\section PreProcessor Pre-Processor Directives

No pre-processor directives available for this component

\section DesignSafety Design Safety

\subsection SIR SIR Requirement tracing

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
SIR-     | The SW Application shall only        | Memory is only allocated during init
SIR-      | Cross-compare       | All persistent data in RadioChannel is registered for Cross-compare.



\section DesignReliability Design Reliability
The failure modes of the design and their effect on operation and performance.

\section Traceability Traceability

\subsection Requirements Requirements
The applicable requirements for this component is defined in [SYSREQ] and [SIR].
However, the [SWAS] adds constraints on the design and implementation of this component.

Each applicable Safety Integrity Requirement is addressed in the Design Safety chapter.

The requirements relevant for this component:

Req        | CTT   | Short requirement description       | Chapter         | Function
---------- | ----- | ----------------------------------- | --------------- | --------------
12345     | 12345 | Abcdefghijklmn..             | \ref runIn      | runIn()

*/

