/**
\if AsMainPage
\mainpage
\endif
\tableofcontents

\section MainPurpose Purpose
This SCDS specifies the common functionality and processes for all [ATC] (\ref compIndex) software components.
Common information such as [Abbreviations] (\ref abbreviations) and [References] (\ref References) can also be found here.

\section IntendedAudience Intended Audience
This system design specification is intended to be read by:
-   SW-Designers
-   Developers
-   Component Testers
-   Sub System Testers
-   Validators
-   Assessors
-   Verifiers

The reader of this document is assumed to be knowledgeable up to the state of the art of the
following items:
-   Software Development Skills
-   The C/C++ programming language
-   Common SW data structures & algorithms

All of these items may be used without further references to their definitions

\section HowToRead How to read this document
A reader that only wants to get an overview of the AOS_IF150 %ATP SW environment should read the chapter
"Overview" of each component. This chapter shall be consumed anyway by every reader since it might contain
additional hints on where important information is to be found.

\section References References

Ref           | Document Name                                      | Document Id      | Version
------------- | ---------------------------------------------------| ---------------- | ----------
[AOS_EN50128] | Fulfilment of EN 50128 Checklist                   | 1DOC-1013796     | *
[AOSEvents]   | AOS Event Details                                  | 1DOC-1038341     | &ndash;
[C++_CONV]    | C/C++ Programming Conventions Interflo 150 AOS     | 1DOC-1015360     | *
[DocPlan]     | Document Plan AOS IF150 BHP                        | 1DOC-1013808     | &emsp;
[FIS_JRU]     | FIS TCC JRU                                        | 3NSS011034D0138  | 1.2
[SSAS]        | AOS Subsystem Architecture Specification           | 1DOC-1019714     | 1.6
[SSRS]        | Interflo 150 AOS Requirement Specification (Core)  | 1DOC-1015370     | 1.25
[SWAS]        | %ATP Software Architecture Specification           | 1DOC-1021693     | 1.12
[VFW]         | Application Manual for VFW                         | 3NSS010519D0063  | *

&ndash; Same as the SW version <br>
&lowast; See latest [DocPlan] for respective document version

\section EN50128 EN50128 Requirements
The software component implementation shall adhere to [AOS_EN50128].

\section ProgrammingConvention C++ Programming Conventions
The software component implementation shall adhere to the programming conventions in [C++_CONV].

\section SoftwareDevelopmentEnvironment Software Development Environment
The software development environment is described in [SSAS].

\section MainCrossCompare Cross-compare
See %ATP [SWAS], chapter 10.2 "ATP Cross Compare" for information about cross compare in %ATP.

\section MainFaultReporting Fault Reporting
All errors/events are reported using a standardized %ATP error/event handling.
The list of errors/events and their severity and categories are defined in [AOSEvents].

\section MainConvertOrder Convert to network order
The VFW functions declared in vfw_buffer.h shall be used to convert to/from network byte order before an incoming message is processed and before an outgoing message is sent.

Use
- vfwInitBuffer() to initialize a buffer for use.
- vfwPut*() to write values to buffer and convert to network byte order
- vfwGet*() to read values from buffer and convert to host byte order

See [VFW] for more information.

\section MainTraceability Traceability

\subsection MainSSRS Functional Requirements
The functional requirements are defined in [SSRS].

Functional requirements applicable to all components mentioned in this SCDS are:

N/A

\subsection MainSSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Architectural requirements applicable to all components mentioned in this SCDS are:

N/A

\page compIndex Component Index
Click on individual component to read its complete SCDS.

[Abstract Analyzer IF](\ref aif) \n
[ATP Application](\ref app) \n
[Basic IP](\ref bip) \n
[Config](\ref cfg) \n
[Console](\ref console) \n
[Event Handler](\ref eh) \n
[Log Handler](\ref lh) \n


\page abbreviations Abbreviations
Abbreviation   | Definition
-------------- | ------------------------------------------------------------------
AIF            | Analyzer Interface
AOS            | %ATP Onboard System
API            | Application Program Interface
%ATC           | Automatic Train Control
ATO            | Automatic Train Operation
%ATP           | Automatic Train Protection
BDS            | Basic Diagnostic System
BHP            | Broken Hill Proprietary
BTM            | Balise Transmission Module
CAU            | Compact Antenna Unit
CBSS           | Computer Based Signaling System
COD            | Common Odometer
CoHP-2         | Common Hardware Platform version 2
CPU            | Central Processing Unit
CRC            | Cyclic Redundancy Checksum
DMI            | Driver Machine Interface
EB             | Emergency Brake
ECPB           | Electronically Controlled Pneumatic Brakes
EMD            | Electro Motive Diesel (locomotive manufacturer)
GSP-2          | Generic Safe Platform version 2
HIL            | Hardware In Loop
IP             | Internet Protocol
IO             | Input Output (I/O)
LCS            | Locomotive Control System
MA             | Movement Authority
NA             | Not applicable
N-JRU          | Non-Juridical Recording Unit
NVS            | Non-Volatile Storage
NVSH           | Non Volatile Storage Handler
OPC            | Onboard Protocol Converter
RU             | Recording Unit
SB             | Service Brake
SIL            | Software In Loop
TCC            | Train Control Centre
TCO            | Traction Cut Out
TCP            | Transmission Control Protocol
TIC            | Train Integrity Control
TIMS           | Train Integrity Management System
UDP            | User Datagram Protocol
VFW            | Vital Framework
VIOH           | Vital IO Handler

*/

