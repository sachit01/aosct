/**
\if AsMainPage
\mainpage
\endif
\tableofcontents

\section MainPurpose Purpose
This SCDS specifies the common functionality and processes for all [ATP Core] (\ref compIndex) software components.
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

Ref             | Document Name                                                      | Document Id      | Version
--------------- | ------------------------------------------------------------------ | ---------------- | -------
[AOS_EN50128]   | Fulfilment of EN 50128 Checklist                                   | 1DOC-1013796     | *
[AOSEvents]     | AOSEvents                                                          | 1DOC-1038341     | &ndash;
[C++_CONV]      | C/C++ Programming Conventions Interflo 150 AOS                     | 1DOC-1015360     | *
[DocPlan]       | Document Plan AOS IF150 BHP                                        | 1DOC-1013808     | &emsp;
[ERTMS]         | ERTMS/ETCS System Requirements Specification SUBSET-026            |                  | 2.3.0
[FFFIS TCC-AOS] | Communication protocol between TCC and %ATP                        | 3NSS004300D0107  | 5.41
[IFS_DMI]       | FIS %ATP - DMI Interflo 150 AOS_BHP                                | 1DOC-1019712     | 2.47
[IFS_OPC]       | ETC, OPC and VAP Interface Specification                           | 1DOC-1002321     | 1.19
[IFS_COD]       | Generic Safe Platform 2 Common Odometer IF                         | 3NSS012264D0033  | 6.13
[ITF_SPL]       | Profibus Safety Layer Generic Application Interface                | 3NGM005003       | 53
[INST_SPL]      | SPL Application Instructions                                       | 3NGM005133       | 3.2
[TSF_SPL]       | Safe Profibus Layer Library over Ethernet, Technical Safety Report | 3NGM005624       | 2.3
[REF_TIME]      | RefTimeSyncServer Software Architecture and Design Specification   | 1DOC-1013929     | 2.1
[SSAS]          | AOS Subsystem Architecture Specification                           | 1DOC-1019714     | 1.7
[SSAS-APP]      | AOS SSAS Appendix                                                  | 1DOC-1015439     | 1.9
[SSRS]          | Interflo 150 AOS Requirement Specification (Core)                  | 1DOC-1015370     | 1.25
[SWAS]          | %ATP Software Architecture Specification (Project specific)        | 1DOC-1021693     | 1.12
[TCC-AOS-SEQ]   | TCC-AOS Sequencing Interflo 150                                    | 3NSS013969D0136  | 1.21
[VFW]           | Application Manual for VFW                                         | 3NSS010519D0063  | *

&ndash; Same as the SW version <br>
&lowast; See latest [DocPlan] according to PVI for respective document version

\section EN50128 EN50128 Requirements
The software component implementation shall adhere to [AOS_EN50128].

\section ProgrammingConvention C++ Programming Conventions
The software component implementation shall adhere to the programming conventions in [C++_CONV].

\section SoftwareDevelopmentEnvironment Software Development Environment
The software development environment is described in [SSAS].

\section MainCrossCompare Cross-compare
See %ATP [SWAS], chapter 10.2 "ATP Cross Compare" for information about cross compare in %ATP.

\section CommonDependencies Common Dependencies
Most of the components have dependencies to these components:
- Config: To retrieve configuration parameters.
- Cross Compare: To implement cross comparison.
- Event Handler: To report events.
- TSetup: To retrieve information about train setup, vehicles and brake systems.

\section FaultReporting Fault Reporting
All errors/events are reported using a standardized %ATP error/event handling.
The list of errors/events and their severity and categories are defined in the [AOSEvents].

\section ConvertOrder Convert to network order
The VFW functions declared in vfw_buffer.h shall be used to convert to/from network byte order before an incoming message is processed and before an outgoing message is sent.

Use
- vfwInitBuffer() to initialize a buffer for use.
- vfwPut*() to write values to buffer and convert to network byte order
- vfwGet*() to read values from buffer and convert to host byte order

See [VFW] for more information.

\section MainTraceability Traceability

\subsection MainSSRS Functional Requirements
The functional requirements are defined in [SSRS].

Core functional requirements applicable to all components mentioned in this SCDS are:
+ AOS 1037
+ AOS 1040
+ AOS 1799 S
+ AOS 1809 S
+ AOS 1811 S
+ AOS 1989 S
+ AOS 2339 S
+ AOS 2291
+ AOS 2292
+ AOS 2527
+ AOS 2528
+ AOS 2530
+ AOS 2531
+ AOS 2985
+ AOS 3131
+ AOS 3135 S
+ AOS 3148

\subsection MainSSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Core architectural requirements applicable to all components mentioned in this SCDS are:
+ AOS_AS-38 S
+ AOS_AS-41 S
+ AOS_AS-42 S
+ AOS_AS-44 S
+ AOS_AS-51 S
+ AOS_AS-66 S
+ AOS_AS-67 S
+ AOS_AS-68 S
+ AOS_AS-69 S
+ AOS_AS-73 S
+ AOS_AS-76 S
+ AOS_AS-83 S
+ AOS_AS-93 S
+ AOS_AS-95 S
+ AOS_AS-98 S
+ AOS_AS-100 S
+ AOS_AS-103 S
+ AOS_AS-129 S
+ AOS_AS-155 S
+ AOS_AS-167 S
+ AOS_AS-349 S
+ AOS_AS-758 S


\page compIndex Component Index
Click on individual component to read its complete SCDS.

[ATP Application](\ref app) \n
[Brake](\ref bk) \n
[BTM Handler](\ref bh) \n
[Config](\ref cfg) \n
[Cross Compare](\ref cc) \n
[Decode](\ref dec) \n
[DMI Channel](\ref dc) \n
[DMI Handler](\ref dh) \n
[Loco IO](\ref lio) \n
[Message Handler](\ref mh) \n
[Mode Control](\ref mc) \n
[Odometry](\ref odo) \n
[Position](\ref pos) \n
[Radio Channel](\ref rc) \n
[Radio Handler](\ref rh) \n
[Supervise](\ref sup) \n
[Target Calculation](\ref tc) \n
[TIC](\ref ti) \n
[TIMS](\ref tm) \n
[Targets](\ref ta) \n
[Tracks](\ref trk) \n
[TSetup](\ref ts) \n
[Vehicle Com](\ref vc) \n

\page abbreviations Abbreviations
Supervision and Target Calculation related abbreviations can be found here: \ref supAbbreviations.

Abbreviation   | Definition
-------------- | ----------
AIF            | Analyzer Interface
AIOU           | Analog Input Output Unit
AOS            | %ATP Onboard System
API            | Application Program Interface
%ATC           | Automatic Train Control
ATO            | Automatic Train Operation
%ATP           | Automatic Train Protection
BAL            | BaliseSearch Mode
BDS            | Basic Diagnostic System
BHP            | Broken Hill Proprietary
BTM            | Balise Transmission Module
CAU            | Compact Antenna Unit
CPU            | Central Processing Unit
CBSS           | Computer Based Signaling System
COD            | Common Odometer
CoHP-2         | Common Hardware Platform version 2
CON            | Configuration Mode
CRC            | Cyclic Redundancy Checksum
DMI            | Driver Machine Interface
EB             | Emergency Brake
ECPB           | Electronically Controlled Pneumatic Brakes
EMD            | Electro Motive Diesel (locomotive manufacturer)
ETCS           | European Train Control System
ETH            | Ethernet
ERTMS          | European Rail Traffic Management System
GSP-2          | Generic Safe Platform version 2
GPIO           | General Purpose Input Output
IO             | Input Output (I/O)
IP             | Internet Protocol
JON            | Join Mode
LCS            | Locomotive Control System
LOC            | Location Mode
MA             | Movement Authority
MVB            | Multifunction Vehicle Bus
NA             | Not Applicable
N-JRU          | Non-Juridical Recording Unit
NOR            | Normal Mode
NVS            | Non-Volatile Storage
NVSH           | Non Volatile Storage Handler
OPC            | Onboard Protocol Converter
POD            | Powering Down Mode
POS            | Possession Mode
POU            | Power Up Mode
REG            | Registration Mode
RU             | Recording Unit
SAP            | Service Access Point
SB             | Service Brake
SBS            | SafeBrakeToStop Mode
SHN            | Shunting Mode
SHR            | Shunting Route Mode
SHT            | SafetyHalt Mode
SLE            | Sleeping Mode
SPL            | Safe Profibus Library
SPT            | Split Mode
SRP            | StaffResponsible Mode
SVD            | Synchronized Driver Unit
TBSW           | Tigris Base Software
TCC            | Train Control Centre
TCO            | Traction Cut Out
TCP            | Transmission Control Protocol
TIC            | Train Integrity Control
TIMS           | Train Integrity Management System
UDP            | User Datagram Protocol
URE            | Unregistered Mode
VFW            | Vital Framework
VIO            | Vital Input Output
VIOH           | Vital IO Handler
VOU            | Vital Output Unit
YAR            | Yard Mode


*/
