/** 
\if AsMainPage
\mainpage
\endif
\tableofcontents

\section MainPurpose Purpose
This SCDS specifies the common functionality and processes for all [ATP BHP] (\ref compIndex) software components.
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
  Ref               | Document Name                                      | Document Id       | Version
------------------- | -------------------------------------------------- | ----------------- | ---------
[AOS_EN50128]       | Fulfillment of EN 50128 Checklist                  | 1DOC-1013796      | *
[AOSEvents]         | AOSEvents                                          | 1DOC-1038341      | &ndash;
[C++_CONV]          | C/C++ Programming Conventions Interflo 150 AOS BHPB| 1DOC-1015360      | *
[DocPlan]           | Document Plan AOS IF150 BHP                        | 1DOC-1013808      | &emsp;
[FFFIS-LCS]         | FFFIS AOS-LCS                                      | 3NSS015103D0164   | 2.11
[FFFIS TCC-AOS-BHP] | FFFIS TCC-AOS Interflo 150 BHPB Adaptation         | 3NSS015103D0203   | 1.29
[OBRD]              | FFFIS AOS-OBRD                                     | 3NSS015103D0234   | 1.11
[SSAS]              | AOS Subsystem Architecture Specification           | 1DOC-1019714      | 1.6
[SSAS-APP]          | AOS SSAS Appendix                                  | 1DOC-1015439      | 1.9
[SSRS]              | AOS Subsystem Requirements Specification           | 1DOC-1018599      | 1.52
[SWAS]              | %ATP Software Architecture Specification           | 1DOC-1021693      | 1.12
[VFW]               | Application Manual for VFW                         | 3NSS010519D0063   | *

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

BHP specific functional requirements applicable to all components mentioned in this SCDS are:
+ AOS_BHPB 2600
+ AOS_BHPB 2674
+ AOS_BHPB 2680
+ AOS_BHPB 3191
+ AOS_BHPB 5020

\page compIndex Component Index
Click on individual component to read its complete SCDS.

[Analyzer IF](\ref aif) \n
[ATP Application](\ref app) \n
[ATP Main](\ref atp_main) \n
[Basic IP](\ref bip) \n
[BTM Handler](\ref bh) \n
[Brake](\ref bk) \n
[Config](\ref cfg) \n
[Console](\ref console) \n
[Cross Compare](\ref cc) \n
[Decode](\ref dec) \n
[DMI Handler](\ref dh) \n
[Event Handler](\ref eh) \n
[Log Handler](\ref lh) \n
[Loco IO](\ref lio) \n
[Message Handler](\ref mh) \n
[Mode Control](\ref mc) \n
[Odometry](\ref odo) \n
[Position](\ref pos) \n
[Radio Handler](\ref rh) \n
[Supervise](\ref sup) \n
[Targets](\ref ta) \n
[Target Calculation](\ref tc) \n
[TIC](\ref ti) \n
[TIMS](\ref tm) \n
[Tracks](\ref trk) \n
[TSetup](\ref ts) \n
[Vehicle Com](\ref vc) \n


\page abbreviations Abbreviations

Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
ADS            | Automatic Driving System
AIF            | Analyzer Interface
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
CBSS           | Computer Based Signaling System
COD            | Common Odometer
CoHP-2         | Common Hardware Platform version 2
CON            | Configuration Mode
CPU            | Central Processing Unit
CRC            | Cyclic Redundancy Checksum
DMI            | Driver Machine Interface
EB             | Emergency Brake
ECPB           | Electronically Controlled Pneumatic Brakes
EMD            | Electro Motive Diesel (locomotive manufacturer)
ETCS           | European Train Control System
ERTMS          | European Rail Traffic Management System
GSP-2          | Generic Safe Platform version 2
IO             | Input Output (I/O)
IP             | Internet Protocol
JON            | Join Mode
LCS            | Locomotive Control System
LOC            | Location Mode
MA             | Movement Authority
NA             | Not Applicable
N-JRU          | Non-Juridical Recording Unit
NOR            | Normal Mode
NVS            | Non-Volatile Storage
NVSH           | Non Volatile Storage Handler
OBRD           | On-board Broken Rail Detection
OPC            | Onboard Protocol Converter
POD            | Powering Down Mode
POS            | Possession Mode
POU            | Power Up Mode
REG            | Registration Mode
RU             | Recording Unit
SB             | Service Brake
SBS            | SafeBrakeToStop Mode
SDP            | Speed and Distance Processor
SHN            | Shunting Mode
SHR            | Shunting Route Mode
SHT            | SafetyHalt Mode
SLE            | Sleeping Mode
SPT            | Split Mode
SRP            | StaffResponsible Mode
TCC            | Train Control Centre
TCO            | Traction Cut Out
TCP            | Transmission Control Protocol
TIC            | Train Integrity Control
TIMS           | Train Integrity Management System
UDP            | User Datagram Protocol
URE            | Unregistered Mode
VFW            | Vital Framework
VIOH           | Vital IO Handler
YAR            | Yard Mode



*/

