/** 
\if AsMainPage
\mainpage VIOHSim Component Specification
\endif

\ifnot AsMainPage
\class VIOHSim
\endif

\section VersionLog Version Log

Version | Date       | Description                   | Signature
------- | --------   | ----------------------------- | ---------
1.0     | 2016-05-04 | Start                         | adgupta
2.0     | 2016-07-24 | Updated SCDS for analog Signal| akushwah

\section Summary Executive Summary

\subsection Purpose Purpose
This document specifies the software design for the VIOHSim component. 
Sufficient details provided for the module to be implemented.

\subsection IntendedAudience Intended Audience
This software compoenent design specification is intended to be read by:
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
?          | AOS Simulator Interface                  | ?                 | 1.2
?          | Interface Description Vital I/O Handler  | 3NSS012264D0063   | 1.12
?          | Application Programming Interface for VFW| 3NSS010519D0020   | 1.3


\subsection DefinitionsAndABbreviations Definitions and Abbreviations
\subsubsection Definitions Definitions 
Term           | Definition                                   
-------------- | ------------------------------------------------------------------
-              | -

\subsubsection Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
VIOH           | Vital I/O Handler
VFW  		       | CoHP-2 Vital Frameworks
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

The VIOHSim component is simulating the functionality normally carried out by the Vital I/O Handler
by faking the calls to VIOH Handler client and redirecting them via vioh_client_sim to VIOHSim.
VIOHSim has a mirror simulating the vital I/O channels status. The VIOHSim is connected to 
AOS PC via VFW channel. AOS PC sends simulated inputs to the VIOHSim which in turn is used by
ATP via access functions.

\subsection GeneralFunctionality General Functionality

- Opens VFW channel to communicate with the AOS-PC.
- Stubs most of the VIOH Client functions to return a success.
- Implements important VIOH Client functions to fake simulated values via a local mirror of simulated I/O values.
- Update the mirror via AOS PC to get test/simulated values.
- Send the updated local mirror digital output values to AOS PC via the VFW channel opened to AOS-PC.

\subsection Dependencies Dependencies
- Vital framework(VFW)
  + setting up vfw-channels for sending/receiving of data to/from the AOS PC.
  + converting message data to/from network byte order.

- LocoIO
  + calls the access functions of VIOHSim to get vital I/O health state and inputs.
  + calls the access functions of VIOHSim to set output.

- AOS PC
  + sends the simulated values of the input channels.
  + receives the simulated output values from VIOHSim.
 

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection BehaviouralDescription Behavioural description
\subsubsection Initialisation Initialisation 
The init() method performs all required initalisation such as

- Open channels to be used when reading from/writing to AOS PC.
- Initializing the mirror of simulated I/O values with default values.


\subsubsection ModeDependentOperation Mode dependent operation

\subsubsection Scheduling Scheduling

The VIOHSim has two functions that must be called each execution-cycle:

\paragraph runIn runIn()
- reads packets of bytes from the vfw-channel queue (until no more data available).
- extracts the read bytes from the queue and updates the mirror.

@image html vioh_sim_run_in.png
@image latex vioh_sim_run_in.png

#####Reading from the channel
The reading of messages from the channel is done one by one and is done until the channel is empty.
While reading from the channel it should be taken care that the byte order should be converted from
network byte order to the host byte order.


\paragraph runOut runOut()
- checks if the mirror is updated with any new values.
- writes to the AOS PC via the initialized VFW channel.

#####Writing to the channel
While writing to the channel it should be taken care that the byte order should be converted from
host byte order to the network byte order.


@image html vioh_sim_run_out.png
@image latex vioh_sim_run_out.png


\subsection ClassDiagram Class Diagram

@image html vioh_sim_class_diagram.png
@image latex vioh_sim_class_diagram.png

\subsection ExternalInterfaces External Interfaces

See chapter 2.2 "Public Member Functions"
\subsubsection AccessFunctions Access-functions
The VIOHSim
- calls VIUGetState() to fake the state and health status of the VIOH digital inputs.
- calls VOUSetOutput() to fake the state and sync status of the VIOH digital output.
- calls AIOUGetState() to fake the state and health status of the VIOH analog inputs.

\subsection Diagnostics Diagnostics

\subsubsection Trace Trace-commands

The following component-specific trace-commands shall be implemented

#####Trace Level 1
Display brief information about message received/sent by VIOH Sim.

#####Trace Level 2
Display detailed information about message received/sent by VIOH Sim.

\paragraph Analyze Analyze
VIOHSim has no values to be registered for Analyzer.

\subsection FaultReporting Fault Reporting
All errors/events shall be reported using a standardized ATP Error/Event handling.
The list of errors/events and their severity and categories remain to be defined.

\subsection TestFeatures Test Features
To be defined during implementation.

\subsection Miscellaneous Miscellaneous functions
\subsubsection ConvertOrder Convert to network order
The Vfw-functions declared in vfw_buffer.h shall be used to convert to/from network-byte order before an incoming message is processed and before an outgoing message is sent.

Use
- vfwInitBuffer() to initialise a buffer for use.
- vfwPut*() to write values to buffer and convert to network byte order
- vfwGet*() to read values from buffer and convert to host byte order


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
Not required as there is no requirement available for a Simulation component.

*/

