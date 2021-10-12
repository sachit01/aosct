/** 
\if AsMainPage
\mainpage DispMain(Dispatcher) Component Specification
@anchor main
\endif

\ifnot AsMainPage
\class Template
\endif

\section VersionLog Version Log

Version | Date       | Description                    | Signature
------- | --------   | -------------------------------| ---------
1.0     | 2016-12-12 | created and updated the SCDS   | spandita
1.1     | 2016-01-03 | Updated the SCDS with preinit()| spandita

\section Summary Executive Summary

\subsection Purpose Purpose
This document specifies the software design for the DispMain component. 
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

The reader of this document is assumed to be knowledgeable up to the state of the art of the 
following items:
-   Software Development Skills
-   The C/C++ programming language
-   Common SW data structures & algorithms

All of these items may be used without further references to their definitions

\subsection HowToRead How to read this document
For a reader who just wants to get a first overview of the AOS_IF150 ATP SW environment it should 
be enough to read the chapter "Overview". This chapter shall be consumed anyway by every reader 
since it might contain additional hints on where important information is to be found.
 
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
x              | y

\subsubsection Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
VFW            |  CoHP-2 Vital Framework
VIOH           | Vital Input Output Handler
GSP2            | Generic Safety Platform2
ATP            | Automatic Train Protection
CPU            | Central Processing Unit
NVSH           | Non-Volatile Storage Handling

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
The DispMain component is responsible to setup the GSP2 platform and interface with the
DispApplication and other components.

\subsection GeneralFunctionality General Functionality
The component is the main executable entry point for the Dispatcher application. The component sets up the
platform specific components from GSP2 like VFW and other interfaces to provide the environment for the 
DispApplication to run. <br>

\subsection Dependencies Dependencies
The component is dependent on:
 - DispApplication to create, initialize and execute the dispatcher application.
 - GSP2 platform interfaces to initialize and interface: VFW

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection main main
The main function is the starting point of the dispatcher software. It is responsible for setting up the 
environment. The main function should never exit and hence it ends with an infinite while loop.
The flow diagram for the main function is shown below:

@image html disp_main_main.png
@image latex disp_main_main.png

\subsection mainLoop mainLoop
The mainLoop is the function which is called by the VFW upon the expiry of the sync timer. 
This function is responsible for calling the interface functions of the Dispatcher application component.

The mainLoop should kick the watch dog timer. This means reset the VFW software watchdog using the vfwWatchdogKick function.
If we do not "kick" the watchdog again within "watchdogTimeout" milliseconds a SIGALRM will be sent towards our application.

The mainLoop should reactivate the cyclic sync timer using vfwSyncTimerReactivateCyclic() function.
This must be done within the callback, since if the timer is neither restarted or stopped within the callback
it will be automatically stopped after the callback has returned.

The mainLoop should also measure performance parameters like execution time, network and memory usage.

The flow diagram for the function is as follows:

@image html disp_main_mainLoop.png
@image latex disp_main_mainLoop.png

\subsection BehaviouralDescription Behavioral description
\subsubsection Initialisation Initialisation 
The init function in the component is responsible to initialize the system environment before the 
main loop can start executing.
The init component should perform the following actions:
- Set up the Vital framework
- Setup the cyclic sync timer and the callbacks for it
- Call dispatcher application add components
- Call preInit() of dispatcher application
- Set up the handler to be called at vfwHalt()
- Set up the signal handler 
- call vfwStart() 
- Calculate the watchdog timeout which is some percentage (maybe 20%) higher than the cycle time. This ensures that
if the system gets stuck in an infinite loop, the watchdog will timeout and system will go to a safe state.

The flow diagram below details the initialization procedure:

@image html disp_main_init.png
@image latex disp_main_init.png

\subsubsection ModeDependentOperation Mode dependent operation
Not applicable.

\subsubsection Scheduling Scheduling
The DispMain shall setup the VFW to cyclically trigger execution of the main dispatcher cycle. The VFW event 
shall be forwarded to the DispApplication to execute one "Dispatcher cycle".

\subsection ClassDiagram Class Diagram
The Dispatcher Main component is not a class. It is an standalone C++ file.
The diagram below shows the DispMain with the dependency on Dispatcher Application.

@image html disp_main_classDiagram.png
@image latex disp_main_classDiagram.png


\subsection ExternalInterfaces External Interfaces
See chapter "Public Member Functions"

\subsection Diagnostics Diagnostics

\subsubsection Trace Trace-commands
Not yet defined if to support or not.

\paragraph Analyze Analyze
Not supported.

\subsection FaultReporting Fault Reporting
Any faults detected in the DispMain are to be handled outside the normal event processing defined for
the Dispatcher. The faults shall be logged in the N-JRU and if possible sent to the console prior to execution
termination.

\subsection TestFeatures Test Features
Not yet defined if to support or not.

\subsection Miscellaneous Miscellaneous functions

\subsubsection CrossCompare Cross-compare
To be decided, no internal data is currently planned for cross comparison.

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component

\section DesignSafety Design Safety

\subsection SIR SIR Requirement tracing  

Req        | Short requirement description         | Justification \n
---------- | ------------------------------------- | ---------------------------------------
 

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
12345      | 12345 | Abcdefghijklmn..                    | x               | y

*/
