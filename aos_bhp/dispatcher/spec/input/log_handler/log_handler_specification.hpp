/**
\if AsMainPage
\mainpage Log Handler Component Specification(Dispatcher)
@anchor lh
\endif

\ifnot AsMainPage
\class LogHandler
\endif

\section VersionLog Version Log

Version | Date       | Description                   | Signature
------- | --------   | ----------------------------- | ---------
1.0     | 2016-11-16 | Start                         | akushwah
2.0     | 2016-11-23 | Incorporated Review comments  | akushwah

\section Summary Executive Summary

\subsection Purpose Purpose
This document specifies the software design for the LogHandler component in Dispatcher.
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
X              | Y

\subsubsection Abbreviations Abbreviations
Abbreviation   | Definition
-------------- | ------------------------------------------------------------------
JRU            | Juridical Recording Unit
N-JRU          | Non-Juridical Recording Unit
BDS            | Basic Diagnostic System
TCC            | Train Control Center
ATP            | Automatic Train Protection
ATO            | Automatic Train Operation

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
The Log handler is the dispatcher Adaptation of the abstract Log handler class. The main purpose at
this time is to instantiate the abstract log handler for use within the Dispatcher block.

\subsection GeneralFunctionality General Functionality
The log handler component shall instantiate the abstract log handler and implement the below function:
- addHeaderToNJRUBuffer() function.


In addHeaderToNJRUBuffer() function, Dispatcher log handler formats the logs of N-JRU in order to have "Formatting Header" in form of:
- Current System Time
- TrackId
- Position with Track
- Speed
- Subsystem(DISP)
- Side(C)
- Component name<br>
The "formatting header" to N-JRU will be of the form<br>
HH:MM:SS TTTTT:+PPPPPPP SSS XXXX Y CCCCC : <LogString><br>
where<br>
HH:MM:SS = Time<br>
TTTTT = TrackId<br>
PPPPPPP = Position within Track<br>
SSS = Speed<br>
XXXX = Sub-SubSystem (DISP)<br>
Y = Side(C)<br>
CCCCC = Component name (short name)<br>
LogString = String which needs to be logged to N-JRU<br>
<br>

NOTE: TrackId, Position within Track and Speed are always blank in Dispatcher log handler.


\subsection Dependencies Dependencies

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection BehaviouralDescription Behavioural description
\subsubsection Initialization Initialization
\subsubsection ModeDependentOperation Mode dependent operation
\subsubsection Scheduling Scheduling
The Dispatcher Log Handler component is fully scheduled inside the abstract log handler and add the
required set-up.

\subsection ClassDiagram Class Diagram
The class diagram for Dispatcher log handler adaptation is as follows:
@image html log_handler_class_diagram.png
@image latex log_handler_class_diagram.png

\subsection ExternalInterfaces External Interfaces
See chapter "Public Member Functions"

\subsection Diagnostics Diagnostics
\subsubsection Trace Trace-commands
\paragraph Analyze Analyze
\subsection FaultReporting Fault Reporting
\subsection TestFeatures Test Features
\subsection Miscellaneous Miscellaneous functions
\subsubsection CrossCompare Cross-compare
The cross comparison is managed by the abstract log handler.

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component

\section DesignSafety Design Safety

\subsection SIR SIR Requirement tracing

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
SIR -      | The SW Application shall only         | Memory is only allocated during init
SIR-       | Cross-compare                         | All persistent data is registered for Cross-compare.
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
12345      | 12345 | Abcdefghijklmn..                    | run             | run()

*/

