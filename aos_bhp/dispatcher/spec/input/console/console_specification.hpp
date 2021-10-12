/** 
\if AsMainPage
\mainpage Console Component Specification
@anchor console
\endif

\ifnot AsMainPage
\class Console
\endif

\section VersionLog Version Log

Version | Date       | Description                   | Signature
------- | --------   | ----------------------------- | ---------
1.0     | 2016-11-09 | Created                       | saprasad

\section Summary Executive Summary

\subsection Purpose Purpose
This document specifies the software design for the Dispatcher adaptation part for Console component. 
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
-              | -              

\subsubsection Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
ATC            | Automatic Train Control
ATP            | Automatic Train Protection
VFW            | Vital Framework
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

Console component consists of the ATC and Dispatcher adaptation part. Console component shall provide all the functionalities for 
handling of all the communication from the console to the Dispatcher and vice-versa.

\subsection GeneralFunctionality General Functionality
The dispatcher shall create the instance, initialize the prompt to be displayed on the putty or other console-client and it should offer the iterators 
to Dispatcher Base class so that it should able to call the consoleCall functionality of each component.

__Version Information__

Console component of adaptation part should implement current version information about Dispatcher.

\subsection ObjectDiagram Object Diagram
\subsection Dependencies Dependencies
The AbstractConsole class shall be inherited by the console in Dispatcher.

Dispatcher Main Application
  + Dispatcher main application component should list component vector with components' base pointers and should provide access to its iterators.
\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection BehaviouralDescription Behavioural description
\subsubsection Initialisation Initialisation
The virtual init() - function is not implemented in the Dispatcher adaptation.All the initialisation activity done in the ATC Core part of the component.
\subsubsection ModeDependentOperation Mode dependent operation
Console component is independent of ATP mode.
\subsubsection Scheduling Scheduling
The Console has one functions that must be called each execution-cycle:
\paragraph run run()
The virtual run() - function is not implemented in the adaptation.
\paragraph writeVersion writeVersion()
The class method writeVersion() shall implement the version information of Dispatcher. Dispatcher version information should consist of major 
version number, minor version number, subversion number and current build time.The Dispatcher version information should be displayed before 
the Dispatcher prompt when the putty or console-client is connected to the Dispatcher.

\paragraph getComponentIter getComponentIter()

The class method getComponentIter() shall return begining iterator from the component objects vector list(CompPtrIter).

\paragraph getComponentIterEnd getComponentIterEnd()

The class method getComponentIterEnd() shall return ending iterator from the component objects vector list(CompPtrIter).

\subsection ClassDiagram Class Diagram

@image html console_class_diagram.png
@image latex console_class_diagram.png


\subsection ExternalInterfaces External Interfaces

See chapter 2.2 "Public Member Functions"


\subsection Diagnostics Diagnostics

\subsubsection Trace Trace-commands
Not yet defined for the Console Component.

\paragraph Analyze Analyze
Not applicable

\subsection FaultReporting Fault Reporting
All errors/events shall be reported using Log handler. No error in Console component should trigger any kind of Actions
such as SB/EB activation. Console is purely a utility implementation and should only be logged without any action.

\subsection TestFeatures Test Features
No specific features added for tests.

\subsection Miscellaneous Miscellaneous functions

\subsubsection CrossCompare Cross-compare
Not applicable

\section PreProcessor Pre-Processor Directives

No pre-processor directives available for this component

\section DesignSafety Design Safety

\subsection SIR SIR Requirement tracing

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
 SIR-     | The SW Application shall only        | Memory is only allocated during init



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
12345     | 12345 | Abcdefghijklmn..             | \ref run      | run()         

*/

