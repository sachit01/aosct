/**
\if AsMainPage
\mainpage DispApplication:  Dispatcher Adaptation Component Specification
@anchor app
\endif

\ifnot AsMainPage
\class Template
\endif

\section VersionLog Version Log

Version | Date       | Description                   | Signature
------- | --------   | ----------------------------- | ---------
1.0     | 2016-11-09 | Start                         | nsyed
1.1     | 2017-01-04 | updated with preInit()        | spandita

\section Summary Executive Summary

\subsection Purpose Purpose
This document specifies the software design for the DispApplication adaptation component.
Sufficient details are provided for the module to be implemented.

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
ATC            | Automatic Train Control
VFW            | CoHP-2 Vital Framework

\subsection EN50128 EN50128 Requirements
ID         | Technique/Measure           | Justification
---------- | --------------------------- | -----------------------------------------------
A4.2       | Modelling                    | Class diagrams and flowcharts
A4.3       | Structured methodology      | ...
A4.4       | Modular Approach            | ...
A4.5       | Components                  | ...


\subsection Deviations Deviations and questions

\latexonly \newpage \endlatexonly
\section Overview Overview
The DispApplication Dispatcher Adaptation component is responsible to manage and schedule all other components in Dispatcher.
The hardware, platform and Vital FrameWork (VFW) setup is managed outside this component.

\subsection GeneralFunctionality General Functionality
The DispApplication Dispatcher Adaptation provides the instantiation of its component objects and registers all created component objects.<br>

\subsection ObjectDiagram Object Diagram

\subsection Dependencies Dependencies
The component is dependent on the AbstractApplicationBase class and the adaptation component classes to be created and scheduled as part of the Dispatcher process.\n
Call to dispatcher application API(run,preInit & run) should be done from DispMain .


\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection BehaviouralDescription Behavioural description

\subsubsection Preinitialisation Preinitialisation

Pre-initialization calls the preInit() function of all component classes.
@image html disp_application_preInit.png
@image latex disp_application_preInit.png


\subsubsection Initialisation Initialisation
Initialization calls the init() function in the component classes.
@image html disp_application_init.png
@image latex disp_application_init.png

\subsubsection ModeDependentOperation Mode dependent operation
The class shall not recognize any modes, it shall schedule all components based on the planned
sequence independently until execution is terminated.

\subsubsection Scheduling Scheduling
The DispApplication::run() method shall be scheduled by the vital framework (VFW) event timer.
The method has the following logic:
@image html disp_application_run.png
@image latex disp_application_run.png

\subsection ClassDiagram Class Diagram
@image html disp_application_class.png
@image latex disp_application_class.png

\subsection ExternalInterfaces External Interfaces
See chapter "Public Member Functions"

\subsection Diagnostics Diagnostics

\subsubsection Trace Trace-commands
The following component-specific trace-commands shall be implemented<br>
Not yet defined if to support or not.

\paragraph Analyze Analyze
Not supported.

\subsection FaultReporting Fault Reporting
Not yet defined.

\subsection TestFeatures Test Features
Describe any features that facilitate the testing of the design, both in development and at its target location.

\subsection Miscellaneous Miscellaneous functions


\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component

\section DesignSafety Design Safety

\subsection SIR SIR Requirement tracing

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
SIR-       | The SW Application shall only         | Memory is only allocated during init
SIR-       | Cross-compare                         | All persistent data in RadioChannel is registered for Cross-compare.



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

