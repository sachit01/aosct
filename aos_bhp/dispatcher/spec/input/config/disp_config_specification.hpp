/** 
\if AsMainPage
\mainpage Config Component Specification
@anchor cfg
\endif

\ifnot AsMainPage
\class Template
\endif

\section VersionLog Version Log

Version | Date       | Description                   | Signature
------- | --------   | ----------------------------- | ---------
1.0     | 2016-11-23 | Created                       | saprasad
1.1     | 2017-01-03 | updated with preInit()        | spandita 

\section Summary Executive Summary

\subsection Purpose Purpose
This document specifies the software design for the Config component(Dispatcher). 
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
For a reader who just wants to get a first overview of the AOS_IF150 Dispatcher SW environment it should 
be enough to read the chapter "Overview". This chapter shall be consumed anyway by every reader 
since it might contain additional hints on where important information is to be found.
 
\subsection References References
\subsubsection ProjectDocuments Project Documents
Ref        | Document Name                            | Document Id       | Version
---------- | ---------------------------------------- | ----------------- | ---------
[SWAS]     | ATP Software Architecture Specification  | 3NSS?             | 1.0
[SYSREQ]   | ATP (Sub)System Requirements             | DOORS             | Baseline ?
[SIR]      | Safety Integrity Requirements            | ?                 | ?
[CPD]      | Config Parameter for Dispatcher          | ?                 | ?

\subsection DefinitionsAndABbreviations Definitions and Abbreviations
\subsubsection Definitions Definitions 
Term           | Definition                                   
-------------- | ------------------------------------------------------------------
    -          |     -   

\subsubsection Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
AOS            | ATP/ATO Onboard System 
ATP            | Automatic Train Protection
OPC            | Onboard Protocol Converter
COD            | Common Odometry
VIOH           | Vital IO Handler
API            | Application Program Interface
TCC            | Train Control Centre 
DMI            | Driver Machine Interface
NVSH           | Non-Volatile Storage Handling   


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
\subsection GeneralFunctionality General Functionality

Config is the Dispatcher adaptation of the AbstractConfigBase class. The main purpose of this component is to set up the memory areas for config parameters, Add 
config parameters required by Dispatcher during initialization,and instantiate the Config component for use within the dispatcher user process.

\subsection Dependencies Dependencies

The Config dispatcher component depends on VFW  and  NVSH API.\n 
DispApplication need to execute the config API like init(),preInit() and run().

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection BehaviouralDescription Behavioural description
\subsubsection Initialization Initialization 

\paragraph preInit preInit()

-  Set up the memory areas for config parameters 
-  Add the three binary file names for reading the config parameters in Dispatcher
-  Call VFW halt in case of any failure generated while setting up the memory areas

\paragraph init init()

When init() is called there are two different paths of action to take depending on boolean variable(runOnce) .

If runOnce  is true, Then perform following operations :
-    Add config parameters for dispatcher
-    Set the runOnce to false
-    Call the init function of AbstractConfigBase class

If runOnce is false,then 
-    Call the init function of AbstractConfigBase class

Please refer the init flowchart for more details.

@image html disp_config_init.png 
@image latex disp_config_init.png 

\subsubsection ModeDependentOperation Mode dependent operation
Dispatcher config is mode independent.

\subsubsection Scheduling Scheduling

The virtual run() - function is not implemented in the Dispatcher.

\subsection ClassDiagram Class Diagram
@image html config_class_diagram.png 
@image latex config_class_diagram.png 

\subsection ExternalInterfaces External Interfaces
See chapter "Public Member Functions"

\subsection Diagnostics Diagnostics
\subsubsection Trace Trace-commands
\paragraph Analyze Analyze
\subsection FaultReporting Fault Reporting
\subsection TestFeatures Test Features
\subsection Miscellaneous Miscellaneous functions
\subsubsection CrossCompare Cross-compare
The cross comparision is managed by the abstract config base.

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component

\section DesignSafety Design Safety

\subsection SIR SIR Requirement tracing

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
 SIR -     | The SW Application shall only         | Memory is only allocated during init
 SIR-      | Cross-compare                         | All persistent data in Config is registered for Cross-compare. 
 
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

