namespace ATP::Kernel
{
/** 
\if AsMainPage
\mainpage ATP Application BHP Adaptation Component Specification
@anchor app
\endif

\ifnot AsMainPage
\class Template
\endif

\section Purpose Purpose
This document specifies the details of the software design and implementation for the ATP Application adaptation component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality

The ATPApplication class is inherited from AbstractATPApplication.
The component is responsible for
- Initializing, Managing and scheduling all ATP components for I/O Handling, main processing and background processing.
- Adding all the ATP components to the component list and initializing them.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
  
 The ATP component classes are created and scheduled as part of the ATP thread.

 Other components have dependencies to this component because they use its public types
 and methods, see ATPApplication Class Reference.
  
\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection BehaviouralDescription Behavioural description
\subsubsection Initialization Initialization

The ATP Application component schedules the execution of the components in the methods
  -	ATPApplication::addAllComponents() function adds all the components to the component list.
  -	ATPApplication::preInit() shall be called once by ATPMain after that the components are created with ATPApplication::addAllComponents().
  -	After ATPApplication::preInit(), Setup the Vital Framework to call the  ATPApplication::init() shall be called each cycle(100ms) until it is completed and returns true
  -	ATPApplication::run() shall be called each cycle after ATPApplication::init() has been completed.

Initialization happens in four stages
- stage 1, config component is initialized to fetch the values from NVSH. This is done so that when other components are initialized they can get their configuration values.
- stage 2, the debugging, logging and the Basic IP components are initialized.
- stage 3, the Basic IP, debugging and logging components are run, while the remaining components are initialized.
- stage 4, returns the final status of init() function.

The config component has to be started before all other component because provides access to the stored configuration parameters. 
The ATP config component organizes the parameters in the different memory areas.

\subsubsection ModeDependentOperation Mode dependent operation
The component is independent of any modes, it invokes all components based on the planned
sequence independently of any mode.  

\latexonly \newpage \endlatexonly
\subsubsection Scheduling Scheduling

\subsubsection run run
 
The ATPApplication::run() function is scheduled by the vital framework (VFW) event timer and executes all the components cyclically
by calling the parent class function AbstractATPApplication::run().
It also keeps track of the execution time of the last cycle and updated the min and max cycle-execution times.

The cycle time being the time it takes execute all the components is calculated as below:

Last Execution Time = ((time after core ATP application call - time before core ATP application call)) / 1000000 (converting to milliseconds).  

@image html atp_application_run.png "ATPApplication::run()"
@image latex atp_application_run.png "ATPApplication::run()"
  

\latexonly \newpage \endlatexonly
\section ClassDiagram Class Diagram
@image html atp_application_component.png "ATP Application"
@image latex atp_application_component.png "ATP Application"

\section Diagnostics Diagnostics

\subsection Console Console-commands
N/A

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
The adaptation part of the ATP Application component is responsible for instantiating and initializing all the components listed in \ref Dependencies.

\section PreProcessor Pre-Processor Directives
GENERATE_TCC_EVENTLIST_XML_PATH : To generate the event list into xml files.  

\section Traceability Traceability

\subsection SSRS Functional requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP BHP.

\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP BHP.

*/
}
