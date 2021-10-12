namespace ATP
{

/** 
\if AsMainPage
\mainpage Log Handler Component Specification
@anchor lh
\endif

\ifnot AsMainPage
\class LogHandler
\endif

\section Purpose Purpose
This document specifies the software design for the class LogHandler, the BHP adaptation part
of the Log Handler component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
LogHandler is the adaptation of the AbstractLogHandler class. The main purpose of LogHandler
is to instantiate the component for use within ATP. It also adds some adaptation
specific functionality, see \ref CoreAdaptation.

\subsection DeploymentDiagram Deployment Diagram
N/A.

\subsection Dependencies Dependencies
The LogHandler class has the following dependencies to other components:
 - Odometry, to get the speed of the train.
 - Position, to get the position of the train.

Other components have dependencies to this component because they use its public types
and methods, see \ref LogHandler Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization 
All the initialization activity is done in the core part of the component.

\subsection ModeDependentOperation Mode Dependent Operation
LogHandler is independent of ATP mode of operation.

\subsection Scheduling Scheduling
The component is executed every cycle using the core functionality. No adaptation of the core scheduling is needed.

\subsection Functionality Functionality

\subsubsection instance instance()
\ref LogHandler::instance() instantiates the singleton instance and returns a reference to it.

\subsubsection addApplicationIdToBuffer addApplicationIdToBuffer()
\ref LogHandler::addApplicationIdToBuffer() adds the application Id "ATP" to the N-JRU log.

\subsubsection addPositionAndSpeedToBuffer addPositionAndSpeedToBuffer()
\ref LogHandler::addPositionAndSpeedToBuffer() adds the following information to the N-JRU log:
- The track and position of the leading end of the train.
- The speed of the train.

\subsubsection interfaceIdToString interfaceIdToString()
\ref LogHandler::interfaceIdToString() converts the ID of an interface to a text string written to the
log. It is overridden here to add support for the interfaces only present in adaptation (LCS and OBRD).

\section ClassDiagram Class Diagram
The class diagram for LogHandler is as follows:
@image html log_handler_class_diagram.png "Adaptation LogHandler class diagram"
@image latex log_handler_class_diagram.png "Adaptation LogHandler class diagram"

\section Diagnostics Diagnostics

\subsection Console Console-commands
N/A.

\subsection Analyze Analyze
N/A.

\section CoreAdaptation Core / Adaptation
LogHandler is derived from AbstractLogHandler and adds the following functions:

- \ref instance
- \ref addApplicationIdToBuffer
- \ref addPositionAndSpeedToBuffer
- \ref interfaceIdToString

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component.

\section Traceability Traceability
\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP BHP.

The requirements relevant for this component are:

Req           | Chapter                          | Function
------------- | -------------------------------- | --------
AOS_BHPB 2794 | \ref interfaceIdToString         | \ref LogHandler::interfaceIdToString()
AOS_BHPB 2817 | \ref interfaceIdToString         | \ref LogHandler::interfaceIdToString()
AOS_BHPB 3347 | \ref interfaceIdToString         | \ref LogHandler::interfaceIdToString()

\subsection SSAS Architectural requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP BHP.

*/

}
