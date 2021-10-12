namespace ATP::TG
{

/**
\if AsMainPage
\mainpage TIC Component Specification
@anchor ti
\endif

\ifnot AsMainPage
\class AbstractTIC
\endif

\section Purpose Purpose
This document specifies the software design for the class AbstractTIC, the core part
of the Train Integrity Control component (TIC).

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
The TIC component is responsible for detecting the availability of automatic train configuration.
If automatic train configuration is available, AOS fetches the train configuration from there
instead of requesting manual entry by the driver.

The component or system from which the train configuration is obtained is defined in the adaptation part.
The availability of automatic train configuration will therefore be determined in the adaptation part.
It is responsible for facilitating the monitoring of TIC Availability. Actual handling is done in the adaptation part.

AbstractTIC will provide access functions for the followed mentioned major functionality:
- TIC Availability, which states whether or not TIC is available in the train system.
- TIC Configuration Status, which indicates if the Automatic Train Configuration is in Idle, In Progress, Pending, Completed or Error state.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
AbstractTIC has dependencies to the following component:
+ Vehicle Com: To determine whether Vehicle Com is connected.

Other components have dependencies to this component because they use its public types
and methods, see \ref AbstractTIC Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization
During initialization, AbstractTIC resets the state variables to default.
It sets the TIC configuration status to Idle, TIC availability to false and initiates cross-compare.

\subsection  ModeDependentOperation Mode dependent operation
TIC component is independent of ATP mode.

\subsection Scheduling Scheduling
The AbstractTIC component has the AbstractTIC::run() function that is called each execution cycle:

\subsubsection run run()
The run() function retrieves the TIC availability and handles TIC train configuration states accordingly.
If the TIC availability is true, then it further handles by calling \ref AbstractTIC::handleTicAvailable()
else it checks to report Train Configuration status as Error.

The function evaluateTICAvailable() is called by run(). It returns false indicating that TIC is not available 
but adaptation defines and overrides the value.

The function \ref AbstractTIC::handleTicAvailable() performs the following:
+ If TIC configuration status is Idle or Error, it checks whether Automatic Train Config Requested or not.
 + If requested then the configuration status changes to Pending or In Progress, it checks for TIC timer for timeout expire. 
      If the timer has expired, then the configuration status is set to Error, else it updates the result from 
      virtual function \ref AbstractTIC::evaluateConfigReqStatus().
+ If TIC configuration status is Complete, then it resets the TIC timer.

\subsection getTICAvailability TIC Availability
The function AbstractTIC::getTICAvailable() returns the availability of TIC.
If availability has been evaluated to true by adaptation layer method TIC::evaluateTICAvailable(), then
it is set to true.

\subsection abortConfig Abort train configuration
The function AbstractTIC::abortConfig() aborts any retrieval of Train Configuration In Progress. 
It will set TIC Configuration status to Idle and stop the timer for fetching train configuration.

\subsection requestConfig Request train configuration
The function AbstractTIC::requestConfig() starts retrieving the current Train Configuration.

\section ClassDiagram Class Diagram

@image html abstract_tic_class_diagram.png "AbstractTIC Class diagram"
@image latex abstract_tic_class_diagram.png "AbstractTIC Class diagram"

\section Diagnostics Diagnostics

\subsection ConsoleCommands Console Commands
The following component-specific console command is implemented:
+ tic - prints TIC availability, TIC config status and whether Vehicle Com is connected.

\subsection Analyze Analyze
No values are registered for analysis for TIC component.

\section CoreAdaptation Core / Adaptation
All the major functionality in TIC is in the adaptation part. To implement automatic train configuration,
the adaptation overrides the following methods:

- AbstractTIC::evaluateTICAvailable()
- AbstractTIC::evaluateConfigReqStatus()
- AbstractTIC::getLocoOrientationAvailable()

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component

\section Traceability Traceability

\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP Core.

The requirements relevant for this component are:

Req        | Chapter                     | Function
---------- | --------------------------  | --------------
AOS 1262   | \ref FunctionalDesign       | AbstractTIC::requestConfig()
AOS 2298   | \ref FunctionalDesign       | AbstractTIC::evaluateTICAvailable(), AbstractTIC::getLocoOrientationAvailable()
AOS 2850   | \ref CoreAdaptation         | AbstractTIC::getLocoOrientationAvailable()
AOS 2804   | \ref FunctionalDesign       | AbstractTIC::handleTicAvailable()


\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP Core.

*/

}
