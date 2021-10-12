namespace ATP::Console
{
/**
\if AsMainPage
\mainpage Console Component Specification
@anchor console
\endif

\ifnot AsMainPage
\class Console
\endif

\section Purpose Purpose
This document specifies the software design for the \ref Console class, the adaptation part of the Console component.

\latexonly \newpage \endlatexonly
\section Overview Overview
Console is a component that provides the means for a user to connect to AOS and retrieve information for diagnostic
purposes.

\subsection GeneralFunctionality General Functionality
The adaptation class \ref Console creates the instance of the component, provides adaptation specific version information that
is written to the console and provides an interface for iterating over all component instances.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
This component is dependent on the following components:
- ATP Application component to provide access to the data structure that should have been populated with all components.

Other components have dependencies to this component because they use its public types
and methods, see \ref ATP::Console

\section FunctionalDesign Functional Design
\subsection Initialization Initialization
All the initialization activity done in the Core part of the component.

\subsection ModeDependentOperation Mode dependent operation
The Console component is not mode dependent.

\subsection Scheduling Scheduling
This component is executed every cycle using the core functionality. No adaptation of this core functionality is needed.

\section ClassDiagram Class Diagram
@image html console_class_diagram.png "Console class diagram"
@image latex console_class_diagram.png "Console class diagram"

\section Diagnostics Diagnostics
\subsection ConsoleCommands Console Commands
The following console commands are supported by Console:
 - \a help - prints help information about supported commands
 - \a version - prints the versions of the application, its configuration files and the supported TCC and DMI protocols
 - \a cyctime - prints the last/min/max execution times for one cycle
 - \a cyctime \a clear - resets the min/max execution time values

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
The following virtual functions override or implement the respective function declared in the abstract class:
- \ref consoleCall() - handles the supported console commands, see \ref ConsoleCommands
- \ref getComponentIter() - returns iterator pointing at the start of the component list
- \ref getComponentIterEnd() - returns iterator pointing at the end of the component list
- \ref writeVersion() - writes the application version to the console (called when a console connection is established)

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component.

\section Traceability Traceability

\subsection SSRS Functional Requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP BHP.

\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Safety tagged requirements and common requirements are specified in SCDS ATP BHP.

*/

}
