namespace ATC::AbstractConsole
{
/** 
\if AsMainPage
@anchor console
\mainpage Console Component Specification
\endif

\ifnot AsMainPage
\class Abstract Console
\endif

\section Purpose Purpose
This document specifies the software design for the \ref AbstractConsole class, the ATC part of the Console component.

\latexonly \newpage \endlatexonly
\section Overview Overview
Console is a component that provides the means for a user to connect to AOS and retrieve information for diagnostic
purposes. It provides access to information provided by each component. Each component is responsible for implementing
the \ref consoleCall() function and provide any information that may be of special importance. This ATC class is
dependent on an adaptation class to handle certain project specific functionality.

\subsection GeneralFunctionality General Functionality
Console is implemented as a TCP host which it is possible to connect to and send queries in the form of console commands
and receive the requested information. The handling of a console command is either distributed among multiple components
or handled by a single component.

The Console component also provides means for other components to print trace information to the console. This is done
using the \ref TraceInterface class. To avoid printing all available information from all components all the time, the
amount of traces can be regulated with the help of a trace level. The user can enable or disable traces, as well as set
the trace level for each component, see \ref ConsoleCommands.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
Console is dependent on the BasicIP component for handling the TCP/IP communication between AOS and the client application (e.g. putty). 

All components implementing the consoleCall() functions shall provide a response to the \a help command which is one of the distributed 
commands. Generally the consoleCall() function of a component is expected to return true when the command is handled successfully, or
false when the command is not recognized. For distributed commands the return value doesn't matter, since the call is propagated to all
components anyway.

Other components have dependencies to this component because they use its public types
and methods, see \ref ATC::AbstractConsole

\section FunctionalDesign Functional Design
\subsection Initialization Initialization
Initialization is done in parts by the constructor and the init() function. 
The constructor handles the initialization of the member variables. The rest of the initialization is implemented in init() which
performs the following:
- Initializes a TCP/IP host with the port number retrieved from the configuration.
- Adds the trace objects of all the available components into the trace object vector.

The initialization in init() is only performed for ATP-A and Dispatcher.

\subsection ModeDependentOperation Mode dependent operation
Console is not mode dependent.

\subsection Scheduling Scheduling
The Console component has the following function that must be called each execution-cycle:

\subsubsection run run()
The run() function monitors the status of the connection towards the client. When the connection becomes active, the application
version and a command prompt is written to the console. As long as the connection is active the following is performed:
- Read any incoming commands from the client. If a command was received, invoke the \ref consoleCall function of each component
  until a component reports that it has handled the command successfully. If it is a distributed command, however, consoleCall
  is called for all components.
- Schedule any buffered output for transmission to the client application.

@image html console_run.png "The run function"
@image latex console_run.png "The run function"

\subsection distributedCalls Distributed Commands
One of the two types of commands supported by AbstractConsole is the distributed type of command. The distributed command is like
a regular command except the handling is implemented by multiple components. Therefore, AbstractConsole calls the \ref consoleCall
function for all components if the command is distributed.

For the handling of distributed commands, \ref isDistributedConsoleCall() is implemented to determine whether or not a command
is distributed. AbstractConsole defines the following commands as distributed:
 - "help"
 - "chstat"
 - "dmich"

\subsection traceInterface Tracing
One part of the Console component's functionality is to provide means for the developer to print valuable runtime information to the console.
This is implemented in the \ref TraceInterface class. All components have an instance of TraceInterface due to the inheritance of \a BaseComponent
and each component must provide the necessary parameters to instantiate the TraceInterface object. Among the needed parameters is a \a shortname
that is used to enable and disable tracing of the specific components.

AbstractConsole's consoleCall() function implements the user interface towards the trace functionality. (See \ref consoleConsoleCall)

TraceInterface uses AbstractConsole for the purpose of sending information to the client application.

Components such as DMIChannel and RadioChannel that explicitly create TraceInterface objects (in addition to the default one already
in BaseComponent) need to add these objects to Console's trace list in order for them to be handled by the Console component.

\subsection TheConsoleCallFunction The consoleCall function
\ref consoleCall is a virtual function defined in \a BaseComponent, that can be implemented by other components to support console commands.
The intention of this function is for the user to be able to display diagnostic information provided by any component while AOS is running.

\subsubsection consoleConsoleCall consoleCall in AbstractConsole
AbstractConsole also implements consoleCall() and since the Console component is the first component in the ATP application component list, it's 
consoleCall() is the first to be invoked when a command is received. The reason is to define the format of output to which all other components must 
follow and to handle pre-text for commands that may need it which are generally the distributed commands, such as \a help.

The consoleCall() in AbstractConsole also implements the interface towards the trace objects, allowing the user to enable and disable traces of any
component with available trace-levels. This is done with the \a trace command. See below flowchart for details about the consoleCall() implementation.

@image html console_console_call.png "AbstractConsole's \a consoleCall function"
@image latex console_console_call.png "AbstractConsole's \a consoleCall function"

\subsubsection Write Writing to Console
The AbstractConsole::write() is used by TraceInterface for writing to the console. TraceInterface in turn provides a set of overloaded 
TraceInterface::write() functions that should be used by other components to write anything onto the console. 
AbstractConsole::write() copies the given data onto the output buffer which will be scheduled for transmission in the \ref run function.

\section ClassDiagram Class Diagram

@image html abstract_console_class_diagram.png
@image latex abstract_console_class_diagram.png

\section Diagnostics Diagnostics
\subsection ConsoleCommands Console Commands
The following console commands are supported by AbstractConsole:
 - \a help - prints help information about supported commands
 - \a chstat - prints table heading for the information provided by other components.
 - \a trace - prints, for each component, its short name, whether the trace is enabled and its current trace level.
 - \a trace \a component-shortname - enables or disables the trace for the given component.
 - \a trace \a component-shortname \a trace-level - sets the trace level of the trace for the given component.

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
AbstractConsole defines the following pure virtual functions that must be implemented by an adaptation class:
- \ref getComponentIter() - returns an iterator that refers to the start of the component list
- \ref getComponentIterEnd() - returns an iterator that refers to the end of the component list
- \ref writeVersion() - writes the application version to the console (called when a console connection is established)

\section PreProcessor Pre-Processor Directives
- _DISPATCHER is used to distinguish the dispatcher from ATP-A and ATP-B during initialization.

\section Traceability Traceability

\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

The requirements relevant for this component are specified in SCDS ATC.

\subsection SSAS Architectural requirements
The architectural requirements are defined in [SSAS-APP].

Safety tagged requirements and common requirements are specified in SCDS ATC.

*/

}
