namespace ATC
{

/** 
\if AsMainPage
\mainpage Log Handler Component Specification
@anchor lh
\endif

\ifnot AsMainPage
\class AbstractLogHandler
\endif

\section Purpose Purpose
This document specifies the software design for the abstract class AbstractLogHandler, the ATC part
of the Log Handler component.

\latexonly \newpage \endlatexonly
\section Overview Overview
AbstractLogHandler is responsible for the logging mechanism in ATP and other AOS applications such as ATO
and Dispatcher. AbstractLogHandler forwards the data to be logged to the logging services RU, N-JRU and BDS.

<b>Please note:</b> Logging onto the TCC and DMI is done using the Event Handler component by reporting an event.

\subsection GeneralFunctionality General Functionality
AbstractLogHandler implements the core functionality to support logging used within AOS. AbstractLogHandler
supports logging the following information:
- Events
- Text messages, with or without an additional value
- Raw (binary) messages
- Digital and analog I/O

AbstractLogHandler forwards the information to one or more of the logging services below, depending on the API called:
 - RU (Recording Unit)
 - N-JRU (Non-Juridical Recording Unit)
 - BDS (Basic Diagnostic System)

AbstractLogHandler is placed within ATC so it can be used by both ATP and other applications in AOS.

\subsection DeploymentDiagram Deployment Diagram
The following diagram shows how Log Handler interacts with the logging services.
ATP is shown as an example. Deployment in other AOS applications will be analogous.

@image html abstract_log_handler_deployment.png "Deployment diagram"
@image latex abstract_log_handler_deployment.png "Deployment diagram"

\subsection Dependencies Dependencies
AbstractLogHandler has dependencies to the following components:
- Basic IP: For managing IP connections.
- Config: For getting log levels, IP addresses and port numbers for N-JRU, RU and BDS.
- Console: For writing text to the console.

Other components have dependencies to this component because they use its public types
and methods, see \ref AbstractLogHandler Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization 

AbstractLogHandler will do the following initialization at start-up via calling the init() function:
- Initialize all member variables that were not already initialized by the constructor.
- Establish connections to the logging servers (RU for ATP A, N-JRU for ATP A and Dispatcher).
- Establish connection to BDS.

Please note that AbstractLogHandler on ATP B does not log anything towards RU or N-JRU.

\subsection ModeDependentOperation Mode Dependent Operation
AbstractLogHandler is independent of ATP mode of operation.

\subsection Scheduling Scheduling
\subsubsection run run()
AbstractLogHandler is scheduled by calling the run() function in each execution cycle and this function
should be called after all other components that have data to be logged. run() performs the following:

- Sends any data present in the N-JRU buffer to the N-JRU.
- Sends any data present in the RU buffer to the RU.

Note: The logging towards BDS isn't buffered by AbstractLogHandler and therefore, it doesn't need any handling in run().

\subsection LoggingFunctions Logging Functions
\subsubsection logRU logRU()
This function is an overloaded function that logs information towards the RU.
There are three logRU() functions for logging raw (binary) messages, digital I/O and analog I/O, respectively.
 - Raw messages will be converted to hexadecimal numbers.
 - For digital I/O, it will log the digital value.
 - For analog I/O, it will log the analog value.
 - The information to be logged is formatted as text and appended to the RU buffer for later transmission, see \ref run.

\subsubsection writeEventToLogger writeEventToLogger()
AbstractLogHandler logs the data when any event is triggered. The Event Handler component is responsible
for calling writeEventToLogger() to do so.

The information logged is:
- The time when the event occurred (and the date, which is added by N-JRU)
- The position and speed of the train (added by adaptation, see \ref CoreAdaptation)
- Application Id and component Id
- The text message stored in the Event
- The optional dynamic text stored in the Event
- File name and line number indicating where this event was raised

This information is appended to the N-JRU buffer for later transmission, see \ref run.

\subsubsection writeToLog writeToLog()
AbstractLogHandler provides the function writeToLog() to all components to enable them to write log messages
to the external loggers N-JRU and BDS. The function is implemented with several overloads that support logging
different integer types, in addition to a text string.

- Logs the message only if the log level given to writeToLog() is less than or equal to the current N-JRU log level.
- Builds the log message from the given string, integer value, file name and line number.
- Adds this log message to the N-JRU buffer for later transmission, see \ref run.
- Sends the log message to BDS (if the log level given to writeToLog() is less than or equal to the BDS log level).

\subsection ruLogFormat RU Log Format
AbstractLogHandler formats the logs to RU according to the format described in [FIS_JRU].

\section ClassDiagram Class Diagram
The class diagram for AbstractLogHandler is as follows:
@image html abstract_log_handler_class_diagram.png "AbstractLogHandler class diagram"
@image latex abstract_log_handler_class_diagram.png "AbstractLogHandler class diagram"

\section Diagnostics Diagnostics

\subsection Console Console-commands
The following component-specific console commands are implemented:

- logstat, which prints information about all the messages received and send from LogHandler Component.
- loglevel, which prints the current N-JRU and BDS log levels.
- loglevel X Y, which sets the N-JRU log level to X and BDS log level to Y.

\subsection Analyze Analyze
N/A.

\section CoreAdaptation Core / Adaptation

The following logging functionality can or must be implemented by an adaptation class:

- The adaptation must derive a concrete class from AbstractLogHandler and instantiate this concrete class.
- The class in adaptation must implement the pure virtual function \ref AbstractLogHandler::addApplicationIdToBuffer()
  in order to log the adaptation specific application Id.
- The virtual function \ref AbstractLogHandler::addPositionAndSpeedToBuffer() can be overloaded to log the position
  and speed of the train.
- The virtual function \ref AbstractLogHandler::interfaceIdToString() can be overloaded by the adaptation class to
  support logging of data from interfaces not defined in AbstractLogHandler.

\section PreProcessor Pre-Processor Directives
Pre-processor directives available for this component is:
- _DISPATCHER is used to exclude RU logging on Dispatcher and avoid execution of unnecessary
and invalid code on Dispatcher.

\section Traceability Traceability

\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATC.

The requirements relevant for this component are:

Req        | Chapter                  | Function
---------- | ------------------------ | --------
AOS 935    | \ref writeEventToLogger  | \ref AbstractLogHandler::addNJRUHeaderToBuffer()
AOS 1995   | \ref writeEventToLogger  | \ref AbstractLogHandler::writeEventToLogger()
AOS 2015   | \ref writeEventToLogger  | \ref AbstractLogHandler::writeEventToLogger()
AOS 2021   | \ref writeEventToLogger  | \ref AbstractLogHandler::writeEventToLogger()
AOS 2027   | \ref writeEventToLogger  | \ref AbstractLogHandler::writeEventToLogger()
AOS 2676   | \ref logRU               | \ref AbstractLogHandler::logRU()
AOS 2677   | \ref logRU               | \ref AbstractLogHandler::logRU()

\subsection SSAS Architectural requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATC.

*/

}
