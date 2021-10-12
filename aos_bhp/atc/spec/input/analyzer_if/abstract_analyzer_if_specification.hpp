namespace ATC::AbstractAnalyzerIF
{
/**
\if AsMainPage
@anchor aif
\mainpage AnalyzerIF Component Specification
\endif

\ifnot AsMainPage
\class ATC::AbstractAnalyzerIF
\endif

\section Purpose Purpose
This document specifies the software design for the AnalyzerIF (Analyzer Interface) component.

\latexonly \newpage \endlatexonly
\section Overview Overview
This component is the ATP part of the AOS Analyzer which is a graphical tool that can be used to record ATP/ATO measurable data and 
represent in graphical format. It can be used on target and simulated environments.
It records values of measurable variables registered to the AnalyzerIF component.

\subsection GeneralFunctionality General Functionality
The AnalyzerIF component is a shared component implementing the following:
- TCP Host application accepting a connection from the AOS Analyzer.
- When AOS Analyzer is connected, it sends information about the application within a message called \a UnitData and registered measurement variables to AOS Analyzer.
  AnalyzerIF implements the function of managing parameters that can be changed from AOS Analyzer. The list of these parameters must also be sent.
- Interface for other components to use for registering measurement variables.
- When measurement is started, Analyzer IF reports the values of the registered measurement variables to the AOS Analyzer as a semicolon-separated string at an
  interval defined as a configuration parameter (default 10 seconds).
- Console command, "Analyzer" shows informations about the registered measurement variables.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
The AnalyzerIF component has the following dependencies:

AnalyzerIF is dependent on the BasicIP component for handling the TCP/IP communication with the externally run AOS Analyzer.
BasicIP provides the means for initializing a TCP host connection to which the AOS Analyzer being the client can connect to, 
write data in terms of measurement variable values to the outgoing buffer and read data from the input buffers being the START/STOP commands from AOS Analyzer.

Other components are dependent on this components functionality by its public functions. <br>
Refer to Public Member Functions from \ref ATC::AbstractAnalyzerIF

\section FunctionalDesign Functional Design

\subsection Initialization Initialization
AnalyzerIF does the following during initialization.
- class variables are assigned default values within the constructor.
- Initializes a TCP/IP host connection with the port number retrieved from the configuration. The connection is setup only on ATP-A and the dispatcher.

ATP components that want to publish their internal variables must register these using \ref registerMeasurement in their initialization phase.

\subsection ModeDependentOperation Mode dependent operation
AnalyzerIF is independent of ATP Mode.

\subsection Scheduling Scheduling
The AnalyzerIF component implements the run() function (inherited from the base-class ProcComponent)
that must be called once each execution-cycle.

\subsubsection run run()
AnalyzerIf implements a set of states valid only to this component namely \a AIFNotConnected, \a AIFConnected and \a AIFmeasuring.
The state initially set to \a AIFNotConnected is checked in the run function and actions are taken based on it, but to consider 
the state, the connection towards AOS Analyzer must first be established as indicated by the BasicIP component.
If the connection is lost with AOS Analyzer the state is set to \a AIFNotConnected

In state \a AIFNotConnected 3 messages are sent to the AOS Analyzer after which the applications are considered connected thus the
state is changed to \a AIFConnected. The messages are sent in the order as listed below.

- \a UnitData, consisting of application specific information such as application name, version and protocol version. 
  The function handling this writeAIFUnitData() is defined as a pure virtual function thus is the responsibility of the adaptation to implement.
- list of measurable variables that are registered by each component at their initialization phase. See \ref writeAIFMeasurableList
- list of parameters that are registered as changeable from AOS Analyzer. Currently there are no such variables 
  but the message with the ParamterStart and ParameterEnd tags mush be sent.

When entered the \a AIFConnected state incoming messages are check for the \a START-command. When the start command is received 
the state changes to \a AIFMeasuring and the time stamp is saved. The time stamp is needed to relate time of measurements to 
start of the measuring.

In the \a AIFMeasuring state, incoming messages are checked for the \a STOP-command which is an indicator that the user of 
AOS Analyzer pressed the stop button. This will lead to the state getting set back to \a AIFConnected.
Unless the stop command is detected a snapshot of the measurement variables are taken and sent to AOS Analyzer.

@image html abstract_analyzer_if_run.png "Analyzer IF \a run function"
@image latex abstract_analyzer_if_run.png "Analyzer IF \a run function"

\subsection registerMeasurement Register measurement variables
Two functions are implemented for handling registration of measurement variables. One is the public function registerMeasurement() that is the 
interface towards other components. As variables may have different data-types this function is overloaded to handle this. This function implements
part of the registration that is data-type specific such as handling of min, max values and signedness. The other function is the private
function registerMeasurementInternal() that implements common parts of the registration, thus it is used internally by the overloaded
registerMeasurement() functions.
There is limit to the number of variables allowed to be registered. The limit is defined by the constant maxMeasureDataList.

The following data types are supported for registration
 - Boolean
 - Unsigned 8 bit int 
 - Unsigned 16 bit int
 - Signed 16 bit int  
 - Unsigned 32 bit int 
 - Signed 32 bit int 
 - Enumeration

\subsection writeAIFMeasurableList Send list of registered variables
At the initial phase of the AnalyzerIF-AOS Analyzer communication setup, in the run() function, one of the information that must be sent to 
AOS Analyzer is the list of registered variables. This information is compiled and written to the outgoing buffer in writeAIFMeasurableList().
The message must have the format described below, with a start-tag, one tag for each variable and finally the end-tag.
This message is an important input for the AOS Analyzer which gives the user the option to record and monitor the values of these variables.<br>
The format of the message:
- [MeasurablesStart]\\r\\n
- Name; Description; Type; Unit; Min; Max \\r\\n        (The text must be replaced with corresponding variable information)
- [MeasurablesEnd]\\r\\n

\subsection sendAIFSampleMeasurableData Sending sample data to AOS Analyzer 
Variables registered in Analyzer IF are sampled cyclically at a rate defined by the configuration parameter \a SendCycleAIF. Each time the
number of cycles has passed the registered variables are sampled and a message is created and written to the outgoing buffer including all values. 
The message must include a time-stamp and the variable values in the same order as they were registered. The values must be semicolon separated.
The time-stamp must be relative to the start of the recording, that is when the \a START command was received.

@image html abstract_analyzer_if_sendAIFSampleMeasData.png "\a sendAIFSampleMeasData function"
@image latex abstract_analyzer_if_sendAIFSampleMeasData.png "\a sendAIFSampleMeasData function"

\subsection readAIFCmd Reading commands
At each cycle the buffer for incoming messages is checked for new messages. To start sampling of variable values and to stop is triggered by commands 
sent from AOS Analyzer. The sampling is started upon receiving of the message containing \a START and then stopped upon receiving of a message 
containing \a STOP. These messages must be terminated with a line-break either with \\n or \\r.

\latexonly \newpage \endlatexonly
\section ClassDiagram Class Diagram
The class diagram for AbstractAnalyzerIF class is as follows:
@image html abstract_analyzer_if_class_diagram.png "Core Analyzer IF class diagram"
@image latex abstract_analyzer_if_class_diagram.png "Core Analyzer IF class diagram"

\section Diagnostics Diagnostics
\subsection Console Console-commands
The following console commands are supported in AnalyzerIF.
- Checks the Input as "help"
  + Prints the help message for Analyzer IF.
- Checks the Input as "Analyzer"
  + Checks any AIF measurable data is present in the measureListCalls and print the registered measurable variables.

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
AbstractAnalyzerIF defines the following pure virtual function that has to be implemented by an adaptation component.
 - writeAIFUnitData() - To generate and send the application specific information message \a UnitData to AOS Analyzer.
 - getConnectionID() - To provide the connection id used for TCP/IP connection handling.

\section PreProcessor Pre-Processor Directives
 - _DISPATCHER is used to separate code intended for CPU-A/B from being run on the CPU-C (dispatcher). 

\section Traceability Traceability
\subsection SSRS Functional Requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATC.

\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Safety tagged requirements and common requirements are specified in SCDS ATC.

Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

*/
}