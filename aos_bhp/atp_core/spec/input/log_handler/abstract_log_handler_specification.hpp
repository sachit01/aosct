/** 
\if AsMainPage
\mainpage Abstract Log Handler Component Specification(Core/ATC)
@anchor lh
\endif

\ifnot AsMainPage
\class AbstractLogHandler
\endif

\section Purpose Purpose
This document specifies the software design for the Abstract Log Handler component. 

\latexonly \newpage \endlatexonly
\section Overview Overview
The Abstract Log Handler component is responsible for handling the logging mechanism of ATP and other AOS subsystem
blocks such as ATO and dispatcher onto the external interfaces i.e. RU, N-JRU and BDS. The logging onto the TCC and DMI should be done via event handler by creating an event of event type as "log".

\subsection GeneralFunctionality General Functionality
The Abstract Log Handler defined within ATC will be used as a shared component by ATP and dispatcher (no RU for dispatcher). The Abstract Log Handler implements the core functionality to support the general log handling mechanism
to be used within AOS. In general, the log handling supports a collection of logging events and texts with/without a
value within a user process and provides external access to current state.

The Abstract Log Handler is responsible for logging onto the RU, N-JRU and BDS. Logs to the N-JRU are also
sent to the BDS depending upon the BDS level and config BDS levels set. Logging to the RU is done via separate
API calls.
In order to log onto the TCC or DMI, component will create an event with the event handler of event type as "log"
and then call this event.

The Abstract Log Handler provides the following :
 - A generic interface for distributed logging to allow components to call Log Handler to log information.
 - Forwarding logged information to different storage units (N-JRU, RU, BDS) based on log levels as config
 parameter settings and/or type of information.
 - A buffering mechanism necessary because of the design of the interface to the destination storage.
 - Console commands to show statistics, set/show log-levels.

 Abstract Log Handler forwards the information to any one of the systems below, depending on the API called.
 - RU (Recording Unit)
 - N-JRU (Non-Juridical Recording Unit)
 - BDS (Basic Diagnostic System)

 
The Abstract Log Handler component is instantiated by an adaptation component which will be scheduled by the user process scheduling.

\subsection ObjectDiagram Object Diagram
N/A.

\subsection Dependencies Dependencies
The Abstract Log Handler component is dependent or has dependencies with following components:
- AbstractConfigBase: For getting configured levels, IP and port numbers for NJRU, RU and BDS.
- AbstractBasicIP: For IP connections. 
- AbstractConsole: For getting the iterators of components.

Other components are dependent on Abstract Log Handler component via following public methods:<br>
Refer to public member functions from \ref ATC::AbstractLogHandler.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection BehaviouralDescription Behavioural description

\subsubsection Initialization Initialization 

{ATC_LH0010} Abstract Log Handler will do the following for initialization at start-up via calling the init() function:
- Initialise number of messages sent to N-JRU,RU and BDS.
- Establish connection for CPU-A and dispatcher with the RU and N-JRU servers.
- Initialise connection with BDS.

{/} 

\subsubsection ModeDependentOperation Mode dependent operation
Abstract Log Handler is independent of ATP mode of operation.

\subsubsection Scheduling Scheduling
Abstract Log Handler is scheduled with below calls:

\paragraph run run()
{ATC_LH0002} Abstract Log Handler performs the below activity as part of the run() function:

- Write the data available in the N-JRU buffer onto the N-JRU.
- Write the data available in the RU buffer onto the RU.{/}

\subsubsection writeToLog writeToLog()
Abstract Log Handler opens up an API to all the components to enable them to write to external interface via writeToLog().
This function is responsible to:
- Check whether the level received from component is less or equal to the current supported config level for
N-JRU.
- writeToLog() is implemented as several overloaded functions with different types of arguments along with text
argument.
  + Check whether any arguments while calling writeToLog() contain value as argument
    + Concatenate the text received with the value received to form a TEXT
  + Concatenate the TEXT to njruBuffer
  + Call writeToLogInternal()

{ATC_LH0003}
@image html abstract_log_handler_write_to_log.png "Write to log"
@image latex abstract_log_handler_write_to_log.png "Write to log"
{/}

\subsubsection writeToLogInternal writeToLogInternal()
This function is responsible for:
{ATC_LH0001}
- It will check the file name and append it to N-JRU buffer.
- Concatenate the line number from where this data text has been logged.
- Call writeToBDS(), if it should be written to BDS.
{/}

\subsubsection logRU logRU()
{ATC_LH0004} Logs information towards the RU. It is an overloaded function, there are three logRU() functions for logging raw messages, digital I/O and analog I/O,
respectively.
 - For raw messages information includes fragment type whether it is single fragment, first fragment, in continuation or last fragment.
 - For digital I/O, it will log the digital value.
 - For analog I/O, it will specify the analog value.
 The information to be logged is formatted as text and appended to the RU buffer.{/}

\subsubsection writeEventToLogger writeEventToLogger()
Abstract log handler logs the data when any event is triggered. The event handler component is responsible to call writeEventToLogger() in log handler to do so. The writeEventToLogger():
 - Extracts the text from the received event to be logged.
 - Appends the file name and line number from the arguments to the extracted text .
 - Concatenates and stores the above combined data in the NJRU-Buffer.
 - Calls writeToBDS(text) to write the event code and text to BDS via BDS interface if bdslevel is greater than detailed level.

{ATC_LH0005}
@image html abstract_log_handler_write_Event_to_logger.png "Write event to logger"
@image latex abstract_log_handler_write_Event_to_logger.png "Write event to logger"
{/}

\subsubsection interfaceIdToString interfaceIdToString()
{ATC_LH0006} This function will return the interface from which the data is being logged. For core, data logging will be for DMI or IO.{/}

\subsubsection addHeaderToRUBuffer addHeaderToRUBuffer()
{ATC_LH0007} It will append the header to RU buffer with the date, time, interface and fragments which shall be logged on RU. 

Abstract Log Handler formats the logs to RU according to the following format:
\code
entry = syslog_header " ," session_id "," node "," datetime "," log_entry

syslog_header = full_datetime " " host " " process "[" pid "]: " [structured_data]
log_entry = normal_entry / jru_entry
normal_entry = level "," thread_id "," file ":" line "," component_id "," normal_message
jru_entry = "JRU_LOG," block "," interface "," version "," frag_marker "," jru_message

level = "critical" / "warning" / "field_test"
thread_id = number
file = string
line = number
component_id = string
normal_message = string

block = string
interface = "ETCS-SS026" / "ETCS-SS098-ITT-IN" / "ETCS-SS098-ITT-OUT" /
"FFFIS-IH" / "IF150" / "FFFIS-HMI" / "FFFIS-AP" / adaptation_interface
version = string
frag_marker = "F" / "C" / "L" / "S" ; F=First, C=Continuation, L=Last, S=Single
jru_message = text_message / binary_message
text_message = string ;
binary_message = 1*HEXDIG ; hexadecimal encoding of binary message

adaptation_interface = string ; adaptation protocol identifier

full_datetime = date "T" time "." centiseconds ("+" / "-") utc_offset
datetime = date " " time "." milliseconds
structured_data = "[ID " message_id user_level "]"

date = year "-" month "-" day
time = hours ":" minutes ":" seconds
utc_offset = hours ":" minutes
user_level = "user." syslog_level

host = string
process = string
pid = number
message_id = number
session_id = number
node = "A" / "B" / "C"
syslog_level = "warning" / "notice"
year = 4DIGIT
month = 2DIGIT
day = 2DIGIT
hours = 2DIGIT
minutes = 2DIGIT
seconds = 2DIGIT
centiseconds = 2DIGIT
milliseconds = 3DIGIT

string = 1*VCHAR
number = 1*DIGIT
\endcode
{/}

\subsubsection writeToNJRU writeToNJRU()
{ATC_LH0008} Calls njruBuffer.writeBufferTo() which sends the buffered characters to the N-JRU component.{/}

 \subsubsection writeToRU writeToRU()
{ATC_LH0009} Analogous to writeToNJRU(), i.e. does the same things but for the RU-Buffer.{/}

\section ClassDiagram Class Diagram
The class diagram for Abstract Log Handler is as follows:
@image html abstract_log_handler_class_diagram.png "Abstract Log Handler class diagram"
@image latex abstract_log_handler_class_diagram.png "Abstract Log Handler class diagram"

\section Diagnostics Diagnostics

\subsection Console Console-commands
The following component-specific Console-commands are implemented:<br>
 - Check the Input as "logstat"
 - Print the number of messages sent. The send counter is incremented every time a new message is logged to RU,N-JRU and BDS.
 - Check the Input as "loglevel"
 - Print the njruLevel and bdsLevel
 - Check the Input as "loglevel X Y"
 - Set njruLevel as X and bdsLevel as Y
 - Print the njruLevel and bdsLevel. 

\subsection Analyze Analyze
N/A.

\section Checkpoints Checkpoints
Checkpoints shall be inserted with the intention of verifying that the program flow of the ATP Software running on the
diversified hardware (CPU-A and CPU-B) have taken the same execution path. Checkpoints has been inserted to the below function
+ run()

\section CrossCompare Cross-compare
N/A.

\section CoreAdaptation Core / Adaptation
- Abstract Log Handler handles the logging procedure and logs information to RU, N-JRU and BDS.
- Adaptation of the Log Handler component adds the header to the given buffer in a text format and also translates the interface id to the string.

\section PreProcessor Pre-Processor Directives
Pre-processor directives available for this component is:
 - _DISPATCHER is used to exclude RU logging on dispatcher and avoid execution of unnecessary and invalid code on dispatcher.

\section Traceability Traceability

\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

The requirements relevant for this component:

Req        | CTT        | Chapter                  | Function       
---------- | ---------- | ------------------------ | -------------- 
AOS 935    | ATC_LH0007 | \ref addHeaderToRUBuffer | addHeaderToRUBuffer()
AOS 2676   | ATC_LH0004 | \ref logRU               | interfaceIdToString()
AOS 2027   | ATC_LH0005 | \ref writeEventToLogger  | writeEventToLogger()
-          | ATC_LH0003 | \ref writeToLog          | writeToLog()
-          | ATC_LH0006 | \ref interfaceIdToString | interfaceIdToString()
-          | ATC_LH0002 | \ref run                 | run()
-          | ATC_LH0008 | \ref writeToNJRU         | writeToNJRU()
-          | ATC_LH0009 | \ref writeToRU           | writeToRU()
-          | ATC_LH0001 | \ref writeToLogInternal  | writeToLogInternal()


\subsection SSAS Architectural requirements
The architectural requirements are defined in [SSAS-APP].

Only the architectural requirements traced explicitly to this component is included in the table below.
Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].


Req          | CTT        | Chapter             | Function
------------ | ---------- | ------------------- | --------------
AOS_AS 349 S | ATC_LH0010 | \ref Initialization | init()

*/

