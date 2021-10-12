/**
\if AsMainPage
\mainpage Recording Unit
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2018-03-15 | First version                                 | marlundg


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
RU             | Recording Unit

\section Introduction Introduction

\subsection Design Design Overview

The Recording Unit (ATP Part) is used to provide logging information to be stored in the RU-log.

The log-information (from the interfaces specified in requirements below), are processed, formated and sent further to the RU-server on DMI.
The RU-server will store the log-files, which can be analyzed by a post-processing tool.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
AOS_BHPB 2794 | The AOS shall record the messages specified by the FFFIS AOS-LCS, ref [101], when sent or received on the LCS interface. Cyclic messages shall be recorded such that all relevant information is stored.  | Not Implemented
AOS 2676      | The AOS shall record the Driver Interface I/O listed below: Displayed Driver information specified in chapter 'DMI', Driver input specified in chapter 'DMI' | Not Implemented
AOS 2677      | The AOS shall record all state changes in the Vehicle Interface I/O specified in chapter 'Parallel'.| Not Implemented
AOS_BHPB 2817 | The AOS shall record Brake Pressure 1 (BP1) and Brake Pressure 2 (BP2) received in the Vehicle Interface in such way that all relevant information is stored.| Not Implemented
AOS 935       | All recorded data shall be logged with date, time (UTC) and position. | Not Implemented

\section SystemArchitecturalDesign System Architectural Design

\subsection ChosenSystemArchitecture Chosen System Architecture

The existing infrastructure for logging to N-JRU can be reused. The LogHandler class is extended to handle also RU-logging with the decided format.
The RU-logging will use a new socket connection through the BasicIP component to connect to a RU-Server that will execute in parallel with the
N-JRU Server. 

@image html deployment_diagram.png
@image latex deployment_diagram.png

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

Storage of data to BDS not possible due to to the size of the messages.

\subsection ExternalInterfaceDescription External Interface Description

A new TCP/IP Client connection will be created from the LogHandler to connect to the RU-Server in DMI.

\subsubsection RUFormat RU-Format

The message-format used for TCC shall be used as a base (minor modifications will be done to cover AOS requirements, adding Position and relevant interfaces). It is desirable that the same external tool to post-process the JRU-log can be used
for both TCC and AOS (see FIS TCC JRU, 3NSS011034-01).

\code
entry = syslog_header " ," session_id "," node "," datetime "," log_entry

syslog_header = full_datetime " " host " " process "[" pid "]: "
[structured_data]
log_entry = normal_entry / jru_entry
normal_entry = level "," thread_id "," file ":" line "," component_id ","
normal_message
jru_entry = "JRU_LOG," block "," interface "," version "," frag_marker ","
jru_message

level = "critical" / "warning" / "field_test"
thread_id = number
file = string
line = number
component_id = string
normal_message = string

block = string
interface = "ETCS-SS026" / "ETCS-SS098-ITT-IN" / "ETCS-SS098-ITT-OUT" /
"FFFIS-IH" / "IF150" / "FFFIS-HMI" / "FFFIS-AP" /
adaptation_interface
version = string
frag_marker = "F" / "C" / "L" / "S" ; F=First, C=Continuation, L=Last,
; S=Single
jru_message = text_message / binary_message
text_message = string ; see [7], [8] or specification provided by
; adaptation
binary_message = 1*HEXDIG ; hexadecimal encoding of binary message:
; see [3], [4], [5], [6] or specification
; provided by adaptation

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


Updates for this format (in progress in system document) is:

- Possibility to add a entry in the beginning of message to specify which Node that has produced the JRU-log (i e TCC/AOS), this would make it possible for the external tool to distinguish how to interpret the log ?
- Some fields would be possible to remove for the AOS (Host, Process, Pid) or might be set to '-')
- Position needs to be added for AOS (pending discussion within system team)
- AOS interfaces needs to be added for 'interface': LCS(adaptation), AOS_IO and DMI.

\section DetailedDescriptionOfComponents Detailed Description of Components

Three different interfaces shall be logged (LCS, AOS_IO and DMI). Detailed descriptions follows for each component. 
(OPC i/f is not part of AOS SSRS, currently under discussion)

The LogHandler needs to be extended with connection to the RU via Basic IP, and functionality to process a log request towards the RU.
Finally the configuration needs to be extended with some new parameters.

\subsection LogHandler Log Handler

The Log-handler extensions that is needed:

- Establish connection with RU server (Only on A-side)
- New interface method to log to RU (eg logRU(...))
- Fetch time, position, component ID and binary data and format this according to Format Chapter
- Write to RU channel (Only on A-side)

\subsubsection LogHandlerDetails Log Handler Details

<br><code>
AbstractLogHandler::init()
</code>

<br>Need to add initialization of ip/port to RU in DMI. No Log-level. This shall only be done on the A-side.

<code>
AbstractLogHandler::logRU(enum interface_id id, uint32_t length, uint8_t* buffer)
</code>

<br>New method to compose the message according to RU-Format chapter (E g collect all information time, position, component and binary data).
Write to RU buffer.

<br><code>
AbstractLogHandler::writeToRU()
</code>

Similar to writeNJRU, but write to RU-buffer to RU Connection. This shall only be done on the A-side.

<br><code>
AbstractLogHandler::appendToNjruBuffer(..)
</code>

<br>
This functionality will be moved to new xRuBuffer class.


A new class (e g xRuBuffer) is created to relief LogHandler. The xRuBuffer will process and work with the NJRU- and RU buffer.
The functionality is basically from the <code>AbstractLogHandler::appendToNjruBuffer</code>, and also encapsulates data and methods related to the buffer handling:

@image html class_diagram.png
@image latex class_diagram.png

The methods using the njru-buffer will be updated to handle the new class instead.

\subsection VehicleCom Vehicle Com

Log every message (since time fields is updated every second), sent/received every 500ms.

LogData Format: InterfaceID = 'LCS', EMP Layer and FFFIS AOS-LCS Layer for both input/output

Outgoing data: <code>LCSMessageHandler::runOut(void)</code>
<br>Add logging after: <code>traceBinaryData(trace, ATC::veryDetailedTrace, empMessage.getEMPBuffer(), empMessage.getEMPMessageActualLen());</code>

Incoming data: <code>LCSMessageHandler::runIn(void)</code>
<br>Add logging after: <code>traceBinaryData(trace, ATC::veryDetailedTrace, empMessage.getEMPBuffer(), empMessage.getEMPMessageActualLen());</code>

\subsection LocoIO Loco I/O

<br>LogData Digital Output Format: InterfaceID = 'I/O', Digital, Output, ID, value
<br>Outgoing data: <code>AbstractLocoIO::writeOutputs(void)</code>
<br>Add logging after: <code>retVal = viohClientHandle->VOUSetOutput(digitalOutputs[outputsCount].signalOut, digitalOutputs[outputsCount].newValue, static_cast<bool_t>(true));</code>

<br>LogData Digital Input Format: InterfaceID = 'I/O', Digital, Input, ID, value
<br>Incoming data: <code>AbstractLocoIO::readInputs(void)</code>
<br>Add logging after this line, if value has changed since last call: <code>retVal = viohClientHandle->VIUGetState(digitalInputs[inputsCount].signalIn, &state, &healthState);</code>

<br>LogData Analog Input Format: InterfaceID = 'I/O', Analog, Input, ID, value
<br>Incoming data: <code>AbstractLocoIO::readInputs(void)</code>
<br>Add logging after this line, if value has changed more than a configured (see Configuration chapter) value since last call: <code>retVal = viohClientHandle->AIOUGetState(analogInputs[inputsCount].analogSignalIn, &analogValue, &healthState);</code>

\subsection DMI DMI Comm

Add a new variable (e g bool logToRU) to the AbstractDMIMessageOut/In class to tell if message shall be logged to RU or not.
This variable needs to be set in the actual message class if log criteria is full-filled (either changed state or timer for dynamic data has passed).

This needs to be done only for the continuous data, all the intermittent messages can set this variable to true all the time:

- DMIMessageOutATPModesAndStatus (changed state)
- DMIMessageOutDriverInfo (changed state and dynamic data)
- DMIMessageOutSpeedAndDistance (dynamic data)

- DMIMessageInDMIStatus (changed state)

In order to check if the state has changed, the old values from last cycle must be stored and compared to the new collected values.

LogData Format: InterfaceID = 'DMI', Message Type + Message Data

Incoming data: <code>AbstractDMIHandler::runIn(void)</code>

\code{.c}
if (!isValidationOK)
{
  trace.write(ATC::detailedTrace, "DMI Handler:Validation of the Incoming DMI Message Failed");
  writeToLog(ATC::DetailedLog, "DMI Handler:Validation of the Incoming DMI Message Failed");
}
else
{
  // Add logging here.
  // Check if logging shall be done.
  // Start: currDMIMessage.dmiData Length: currDMIMessage.msgLen
}
\endcode

Outgoing data: <code>AbstractDMIHandler::runOut(void)</code>

\code{.c}
// Write DMI Message to the Active Channel
bool dmiMessageWritten = activeChannel->writeMessage(dmiMessageToSend);

// Add logging here.
// Check if logging shall be done.
// Start: dmiMessageToSend.dmiData Length: dmiMessageToSend.msgLen
\endcode

\subsection BasicIP Basic IP

Add new connection to RU.

\subsection Configuration Configuration

\subsubsection Core Core

- New parameter to configure RU-logging-rate for dynamic values.
- New parameter for RU IP (default value same as NJRU-IP)
- New parameter for RU Port (default value 30180)
- New parameter for how much Analog Input Values can differ without sending to RU (In raw-unit, default value 30)

\section UserInterfaceDesign User Interface Design

\subsection DescriptionOfTheUserInterface Description of the User Interface

\subsubsection ScreenImages Screen Images

\subsubsection ObjectsAndActions Objects and Actions

\section ImplementationDistribution Task Distribution

Implementation can be split in three parts:

- New/updated logging functionality for RU (setting up new basic-ip connection, updating NJRU-methods to handle also RU, new configuration parameters)
- Formating of data to be written to the RU according to Formatting Chapter.
- Add logging from different components (Check if RU-data shall be written (dynamic- and state-changes for LCS, IO and DMI).
 
\section AdditionalMaterial Additional Material

3NSS011034-01, FIS TCC JRU - Data format of the TCC Juridical Recorder Unit 

*/