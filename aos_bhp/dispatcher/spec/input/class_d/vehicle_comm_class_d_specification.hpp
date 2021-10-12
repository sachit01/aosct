/** 
\if AsMainPage
\mainpage Vehicle Comm Class D Messaging Specification
@anchor vc
\endif

\ifnot AsMainPage
\class Class D Messaging Specification
\endif

\section VersionLog Version Log

Version | Date       | Description                   | Signature
------- | --------   | ----------------------------- | ---------
1.0     | 27-12-2016 | Initial                       | adgupta

\section Summary Executive Summary

\subsection Purpose Purpose
This document specifies the design for the Class D Messaging to be used by ATP/ATO components.
Sufficient details are provided for the module to be implemented.

\subsection IntendedAudience Intended Audience
This system design specification is intended to be read by:
-   SW-Designers 
-   Developers
-   Component Testers
-   Sub System Testers
-   Validators
-   Assessors
-   Verifiers

The reader of this document is assumed to be knowledgeable up to the state of the art of the following items:
-   Software Development Skills
-   The C/C++ programming language
-   Common SW data structures & algorithms

All of these items may be used without further references to their definitions

\subsection HowToRead How to read this document
For a reader who just wants to get a first overview of the AOS_IF150 ATP SW environment it should be enough to read the chapter "Overview".
This chapter shall be consumed anyway by every reader since it might contain additional hints on where important information is to be found.
 
\subsection References References
\subsubsection ProjectDocuments Project Documents
Ref        | Document Name                            | Document Id       | Version
---------- | ---------------------------------------- | ----------------- | ---------
S-9356     | Class D Messaging Specification          | S-9356            | -
S-9354     | Edge Message Protocol Specification      | S-9354            | -
-          | Edge Message Protocol Design             | TBD               | TBD

\subsection DefinitionsAndABbreviations Definitions and Abbreviations
\subsubsection Definitions Definitions 
Term           | Definition                                   
-------------- | ------------------------------------------------------------------
EMP            | Edge Message Protocol
TCP            | Transmission Control Protocol
IP             | Internet Protocol
LCS            | Locomotive Control System
VFW            | CoHP-2 Vital Frame Work

\subsubsection Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
CRC         | Cyclic Redundancy Checksum

\subsection EN50128 EN50128 Requirements
ID         | Technique/Measure           | Justification
---------- | --------------------------- | -----------------------------------------------
A4.2       | Modelling                    | Class diagrams and flowcharts
A4.3       | Structured methodology      | ...
A4.4       | Modular Approach            | ...
A4.5       | Components                  | ...

\subsection Deviations Deviations and questions
- Maximum length of the sending and receiving data supported should be decided.
- The keep-alive messages are not implemented however, there may be a need to implement this in future.
- Data Acks for the received messages are not used as of now however, there may be a need to implement this in future.
- When using the ClassD protocol for platform not having VFW, the low level calls(which are handled by VFW) should be ported for that platform to be usable.

\latexonly \newpage \endlatexonly
\section Overview Overview
The Class D messaging protocol is a messaging protocol that is to be used to communicate with the EMD locomotives(LCS).
This is intended for applications requiring reliable point-to-point message delivery and uses TCP/IP connections internally for communication.
Any communication to be done between ATP and LCS should use the Class D messaging. The Class D messages for ATP need to have messages in the EMP message envelop.
Refer EMP Design(TBD) for details. The purpose for Class D messaging is to initiate and maintain/upkeep of communication link, transmission/receiving of messages 
via the established link and safe termination of communication link. Dispatcher should use APIs provided by the Class D.

\subsection GeneralFunctionality General Functionality
The functionality of Class D messaging can be classified into:-
- Link Initiation - It shall support ability to configure one or more communication links which can be Host or Client. 
This is done by receiving connection details like Port Number, IP address, buffer size, etc.\n
- Message Transmission/Reception - It shall support transmission and reception of messages via underlying TCP/IP protocol. 
Packing and parsing of data with Class D header and footer and verification of the same and reporting of Error logs.\n
- Link management - It shall maintain and upkeep all the established connections.\n
- Link monitoring - It shall monitor each connections for link losses via TCP/IP keep-alive messages. There is no explicit keep-alive message implementation yet in this protocol.
It will try to re-connect in case of connection loss and log errors.

\subsection ObjectDiagram Object Diagram

\subsection Dependencies Dependencies

- Dispatcher
  + Dispatcher in ATP will depend on Class D messaging to send/receive data to/from the LCS. It will use APIs provided by Class D for any communication. ATP components in turn will use VFW
  Channels to send message to Dispatcher.
- ATO
  + ATO in future may directly use Class D messaging to send/receive data to/from the LCS. As of now, it is proposed that all the communication by ATO to LCS be done via ATP.
- Vital Framework
  + When used in ATP(only), this should use vital framework APIs for low level calls like network to byte conversion and vice-versa, timers, time, etc.
- TCP/IP protocol
  + Class D messaging depends on the TCP/IP protocol to perform all the network interface activities link socket creation, message send/receive, closing of socket, etc.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection BehaviouralDescription Behavioural description
\subsubsection Initialisation Initialisation
Initialization of the Class D connection will be done by calling the initialize() function with the parameters having IP address, port Number, connection Type, etc.
The initialize() function takes the input parameters and updates the base classes' variables after validating them to be correct. It also updates the connection Status to
StatusInit to enter into the connection handling/maintenance cycle.
@image html ClassDInitialize.png
@image latex ClassDInitialize.png

\subsubsection ModeDependentOperation Mode dependent operation
Class D is not dependent on any ATP mode

\subsubsection Wrapping Wrapping of Low level calls/structures
- The low levels/system level calls like TCP/IP calls related calls(socket, listen, write, etc.) and VFW related calls should be wrapped around another function call while
implementing the protocol so that it can be easily ported to another platform e.g.-When used with the LCS simulator.
- The platform dependent structures like Time structures, Timer Structures should be wrapped around a wrapper structure and to be used by wrapper functions for implementation.

\subsubsection Reconnect Re-connection Logic
There will be 3 levels of re-connection attempt to be done by Class D client when it cannot establish a connection.
- It will allow the connection attempt Timeout(configurable) amount of time to be elapsed while making a single connection to establish a connection.
- It will wait for connection delay(configurable) amount of time before re-attempting to establish a connection.
- It will retry the connection retry limit(configurable) number of times to retry connecting to the same created socket.
- It will retry the reconnection limit(configurable) number of times by closing and starting a new socket connection before giving up on connection establishment.

\subsubsection Scheduling Scheduling
The Class D has one function that must be called each execution-cycle:

\paragraph execute execute()
The execute() function checks for the connection status and calls the corresponding handle functions for Client and Host connection nodes.
Host - handleHostConnection()\n
Client - handleClientConnection()

@image html ClassDExecute.png
@image latex ClassDExecute.png

\paragraph handleHostConnection handleHostConnection()
It will handle the Host connection including the re-connection retry logic for Host node. It maintains a state machine variable connectionStatus to keep 
maintenance and upkeep of the TCP connection.

@image html ClassDHandleHostConnection.png
@image latex ClassDHandleHostConnection.png

\paragraph handleClientConnection handleClientConnection()
It will handle the Client connection including the re-connection retry logic for Client node. It maintains a state machine variable connectionStatus to keep
maintenance and upkeep of the TCP connection.

@image html ClassDHandleClientConnection.png
@image latex ClassDHandleClientConnection.png

\paragraph send send()
It will pack the message to be sent with the ClassD Header and Footer and send it over TCP connection.

@image html ClassDSend.png
@image latex ClassDSend.png

\paragraph packMessage pack message()
It will pack the data to be sent adding the ClassD header and footer having appropriate commId, message number along with Endianness correction.

@image html ClassDPackMessage.png
@image latex ClassDPackMessage.png

\paragraph receive receive()
It will read data from socket if connected, parse the data to have correct ClassD header and footer.

@image html ClassDReceive.png
@image latex ClassDReceive.png

\paragraph parseMessage parse message()
This will parse the data received from the network, validates the message format and logs appropriate error in case of any error.

@image html ClassDParseMessage.png
@image latex ClassDParseMessage.png


\subsection ClassDiagram Class Diagram

@image html ClassDClassDiagram.png
@image latex ClassDClassDiagram.png

\subsection ExternalInterfaces External Interfaces

See chapter 2.2 "Public Member Functions"


\subsection Diagnostics Diagnostics

\subsubsection ErrorReporting Error Reporting and Handling
The Error is Logged and reported to the corresponding calling component as follows:
- The calling component should create an ErrorLog structure having variables to log:
  + Time
  + Linker Identifiers - Client and Host socket Id and Host IP address
  + Error Type
  + Recovery Action
- The execute() function will return boolean false in case of any error is encountered while executing.
- Call getErrorLog() function to get the Error logged while executing with all the details.

\paragraph Analyze Analyze
Using of Analyzer is not planned as of now.

\subsection FaultReporting Fault Reporting
Event handling is not applicable for the Class D protocol all the error reporting are done via Error Log described in Section 'Error Reporting and Handling'.

\subsection TestFeatures Test Features
Not 

\subsection Miscellaneous Miscellaneous functions
\subsubsection ConvertOrder Convert to network order
The Vfw-functions declared in vfw_buffer.h shall be used to convert to/from network-byte order before an incoming message is processed and before an outgoing message is sent.

Use
- vfwInitBuffer() to initialise a buffer for use.
- vfwPut*() to write values to buffer and convert to network byte order
- vfwGet*() to read values from buffer and convert to host byte order


\subsubsection CrossCompare Cross-compare
Cross compare is not used. Here

\section PreProcessor Pre-Processor Directives

No pre-processor directives available for Class D.

\section DesignSafety Design Safety

\subsection SIR SIR Requirement tracing

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
-          | -                                     | -


\section DesignReliability Design Reliability

\section Traceability Traceability

\subsection Requirements Requirements
The applicable requirements for this component is defined in [SYSREQ] and [SIR]. 
However, the [SWAS] adds constraints on the design and implementation of this component.

Each applicable Safety Integrity Requirement is addressed in the Design Safety chapter.

The requirements relevant for this component:

Req        | CTT   | Short requirement description       | Chapter         | Function       
---------- | ----- | ----------------------------------- | --------------- | --------------  
12345     | 12345  | Abcdefghijklmn..                    | \ref -          | -         

*/

