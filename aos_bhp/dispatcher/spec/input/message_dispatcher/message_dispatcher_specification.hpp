/**
\if AsMainPage
\mainpage MessageDispatcher
@anchor md
\endif

\ifnot AsMainPage
\class Template
\endif

\section VersionLog Version Log

Version | Date       | Description                     | Signature
------- | --------   | --------------------------------| ---------
1.0     | 2016-12-06 | created                         | spandita
1.1     | 2017-01-03 | updated with preInit()          | spandita
1.2     | 2017-02-06 | Updated with Class D            | spandita

\section Summary Executive Summary

\subsection Purpose Purpose
This document specifies the software design for the MessageDispatcher component.
Sufficient details provided for the module to be implemented.

\subsection IntendedAudience Intended Audience
This system design specification is intended to be read by:
-   SW-Designers
-   Developers
-   Component Testers
-   Sub System Testers
-   Validators
-   Assessors
-   Verifiers

The reader of this document is assumed to be knowledgeable up to the state of the art of the
following items:
-   Software Development Skills
-   The C/C++ programming language
-   Common SW data structures & algorithms

All of these items may be used without further references to their definitions

\subsection HowToRead How to read this document
For a reader who just wants to get a first overview of the AOS_IF150 ATP SW environment it should
be enough to read the chapter "Overview". This chapter shall be consumed anyway by every reader
since it might contain additional hints on where important information is to be found.

\subsection References References
\subsubsection ProjectDocuments Project Documents
Ref        | Document Name                            | Document Id       | Version
---------- | ---------------------------------------- | ----------------- | ---------
[SWAS]     | ATP Software Architecture Specification  | 3NSS?             | 1.0
[SYSREQ]   | ATP (Sub)System Requirements             | DOORS             | Baseline ?
[INFO]     | Handover of Dispatcher                   |  -                | 1.0
S-9356     | Class D Messaging Specification          | S-9356            | -
[INFO]     | SCDS_Vehicle_Com                         | -                 | 1.3

\subsection DefinitionsAndABbreviations Definitions and Abbreviations
\subsubsection Definitions Definitions
Term           | Definition
-------------- | ------------------------------------------------------------------
x              | y

\subsubsection Abbreviations Abbreviations
Abbreviation   | Definition
-------------- | ------------------------------------------------------------------
ATP            | Automatic Train Protection
TCC            | Train Control Center
DMI            | Driver Machine Interface
AOS            | ATP On-board System
VFW            | CoHP-2 Vital Framework
OPC            | OnBoard Protocol Converter
CPU            | Central Processing Unit
IP             | Internet Protocol
LCS            | Locomotive Control System
LIG            | Locomotive Interface Gateway

\subsection EN50128 EN50128 Requirements
ID         | Technique/Measure           | Justification
---------- | --------------------------- | -----------------------------------------------
A4.2       | Modeling                    | Class diagrams and flowcharts
A4.3       | Structured methodology      | ...
A4.4       | Modular Approach            | ...
A4.5       | Components                  | ...


\subsection Deviations Deviations and questions

#####Deviation 
First phase of implementation of message dispatcher component will be done on following assumptions: \n
-Dispatcher will receive full message from ATP A via single VFW channel as in first phase their will not be any packing mechanism
in message dispatcher.
- Below design will remain same as it is.
- Receiving of message from OPC  will not get implemented in this phase as it is not yet clarified.
- Current implementation shall be based on one DMI channel,one TCC channel,one channel for VIOHSim to AOSPC and one channel for both AOSPC To CodSim and AOSPC to VIOHSim and
to LCS/LCS simulator.
- Only one message will be received from ATP A & B at one time.

\latexonly \newpage \endlatexonly
\section Overview Overview
MessageDispatcher is component in dispatcher that handles the messages from ATP A and ATP B to external interface and vice versa.

\subsection GeneralFunctionality General Functionality
The MessageDispatcher component provides the following functionality:
- Establish IP connections with external interfaces
- Receive incoming messages from external interfaces like TCC, DMI, AOSPC,LCS and forward them via VFW channel to the ATP applications which is running on CPU A and CPU B\n
- The messages from external interfaces are received via IP connections which is administrated by an adaptation of the BasicIP component
- The receiving components in the ATP receives the messages via channels synchronized by the Vital Framework (VFW)
in order to make sure that the messages will arrive in the same time on both CPU A /  B
- Receive outgoing messages from the  ATP A and B via VFW channel and send together to external interface like DMI/TCC/AOSPC/LCS
(first half of message will be received from CPU-A and rest from CPU-B)
- For communication with LCS, the Message Dispatcher makes use of Class D protocol to pack and un-pack the data.
- Functionality of Class D available in the Section \ref OverviewClassD .
\subsection Dependencies Dependencies

- Message from external components shall be received/transmitted via basic IP component
- Message from ATP A/B shall be received via VFW Channels
- Console component should be available for console call implementation
- Log handler component should be available for Logging into N-JRU/JRU
- Config should be available for getting the Default IP and port number values etc
- VFW need to be initialized
- Class D should be initialized in the init() part of Message Dispatcher.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection BehaviouralDescription Behavioural description
\subsubsection Initialization Initialization

\paragraph preInit preInit()

The preInit() function will be responsible for setting up GSP-2 VFW framework related initialization.
It includes
- Initialization of VFW channels for writing the messages from dispatcher to ATP A & B.
- Initialization of VFW channels for reading the messages from ATP A & B to dispatcher
- Register the callbacks related to input channels for messages.

\paragraph init init()

The init() function will be responsible for initializing the following parameters
- Read the config parameters like IP address and port numbers
- Initialize the IP connections used for communication between dispatcher and external interfaces(TCC, DMI, AOSPC and LCS)\n

@image html messageDispatcherInit.png
@image latex messageDispatcherInit.png

\subsubsection ModeDependentOperation Mode dependent operation

\subsubsection Scheduling Scheduling

Message Dispatcher has two function runIn() and runOut() that must be called in  each execution cycle.

\paragraph runIn runIn()

This function is responsible for receiving the messages from external component and transmit the same received messages to ATP A and ATP B.\n
To achieve the above functionality, following steps has been carried out
- It will iterate all the connection ID(which will be defined in basic IP component) and depending upon the connection type it will check for 
connection status like connection is active or not of each connection ID
- If the connection status is active(connected) it will read all the messages from respective Connection id and write in the inputBuffer buffer
- In case of LCS connection id, it will first parse the received LCS message by using class D API
- If any error occurs while parsing the LCS messages discard the message and log the error
- Call writeToVfwChannel(Connection id) function

@image html messageDispatcherRunIn.png
@image latex messageDispatcherRunIn.png

##### writeToVfwChannel
Depending upon the type of connection ID it will write the messages on respective channel A and B
Refer below diagram for more details

@image html messageDispatcherVfw.png
@image latex messageDispatcherVfw.png

\paragraph event Event handling

- Input Message from ATP A & B shall be received via VFW event by using  VFW_SyncChannelHandler API
- Once the message is available on registered channel,the respective event will get triggered
- Once the complete message is received (first half from ATP A and second half from ATP B) write the complete message to respective connection Id of external interfaces
(this will be done in respective event handler of ATP A or ATP B)
- There shall be exception for LIG/LCS messages (class D messages) once the complete message is received, it shall first call packMessage() API of class D to pack Class D header 
and footer before sending to respective connection Id of external interface.
- Refer table \ref Events and below diagram for more details regarding the events handler


\paragraph  Events Event handler with description

Event Handler Name          | Description            
----------------------------|------------------------------------------------------
handleMessageToTcc1FromATPA | Event handler for Radio channel1 message from ATP A to TCC1
handleMessageToTcc2FromATPA | Event handler for Radio channel2 message from ATP A to TCC2
handleMessageToTcc1FromATPB | Event handler for Radio channel1 message from ATP B to TCC1
handleMessageToTcc2FromATPB | Event handler for Radio channel2 message from ATP B to TCC2
handleMessageToDmi1FromATPA | Event handler for DMI channel1 message from ATP A to DMI1
handleMessageToDmi2FromATPA | Event handler for DMI channel2 message from ATP A to DMI2
handleMessageToDmi1FromATPB | Event handler for DMI channel1 message from ATP B to DMI1
handleMessageToDmi2FromATPB | Event handler for DMI channel2 message from ATP B to DMI2
handleMessageToViohFromATPA | Event handler for VIOH channel message from ATP A to VIOH
handleMessageToViohFromATPB | Event handler for VIOH channel message from ATP B to VIOH
handleMessageToLigFromATPA  | Event handler for LIG channel message from ATP A to LIG
handleMessageToLigFromATPB  | Event handler for LIG channel message from ATP B to LIG


@image html messageDispatcherAEvent.png
@image latex messageDispatcherAEvent.png
@image html messageDispatcherBEvent.png
@image latex messageDispatcherBEvent.png

\subsubsection VFWchannels VFW channels with direction details

VFW channels Name           | Direction         
----------------------------|-------------------
Disp_To_CODSim_A            |Disp to ATP A      
Disp_To_CODSim_B            |Disp to ATP B      
Disp_To_VIOHSim_A           |Disp to ATP A      
Disp_To_VIOHSim_B           |Disp to ATP B      
VIOHSim_To_Disp_A           |ATP A to Disp      
VIOHSim_To_Disp_B           |ATP B to Disp      
Disp_To_RadioChannel1_A     |Disp to ATP A      
Disp_To_RadioChannel1_B     |Disp to ATP B      
Disp_To_RadioChannel2_A     |Disp to ATP A      
Disp_To_RadioChannel2_B     |Disp to ATP B      
RadioChannel1_To_ Disp_A    |ATP A to Disp      
RadioChannel1_To_ Disp_B    |ATP B to Disp      
RadioChannel2_To_ Disp_A    |ATP A to Disp      
RadioChannel2_To_ Disp_B    |ATP B to Disp      
Disp_To_MMIChannel1_A       |Disp to ATP A      
Disp_To_MMIChannel1_B       |Disp to ATP B      
Disp_To_MMIChannel2_A       |Disp to ATP A      
Disp_To_MMIChannel2_B       |Disp to ATP B      
MMIChannel1_To_ Disp_A      |ATP A to Disp      
MMIChannel1_To_ Disp_B      |ATP B to Disp      
MMIChannel2_To_ Disp_A      |ATP A to Disp      
MMIChannel2_To_ Disp_B      |ATP B to Disp      
Disp_To_LIGCom_A            |Disp to ATP A      
Disp_To_LIGCom_B            |Disp to ATP B      
LIGCom _To_ Disp _A         |ATP A to Disp      
LIGCom _To_ Disp _B         |ATP B to Disp      


Note: -OPC related channels are under discussion \n
      -Communication to NJRU and BDS is not via the Dispatcher \n
      -Channels related to VIOH and COD should get removed in VSIM 

\subsection ClassDiagram Class Diagram

@image html messageDispatcherclassdiagram.png
@image latex messageDispatcherclassdiagram.png


\subsection ExternalInterfaces External Interfaces
See chapter "Public Member Functions"

\subsection Diagnostics Diagnostics
\subsubsection Trace Trace-commands
\subsubsection Console Console-commands
##### "msgDisp"
it should display the following parameter of each message received from external interface to ATP or vice versa.
- Source Name\n
- Destination name\n
- No of bytes transferred\n

\paragraph Analyze Analyze
\subsection FaultReporting Fault Reporting
\subsection TestFeatures Test Features
\subsection Miscellaneous Miscellaneous functions

\latexonly \newpage \endlatexonly
\subsection Deviations Deviations and questions
#####Deviation 
- Only host connection is supported for Class D connection as of now however, there may be a need to implement Client connection in future.
- Maximum length of the sending and receiving data supported should be decided.
- The keep-alive messages are not implemented however, there may be a need to implement this in future.
- Data Acks for the received messages are not used as of now however, there may be a need to implement this in future.
- When using the ClassD protocol for platforms not having VFW, the low level calls(which are handled by VFW) should be ported for that platform to be usable.
- Reconnection logic can be modified later after feedback from EMD.

\section OverviewClassD Overview
The Class D messaging protocol is a messaging protocol that is to be used to communicate with the EMD locomotives(LCS).
This is intended for applications requiring reliable point-to-point message delivery and uses TCP/IP connections internally for communication.
Any communication to be done between ATP and LCS should use the Class D messaging. The Class D messages for ATP need to have messages in the EMP message envelope.
Refer Vehicle Com component Functional Design EMP Message Protocol for details. The purpose for Class D messaging is to initiate and maintain/upkeep of communication link, 
transmission/receiving of messages via the established link and safe termination of communication link. Dispatcher should use APIs provided by the Class D.

\subsection GeneralFunctionalityClassD General Functionality
The functionality of Class D messaging can be classified into:-
- Link Initiation - It shall support ability to configure one communication link which should be the Host.
This is done by receiving connection details like Port Number, IP address, buffer size, etc.\n
- Message Transmission/Reception - It shall support transmission and reception of messages via underlying TCP/IP protocol.
Packing and parsing of data with Class D header and footer and verification of the same and reporting of Error.\n
- Link management - It shall maintain and upkeep all the established connections.
- Link monitoring - It shall monitor each connections for link losses via TCP/IP keep-alive messages. There is no explicit keep-alive message implementation yet in this protocol.


\subsection DependenciesClassD Dependencies

- ATO
+ ATO in future may directly use Class D messaging to send/receive data to/from the LCS. As of now, it is proposed that all the communication by ATO to LCS be done via ATP.
- Vital Framework
+ When used in ATP(only), this should use vital framework APIs for low level calls like network to byte conversion and vice-versa, timers, time, etc.
- TCP/IP protocol
+ Class D messaging depends on the TCP/IP protocol to perform all the network interface activities link socket creation, message send/receive, closing of socket, etc.

\latexonly \newpage \endlatexonly
\section FunctionalDesignClassD Functional Design
\subsection BehaviouralDescription Behavioural description
\subsubsection Initialisation Initialisation
Initialization of the Class D connection in ATP will be done in init() of message dispatcher via Basic IP connection 

\paragraph parseMessage parse message()
This will parse the data received from the network, validates the message format and logs appropriate error in case of any error.

@image html ClassDParseMessage.png
@image latex ClassDParseMessage.png

\paragraph packMessage packMessage()
This will pack the message body with class D header and footer

@image html ClassDPackMessage.png
@image latex ClassDPackMessage.png

\subsection ClassDiagram Class Diagram

@image html ClassDClassDiagram.png
@image latex ClassDClassDiagram.png

\subsection ConnectionSequenceDiagram Connection Sequence Diagram

@image html ClassDConnectionReadSequence.png
@image latex ClassDConnectionReadSequence.png

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component

\section DesignSafety Design Safety

\subsection SIR SIR Requirement tracing

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
SIR-       | x                                       |y


\section DesignReliability Design Reliability
The failure modes of the design and their effect on operation and performance.
\section Traceability Traceability
\subsection Requirements Requirements
The applicable requirements for this component is defined in [SYSREQ] and [SIR].
However, the [SWAS] adds constraints on the design and implementation of this component.

Each applicable Safety Integrity Requirement is addressed in the Design Safety chapter.

The requirements relevant for this component:

Req        | CTT   | Short requirement description       | Chapter         | Function
---------- | ----- | ----------------------------------- | --------------- | --------------
12345     | 12345 | Abcdefghijklmn..             | \ref runIn      | runIn()

*/

