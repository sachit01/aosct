namespace ATP::IO
{

/**
\if AsMainPage
\mainpage BTM Handler Component Specification
@anchor bh
\endif

\ifnot AsMainPage
\class AbstractBTMHandler
\endif

\section Purpose Purpose
This document specifies the software design of the AbstractBTMHandler class, the core part of the BTM Handler component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
The BTM Handler component acts as an interface between the BTM and the ATP software.
BTM Handler communicates with the BTM via the OPC and this communication is implemented in the SplHandler class.
SPL Handler is a part of BTM Handler and it uses the SPL Library. The API to the SPL Library is described in
[ITF_SPL] and how to use this library is described in [INST_SPL]. For the OPC communication, [IFS_OPC] can be referred.

In short, BTM Handler commands the BTM and supervises its status. It receives
balise information from the BTM and forwards this information to the Decode component.

AbstractBTMHandler class provides the following functionality:
- Configure the BTM using the \ref btm_command_message.
- Manage turning on/off the BTM Antenna.
- Perform and supervise that test is run, and supervise the result of the test.
- Receiving the correlation between MVB time and VFW time from OPC.
- Converting the time from MVB reference to VFW reference and vice versa.
- Monitoring the BTM status from the received messages.
- Sending out the odometer values to the BTM.
- Receive BTM telegrams, unpack the received message and verify their validity.

\subsection DeploymentDiagram Deployment Diagram

@image html abstract_btm_handler_atp_opc_btm.png "Communication between ATP, OPC and BTM"
@image latex abstract_btm_handler_atp_opc_btm.png "Communication between ATP, OPC and BTM"

\subsection Dependencies Dependencies
BTM Handler class depends on the following components:

+ DMI Handler: for handling user interaction regarding BTM routine test
+ Loco IO: for detecting if the direction control is in neutral
+ Mode Control: for retrieving current mode, active cabin and more
+ Odometry: for generating the BTM odometry message
+ Position: for retrieving current position

Other components have dependencies to this component because they use its public types
and methods, see \ref AbstractBTMHandler Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization

\subsubsection preInit preInit()
The \ref AbstractBTMHandler::preInit function calls the \ref SplHandler::preInit
function, in order to initialize the communication channels (see \ref SPLChannelsUsed) for the communication with OPC.

\subsubsection init init()
The \ref AbstractBTMHandler::init function is responsible for initializing values for
cross compare for BTM Handler. It calls the \ref SplHandler::init function to initialize the SPL
library. It reads the calendar time when the last routine test was performed from the runtime
configuration parameters.

\subsection ModeDependentOperation Mode dependent operation
The BTM Handler component is scheduled in all modes but it is inactive in certain modes, see \ref AntennaPowerStateMachine.

\subsection Scheduling Scheduling
The core component has two functions (inherited from the base-class IOComponent) namely \ref AbstractBTMHandler::runIn
and \ref AbstractBTMHandler::runOut, called in each execution cycle.

\subsubsection runIn runIn()
Firstly, it is checked if there is any non handled balises, then BTM supervision failure is reported.
BTM Handler changes the btm service state to 'Service Failed' and leads to safety halt event.
The runIn() function is responsible for calling \ref SplHandler::runIn.

Once \ref SplHandler establishes a connection with OPC it starts supervising the state of this connection.
If no messages are received for a defined number of cycles, a Safety Halt event is raised.

The function \ref SplHandler::runIn reads incoming messages from OPC and handle them appropriately:
balise telegrams are passed to \ref processTelegramMessage and status telegrams to \ref processStatusMessage.

\subsubsection runOut runOut()
Supervision of BTM is performed when SPL Handler is connected and OPC system is running, firstly, the runOut() function updates the \ref AntennaPowerStateMachine and the \ref RoutineTestStateMachine.
Then it prepares the messages to be sent to OPC for that it sends \ref btm_command_message. Finally, it calls \ref SplHandler::runOut in order to actually
send the messages to OPC.

\subsection OPCMessageHandling OPC Message Handling

\subsubsection processTelegramMessage Process Telegram Message
AbstractBTMHandler::processTelegramMessage() is called from \ref SplHandler, when OPC system is running. This is 
used to process the BTM telegram message and update the telegram list after processing.

processTelegramMessage() calls \ref BtmTelegram_unpack to unpack if valid telegram is received and then verify the telegram. 
Errors that can not be recovered will enter Safety Halt.

\subsubsection BtmTelegram_unpack Telegram Unpack
The BTM telegrams are unpacked by calling the unpack function in the BtmTelegram class. It will unpack the BtmTgm2BtmH
described in [OPC_IFS] from the 5 ports and check if the data is valid on all ports. It will also check that all ports have the
telegram with same sequence number.

The lobe begin and the lobe end time stamps shall be compensated by the MVB Reference Time Offset that is received from
the OPC and stored in BTM Handler.

\subsubsection processStatusMessage Process Status Message
AbstractBTMHandler::processStatusMessage() is called from \ref SplHandler, when the message BtmStatus2BtmH in [OPC_IFS] from the OPC is received.
This is used to process the BTM Status message if valid data is received and update the member variables after processing.
If invalid data is received then error is occurred. It also handles Status messages where it reports if any error is received like an incorrect update of the BSA counter,
Balise service availability, etc. Errors that can not be recovered will enter Safety Halt.

\subsubsection btm_command_message BTM Command Message
BTM Command Message represents a command  BtmH2Btm in [COD_IFS] which will be sent to the BTM. BTM Command message is used to perform preliminary availability test.
Moreover, it is used to control the interface 'K' (Activate IF K, IF K BTM ID), handles Tele-powering (ON/OFF), enables type of telegram 
formats. It is also used to perform a routine test and the selection of BTM options (operating modes of the BTM).

\subsubsection BTM_packet BTM Packet
The only packet that exists on the application level between the Odometer System and the BTM (Balise Transmission
Function) is the 'BTM Packet'. The 'BTM Packet' OdoH2Btm in [OPC_IFS] is sent periodically to the BTM.
The packet is not used during start-up. The packet is supposed to be sent via the safe MVB.

@image html abstract_btm_handler_btm_packet.png "BTM Packet from Odometer System to Balise Transmission Function"
@image latex abstract_btm_handler_btm_packet.png "BTM Packet from Odometer System to Balise Transmission Function"


\subsection MvbTimeSynchronization MVB Time Synchronization
This packet is unpacked from the SplHandler class.
The packet TigrisOffset is received from the TigrisOffset2 VFW-channel. The version number of TimeSyncServer is checked
with that of TigrisOffset version. SPL handler is failed if incorrect version number received.

\subsection BTMServiceState BTM Service States
The BTM service states indicate the service levels of the BTM.

The BTM service state can only change from "Startup" to "Service OK"
if the BTM protocol version and the OPC version are acceptable.
The BTM service state is set to "Service Failed" if there is failure in BTM service.
There is no recovery when the state is set to "Service Failed".

@image html abstract_btm_handler_btm_service_state.png "BTM Service States"
@image latex abstract_btm_handler_btm_service_state.png "BTM Service States"

\subsection AntennaPowerStateMachine Antenna Power State Machine
The antenna power state machine indicates if the BTM Antenna is powered On/Off.
This state machine is updated in the \ref runOut function before generating the \ref btm_command_message.

Verify BTM Telegram format and check BTM Antenna. If Powered Off, then event is logged else check for Balise Service State.

AOS shall read all balises except in the following ATP modes:
 - Power Up
 - Powering Down
 - Sleeping
 - Safety Halt

@image html abstract_btm_handler_BTMAntennaPowerStateMachine.png "Antenna Power States"
@image latex abstract_btm_handler_BTMAntennaPowerStateMachine.png "Antenna Power States"

\subsection RoutineTestStateMachine Routine Test State Machine
To ensure safe balise detection, a successful BTM routine test must be performed before configurable max allowed
calendar time has elapsed since the last successful BTM routine test. If no successful test has been performed
within this time the AOS will apply brakes and perform the BTM routine test. If the BTM Routine test fails,
the AOS will enter ATP mode Safety Halt. A successful test is mandatory upon power on and cabin activation before
the operation can continue.

If the train is standing over a balise after configurable max allowed time has elapsed, the AOS will allow movement
until no balise is detected and perform the BTM routine test.


This state machine is used to manage the routine test of the BTM. The state machine has the following five states:

\subsubsection RoutineTestNeeded RoutineTestNeeded
  This state is to indicate that the Routine test should be performed as soon as possible.
  The state machine will remain in this state until the conditions required to perform the routine tests are met
  OR the time since the last successful test is more than \ref timeout_for_routinetest_mandatrory.

  \paragraph timeout_for_routinetest_needed timeout for routine test needed
  It is a config variable with the 96 hours calendar time maximum value

  \paragraph timeout_for_routinetest_mandatrory timeout for routine test mandatory
  It is a config variable with the 168 hours calendar time maximum value

  \paragraph leave_for_routine_test_started Leave for RoutineTestStarted

  The AOS shall perform a BTM routine test when \ref timeout_for_routinetest_needed
  has elapsed since last successful BTM Routine test AND
   -  At cabin activation OR
   -  The train is in state Idling in ATP mode Normal, Staff Responsible OR Shunting Route.

\subsubsection RoutineTestStarted RoutineTestStarted
In RoutineTestStarted, when BSA (Balise Service Available) has the value
"start up or routine test is in progress", change state to RoutineTestInProgress.

\subsubsection RoutineTestInProgress RoutineTestInProgress
  This is the state when the BTM is performing the Routine Test. BTM Handler will send
  parameter to start the test in \ref btm_command_message. The state machine will monitor the status of the test
  received in the BTM Status message and transition to RoutineTestOK or RoutineTestNeeded depending on whether
  the test passed or failed.

  The AOS shall issue a Standstill event when the BTM routine test is in progress.

  The AOS shall issue a Log event if the BTM test is successful.

  If the BTM routine test fails AND not more than configurable max allowed calendar time has
  elapsed since the last successful BTM Routine the AOS shall:
   - Issue a Log event, AND
   - Enter state RoutineTestNeeded.

  When in ATO mode Manual AND the BTM routine test is in progress, BTM Handler must provide
  the following information (to be shown to the driver):
   - BTM routine test in progress
   - Result of the test (failed/successful)

  If the BTM routine test fails in ATO mode Manual AND not more than configurable max allowed
  calendar time has elapsed since the last successful BTM Routine test, BTM Handler must provide the following
  information (to be shown to the driver):
   - Remaining time until the BTM Routine test is mandatory.

  When more than configurable max allowed calendar time has elapsed since the last successful
  BTM Routine test AND If the BTM routine test fails a Safety Halt event shall be issued.

\subsubsection RoutineTestMandatory RoutineTestMandatory
  This state is reached when it is no longer possible to postpone the Routine Test. This state
  will apply the service brake to stop the train so that the Routine test can progress. However if the antenna is on a
  balise it will allow movement and apply service brake as soon as the antenna has passed the balise. The state
  machine will transition to performing the Routine test as soon as the conditions to perform Routine test are
  fulfilled.

  When more than configurable max allowed calendar time has elapsed since the last successful
  BTM Routine test the AOS shall in all ATP modes except for Safety Halt, Powering Down and Sleeping:
   - If not at standstill issue a Brake event until standstill AND
   - If in ATO mode Manual inform the driver about BTM test timeout AND
   - At standstill if no balise is detected, change state to RoutineTestStarted.

  When \ref timeout_for_routinetest_mandatrory has elapsed since the last successful
  BTM Routine test AND a balise is detected at standstill the AOS shall in all ATP modes except for Safety Halt,
  Powering Down and Sleeping:
   - When in ATO mode Manual inform the driver that the BTM routine test cannot be started due to detection of a balise and the train must be moved to perform the BTM routine test AND
   - When a balise is no longer detected issue a Brake event to standstill, AND
   - At standstill, change state to RoutineTestStarted.

\subsubsection RoutineTestOK RoutineTestOK
  This is the state if the Routine test has been successful. A passed routine test is valid for \ref timeout_for_routinetest_needed before there is the need to perform Routine test again. The system will transition to Routine Test needed after
  \ref timeout_for_routinetest_needed.

  When the system is powered on it will start in the RoutineTestOK state, with the time since last passed test is
  set to the value in the stored runtime parameter.

  When \ref timeout_for_routinetest_needed has elapsed since the last successful BTM Routine the AOS shall go to the state RoutineTestNeeded.

@image html abstract_btm_handler_BTMRoutineTestStateMachine.png "Routine Test States"
@image latex abstract_btm_handler_BTMRoutineTestStateMachine.png "Routine Test States"

\subsubsection OpcVerValidation Validate OPC version
The OPC version values which is configured as major, middle and minor, where minor can have
an ignore value (255), is validated during routine test state machine at the 'Start up' state 
of balise service. Balise service state is set to 'Service OK' once the OPC version is validated
correctly whereas balise service state is 'Service Failed' if the OPC version is wrong.

\subsection SPLHStateMachine SplHandler State Machine
The SplHandler state machine is used to manage the SPL and the connection to the OPC. The state machine has the following 6 states [see Fig. SplHandler State Machine]
- Clock Synchronizing: Wait for SPL to report "Clock Synchronized". Once it is reported it will change state to
"Send Clock Sync"
- Wait For Leaving Idle: Wait for the OPC to change its application state from idle. This is done by inspecting
the app status UDP-packet from the OPC, the status should be "Starting" or "Running". When the status has changed from "Idle",
the next state is "Send Clock Sync"
- Send Clock Sync: In order for the OPC to be able to synchronize the time need to send at least 20 clock syncs. Only after that 
 SPL can be connected. When SPL has generated 20 clock sync packets need to do a SPL_Connect and leave this state to "Connecting".
- Connecting: Need to wait for SPL to change the status from "Connecting" to "Connected", then the state is changed to Connected.
- Connected: Need to stay in this state as long as SPL reports the status as "Connected".
- Failed: Need to get here if an error condition in the communication the OPC occurs.

@image html abstract_btm_handler_spl_state_machine.png "SplHandler State Machine"
@image latex abstract_btm_handler_spl_state_machine.png "SplHandler State Machine"


\subsubsection SPLHandlerComm SPL Handler Communication

SPL communicates with the units using SAP-channels. SPL uses two communication channels to the
OPC for SPL-messages, one channel is for the clock synchronization (SAP-32), and one for the data to the OPC (SAP-19).
The messages implemented below are described in [IFS_OPC] and [REF_TIME]. Since there is only communication with the OPC
so a limited part of the functionality of the SPL library is used. The OPC also sends the AppStatus message
in a separate UDP-connection to the dispatcher. The dispatcher will then relay the AppStatus messages
in the VFW-channel DispAppData_To_BTMH_A/B.

\subsubsection ListOfPacketsReceived List of Packets Received by SplHandler
Packet name       | NID_PACKET | SAP |  UDP Port  | Protocol
----------------- | -----------|-----|------------|---------
BtmTgm2BtmH       |  513       | 19  |  50010     | SPL
BtmStatus2BtmH    |  514       | 19  |  50010     | SPL
ATPCULifesign     |  266       | 19  |  50010     | SPL
VersionReply      |  522       | 20  |  50010     | SPL
AppStatus         |  270       | N/A |  50050     | UDP
TigrisOffset      |  300       | N/A |  N/A       | VFW


\subsubsection ListOfPacketsSent List of Packets Sent by SplHandler
Packet name    | NID_PACKET | SAP |  UDP Port  | Protocol
-------------- | -----------|-----|------------|---------
BtmH2Btm       |  256       | 19  |  50011     | SPL
ATPCULifesign  |  266       | 19  |  50011     | SPL
OdoH2Btm       |  267       | 19  |  50011     | SPL
VersionRequest |  268       | 19  |  50011     | SPL
ATPCUService   |  269       | 19  |  50011     | SPL
SPLClockSync   |  N/A       | 32  |  50013     | SPL

\subsubsection SPLChannelsUsed List of VFW-Channels
VFW-Channel name           | SAP | Direction
-------------------------- | ----|----------
BTMH_To_Disp_A/B           | 19  | ATP to OPC
Disp_To_BTMH_A/B           | 19  | OPC to ATP
Opc_Clock_Sync_To_Disp_A/B | 32  | ATP to OPC
DispAppData_To_BTMH_A/B    | N/A | OPC to ATP
TigrisOffset2A/B           | N/A | OPC to ATP

\subsection SPLHInterfaces SplHandler Interface

The application must implement the following functions and classes in order for SPL to work, see [INST_SPL]:
- function _IOgetProfibusInputData for reading incoming SPL telegrams
- function _IOsetProfibusOutputData for writing outgoing SPL telegrams

\subsection wrapperClasses Wrapper Classes

BTM Handler component also includes the below wrapper classes. For more information, [TSF_SPL] can be referred.

\subsubsection bs_wrapper bs_wrapper
This wrapper class defines BS Wrapper functions used by SPL-library.

bs_wrapper emulates the operating system functions from TBSW (Tigris Base Software: a diversified operating system layer).

\subsubsection cc_wrapper cc_wrapper
This wrapper class defines Cross Compare Wrapper functions used by SPL-library.
 - CrossCompare 
 - CrossCompareFolder
 - CrossCompareHandler
 - CrossCompareSPL



\latexonly \newpage \endlatexonly
\section ClassDiagram Class Diagram
In addition to the AbstractBTMHandler class, this component contains the following classes:

+ \ref SplHandler, which is used for the communication with the BTM. SplHandler depends
on the Vital Framework to talk to OPC via the Dispatcher. The OPC then forwards the messages to the BTM.
+ \ref BtmTelegram, which represents a balise message received from the BTM.
+ \ref BtmStatusMessage, which represents a status message received from the BTM.
+ \ref BtmCommandMessage, which represents a command which will be sent to the BTM.

@image html abstract_btm_handler_class_diagram.png "Class Diagram"
@image latex abstract_btm_handler_class_diagram.png "Class Diagram"

\section Diagnostics Diagnostics

\subsection Console Console Commands
The following component-specific console commands are implemented:
- help       : Prints help information about supported commands
- btmTel     : Prints the last received BTM balise telegram
- btmSta     : Prints the last received BTM status telegram
- btmCmd     : Prints the last sent BTM command
- btmInfo    : Prints BTM state information

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
The BTM Handler component is split in core and adaptation parts.
The core part deals with the interface between the OPC and the AOS SW.
The adaptation only contains instantiation and other adaptation-specific details.

\section PreProcessor Pre-Processor Directives
No pre-processor directives are available for this component.

\section Traceability Traceability

\subsection SSRS Functional Requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP Core.

The requirements relevant for this component are:

Req        | Chapter                          | Function
---------- | -------------------------------- | -----------
AOS 209 S  | \ref AntennaPowerStateMachine    | AbstractBTMHandler::runOut()
AOS 656    |  \ref Scheduling                 | AbstractBTMHandler::runIn()
AOS 1198   |  \ref Scheduling                 | AbstractBTMHandler::runIn(), AbstractBTMHandler::processTelegramMessage()
AOS 2509   |  \ref RoutineTestStateMachine    | AbstractBTMHandler::runOut()
AOS 2510   |  \ref RoutineTestStateMachine    | AbstractBTMHandler::runOut()
AOS 2511   |  \ref RoutineTestStateMachine    | AbstractBTMHandler::runOut()
AOS 2513   |  \ref RoutineTestStateMachine    | AbstractBTMHandler::runOut()
AOS 2514   |  \ref RoutineTestStateMachine    | AbstractBTMHandler::runOut()
AOS 2515 S |  \ref RoutineTestStateMachine    | AbstractBTMHandler::runOut()
AOS 2516 S |  \ref RoutineTestStateMachine    | AbstractBTMHandler::runOut()
AOS 2593   |  \ref RoutineTestStateMachine    | AbstractBTMHandler::runOut()
AOS 2625 S |  \ref Scheduling                 | AbstractBTMHandler::runIn()
AOS 2780 S |  \ref RoutineTestStateMachine    | AbstractBTMHandler::init(), AbstractBTMHandler::runOut()


\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP Core.

Only the architectural requirements traced explicitly to this component are included in the table below.
Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

Req          | Chapter                              | Function
------------ | ------------------------------------ | ------------------------------
AOS_AS-47 S  | \ref runIn                           | SplHandler::runIn()
AOS_AS-50 S  | \ref preInit                         | SplHandler::preInit()
AOS_AS-58 S  | \ref runIn                           | SplHandler::readTigrisOffset()
AOS_AS-70 S  | \ref runIn                           | AbstractBTMHandler::runIn()
AOS_AS-71 S  | \ref init                            | SplHandler::getInputFromOpc
AOS_AS-72 S  | \ref init                            | SplHandler::sendOutputToOpc()
AOS_AS-88 S  | \ref init                            | SplHandler::preInit()
AOS_AS-164   | \ref runIn , \ref runOut             | SplHandler::runIn(), SplHandler::runOut()
AOS_AS-358 S | \ref runIn                           | SplHandler::initSplConnection()
AOS_AS-360 S | \ref SPLHandlerComm                  | SplHandler::runIn(), SplHandler::runOut()
AOS_AS-363 S | \ref SPLHandlerComm                  | SplHandler::initSplConnection()
AOS_AS-366 S | Verified by Analysis                 | verified by Analysis
AOS_AS-367 S | \ref runIn                           | AbstractBTMHandler::runIn()
AOS_AS-370 S | \ref runIn                           | AbstractBTMHandler::runIn()
AOS_AS-371 S | \ref runIn                           | AbstractBTMHandler::runIn()
AOS_AS-372 S | \ref processTelegramMessage          | AbstractBTMHandler::processTelegramMessage()
AOS_AS-373 S | \ref runIn                           | AbstractBTMHandler::runIn()
AOS_AS-374 S | \ref SPLHandlerComm                  | SplHandler::readTigrisOffset()
AOS_AS-411 S | \ref runIn                           | AbstractBTMHandler::processStatusMessage()
AOS_AS-421 S | \ref runIn                           | SplHandler::handleOpcSplConnecting()
AOS_AS-436 S | \ref AntennaPowerStateMachine        | AbstractBTMHandler::processStatusMessage(), AbstractBTMHandler::processTelegramMessage()
AOS_AS-437 S | \ref AntennaPowerStateMachine        | AbstractBTMHandler::processStatusMessage(), AbstractBTMHandler::processTelegramMessage()
AOS_AS-438 S | \ref AntennaPowerStateMachine        | AbstractBTMHandler::runAntennaPowerStateMachine()
AOS_AS-439 S | \ref AntennaPowerStateMachine        | AbstractBTMHandler::runAntennaPowerStateMachine()
AOS_AS-465 S | \ref processStatusMessage            | AbstractBTMHandler::handleBtmStatusMessage()
AOS_AS-466 S | \ref AntennaPowerStateMachine        | AbstractBTMHandler::runAntennaPowerStateMachine()
AOS_AS-477 S | \ref AntennaPowerStateMachine        | AbstractBTMHandler::runAntennaPowerStateMachine()
AOS_AS-617 S | \ref OPCMessageHandling              | SplHandler::handleLifeSign()
AOS_AS-622 S | \ref runIn, \ref runOut              | AbstractBTMHandler::runIn(), AbstractBTMHandler::runOut()
AOS_AS-626 S | \ref runOut                          | AbstractBTMHandler::populateAndSendBtmCommand()
AOS_AS-628 S | \ref runOut                          | SplHandler::runOut()
AOS_AS-629 S | \ref OPCMessageHandling              | SplHandler::packLifeSignPacket()
AOS_AS-630 S | \ref runOut                          | SplHandler::runOut()
AOS_AS-631 S | \ref OPCMessageHandling              | SplHandler::sendOutputToOpc()
AOS_AS-633 S | \ref runOut                          | AbstractBTMHandler::populateAndSendOdoCommand()
AOS_AS-654 S | \ref runOut                          | BtmCommandMessage::pack
AOS_AS-655 S | \ref processTelegramMessage          | AbstractBTMHandler::processTelegramMessage()
AOS_AS-657 S | \ref processTelegramMessage          | AbstractBTMHandler::processTelegramMessage()
AOS_AS-658 S | \ref runOut                          | AbstractBTMHandler::populateAndSendBtmCommand()
AOS_AS-659 S | \ref processTelegramMessage          | AbstractBTMHandler::processTelegramMessage()
AOS_AS-660 S | \ref RoutineTestStateMachine         | BtmStatusMessage::isInitialized() 

*/

}
