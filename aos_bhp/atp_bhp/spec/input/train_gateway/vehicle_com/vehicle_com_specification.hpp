namespace ATP::TG
{
  /**
  \if AsMainPage
  \mainpage Vehicle Com Component Specification (Adaptation)
  @anchor vc
  \endif

  \ifnot AsMainPage
  \class VehicleCom
  \endif

  \section Purpose Purpose
  This document specifies the software design for the Adaptation part of the Vehicle Com component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview
  The Vehicle Com component is responsible for interfacing the Locomotive Control System (LCS).

  \subsection GeneralFunctionality General Functionality
  The Adaptation part for Vehicle Com component will include the interface, logic and protocol handling  with LCS  by using Locomotive Interface Gateway (LIG).
  All the communication to the locomotive applications or interface needs to follow Inter-operable Train Control (ITC) and EMP protocol is used to follow Interoperability.
  Vehicle Com component provides all this functionality to communicate with  interfaces of Locomotive.
  This defines the packet structure that should be used to send/receive a message while communicating with the EMD locomotives (LCS).
  This is intended for applications requiring reliable point-to-point message delivery and uses Class D connection protocol for internal communication.
  Any communication to be done between ATP and LCS should use the EMP messaging protocol. The class D messaging protocol is used to send/receive ATP messages should have EMP message envelop.
  The Vehicle Com component should maintain all the necessary details (Message type, Id, etc.) to be able to make the EMP header/footer and validate the received EMP messages.
  The EMP message protocol is placed below ATP application and above Class D protocol in the layered stack model.


  The protocol layers are as follows:
  - Application Layer Messages between AOS and LIG ref [FFFIS-LCS]
  - EMP, Primarily a message envelope with CRC (Edge Message Protocol S-9354) ref [FFFIS-LCS].
  - Class-D, Establishment of connections, message transport and error handling (Class D Messaging Specification S-9356) ref [FFFIS-LCS].
  - TCP/IP

  The EMP messaging protocol has following major functionalities:
  - Reliable point-to-point communication by the use of appropriate message type, message versions, etc. in the header of the EMP message packet.
  - Data integrity for the message packet via. using of CRC as message footer.

  Vehicle Com component also manages all the messages to and from the Locomotive. It handles the messages as defined in FFFIS AOS-LCS.
  By validating an EMP message, the Vehicle Com component will also check any discrepancies with received messages to provide a reliable communication.
  It is also responsible for various statuses as well, like brake type, ECPB Status, ECPB modes, percentage operative brakes and etc.
  This component provides various interfaces which provides the information received from LCS/LIG/Locomotive
  

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  The Vehicle Com component has dependencies with following components:-
  - Mode Control: Fetching information about current mode, travel direction and configuration type.
  - Train Setup: Setting of temporary train setup.
  - LocoIO: Fetch ATO mode.
  - Log Handler: For logging the information on RU/NJRU.
  - Cross Compare: For cross comparing the data.
  - Position: To get the trailing and leading position.
  - Tracks: To fetch information for tracks.
  - Message Handler: To get the Qsetup.
  - Target Calculation: To get the warning curve message.
  - Targets: To get primary targets for RCL Information message.

  Other components have dependencies to this component because they use its public types
  and functions, see \ref VehicleCom Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design
  \subsection Initialization Initialization
  The VehicleCom initializes using constructor, initialization and pre-initialization functions.
  However, Constructor of vehicleCom initializes all internal variables of VehicleCom. Initialization and pre-initialization functions of VehicleCom
  initialize LCSMessageHandler class.

  The LCSMessageHandler is responsible to send/receive messages to LCS. Where AbstractLCSMessageIn is the base class of parsers for incoming LCS messages. It is responsible to provide common functions to the parser classes(i.e AbstractLCSMessageIn).
  Similarly, AbstractLCSMessageOut is the base class of creators for outgoing LCS messages. It is responsible to provide common functions to the creator classes(i.e LCSMessageOutStatus).

  Initialization of EMP messaging
  - There is no initialization for the EmpMsg in EMP messaging. Vehicle Com component will directly call corresponding
  functions to add header and footer and also the validation of incoming messages for correct EMP header and footer.

  \subsection ModeDependentOperation Mode dependent operation
  Vehicle Com component is independent of ATP mode.

  \subsection Scheduling Scheduling
  The VehicleCom has two functions(runIn and runOut), these must be called each execution-cycle:


  \subsubsection runIn runIn()

  The execution flow for receiving a EMP message from LCS is as follows:
  - LCS-message is received from the LCS and after parsing of TCP/IP and Class-D it is sent through VFWChannel to ATP.
  - The LCSMessageHandler reads EMP message from VFWChannel and parses the EMPHeader using EmpMsg.

  The sequence diagram below describes the execution flow for receiving a message from the LIG.

  @image html vcom_message_receive.png "Sequence diagram of incoming EMP message"
  @image latex vcom_message_receive.png "Sequence diagram of incoming EMP message"

  The LCSMessageHandler::runIn() function is structured as follows:
  - Check for messages on the LCS VFW Channel
  - Pick first message in queue
  - Parse the EMP Header
  - If EMP layer contains any error (Incorrect header, Incorrect sequence number (e g one message lost), Incorrect CRC or a too long delay from LCS) a log-event is issued and message is discarded.
  - Conversion of byte order from network to host
  - Extraction of message fields depending on the messageType (as described in ref[FFFIS-LCS])
  - Validate the data (both syntax and range)
  - Publish the data
  - Access-functions providing validated data until next message is received (or connection towards LCS is down)


  @image html vcom_lcsmessagehandler_runin.png "runIn function of LCSMessageHandler"
  @image latex vcom_lcsmessagehandler_runin.png "runIn function of LCSMessageHandler"

  The VehicleCom::runIn() function is structured as follows:
  - It will call the runIn() function from the lcsMessageHandler to read incoming LCS messages and publish data.
  - It will raise the log event if Locomotive system fault is present in train status message.

  @image html vcom_runin.png "runIn function of VehicleCom"
  @image latex vcom_runin.png "runIn function of VehicleCom"

  \subsubsection runOut runOut()
  The execution flow for transmitting a message to LCS is as follows:
  - Process state-machines in VehicleCom
  - The LCSMessageHandler collects information (both from states within VehicleCom and data in external components) and validates outgoing messages
  - The LCSMessageHandler then adds the EMPHeader/Footer and sends the message further to through VFWChannel to Dispatcher.
  - The Dispatcher adds a Class-D Header/Footer and sends the message further to the LCS via TCP/IP.


  @image html vcom_message_transmit.png "Sequence diagram of outgoing EMP message"
  @image latex vcom_message_transmit.png "Sequence diagram of outgoing EMP message"

  The LCSMessageHandler::runOut() function is structured as follows:
  - Prepares any outgoing LCSMessage to be transmitted to the LCS
  - The creation of outgoing messages primarily depends on which data is published by the other components.
  - Iterate through all message creators
  - Collect data form other components depending on the LCSMessageType
  - Validate data and assemble data into an outgoing message in network order
  - Add EMPHeader and Footer
  - Write the EMPMessage to the LCS VFWChannel.


  @image html vcom_lcsmessagehandler_runout.png "runOut function of LCSMessageHandler"
  @image latex vcom_lcsmessagehandler_runout.png "runOut function of LCSMessageHandler"


  \subsection outgoingMessageHandling Outgoing Message Handling

  The message type and message fields of outgoing messages and sequences are given in detail in ref[FFFIS-LCS].

  \subsubsection AOSStatusMessage AOS Status Message [Message Type = 50001]
  The LCSMessageOutStatus sends AOS Status Message in every 500ms to the LCS. It contains different status information(\ref LCSAOSStatusType) from the AOS.
  The message contains Cabin selector which will tell if the ATO cabin mode is manual, automatic or supervised. The train idling status will be set
  according to the ATP modes. The position report field will contain information related to train orientation, travel direction and track information.

  \subsubsection atpCommandMessage ATP Command Message [Message Type = 50002]
  The LCSMessageOutATPCommand sends ATP Command Message(\ref LCSATPCommandType) as below:
  + Issue a ECPB Train Composition Request when the TIC Component is asked to request train-configuration.
  + Issue a Holding Brake when the 'StandStill' event is active.
  + Provide a new train weight when a new TrainComposition message is sent to the LCS.

  \subsubsection maMessage Movement Authority Message [Message Type = 50004]
  The LCSMessageOutMovementAuthority sends Movement Authority Message(\ref LCSMovementAuthorityType) to LCS, when it is received and accepted from TCC.
  It will get published MA from the Message Handler component and then append the track id and position from the end of MA, also the direction
  whether the loco is leading or trailing.

  \subsubsection atpWarningCurve ATP Warning Curve [Message Type = 50005]
  The LCSMessageOutWarningCurve sends ATP warning curve message(\ref LCSWarningCurve) to LCS. This message contains the speed curve points and
  the data will be collected if most restrictive target is changed or passed.

  \subsubsection trainCompositionmessage Train Composition Message [Message Type = 50009]
  The LCSMessageOutTrainComposition sends Train Composition Message(\ref TrainCompositionType). It is used to send the train configuration to the LCS in TrainConfig Mode (submode TrainConfigFinishOK).

  \subsubsection pathMessage Path Message [Message Type = 50011]
  The LCSMessageOutPath sends Path message(\ref LCSPathType) to the LCS, when Path Message is received and accepted from the TCC.
  It retrieves the path from Message Handler component and if it is valid, it will append the number of tracks and the Track Id of each track included in the path in order of travel.
  The message also contains the speed at the beginning of path and changes in the speed. The information related to track and position in track of the next target is also included.

  \subsubsection rclInformationMessage RCL Information Message [Message Type = 50013]
  The LCSMessageOutRclInformation sends RCL Information Message in immediately after each AOS status to the LCS.
  It contains following RCL Information(\ref LCSRclInformationType):
  + AOS Operational mode
  + AOS intervention applied
  + Allowed train movement
  + Forward Distance to go
  + Reverse Distance to go
  + Train orientation
  + Current ceiling speed



  \subsection incomingMessageHandling Incoming Message Handling

  The message type and message fields of incoming messages and sequences are given in detail in ref[FFFIS-LCS].

  \subsubsection trainStatusMessage Train Status Message [Message Type = 50101]
  This message is received in response to an AOS Status Message by LCSMessageInTrainStatus. It contains status information(\ref LCSTrainStatusType) from the Train. This information is published when it
  is parsed and validated. The status information is valid until the next Train Status message is received, or if the connection towards the LCS is down.
  This message is also used to monitor the link status. If a message is not received within 5 seconds, an event is generated and the LCS connection will be lost.

  \subsubsection ecpbTrainCompositionMessage ECPB Train Composition Message [Message Type = 50102]
  When TIC is available the LCS provides the ECPB Trainconfiguration(\ref ECPBTrainCompositionType) via this message(\ref LCSMessageInECPBTrainComposition). It is stored in the preliminary train-setup.

  \subsubsection rclStatusMessage  RCL Status Message[Message Type = 50103]
  This message is received in response to a RCL Information Message through LCSMessageInRclStatus from LCS. It is received immediately after each Train status message. It contains status of Handling Done.

  \subsection EMPMessageHandling EMP Message Handling
  EMP Message handling is performed by The EmpMsg class. It provides the functionality of adding EMP Header and footer in outgoing message to LCS and fetch
  and validate the incoming message from LCS according to ref[FFFIS-LCS].

  \subsubsection addEMPEnvelope EMP Message Header and Footer
  The EMPMsgHeader and footer(CRC) will be added the outgoing EMP message before sending to the class D messaging protocol as below:

  @image html vcom_empadd_header.png "addEMPEnvelope function"
  @image latex vcom_empadd_header.png "addEMPEnvelope function"

  \subsubsection parseEMPMessage Parse EMP Message
  EmpMsg::parseEMPMessage() will parse and validate the received EMP message to have expected EMPMsgHeader and footer(CRC) values and send the parsed message buffer to the VehicleCom for processing.

  \subsubsection empFlags EMP message Flags
  For populating the Flags field in the EMPMsgHeader following values must be set:
  + Bit0: Time Stamp format - 1(Absolute time/UTC time)\n
  + Bit1: Encryption - 0(Body is not encrypted)\n
  + Bit2: Compression - 0(Body is not compressed)\n
  + Bit3: Data integrity - 1(CRC calculated)\n
  + Bits 5-7: Reserved - Don't care bits.

  \section ClassDiagram Class Diagram

  The class-diagram below shows the VehicleCom component together with its included protocol layers.
  @image html vcom_vcom_class_diagram.png "VehicleCom Class"
  @image latex vcom_vcom_class_diagram.png "VehicleCom Class"

  The class-diagram below shows the LCSMessageHandler class.
  @image html vcom_lcsmessagehandler_class_diagram.png "LCSMessageHandler Class"
  @image latex vcom_lcsmessagehandler_class_diagram.png "LCSMessageHandler Class"

  The class-diagram below shows the Message Creators Classes.
  @image html Message_Creators_Class_Diagram.png "Message Creators Class Diagram"
  @image latex Message_Creators_Class_Diagram.png "Message Creators Class Diagram"

  The class-diagram below shows the Message Parsers Classes.
  @image html Message_Parsers_Class_Diagram.png "Message Parsers Class Diagram"
  @image latex Message_Parsers_Class_Diagram.png "Message Parsers Class Diagram"

  \section Diagnostics Diagnostics
  \subsection ConsoleCommands Console commands

  - vc stat: To get status for last incoming status-data to VehicleCom.
  - vc comp: To get status for last incoming train-composition-data to VehicleCom.
  - vc tstamp: To get the last sent/received time to/from LCS.

  \subsection Analyze Analyze
  No values are registered for analysis for VehicleCom component.

  \section CoreAdaptation Core / Adaptation
  Entire functionality of Vehicle Com component is implemented in Adaptation part. However, Core part provides the interface to get connection status towards vehicle
  and start up and health supervision test for vehicle

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS Core.

  The requirements relevant for this component:

  Req                | Chapter                        | Function
  ------------------ | ------------------------------ | -----------------
  AOS_BHPB 2644      | \ref runIn, \ref runOut  | LCSMessageHandler::runIn(), LCSMessageHandler::runOut()
  AOS_BHPB 2846      | \ref trainStatusMessage | VehicleCom::getFreeRollingStatus()
  AOS_BHPB 2782      | \ref trainStatusMessage | VehicleCom::getBrakeSystem()
  AOS_BHPB 2700 S    | \ref trainStatusMessage | VehicleCom::getBrakeSystem()
  AOS_BHPB 2641 S    | \ref trainStatusMessage | VehicleCom::getBrakeSystem()
  AOS_BHPB 2884 S    | \ref trainStatusMessage | VehicleCom::getBrakeSystem()
  AOS_BHPB 2885 S    | \ref trainStatusMessage | VehicleCom::getBrakeSystem()
  AOS_BHPB 2886 S    | \ref trainStatusMessage | VehicleCom::getBrakeSystem()
  AOS_BHPB 2640 S    | \ref trainStatusMessage | VehicleCom::getBrakeSystem()
  AOS_BHPB 2649 S    | \ref trainStatusMessage | VehicleCom::getBrakeSystem()
  AOS_BHPB 2626      | \ref trainStatusMessage | VehicleCom::getFreeRollingStatus()
  AOS_BHPB 2578      | \ref trainStatusMessage | VehicleCom::getLocoSystemFaults()
  AOS_BHPB 2612      | \ref atpCommandMessage  | LCSMessageOutATPCommand::collectData()
  AOS_BHPB 2794      | \ref runIn, \ref runOut  | LCSMessageHandler::runIn(), LCSMessageHandler::runOut()
  AOS_BHPB 2638 S    | \ref trainStatusMessage | LCSMessageInTrainStatus::parseMessageData(), VehicleCom::connectedVehicleComm()
  AOS_BHPB 3246      | \ref runIn              | LCSMessageHandler::runIn()
  AOS_BHPB 3247      | \ref runIn              | LCSMessageHandler::runIn()
  AOS_BHPB 3028      | \ref addEMPEnvelope     | EmpMsg::addEMPEnvelope()
  AOS_BHPB 3029      | \ref runIn              | LCSMessageHandler::runIn()
  AOS_BHPB 3031      | \ref AOSStatusMessage   | LCSMessageOutStatus::collectData()
  AOS_BHPB 3016      | \ref AOSStatusMessage   | LCSMessageOutStatus::collectData()
  AOS_BHPB 3020      | \ref atpCommandMessage  | LCSMessageOutATPCommand::collectData()
  AOS_BHPB 2695      | \ref atpCommandMessage  | LCSMessageOutATPCommand::collectData()
  AOS_BHPB 2616      | \ref maMessage          | LCSMessageOutMovementAuthority::collectData
  AOS_BHPB 2699      | \ref atpWarningCurve    | LCSMessageOutWarningCurve::collectData()
  AOS_BHPB 2618      | \ref ecpbTrainCompositionMessage | LCSMessageInECPBTrainComposition::parseMessageData()
  AOS_BHPB 3025      | \ref atpCommandMessage  | LCSMessageOutATPCommand::collectData()
  AOS_BHPB 5342      | \ref rclInformationMessage | LCSMessageOutRclInformation::collectData()
  AOS_BHPB 5340      | \ref rclInformationMessage | LCSMessageOutRclInformation::collectData()
  AOS_BHPB 5343      | \ref rclInformationMessage | LCSMessageOutRclInformation::collectData()
  AOS_BHPB 2624      | \ref pathMessage        | LCSMessageOutPath::collectData()
  AOS_BHPB 2949 S    |                         | VehicleCom::getBrakeSystem()
  AOS_BHPB 5341      | \ref rclStatusMessage   | VehicleCom::getHandlingDoneRequestReceived()
  AOS_BHPB 3030      | \ref trainStatusMessage | VehicleCom::getAdsEtaStatus()

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP BHP.

  Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

  */
}

