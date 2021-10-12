namespace ATP::Kernel
{
/**
\if AsMainPage
\mainpage Message Handler (Core) Component Specification
@anchor mh
\endif

\ifnot AsMainPage
\class MessageHandler
\endif

\section Purpose Purpose
This document specifies the software design for the AbstractMessageHandler class, the core part of the Message Handler component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
AbstractMessageHandler is responsible for fetching incoming messages from Radio Handler including parsing, validating and publishing.
Outgoing messages collect data from various components and validates them, if successful the messages are then queued for the Radio Handler to send.
It is also responsible for rejecting and discarding messages that do not successfully pass these steps, as well as raising the event specified by
the message to handle the corresponding faults, which could range from only logging on RU to a safety halt event. Also the protocol version is 
verified when a connection is established, see \ref ProtocolVersion.

AbstractMessageHandler also provides utility functions for other components, see \ref AbstractMessageHandler.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
The AbstractMessageHandler uses Radio Handler to fetch radio messages from the Radio Channel and Event Handler to raise events.
Depending on which message the component is parsing or validating there are more components that are used for validation, such as:
  + BTM Handler in order to adjust the BTM calendar time.
  + Mode Control to get current mode, idle state, emergency alert, storage of the indication if required to send "AbortSetup" 
  + Position to get accuracy state, current leading position, information about received balises.
  + Radio Handler to get radio messages, and see if a handover situation is present.
  + Tracks to get the position of track data items, validation of tracks, as well as storage of new tracks
  + TIMS to retrieve rear positions, balise windows, TIMS statuses, as well as storage of position report data.
  + Train Setup for validation of current setup and its correlation to new tracks, Train Setup for outgoing messages, as well as storage of new Train Setup
  + Targets for validation of incoming new targets, travel direction, as well as storage

Other components depend on this component's functionality by using its public functions.
Refer to Public Member Functions under \ref AbstractMessageHandler.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection Initialization Initialization

AbstractMessageHandler class initializes the attributes in the constructor, lists for in and outgoing messages, and events that could be handled
during the execution of the class. The initialization adds data which persists over the application cycle to cross-compare. The adaptation
handles all other initialization, i.e. the creation of the message-parsers and message-creators.

\subsection ModeDependentOperation Mode Dependent Operation

The tables below shows only the mode dependent part of accepted messages depending on current ATP mode. There are also other reasons that depends on which data is
published by the other components.

A = Accepted\n
x = Discarded\n
R = Rejected with failure\n

\latexonly \newpage \endlatexonly

 Message             | TrainConf | BaliseSrch | Normal  | Shunting | Location | Yard
 ------------------- | --------- | ---------- | ------- | -------- | -------- | ----
%DriverLogonStatus   | A         | A          | A       | A        | A        | A
%EmergencyAlert      | A         | A          | A       | A        | A        | A
%MovementAuthority   | R         | A          | A       | R        | A        | R
%PositionRepReq      | A         | A          | A       | A        | A        | A
%RevokeEmAlert       | A         | A          | A       | A        | A        | A
%StopTrain           | x         | x          | A       | x[10]    | A        | x[10]
%TrainSetup          | A         | R          | A       | R        | R        | R
%Unregistration      | A         | A[8]       | A[6]x[1]| x[10]    | x[9]     | x[10]
%ApproximatePosition | R         | R          | R       | R        | R        | R
%AreaRequest         | A[5]      | A[5]       | A[5]    | A[5]     | A[5]     | A[5]
%ATORemoteControl    | x         | x          | x       | x[10]    | A        | x[10]
%CommandMessage      | A         | A          | A       | x[10]    | A        | x[10]
%ConfigurationData   | A         | R          | R       | R        | R        | A
%ExternalData        | A         | A          | A       | x[10]    | A        | x[10]
%JoinCommand         | x         | x          | A       | x[10]    | x        | x[10]
%Path                | A         | A          | A       | x[10]    | A        | x[10]
%PossessionAck       | A[5]      | A[5]       | A[5]    | A[4]x[3] | A[5]     | A[4]x[3]
%ProtocolVersion     | A         | A          | A       | A        | A        | A
%RejectConfiguration | A         | x          | x       | x[10]    | x        | x[10]
%ShuntingAck         | A[5]      | A[5]       | A[5]    | x[10]    | A[5]     | A[5]
%YardAck             | A         | A          | A       | A        | A        | x[10]
%UncondShortening    | R         | R          | A       | R        | R        | R

 Message             | Sleeping | StaffResp | ShuntRt | Possession | Split   | Join
 ------------------- | -------- | --------- | ------- | ---------- | ------- | ----
%DriverLogonStatus   | A[4]x[3] | A         | A       | A          | A       | A
%EmergencyAlert      | x        | A         | A       | A          | A       | A
%MovementAuthority   | R        | A         | A       | R          | A       | A
%PositionRepReq      | A        | A         | A       | A          | A       | A
%RevokeEmAlert       | x        | A         | A       | A          | A       | A
%StopTrain           | x        | A         | A       | x[10]      | A       | A
%TrainSetup          | R        | A         | R       | R          | R       | R
%Unregistration      | x        | A[6]x[1]  | A[6]x[1]| x[10]      | A[6]x[1]| A[6]x[1]
%ApproximatePosition | R        | R         | R       | R          | R       | R
%AreaRequest         | A[4]x[3] | A[5]      | A[5]    | A[5]       | A[5]    | A[5]
%ATORemoteControl    | x        | x         | x       | x[10]      | x       | x
%CommandMessage      | x        | A         | A       | x[10]      | A       | A
%ConfigurationData   | A[4]R[3] | R         | R       | R          | R       | R
%ExternalData        | x        | A         | A       | x[10]      | A       | A
%JoinCommand         | x        | x         | x       | x[10]      | x       | x
%Path                | x        | A         | A       | x[10]      | A       | A
%PossessionAck       | A[4]x[3] | A[5]      | A[5]    | x[10]      | A[5]    | A[4]x[3]
%ProtocolVersion     | A[4]x[3] | A         | A       | A          | A       | A
%RejectConfiguration | x        | x         | x       | x[10]      | x       | x
%ShuntingAck         | A[4]x[3] | A[5]      | A[5]    | A          | A[5]    | A[5]
%YardAck             | A[4]x[3] | A         | A       | A          | A       | A
%UncondShortening    | R        | A         | A       | R          | A       | A

\latexonly \newpage \endlatexonly

 Message             | Reg   |  SBStop | PowerDown| Unreg| SafetyHalt | PowerUp
 ------------------- | ----- |  ------ | ---------| -----| ---------- | -------
%DriverLogonStatus   | A     |  A      | A        | A    | A          | A
%EmergencyAlert      | A     |  A      | x        | A    | x          | x
%MovementAuthority   | A     |  A      | R        | R    | R          | R
%PositionRepReq      | A     |  A      | A        | A    | A          | A
%RevokeEmAlert       | A     |  A      | x        | A    | x          | x
%StopTrain           | x     |  x      | x        | x    | x          | x
%TrainSetup          | R     |  R      | R        | R    | R          | R
%Unregistration      | A     |  A[7]   | x        | x    | x          | x
%ApproximatePosition | A[REF]|  A[REF] | R        | R    | R          | R
%AreaRequest         | A[5]  |  A[5]   | A[5]     | A[5] | x          | A[5]
%ATORemoteControl    | x     |  x      | x        | x    | x          | x
%CommandMessage      | A     |  A      | A        | A    | A          | A
%ConfigurationData   | R     |  R      | R        | R    | R          | A
%ExternalData        | A     |  A      | x        | x    | x          | x
%JoinCommand         | x     |  x      | x        | x    | x          | x
%Path                | A     |  A      | x        | A    | x          | A
%PossessionAck       | A[5]  | A[5]    | x        | A[5] | x          | A[4]x[3]
%ProtocolVersion     | A     |  A      | A        | A    | A          | A
%RejectConfiguration | x     |  x      | x        | x    | x          | x
%ShuntingAck         | A[5]  |  A[5]   | x        | A[5] | x          | A[5]
%YardAck             | A     |  A      | A        | A    | A          | A
%UncondShortening    | R     | R       | R        | R    | R          | R

[1] = Must be idle. \n
[2] = Message is only accepted when ATO Mode is Automatic. \n
[3] = Sleeping with sleeping signal active. \n
[4] = Sleeping with sleeping signal inactive. \n
[5] = Train is standstill, Reject/ignore otherwise. \n
[6] = Not Idle. Reject and raise a SafetyHalt event \n
[7] = Accept if standstill, ignore and raise a SafetyHalt event otherwise. \n
[8] = With an Active MA. \n
[9] = Reject and raise a SafetyHalt event. \n
[10] = Discard and raise a log event.
[REF] = See \ref Requirements. \n

Also the creation of outgoing messages is mode-dependent but primarily depends on which data is published by the other components.

\latexonly \newpage \endlatexonly
\subsection Scheduling Scheduling
The message handler has two functions AbstractMessageHandler::runIn and AbstractMessageHandler::runOut which are called each execution cycle.\n

\subsubsection runIn runIn()
- For each incoming message parser, invalidate the old data.
- Calls Radio Handler to read any new radio messages received and the id of the source TCC.
- Extract the messageType for a message and call its associated parser
- Parse the data
  + Extraction of message fields, depending on the messageType. The message and message fields are described in [FFFIS TCC-AOS].
- Validate the data, see \ref ValidateMessages
- Publish the data
  + targets, track and balise information extracted from the MA message is validated and stored in the associated storage components Tracks and Targets.
  + Train setup information extracted from the TrainSetup message is validated and stored in the associated storage component TSetup.
  + Access functions will provide data from the validated messages for a duration of one ATP execution cycle. The data is invalidated each cycle so a
    consuming component must check and read the data every cycle. For more details, see \ref PublishIncoming
- Send MessageAcknowledge to inform TCC if the message has been accepted or rejected (only for the messages RadioMessageInApproximatePosition, 
   RadioMessageInConfigurationData, RadioMessageInMovementAuthority, RadioMessageInTrainSetup).
- For other messages, when a valid telegram is received from TCC and the message data contains values for which the handling is not defined, AOS issues a Safety halt event. 

\latexonly \newpage \endlatexonly
\subsubsection runOut runOut()

If an outgoing message is going to be created and sent out depends primarily on which data is published by the other components, as well as acknowledging
incoming packets.

- For each message creator:
  + Collect data depending on the messageType
  + Validate data, see \ref ValidateMessages
  + If data is available to send, assemble an outgoing message according to [FFFIS TCC-AOS].
  + Queue any available and valid outgoing message. To each outgoing message there is a destination TCC, either all TCCs or a specified TCC.
  + If the message created was a position report, the information is stored in message handler (and can be fetched from radio handler if no other message
    is queued). There is one default position report storage for each region.

\subsection ValidateMessages Validate Messages
Many messages require additional validation apart from validating the mode and the parsing of the message, e.g. validating that the tracks, balise and targets are consistent internally and with the
stored equivalent, or validating pre-requisites such as a driver cannot be logged in before receiving a DriverLogonStatus message. Any messages not mentioned will not perform any additional
validation of data.

\subsubsection ValidateApproxPos Approximate Position
An approximate position message is valid if the Track Data sent in the message is continuous and the new Train footprint calculated from the train front position defined in the message is within the tracks received.

For a partial approximate position message, it only checks the tracks data to be continuous and only when the final one is received does it check the combined track data for gaps.

\subsubsection ValidateConfData Configuration Data
The configuration data is considered valid only if a configuration message has not been received previously since power on, the parameters received are all considered to have valid
values and the driver has not yet logged in. If the ConfigurationData message was rejected due to invalid values of Configuration Data parameters, issue a Safety Halt event 
and inform the TCC about the reason for the Safety Halt.

\subsubsection ValidateLogonStatus Driver Logon Status
The DriverLogonStatus message is invalid if the driver is already logged in. This message is sent from TCC following a DriverInformation message from AOS.

\subsubsection ValidateMA Movement Authority (MA)
TCC sends a Movement Authority message to the train to inform the train where it is allowed to move. The MA is considered consistent if the tracks correspond with
the current stored tracks and, if needed, extend with more tracks. Any track that already exists in the stored data, and is provided in the MA must be 
identical to be considered consistent. It is also required that all positions mentioned in the MA are described in either the existing tracks or are 
provided in the MA.

For an MA to be considered continuous it is required that all the tracks have a previous track that exist, or 0, which would indicate that the MA is a 
"MA from scratch". MA from scratch is an MA with a track where the previous track ID set as 0.

It is also required for the track data to have the same direction as the MA, and it will verify that all the tracks and positions are placed in between the 
start of the MA and the end of the MA as well as also checking that all the tracks in the location borders must to be less or equal to the MA end.

Furthermore the MA end position must be after or equal to the start position in the travel direction. AOS must also be able to store all of the data received
in the MA. 

When a Primary Target exists, the AOS accepts an MA extension if:
 - The train position is Known or Approximate, AND
 - The start of MA is the same as current Primary Target.


The MA message is validated as described below:

Data process order:\n
1 - Tracks\n
2 - Balises\n
3 - Primary targets; MAEnd, LocationStart, LocationEnd\n
4 - Other targets; Speed, Gradient, Track Data Items, Conditional Targets\n
 \n
Data storage:\n
- Tracks contains existing track and balise definitions
- Targets contain existing targets
- Internal representation to store extracted tracks/targets

If MA is accepted, generate an acknowledge to TCC, otherwise reject MA.
Once the MA is accepted the data shall be added to first Tracks and then Targets. The order is important to allow calculation of valid odometer values when adding Targets.

\paragraph ValidateTracks Validating Tracks
- Extract all tracks from the MA and store in local data
- Check correctness of local data received from the MA:
  - All new tracks make a continuous list, no gaps, same travel direction
  - Check if already stored in Tracks, then it must have the same position
  - Check if the ID is unique
  - Check if the end of MA from scratch with PARTLY_MA block is equal to the start of new MA extension
  - Check if the provided MA has tracks defined for start and end position


\paragraph ValidateBalises Validating Balises
- Extract balises from the MA and check correctness towards track setup
- Check if each balise can be placed on a valid track from the MA or a track stored on board.
- Check if there are any duplicated balise identities, checking combined MA and Tracks data

\paragraph ValidatePrimaryTargets Validating Primary Targets
- Extract new primary target
- Check for valid placement on track, local or Tracks
- Check if locomotive orientation is consistent with MA and TSetup
- Check that MAEnd/LocationEnd does not shorten the travel distance
- When in train state Idling the AOS shall accept an MA from scratch if the MA covers the train footprint
- Check number of LocationStart/LocationEnd (max 1 pair, i.e. one of each) and that each LocationStart has a matching LocationEnd

MA route type shall be checked:

 Route type      | Allowed in modes/conditions
 --------------- | ------------------------------------------------------
Normal           | Normal, BaliseSearch, LocationHandlig+HandlingDone, StaffResponsible
Shunting         | Normal+Idling
LocationEnd      | Normal
LocationStart    | Normal
Re-registration  | BaliseSearch, TrainConfiguration

\paragraph ValidateOther Validating Other Targets
- Extract target
- Check total number of stored targets vs max allowed per type
- Check for valid placement on track, local or Tracks
- Check that new target is in footprint of the received MA.

\paragraph ValidateLocation Validating Location
The AOS accepts an MA of Q_ROUTE_TYPE Normal with LOCATION_BORDERS block if
 -	The ATP mode is Normal OR Location, AND
 -	The position of the Location End is the same as the end of MA position, AND
 - 	The position of Location End is after the Location Start in the direction of MA, AND
 - 	The Location area is big enough to accommodate the train footprint, AND
 -	The Location Start and End are within the tracks in the MA or stored tracks.

In ATP mode Normal, an MA which updates the Location area boundary is accepted if:
 - The Q_ROUTE_TYPE of MA is Normal, AND
 - The MA contains the LOCATION_BORDERS block, AND
 - The direction of MA is consistent with the Location start and Location End Targets AND
 - The train footprint is inside the new Location area boundary AND
 - The currently supervised location boundary is not moved closer to the train.
 
\subsubsection ValidateApproximatePosition ApproximatePosition Message
RadioMessageInApproximatePosition class will perform following operations:
- Parsing of the message
- Validating the message for valid modes
- Validating the message data:
  - Message will be invalid if NID_TRACK equals 0.
  - Message will be validated for continues tracks received.
  - Train footprints (front and rear end) should be within the received tracks.
  - Max number of tracks are 10

\subsubsection ValidatePath Path
A %Path message is valid if target track id is received in the tracks block and all speed change position tracks and positions are represented in the tracks block.

\subsubsection ValidateTrainSetup Train Setup
A valid train setup message has all its vehicle types referred to in the vehicle data defined by a vehicle type data block, there are no duplicates of any vehicle type data and all the vehicle ids in the vehicle data are unique or 0. In addition so must the calculated lambda for empty and loaded train weights not be lower than configured "Minimum allowed lambda".

\subsubsection ValidateUncondShort Unconditional Shortening
The unconditional shortening message is only valid if the end of the MA position is within the stored track data, its position is earlier than the current primary target and it is also ahead of the current train position.

\subsubsection ValidateOutgoing Validate Outgoing Messages
When validating outgoing messages, the creator for the specific message will verify whether such a message will be sent as well as whether the message data and structure is correct.

\subsection PublishIncoming Publish Incoming Messages
Many messages are published through dedicated access functions in message handler or published to the storage classes, but some are of design or scope that needs further explanation which will be added below.

\subsubsection PublishCommandMsg Command Message
The data received in a configuration data message will be either the name which is published to the Train Setup storage, or the system time which will be used to update system time, the BTMHandler system time and the DMI time values, or both.

\subsubsection PublishConfData Configuration Data
Configuration data is published by saving the received information in the Config component.

\subsubsection PublishLogonStatus Driver Logon Status
The information from the Driver Logon message informs AOS if the driver was successfully logged in, which will be available for other components to read,
and it also sets the system time, see Command Message above.

\subsubsection PublishMA Movement Authority (MA)
If this is not the first part of a partial MA, perform following operations:
 - Publish the balises
 - Remove the existing primary target and replace it before publishing the new primary target.
 - Publish the track data
 - Publish target data
 - Publish location data and location border

\subsubsection PublishApproximatePosition ApproximatePosition Message
RadioMessageInApproximatePosition class will perform following operations:
- Remove existing tracks and balise data before publishing new tracks data.
- Publishing the tracks in data storage and updating the tracks odo information according to train front position received in message.

\subsubsection PublishProtocolVersion Protocol Version
When a ProtocolVersion message with an unrecoverable mismatch is received from the TCC the AOS shall issue a Log event.

\subsubsection PublishTrainSetup Train Setup
Train Setup provides the information regarding the makeup of the train and it will store the information in the TrainSetup storage component. It will store the configuration, weights and sizes of
any cars, locomotive as well as all the other necessary data to calculate the brakeability of the train.

\subsection Special Special Messages
Some messages require special handling, such as the Area Request and Registration Area message sequence and the specifics will be mentioned below.

\subsubsection AreaRequest Area Request and Registration Area
The first Area request message received from the central TCC will be accessible for other components e g by the DMI Handler (to send out the Area-request message to DMI).
The Message Handler gets the RegistrationArea from the DMI Handler and provides the complete RegistrationArea message to the Radio Handler (and further to the TCC).
When the next AreaRequest arrives, the RegistrationArea is already available in Message Handler and the AreaRequest message will be processed (and consumed) by the Radio Handler.
It is important to note that only the Central TCC will send the area request and when the driver has responded to the dialog the response will be sent in the same cycle, not the following one which is the standard behaviour.

\subsubsection ProtocolVersion Protocol Version Handling
The version of the protocol used for message exchange is checked by TCC when a communication link is set up. When the Protocol Version message is received by AOS, the AOS Protocol Version message needs to
be sent in the same cycle as a response. So in this case the message will not be placed in the outgoing message queue, but there will be special treatment in the Radio Handler to send this message before
the normal handling of outgoing messages.

This means that the Protocol version handling is done in the Radio Handler with the help of APIs in Message handler.
- validateProtocolVersion(...) : Validates the ProtocolVersion message, The RadioMessageInProtocolVersion stores the status of protocol validation for each channel.
- getProtocolVersionResponse(...) : Creates and returns an outgoing ProtocolVersionMsg (with help from RadioMessageOutProtocolVersion) if needed.
- isProtocolVersionMatching(...): Returns status of the protocol validation for requested channel.

\subsubsection TimeSync ATP-TCC Calendar Time Synchronization
ATP will synchronize its calendar time with that of the TCC when it receives DriverLogonStatus and command messages. Message handler will parse the Time field from these
messages and use the setUTCTime utility function to set the system calendar time. The BTM calendar time is updated by calling updateBtmCalendarTime in the BTM Handler.

\subsubsection ackDefaultPositionReport Default Position Report Sent
AbstractMessageHandler::ackDefaultPositionReport function is called by Radio Handler to acknowledge that the variable length data of the current
defaultPositionReportMsg (position and status information for the train) has been sent to TCC.
Message Handler will only change the fixed parts of the defaultPositionReportMsg such as track, position, direction, speed, train status if the previous
defaultPositionReportMsg has not yet been sent to TCC. If TIMS is connected, the TIMS position report will be updated with accuracy state, front and rear track positions.

\subsubsection StopDistance Stop Distance
When the TrainSetup message is accepted by AOS, calculate the distance required to safely stop the train. The stop distance shall be sent
to the TCC in the PositionReport message used to acknowledge the TrainSeup.

\subsubsection HandOver Handover Procedure
When a train is communicating with two regional TCC simultaneously, close to or covering a region border, the handover procedure will take place.
During this procedure, active communication links are polled by both TCCs according to [FFFIS TCC-AOS] and sequenced operation such as an MA transmission will
be synchronized between the two TCCs before the MAs are sent.
AOS is considered in communication with a TCC as long as it is receiving a valid message other than Area Request.
AOS in turn ensures the following:
  - Both TCCs are to receive both AbortSetup and DriverInformation messages.
  - Any message with the following blocks are sent to both TCCs:
    - TRAIN_NAME
    - YARD_REQUEST
    - POSSESSION_REQUEST
  - AOS shall send a StartUpMessage to both TCCs when in the special case of a configuration following a ShuntingRoute.
  - When AOS gets a PositionReportRequest from a TCC it shall send all the events generated to the requesting TCC.
  - When requesting a mode change, such as Yard or Possession, both TCCs will receive the request but only the TCC responsible for the Possession or Yard will respond to it.


\subsubsection RegularHandOver Region Border Passage Handover
The purpose of the handover procedure is to make sure that a train can safely pass from the region of one TCC to another. 
Normally this is handled when the TCC in the originating region (T1) discovers that the train is nearing its borders which triggers it to notify the second regions TCC (T2) that the train is approaching which causes T2 to start polling the train.

This starts with the normal initiating procedure of a protocol version check before the polling commences. Once T2 has locked a route to the target T1 sends a partial MA to the train which gets a message ack response. 
This is followed by T1 informing T2 that it can send the extending MA which will complete the previous partial MA. As this MA is accepted, the train is free to move into the T2 Region and as the train leaves the T1 region T1 will stop polling the train.

The partial MA sent by T1 here has the purpose of an initial MA, where if it is accepted it is stored for later evaluation. When the second MA is accepted, and 
together with the information from the first partial MA, the combined information is validated. Only then the combined MAs are used to publish new targets,
allowing the train to move across the region border safely.

All of this is described further in [TCC-AOS-SEQ] Section 9, specifically a normal border passage.

\subsection SpecialHandOver Other Handover Cases
In addition to the normal region border passage scenario, there are other cases for how the train could be passing a region border. Such as while in possession, sleeping, shunting route or while in a radio hole.
Furthermore a train might be needing to do a reposition or reregistration procedure while at a region border. The reposition procedures requires using a partial Approximate Position message to get the T1 track data to merge with T2 track data, as well as many other steps to ensure a safe and stable region change.

All of these are described in [TCC-AOS-SEQ] Section 9 and will not be mentioned further here.

\latexonly \newpage \endlatexonly
\section ClassDiagram Class Diagram

@image html abstract_message_handler_class_diagram.png "Class Diagram"
@image latex abstract_message_handler_class_diagram.png "Class Diagram"

For a full description of the classes, see AbstractMessageHandler, AbstractRadioMessageIn and AbstractRadioMessageOut.

\section Diagnostics Diagnostics

\subsection Console Console Commands
N/A

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
The message handling classes are divided into the adaptation classes and the core classes.
The adaptation class of AbstractMessageHandler is responsible for initializing all the message parser instances for both incoming and outgoing messages,
derived from  AbstractRadioMessageIn and AbstractRadioMessageOut, to handle all the messages that are to be received or sent out.

To allow for a flexible core/adaptation solution, the AbstractMessageHandler class delegates the parsing of incoming messages to instances of classes derived 
from the AbstractRadioMessageIn class.
Pointers to the base class of the parsers are stored in an array where the messageType serves as an index in the array to locate the correct parser.

For outgoing messages the solution is similar to the incoming message classes. For any new message type, a corresponding parser or creator must be instantiated by adaptation.
Pointer to message creators are stored in an array where the messageType serves as an index in the array which is iterated through each cycle and the creators are
called to collects and validate data as well as building and queueing messages.

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component

\section Traceability Traceability

\subsection Requirements SSRS Functional Requirements
The functional requirements are defined in [SSRS].
Common functional requirements are described in SCDS ATP Core.

The requirements relevant for this component are:

  Req        | Chapter                      | Function
  ---------- | ---------------------------- | ---------------------
 AOS 34      | \ref runOut |  RadioMessageOutStartUpMessage::collectData()
 AOS 40      | \ref runIn  |  RadioMessageInTrainSetup::publishData()
 AOS 65      | \ref runOut |  RadioMessageOutTrainRegistrationInformation::collectData()
 AOS 92 S    | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 102     | \ref ModeDependentOperation | AbstractRadioMessageIn::validate()
 AOS 144     | \ref ValidateMessages | RadioMessageInDriverLogonStatus::validate()
 AOS 145     | \ref ValidateMessages | RadioMessageInDriverLogonStatus::validate()
 AOS 148     | \ref runOut |  RadioMessageOutDriverInformation::collectData()
 AOS 149     | \ref ValidateMessages | RadioMessageInDriverLogonStatus::validate()
 AOS 150     | \ref ValidateMessages | RadioMessageInDriverLogonStatus::validate()
 AOS 158     | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 159     | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 160 S   | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 163     | \ref PublishIncoming | RadioMessageInMovementAuthority::publishTracks()
 AOS 172     | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 173     | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 175     | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 226     | \ref ValidateMA | RadioMessageInMovementAuthority::validateTargetsData()
 AOS 397 S   | \ref ValidateMessages |  RadioMessageInEmAlert::validateMode()
 AOS 399     | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 400     | \ref PublishIncoming | RadioMessageInEmAlert::getEmAlertReason()
 AOS 447     | \ref ModeDependentOperation |  RadioMessageInUnregistration::validateMode()
 AOS 458     | \ref ModeDependentOperation |  RadioMessageInStopTrain::validateMode()
 AOS 473     | \ref runIn |  RadioMessageInCommandMessage::publishData()
 AOS 486     | \ref PublishIncoming | RadioMessageInCommandMessage::getTextMessage()
 AOS 637     | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 689     | \ref PublishIncoming | RadioMessageInCommandMessage::publishData() ,  RadioMessageInTrainSetup::publishData()
 AOS 690     | \ref runIn |  AbstractMessageHandler::updateSystemTime()
 AOS 757     | \ref runOut |  AbstractMessageHandler::runOut()
 AOS 769     | \ref runIn |  AbstractMessageHandler::runIn()
 AOS 1085    | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 1120 S  | \ref runIn |  AbstractRadioMessageIn::validate() & AbstractRadioMessageOut::validate()
 AOS 1157    | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 1203    | \ref runOut |  RadioMessageOutStartUpMessage::collectData()
 AOS 1212    | \ref runIn |  RadioMessageInAreaRequest::validateMode()
 AOS 1213    | \ref runOut |  RadioMessageOutRegistrationArea::collectData()
 AOS 1246    | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 1247    | \ref PublishIncoming | RadioMessageInProtocolVersion::getProtocolVersionFromTCC()
 AOS 1249    | \ref ProtocolVersion | RadioMessageInProtocolVersion::publishData()
 AOS 1287    | \ref ValidateMA | RadioMessageInMovementAuthority::validateBaliseData()
 AOS 1305    | \ref ValidateMessages |  RadioMessageInApproximatePosition::validateApproxPosMessage()
 AOS 1674    | \ref PublishIncoming | RadioMessageInPositionReportRequest::getInitiateConfig()
 AOS 1705    | \ref PublishIncoming | AbstractMessageHandler::getRejectConfigurationInfo()
 AOS 1732    | \ref ModeDependentOperation |  RadioMessageInUnregistration::validateMode()
 AOS 1737 S  | \ref ConfigurationData |  RadioMessageInConfigurationData::setValidatedConfigData()
 AOS 1738    | \ref ModeDependentOperation |  RadioMessageInYardAcknowledge::validateMode(),  RadioMessageInPossessionAcknowledge::validateMode(),  RadioMessageInShuntingAcknowledge::validateMode()
 AOS 1758    | \ref runOut |  RadioMessageOutAbortSetup::collectData()
 AOS 1761    | \ref runOut |  RadioMessageOutStartUpMessage::collectData()
 AOS 1823 S  | \ref ValidateMA | RadioMessageInMovementAuthority::validateMode()
 AOS 1830 S  | \ref ValidateMA | RadioMessageInMovementAuthority::validateMode()
 AOS 2031    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2032    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2033    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2034 S  | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2035    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2036    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2037    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2038    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2039    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2040    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2041    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2056    | \ref runIn |  RadioMessageInTrainSetup::publishData()
 AOS 2082    | \ref PublishApproximatePosition | RadioMessageInApproximatePosition::publishTracks()
 AOS 2114    | \ref runIn |  RadioMessageInTrainSetup::validateModeConfiguration()
 AOS 2116    | \ref runIn |  RadioMessageInTrainSetup::publishData()
 AOS 2141    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2142    | \ref runIn |  RadioMessageInMovementAuthority::publishPrimaryTarget()
 AOS 2163    | \ref runIn |  RadioMessageInMovementAuthority::publishData()
 AOS 2174 S  | \ref runIn |  RadioMessageInMovementAuthority::publishPrimaryTarget()
 AOS 2194    | \ref ValidateMA | RadioMessageInMovementAuthority::validateLocationTargetData()
 AOS 2196    | \ref PublishIncoming | RadioMessageInDriverLogonStatus::validate()
 AOS 2214 S  | \ref runIn |  RadioMessageOutPositionReport::collectData()
 AOS 2216    | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 2232    | \ref runIn |  RadioMessageInTrainSetup::validate()
 AOS 2263    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2264    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2266    | \ref ValidateApproximatePosition | RadioMessageInApproximatePosition::validateApproxPosMessage()
 AOS 2268    | \ref ValidateApproximatePosition | RadioMessageInApproximatePosition::validateApproxPosMessage() & RadioMessageInApproximatePosition::validateMode()
 AOS 2290    | \ref PublishIncoming | RadioMessageInCommandMessage::publishData()
 AOS 2299    | \ref runOut |  RadioMessageOutStartUpMessage::collectData()
 AOS 2363    | \ref runOut |  RadioMessageOutDriverInformation::collectData()
 AOS 2364    | \ref ModeDependentOperation | AbstractRadioMessageIn::validate()
 AOS 2418    | \ref ModeDependentOperation |  RadioMessageInUnregistration::validateMode()
 AOS 2419 S  | \ref ModeDependentOperation |  RadioMessageInUnregistration::validateMode()
 AOS 2420    | \ref ModeDependentOperation |  RadioMessageInUnregistration::validateMode()
 AOS 2421 S  | \ref ValidateMessages |  RadioMessageInConfigurationData::validate()
 AOS 2430 S  | \ref ModeDependentOperation |  RadioMessageInUnregistration::validateMode()
 AOS 2446    | \ref runIn |  RadioMessageInPath::validateTracks(), RadioMessageInPath::validateSpeedChangePosition()
 AOS 2448    | \ref runIn |  RadioMessageOutPositionReport::collectData()
 AOS 2450    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2465    | \ref AreaRequest |  AbstractMessageHandler::getRegistrationAreaMessageSentToCentralTCC()
 AOS 2466    | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 2526    | \ref runIn |  AbstractMessageHandler::runIn()
 AOS 2591    | \ref PublishApproximatePosition | RadioMessageInApproximatePosition::publishTracks()
 AOS 2624 S  | \ref TimeSync |  AbstractMessageHandler::compensateCalendarCalculations()
 AOS 2696    | \ref ValidateMA | RadioMessageInMovementAuthority::validateNotReadyToDriveStatus()
 AOS 2703 S  | \ref runOut |  RadioMessageOutPositionReport::collectData()
 AOS 2728 S  | \ref PublishTrainSetup | RadioMessageInTrainSetup::validateAndCalculateVehicleData()
 AOS 2735 S  | \ref runIn |  RadioMessageInTrainSetup::publishData()
 AOS 2771    | \ref runOut |  AbstractRadioMessageOut::collectTrainStatusInfo()
 AOS 2787    | \ref runIn |  RadioMessageInPossessionAcknowledge::publishData()
 AOS 2788    | \ref PublishIncoming | RadioMessageInMovementAuthority::publishLocationTarget()
 AOS 2797    | \ref ModeDependentOperation |  RadioMessageInConfigurationData::validateMode()
 AOS 2798    | \ref runIn, \ref runOut |  RadioMessageInConfigurationData::validate(),  RadioMessageOutPositionReport::collectData()
 AOS 2799    | \ref runOut |  AbstractRadioMessageOut::collectTrainStatusInfo()
 AOS 2835    | \ref ModeDependentOperation |  RadioMessageInMovementAuthority::validateMode()
 AOS 2841    | \ref ModeDependentOperation | AbstractRadioMessageIn::validate()
 AOS 2842    | \ref ModeDependentOperation | AbstractRadioMessageIn::validate()
 AOS 2850    | \ref runOut |  RadioMessageOutStartUpMessage::collectData()
 AOS 2930    | \ref ValidateApproximatePosition | RadioMessageInApproximatePosition::validateApproxPosMessage()
 AOS 2933    | \ref runIn, \ref runOut |  RadioMessageInTrainSetup::publishData(),  RadioMessageOutPositionReport::collectData()
 AOS 2936    | \ref runOut |  RadioMessageOutAbortSetup::collectData()
 AOS 2941    | \ref runIn |  AbstractMessageHandler::runIn()
 AOS 2949    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2951    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 2952    | \ref ValidateMA | RadioMessageInMovementAuthority::validate()
 AOS 3019    | \ref runOut | RadioMessageOutPositionReport::collectMsgAckData()
 AOS 3121 S  | \ref runIn | AbstractMessageHandler::runIn()
 AOS 3126    | \ref runIn |  RadioMessageInConfigurationData::validate()
 AOS 3159    | \ref runOut |  RadioMessageOutStartUpMessage::collectData()
 AOS 3163 S  | \ref ValidateUncondShort |  RadioMessageInUnconditionalShortening::validate()
 AOS 3164    | \ref runIn |  RadioMessageInUnconditionalShortening::validate()
 AOS 3165    | \ref runIn |  RadioMessageInUnconditionalShortening::publishData()
 AOS 3166    | \ref ValidateUncondShort |  RadioMessageInUnconditionalShortening::validate()
 AOS 3167    | \ref ValidateUncondShort |  RadioMessageInUnconditionalShortening::validate()
 AOS 3168 S  | \ref runIn |  RadioMessageInUnconditionalShortening::validatePositionInStorage()
 AOS 3199 S  | \ref PublishIncoming | RadioMessageInMovementAuthority::publishPrimaryTarget()
 AOS 3218    | \ref runOut |  AbstractMessageHandler::runOut(void), RadioMessageOutStartUpMessage::getChannelId()
 AOS 3262    | \ref runIn |  RadioMessageInMovementAuthority::publishData()
 AOS 3270    | \ref ValidateTrainSetup |  RadioMessageInTrainSetup::validateMode()
 AOS 3271    | \ref ModeDependentOperation |  RadioMessageInTrainSetup::validateMode()
 AOS 3296    | \ref StopDistance |  RadioMessageOutPositionReport::collectStopDistData()
 AOS 3306    | \ref AreaRequest |  RadioMessageInAreaRequest::parseMessageData()
 AOS 3307    | \ref AreaRequest |  RadioMessageInAreaRequest::parseMessageData()

\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP Core.

Fulfillment of architectural requirements allocated to the ATP is described in [SWAS].
*/
}