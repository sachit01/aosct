namespace ATP::RadioCom
{

  /**
  \if AsMainPage
  \mainpage RadioChannel (Core) Component Specification
  @anchor rc
  \endif

  \ifnot AsMainPage
  \class RadioChannel
  \endif

  \section Purpose Purpose
  This document specifies the software design for the RadioChannel component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality
  The RadioChannel component is handling the reception and sending of radio-messages to/from one TCC.
  The component provides the following functionality:
  - Collects incoming byte stream to build a message.
  - Transmits outgoing messages to TCC.
  - Checks basic validity of incoming messages; site id, region id, CRC, etc.
  - Updates header and CRC for outgoing messages.
  - Checks below fields of incoming messages.
    + CRC
    + Site id and region id
  - Manages and validates time stamps on incoming and outgoing messages.

  \subsection DeploymentDiagram Deployment Diagram
  @image html abstract_radio_handler_objects.png "Deployment diagram"
  @image latex abstract_radio_handler_objects.png "Deployment diagrams"

  \subsection Dependencies Dependencies
  The RadioChannel depends on Dispatcher and Vital Framework APIs. RadioHandler creates the three instances of RadioChannel.

  Other components depend on this component's functionality by using its public functions.
  Refer to Public Member Functions under \ref RadioChannel  \n

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design
  \subsection Initialization Initialization

  Initialization includes calling of preInit() and init() functions:
  - The preInit() function performs required initialization such as :
    + Open a channel to be used when reading from the dispatcher
    + Open a channel to be used when writing to the dispatcher
    + Synchronize the channel with the diversified channel (A/B)
    + Deactivate the event driven callback functionality.

  - The init() function performs required initialization such as
    + Initialize the channel statistics for the component
    + Adding the CRC tables to cross-compare only to RadioChannelCentral to avoid multiple times of comparing of same data. 
    + Add the component to the Console Trace Vector
    + Validates if all the initializations are done successfully.

  The RadioChannel constructor saves the names of the vfw-channels dedicated to the channel.

  \subsection ModeDependentOperation Mode Dependent Operation
  N/A

  \subsection Scheduling Scheduling
  The RadioChannel component has RadioChannel::runIn and RadioChannel::runOut that must be called in each execution cycle:

  \subsubsection runIn runIn()
  RadioChannel::readFromChannel function reads all incoming byte stream in the input buffer from Dispatcher. Incoming messages should be received within the configured timeout,
  or Radio connection will be considered lost. After receiving the byte stream in a buffer, the incoming messages are extracted and validated by \ref RadioChannel::extractMessage function.\n
  Validated messages are either discarded based on protocol validation, performed in the RadioChannel Component (refer to [FFFIS TCC-AOS]) or pushed in 
  the incoming message queue in the same cycle. However, the incoming message queue can handle up to 5 messages in each cycle.\n 
  When there is a new message in the incoming message queue, transmission mode is set to be ready to send a new message or set to resend the previous message according to ref [FFFIS TCC-AOS].\n 


  @image html radio_channel_runin.png "runIn() flow chart"
  @image latex radio_channel_runin.png "runIn() flow chart"

  \subsubsection runOut runOut()
  The component is designed to only send a message as a response to an incoming message from the TCC. Each outgoing message is sent by RadioChannel::runOut as response to an incoming message. 
  These outgoing messages are sent to the channel from which the incoming messages originated from. RadioChannel::runOut does this by taking the next message from the outgoing message queue and adding a Header and time-stamps before sending it.
  If RadioChannel::runIn function discovers that the incoming message is incomplete, corrupted or invalid to receive, then RadioChannel::runOut function will resend the previous message. 
  It is important that the outgoing message is sent within allocated time-period or TCC will re-send the same message.\n
  RadioChannel allows max of 5 messages in outgoing queue, which is used by the RadioChannel::writeMessage.

  @image html radio_channel_runout.png "runOut() flow chart"
  @image latex radio_channel_runout.png "runOut() flow chart"

  \subsection HandshakingandResending Handshaking and Resending
  The TCC periodically polls each of the AOS. Polling occurs by the TCC sending the telegram that is pending for the particular train.
  Having sent the pending telegram, a message return timeout is initiated. If reception of a response telegram from the AOS system has not begun
  within configured timeout, then the telegram will be re-sent at the next polling. TCC cannot send a new telegram to a particular AOS system before the previous
  telegram has been acknowledged or a message return timeout has occurred. Handshaking and Resending between AOS and connected TCC is handled by 
  RadioChannel::runIn and RadioChannel::runOut functions.

  \subsection Flowcontrol Flow control
  Radiochannel only sends a message in response to a successfully received message from a connected TCC.
  Therefore half duplex communication establishes between AOS and TCC. This flow control is maintained by the connected TCC.

  \subsection SequenceNumbering Sequence Numbering
  The component creates the double time stamps to perform a safe application layer protection against data obsolescence in messages transmitted between AOS and connected TCC. 
  Refer to Sequence Numbering section of [FFFIS TCC-AOS]. Whereas RadioChannel::generateTimeStamp, RadioChannel::handleTxTimeStamps and RadioChannel::isValidTimeStamp functions handle Sequence Numbering.

  \subsubsection  Validatetimestamps Validate time-stamps
  RadioChannel::isValidTimeStamp validates the sequence numbering of the incoming messages.
  - The received message header includes two timestamps:
    + T_Sender, which is the time-stamp created by TCC when the message was created.
    + T_Ref, which is the time-stamp from the previous message sent from AOS to TCC(as T_Sender) and copied by TCC to T_Ref.\n
  - Normally
    + T_Ref should be identical to the value of T_Sender in the previous message AOS transmitted to TCC.
    + T_Sender will be saved for later use as T_Ref when the response is transmitted.\n \n
  - Possible scenarios and their effects on the time stamps are as follows:
    + The time-stamp is correct which means a new message(either PositionReport or other message prepared by MessageHandler & RadioHandler) will be transmitted.
  It should also shift the saved T_Ref and other values.
    + The first message received after startup. AOS accepts any value of T_Ref.
    + The received T_Ref is default T_Ref  which means TCC has just restarted or the link is reconnected after loss of communication.
  Accept this and save the time stamp and log as an event. In this case, the component will not be able to detect retransmissions.
    + If a response from AOS takes too long it will cause TCC to transmit a new poll before receiving a response, TCC should skip this last message. Otherwise, 
    TCC will retransmit.
    + If the value of T_Ref is neither of the last two values transmitted by the RadioChannel, it is considered an error and the message is discarded.
    TCC will be expected to re-transmit. Communication loss(timeout) will be handled by the RadioHandler.\n
  - Refer to the Sequence numbering of [FFFIS TCC-AOS].

  \subsubsection Handletransmittimestamps  Handle transmit time-stamps
  RadioChannel::handleTxTimeStamps assigns time-stamp from the sender and previous timestamps according to Sequence Numbering section of [FFFIS TCC-AOS].
  Whereas RadioChannel::generateTimeStamp generates time stamps for the sender.

  \subsubsection Generateanewtimestamp Generate a new time-stamp
  RadioChannel::generateTimeStamp generates new time stamp from platform time. It makes sure the time-stamp is different than the last one used and different than the default time-stamp used at start-up.
  Since, the resolution of the time stamps maintained is 16 ms thus, minimum difference between two consecutive time-stamps will be at least 16 ms (Refer to [FFFIS TCC-AOS]).

  \subsection Telegramformat Telegram format at a Safety Level
  The RadioChannel component adds or removes the double time-stamping, locomotive ID, message length and CRC based on incoming or outgoing messages.
  It reports any discrepancies with incoming time-stamp data. The safety level will check for correct telegram sequence and verify the CRC checksum.
  Telegram re-sends and time-outs will also be handled on this level. Since the CRC handles a maximum length of 4095 bytes, the message data shall be divided in 4000 byte chunks,
  each with an additional CRC (Refer to [FFFIS TCC-AOS]).

  \subsubsection Validateanincomingmessage Validate an incoming message
  An incoming message is validated by a call to RadioChannel::validateMessage() before it becomes available for the RadioChannel::readMessage() access-function.
  The checks include
  - The message starts with STX
  - The CyclicRedundancyCode in the message  matches a calculated CRC
  - The RadioId in the message is matching the configured RadioId
  - The SiteId in the message is matching the configured SiteId.

  \subsubsection Packtheoutgoingmessage Pack the outgoing message
  RadioChannel::packMessage packs outgoing message as below:
  - Insert STX.
  - Convert header fields(Id, Site Id, Region Id, Length, time-stamps) to network order.
  - Append data (already in network order).
  - Calculate and append CRC in network byte order.

  \subsubsection CRCcalculation CRC calculation
  Region CRC: RadioChannel::calculateCRC creates CRC by using region polynomial and initial value according to ref [FFFIS TCC-AOS]. This CRC64 is used for Region TCC. \n\n
  Central CRC: RadioChannelCentral::calculateCRC creates CRC by using central polynomial, initial value and final XOR described in [FFFIS TCC-AOS].
  This CRC is used for central TCC.\n\n
  Note: The algorithm to calculate the CRC is generated by a Python Package called crcmod1.7. The 64 bit CRC is calculated based on the length of the message data. The message data is divided into different 4000 byte message chunks based on its length in
  order to calculate the number of message chunks. There will be a 64 bit CRC for each message chunk. The header will be included only along with the first message chunk for CRC calculation and
  the remaining CRC will be calculated without it.


  \section ClassDiagram Class Diagram

  @image html radio_channel_Class_Diagram.png "Class diagram"
  @image latex radio_channel_Class_Diagram.png "Class diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console-commands
  - The 'chstat'- command will display following parameters:
   +  List of channel names(connected/not connected), ChannelType, Number of Messages, Number of Bytes received.

  \subsection Analyze Analyze
  No values are registered for analysis for RadioChannel component.

  \section Checkpoints Checkpoints
  Checkpoints shall be inserted with the intention of verifying that the program flow of the ATP Software running on the
  diversified hardware (CPU-A and CPU-B) have taken the same execution path. Checkpoints have been inserted into the below functions
  + runIn()
  + runOut()

  \section CoreAdaptation Core / Adaptation
  RadioChannel does not have an adaptation part.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  The requirements relevant for this component are:

  Req        |Chapter                          | Function
  ---------- |---------------                  | --------
  AOS 1120 S |\ref runIn                       | RadioChannel::runIn()
  AOS 634    |\ref runIn                       | RadioChannel::runIn()
  AOS 3118   |\ref runIn                       | RadioChannel::runIn()
  AOS 636 S  |\ref runOut                      | RadioChannel::runOut()
  AOS 2481   |\ref Scheduling                  | RadioChannel::runIn(), RadioChannel::runOut()

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP Core. Only the architectural requirements traced explicitly to this component are included in the table below.
  Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

  Req             | Chapter         | Function
  --------------- | --------------- | --------
  AOS_AS-47 S     | \ref runIn      | RadioChannel::runIn()
  AOS_AS-50 S     | \ref runIn      | RadioChannel::runIn()
  AOS_AS-70 S     | \ref runIn      | RadioChannel::runIn()
  AOS_AS-71 S     | \ref runIn      | RadioChannel::runIn()
  AOS_AS-72 S     | \ref runIn      | RadioChannel::runIn()
  AOS_AS-88 S     | \ref runIn      | RadioChannel::runIn()
  AOS_AS-89 S     | \ref runIn      | RadioChannel::runIn()
  AOS_AS-90 S     | \ref runIn      | RadioChannel::runIn()

  */

}

