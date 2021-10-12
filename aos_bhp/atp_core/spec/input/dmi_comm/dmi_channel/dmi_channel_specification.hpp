namespace ATP::DMICom
{
  /**
  \if AsMainPage
  \mainpage DMI Channel Component Specification
  @anchor dc
  \endif

  \ifnot AsMainPage
  \class DMIChannel
  \endif

  \section Purpose Purpose
  This document specifies the software design of the DMIChannel class.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality
  The DMI Channel component handles the reception and transmission of the DMI messages to/from Dispatcher
  via VFW channels. DMI Channel performs verification of incoming messages and assembly of outgoing messages(checksum, etc).
  
  The DMIChannel handles -
  - Protocol details, such as checksum and acknowledge as described in [IFS_DMI].
  - A message \a in \a queue which stores the messages arriving from DMI.
  - A message \a out \a queue for storing the messages which need an 'acknowledge' as response from DMI.
  - A message \a out \a queue for storing the messages which need 'no acknowledge' as response from DMI.

  DMI Channel sends one message at a time from the 'message out queue' and waits for an acknowledge of the same before the next message can be sent.
  The DMI as a response will send only one message at a time needing acknowledge.
  Whereas, DMI Channel sends all messages available in the 'message out queue which needs no ack' immediately to DMI.
  DMI Handler creates one instance of DMIChannel per DMI to communicate.

  It is to be noted that DMI Handler receives messages from the Dispatcher on synchronized VFW channels and
  sends messages to Dispatcher on unsynchronized VFW channels.

  \subsection DeploymentDiagram Deployment Diagram
  @image html abstract_dmi_handler_object_diagram.png "Deployment diagram"
  @image latex abstract_dmi_handler_object_diagram.png "Deployment diagram"

  \subsection Dependencies Dependencies
  This component has dependencies to the below components:
  + Cross Compare : To cross compare data received from and sent to DMI via Dispatcher.
  + EventHandler: To report events

  Other components have dependencies to this component because they use its public types
  and methods, see \ref ATP::DMICom::DMIChannel Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization

  It includes calling of preInit() and init() methods:

  The preInit() method performs initialization such as:
    + Open a channel to be used when reading from Dispatcher
    + Open a channel to be used for writing by calling the initChannel of cross compare with the channel name
    + Synchronize the read channel with the corresponding diversified channel (A/B)
    + Deactivate the event driven callback functionality 

  The Init() method performs initialization such as:
    + Initialize the channel statistics for the component
    + Prepare any values for cross-compare
    + Add the component to the Console Trace Vector

  \subsection ModeDependentOperation Mode dependent operation
  N/A

  \subsection Scheduling Scheduling

  The DMIChannel component has these function(s) that must be called each execution cycle:
  - runIn()
  - runOut()

  \subsubsection runIn runIn()
  DMIChannel reads the incoming packets from the synchronized VFW channel (until no more DMI message is available to read).
  DMIChannel reads one message at a time. The successfully read messages are extracted for the message headers of type: 'Unacknowledged msg', 'Acknowledged msg' and 'Acknowledge'. 
  If header type is none of these three then it is invalid DMI message. If the received DMI message type is the one that is ack message itself (i.e. Acknowledge) and it is the correct ack message 
  then DMIChannel updates the transmit mode to allow sending a new acknowledged message. Unknown received ack messages are reported as log messages.

  DMI messages received are pushed to the In queue. Connection is monitored through timers.
  DMI messages are discarded if the DMI channel is not active. For detailed description of the runIn() method refer below flow-chart :

  @image html DMI_Channel_runIn.png "DMIChannel::runIn()"
  @image latex DMI_Channel_runIn.png "DMIChannel::runIn()"


  \subsubsection runOut runOut()
  DMIChannel::runOut() method sends any pending acknowledge, prepared by runIn(),
  back to the DMI as a response to a received DMI message needing acknowledge. 
  It maintains one message out queue with messages  'needing acknowledge' and another
  message out queue for messages 'not needing acknowledge' as response from DMI. The DMI messages needing acknowledge shall be sent one by one waiting 
  for each message to be acknowledged before the next message can be sent.

  DMIChannel performs the handling of messages 'needing acknowledge' by three transmit mode states:
  - transmit mode 'Idle', where new message (needing acknowledge) can be sent to DMI via Dispatcher
  - transmit mode 'Waiting ack', where a message has already been sent and expecting an ack within timeout
  - transmit mode 'Resend', where the ack did not arrive within timeout. Need to resend the last (saved) message

  DMIChannel sends all pending messages available in the out queue with messages 'not needing any acknowledge'.
  
  Timers are used to keep the track of the ack messages to receive the response within timeout.
  All this handling is done when DMI is active. For more details , refer the flow-chart below:


  @image html DMI_Channel_runOut.png "DMIChannel::runOut()"
  @image latex DMI_Channel_runOut.png "DMIChannel::runOut()"


  \subsubsection PackingOfOutgoingMessage Packing of outgoing message

  DMIChannel::packMsg() method is used to pack the outgoing DMI messages or Ack-message according to section 4(DMI Communication Layer) of the [IFS_DMI].
  The message header and message data is written to the out buffer in network order using vfw-functions.
 
  Finally a CRC is calculated for the header and data and written as a trailer in network order.
  The structure of the packets flowing to/from the DMI should be according to the section 4(DMI Communication Layer) of the [IFS_DMI].


  \subsubsection extractMessage extractMessage()
  The function DMIChannel::extractMessage() extracts the message received from DMI onto the object member 
  variables and returns true if message was extracted and validated successfully.

  Vfw-functions are used to convert the DMI message data from host order to network byte order. 
  The raw DMI message data is stored in write buffer in read mode.
  If the DMI message data is not the one from the below 3 type:
    - Message which does not need an ack
    - Message which needs an ack
    - Ack message Itself,

  then incoming DMI message is not valid.

  Finally a CRC is calculated for the header and data to be read in network order.

  \subsubsection readFromChannel readFromChannel()
  + This function reads the data from the DMIChannel via VFW and writes it to the local buffer: the InBuffer.
  This method is called in runIn() function to read the incoming DMI messages until there are no more messages 
  available to read.

  For more details refer the below flow-chart:

  @image html DMI_Channel_readFromChannel.png "DMIChannel::readFromChannel()"
  @image latex DMI_Channel_readFromChannel.png "DMIChannel::readFromChannel()"


  \subsection CRCCalculation CRC calculation
  The CRC is calculated as a 32-bit CRC using 0 as initial value. See [IFS_DMI] for more info.

  \section ClassDiagram Class Diagram

  DMIHandler creates as many instances of the DMIChannel as is required by the adaptation for the project.
  DMIChannel does not have an adaptation part yet.

  @image html DMI_Channel_class_Diagram.png
  @image latex DMI_Channel_class_Diagram.png

  \section Diagnostics Diagnostics

   \subsection Console Console-commands
  The 'dmich'- command will display following parameters:
  - DMIChannel 'channel id' 'active/not active' and 'connected/not connected'.

  The 'chstat'- command will display following parameters:
  - List of channel names(connected/not connected), ChannelType, No of Messages, No of Bytes received.

  \subsection Analyze Analyze
  No values are registered for analysis for DMIChannel component.

  \section CoreAdaptation Core / Adaptation
  The virtual functions of the DMIChannel core component can be overridden 
  to extend the functionality.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component.

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP Core.

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP Core. Only the architectural requirements traced explicitly to this component are included in the table below.
  Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

  Req             | Chapter         | Function
  --------------- | --------------- | --------
  AOS_AS-47 S     | \ref runIn      | \ref DMIChannel::runIn()
  AOS_AS-50 S     | \ref runOut     | \ref DMIChannel::runOut()
  AOS_AS-70 S     | \ref runIn      | \ref DMIChannel::runIn()
  AOS_AS-71 S     | \ref Initialization | \ref DMIChannel::init()
  AOS_AS-72 S     | \ref Initialization | \ref DMIChannel::init()
  AOS_AS-88 S     | \ref runOut     | \ref DMIChannel::runIn()
  AOS_AS-89 S     | \ref runOut     | \ref DMIChannel::runIn()
  AOS_AS-90 S     | \ref extractMessage | \ref DMIChannel::extractMessage()

  */
}