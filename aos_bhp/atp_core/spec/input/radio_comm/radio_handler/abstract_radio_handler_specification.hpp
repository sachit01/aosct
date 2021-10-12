namespace ATP::RadioCom
{

  /**
  \if AsMainPage
  \mainpage Radio Handler (Core) Component Specification
  @anchor rh
  \endif

  \ifnot AsMainPage
  \class RadioHandler
  \endif

  \section Purpose Purpose
  This document specifies the software design for core part of the Radio Handler component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview
  \subsection GeneralFunctionality General Functionality
  The core class AbstractRadioHandler manages several RadioChannel instances to handle communication with several TCCs.
  However, an adaptation class derived from AbstractRadioHandler must create and initialize these instances of RadioChannel.

  AbstractRadioHandler is responsible for:
  - Forwarding messages received from Region TCC to Message Handler.
  - Responding to a received message from Region TCC by sending a Position Report or any pending outgoing message.
  - Responding to Protocol Version received from Region TCC by sending Protocol Version.
  - Responding to Area Request received from Central TCC by sending Registration Area.

  \subsection DeploymentDiagram Deployment Diagram
  @image html abstract_radio_handler_objects.png "Deployment diagram"
  @image latex abstract_radio_handler_objects.png "Deployment diagram"

  \subsection Dependencies Dependencies
  AbstractRadioHandler depends on the following components:
  - Radio Channel: AbstractRadioHandler exchanges messages with the Central and Region TCCs via one instance of RadioChannelCentral
    and two instances of RadioChannel.
  - Message Handler: To retrieve pending messages or Position Report from ATP to respond to messages from Region TCCs.
  - Message Handler: To retrieve region selected by driver and respond to Area Request from Central TCC.
  - Message Handler: To retrieve Protocol Version response to respond to Protocol Version message from Region TCCs.

  Other components depend on this component's functionality by using its public functions.
  Refer to Public Member Functions under \ref AbstractRadioHandler.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design
  \subsection Initialization Initialization
  AbstractRadioHandler does not have an init() function because it leaves the initialization to the adaptation class.

  \subsection ModeDependentOperation Mode Dependent Operation
  N/A

  \subsection Scheduling Scheduling
  The function AbstractRadioHandler::run() must be called in each execution cycle.

  \subsubsection run run()
  AbstractRadioHandler::run() performs the following:
  - Calls RadioChannel::runIn() for each instance of RadioChannel.
  - For each connected RadioChannel:
    - If a message from Central TCC has been received, calls AbstractRadioHandler::handleCentral() which enqueues a Registration Area
      message in response to an Area Request message (see \ref AreaRequestHandling).
    - If a message from a Region TCC has been received, calls AbstractRadioHandler::handleRegion() which responds by enqueuing
      a pending outgoing message from Message Handler or a default message (Position Report or Protocol Version,
      see \ref PositionReportHandling and \ref ProtocolVersionHandling).
    - Calls RadioChannel::runOut() to send enqueued messages to the respective TCC.
  - Determine if ATP is allowed to enter ATP mode Yard, by checking the connection status of the radio channels.

  \subsection ReceivingMessages Receiving TCC Messages
  AbstractRadioHandler provides the function AbstractRadioHandler::readMessage() which dequeues and returns the first TCC message
  received (if any) by a given RadioChannel instance.

  \subsection PositionReportHandling Position Report Handling
  AbstractRadioHandler responds to any message arriving from a Region TCC with either a Position Report prepared
  by Message Handler or any other message pending in the output queue in Message Handler.

  \subsection ProtocolVersionHandling Protocol Version Handling
  AbstractRadioHandler responds to a Protocol Version message arriving from a Region TCC with a Protocol Version message
  retrieved from Message Handler. The Region TCC handles any mismatch of protocol versions between AOS and the TCC itself.

  \subsection AreaRequestHandling Area Request and Registration Area Handling
  AbstractRadioHandler will only respond with a Registration Area message prepared by Message Handler to an Area Request message
  arriving from the  Central TCC, if any Registration Area has been selected by the driver. Otherwise, no response at all
  is sent to the Central TCC.
  The Central TCC will keep sending Area Request messages until a Registration Area message is received from AOS.

  \section ClassDiagram Class Diagram

  @image html abstract_radio_handler_class_diagram.png "Class diagram"
  @image latex abstract_radio_handler_class_diagram.png "Class diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console-commands
  N/A

  \subsection Analyze Analyze
  AbstractRadioHandler does not register any variables for analysis.

  \section CoreAdaptation Core / Adaptation
  The adaptation part of the Radio Handler component must create one RadioChannel instance per required radio channel
  supported (one RadioChannel per simultaneously connected TCC).

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives are available for this component.

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP Core.

  The requirements relevant for this component are:

  Req        | Chapter                                       | Function
  ---------- | --------------------------------------------- | --------
  AOS 138    | \ref Scheduling                               | AbstractRadioHandler::run()
  AOS 632    | \ref Scheduling                               | AbstractRadioHandler::run()
  AOS 637    | \ref PositionReportHandling                   | AbstractRadioHandler::handleRegion()
  AOS 757    | \ref PositionReportHandling                   | AbstractRadioHandler::handleRegion()
  AOS 1019   | \ref Scheduling                               | AbstractRadioHandler::run()
  AOS 1120 S | \ref Scheduling <br /> \ref ReceivingMessages | AbstractRadioHandler::run() <br /> AbstractRadioHandler::readMessage()
  AOS 2481   | \ref Scheduling                               | AbstractRadioHandler::run()

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in the SCDS ATP Core.


  */

}

