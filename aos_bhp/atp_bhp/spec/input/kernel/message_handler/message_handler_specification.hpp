namespace ATP::Kernel
{
/**
\if AsMainPage
\mainpage Message Handler Component Specification (BHP adaptation)
@anchor mh
\endif

\ifnot AsMainPage
\class Message Handler
\endif

\section Purpose Purpose
This document specifies the software design for the MessageHandler class, the BHP adaptation part of the Message Handler component.

\latexonly \newpage \endlatexonly

\section Overview Overview
MessageHandler instantiates the Message Handler component for the supported messages. The messages in the TCC-AOS interface that are needed by the
adaptation are implemented in the adaptation of Message Handler. These adaptation messages are defined in [FFFIS TCC-AOS-BHP].

\subsection GeneralFunctionality General Functionality

The virtual functions runIn() and runOut() are not overridden in the MessageHandler class.
Instead access functions are implemented specific to the project configuration. Configuration version is received and made 
available to other components. Adaptation specific block are parsed and published.

MessageHandler handles transmission of the following information to TCC:
 - \ref TrainSetup
 - \ref PositionReport
 - \ref PathMessage
 - \ref StartUpMessage
 - \ref CommandMessage
 - \ref ConfigMessage

The class is also responsible for overriding the adhesion values from a Movement Authority message so that it would always return the BHP specific value.

MessageHandler provides interfaces for validation functions and validation logic for messages with BHP specific contents.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
The adaptation part of Message Handler depends on the following components:
  + Train Setup provides information about the train load status when sending startup message.
  + Brake provides information about rapid loss of brake pressure when sending position report.
  + Mode Control in order to set the static configuration version.
  + Tracks to get current position or validation of tracks.
  + Targets to override the adhesion value received from the MA.

Other components have dependencies to this component because they use its public types and methods, see MessageHandler Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection Initialization Initialization

The virtual init() - function does following:
 - Instantiates the parser classes that parse all messages sent from TCC to AOS.
 - Instantiates the creator classes that create all messages sent from AOS to TCC.

\subsection ModeDependentOperation Mode Dependent Operation

MessageHandler class is independent of ATP mode.

\subsection Scheduling Scheduling

The virtual run(), runIn() and runOut() functions are not overridden in the adaptation.

\subsection Messages Adaptation Message Handling
Some of the messages requires adaptation and are mentioned below.

\subsubsection TrainSetup Train Setup Message
BHPB_CONFIG_VERSION, a block for the static configuration version used by TCC track layout, is parsed and published.
The configuration version is received and made available to other components. Also the longest length of consecutive
cars (not including any loco) is calculated by finding the maximum number of cars without loco in between. 
This figure is then multiplied by train length and divided by total number of cars in order to get the longest 
length of consecutive cars. This length is sent to the TrainConfig component.

\subsubsection PositionReport Position Report
The Position Report message has two new blocks to set for the adaptation, they are parsed and published.
- B_BHPB_TRAIN_STATUS, a block to indicates whether "Rapid loss of brake pressure" has been detected.
- BHPB_SET_APPROACH_SPEED, a block containing the ceiling speed set by AOS approaching a level crossing.

\subsubsection PathMessage Path Message
BHPB_CONFIG_VERSION, a block for the static configuration version used by TCC track layout, is parsed and published.

\subsubsection StartUpMessage StartUp Message
BHPB_LOAD_STATUS, a block for Load status set by driver is added to the startup message.


\subsubsection CommandMessage Command Message
The Command message has two new blocks to set for the adaptation that are parsed and published:
 - BHPB_RADIO_CHANNEL, the radio channel name to be used by locomotive driver.
 - BHPB_SAFE_FOR_BOARDING, safe for boarding is active, see also \ref ATOPrep.

\subsubsection ConfigMessage Configuration Message
Validate the configuration against the list of five typical car configurations.

\subsection ATOPrep ATO Support Functions
There is one function created for the sole purpose of supporting a future implementation of ATO: MessageHandler::getBHPBSafeForBoarding(). This function is not used until ATO support is included.

\section ClassDiagram Class Diagram
@image html message_handler_class_diagram.png "MessageHandler class diagram"
@image latex message_handler_class_diagram.png "MessageHandler class diagram"

For a full description of the classes, see MessageHandler
- Adaptation Parsers
  - RadioMessageInCommandMessageBHP
  - RadioMessageInConfigurationDataBHP
  - RadioMessageInMovementAuthorityBHP
  - RadioMessageInPathBHP
  - RadioMessageInTrainSetupBHP

- Adaptation Creators
  - RadioMessageOutPositionReportBHP
  - RadioMessageOutStartUpMessageBHP	

\section Diagnostics Diagnostics
\subsection Console Console Commands
N/A

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
This is the adaptation part of the Message Handler component. The main area of responsibility is to instantiate and initialize the component, and the parsers and creators 
chosen to be used by the adaptation, see also \ref Overview.

\section PreProcessor Pre-Processor Directives
N/A

\section Traceability Traceability
N/A

\subsection SSRS Functional Requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP BHP.

The requirements relevant for this class are:

  Req           | Overriden Req | Chapter                   | Function
--------------- | -----------   |-               ---------- | --------
AOS_BHPB 2947   | AOS 2174 S    | \ref GeneralFunctionality | RadioMessageInMovementAuthorityBHP::publishAdhesion()
AOS_BHPB 3136 S |  -            | \ref PositionReport       | RadioMessageOutPositionReportBHP::collectData()
AOS_BHPB 3351   |  -            | \ref StartUpMessage       | RadioMessageOutStartUpMessageBHP::collectData()
AOS_BHPB 3358   |  -            | \ref ConfigMessage        | RadioMessageInConfigurationDataBHP::validateData()
AOS_BHPB 3420 S |  -            | \ref PositionReport       | RadioMessageOutPositionReportBHP::assembleAdditionalBlocks()
AOS_BHPB 2624   |  -            | \ref PathMessage          | MessageHandler::getStaticConfigurationVersionInPath()
AOS_BHPB 5017   |  -            | \ref StartUpMessage       | RadioMessageOutStartUpMessageBHP::collectData()
AOS_BHPB 5079 S |  -            | \ref TrainSetup           | RadioMessageInTrainSetupBHP::calculateBrakeParameters()

\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP BHP.

Fulfillment of other architectural requirements allocated to the ATP is described in [SWAS].

*/
}