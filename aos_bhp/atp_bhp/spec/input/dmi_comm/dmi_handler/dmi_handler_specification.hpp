namespace ATP
{
namespace DMICom
{
/**
\if AsMainPage
\mainpage DMI Handler Component Specification(BHP Adaptation)
@anchor dh
\endif

\ifnot AsMainPage
\class DMIHandler
\endif

\section Purpose Purpose
This document specifies the software design for the DMIHandler class, the adaptation part of the DMI Handler component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
The DMIHandler adaptation component class shall inherit from the AbstractDMIHandler class.
The adaptation will create a singleton object of the DMIHandler class and provide access to it.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies

The DMIHandler adaptation component class and the inherited adaptation classes are dependent on below components-
- Mode Control: To provide the information about the current mode, travel direction and configuration type
- TSetup: To store the train storage data
- Tracks: To get track and position value from odometer value
- Targets: To retrieve and use the data related to targets
- Position: To get the safe trailing odometry position and current leading odometry position based on MA direction
- MessageHandler: To provide the data necessary to create the DMIMessages
- Event Handler: To report events

Other components have dependencies to this component because they use its public types
and methods, see \ref ATP::DMICom::DMIHandler Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization

+ Create message parsers (implemented in core) for incoming messages.
+ Fill a map container with the parsers with DMIMessageType as a key.
+ Create message creators (implemented in core/adaptation) for outgoing messages.
+ Fill a vector container with the parsers.

All the parsers for incoming messages are implemented in the core.
Most creators for outgoing messages are implemented in core but others are implemented in the adaptation. E.g. <br>
- DMIMessageOutATPModesAndStatusBHP
- DMIMessageOutRadioChannelBHP
- DMIMessageOutCeilingSpeedListBHP
- DMIMessageOutTypicalConfigBHP
- DMIMessageOutTrainWeightBHP
- DMIMessageOutETABHP

\subsection LoadStatus Train Loaded Status Requested By Driver

DMIHandler component implements the getTrainLoadedStatusRequestedByDriver() functionality that returns the train loaded status requested by driver.
This method is used by TrainConfigModeBHP class which is responsible for handling the confirmation on loaded status.

\subsection ModeDependentOperation Mode dependent operation
DMIHandler component is dependent on following mode dependent operations:
+ getCurrentMode() to be able to create mode dependent outgoing messages
+ getTrainConfigModeState() to be able to create train configuration mode state dependent outgoing messages

\subsection Scheduling Scheduling
N/A

\section ClassDiagram Class Diagram
@image html dmi_handler_class_diagram.png "DMIHandler class diagram"
@image latex dmi_handler_class_diagram.png "DMIHandler class diagram"
<br /><br /><br /><br /><br />
@image html DMIMessageOut_ClassDiagram.png "DMIMessageOut class diagram"
@image latex DMIMessageOut_ClassDiagram.png "DMIMessageOut class diagram"
<br /><br /><br /><br />
\section Diagnostics Diagnostics

\subsection Console Console commands
N/A

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
The adaptation of the DMIHandler component is responsible for creating parsers for incoming messages and creators for outgoing messages.

\section PreProcessor Pre-Processor Directives
N/A

\section Traceability Traceability

\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP BHP.

The requirements relevant for this component:

Req           |  Chapter             | Function
------------- | -------------------- | ---------------------------------------------------
AOS_BHPB 3349 | \ref LoadStatus      | DMIHandler::getTrainLoadedStatusRequestedByDriver
AOS_BHPB 3350 S | \ref Initialization  | DMIHandler::getTrainLoadedStatusRequestedByDriver()
AOS_BHPB 3351 | \ref Initialization  | DMIMessageOutATPModesAndStatusBHP::collectData()
AOS_BHPB 2816 | \ref Initialization  | DMIMessageOutRadioChannelBHP::validate()
AOS_BHPB 2815 | \ref Initialization  | DMIMessageOutATPModesAndStatusBHP::collectData()
AOS_BHPB 3188 | \ref Initialization  | DMIMessageOutATPModesAndStatusBHP::collectData()
AOS_BHPB 2541 | \ref Initialization  | DMIMessageOutETABHP::collectData()
AOS_BHPB 3117 | \ref Initialization  | DMIMessageOutETABHP::collectData()
AOS_BHPB 3363 | \ref Initialization  | DMIMessageOutATPModesAndStatusBHP::collectData()
AOS_BHPB 3357 | \ref Initialization  | DMIMessageOutTypicalConfigBHP::collectData()
AOS_BHPB 3118 | \ref Initialization  | DMIMessageOutATPModesAndStatusBHP::collectData()
AOS_BHPB 5082 | \ref Initialization  | DMIMessageOutATPModesAndStatusBHP::collectData()
AOS_BHPB 5084 | \ref Initialization  | DMIMessageOutATPModesAndStatusBHP::collectData()
AOS_BHPB 5085 | \ref Initialization  | DMIMessageOutATPModesAndStatusBHP::collectData()
AOS_BHPB 3317 | \ref Initialization  | DMIMessageOutATPModesAndStatusBHP::collectData()
AOS_BHPB 3318 | \ref Initialization  | DMIMessageOutATPModesAndStatusBHP::collectData()
AOS_BHPB 3319 | \ref Initialization  | DMIMessageOutATPModesAndStatusBHP::collectData()
AOS_BHPB 2629 | \ref Initialization  | DMIMessageOutATPModesAndStatusBHP::collectData()
AOS_BHPB 2768 S | \ref Initialization | DMIMessageOutATPModesAndStatusBHP::collectData()


\subsection SSAS Architectural requirements
The architectural requirements are defined in [SSAS-APP] and are traced and described in the core SCDS.

*/
}
}
