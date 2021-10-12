namespace ATP
{
namespace DMICom
{
/** 
\if AsMainPage
\mainpage DMI Handler Component Specification
@anchor dh
\endif

\ifnot AsMainPage
\class AbstractDMIHandler
\endif

\section Purpose Purpose
This document specifies the software design for the AbstractDMIHandler class, the core part of the DMI Handler component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
The AbstractDMIHandler component is handling the reception and sending of DMI messages to/from the active DMI Channels (each DMI Channel is handled by a DMIChannel object).

The AbstractDMIHandler component shall provide the following functionality:

- Creates two DMI Channels.
- Reads/writes DMI messages from/to the active DMI Channel.
- Parses incoming DMI messages and publishes data to other components (or in some case stores in data storages components).
- Collects data for outgoing DMI messages published by other components (or in some case reads from data storage components).
- Creates outgoing DMI messages.

In general, DMIHandler receives messages from the Dispatcher on synchronized VFW-channels and 
sends messages to Dispatcher on unsynchronized VFW-channels.

\subsection DeploymentDiagram Deployment Diagram
@image html abstract_dmi_handler_object_diagram.png "Deployment diagram"
@image latex abstract_dmi_handler_object_diagram.png "Deployment diagram"

\subsection Dependencies Dependencies
This component has dependencies to the following components:

+ ModeControl: To provide the information about the current mode, travel direction and configuration type
+ TSetup: To store the train storage data
+ Tracks: To get track and position value from odometer value
+ Targets: To retrieve and use the data related to targets
+ Position: To get the safe trailing odometry position and current leading odometry position based on MA direction
+ MessageHandler: To retrieve information about incoming messages from TCC
+ EventHandler: To report events

Other components have dependencies to this component because they use its public types
and methods, see \ref ATP::DMICom::AbstractDMIHandler Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization
Initialization includes calling of preInit() and init() methods.

\subsubsection preInit preInit()

The preInit() method of AbstractDMIHandler calls the preInit() of the two DMI Channel objects.

\subsubsection init init()

The init() method performs below actions such as:
  + Calls init() method of each DMI Channel instance.
  + Adds the DMI Channel instances in the vector of DMI Channels handled by the DMI Handler.
  + Adds the DMI Channel instances to the list of components handled by the ATP Application.
  + Performs cross-compare initialization of the variables.

\subsection  ModeDependentOperation Mode Dependent Operation
AbstractDMIHandler component and/or parser classes are dependent on public functions of AbstractModeControl component.
Few mode dependent operations are:
+ getCurrentMode() and getCurrentModeState() to be able to create outgoing messages and validate incoming messages.
+ getActiveCab() to read the active cabin number in order to select the active DMIChannel.
+ getStartUpPassed() to check if start-up of ATP completed.

\subsection Scheduling Scheduling

The AbstractDMIHandler has two functions (inherited from the base-class IOComponent), runIn() and runOut(), that must be called each execution-cycle:
\subsubsection runIn runIn()

runIn() is called by ATP Application and shall handle incoming data :
+ For each dmiMessageInParser, calls invalidate() to invalidate the old DMI message data.
+ Once the start-up of ATP completed, gets active cabin from ModeControl component and activates the DMI Channels based on activeCabinNumber.
+ Calls DMIChannel::runIn() function for each DMIChannels.
+ Calls DMIChannel::readMessage() to read DMIMessage from the active DMIChannel.
+ Extracts the DMI message type and calls its associated parser.
+ Validates the incoming data. Incoming data after validated 'Ok' is published 
to be used by other components or stored in data storage components.
+ Stores the Tachometer and Doppler failure values from odometry and uses these values for the confirmation if required by the Driver.
+ Handles DMI buttons related to odometer failure which will be used by Driver for the confirmation.

@image html abstract_dmi_handler_runin.png "AbstractDMIHandler::runIn()"
@image latex abstract_dmi_handler_runin.png "AbstractDMIHandler::runIn()"

\subsubsection runOut runOut()

runOut() is called by ATP Application and shall handle outgoing data :
+ Performs conditional check if DMI Channel is active.
+ For each dmiMessageOutCreator, calls invalidate() to invalidate the outgoing messages.
+ Collects DMIMessageType and mode dependent data from other components.
+ Validates the collected input data and creates the outgoing message in network-byte-order.
+ Writes DMI message to the Active Channel.
+ Calls DMIChannel::runOut() function to Schedule transmission of all outgoing messages on the active DMI channel.

@image html abstract_dmi_handler_runout.png "AbstractDMIHandler::runOut()"
@image latex abstract_dmi_handler_runout.png "AbstractDMIHandler::runOut()"


\subsection DMIMessages DMI Messages
Incoming and Outgoing DMI messages are handled by AbstractDMIMessageIn and AbstractDMIMessageOut classes respectively.

\subsubsection IncomingMessages Incoming DMI Messages
Parser classes that manages incoming DMI messages and provides interfaces to other components are listed below:
+ DMIMessageInDriverIDandPassword : to provide driver Id and Password which will be used to send login information to TCC.
+ DMIMessageInDMIStartup : to provide the DMI start-up status which will be used during registration process.
+ DMIMessageInDMIStatus : to provide the DMI status word which is used to log the message on RU.
+ DMIMessageInDMIToATPData : to provide the driver's interaction by implementing DMI button status which will be used by other components.
+ DMIMessageInLocoVsTrainDir : to provide the loco Vs train direction.
+ DMIMessageInVehicleData : to provide the DMI vehicle data which implements the functionality to know whether manual train setup is confirmed by driver in DMI.
+ DMIMessageInConfirmation : to provide the confirmation status by DMI which will be used during train configuration to accept or reject train setup.
+ DMIMessageInTrainVsTrackDir : to provide the train orientation which is used during train registration process.
+ DMIMessageInRegistrationArea : to provide the incoming registration area which will be used to update the area based on area ID selected by the driver.
+ DMIMessageInTrainName : to provide the incoming train name message which implements the functionality for changed train name if requested by driver.
+ DMIMessageInTrainLoaded : to provide the incoming train loaded status which implements the functionality to provide the train loaded status requested by the driver during train configuration.

\subsubsection OutgoingMessages Outgoing DMI Messages
Creator classes that creates outgoing DMI messages are listed below:
+ DMIMessageOutSpeedAndDistance : to collect the speed and distance data from odometry and tracks for creating outgoing 'Speed and Distance' DMI message.
+ DMIMessageOutReRegSelected : to collect the tims related data during TrainConfig mode which will be used for creating outgoing 'Re-Registration selected' DMI message.
+ DMIMessageOutTrainConfigData : to collect the valid train name, train length, tims related data, balise antenna position based on mode-dependent data for creating outgoing 'Train Configuration' DMI message.
+ DMIMessageOutVehicleData : to collect vehicle data based on required mode for creating outgoing 'Vehicle' DMI message.
+ DMIMessageOutGradientDataList : to collect gradient data block for creating outgoing 'Gradient Data List' DMI message.
+ DMIMessageOutDriverInfo : to collect driver info - driving direction, permitted speed, target speed, time to intervention, remaining distance to target point, remaining distance to BCA etc to create outgoing 'Driver Info' DMI message.
+ DMIMessageOutErasePlanningArea : to collect erase planning data for creating the outgoing 'Erase planning Area' DMI message.
+ DMIMessageOutLocoVsTrainDir : to collect train orientation data during TrainConfig mode which will be used for creating the outgoing 'Loco Vs Train Dir' DMI message.
+ DMIMessageOutManualConfigSelected : to collect data for manual configuration which will be used outgoing 'Manual Configuration Selected' DMI message.
+ DMIMessageOutTrainName : to collect train name data from train setup for creating outgoing 'Train Name' DMI message.
+ DMIMessageOutTrainVsTrackDirWanted : to collect train Vs track direction during train registration process for creating outgoing 'Train Vs Track Dir' DMI message.
+ DMIMessageOutVersion : to collect the DMI compatibility version for creating outgoing 'Version' DMI message.
+ DMIMessageOutAtpNotification : to collect the info associated with the reject configuration for creating outgoing 'ATP Notification' DMI message.
+ DMIMessageOutDMIStartupHistory : to collect the start-up history data for creating outgoing 'Startup History' DMI message.
+ DMIMessageOutTextMessage : to collect any event for DMI for creating outgoing 'Text Message' DMI message.
+ DMIMessageOutPredefinedTextMessage : to collect the reason for emergency alert info for creating outgoing 'Predefined Text' DMI message.
+ DMIMessageOutAreaRequest : to collect the information if there is an AreaRequest from TCC where driver is requested to select one registration area. DMI responds with the selected area with outgoing 'Area Request' DMI message.
+ DMIMessageOutTime: to collect the system time from TCC which is to be  updated on ATP before it is sent to DMI. DMI Handler sends this information with outgoing 'Out Time' DMI message.
+ DMIMessageOutVehicleTypes : to collect the vehicle type names from Configuration component for creating outgoing 'Vehicle Type' DMI message.
+ DMIMessageOutLocationData : to collect location name and location type from Location mode for creating outgoing 'Location Data' DMI message.

\subsubsection AreaRequest Area Request Handling
TCC sends an Area Request message to AOS including the available registration areas.
These registration-areas are published by MessageHandler and retrieved by DMI Handler
and forwarded to the DMI in the  AreaRequest-message. The driver is requested to select one registration area. DMI responds with the selected area in the Registration Area message to DMIHandler which is publishing the selected area to be retrieved by MessageHandler.


\subsection handleOdometerFailures Handling of Odometer Failures
The AbstractDMIHandler implements the functionality to handle odometer failures. The DMI Handler component fetches the status of the
'Tachometer1' and 'Tachometer2' failures from AbstractOdometry component. If they are failed then DMI Handler stores the failure status until the driver confirms 
the failure. Similarly, it fetches the status of the 'Doppler' failure, and stores the status value until it is confirmed by the driver.

\subsection getDriverOdoFailDMIButtonConfirmation  Handling of DMI buttons related to odometer failure
The AbstractDMIHandler implements getDriverOdoFailDMIButtonConfirmation() method to handle the DMI buttons related to odometer failure.
Based on DMI button status fetched from getDMIButtonStatus() method, DMI Handler sets the value of driver's confirmation on 'Tachometer1', 'Tachometer2' and 'Doppler' failures.

\section ClassDiagram Class Diagram

@image html abstract_dmi_handler_class_diagram.png "AbstractDMIHandler class diagram"
@image latex abstract_dmi_handler_class_diagram.png "AbstractDMIHandler class diagram"
<br /><br /><br /><br />
@image html abstract_dmi_messageIn_ClassDiagram.png "AbstractDMIMessageIn class diagram"
@image latex abstract_dmi_messageIn_ClassDiagram.png "AbstractDMIMessageIn class diagram"
<br /><br /><br /><br />
@image html abstract_dmi_messageOut_ClassDiagram.png "AbstractDMIMessageOut class diagram"
@image latex abstract_dmi_messageOut_ClassDiagram.png "AbstractDMIMessageOut class diagram"
<br /><br /><br /><br />
To allow for flexible core/adaptation the AbstractDMIHandler delegates the parsing of incoming DMIMessages to instances
of classes inherited from AbstractDMIMessageIn. Pointers to the base class of the "parsers" are kept in the map
where the DMIMessageType serves as a key in the map to locate the proper parser. Non Defined parser of the DMIMessageType
should report an error.

In a similar way DMIMessage creation is handled by "creators" inherited from AbstractDMIMessageOut. Each creator
has a collectData() function where the data for the DMIMessageType shall be collected from other components/data Storage.

\section Diagnostics Diagnostics

\subsection Console Console commands
The command 'help' provides the DMI channel connection related information.

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation

+ The Core of the DMIHandler component creates two DMIChannel (one DMIChannel per simultaneously connected DMI)
+ The Adaptation of the DMIHandler component is responsible for creating parsers for incoming messages and creators for outgoing messages.

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component.

\section Traceability Traceability

\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP Core.

The requirements relevant for this component are:

Req        | Chapter             | Function       
---------- | --------------------|-------------------------------------------------------------------------------------
AOS 769    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 823    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 1093 S |  \ref runOut        | DMIMessageOutTextMessage::assembleDMIMessageData() <br /> DMIMessageOutTextMessage::collectData() <br /> DMIMessageOutTextMessage::validate()
AOS 656    |  \ref runIn         | AbstractDMIHandler::startupAndHealthSupTest()
AOS 1705   |  \ref runOut        | DMIMessageOutAtpNotification::collectData()
AOS 1261   |  \ref runOut        | DMIMessageOutATPModesAndStatus::getATPModeSubState()
AOS 46 S   |  \ref runIn         | DMIMessageInVehicleData::validate()
AOS 2448   |  \ref runIn         | DMIMessageInTrainName::getChangedTrainName()
AOS 505    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 506 S  |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 1695   |  \ref runOut        | DMIMessageOutATPModesAndStatus::getConfirmModeChange()
AOS 2462   |  \ref runOut        | DMIMessageOutATPModesAndStatus::getConfirmModeChange()
AOS 1839   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2464   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2560   |  \ref runIn         | DMIMessageInDMIToATPData::validate()
AOS 2465   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2466   |  \ref runIn         | DMIMessageInDMIToATPData::validate()
AOS 2935   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2936   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2937   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 1762 S |  \ref runOut        | DMIMessageOutATPModesAndStatus::getATPModeSubState
AOS 793    |  \ref runOut        | DMIMessageOutTrainConfigData::collectData()
AOS 794    |  \ref runOut        | DMIMessageOutTrainConfigData::collectData()
AOS 2564   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2569   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2574   |  \ref runIn         | DMIMessageInDriverIDandPassword::validate()
AOS 2449   |  \ref runIn         | DMIMessageInTrainName::validate()
AOS 454    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 808    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 786    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 785    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 820    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 827    |  \ref runOut        | DMIMessageOutTime::collectData()
AOS 848    |  \ref runOut        | DMIMessageOutCeilingSpeedList::collectData()
AOS 851    |  \ref runOut        | DMIMessageOutCeilingSpeedList::collectData()
AOS 852    |  \ref runOut        | DMIMessageOutCeilingSpeedList::collectData()
AOS 853    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 859    |  \ref runOut        | DMIMessageOutCeilingSpeedList::collectData()
AOS 861    |  \ref runOut        | DMIMessageOutGradientDataList::collectData()
AOS 864    |  \ref runOut        | DMIMessageOutGradientDataList::collectData()
AOS 867    |  \ref runOut        | DMIMessageOutGradientDataList::collectData()
AOS 868    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 872    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 873    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 469    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2608   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 874    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 875    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 876    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 878    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 879    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2300   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 1811 S |  \ref runIn         | DMIMessageInDMIToATPData::validate()
AOS 2290   |  \ref runOut        | DMIMessageOutATPModesAndStatus::validate()
AOS 2984   |  \ref runOut        | DMIMessageOutTime::collectData()
AOS 3148   |  \ref runOut        | DMIMessageOutTime::collectData()
AOS 3140   |  \ref runOut        | DMIMessageOutTime::collectData()
AOS 3150   |  \ref runOut        | DMIMessageOutTime::collectData()
AOS 1738   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 1019   |  \ref runOut        | DMIMessageOutATPModesAndStatus::getConfirmModeChange
AOS 1664   |  \ref runOut        | DMIMessageOutATPModesAndStatus::getConfirmModeChange
AOS 658    |  \ref runOut        | DMIMessageOutATPModesAndStatus::getConfirmModeChange
AOS 2803   |  \ref runOut        | DMIMessageOutTrainConfigData::collectData()
AOS 2804   |  \ref runOut        | DMIMessageOutTrainConfigData::collectData()
AOS 1245 S |  \ref runOut        | DMIMessageOutATPModesAndStatus::getConfirmModeChange
AOS 2824 S |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2135   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2136   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2978   |  \ref runOut        | DMIMessageOutATPModesAndStatus::getConfirmModeChange
AOS 122    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2413   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 19     |  \ref runIn         | DMIMessageInDMIStartup::getDMIStartupStatus()
AOS 2611   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2615 S |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2626   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2708 S |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2638   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2636 S |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2642   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 1212   |  \ref AreaRequest   | DMIMessageInRegistrationArea::validate()
AOS 1213   |  \ref AreaRequest   | DMIMessageInRegistrationArea::validate()
AOS 2196   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2106   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 400    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 486    |  \ref runOut        | DMIMessageOutAtpNotification::collectData()
AOS 835    |  \ref runOut        | DMIMessageOutTextMessage::collectData()
AOS 840    |  \ref runOut        | DMIMessageOutTextMessage::collectData()
AOS 844    |  \ref runOut        | DMIMessageOutCeilingSpeedList::collectData()
AOS 810    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 813    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 869    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 817    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2998   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2647   |  \ref runOut        | DMIMessageOutTrainConfigData::collectData()
AOS 2570   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 3219 S |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2769   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2676   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2677   |  \ref runOut        | DMIMessageOutVehicleData::collectData()
AOS 1709   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 678    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 679    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 1772 S |  \ref runIn         | DMIMessageInDMIStartup::getDMIStartupStatus()
AOS 1773 S |  \ref runIn         | DMIMessageInDMIStartup::validate()
AOS 2483   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 1037   | \ref Console        | AbstractDMIHandler::consoleCall()
AOS 3177   |  \ref runIn         | DMIMessageInVehicleData::validate()
AOS 1758   |  \ref runIn         | DMIMessageInConfirmation::parseDMIMessageData()
AOS 2941   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2510   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2513   |  \ref runOut        | DMIMessageOutTextMessage::collectData()
AOS 2514   |  \ref runOut        | DMIMessageOutTextMessage::collectData()
AOS 2629 S |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2641   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2981   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 2291   |  \ref runIn         | AbstractDMIHandler::runIn()
AOS 2015   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 858    |  \ref runIn         | AbstractDMIHandler::runIn()
AOS 775    |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()
AOS 3297   |  \ref runOut        | DMIMessageOutATPModesAndStatus::collectData()

\subsection SSAS Architectural requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP Core.

Only the architectural requirements traced explicitly to this component are included in the table below.
Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

Req          | Chapter            | Function
------------ |------------------- | --------
AOS_AS-163   |  \ref runOut       | AbstractDMIHandler::runOut()
AOS_AS-394 S |  \ref runOut       | \ref AbstractDMIHandler::handleOdometerFailures()  <br /> \ref AbstractDMIHandler::getDriverOdoFailDMIButtonConfirmation()
*/
}
}
