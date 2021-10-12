namespace ATP::TG
{

/**
\if AsMainPage
\mainpage TIMS Component Specification (Adaptation)
@anchor tm
\endif

\ifnot AsMainPage
\class TIMS
\endif

\section Purpose Purpose
This document specifies the software design for the TIMS class, the BHP adaptation part of the TIMS component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
Detection of train integrity is essential for ensuring safety.
The TIMS component is responsible for the last car supervision and checks the integrity of the train.

For the monitoring of train integrity, the TIMS class extends the AbstractTIMS class by adding functionality
for handling automated integrity reports from ECPB and OBRD. TIMS uses signalling from ECPB if that is available,
otherwise from OBRD.

For the ECPB communication, TIMS uses the Vehicle Com component. For the OBRD communication, TIMS uses
the class \ref OBRDMessageHandler, which is implemented as part of this component.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
TIMS depends on the following components:
- Loco IO for reading brake pressure
- Tracks for the calculation of distances
- TSetup for retrieving train setup and brake configuration
- Vehicle Com for receiving operating mode and train integrity status from ECPB

Other components have dependencies to this component because they use its public types
and methods, see \ref TIMS Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization

\subsubsection preInit preInit()
Calls OBRDMessageHandler::preInit().

\subsubsection init init()
Initializes member variables, starts cross-comparison and calls OBRDMessageHandler::init().

\subsection ModeDependentOperation Mode Dependent Operation
TIMS is not mode dependent.

\subsection Scheduling Scheduling
The TIMS processing is performed by runIn(), run() and runOut() which must be called by the framework in each cycle.

\subsubsection runIn runIn()
TIMS::runIn() receives messages from the OBRD subsystem, by calling OBRDMessageHandler::runIn().

\subsubsection run run()
TIMS::run() is the entry point for the cyclic %TIMS processing. It does the following:
+ Performs checks on the input data, which is read from Vehicle Com if ECPB is active, otherwise from \ref OBRDMessageHandler
+ Calls AbstractTIMS::run() which in turn calls:
  - The virtual function \ref updateTimsAvailable
  - The virtual function \ref updateTimsStatus

\subsubsection runOut runOut()
TIMS::runOut() transmits messages to the OBRD subsystem, by calling OBRDMessageHandler::runOut().

\subsection UpdatingStates Updating States

\subsubsection updateTimsAvailable updateTimsAvailable()
TIMS::updateTimsAvailable() checks whether TIMS is available on this train. TIMS is available if the TrainSetup
indicates that TIMS is required, or if we haven't received the TrainSetup yet, if TIMS has received integrity
status from ECPB.

\subsubsection updateTimsStatus updateTimsStatus()
TIMS::updateTimsStatus() performs the actual evaluation of train integrity. If %TIMS Supervision
is \a Supervised or \a Inhibited, integrity is evaluated by:
- checking brake pressure in the locomotive
- checking ECPB integrity (if ECPB is active)
- checking OBRD integrity (if ECPB is not active)

If a check fails, %TIMS Status is set to \a Broken. If all checks pass, %TIMS Confirmed is set to \a True.

Note: %TIMS Confirmed is an intermediate result that, if \a True, causes %TIMS Status to be \a Intact if
%TIMS Supervision is \a Supervised. If Supervision is \a Inhibited, then the driver can choose to return
to \a Supervised (this is known as "resume supervision").

\subsection UpdatingLastCarPosition Updating Last Car Position

\subsubsection getAutomatedReportTime getAutomatedReportTime()
TIMS::getAutomatedReportTime() provides input to the calculation of last car position and returns the time when the most
recent integrity report was received from ECPB or OBRD.

\subsection BrakePressureCheck Brake Pressure Check

\subsubsection checkLastCarBPDrop checkLastCarBPDrop()
TIMS::checkLastCarBPDrop() checks if the brake pressure reported by OBRD has dropped below the configured limit for the
pre-departure brake check.

\subsection OBRDMessageHandler OBRDMessageHandler
The OBRDMessageHandler class implements the [OBRD] interface to the OBRD unit. The primary purpose
of this interface is to deliver train integrity information to AOS. OBRD is used for trains that do
not have ECPB.

The OBRD interface defines the following messages, each handled by its own class:
- OBRDMessageInProtocolVersion for sending the protocol version to AOS
- OBRDMessageInUnitStatus for sending integrity information to AOS
- OBRDMessageOutProtocolVersion for sending the protocol version from AOS
- OBRDMessageOutRejectMessage for sending errors from AOS

@image html obrd_message_handler_class_diagram.png "OBRDMessageHandler class diagram"
@image latex obrd_message_handler_class_diagram.png "OBRDMessageHandler class diagram"

\latexonly \newpage \endlatexonly
\section ClassDiagram Class Diagram

@image html tims_class_diagram.png "TIMS class diagram"
@image latex tims_class_diagram.png "TIMS class diagram"

\section Diagnostics Diagnostics

\subsection Console Console Commands
N/A

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
The TIMS class is derived from the core class AbstractTIMS and implements these overrides:
- \ref updateTimsAvailable
- \ref updateTimsStatus
- \ref getAutomatedReportTime

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component.

\section Traceability Traceability

\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP BHP.

The requirements relevant for this component are:

Req             | Chapter                        | Function
--------------- | ------------------------------ | --------
AOS_BHPB 2852 S | \ref UpdatingStates            | TIMS::updateTimsStatus()
AOS_BHPB 2853   | \ref UpdatingStates            | TIMS::updateTimsStatus()
AOS_BHPB 2854 S | \ref UpdatingStates            | TIMS::updateTimsStatus()
AOS_BHPB 2855 S | \ref UpdatingStates            | TIMS::updateTimsStatus()
AOS_BHPB 2948 S | \ref UpdatingStates            | TIMS::updateTimsStatus()
AOS_BHPB 3298 S | \ref OBRDMessageHandler        | OBRDMessageHandler::runIn(), OBRDMessageHandler::runOut()
AOS_BHPB 3317   | \ref BrakePressureCheck        | TIMS::checkLastCarBPDrop()
AOS_BHPB 3318   | \ref BrakePressureCheck        | TIMS::checkLastCarBPDrop()
AOS_BHPB 3319   | \ref BrakePressureCheck        | TIMS::checkLastCarBPDrop()
AOS_BHPB 3324   | \ref OBRDMessageHandler        | OBRDMessageHandler::runIn()
AOS_BHPB 3325   | \ref OBRDMessageHandler        | OBRDMessageHandler::runIn()
AOS_BHPB 3338 S | \ref UpdatingStates            | TIMS::updateTimsStatus()
AOS_BHPB 3339 S | \ref UpdatingStates            | TIMS::updateTimsStatus()
AOS_BHPB 3347   | \ref OBRDMessageHandler        | OBRDMessageHandler::runIn(), OBRDMessageHandler::runOut()
AOS_BHPB 3348 S | \ref Scheduling                | TIMS::run()
AOS_BHPB 5017   | \ref UpdatingStates            | TIMS::updateTimsAvailable()
AOS_BHPB 5018   | \ref UpdatingStates            | TIMS::updateTimsAvailable()
AOS_BHPB 5047   | \ref OBRDMessageHandler        | OBRDMessageHandler::runIn()
AOS_BHPB 5082   | \ref BrakePressureCheck        | TIMS::checkLastCarBPDrop()

\subsection SSAS Architectural requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP BHP.

*/

}
