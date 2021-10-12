namespace ATP::TG
{

/**
\if AsMainPage
\mainpage TIC Component Specification
@anchor ti
\endif

\ifnot AsMainPage
\class TIC
\endif

\section Purpose Purpose
This document specifies the software design for the class TIC, the BHP adaptation part
of the Train Integrity Control component.

\latexonly \newpage \endlatexonly
\section Overview Overview
\subsection GeneralFunctionality General Functionality
The purpose of TIC is to evaluate the availability of automatic train configuration and
if available, request the train configuration by interfacing with LCS via the Vehicle Com component.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
TIC has dependencies to the following component:
- Vehicle Com: To retrieve brake system type and train configuration status.

Other components have dependencies to this component because they use its public types
and methods, see \ref TIC Class Reference.

\latexonly \newpage \endlatexonly

\section FunctionalDesign Functional Design

\subsection Initialization Initialization
The Initialization of TIC is performed by its constructor and by TIC::initCrossCompare().

\subsection ModeDependentOperation Mode dependent operation
The TIC component is independent of ATP mode.


\subsection Scheduling Scheduling
The TIC component is fully scheduled inside the AbstractTIC class.
It implements the virtual functions declared in the core part.

\subsection evaluateTICAvailable Evaluate TIC availability
The TIC::evaluateTICAvailable() implements the virtual function declared in AbstractTIC and returns the availability of TIC.
The function performs the following:
- Check that operating ECPB mode is valid (LCS is connected) and get the ECPB operating mode from the Vehicle Com component :

- If ECPB operating mode is in Run Mode, brake system is ECPB and Loco-type is EMD which is available for automatic configuration:
    + Stop the timer for TIC initialization and TIC availability.
- If the ECPB operating mode is Not Available then stop the timer for TIC initialization and set the TIC availability.
- If the ECPB operating mode is in either Initialization, Switch or CutOut Mode,
    + If it is not running, start the timer for the mode transitions.
    + If the TIC initialization time has expired, set the TIC Available value accordingly.

\subsection evaluateConfigReqStatus Evaluate Config Request Status
The function TIC::evaluateConfigReqStatus() sets the TIC configuration status based on ECPB composition
and returns the status of automatic train configuration request.
The function performs the following:
   + If ECPB train composition is valid, it returns request status Config Received.
   + Else it checks if a valid ECPB sequence status to return request status Config Waiting.
- Else it returns Request Not Sent status.

\subsection getLocoOrientationAvailable Get Loco Orientation
In the adaptation part of the TIC component, it returns false.

\section ClassDiagram Class Diagram

@image html tic_class_diagram.png "TIC Class diagram"
@image latex tic_class_diagram.png "TIC Class diagram"

\section Diagnostics Diagnostics

\subsection ConsoleCommands Console commands
No Console Commands has been added for TIC adaptation class.

\subsection Analyze Analyze
No values are registered for analysis for TIC component.

\section CoreAdaptation Core / Adaptation
The TIC class overrides the following virtual methods declared in AbstractTIC:
- \ref evaluateTICAvailable
- \ref evaluateConfigReqStatus
- \ref getLocoOrientationAvailable

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component


\section Traceability Traceability

\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

Common requirements are specified in SCDS ATP BHP.

The requirements relevant for this component are:

Req           | Chapter                     | Function
------------- | --------------------------  | --------
AOS_BHPB 2612 | \ref FunctionalDesign       | TIC::evaluateTICAvailable()
AOS_BHPB 2702 | \ref FunctionalDesign       | TIC::evaluateTICAvailable()
AOS_BHPB 2703 | \ref FunctionalDesign       | TIC::evaluateTICAvailable()


\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP BHP.



*/
}

