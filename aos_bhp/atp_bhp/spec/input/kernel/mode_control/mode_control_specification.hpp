namespace ATP::Kernel
{
/**
\if AsMainPage
\mainpage Mode Control  Adaptation Component Specification
@anchor mc
\endif

\ifnot AsMainPage
\class Template
\endif

\section Purpose Purpose
This document specifies the software design for Adaptation part of the Mode Control component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
The ModeControl class is the adaptation implementation of the Mode Control component. 
The primary function of ModeControl is to instantiate the AbstractModeControl class for use within the ATP and other applications in AOS.

The ModeControl adaptation component inherits the AbstractModeControl class. The adaptation will create a singleton object
of the ModeControl class and provides access to it.

The core component provides functionality to modify the mode in the adaptation. 
The adaptation extends the existing Modes and Sequences of Core. See Mode Control core SCDS for more information about Modes and Sequences.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
- DMI Handler
  + In Train Configuration Mode, during Load Status 'Acknowledgment Procedure' the load status is set into the train setup after Driver 'Confirms' the changed Train Loaded status.
- Vehicle Com to get the version of the ADS map.
- LocoIO to get the revision Id for Digital Input and Output.
- Odometry to get the SDP version.
- TIMS to check statue of the brake pressure in the last car.
- Brake to get brake pressure readings when running \ref preDepartureTest


  Other components have dependencies to this component because they use its public types
  and methods, see \ref ModeControl Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization
The adaptation implements the virtual function init() which initializes the AbstractModeControl class and prepares values for cross-compare.

\subsection ModeDependentOperation Mode dependent operation
N/A

\subsection Scheduling Scheduling
The virtual run() - function is not implemented in the adaptation.

\subsection trainConfigMode Train Configuration Mode
The adaptation implementation of the Train Configuration mode handles additional functionality compare to the core version. That functionality is pre-departure brake test and train Load Status changes.

\subsubsection preDepartureTest Pre-departure brake test
Pre-departure brake test ensures the brake pipe is intact along the whole length of the train. 
This is done by applying service brake and ensure the pressure in the brake pipe drops as expected and reported within expected time by OBRD.
It is implemented within the following two extra states.
- trainConfigPreDepartureBrakeTest starts the brake test by applying service brake when certain conditions are met.
- trainConfigCheckValidBrkPrFeedBack ensures last car brake pressure reported by OBRD drops within expected time from when service brake is applied.

Parts of the pre-departure brake test functionality is implemented within existing states that are partly overwritten as described below.
- trainConfigConfirmNewConfigFrmTCC continues to the trainConfigPreDepartureBrakeTest state in normal case when a valid train setup is confirmed received.
- trainConfigTSetupAccepted redirects to the state trainConfigWaitForDepartureTest.
- trainConfigWaitForDepartureTest waits for the driver to confirm performed pre-departure test.

\subsubsection loadStatusConfirm Train Load Status change
AOS checks if there are any changes in the Load Status of the train. This applies for both automatic train configuration and manual, leading to the states
listed below getting affected in this adaptation implementation.
Where AOS sees difference in the Load Status from the train setup compared to request by the driver, the driver is required to confirm this change.
AOS requests the driver to confirm the Load Status change within 5 seconds using the Acknowledge Command Procedure. 
If the driver confirms it will be followed by the StartUp message from TCC. If not confirmed within 5 seconds or the driver cancels the Load Status
change, ATP proceeds as done in the core state.
- trainConfigWaitForAcceptAutomatic 
- trainConfigWaitNewConfigDMI 


\subsection locationMode Location Mode
The LocationModeBHP class implements the functionality related to ATP mode Location.
It handles below functionalities.
- Handling the free rolling train state in location mode.
- Manage final state of location mode.
- Set the Handling Done status when:
    - The ATP Mode is Location, AND
    - The ATO Mode is Manual, AND
    - Handling done request is received from the LCS, AND
    - Train is at standstill

\subsection driverLoginSeq Driver Login Sequence

This adaptation implementation of the Driver Login Sequence does special handling of the driverLoginVerification 
state which allows for 4 AOS - TCC communication timeouts when waiting for the driverLogonStatus message before the login attempt is considered as failed.

\section ClassDiagram Class Diagram

@image html mode_control_class_diagram.png "Mode Control Class Diagram"
@image latex mode_control_class_diagram.png "Mode Control Class Diagram"

@image html mode_bhp.png "Mode BHP Class Diagram"
@image latex mode_bhp.png "Mode BHP Class Diagram"


@image html sequence_bhp.png "Sequence BHP Class Diagram"
@image latex sequence_bhp.png "Sequence BHP Class Diagram"

\section Diagnostics Diagnostics

\subsection Console Console-command
 N/A

\subsection Analyze Analyze
 N/A

\section CoreAdaptation Core / Adaptation
The Mode Control component is split in core and adaptation.
The core component provides virtual run and init functions for the adaptation to extend the functionality.
To enable this, the function to execute the mode and the functions to execute internal mode states are declared as virtual.

\section PreProcessor Pre-Processor Directives
N/A

\section Traceability Traceability

\subsection SSRS Functional Requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP BHP.

The requirements relevant for this component:

Req            | Overridden req | Chapter                           | Function
----------     | -------------- | --------                          | --------
AOS_BHPB 3188  | AOS 3236       | \ref trainConfigMode              | TrainConfigModeBHP::runTrainConfigPreDepartureBrakeTest()
AOS_BHPB 2629  |                | \ref locationMode                 | LocationModeBHP::manageFreeRolling()
AOS_BHPB 2846  |                | \ref locationMode                 | LocationModeBHP::manageFreeRolling()
AOS_BHPB 2626  |                | \ref locationMode                 | LocationModeBHP::manageFreeRolling()
AOS_BHPB 2627  | AOS 1605       | \ref locationMode                 | LocationModeBHP::manageFreeRolling()
AOS_BHPB 3350 S|                | \ref trainConfigMode              | TrainConfigModeBHP::runModeFunction() 
AOS_BHPB 3030  |                | -                                 | ModeControl::setStaticConfigurationVersion()
AOS_BHPB 5341  |                | \ref locationMode                 | LocationModeBHP::manageHandlingDone
AOS_BHPB 5085  |                | -                                 | TrainConfigModeBHP::handleAbortBrakeTestActions()
AOS_BHPB 3318  |                | -                                 | TrainConfigModeBHP::handleAbortBrakeTestActions()
AOS_BHPB 3319  | AOS 3235       | -                                 | TrainConfigModeBHP::checkBrakePressureLocoBelowPsb()
AOS_BHPB 2768 S| AOS 1604       | -                                 | TrainConfigModeBHP::handleAbortBrakeTestActions()
AOS_BHPB 2822 S|                | -                                 | TrainConfigModeBHP::handleAbortBrakeTestActions()
AOS_BHPB 3317  | AOS 3235       | -                                 | TrainConfigModeBHP::runTrainConfigPreDepartureBrakeTest()

\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP BHP.
*/
}
