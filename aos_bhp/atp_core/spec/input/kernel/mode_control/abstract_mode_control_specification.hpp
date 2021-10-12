namespace ATP::Kernel
{

  /**
  \if AsMainPage
  \mainpage Mode Control Component Specification
  @anchor mc
  \endif

  \ifnot AsMainPage
  \class AbstractModeControl
  \endif

  \section Purpose Purpose
  This document specifies the software design for the core part of the Mode Control component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality
  Mode control is the central component responsible for deciding the operating mode of the AOS system. 
  As part of this central functionality it implements \ref sequences and \ref modes, to handle modes and mode transitions.
  Additionally Mode Control provides the following functionality:
  - Providing the current mode and mode state to other components. 
  - Performing mode dependent operations and manage mode transitions.
  - Setting or clearing the Idle, MA Timeout, Stop Train, Handling Done and Train states in different modes.
  - Managing the sleeping signal, Cab status and Ready to drive status.
  - Managing Stop Train when message is received from TCC.


  \subsubsection abstractModeControl AbstractModeControl
  AbstractModeControl is the main class of this component handling most of the functionality as well as executing the sequences and the modes. 
  It reports the current mode of the ATP system and handles the transitions between the them.
  AbstractModeControl also performs tasks that are common to all modes.
  AbstractModeControl provides interfaces to access information such as:
  - Current and previous ATP mode.
  - State of the modes.
  - Current state of the sequences.
  - Status of Idle, free rolling, MA Timeout, Stop Train and Handling Done states.

  \subsubsection sequences Sequences
  A Sequence is a process in the system implemented as state machine whose state affects modes or other components.
  Each sequence is represented as a class and has a run() function being the starting point of the execution and does execute all the states.
  The sequences implemented in the component are:
      - EmergencyAlertSeq: This state machine deals with activation of Emergency Alert, forcing the train to stop and then revoking the Emergency Alert. See \ref eaSeq.
      - DriverLoginSeq: This handles the process of the driver logging in the AOS system. See \ref driverLoginSeq.
      - YardModeRequestSeq: This sequence indicates if transition to Yard Mode is possible and handles the transition to Yard Mode triggered by the button on the DMI. See \ref yardModeReqSeq.
      - ShuntModeRequestSeq: This sequence indicates if transition to Shunting Mode is possible and handles the transition to Shunting Mode triggered by the button on the DMI. See \ref shuntModeReqSeq.
      - PosModeRequestSeq: This sequence indicates if transition to Possession Mode is possible and handles the transition to Possession Mode triggered by the button on the DMI. See \ref posModeReqSeq.
      - ConfigModeRequestSeq: This sequence indicates if transition to Config Mode is possible and handles the transition to Config Mode using the button on the DMI. See \ref configModeReqSeq.

  \subsubsection modes  Modes  
  Every operating mode is represented as a class which is derived from the AbstractMode class. Each mode in turn has mode states that is handled within virtual functions. This design enable adaptation 
  implementations to modify a mode and introduce additional mode states if needed.

  The following are a few of the function that are vital to this design.
  - AbstractMode::handleMode() is a pure virtual function responsible for executing the mode depending on the mode state.
  - AbstractMode::getModeId() is a pure virtual function to handle identification of the mode.
  - AbstractMode::getNextModeId() to indicate the next mode to transition or the current mode if no transition is required.
  - AbstractMode::resetMode() to reset the internal state machine of the mode. It is generally called when a new mode is entered.
  - AbstractMode::getModeState() to get the state of the mode.
  
  See \ref ClassDiagram for more details.

  The following table defines the acceptable mode transitions.
  The table is setup as follows:
  - Rows define the original mode
  - Columns define the new mode
  - A : Allowed
  - - : Transition not possible or invalid


   From mode           |POU|CON|REG|BS |NOR|SHN|LOC|YAR|URE|POD|SHT|SLE|SRP|SHR|POS|SPT|JON|SBTS|
  ---------------------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
  PowerUp, POU         | - | A | - | - | - | A | - | A | - | A | A | A | - | - | A | - | - | - |
  Configuration, CON   | - | - | A | - | A | A | - | A | A | A | A | - | - | - | A | - | - | - |
  Registration, REG    | - | A | - | A | A | A | - | A | A | A | A | - | A | - | A | - | - | A |
  BaliseSearch, BS     | - | A | - | - | A | A | - | A | A | A | A | - | - | - | A | - | - | A |
  Normal, NOR          | - | - | - | - | - | A | A | A | A | A | A | - | A | A | A | A | A | A |
  Shunting, SHN        | - | A | - | - | - | - | - | A | - | A | A | A | - | - | A | - | - | - |
  Location, LOC        | - | - | - | - | A | A | - | A | - | A | A | - | - | - | A | - | - | A |
  Yard, YAR            | - | A | - | - | - | A | - | - | - | A | A | A | - | - | A | - | - | - |
  Unregistered, URE    | - | A | - | - | - | A | - | A | - | A | A | - | - | - | A | - | - | - |
  Powering Down, POD   | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - | - |
  SafetyHalt, SHT      | - | - | - | - | - | - | - | - | - | A | - | - | - | - | - | - | - | - |
  Sleeping, SLE        | - | A | - | - | - | A | - | A | - | A | A | - | - | - | A | - | - | - |
  StaffResponsible, SRP| - | - | - | - | A | A | - | A | A | A | A | - | - | - | A | - | - | A |
  Shunting Route, SHR  | - | A | - | - | - | A | - | A | A | A | A | - | - | - | A | - | - | - |
  Possession, POS      | - | A | - | - | - | A | - | A | - | A | A | - | - | - | - | - | - | - |
  Split, SPT           | - | A | - | - | - | A | - | A | A | A | A | - | - | - | A | - | - | - |
  Join, JON            | - | A | - | - | - | A | - | A | A | A | A | A | - | - | A | - | - | A |
  SafeBrakeToStop, SBTS| - | - | - | - | - | A | - | A | A | A | A | - | A | - | A | - | - | - |



  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies

  This component has dependencies to the following components:
  - Radio Handler and Message Handler component to provide the messages from TCC.
  - DMI Handler to provide the inputs from the driver.
  - Odometry to get the train standstill status
  - Loco IO to get the current value of an input from CoreDigitalInputs and the status of the sleeping signal.
  - Brake to manage brake test.
  - Targets to manage target list with respect to train not ready to drive.
  - TSetup to remove the train setup when entering certain modes that should not have a train setup.
  - TIMS to get TIMS based position data.
  - BTM Handler to verify BTM is available during power up.
  - TIC to get TIC availability and status during configuration.
  - Position to get position information about the train and balises.
  - Supervise to check if there is an emergency alert request.
  - Tracks to perform operations on data (e.g. track data and balise data) maintained by the component.


  
  Other components have dependencies to this component because they use its public types
  and methods, see \ref AbstractModeControl Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  The AbstractModeControl class initialization does the following:
  - Create and initialize objects of all the modes.
  - Create and initialize the sequences.
  - Initialize the current mode and previous mode values.
  - Initialize the current and previous cab status.
  - Registers the persistent values for cross compare.
  - Reset the state of current mode.
  - Initialize the time in seconds that AOS is allowed to run without reset.
  
  \subsection ModeDependentOperation Mode dependent operation
  N/A

  \subsection Scheduling Scheduling
  The AbstractModeControl has run() function that must be called each execution-cycle.

  \subsubsection run run()
  The AbstractModeControl::run() performs the following tasks:
  - Manage initial preconditions for continued operation such as version logging, sleeping signal, readiness to drive and Brake test.
  - Execute all the sequences.
  - Tasks common to all modes and which can affect the current mode:
      - Check for entering Power Down Mode.
      - Check for entering Safety Halt Mode.
      - Check for entering Safe Break To Stop Mode.
      - Check for entering Location Mode.
  - Execute the current mode by calling its corresponding AbstractMode::handleMode() function.
  - Update the current mode and previous mode.
  - Handles the ATP reset conditions.

  \subsection powerUpMode Power Up Mode
  The PowerUp mode is the starting mode of the AOS system.
  A standstill event is triggered and maintained throughout the execution of this mode.

  The state diagram details the state of the mode, transitions between them and the actions to do in each state.

  @image html power_up_mode.png "Power Up Mode"
  @image latex power_up_mode.png "Power Up Mode"

  \subsection trainConfigMode Train Configuration Mode
  There are 3 possible train configuration scenarios:
  - New configuration for which the configuration parameters are entered manually from DMI.
  - New configuration for which the configuration parameters are received from TIC system.
  - Reconfiguration where the configuration parameters are acknowledged from DMI.

  A standstill event is triggered and maintained throughout the execution of this mode.

  Below state diagram details the transitions between the states and related actions.

  @image html train_config_mode.png "Train Configuration Mode"
  @image latex train_config_mode.png "Train Configuration Mode"

  \subsection trainRegMode Train Registration Mode
  The Train Registration mode is responsible for handling the different ways to register the train based on the Train Setup message provided by TCC. 
  A standstill event is triggered and maintained throughout the execution of this mode.

  Below state diagram details the transitions between the states and related actions.

  @image html train_registration_mode.png "Train Registration Mode"
  @image latex train_registration_mode.png "Train Registration Mode"

  \subsection baliseSearchMode Balise Search Mode
  The purpose of BaliseSearch mode is to establish the position of the train and get it to Normal mode. 
  For this to be possible the train is allowed to drive a limited distance to find balises.
  Depending on whether the train will do a new registration or re-registration it is required to pass 2 or 1 balise.

  Below state diagram details the transitions between the states and related actions.

  @image html balise_search_mode.png  "Balise Search Mode"
  @image latex balise_search_mode.png "Balise Search Mode"

  \subsection normalMode Normal Mode
  The Normal mode is the mode that provides full ATP protection. It implements functionality such as monitoring the communication with TCC and odometer validity.
  Normal mode is entered after a successful configuration and registration of the train.
  The mode checks if a transition is requested to other modes that are allowed from the normal mode.  See \ref mode for all possible transitions.
  The following are allowed modes triggered by an MA:
  - Transition to Location mode if MA with Location data is received and train is within the location boundaries.
  - Transition to Join if a Join MA or Join-command is received.
  - Other modes possible to enter based on MA are ShuntingRoute, StaffResponsible and Split. 

  \subsection safetyHaltMode Safety Halt Mode

  The SafetyHalt mode is one of two modes to indicate the erroneous situation within the AOS system.
  This mode indicates problems with the highest severity.
  It does the following: 
  - Delete all the tracks and targets.
  - Inform the Driver with a continuous sound and will present Safety Halt screen.
  - ATP Lamp Status output will toggle.

  \subsection powerDownMode PowerDown Mode
  The PowerDown mode can be entered from almost any state when the conditions are met.
  The conditions to enter PowerDown are as follows:

  ATP Off pressed more than 5 seconds and train at standstill and ATO mode manual
  - for modes POU, CON, REG, BS, POS, SHT, YAR, SLE, SBTS, URE, SHN.
  - and for modes NOR, SRP, SHR, SPT, JON, LOC: Primary target doesn't exist unless TCC communication is lost.

  \subsection sleepMode Sleeping Mode
  The main purpose is to inhibit all brakes when entering sleeping mode, and allow brakes when exiting sleeping mode.

  @image html sleep_mode.png "Sleeping Mode"
  @image latex sleep_mode.png "Sleeping Mode"

  \subsection joinMode Join Mode
  Join mode handles the joining of two trains for which the rear train will continue in Sleeping mode and the leading in Configuration mode, letting the trains be configured as one.
  
  @image html join_mode.png "Join Mode"
  @image latex join_mode.png "Join Mode"

  \subsection splitMode Split Mode
  The main purpose of the Split mode is allowing the leading loco in a train consist to split the train in two rakes with a leading locomotive in each rake.
  
  @image html split_mode.png "Split Mode"
  @image latex split_mode.png "Split Mode"

  \subsection shuntingRouteMode Shunting Route Mode
  The main purpose of ShuntingRoute mode is to allow a train to move according an MA of type ShuntingRoute and couple or uncouple cars.
  
  @image html shunting_mode.png "Shunting Route Mode"
  @image latex shunting_mode.png "Shunting Route Mode"

  \subsection yardMode Yard Mode
  Yard mode is used if the driver requests to drive within a yard area, such as workshops, that is not controlled by TCC. 
  The train can enter Yard mode even if TCC is not connected but if AOS and TCC is connected TCC must acknowledge the mode change. 

  @image html yard_mode.png "Yard Mode"
  @image latex yard_mode.png "Yard Mode"

  \subsection srMode Staff Responsible Mode
  StaffResponsible mode is used when e.g. a point machine fails thus it is not possible to lock a route with full supervision. In this case when
  entering StaffResponsible mode the driver accepts the responsibility to supervise that it is safe to move the train.
  This mode is also a way of continuing from the fault handling mode SBTS. 

  @image html staff_responsible_mode.png "Staff Responsible Mode"
  @image latex staff_responsible_mode.png "Staff Responsible Mode"

  \subsection driverLoginSeq Driver Login Sequence
  The Driver Login sequence handles the procedure for logging in DMI. AOS shall issue a standstill event while Driver Login status is not yet authorized.
  Only the ATO mode manual is currently supported in AOS.
  
  @image html driver_login_seq.png "Driver Login Sequence"
  @image latex driver_login_seq.png "Driver Login Sequence"

  \subsection eaSeq Emergency Alert Sequence
  This sequence details the various states in the process of raising an Emergency Alert and releasing the Emergency Alert.
  The state diagram shows the sequence states and the transitions between the states.

  @image html emergency_alert_seq.png "Emergency Alert Sequence"
  @image latex emergency_alert_seq.png "Emergency Alert Sequence"


  \subsection yardModeReqSeq Yard Mode Request Sequence

  This sequence details the process when Yard mode is requested from the DMI.
  It is to be noted that this sequence will not change the operating mode of AOS. Mode Control will check the state
  of this sequence and decide if the current mode should change to Yard Mode.

  See \ref yardMode for more information.

  @image html yard_mode_select_seq.png "Yard Mode Request Sequence"
  @image latex yard_mode_select_seq.png "Yard Mode Request Sequence"

  \subsection posModeReqSeq Possession Mode Request Sequence

  This sequence details the process of selecting the Possession mode from the DMI.
  It is to be noted that this sequence will not change the operating mode of AOS. Mode Control will check the state
  of this sequence and decide if the current mode should change to Possession mode.

  @image html pos_mode_select_seq.png "Possession Mode Request Sequence"
  @image latex pos_mode_select_seq.png "Possession Mode Request Sequence"

  \subsection shuntModeReqSeq Shunting Mode Request Sequence

  This sequence details the process of selecting the Shunting mode from the DMI. 
  It is to be noted that this sequence will not change the operating mode of AOS. Mode Control will check the state
  of this sequence and decide if the current mode should change to Shunting mode.

  @image html shunt_mode_select_seq.png "Shunting Mode Request Sequence"
  @image latex shunt_mode_select_seq.png  "Shunting Mode Request Sequence"

  \subsection configModeReqSeq Configuration Mode Request Sequence

  This sequence details the process of selecting the Configuration mode from the DMI. 
  It is to be noted that this sequence will not change the operating mode of AOS. Mode Control will check the state
  of this sequence and decide if the current mode should change to Configuration mode.

  @image html config_mode_select_seq.png "Configuration Mode Request Sequence"
  @image latex config_mode_select_seq.png "Configuration Mode Request Sequence"

  \section ClassDiagram Class Diagram

  The Class structure for core part of the Mode Control component is shown below. It is to be noted that the diagram below does not show all
  the mode classes and sequences in order to avoid clutter.

  @image html abstract_mode_control_class_diagram.png "Abstract ModeControl Class Diagram"
  @image latex abstract_mode_control_class_diagram.png "Abstract ModeControl Class Diagram"

  @image html abstract_mode.png "Abstract Mode Class Diagram"
  @image latex abstract_mode.png "Abstract Mode Class Diagram"

  @image html mode_request_seq.png "ModeRequestSequence Class Diagram"
  @image latex mode_request_seq.png "ModeRequestSequence Class Diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console Commands
  Following console commands have been defined for Mode Control:
  - mode : Print current mode and its mode state.
  - cabStat : Print  current cabin state.
  - prvMode : Print previous mode.
  - seq : List all sequences and their states.
  - MCVar : Print Mode Control variables like MA TimeOut, Train Idling, Stop Train.

  \subsection Analyze Analyze
  The registered measurement data for analysis are
  - ATP Mode: It indicates the current mode


  \section CoreAdaptation Core / Adaptation
  The Mode Control component is split in core and adaptation.

  The core provides access to the adaptation to extend its functionality. To enable this, the function to execute the mode and
  the functions to execute internal mode states are declared as virtual.

  The Mode Control adaptation class inherits the AbstractModeControl class. It overrides the virtual functions which provide objects for PowerUpMode class.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component.

  \section Traceability Traceability

  \subsection SSRS Functional Requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP Core.

  The requirements relevant for this component:

  Requirement | Chapter                          | Function
  ----------  | ---------------------            | --------
  AOS 1007    | \ref run                         | AbstractMode::isModeChangedByMA()
  AOS 2156    | \ref AbstractModeControl         | AbstractModeControl::run()
  AOS 1314    | \ref AbstractModeControl         | AbstractModeControl::manageCabActiveStatus()
  AOS 1436 S  | \ref AbstractModeControl         | AbstractModeControl::manageCabActiveStatus()
  AOS 1298    | \ref AbstractModeControl         | AbstractModeControl::manageCabActiveStatus()
  AOS 146     | \ref driverLoginSeq              | DriverLoginSeq::DriverLoginSeq()
  AOS 1032    | \ref driverLoginSeq              | DriverLoginSeq::runDriverLoggedIn()
  AOS 769     | \ref driverLoginSeq              | DriverLoginSeq::runDriverLoggedOut()
  AOS 144     | \ref driverLoginSeq              | DriverLoginSeq::runDriverLoginVerification()
  AOS 150     | \ref driverLoginSeq              | DriverLoginSeq::runDriverLoginFailed()
  AOS 154 S   | \ref driverLoginSeq              | DriverLoginSeq::run()
  AOS 2112    | \ref driverLoginSeq              | DriverLoginSeq::runDriverLoggedIn()
  AOS 148     | \ref driverLoginSeq              | DriverLoginSeq::runDriverLoggedOut()
  AOS 2195    | \ref driverLoginSeq              | DriverLoginSeq::runDriverLoggedIn()
  AOS 149     | \ref driverLoginSeq              | DriverLoginSeq::runDriverLoginVerification()
  AOS 64 S    | \ref baliseSearchMode            | BaliseSearchMode::runBaliseSearchWaitForBaliseReg()
  AOS 1942 S  | \ref baliseSearchMode            | BaliseSearchMode::runBaliseSearchWaitMA()
  AOS 1093 S  | \ref baliseSearchMode            | BaliseSearchMode::runBaliseSearchWaitForBaliseReg()
  AOS 1674    | \ref trainConfigMode             | TrainConfigMode::handleMode()
  AOS 658     | \ref run                         | AbstractModeControl::run()
  AOS 159     | \ref powerUpMode                 | PowerUpMode::runPowerUpStart()
  AOS 1085    | \ref run                         | YardMode::handleMode(), ShuntingMode::handleMode(), PossessionMode::handleMode(), AbstractModeControl::run()
  AOS 656     | \ref powerUpMode                 | PowerUpMode::runPowerUpActivation()
  AOS 18 S    | \ref powerUpMode                 | PowerUpMode::runPowerUpTest()
  AOS 1713 S  | \ref powerUpMode                 | PowerUpMode::runPowerUpTest()
  AOS 2143    | \ref normalMode                  | NormalMode::handleMode()
  AOS 2144    | \ref yardMode, \ref shuntingRouteMode, \ref run | YardMode::handleMode(), ShuntingMode::handleMode(), SafetyHaltMode::handleMode(), LocationMode::handleMode(), PossessionMode::handleMode()
  AOS 2197    | \ref run                         | LocationMode::handleMode()
  AOS 2140    | \ref shuntingRouteMode, \ref run | YardMode::handleMode(), ShuntingMode::handleMode(), SafetyHaltMode::handleMode(), LocationMode::handleMode(), PossessionMode::handleMode()
  AOS 283     | \ref normalMode                  | NormalMode::handleMode()
  AOS 1243    | \ref srMode                      | StaffResponsibleMode::runStaffResponsibleConfirmMAScratch()
  AOS 1245 S  | \ref srMode                      | StaffResponsibleMode::runStaffResponsibleStart()
  AOS 1705    | \ref GeneralFunctionality        | AbstractModeControl::run(void)
  AOS 32      | \ref trainConfigMode             | TrainConfigMode::runTrainConfigWaitNewConfigDMI()
  AOS 1261    | \ref trainConfigMode             | TrainConfigMode::runTrainConfigWaitNewConfigDMI()
  AOS 2056    | \ref trainConfigMode             | TrainConfigMode::runTrainConfigFinishOK()
  AOS 40      | \ref trainConfigMode             | TrainConfigMode::runTrainConfigFinishOK()
  AOS 1822    | \ref trainRegMode                | TrainRegistrationMode::runTrainRegistrationStart()
  AOS 1823 S  | \ref trainRegMode                | TrainRegistrationMode::runTrainRegistrationWaitReRegMA()
  AOS 1829    | \ref trainRegMode                | TrainRegistrationMode::runTrainRegistrationRePos()
  AOS 1830 S  | \ref trainRegMode                | TrainRegistrationMode::runTrainRegistrationRePos()
  AOS 1832    | \ref trainRegMode                | TrainRegistrationMode::runTrainRegistrationStart()
  AOS 34      | \ref trainRegMode                | TrainRegistrationMode::runTrainRegistrationStart()
  AOS 56      | \ref trainRegMode                | TrainRegistrationMode::runTrainRegistrationStart
  AOS 793     | \ref trainRegMode                | TrainRegistrationMode::runTrainRegistrationSetupBSTarget()
  AOS 1817    | \ref trainRegMode                | TrainRegistrationMode::runTrainRegistrationSetupBSTarget()
  AOS 1030    | \ref trainRegMode                | TrainRegistrationMode::runTrainRegistrationFinish()
  AOS 70 S    | \ref baliseSearchMode            | BaliseSearchMode::runBaliseSearchFinishOK()
  AOS 1951 S  | \ref normalMode                  | NormalMode::handleMode()
  AOS 823     | \ref driverLoginSeq              | DriverLoginSeq::runDriverLoginVerification()
  AOS 305     | \ref normalMode                  | NormalMode::handleMode()
  AOS 460 S   | \ref normalMode                  | NormalMode::handleMode()
  AOS 401 S   | \ref eaSeq                       | EmergencyAlertSeq::runEmergencyAlertSeq()
  AOS 1025    | \ref eaSeq                       | EmergencyAlertSeq::runEmergencyAlertInactive()
  AOS 297 S   | \ref GeneralFunctionality        | AbstractMode::manageTrainIdling()
  AOS 1723 S  | \ref GeneralFunctionality        | AbstractMode::manageTrainIdling()
  AOS 1595 S  | \ref GeneralFunctionality        | AbstractMode::manageTrainIdling()
  AOS 672 S   | \ref GeneralFunctionality        | AbstractMode::manageMATimeout()
  AOS 305     | \ref GeneralFunctionality        | AbstractMode::manageMATimeout()
  AOS 2152    | \ref GeneralFunctionality        | AbstractMode::manageMATimeout()
  AOS 458     | \ref GeneralFunctionality        | AbstractMode::manageStopTrain()
  AOS 460 S   | \ref GeneralFunctionality        | AbstractMode::manageStopTrain()
  AOS 402     | \ref eaSeq                       | EmergencyAlertSeq::runEmergencyAlertSeq()
  AOS 1025    | \ref eaSeq                       | EmergencyAlertSeq::runEmergencyAlertSeq()
  AOS 2254    | \ref GeneralFunctionality        | LocationMode::runLocYardModeHandlingDone()
  AOS 2255    | \ref GeneralFunctionality        | LocationMode::runLocYardModeHandlingDone()
  AOS 2256    | \ref normalMode                  | NormalMode::handleMode()
  AOS 2551    | \ref eaSeq                       | EmergencyAlertSeq::run()
  AOS 1604    | -                                | AbstractMode::manageFreeRolling()
  AOS 1605    | -                                | AbstractMode::manageFreeRolling()
  AOS 2534    | \ref normalMode                  | NormalMode::handleMode()
  AOS 2600 S  | \ref powerUpMode                 | PowerUpMode::handleMode(), TrainConfigMode::handleMode() ,TrainRegistrationMode::handleMode(), PowerDownMode::handleMode()
  AOS 2461    | \ref normalMode                  | NormalMode::handleMode()
  AOS 2270    | \ref ATPModes                    | SafeBrakeToStopMode::handleMode()
  AOS 2601 S  | \ref ATPModes                    | SafeBrakeToStopMode::handleMode()
  AOS 2192 S  | \ref run                         | AbstractModeControl::run()
  AOS 680     | \ref safetyHaltMode              | SafetyHaltMode::handleMode()
  AOS 432     | \ref safetyHaltMode              | SafetyHaltMode::handleMode()
  AOS 447     | \ref GeneralFunctionality        | AbstractMode::handleUnRegMessage()
  AOS 1732    | \ref GeneralFunctionality        | AbstractMode::handleUnRegMessage() , SafeBrakeToStopMode::handleUnRegMessage()
  AOS 2430 S  | \ref baliseSearchMode            | BaliseSearchMode::handleUnRegMessage()
  AOS 175     | \ref run                         | AbstractModeControl::run()
  AOS 1695    | \ref splitMode                   | SplitMode::handleMode()
  AOS 2533    | \ref normalMode                  | NormalMode::handleMode()
  AOS 2462    | \ref joinMode, \ref yardMode, \ref shuntingRouteMode, \ref powerUpMode | JoinMode::handleMode(), YardMode::handleMode(), ShuntingMode::handleMode(), PowerUpMode::handleMode()
  AOS 2464    | \ref AbstractModeControl         | AbstractModeControl::isAllowedToEnterConfigMode()
  AOS 2586    | \ref AbstractModeControl         | AbstractModeControl::getInhibitAllBrakes()
  AOS 1839    | \ref configModeReqSeq            | ConfigModeRequestSeq::runConfigButtonPressed()
  AOS 1738    | \ref shuntModeReqSeq, \ref posModeReqSeq, \ref  yardModeReqSeq  | PosModeRequestSeq::run(), ShuntModeRequestSeq::run(), YardModeRequestSeq::run()
  AOS 1735 S  | \ref shuntModeReqSeq, \ref posModeReqSeq, \ref  yardModeReqSeq  | PosModeRequestSeq::run(), ShuntModeRequestSeq::run(), YardModeRequestSeq::run()
  AOS 2218    | \ref shuntModeReqSeq, \ref posModeReqSeq, \ref  yardModeReqSeq  | PosModeRequestSeq::run(), ShuntModeRequestSeq::run(), YardModeRequestSeq::run()
  AOS 2219    | \ref shuntModeReqSeq, \ref posModeReqSeq, \ref  yardModeReqSeq  | PosModeRequestSeq::run(), ShuntModeRequestSeq::run(), YardModeRequestSeq::run()
  AOS 1019    | \ref yardModeReqSeq              | YardModeRequestSeq::run()
  AOS 1664    | \ref yardModeReqSeq              | YardModeRequestSeq::run()
  AOS 635 S   | \ref AbstractModeControl         | AbstractModeControl::run()
  AOS 2611    | \ref run                         | AbstractModeControl::manageBrakeTest()
  AOS 2626    | \ref run                         | AbstractModeControl::manageBrakeTest()
  AOS 2629 S  | \ref powerUpMode                 | PowerUpMode::runPowerUpTest()
  AOS 2583    | \ref driverLoginSeq              | DriverLoginSeq::runDriverLoggedIn()
  AOS 2196    | \ref driverLoginSeq              | DriverLoginSeq::runDriverLoggedOut()
  AOS 1957 S  | \ref baliseSearchMode            | BaliseSearchMode::handleMode()
  AOS 2585 S  | \ref run                         | AbstractModeControl::run()
  AOS 2836    | \ref run                         | AbstractModeControl::run()
  AOS 2825    | \ref sleepMode                   | SleepingMode::handleMode()
  AOS 2615 S  | \ref run                         | AbstractModeControl::manageBrakeTest()
  AOS 2482    | \ref run                         | AbstractModeControl::manageSleepingSignal(void)
  AOS 2483    | \ref run                         | AbstractModeControl::manageSleepingSignal(void)
  AOS 2981 S  | \ref ATPModes                    | JoinMode::handleMode()
  AOS 2947    | \ref run                         | AbstractModeControl::run()
  AOS 2824 S  | \ref srMode                      | StaffResponsibleMode::handleMode()
  AOS 2978 S  | \ref shuntingRouteMode           | ShuntingMode::handleMode()
  AOS 2835    | \ref ATPModes                    | SafeBrakeToStopMode::handleMode()
  AOS 2560    | \ref powerUpMode                 | PowerUpMode::runPowerUpStart(), PowerDownMode::handleMode()
  AOS 2415    | \ref ATPModes                    | LocationMode::runLocFinishOK
  AOS 122     | \ref ATPModes                    | AbstractMode::manageHandlingDone()
  AOS 2413    | \ref ATPModes                    | LocationMode::handleMode()
  AOS 2414    | \ref yardMode                    | YardMode::handleMode()
  AOS 2197    | \ref ATPModes                    | LocationMode::runLocFinishOK
  AOS 2199    | \ref ATPModes                    | LocationMode::runLocFinishOK
  AOS 2361    | \ref ATPModes                    | LocationMode::handleMode()
  AOS 2362    | \ref ATPModes                    | LocationMode::handleMode()
  AOS 1772 S  | \ref powerUpMode                 | PowerUpMode::runPowerUpTest()
  AOS 2933    | \ref trainConfigMode             | TrainConfigMode::handleMode()
  AOS 2803    | \ref trainConfigMode             | TrainConfigMode::handleMode()
  AOS 2804    | \ref trainConfigMode             | TrainConfigMode::handleMode()
  AOS 1156    | \ref run                         | AbstractModeControl::manageNotReadyToDrive()
  AOS 3015    | \ref run                         | AbstractModeControl::manageNotReadyToDrive()
  AOS 2038    | \ref AbstractModeControl         | AbstractModeControl::isValidQRouteType()
  AOS 2039    | \ref AbstractModeControl         | AbstractModeControl::isValidQRouteType()
  AOS 1040    | \ref Console                     | AbstractModeControl::consoleCall()
  AOS 51 S    | \ref ATPModes                    | SafeBrakeToStopMode::handleMode()
  AOS 1134    | \ref ATPModes                    | AbstractModeControl::preProcessEventsForAllModes()
  AOS 1132    | \ref powerDownMode               | PowerDownMode::handleMode()
  AOS 2975    | \ref trainConfigMode             | TrainConfigMode::runTrainConfigStart()
  AOS 2976    | \ref trainConfigMode             | TrainConfigMode::runTrainConfigStart()
  AOS 2977    | \ref trainConfigMode             | TrainConfigMode::runTrainConfigStart()
  AOS 3235    | \ref trainRegMode                | TrainRegistrationMode::runTrainRegistrationStart()
  AOS 1758    | \ref trainConfigMode             | TrainConfigMode::runTrainConfigStart()
  AOS 3236    | \ref trainRegMode                | TrainRegistrationMode::runTrainRegistrationStart()
  AOS 2121    | \ref trainRegMode                | TrainRegistrationMode::runTrainRegistrationSetupBSTarget()
  AOS 2941    | \ref trainRegMode                | TrainConfigMode::runTrainConfigRejectedOrAborted()
  AOS 1008    | \ref ATPModes                    | LocationMode::handleMode()
  AOS 2411    | \ref ATPModes                    | LocationMode::handleMode()
  AOS 1244    | \ref run                         | AbstractModeControl::run()
  AOS 3265    | \ref sleepMode                   | SleepingMode::runSleepingNotSleepingWaitStandstill()
  AOS 137     | \ref powerDownMode               | PowerDownMode::handleMode()
  AOS 138     | \ref powerDownMode               | PowerDownMode::handleMode()
  AOS 139     | \ref powerDownMode               | PowerDownMode::handleMode()
  AOS 2229    | \ref powerDownMode               | PowerDownMode::handleMode()
  AOS 2223    | \ref powerDownMode               | PowerDownMode::powerDownRequested()
  AOS 431     | \ref powerDownMode               | PowerDownMode::handleMode()
  AOS 432     | \ref powerDownMode               | PowerDownMode::powerDownRequested()
  AOS 2686    | \ref AbstractModeControl         | AbstractModeControl::run()
  AOS 2942    | \ref AbstractModeControl         | AbstractModeControl::run()
  AOS 2683    | \ref powerDownMode               | PowerDownMode::blink()
  AOS 3264 S  | \ref AbstractModeControl         | AbstractModeControl::manageBrakeTest()
  AOS 2708 S  | \ref AbstractModeControl         | AbstractModeControl::manageBrakeTest()
  AOS 2790 S  | \ref eaSeq                       | EmergencyAlertSeq::runEmergencyAlertSeq()
  AOS 2650 S  | \ref safetyHaltMode              | SafetyHaltMode::handleMode()
  AOS 1995 S  | \ref safetyHaltMode              | SafetyHaltMode::handleMode()
  AOS 2015 S  | \ref ATPModes                    | SafeBrakeToStopMode::handleMode()
  AOS 2034 S  | \ref baliseSearchMode            | BaliseSearchMode::handleMode()
  AOS 2035    | \ref trainRegMode                | TrainRegistrationMode::handleMode()
  AOS 2036    | \ref srMode                      | StaffResponsibleMode::handleMode()
  AOS 2450    | \ref srMode                      | StaffResponsibleMode::handleMode()
  AOS 2951    | \ref joinMode                    | JoinMode::handleMode()
  AOS 2952    | \ref splitMode                   | SplitMode::handleMode()
  AOS 3167    | -                                | BaliseSearchMode::isValidUncondShorteningMsg()
  AOS 3166    | -                                | AbstractMode::isValidUncondShorteningMsg()
  AOS 308     | \ref ATPModes                    | LocationMode::handleMode()
  AOS 2119    | \ref yardMode                    | YardMode::handleMode()
  AOS 2120    | \ref yardMode                    | YardMode::handleMode()
  AOS 229     | \ref ATPModes                    | LocationMode::handleMode()
  AOS 2810 S  | \ref AbstractModeControl         | AbstractModeControl::run()
  AOS 2416    | \ref ATPModes                    | SafeBrakeToStopMode::handleMode()
  AOS 2699    | \ref AbstractModeControl         | AbstractModeControl::manageCabActiveStatus()
  AOS 2465    | \ref run                         | AbstractModeControl::run()
  AOS 1262    | \ref trainConfigMode             | TrainConfigMode::runTrainConfigWaitSetUpFrmATP()
  AOS 3168 S  | \ref eaSeq                       | EmergencyAlertSeq::run()
  AOS 3018    | \ref run                         | PossessionMode::runPossessionModeFinish()
  AOS 3208    | -                                | AbstractMode::manageTCCTimeOut()

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP Core.

  No architectural requirements explicitly related to this component available.

  */
}
