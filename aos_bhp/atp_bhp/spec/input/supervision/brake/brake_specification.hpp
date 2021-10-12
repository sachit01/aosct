namespace ATP::Supv
{

  /**
  \if AsMainPage
  \mainpage Brake Component Specification (Adaptation)
  @anchor bk
  \endif

  \ifnot AsMainPage
  \class Brake
  \endif

  \section Purpose Purpose
  This document specifies the software design for the class Brake, an adaptation of the
  core AbstractBrake class.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality

  The main purpose of this class is to process the adaptation-specific \ref processBrakeTestStateMachine, \ref abortBrakeTest, to supervise external EB-feedback, TCO feedback and
  EB cut-out inputs. It also monitors the rapid pressure loss, brake pressure when no emergency brake is applied.
  Configuration parameters define for EB cut-outs and external brake Pressure feedback sensors. The Brake component reads these parameters from Config component and supervises the brake pressure feedback and EB-cut outs accordingly.

  The Traction cut off (TCO) output order and input feedback can be configured to be used both, only TCO-feedback or neither of them.
  Further on, the brake-test functionality can be configured to supervise the EB and/or the TCO feedback. Brake test machine is also overridden in the adaptation. It also verifies the internal relays of TCO (if configured for use).

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  The Brake class depends on the following components:
  - Loco IO: To get digital input and output values.
  - DMI Handler:- To get the DMI button status.
  - Mode Control: To get the status of cabin, is cabin active or not.
  - Event Handler: To report the events to TCC or DMI.

  Other components have dependencies to this component because they use its public types
  and methods, see \ref ATP::Supv::Brake Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization

  The virtual Brake::init() function is implemented in the adaptation as below:
  - Initializes the internal variables of core class and prepares the values for cross-compare.
  - Prevents multiple initialization.
  - Registers the relevant internal variables in analyzer interface.

  \subsection ModeDependentOperation Mode dependent operation
  Brake component is independent of ATP mode.

  \subsection Scheduling Scheduling
  The Brake has Brake::run() function (inherited from AbstractBrake), that must be called each execution-cycle.

  \subsubsection run run()
  The virtual run() function performs the below task:

  - Run() function from AbstractBrake class is executed first to process service and emergency brakes.
  - Handles the functionality of \ref evaluateRapidPressureLoss.
  - LCS ready check is added to acquire brake feedback only when LCS is connected. This prevents unnecessary logs at startup
    and brake test is not running. As during brake test, there is a gradual brake pressure buildup and during that time if
    brake pressure is less than the value what it should be to consider EB released. It will log brake pressure values
    inaccurate. This will still be the case when EB is released after EB application and brake pressure builds gradually.
  - Handles \ref evaluateBrakePressureFeedbackNoEb functionality to evaluate brake pressure feedback when EB is not ordered.

  @image html brake_run.png  "run function"
  @image latex brake_run.png  "run function"

  \subsubsection getBrakePressureFb Evaluate EB Feedback
  In 'Brake pressure feedback' functionality, the EB feedback signal status(configuration parameter) is read,
  and the brake pressure values are evaluated as follows:
  EB feedback signal status     | Description                | Evaluated brake pressure value
  ------------------------------| ---------------------------| ------------------------------
  0                             | No external EB feedback configured for use | -
  1                             | EB Feedback Sensor 1 configured for use | Analog Value read from BrakePressure1
  2                             | EB Feedback Sensor 2 configured for use | Analog value read from BrakePressure2
  3                             | Both EB Feedback Sensor 1 and 2 configured for use | Mean of the analog values read from BrakePressure1 and BrakePressure2

  If both of EB feedback sensors are configured for use then the difference between both of the values will be calculated.
  If it is found to be less than the maximum difference between the brake pressure sensors then, mean of analog values from both the brake pressure will be calculated.

  \subsubsection checkEbFeedback Check EB feedback
  If external EB feedback sensor(s) are configured for use, the functionality 'Check EB feedback' is called when emergency brakes order is active.
  For more details refer the flow chart below:

  A brake test becomes mandatory, if EB feedback supervision causes a Safety Halt.
  Hence, the cause for the Safety Halt is stored in the brake test reason.

  \ref checkTcoFeedback if TCO order and TCO feedback is used.

  @image html check_eb_feedback.png  "checkEbFeedback function"
  @image latex check_eb_feedback.png  "checkEbFeedback function"

  \subsubsection checkTcoFeedback Check TCO feedback

  If the TCO order and/or feedback is configured for use, 'Check TCO feedback' functionality is responsible to check the relays and external feedback signal.
  A brake test becomes mandatory during the power up if TCO feedback supervision causes a Safety Halt.

  For more details refer the flow chart below:
  @image html check_tco_feedback.png  "checkTcoFeedback function"
  @image latex check_tco_feedback.png  "checkTcoFeedback function"


  \subsubsection evaluateBrakePressureFeedbackNoEb Evaluate brake pressure feedback no EB

  When EB is not ordered, brake pressure values should be sufficiently high to not detect any EB and the brake pressures are continuously monitored for any inconsistency and inaccuracy.
  If configured brake sensors are invalid and if there is ambiguity in 2 values for configured amount of time, corresponding Log events are raised.

  \subsubsection processBrakeTestStateMachine Brake test state machine

  An adaptation of the brake-test state-machine is executed by issuing brake and TCO release/apply orders via the LocoIO component.
  The brake test sequence is as per the table below:

  Step             | TCO Order | EB1 Order | EB2 Order | TCO Feedback | EB Feedback
  ---------------- | ----------| ----------|-----------|--------------|------------
  Step 0           |  0        |  0        |  0        |  0           | 0
  Step 1           |  1        |  0        |  0        |  1           | 0
  Step 2           |  0        |  0        |  0        |  0           | 0
  Step 3           |  0        |  1        |  0        |  1/-         | 1
  Step 4           |  0        |  0        |  0        |  0           | 0
  Step 5           |  0        |  0        |  1        |  1/-         | 1
  Step 6           |  0        |  0        |  0        |  0           | 0

  (0 = release order / released status, 1 = apply order / order is applied, - = don't care (when only TCO feedback is used, and not TCO order))


  It is configurable, if the TCO feedback and/or the EB feedback should be used in the brake-test evaluation.
  If the TCO order is configured not to be used the step 1 and 2 is not executed.

  The time for detecting proper feedback when applying the EB and TCO feedback are configurable (for the release of EB and TCO the timeout is set to the timeout of the whole brake-test).

  \subsubsection abortBrakeTest Aborting the brake test
  The functionality 'Abort brake test' is overridden in adaptation. It evaluates the need to abort a brake test.\n
  To abort the brake test below are responsible:
  - driver or
  - when additional brake orders are active or
  - when cabin is deactivated
  - when Emergency stop active whereas loco type is not EMD

  \subsubsection verifyBrakeTestFeedback Verify brake test feedback
  When the brake test is in progress, it initially verifies the brake test feedback.
  The functionality 'verify brake test feedback' is overridden in adaptation.
  It reads and verifies the EB1,EB2 and TCO relays and/or the external feedback. \n

  - If EB feedback or EB and TCO feedback is requested during EB internal and external brake test then '\ref verifyExternalEbFeedback' is performed.
  - If EB feedback or EB and TCO feedback is not requested during EB internal and external brake test then 'Verification of internal EB relays' (which is the functionality of core class) is performed.
  - If TCO feedback or EB and TCO feedback is requested during TCO internal and external brake test then '\ref verifyExternalTcoFeedback' is performed.
  - If TCO feedback or EB and TCO feedback is not requested during TCO internal and external brake test then 'Verification of internal TCO relays' is performed.
  - If there is timeout during brake test sequence step is in initial step, brake test is unable to start.
  - If there is a timeout and EB internal and external TCO feedback are not ok then, brake test is failed due to internal TCO Feedback failure or 
    external TCO brake pressure feedback failure.
  - If there is a timeout and EB internal and external feedback are not ok then, brake test is failed due to internal EB feedback failure and 
    external EB feedback failure.
  - After all EB/TCO internal and external feedback, diagnostic measurement values are updated in analyzer.

  \subsubsection verifyExternalEbFeedback Verification of external EB feedback
  The 'Verification of external EB feedback' verifies the EB feedback from the brake pressure value and match with the configured value of the brake pressure level.
  It will count the number of valid EB feedback readings on the basis of brake pressure value,
  expected feedback status and previous cycle expected value of EB. If the brake pressure value
  with error margin is less than config value of maximum EB applied feedback or if it is greater
  than minimum EB release feedback then make sure the value is stable for a certain time value.

  \subsubsection verifyExternalTcoFeedback Verification of external TCO feedback
  The 'Verification of external TCO feedback' takes the value from LocoIO (digital input values)
  and compare it with the configured value of TCO order and TCO feedback signal. If TCO feedback is not
  available, verification is not possible. Verification is valid if there are correct consecutive readings.


  \subsubsection processEbCutOut Processing of EB cut-out inputs
  The 'Processing of EB cut-out inputs' is called every cycle. 'EB cut-out inputs' can be used or not,
  is a configured value.
  If in use and the EB cut-out inputs indicate "Cut-out", a Safety Halt is triggered.
  A brake test becomes mandatory during the power up after this Safety Halt and the cause
  for the Safety Halt is stored in run time config by name.

  \subsubsection processTco Processing traction cut-off
  Traction cut-off should be applied whenever emergency brake is applied. This is used by Loco IO to determine the output value for TCO signal. \n
  The 'Process TCO' functionality is called in every cycle which reads the configuration parameter to check if the TCO feedback should be used or not. It first checks if TCO is applied or not. It calculates the time from when the TCO is applied and in turns checks TCO feedback.

  \subsubsection evaluateRapidPressureLoss Evaluating the Rapid Pressure Loss
  This functionality evaluates rapid loss of brake pressure if EB feedback from vehicle brake system (BP1 and BP2) are configured for use and BP1 and/or BP2 detects a pressure drop per time unit greater than the configured values for 'Rapid loss of brake pressure'.

  - Rapid brake pressure loss is detected when last saved delta pressure to trigger rapid loss of brake pressure is more than current
    delta brake pressure.
  - If either of the brake pressure loss is detected, a flag is raised.

  \section ClassDiagram Class Diagram
  @image html brake_class_diagram.png "Brake class diagram"
  @image latex brake_class_diagram.png "Brake class diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console Commands
  The 'brake' command will display following parameter:
  - TCO Test Applied

  \subsection Analyze Analyze
  The Brake component reports the following to Analyzer.
- EB/TCO feedback
- EB/TCO order
- break-sequence step

  \section CoreAdaptation Core / Adaptation
  All the main functionality for the brake is in the core.
  The adaptation is functionality related to the brake-test, abort brake test and check TCO and EB feedback e g define its own state-machine and feedback verifications.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP BHP.
  The requirements relevant for this component:

  Req             | Overriden Req     | Chapter                                                        | Function
  --------------- | ------------------|--------------------------------------------------------------- | ----------------
  AOS_BHPB 2759 S |                   | \ref checkEbFeedback                                           | Brake::checkEbFeedback()
  AOS_BHPB 2761 S |                   | \ref evaluateBrakePressureFeedbackNoEb                         | Brake::evaluateBrakePressureFeedbackNoEB()
  AOS_BHPB 2762 S |                   | \ref evaluateBrakePressureFeedbackNoEb                         | Brake::evaluateBrakePressureFeedbackNoEB()
  AOS_BHPB 2768 S | AOS 2629 S        | \ref processEbCutOut                                           | Brake::processEbCutOut()
  AOS_BHPB 2814 S |                   | \ref processTco                                                | Brake::processTco()
  AOS_BHPB 2822 S |                   | \ref abortBrakeTest                                            | Brake::abortBrakeTest()
  AOS_BHPB 2646 S |                   | \ref processEbCutOut                                           | Brake::processEbCutOut()
  AOS_BHPB 2847 S | AOS 2631 S        | \ref processBrakeTestStateMachine                              | Brake::processBrakeTestStateMachine()
  AOS_BHPB 2848 S | AOS 2630 S        | \ref processBrakeTestStateMachine                              | Brake::processBrakeTestStateMachine()
  AOS_BHPB 2878 S | AOS 2653 S        | \ref verifyBrakeTestFeedback                                   | Brake::verifyBrakeTestFeedback()
  AOS_BHPB 2880 S |                   |\ref verifyExternalEbFeedback, \ref verifyExternalTcoFeedback   | Brake::verifyExternalEbFeedback(), Brake::verifyExternalTcoFeedback
  AOS_BHPB 2962 S |                   |\ref checkTcoFeedback                                           | Brake::checkTcoFeedback()
  AOS_BHPB 2963 S |                   |\ref checkEbFeedback                                            | Brake::checkEbFeedback()
  AOS_BHPB 2966   | AOS 2654          |\ref abortBrakeTest                                             | Brake::abortBrakeTest()
  AOS_BHPB 3136 S |                   |\ref evaluateRapidPressureLoss                                  | Brake::evaluateRapidPressureLoss()

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].
  Common requirements are specified in SCDS ATP BHP.

  Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

  */

}
