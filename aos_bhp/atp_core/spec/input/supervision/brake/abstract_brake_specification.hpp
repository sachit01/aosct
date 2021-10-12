namespace ATP::Supv
{

  /**
  \if AsMainPage
  \mainpage Brake Component Specification
  @anchor bk
  \endif

  \ifnot AsMainPage
  \class Brake
  \endif

  \section Purpose Purpose
  This document specifies the software design for the class AbstractBrake.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality
  The Brake component is responsible for activating, releasing and supervising the service and emergency brakes.
  It is responsible for \ref performBrakeTest, \ref abortBrakeTest, supervising the internal and external emergency brake feedback and service/emergency deceleration supervision.
  It also verifies internal and external TCO feedbacks and EB cut-out inputs configurations which are handled in adaptation of Brake component.

  The main functions of AbstractBrake are:
  - It evaluates request from Event Handler and internal states and then sets the flag that will be used by LocoIO to apply brakes.
  - Manages the flags for releasing the brakes based on events requesting the brake application and the current ATO mode.
  - Supervises the deceleration control for the brakes.
  - Performs Brake Test.

  The Brake component performs the above functions for both service and emergency brakes.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  The Brake class depends on the following components:
  - ATP Application:- To get the ATP Application cycle time in ms.
  - Mode Control:- To indicate if the brake test is requested or not, Brake is inhibited or not, to get ATO mode,
    to get the status of cabin active and to get the driver login sequence state.
  - Odometry:- To get the current speed in cm/s, slipping/sliding status and standstill status.
  - Event Handler:- To report the events and if EB/SB brake are applied or not.
  - Target:- To get the gradient values.
  - TSetup:- To get the brakeability and EB/SB response time.
  - Loco IO:- To get input and output digital values.
  - DMI Handler:- To get the DMI button status.

Other components have dependencies to this component because they use its public types
  and methods, see \ref ATP::Supv::AbstractBrake Class Reference.
  
  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  The Brake initialization performs following tasks:
  - Assigns default values to internal variables
  - Prepares persistent values for cross-compare.

  \subsection ModeDependentOperation Mode dependent operation
  Brake component is independent of ATP mode.

  \subsection Scheduling Scheduling
  The AbstractBrake has run() function, that must be called each execution-cycle:

  \subsubsection run run()

  - \ref ProcessServiceBrake and  \ref ProcessEmergencyBrake for setting, releasing and supervising service and emergency brakes.
  - 'Processing of TCO' and 'Processing of EB cut out' are handled in the adaptation.
  - Handles the functionality of \ref performBrakeTest, if a Brake Test has been requested by Mode Control or if a Brake Test is already in progress.

  @image html abstract_brake_run.png "run function"
  @image latex abstract_brake_run.png "run function"


  \subsubsection ProcessServiceBrake Process Service Brake
  It performs service brake specific functionality. This involves the following steps:
  - \ref checkAndApplySBrakeReq
  - \ref performSBrakeReleaseControl
  - \ref performSBrakeDecControl


  \subsubsection ProcessEmergencyBrake Process Emergency Brake
  It performs emergency brake specific functionality. This involves the following steps:

  - \ref checkAndApplyEBrakeReq
  - \ref performEBrakeReleaseControl
  - \ref performEBrakeDecControl

  \subsubsection performBrakeTest Performing the brake test
  If a brake test is requested by Mode Control, this functionality performs the brake test sequence by calling the \ref processBrakeTestStateMachine functionality,
  if timeout happened before the brake test is finished then it will raise the log and standstill event and reset the brake test machine.

  @image html abstract_brake_performBrakeTest.png "performBrakeTest function"
  @image latex abstract_brake_performBrakeTest.png "performBrakeTest function"

  \subsubsection performVitalDriverTest Performing the vital driver test
  Test of vital driver is performed to ensure that vital IO driver is getting activated or deactivated 
  when required. Initially AbstractBrake::performVitalDriverTest starts by turning the vital IO
  driver active during resetting the brake test state machine. After the brake test is initiated, it waits for
  driver feedback. Once the brake test has been performed, vital IO driver is turned deactivated. When all
  vital outputs are deactivated properly, vital driver test state machine  is set to 'VitalDriverTestSucceeded'.
  To handle various states AbstractBrake::VitalDriverTestState state machine is used.

  \subsubsection abortBrakeTest Aborting the brake test
  The functionality 'Abort brake test' evaluates the need to abort a Brake Test.
  Brake Test can be aborted by:
  - Driver or
  - if emergency or service brake is applied or
  - if brake is ordered by LocoIO or
  - cabin deactivation.

  \subsubsection processBrakeTestStateMachine Process Brake Test State Machine

  Pre-conditions to execute \ref processBrakeTestStateMachine functionality is that the complete brake-test has not timed-out and it is not aborted by a certain condition.

  The brake-test is executed by issuing Brake Release/Apply orders via the LocoIO component.
  The Brake test sequence is as per the table below:


  Step             | EB1 | EB2 | Expected Result from internal EB1/EB2 Relays
  ---------------- | ----|-----|---------------------------------------------
  Step 0           |  0  |  0  |   0/0
  Step 1           |  1  |  0  |   1/0
  Step 2           |  0  |  0  |   0/0
  Step 3           |  0  |  1  |   0/1
  Step 4           |  0  |  0  |   0/0


  Brake also updates the Mode Control and DMI Handler with the progress and result of the Brake Test performed. \n
  The below given sequence diagram describes how a Brake Test is triggered and processed.

  @image html brakeTestSequence.png  "Brake Test Sequence"
  @image latex brakeTestSequence.png  "Brake Test Sequence"

  \subsubsection SBOperations Service Brake Operations

  \paragraph checkAndApplySBrakeReq Checking for service brake requests and acting on them

  The components can request to apply service brake by generating an Event. The Event Handler will receive all the requests.
  This functionality will check if any request for activating the service brakes was received this cycle and then service brake is applied accordingly.

  The following conditions should be considered:
  - Service brake will be applied, if there is a request to apply the service brake, if service brake access is available and the brake inhibit is not active. In this case,
    supervision of the SB deceleration should be started.
  - Service brake will also be applied in a case where, service brake is not requested but following conditions exist:
     - SB is available to be applied,
     - EB is already applied and
     - The brake inhibit is not active.

  Note that if service brake is already applied and brake inhibit is active, in such case service brake is removed.

  \paragraph isSBReleaseReqActive Check and apply service brake releaser as per current request

  Based on the component which has requested the service brake, there is an authority level on who can release the brake.
  Three authority levels are used when releasing the service brake, which in order of decreasing authority level are:
  1. Driver, via DMI
  2. TCC
  3. ATO

  Whenever ATO mode is set to "ATOManual", the brake will be released by driver.

  The Event Handler will report the highest release authority in each cycle based on the service brake requests it has received. 
  The Brake component will keep track of the highest authority level required to release the brake. 
  The authority level can be increased if Event Handler reports higher authority in current cycle but never decreased. 
  The authority level is cleared once the service brake is released.

  The service brake is released if a release signal is received from any releaser equal to or higher in authority than the current service brake release authority level.
  (Refer \ref performSBrakeReleaseControl for more details).

  \paragraph performSBrakeReleaseControl Performing the service brake release functionality
  This functionality will check if any event requested service brake or not. If service brake is applied and emergency brakes are not applied and no event has requested for SB then it will enable the SB release.
  It resets SB parameters when SB release is enabled and SB release request status is enabled and emergency brakes are not applied.

  \paragraph performSBrakeDecControl Supervising the service brake deceleration control

  If service brake is applied, supervision of service brake deceleration is performed. It is continuously monitored.
  If current deceleration is not appropriate enough then we have incorrect readings, which is called \a bad retardation.
  In other words, 'Bad retardation' means deceleration was not sufficient enough as it should have been when service brake was applied.

  Following parameters are supervised:

  Term       | Definition                                        | Unit
  ---------- | ------------------------------------------------- |--------
  \f[t_{SBdec}\f]| Time between a service brake order and start of the supervision of an applied service brake| ms
  \f[t_{SBv0}\f]| Time measured from detection of \f$v_a < v_{SBv0}\f$. The supervision of an applied service brake requires zero speed within this time.| ms
  \f[v_{SBdec}\f]| Upper speed boundary value used during supervision of an applied service brake.| cm/s
  \f[v_{SBv0}\f]| Lower speed boundary value used during supervision of an applied service brake.| cm/s
  \f[R_{sb}\f]| Train service brake deceleration from manufacturer data.| cm/s<sup>2</sup>
  \f[n_{SBSampInRow}\f]| Maximum allowed number of bad retardation readings in a row.| -
  \f[n_{SBSampTotal}\f]| Maximum allowed total number of bad retardation readings during one SB application.| -

  This control takes no consideration of slip-slide. The service brake deceleration check verifies that the resulting deceleration from the service brake is sufficient, when initiated by the ATP.
  The brake reaction time of t<sub>SBdec</sub> ms is taken into account.

  @image html brakeSupervision.png  "Brake Supervision"
  @image latex brakeSupervision.png  "Brake Supervision"

  In the above figure there are 4 zones with different actions, the table below describes each action.

  Zone       | Action
  ---------- | -------------------------------------------------
  A| No action, wait t<sub>SBdec</sub> before starting supervision.
  B| Expected deceleration should be R<sub>sb</sub>. Actual deceleration will be at least configurable deceleration limit percentage of expected deceleration. (see also the filter description below)
  C| Expected deceleration should be calculated according to \f[d_{exp} = \frac{R_{sb}}{v_{SBdec}}.v\;(cm/s^2)\f] Actual deceleration will be at least configurable deceleration limit percentage of expected deceleration. (see also the filter description below)
  D| No deceleration supervision, train speed must be zero within t<sub>SBv0</sub>.

  In a descent less deceleration is required. The expected deceleration described above is reduced with the train's gravitational influence in a descent, which is approximately equal to the track gradient in 0.1%, see figure below.

  @image html brakeGradient.png  "brake Gradient"
  @image latex brakeGradient.png  "brake Gradient"


 If the target gradient is positive, i.e. the train is in an ascent, no reduction of expected deceleration should be made.

  Due to possible oscillation in the train during braking with unbraked cars, occasional readings of the retardation that is less than the expected deceleration may occur. A filter is introduced in zone B and zone C to avoid triggering a failure in these cases.

  This filter shall check two things:
  - That number of bad readings in a row is lower than n<sub>SBSampInRow</sub>.
  - That the total number of bad readings in one brake application is less than n<sub>SBSampTotal</sub>.

  If any one of these check fails then emergency brake is requested.

  \subsubsection EBOperations Emergency Brake Operations

  \paragraph checkAndApplyEBrakeReq Checking for emergency brake requests and acting on them

  The components can request to apply emergency brake by generating an Event. The Event Handler will receive all the requests. 
  This functionality will check if any request for activating the emergency brakes was received this cycle and 
  then emergency brake is applied accordingly.
  
  The following conditions should be considered:
  - Emergency brake can be applied if there is a request to apply the emergency brake or if there is
    a request to apply service brake but it is not available to be applied or if SB is escalated to EB.
    In such case brake inhibit is not active. Supervision of the EB deceleration should be started.
    These are few conditions that arises and should be noticed:
     - EB is applied before SB application.
     - EB is applied instead of SB as no valid train setup exist. It means SB is escalated to EB.
     - EB is applied instead of SB since SB is not available.

  - Otherwise, emergency brake inputs are read from LocoIO component. A safety halt event is reported 
    if any one of the input is active high and feedback relay condition has crossed the maximum time.
    In such case, brake test becomes mandatory after recovering from safety halt.
    EB feedback from the Brake Pressure sensors is read and verified against the expected value.

  Note that if emergency brake is already applied and brake is inhibit then it will remove the emergency brake.

  \paragraph isEBReleaseReqActive Check and apply emergency brake releaser as per current request

  Emergency brake release should be allowed only by the Driver.
  where this functionality returns true when current EB releaser is greater than equal to required releaser otherwise false.
  (Refer \ref performEBrakeReleaseControl for more details).

  \paragraph performEBrakeReleaseControl Performing the emergency brake release functionality

  The Event Handler will report the highest release authority in each cycle based on the emergency brake requests it has received. The Brake component will keep track of the highest authority level required to release the brake. The authority level can increase if Event Handler reports higher authority in current cycle but never decreased. The authority level is cleared once the emergency brake is released.

  When the train is stopped and no emergency brake requests are active, the emergency brake can be released. The emergency brake is released if a release signal is received from any releaser equal to or higher in authority than the current emergency brake release authority level.

  \paragraph performEBrakeDecControl Supervising the emergency brake deceleration control

  The emergency brake deceleration supervision is performed every cycle, when emergency brake is applied. It is monitored continuously.
  The similar graph can be seen for deceleration supervision for emergency brakes (Refer Figure 4, in chapter \ref performSBrakeDecControl).

  Following parameters are monitored:

  Term                  | Definition                                                                                       | Unit
  ----------------------| ------------------------------------------------------------------------------------------------ |---------------------------------------------
  \f[t_{EBdec}\f]       | Time between an emergency brake order and start of the supervision of an applied emergency brake.| ms
  \f[t_{EBv0}\f]        | Time measured from detection of \f$v_a < v_{EBv0}\f$. The supervision of an applied emergency brake requires zero speed within this time.| ms
  \f[v_{EBv0}\f]        | Speed boundary value used during supervision of an applied emergency brake.                      | cm/s
  \f[R_{eb}\f]          | Train emergency brake deceleration from manufacturer data.                                       | cm/s<sup>2</sup>
  \f[n_{EBSampInRow}\f] | Maximum allowed number of bad retardation readings in a row.                                     | -
  \f[n_{EBSampTotal}\f] | Maximum allowed total number of bad retardation readings during one EB application.              | -
  \f[t_{EBR}\f]         | Time measured from an emergency brake order. The emergency brake relay feedback is required within this time.| ms
  \f[t_{MinSlipSlide}\f]| Minimum time the emergency brake deceleration is inhibited from the detection of slip slide.     | s
  \f[t_{MaxSlipSlide}\f]| Maximum time the emergency brake deceleration is inhibited from the detection of slip slide.     | s

  The emergency brake deceleration control verifies that the resulting deceleration from the emergency brake is sufficient when initiated by the ATP. It is considered that from an emergency brake decision, it takes about t<sub>EBdec</sub> before the expected deceleration is achieved.

  The emergency brake deceleration control is inhibited from the moment slip-slide is detected until t<sub>MinSlipSlide</sub> after slip-slide has ceased. However, the emergency brake deceleration control is never inhibited during more than t<sub>MaxSlipSlide</sub> after slip-slide has been detected. When the speed is above v<sub>EBv0</sub> the deceleration required from an emergency brake application is configurable deceleration limit percentage of R<sub>eb</sub>.

  In a descent, negative gradient, less deceleration is required. The expected deceleration is reduced with the train's gravitational influence, which is approximately equal to the track gradient in 0.1%, see section \ref SBOperations.

  The target gradient value is positive, i.e. the train is in an ascent, no reduction of expected deceleration should be made. Below speed v<sub>EBv0</sub> the expected deceleration is considered to be constant, regardless of the track gradient. It is then required that the train halts within t<sub>EBv0</sub>.

  Due to possible oscillation in the train set during braking with unbraked cars, occasional readings of the retardation that is less than configurable deceleration limit percentage of the expected deceleration may occur.  A filter is introduced to avoid failure in these cases.

  This filter shall check two things:
  - That number of bad readings in a row is lower than n<sub>EBSampInRow</sub>.
  - That the total number of bad readings in one brake application is less than n<sub>EBSampTotal</sub>.

  A Fatal Failure Event due to bad emergency brake is raised if any of the above condition fails and the system is shut down.

  \paragraph verifyInternalEbRelays Verify Internal EB Relays

  When emergency brake is applied, the internal output-relays are verified with the expected output.
  The emergency brake relay feedback supervision is performed once the emergency brake is applied during brake test. 
  This check if the emergency brake relays are set within the time t<sub>EBR</sub> from when they were requested, if not the required details are logged.

  \section ClassDiagram Class Diagram

  @image html abstract_brake_class_diagram.png "Class diagram"
  @image latex abstract_brake_class_diagram.png "Class diagram"


  \section Diagnostics Diagnostics


  \subsection Console Console Commands
  The following component-specific console commands are implemented:

  The 'brake'- command will display following parameters:
  - All brakes inhibited
  - EB Applied
  - EB Release Enable
  - SB Applied
  - SB Release Enable
  - Brake Test Step
  - EB1 Test Applied
  - EB2 Test Applied
  - TCO Test Applied

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation

  All the main functionality for the Brake component is in the core part.
  It calls processTco() and processEbCutOut(), pure virtual functions which are handled in the adaptation.
  The adaptation can override functionality related to the brake-test, abort brake test and check TCO and EB feedback e.g. 
  define its own state-machine and feedback verifications.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component.

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP Core.
  The requirements relevant for this component are:

  Req        |Chapter                  | Function
  ---------- |-------------------      | ---------------------------------------------
  AOS 371 S  | \ref SBOperations       | AbstractBrake::performSBrakeDecControl()
  AOS 372 S  | \ref EBOperations       | AbstractBrake::performEBrakeDecControl()
  AOS 2071   | \ref SBOperations       | AbstractBrake::performSBrakeReleaseControl()
  AOS 2072 S | \ref EBOperations       | AbstractBrake::performEBrakeReleaseControl()
  AOS 2073   | \ref SBOperations       | AbstractBrake::performSBrakeReleaseControl()
  AOS 2074   | \ref SBOperations       | AbstractBrake::performSBrakeReleaseControl()
  AOS 2075   | \ref EBOperations       | AbstractBrake::performEBrakeReleaseControl()
  AOS 2098   | \ref SBOperations       | AbstractBrake::checkAndApplySBrakeReq()
  AOS 2099   | \ref EBOperations       | AbstractBrake::checkAndApplyEBrakeReq()
  AOS 2100   | \ref EBOperations       | AbstractBrake::checkAndApplyEBrakeReq()
  AOS 2278   | \ref SBOperations       | AbstractBrake::performSBrakeDecControl()
  AOS 2279   | \ref EBOperations       | AbstractBrake::performEBrakeDecControl()
  AOS 2586   | \ref SBOperations       | AbstractBrake::checkAndApplySBrakeReq()
  AOS 2612 S | \ref processBrakeTestStateMachine | AbstractBrake::processBrakeTestStateMachine()
  AOS 2615 S | \ref performBrakeTest   | AbstractBrake::performBrakeTest()
  AOS 2636 S | \ref performBrakeTest   | AbstractBrake::performBrakeTest()
  AOS 2638   | \ref abortBrakeTest     | AbstractBrake::abortBrakeTest()
  AOS 2640 S | \ref abortBrakeTest     | AbstractBrake::abortBrakeTest()
  AOS 2641   | \ref performBrakeTest   | AbstractBrake::performBrakeTest()
  AOS 2642   | \ref performBrakeTest   | AbstractBrake::performBrakeTest()
  AOS 2643   | \ref performBrakeTest   | AbstractBrake::performBrakeTest()
  AOS 2816   | \ref EBOperations       | AbstractBrake::checkAndApplyEBrakeReq()
  AOS 2959   | \ref EBOperations       | AbstractBrake::checkAndApplyEBrakeReq()
  AOS 3264 S | \ref performBrakeTest   | AbstractBrake::performBrakeTest()

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].
  Common requirements are specified in SCDS ATP Core.

  Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

  */

}
