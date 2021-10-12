namespace ATP::Supv
{    
  /**
  \if AsMainPage
  \mainpage Target Calculation Component Specification (Adaptation)
  @anchor tc
  \endif

  \ifnot AsMainPage
  \class Template
  \endif

  \section Purpose Purpose
  This document specifies the software design for the Target Calculation adaptation component.
  Sufficient details provided for the component to be implemented.

  \subsection Abbreviations Abbreviations

  Abbreviation   | Definition
  -------------- | ------------------------------------------------------------------
  LCS            | Locomotive Control System
  MRT            | Most Restrictive Target

  \latexonly \newpage \endlatexonly

  \section Overview Overview
  The Adaptation of the Target Calculation component adds functionality needed for the BHP Project.
  The adaptation part is inherited from AbstractTargetCalculation.

  \subsection GeneralFunctionality General Functionality
  The component's main functionalities include:
  - Calculate the brake curve points to be sent to the LCS.
  - Restrict the Ceiling speed if Level crossing approach speed is set.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  The Target Calculation component depends on following components:
  - Targets: for the targets list and level crossing speed restriction.
  - Position: for train position.
  - TrainSetup: for brakeability.
  - Supervise: for brake curve calculations.
  - MessageHandler: To get the MA received status.

  Other components have dependencies to this component because they use its public types
  and methods, see \ref AbstractTargetCalculation Class Reference.

  \latexonly \newpage \endlatexonly

  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  This initializes the member variables and registers variables for cross comparison. It then calls the virtual init() function in the abstract class.

  \subsection ModeDependentOperation Mode Dependent operation
  The adaptation of Target Calculation component is independent of ATP mode.

  \subsection Scheduling Scheduling
  The Target Calculation component is scheduled by a call to the virtual run()-function each cycle.

  \subsubsection run run()
  The run function does the following:
  - Reset the last processed target position for the curve points sent to LCS.
  - Call AbstractTargetCalculation::run() for the core - functionality.

  \subsection calcModeCeilSpeed Calculate Mode Dependent Ceiling Speed
  This function overrides the function in AbstractTargetCalculation.
  It will call the AbstractTargetCalculation::calcModeDependentCeilingSpeed() to calculate the mode dependent ceiling speed.

  If the train is approaching a level crossing and the approach speed to level crossing is set, the function will return the minimum of the approach speed and the calculated ceiling speed.

  Otherwise, the calculated ceiling speed from AbstractTargetCalculation::calcModeDependentCeilingSpeed() function is returned.

  \subsection resetTargPos Reset Last Processed Target Position
  The AOS keeps record of the position till which AOS has sent the Warning curve to LCS.
  This value needs to be reset as follows:
  - To current antenna position if:
      - Train is Idling, OR
      - Position is Unknown or Doubtful.
  - To current leading end position if:
      - Brakeability changed
      - Brake response time changed
      - The target list changed due to Unconditional shortening of MA
      - First standstill in Location mode resulting is change of Ceiling speed
      - Direction reversed in Location mode

  \subsection calcCurvePoints Calculate Warning Curve Points
  The function calculates and stores the first warning curve points to be sent to LCS.
  The curve points are calculated when the train position is Known or Approx and:
  - the train position is ahead of the position till which AOS has sent the curve points.
  - the MRT has changed since the curves points were sent.

  The curve points are calculated for supervised targets in the list from the current position until any of the following conditions are true:
  - the supervised target is ahead of the MRT, OR
  - the maximum limit is reached for the curve points that can be sent in the message.

  If the maximum limit is reached, the AOS will send the message with maximum curve points. Remaining curve points to MRT are sent once the train position passes the position of the last sample point sent.

  The flowchart below describes the process of calculating the warning curve points:
  @image html target_calculation_calcCurvePoints.png "Calculate Warning Curve Points"
  @image latex target_calculation_calcCurvePoints.png "Calculate Warning Curve Points"

  If the next supervised target has lower first warning speed than the current sample speed, it will result in the calculation of curve points by reducing speed from the current first warning speed till the target first warning speed in steps of the  configured speed difference value.

  The figure below is an example of the curve points for targets:
  @image html target_calculation_curve_points.png "Example of Warning Curve Points"
  @image latex target_calculation_curve_points.png "Example of Warning Curve Points"

  The different colours of markers indicate that they are sent in separate message.
  The first MRT is STP<SUB>fw</SUB> for CS<SUB>2</SUB> target. So the curve points are sent till the supervised target which is before CS<SUB>2</SUB>.
  Once the train crosses the position of STP<SUB>eb</SUB> for CS<SUB>2</SUB> target, a new message with curve points is sent to the next MRT which is the STP<SUB>sb</SUB> for the Primary Target.

  \section ClassDiagram Class Diagram
  @image html target_calculation_class_diagram.png "TargetCalculation Class Diagram"
  @image latex target_calculation_class_diagram.png "TargetCalculation Class Diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console Commands
  See AbstractTargetCalculation.

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation
  The AbstractTargetCalculation implements the core requirements. The Target Calculation component is derived from AbstractTargetCalculation and overrides the core functions to implement the adaptation requirements.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component

  \section Traceability Traceability

  \subsection SSRS Functional Requirements
  The functional requirements are defined in [SSRS].

  The requirements relevant for this component are:
  
  Req             | Chapter                | Function
  --------------- | ---------------------- | --------
  AOS_BHPB 2699   | \ref calcCurvePoints   | TargetCalculation::calcPermSpeedCurvePoints
  AOS_BHPB 3420 S | \ref calcModeCeilSpeed | TargetCalculation::calcModeDependentCeilingSpeed

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].

  Common architectural requirements are specified in SCDS ATP Core.
  Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].
  
  */
}
