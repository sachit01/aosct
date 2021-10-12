namespace ATP::Supv
{
    /**
  \if AsMainPage
  \mainpage Supervise (BHP Adaptation) Component Specification
  @anchor sup
  \endif

  \ifnot AsMainPage
  \class Template
  \endif

  \section Purpose Purpose
  This document specifies the software design for Supervise, the BHP adaptation of the AbstractSupervise.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality

  The BHP Adaptation of the Supervise component adds functionality needed for the BHP Project.
  The adaptation is inherited from AbstractSupervise.

  The BHP Adaptation of Supervise includes Extended Reversing Supervision which allows train to reverse
  long distances in the direction opposite to MA. The AOS supervises the distance and speed in extended reversing and intervenes to ensure safe operation.

  \subsection DeploymentDiagram Deployment Diagram
  N/A


  \subsection Dependencies Dependencies

  Supervise depends on the following components:

  + Message Handler: for accessing MA.
  + Mode Control: for reading the current mode.
  + Odometry: for detecting movement.
  + Targets: for retrieving direction of MA.

  Other components are dependent on this components functionality by its public functions. <br>
  Refer to Public Member Functions from \ref Supervise Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  N/A

  \subsection ModeDependentOperation Mode Dependent Operation
  \ref ExtdRevSuperv is enabled in ATP mode Normal.

  \subsection Scheduling Scheduling
  The virtual \ref run() function is not implemented in adaptation. Instead, the core component calls the virtual function
  \ref Supervise::revSupervision from its run() function. Supervise::revSupervision()
  is overridden in this component in order to implement \ref ExtdRevSuperv.

  \subsection ExtdRevSuperv Extended Reversing Supervision
  - The feature Extended Reversing Supervision is activated if the config parameter Extended Reversing Distance (ERD) is greater than zero.
  - The following actions are taken by the component when Extended Reversing Supervision is activated:
    + If Extended Reversing Supervision is enabled, the reversing supervision in the core component shall be disabled.
    + Monitor the current speed against Extended Reversing ceiling speed (ERS) when the train moves against the MA's direction.
    + Apply service brake when the accumulated reverse distance travelled is more than ERD.
    + Apply emergency brake when the accumulated reverse distance travelled is more than ERD plus the emergency brake margin.
    + Apply service brake when current speed exceeds ERS plus the service brake margin.
    + Apply emergency brake when current speed exceeds ERS plus the emergency brake margin.
    + Extended Reversing Supervision is disabled if the vehicle is at standstill and the accumulated reverse distance travelled is more than ERD.
    + Extended Reversing Supervision is disabled if the ATP mode is not Normal.

  \subsection timsSup TIMS Supervision
  The adaptation overrides the TIMS supervision by implementing Supervise::superviseTims() function from the core to disable brake application when TIMS is supervised and broken.

  \section ClassDiagram Class Diagram
  @image html supervise_class_diagram.png "Supervise class diagram"
  @image latex supervise_class_diagram.png "Supervise class diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console Commands
  N/A

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation
  \subsection revSup Reverse Supervision
  The virtual function \ref AbstractSupervise::revSupervision is overridden in this component
  in order to implement \ref ExtdRevSuperv (which is BHP specific functionality).

  The Supervise component overrides the core to NOT raise a Brake event if the TIMS is supervised and Broken.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component

  \section Traceability Traceability

  \subsection SSRS Functional Requirements
  The functional requirements are defined in [SSRS].

  The requirements relevant for this component are:

  Req             | Chapter             | Function
  --------------- | ------------------- | --------
  AOS_BHPB 2856   | \ref timsSup        | Supervise::superviseTims
  AOS_BHPB 3433 S | \ref ExtdRevSuperv  | Supervise::isExtRevSupervisionEnabled
  AOS_BHPB 3439   | \ref ExtdRevSuperv  | Supervise::superviseExtRevDistance, Supervise::isExtRevSupervisionEnabled
  AOS_BHPB 3440   | \ref ExtdRevSuperv  | Supervise::revSupervision
  AOS_BHPB 3441 S | \ref ExtdRevSuperv  | Supervise::superviseExtRevDistance
  AOS_BHPB 3442 S | \ref ExtdRevSuperv  | Supervise::superviseExtRevDistance
  AOS_BHPB 3444 S | \ref ExtdRevSuperv  | Supervise::superviseExtRevCeilingSpeed, Supervise::revSupervision
  AOS_BHPB 3453 S | \ref ExtdRevSuperv  | Supervise::isExtRevSupervisionEnabled, Supervise::revSupervision

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].

  Common architectural requirements are specified in SCDS ATP Core.
*/
}
