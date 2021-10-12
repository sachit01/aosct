namespace ATP::DS
{
  /**
  \if AsMainPage
  \mainpage Targets Component Specification
  @anchor ta
  \endif

  \ifnot AsMainPage
  \class Targets
  \endif

  \section Purpose Purpose
  This document specifies the software design for Adaptation part of the Targets component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview
  \subsection GeneralFunctionality General Functionality
  The Targets class of Targets component instantiates the AbstractTargets class. It generates a singleton object
  and provides access to it.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  This component has dependencies to the following components:
  + Position: To get current position of train.
  + Target Calculation: To get mode dependent Ceiling Speed.
  + Odometry: To get current speed.

  Other components have dependencies to this component because they use its public types
  and methods, see \ref Targets Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design
  \subsection Initialization Initialization
  The Targets class is responsible to initialize all attributes of the component and allocate the memory for the TrackDataItemTargetBHP.

 
  \subsection ModeDependentOperation Mode dependent operation
  Targets is not mode dependent.


  \subsection Scheduling Scheduling
  The Targets component is completely scheduled in core part.

  \subsection levelcrossing Level Crossing
  Level Crossing has speed restriction, which applies from the position of a TrackDataItemTargetBHP target until 
  the train front has passed the approach distance from the target.

  \section ClassDiagram Class Diagram
  @image html targets.png  "Targets class diagram"
  @image latex targets.png  "Targets class diagram"

  \section Diagnostics Diagnostics
  \subsection Console Console Commands
  N/A

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation
  Most of the functionalities of Targets Component is implemented in core part of it. Adaptation part adds the project specific (BHPB) functionality
  and creates the singleton instance of Targets Component.

  \section PreProcessor Pre-Processor Directives

  No pre-processor directives available for this component

  \section Traceability Traceability

  \subsection SSRS Functional Requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP BHP.

  The requirements relevant for this component are:

  Req           | Chapter                    | Function
  --------------| ---------------            | ---------
  AOS_BHPB 3420 S | \ref levelcrossing         | TrackDataItemTargetBHP::calcApproachSpeed()

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP BHP.

  Fulfillment of other architectural requirements allocated to the ATP is described in [SWAS].

  */
}
