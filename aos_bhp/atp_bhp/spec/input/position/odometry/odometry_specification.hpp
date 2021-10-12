namespace ATP::Pos::Odometry
{
  /**
  \if AsMainPage
  \mainpage Odometry Component Specification
  @anchor odo
  \endif

  \ifnot AsMainPage
  \class Odometry
  \endif

  \section Purpose Purpose
  This document specifies the software design for the class Odometry,
  the BHP adaptation part for the %Odometry component.
  
  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality
  The adaptation of %Odometry component shall instantiate the AbstractOdometry class. It generates a singleton
  object and provides access to it.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  
  Other components have dependencies to this component because they use its public types
  and methods, see \ref Odometry Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  The virtual init() - function is not implemented in the adaptation.

  \subsection ModeDependentOperation Mode Dependent Operation
  The adaptation part of %Odometry component is mode independent.

  \subsection Scheduling Scheduling
  The virtual run() - function is not implemented in the adaptation.

  \section ClassDiagram Class Diagram
  @image html odometry_class_diagram.png "Class diagram"
  @image latex odometry_class_diagram.png "Class diagram"
   
  \section Diagnostics Diagnostics
  
  \subsection Console Console Commands
  N/A

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation
  The adaptation contains the instantiation of core AbstractOdometry class.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component.

  \section Traceability Traceability

  \subsection SSRS Functional Requirements
  The functional requirements are defined in [SSRS].
  Common functional requirements are described in SCDS ATP BHP.

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].
  Common requirements are specified in SCDS ATP BHP.

  */
}
