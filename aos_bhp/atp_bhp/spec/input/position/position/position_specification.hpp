namespace ATP::Pos
{
  /**
  \if AsMainPage
  \mainpage Position Component Specification (Adaptation)
  @anchor pos
  \endif

  \ifnot AsMainPage
  \class Position
  \endif

  \section Purpose Purpose
  This document specifies the software design for the Adaptation part of the Position component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview
  The Position class is the adaptation of the AbstractPosition class. Its purpose is to
  instantiate the AbstractPosition class for use within the ATP for the project.

  \subsection GeneralFunctionality General Functionality
  The Position adaptation component shall instantiate the AbstractPosition class. It generates a singleton object
  and provides access to it.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  N/A

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design
  \subsection Initialization Initialization
   All the initialization activity is done in the Core part of the component.

  \subsection ModeDependentOperation Mode dependent operation
  N/A

  \subsection Scheduling Scheduling
  This component is executed every cycle using the core functionality. No adaptation of this core functionality is needed.

  \section ClassDiagram Class Diagram
  @image html position_class_diagram.png
  @image latex position_class_diagram.png

  \section Diagnostics Diagnostics

  \subsection Console Console Commands
  N/A

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation
  The adaptation implements the virtual function Position::getBaliseAirGap() and handles instantiation of the component.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component.

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP BHP.

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].

  Common architectural requirements are specified in SCDS ATP BHP.

  */
}

