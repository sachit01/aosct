namespace ATP::DS
{

  /**
  \if AsMainPage
  \mainpage Tracks Component Specification (BHP adaptation)
  @anchor trk
  \endif

  \ifnot AsMainPage
  \class Tracks
  \endif

  \section Purpose Purpose
  This document specifies the software design for the BHP adaptation of the Tracks component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality
  The Tracks adaptation class shall instantiate the Tracks component. It generates a singleton object and provides access to it.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  The Tracks class is independent to other components.

  Other components have dependencies to this component for the public types and methods,
  see \ref Tracks Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  The initialization of the adaptation part of the Tracks component is done in the core part of the component.

  \subsection ModeDependentOperation Mode dependent operation
  Tracks class is mode independent.

  \subsection Scheduling Scheduling
  The adaptation part of the Tracks component does not have any scheduled function. 
 
  \section ClassDiagram Class Diagram
  @image html tracks_class_diagram.png "Class diagram"
  @image latex tracks_class_diagram.png "Class diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console commands
  There are no console commands defined for the adaptation part of the Tracks component.

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation
  The Adaptation of the component only creates an instance of the class. All the major functionality is implemented in the 
  core part of the component.


  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].
  Common functional requirements are described in SCDS ATP BHP.

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].
  Common functional requirements are described in SCDS ATP BHP.

  Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].


  */
}

