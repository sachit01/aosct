namespace ATP::Pos
{
  /**
  \if AsMainPage
  \mainpage Decode Component Specification
  @anchor dec
  \endif

  \ifnot AsMainPage
  \class Decode
  \endif

  \section Purpose Purpose
  This document specifies the software design for the BHP adaptation for the Decode component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality

  The Decode class is the BHP adaptation of the core AbstractDecode class. The main purpose
  is to instantiate the AbstractDecode class for use within the ATP. 
  The virtual function isRedBalise() is overridden to implement an adaptation specific requirement.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies

  Other components have dependencies to this component because they use its public types
  and methods, see \ref Decode Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  N/A

  \subsection ModeDependentOperation Mode dependent operation
  N/A

  \subsection Scheduling Scheduling
  The scheduling activity is handled by the core part of this component.

  \subsection RedBaliseDetec Red Balise detection
  If the NID_C is 1023 and NID_BG is 16383, it is a Red Balise. This is checked in the Decode::isRedBalise function.

  \section ClassDiagram Class Diagram
  @image html decode_class_diagram.png "Decode class diagram"
  @image latex decode_class_diagram.png "Decode class diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console-commands
  N/A

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation
  - All the major functionality of the Decode component is in the core part.
  - The adaptation of Decode component has a virtual method Decode::isRedBalise() that overrides the functionality to detect the red Balise.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component.


  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP BHP.

  The requirements relevant for this component are:

  Req           | Chapter                  | Function
  ------------- | ------------------------ | --------------
  AOS_BHPB 2686 | \ref RedBaliseDetec      | Decode::isRedBalise()

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP BHP.

  There are no architectural requirements traced explicitly to this component.
  Fulfillment of architectural requirements allocated to the ATP is described in [SWAS].


  */
}