namespace ATP
{
  /**
  \if AsMainPage
  \mainpage Basic IP Component Specification
  @anchor bip
  \endif

  \ifnot AsMainPage
  \class BasicIP
  \endif

  \section Purpose Purpose
  This document specifies the software design for the adaptation of the Basic IP component.

  \section Overview Overview

  \subsection GeneralFunctionality General Functionality

  The BasicIP class is the BHP adaptation of the BasicIP component. The main purpose
  is to instantiate the AbstractBasicIP class for use within the ATP and other applications in AOS.

  The Basic IP adaptation component inherits the AbstractBasicIP class. The adaptation creates a singleton object
  of the BasicIP class and provides access to it.
  The adaptation allocates memory for the Connection Control Blocks and provides the maximum number of connections possible.
  It defines all the Connection IDs and returns the maximum number connections possible. Also translates connection id to printable string needed by Abstract BasicIP.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies

  Basic IP does not have any dependencies with other components.
  Other components have dependencies to this component because they use its public types
  and methods, see \ref ATP::BasicIP Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design
  \subsection Initialization Initialization
  The virtual init() - function is not implemented in the adaptation.

  \subsection ModeDependentOperation Mode dependent operation
  The Basic IP adaptation component is independent of ATP mode.

  \subsection Scheduling Scheduling
  The virtual run() - function is not implemented in the adaptation.

  \section ClassDiagram Class Diagram

  @image html basic_ip_bhp_class_diagram.png "Basic IP Class Diagram"
  @image latex basic_ip_bhp_class_diagram.png "Basic IP Class Diagram

  \section Diagnostics Diagnostics

  \subsection Console Console-commands
  No component specific console commands are implemented.

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation 
  The core part of the Basic IP component simplifies and standardizes the usage of IP-connections in AOS. 

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP BHP.

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP BHP
  */
}