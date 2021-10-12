namespace ATC
{
  /**
  \if AsMainPage
  \mainpage Application Component Specification
  @anchor app
  \endif

  \ifnot AsMainPage
  \class Template
  \endif

  \section Purpose Purpose
  This document specifies the software design for the AbstractApplicationBase Class.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality

  The Abstract Application Base component is responsible for the basic functionality which includes
  creating and maintaining the list of components created by the ATP and other applications in AOS.
  The component list is used to provide interface for other components to access registered
  components for actions such as trace and console commands.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies

  Other components have dependencies to this component because they use its public types
  and methods, see AbstractApplicationBase Class Reference.

  \latexonly \newpage \endlatexonly

  \section FunctionalDesign Functional Design
  \subsection BehaviouralDescription Behavioural description
  \subsubsection Initialization Initialization

  The AbstractApplicationBase class is initialized in its constructor.
  Inherited classes are required to register any created component using the provided interfaces.
  All the components are registered through AbstractApplicationBase::addComponent() interface, which stores a pointer to the component in the maintained component list.

  \subsubsection ModeDependentOperation Mode dependent operation
  This component is independent of any modes.

  \subsubsection Scheduling Scheduling

  AbstractApplicationBase does not perform any scheduled operations.

  \subsection ComponentListHandling Component List Handling

  The component list is populated by the adaptation component upon instantiation.
  The list can be accessed by external components using the available iterator methods below:
  + AbstractApplicationBase::getCompIterator()
  + AbstractApplicationBase::getCompIteratorEnd()

  \section ClassDiagram Class Diagram

  @image html abstract_application_base.png "Abstract Application Base Class Diagram"
  @image latex abstract_application_base.png "Abstract Application Base Class Diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console-commands
   N/A

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation
  The class cannot be instantiated, it requires an adaptation class inheriting
  AbstractApplicationBase to allow instantiation.
  The pure virtual functions below must be implemented by the adaptation class: 
  - AbstractApplicationBase::getApplicationName()
  - AbstractApplicationBase::getApplicationVersionString()

  \section PreProcessor Pre-Processor Directives
  N/A

  \section Traceability Traceability

  \subsection SSRS Functional Requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATC.

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATC.

  */
}

