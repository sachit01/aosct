namespace ATP::Kernel
{
  /**
  \if AsMainPage
  \mainpage ATP Application Component Specification
  @anchor app
  \endif

  \ifnot AsMainPage
  \class Template
  \endif

  \section Purpose Purpose
  This document specifies the software design for the core part of the ATP Application component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview
   
  \subsection GeneralFunctionality General Functionality

  The AbstractATPApplication class represents the core part of the ATP Application.
  This class is derived from the ATC class AbstractApplicationBase and is responsible for managing and scheduling all other components.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  The AbstractATPApplication class is dependent on the core interfaces designed for other core components using corePtr() method.

  Other components have dependencies to this component because they use its public types
  and methods, see AbstractATPApplication Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  The class does not require any initialization of internal structures.
  But there is a virtual AbstractATPApplication::init() to be implemented if required by the derived class.

  \subsection ModeDependentOperation Mode dependent operation

  The component is independent of any modes, it invokes all components based on the planned
  sequence independently of any mode.

  \subsection Scheduling Scheduling

  The class is scheduled by the Vital Framework (VFW) event timer. The derived class may override
  the run() method but then inherits the responsibility to provide the core components scheduling.

  \latexonly \newpage \endlatexonly
  \subsubsection SchedulingComponents Scheduling of components
  The scheduling of the components is done as per the below flowchart.
  @image html abstract_atp_application_run.png  "ATP APPLICATION RUN"
  @image latex abstract_atp_application_run.png "ATP APPLICATION RUN"

  \latexonly \newpage \endlatexonly
  \section ClassDiagram Class Diagram
   
  @image html abstract_atp_application.png "Abstract ATP Application Class Diagram"
  @image latex abstract_atp_application.png "Abstract ATP Application Class Diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console-commands
  N/A

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation
  The class cannot be instantiated, it requires an adaptation class inheriting
  AbstractATPApplication to allow instantiation.

  The pure virtual function below have to be implemented by the adaptation class
  - AbstractATPApplication::getVIOHClientHandle()
  - AbstractATPApplication::validateDispatcherVersion()

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component.

  \section Traceability Traceability

  \subsection SSRS Functional Requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP Core.

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP Core.

  */
}