namespace ATP::TG
{
  /**
  \if AsMainPage
  \mainpage Vehicle Com Component Specification (Core)
  @anchor vc
  \endif

  \ifnot AsMainPage
  \class AbstractVehicleCom
  \endif

  \section Purpose Purpose
  \latexonly \newpage \endlatexonly
  This document specifies the software design for the core part of the Vehicle Com component.

  \section Overview Overview
  \subsection GeneralFunctionality General Functionality
  The Vehicle Com component is responsible for the communication between the AOS and Locomotive Control System (LCS)
  through a communication protocol EMP (Edge Message Protocol). 


  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  The AbstractVehicleCom has dependencies with following component:-
  - Cross Compare:- For cross comparing the data.

  Other components have dependencies to this component because they use its public types
  and functions, see \ref AbstractVehicleCom Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design
  \subsection Initialization Initialization
  - Initialize the variables for cross compare.

  \subsection  ModeDependentOperation Mode dependent operation
  Vehicle Com component is independent of ATP mode.

  \subsection Scheduling Scheduling
  The component is fully scheduled inside the Adaptation part.
  However, AbstractVehicleCom class provides the interface to get connection status towards vehicle
  and start up and health supervision test for vehicle. These interfaces will be implemented in Adaptation part of Vehicle Com component. 

  \section ClassDiagram Class Diagram

  The class-diagram below shows the AbstractVehicleCom class, which consists of the core API to be implemented in the Adaptation.
  @image html vcom_abstract_vehiclecom.png
  @image latex vcom_abstract_vehiclecom.png

  \section Diagnostics Diagnostics
  \subsection ConsoleCommands Console commands
  N/A

  \subsection Analyze Analyze
  No values are registered for analysis for Vehicle Com component.

  \section CoreAdaptation Core / Adaptation
  Core part of Vehicle Com component provides pointer to single instance of AbstractVehicleCom class and all the functionalities of Vehicle Com component is implemented in Adaptation part.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS Core.

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP Core. Only the architectural requirements traced explicitly to this component are included in the table below.
  Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

  Req             | Chapter                   | Function
  --------------- | ------------------------- | --------
  AOS_AS-47 S     | \ref Initialization       | AbstractVehicleCom::init()
  AOS_AS-50 S     | \ref Initialization       | AbstractVehicleCom::init()
  AOS_AS-70 S     | \ref Initialization       | AbstractVehicleCom::init()
  AOS_AS-71 S     | \ref Initialization       | AbstractVehicleCom::init()
  AOS_AS-72 S     | \ref Initialization       | AbstractVehicleCom::init()
  AOS_AS-88 S     | \ref Initialization       | AbstractVehicleCom::init()
  AOS_AS-89 S     | \ref Initialization       | AbstractVehicleCom::init()

  */
}

