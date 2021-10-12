namespace ATP::RadioCom
{

  /**
  \if AsMainPage
  \mainpage Radio Handler Component Specification(BHP Adaptation)
  @anchor rh
  \endif

  \ifnot AsMainPage
  \class RadioHandler
  \endif

  \section Purpose Purpose
  This document specifies the software design for BHP Adaptation of the RadioHandler component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview
  The RadioHandler class is the BHP adaptation of the core AbstractRadioHandler class. The main purpose at this time is to
  instantiate the AbstractRadioHandler class for use within the ATP. RadioHandler creates one instance of CentralRadioChannel and two instances of RadioChannel.

  \subsection GeneralFunctionality General Functionality
  The RadioHandler component instantiates the AbstractRadioHandler class. It generates a singleton object
  and provides access to it.

  \subsection Dependencies Dependencies
  The RadioHandler depends on RadioChannel component and it creates one instance of CentralRadioChannel and two instances of RadioChannel.\n\n

   Other components depend on this component's functionality by using its public functions.
   Refer to Public Member Functions under \ref RadioHandler.\n

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design
  \subsection Initialization Initialization
  The component initializes as follows:
  - Creates one RadioChannel instances.
  - RadioHandler::preInit() calls all RadioChannels preInit functions.
  - RadioHandler::init() initializes as follows:
   + calls init functions of all the RadioChannels.
   + Stores the channel instances in a vector.
   + Adds the channel instances to the vector of components maintained by ATP. One reason is to enable the use of console commands.
  - Adds values to be cross-compared.


  \subsubsection ModeDependentOperation Mode dependent operation
  RadioHandler component is independent of ATP mode.

  \subsubsection Scheduling Scheduling
  The virtual run() function is not implemented in the adaptation.

  \section ClassDiagram Class Diagram

  @image html radio_handler_class_diagram.png "Class diagram"
  @image latex radio_handler_class_diagram.png "Class diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console Commands
  N/A.

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation
  The adaptation class RadioHandler inherits from the core class AbstractRadioHandler and adds the following functionality:
  - Instantiates of RadioChannel.
  - Initializes the component.

  \section PreProcessor Pre-Processor Directives
  No component-specific pre-processor directives are used in this component.

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP BHP.

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in the SCDS ATP BHP.


  */

}
