namespace ATP::DS
{
  /**
  \if AsMainPage
  \mainpage TSetup Adaptation Component Specification
  @anchor ts
  \endif

  \ifnot AsMainPage
  \class TSetup
  \endif

  \section Purpose Purpose
  This document specifies the software design for the Adaptation part of the Train Setup component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality

  The TSetup class is the adaptation of the AbstractTSetup class which adds functionality needed for the BHP Project, e.g. project-specific Brakeability.
  The major functionalities in TSetup Class are:
  - Providing object for BrakeabilityBHP.
  - Handling of brake system in use by communicating with LCS.
  - Setting percentage of operative brakes information received from LCS.

  The TSetup class allocates the BHP Brakeability object which is used to implement BHP specific brake and brakeability related information.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies

  The TSetup component has dependencies to the following components:
  - Odometry to get the train standstill status.
  - Vehicle Com to get the percentage Of Operative Brakes from ECPB, Brake system in use and current ECPB operating mode.

  Other components have dependencies to this component because they use its public types
  and methods, see TSetup Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design
  \subsection Initialization Initialization

  The init() function TSetup
  - Creates array of objects for Vehicle Setup and Preliminary vehicle setup and then calls corresponding function to initialize the values to default values.
  - Calls BrakeabilityBhp::init() where Pneumatic brake step objects are created and if locotype is EMD then the ECPB brake step objects are created.
  - sets the variables in the storage to default values for AbstractTSetup::TrainSetupStorage .

  \subsection ModeDependentOperation Mode dependent operation
  N/A

  \subsection handleBrakeBrakeSystemInUse Handle Brake System In Use

  This functionality for EMD loco, shows how different brake handling events are performed when there is change in brake system in use and change in ECPB operating modes.

  - If the brake system in use in ECPB and reported brake system is Pneumatic then at standstill
     + AOS changes the current brake system in use to Pneumatic.
     + Otherwise if the reported and the current brake system in use are different then update the brake system type with the reported brake system.
  - If ECPB is the new brake system then set the valid percentage of operative brakes for the cars.
    + Check if calculated lambda is less then configured minimum lambda then raise standstill.
    + If the ECPB mode reported in train Status Message on the LCS interface is NOT Run mode then issue a standstill event.

  \subsection Scheduling Scheduling

  The component TSetup is scheduled by a call to the virtual run() function each cycle.
  The adaptation calls AbstractTSetup::run() for the core functionality. It also provides functionality to handle the current brake system in use.

  \subsection Brakeability Brake ability

  Brakeability functionality related to BHP/Adaptation is implemented in TSetup class.
  TSetup fetches the brake related information from Vehicle Com(LCS) component in each cycle and recalculates the brakeability,
  if changes occur in operating brakes percentage.

  \section ClassDiagram Class Diagram

  @image html tsetup.png "TSetup Class Diagram"
  @image latex tsetup.png "TSetup Class Diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console Commands
  N/A

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation
  All the major functionality for the TSetup is in the core part. 
  The adaptation of TSetup component has to implement the virtual function TSetup::initOnce() to initialize project specific parameters
  and TSetup::handleBrakeBrakeSystemInUse function for brake system in use handling.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component.

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP BHP.
  The requirements relevant for this component:

  Req             | Chapter         | Function
  --------------- | --------------- | --------
  AOS_BHPB 2884 S | \ref Brakeability    | BrakeabilityBhp::setPercentageOfOperativeBrakes
  AOS_BHPB 2885 S | \ref Brakeability    | BrakeabilityBhp::setPercentageOfOperativeBrakes
  AOS_BHPB 2886 S | \ref Brakeability    | BrakeabilityBhp::setPercentageOfOperativeBrakes
  AOS_BHPB 2949 S | \ref handleBrakeBrakeSystemInUse | TSetup::handleBrakeBrakeSystemInUse()
  AOS_BHPB 2782   | \ref handleBrakeBrakeSystemInUse | TSetup::handleBrakeBrakeSystemInUse()
  AOS_BHPB 2641 S | \ref handleBrakeBrakeSystemInUse | TSetup::handleBrakeBrakeSystemInUse()
  AOS_BHPB 5079 S | \ref Brakeability | BrakeabilityBrakeSystemSteps::updateBrakeDelay()
  AOS_BHPB 3397 | \ref Brakeability | BrakeabilityBrakeSystemSteps::updateBrakeabilitySteps()
  AOS_BHPB 5018 | \ref Brakeability | -
  AOS_BHPB 2695 | \ref Brakeability | -
  AOS_BHPB 2640 S | \ref handleBrakeBrakeSystemInUse | TSetup::handleBrakeBrakeSystemInUse()
  

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP BHP.

  Only the architectural requirements traced explicitly to this component are included in the table below.
  Fulfillment of other architectural requirements allocated to the ATP is described in [SWAS].

  */
}