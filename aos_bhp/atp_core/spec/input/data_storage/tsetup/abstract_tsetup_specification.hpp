namespace ATP::DS
{
  /**
  \if AsMainPage
  \mainpage Train Setup Component Specification
  @anchor ts
  \endif

  \ifnot AsMainPage
  \class TSetup
  \endif

  \section Purpose Purpose
  This document specifies the software design for the core part of the Train Setup component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality

  The AbstractTSetup class represents the core part of the Train Setup component and is responsible for
  - the storage of Train name, Train and Vehicle setup objects.
  - the storage of intermediate/preliminary train and vehicle-setup and Train Load status during train configuration.
  - the storage of Brakeability and related information for all brake models along with default values. It also updates brakeability depending upon train Load status.
  - the storage of Brake delay information for all brake types along with default values. It also updates brake delay depending upon brake model and number of brake cars available.
  - Handling of brake system in use.
  - Setting percentage of operative brakes.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  The TSetup component has dependencies to the following components:
  - Mode Control to get the current mode and train configuration mode state.
  - Targets to get the adhesion value.

  Other components have dependencies on this component because they use its public types and methods,
  see AbstractTSetup class reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  The init() function of AbstractTSetup
  - sets the variables in the storage to default values for AbstractTSetup::TrainSetupStorage and AbstractTSetup::TrainNameStorage.
  - registers the storage variables for cross compare.

  The constructor of AbstractTSetup
  - sets the default value for train load status, loaded and unloaded weight of the whole train.

  \subsection Scheduling Scheduling
  The component TSetup is scheduled by a call to the virtual run() function each cycle.

  \subsection ModeDependentOperation Mode dependent operation
  - TSetup will perform checks on data depending on the ATP mode.
    +   TrainSetup is updated in ATP modes such as Normal, Configuration, ShuntingRoute and StaffResponsible. 
        For this purpose access functions to set the Brake Data, Brake Response time and TIMS availability.
    +   Access to the trainPreliminarySetupStorage and vehiclePreliminarySetupStorage shall not be allowed after configuration is completed.
  - TSetup will force a default TrainSetup in Yard mode.

  Depending upon the current Mode: If the mode is not configuration, it will falsify all the change fields to denote no change in TSetup.
  Also, in the Configuration mode state config start, both Train setup and Preliminary Train Setup are invalidated. So that a new TSetup be accepted.
  In state Train Setup accepted, Preliminary TSetup is invalidated.

  \subsection StorageHandling Storage Handling
    The five storage classes owned by AbstractTSetup are:
  - trainNameStorage
  - trainSetupStorage
  - trainPreliminarySetupStorage
  - vehicleSetupStorage
  - vehiclePreliminarySetupStorage

    Each storage class has
  - a flag indicating that the data in the storage is valid and may be used.
  - a changed flag indicating that the train setup in the storage has changed.
  - an invalidate() function that will set the variables in the storage to default values and reset the flag indicating that the data in the storage is valid.

  The preliminary storage class are used during configuration/registration only.

  AbstractTsetup provides the access functions for acquiring brakeability and brake delay values. It also provides access functions that triggers populating/updating the values of lambda, brakeability, brake delay,
  brake system, load status and brake response time when appropriate conditions are satisfied.

  \subsubsection invalidateTrainSetup Invalidate Train Setup

  Data may not be read from a storage until it is made valid by e.g. a call to setTrainSetup.

  \subsubsection setTrainSetup Set Train Setup

  Train setup is updated when it is invalid OR is valid and any field is updated along with the condition that it is being called in Configuration, ShuntingRoute, StaffResponsible and Normal ATP modes.
  
  \subsubsection setTrainName Set Train Name

  This function is used to set the valid train name received in command message from TCC. The valid train name should not be more than 20 characters.
  When the train name is changed successfully it is displayed on DMI.

  \subsubsection getVehicleSetup Get vehicle setup

  This function returns the valid vehicle setup retrieved from the vehicle setup which is stored by train setup from each vehicle.
  When vehicle setup is valid and vehicle count is not more than 350 it is stored in vehicle setup storage else safety halt event is triggered for invalid value.

  \subsubsection setVehicleSetup Set Vehicle Setup

  This function is used to set the valid vehicle setup received in configuration data message from TCC.
  When valid vehicle setup is received in Configuration mode, train setup is confirmed by TCC and vehicle count is not more than 350 
  it is updated in vehicle setup storage else error is reported with safety halt event.

  \subsubsection getPreliminaryTrainSetup Get Preliminary Train Setup

  This function stores the valid preliminary train setup in train setup when the ATP mode is Configuration.

  \subsubsection setPreliminaryTrainSetup Set Preliminary Train Setup
  
  This function is used to set the valid Train Setup received from DMI or waiting for Train Setup.
  Train setup is updated when it is valid and the ATP mode is configuration.
  There will be safety halt if mode is configuration and vehicle count are out of the range.

  \subsubsection getPreliminaryVehicleSetup Get Preliminary Vehicle Setup
  The detailed description about preliminary vehicle setup can be seen in below flowchart.

  @image html abstract_tsetup_get_preliminary_vehicle_setup.png "AbstractTSetup::getPreliminaryVehicleSetup()"
  @image latex abstract_tsetup_get_preliminary_vehicle_setup.png "AbstractTSetup::getPreliminaryVehicleSetup()"

  \subsubsection setPreliminaryVehicleSetup Set Preliminary Vehicle Setup
  
  This function is used to set the valid preliminary vehicle setup received in configuration data message from TCC.
  When valid preliminary vehicle setup is received in Configuration mode, train setup is confirmed by TCC and vehicle count is not more than 350
  it is updated in vehicle setup storage else error is reported with safety halt event.

  \subsubsection removeTrainSetup Remove train setup

  This function removes all the train setup storage data and sets the train load status as default,
  invalidates the train setup storage, train preliminary setup storage, train name storage, vehicle preliminary setup storage and vehicle setup storage.

  \subsubsection getLocovsTrainOrientation Get Loco vs Train Orientation

  This function returns the locomotive orientation. If the train setup storage data is valid and A end is connected to the cars then return B end as front else A end as the front.

  \subsubsection setOrientationinPreliminaryTSetup Set Orientation in Preliminary TSetup

  If ATP Mode is 'ATPModeConfiguration' then check if train set up is available then
  passed orientation is stored in preliminary setup storage data.

  \subsection Brakeability Brakeability

  Brakeability is the functionality where brake handling is done. This is represented as a class and all the brake related data is stored in brakeability object.
  Another related and inherited classes :
  + BrakeabilityBrakeSystem - the current brake model/brake system in use functionalities are implemented in this class.
  + BrakeabilityBrakeSystemDefault - inherited from 'BrakeabilityBrakeSystem' class that stores default values of brakeability.
  + BrakeabilityBrakeSystemSteps - inherited from 'BrakeabilityBrakeSystem' class and implements core functionalities of brakeability.

  Core Functionalities:

  \subsubsection getBrakeability Get Brakeability

  Brakeability values are stored in steps of speed ranges. Each range has a set of configurable parameters used to calculate the brakeability using the formula:
  Brakeability = A*Lambda + B; where A and B are config parameters which are speed block dependent (speed blocks are again in turn configurable).

  While acquiring the brakeability or brakeability for a range, the current brake model/brake system in use is taken and then this is used to acquire the object to fetch the stored brakeability. Depending upon the
  current speed, the step index from the stored array of brakeability is picked and this value is returned. For acquiring brakeability in range if the speed range lies in more than one step, the most conservative speed
  value is returned. The adhesion value is used for calculation.
  When a valid train setup is not available, default value of Brakeability is used. This is a configurable parameter.

  \subsubsection updateBrakeabilitySteps Update Brakeability Steps

  Lambda values are calculated/updated when train setup message is received as the ratio of the summation of brake weight of all the locos and effective cars to the summation of all the dynamic weight
  of all the locos and cars. Every time there is a change in any of the factors affecting the lambda calculation, Lambda and brakeability is recalculated. Lambda calculations are also affected when
  train load status changes from MA received. Depending upon the Train load status, Lambda calculations are done via the values of either empty Brake and Dynamic weights OR loaded Brake and Dynamic weights.

  When the train setup message is received, brake data values (sum of all the brake and dynamic weights of Locomotives and Cars are updated) of all the brake models are updated. Using these values Lambda values are generated.
  If the lambda values are valid OR in range then each of the Brakeability object's all the step arrays are updated with the new values of Brakeability.

  \subsubsection updateBrakeDelay Update Brake Delay

  Brake delay values for both SB and EB are stored for each of the brake models separately.
  Each of the brake system has a set of config parameters used to calculate the brake delay using formula:
  Brake delay Td = t1 + (t2 * L) + ( (t3 * L * L) /1000 )  where t1, t2 and t3 are configuration parameters.

  For our calculations we use same values of T1, T2 and T3 in calculations of both SB and EB for a particular brake model. <br>

  Brake delay values are calculated/updated when train setup message is received. Every time there is a change in any of the brake related data brake delay is recalculated.
  When the train setup message is received, brake data values of all the brake models are updated. Depending upon the brake model, configuration parameters are fetched and corresponding brake delays for both EB and SB
  is calculated. This process is repeated and brake delay is stored for all the brake models.

  \subsubsection getServiceBrakeResponseTime Fetch the Brake Response Time

  While acquiring the brake delay, the current brake model/brake system in use is taken and then this is used to acquire the object to fetch the stored brake delay. Depending upon the type of brake (SB or EB), brake delay
  is picked and this value is returned.   
  The basic concept of acquiring and storing Brake delay is similar to Brakeability.

   When a valid train setup is not available, default value of Brake delay is used. This is a configurable parameter.

  \subsubsection calculateLambdaPercentage Calculate Lambda Percentage

  First, total brake weight is calculated.

  TotalBrakeWeight = "brake weight of the locomotive" + ("sum of the brake weight of all cars * minimum percentage of operative brakes")
  
  If dynamic weight of the locomotive is greater than zero then
  lambda = totalBrakeWeight / trainDynamicWeight.

  The minimum percentage of operative brakes value is set to 100 and it is configurable in adaptation.

  \subsubsection validateLambda Validate Lambda

  The detailed description Validate Lambda can be seen in below flowchart.

  @image html abstract_tsetup_validate_lambda.png "Validate Lambda"
  @image latex abstract_tsetup_validate_lambda.png "Validate Lambda"

  \subsubsection isValidLambda Is Valid Lambda

  After calculating lambda values for loaded and empty train, get minimum lambda value for corresponding brake type.
  Check if lambda values for loaded and empty train is lower than minimum configured value, then lambda is invalid else valid.

  \subsubsection isBrakeDataEqualOrBetter Is Brake Data Equal Or Better
  The detailed description Brake Data Equal Or Better can be seen in below flowchart.

  @image html abstract_tsetup_is_brake_data_equal_or_better.png "Brakeability::isBrakeDataEqualOrBetter()"
  @image latex abstract_tsetup_is_brake_data_equal_or_better.png "Brakeability::isBrakeDataEqualOrBetter()"

  \latexonly \newpage \endlatexonly
  \section ClassDiagram Class Diagram

  @image html abstract_tsetup.png "Abstract TSetup Class Diagram"
  @image latex abstract_tsetup.png "Abstract TSetup Class Diagram"

  @image html breakeability.png "Brakeability Class Diagram"
  @image latex breakeability.png "Brakeability Class Diagram"

  @image html brakesystem.png "Brake System Class Diagram"
  @image latex brakesystem.png "Brake System Class Diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console-commands
  The Console commands are defined in AbstractTSetup are:
  - TS : To print the train setup
  - PrelTS : To print the preliminary train setup

  \subsection Analyze Analyze
  N/A.

  \section CoreAdaptation Core / Adaptation

  The Train Setup component is split in to core and adaptation part.
  The AbstractTSetup class cannot be instantiated, it requires an adaptation class inheriting AbstractTSetup to allow an instance to be created.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component

  \section Traceability Traceability

  \subsection SSRS Functional Requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP Core.

  The requirements relevant for this component are:

  Req        | Chapter             |Function
  -----------| --------------------|--------------
  AOS 674 S  | \ref Brakeability  |BrakeabilityBrakeSystemSteps::updateBrakeabilitySteps()
  AOS 689    | -                  | AbstractTSetup::setTrainName()  
  AOS 1040   | \ref Console       |-
  AOS 1120 S | -                  |-
  AOS 2154   | \ref setTrainSetup | AbstractTSetup::isAnyTSetUpFieldChange()
  AOS 2155   | \ref setTrainSetup | AbstractTSetup::setTrainSetup()
  AOS 2156   | -                  | AbstractTSetup::removeTrainSetup()
  AOS 2607   | \ref Initialization | -
  AOS 2172   | \ref Brakeability, \ref updateBrakeDelay | BrakeabilityBrakeSystemSteps::getServiceBrakeResponseTime(), BrakeabilityBrakeSystemSteps::updateBrakeDelay()
  AOS 2724 S | \ref Brakeability  | BrakeabilityBrakeSystemSteps::getBrakeStep()
  AOS 2726   | \ref Brakeability | BrakeabilityBrakeSystemSteps::getBrakeStep()
  AOS 2728 S | \ref Brakeability | BrakeabilityBrakeSystemSteps::updateBrakeabilitySteps()
  AOS 2729   | \ref Brakeability | BrakeabilityBrakeSystemSteps::getBrakeStep()
  AOS 2730 S | \ref updateBrakeDelay | BrakeabilityBrakeSystemSteps::updateBrakeDelay()
  AOS 2731 S | \ref Brakeability | BrakeabilityBrakeSystemSteps::updateBrakeabilitySteps()
  AOS 2732 S | \ref Brakeability | BrakeabilityBrakeSystemSteps::updateBrakeabilitySteps()
  AOS 2733 S | \ref updateBrakeDelay | BrakeabilityBrakeSystemSteps::updateBrakeDelay()
  AOS 2735 S | \ref Brakeability | BrakeabilityBrakeSystemSteps::updateBrakeabilitySteps()
  AOS 3198 S | \ref Initialization | AbstractTSetup::initOnce()
  AOS 3199 S | \ref StorageHandling | AbstractTSetup::setTrainLoadStatus

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP Core.

  Only the architectural requirements traced explicitly to this component are included in the table below.
  Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

  */
}

