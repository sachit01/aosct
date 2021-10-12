namespace ATP
{

/** 
\if AsMainPage
\mainpage Config Component Specification
@anchor cfg
\endif

\ifnot AsMainPage
\class Config
\endif

\section Purpose Purpose
This document specifies the software design for the Config class, the BHP adaptation part of the Config component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
Config is the BHP adaptation of the core config component, AbstractConfig. The purpose
of Config is to instantiate the component and once initialized, provide access to the
adaptation config parameters for the components that need them.

Every parameter has its own getter function that retrieves the parameter's value.

A component that uses config parameters should not itself store parameter values but rather
call the getter(s) here whenever a value is needed.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
The Config component only depends on its superclass, AbstractConfig.

Other components have dependencies to this component because they use its public types
and methods, see \ref Config Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization

\subsubsection preInit preInit()
The preInit() function creates an ATC::ConfigFile object for each parameter file. These parameter files are:
- rt_data.bin, for parameters updated by ATP at run-time
- mnt_data.bin, for parameters updated during maintenance
- instance_data.bin, for parameters specific to the vehicle
- type_data.bin, for parameters specific to the vehicle type
- cfg_data.bin, for parameters common to all instances and vehicle types

\subsubsection init init()
The init() function adds and initializes the BHP specific configuration parameters.

The init() function sets the global config names for the BHP parameters that can be changed by TCC (see [FFFIS]).

\subsection ModeDependentOperation Mode Dependent Operation
Config is not mode dependent.

\subsection Scheduling Scheduling
This component is fully scheduled by its base class ATC::AbstractConfigBase so no adaptation specific
implementation is added.

\section ClassDiagram Class Diagram
@image html config.png "Class diagram"
@image latex config.png "Class diagram"

\section Diagnostics Diagnostics
\subsection Console Console Commands
N/A

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
The adaptation class Config inherits from the core class AbstractConfig and adds the following functionality:

- Sets up the configuration files to use
- Sets up and provides access to the adaptation specific config parameters
- Instantiates the component

\section PreProcessor Pre-Processor Directives
No component-specific pre-processor directives are used in this component.

\section Traceability Traceability

\subsection SSRS Functional Requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP BHP.

The requirements relevant for this component are:

Req             | Chapter                                 | Function
----------------| --------------------------------------- | --------
AOS 1737 S      | \ref Initialization                     | Config::init()
AOS_BHPB 2681 S | \ref Initialization                     | Config::init() <br /> Config::getEbCutOutConfigured() <br /> Config::getRailRoadInputAvail()
AOS_BHPB 2765 S | \ref Initialization                     | Config::init() <br /> Config::getEbFbSignalStatus()
AOS_BHPB 2769 S | \ref Initialization                     | Config::init() <br /> Config::getMaxEbApplyFeedback() <br /> Config::getMinEbReleaseFeedback()
AOS_BHPB 2771 S | \ref Initialization                     | Config::init() <br /> Config::getRangeMinBP1() <br /> Config::getRangeMaxBP1() <br /> Config::getRangeMinBP2() <br /> Config::getRangeMaxBP2() <br /> Config::getEbFbInaccuracy()
AOS_BHPB 2772 S | \ref Initialization                     | Config::init() <br /> Config::getMaxEbFbDiff()
AOS_BHPB 2773 S | \ref Initialization                     | Config::init() <br /> Config::getMaxEbFbDiffTimeout()
AOS_BHPB 2829 S | \ref Initialization                     | Config::init() <br /> Config::getNonLeadingLocoInput()
AOS_BHPB 2847 S | \ref Initialization                     | Config::init() <br /> Config::getUseEbTcoFbDuringBrakeTest()
AOS_BHPB 2858   | \ref Initialization                     | Config::init() <br /> Config::getTimsMinBPLoco()
AOS_BHPB 2859   | \ref Initialization                     | Config::init() <br /> Config::getTimsMinBPLastCar()
AOS_BHPB 2956 S | \ref Initialization                     | Config::init() <br /> Config::getEbFeedbackTimeout()
AOS_BHPB 2958 S | \ref Initialization                     | Config::init() <br /> Config::getTcoOrderAndTCOFb()
AOS_BHPB 2959 S | \ref Initialization                     | Config::init() <br /> Config::getTcoFeedbackTimeout()
AOS_BHPB 3137   | \ref Initialization                     | Config::init() <br /> Config::getBPDeltaRapidLoss()
AOS_BHPB 3192   | \ref Initialization                     | Config::init() <br /> Config::getECPBCarsInBrakeCalc()
AOS_BHPB 3193   | \ref Initialization                     | Config::init() <br /> Config::getECPBMinPercBrakeCars()
AOS_BHPB 3202   | \ref Initialization                     | Config::init() <br /> Config::getBPTimeRapidLoss()
AOS_BHPB 3321   | \ref Initialization                     | Config::init() <br /> Config::getSbPrPropagationSpeed()
AOS_BHPB 3340   | \ref Initialization                     | Config::init() <br /> Config::getTimsEcbpTimeOut()
AOS_BHPB 3341   | \ref Initialization                     | Config::init() <br /> Config::getTimsEotTimeOut()
AOS_BHPB 3345 S | \ref Initialization                     | Config::init() <br /> Config::getTimsLengthMargin()
AOS_BHPB 3358   | \ref Initialization                     | Config::init() <br /> Config::getTypicalConfig()
AOS_BHPB 3421   | \ref Initialization                     | Config::init() <br /> Config::getMinApproachSpeed()
AOS_BHPB 3435   | \ref Initialization                     | Config::init() <br /> Config::getMaxExtReversingDistance()
AOS_BHPB 3446   | \ref Initialization                     | Config::init() <br /> Config::getMaxExtReversingSpeed()
AOS_BHPB 5014   | \ref Initialization                     | Config::init() <br /> Config::getOBRDBrakeTestPressure()
AOS_BHPB 5022   | \ref Initialization                     | Config::init() <br /> Config::getDeleteTrackMargin()

\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP BHP.

*/

}
