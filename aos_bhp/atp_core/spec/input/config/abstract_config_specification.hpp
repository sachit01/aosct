namespace ATP
{

/** 
\if AsMainPage
\mainpage AbstractConfig Component Specification
@anchor cfg
\endif

\ifnot AsMainPage
\class AbstractConfig
\endif

\section Purpose Purpose
This document specifies the software design for the AbstractConfig class, the core part of the Config component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
AbstractConfig is the ATP Core subclass of ATC::AbstractConfigBase. Its main purpose
is to configure ATP Core config items during initialization and thereafter provide access
to these config items for the components that need them.

Every parameter has its own getter function that retrieves the parameter's value. A few
parameters also have a setter function that makes it possible to save a value persistently.

A component that uses config parameters must not itself store parameter values but rather
call the getter(s) here whenever a value is needed.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
AbstractConfig only depends on its superclass, ATC::AbstractConfigBase.

Other components have dependencies to this component because they use its public types
and methods, see \ref AbstractConfig Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization

\subsubsection init init()
The init() function adds and initializes the core configuration parameters.

The init() function sets the global config names for the parameters that can be
changed by TCC, see [FFFIS TCC-AOS-BHP].

\subsection ModeDependentOperation Mode Dependent Operation
AbstractConfig is not mode dependent.

\subsection Scheduling Scheduling
This component is fully scheduled by its base class ATC::AbstractConfigBase so no core
implementation is added.

\subsection CrossCompare Cross Comparison
AbstractConfig adds all instances of ATC::AbstractConfigBase, ATC::ConfigFile and ATC::BaseConfigItem
(see Config ATC SCDS) for cross comparison. This is necessary because ATP requires cross comparison but
cross comparison doesn't exist in ATC.

\section ClassDiagram Class Diagram
@image html abstract_config_class.png "Class diagram"
@image latex abstract_config_class.png "Class diagram"

\section Diagnostics Diagnostics
\subsection Console Console Commands
N/A

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
The core class AbstractConfig inherits from the ATC class AbstractConfigBase and adds the following functionality:

- Sets up and provides access to the core config parameters

\section PreProcessor Pre-Processor Directives
No component-specific pre-processor directives are used in this component.

\section Traceability Traceability

\subsection SSRS Functional Requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP Core.

The requirements relevant for this component are:

Req        | Chapter                                 | Function
---------- | --------------------------------------- | --------
AOS 186    | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBalAntennaPosEnd() <br /> AbstractConfig::getBalAntennaPosFront()
AOS 663    | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getYardSpeed()
AOS 666    | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getMaxTimeAOSRun()
AOS 667    | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBalAntennaPosFront() <br /> AbstractConfig::getBalAntennaPosEnd() <br /> AbstractConfig::getCODSensorConfiguration() <br /> AbstractConfig::getTractionControlValue() <br /> AbstractConfig::getTacho1Direction() <br /> AbstractConfig::getTacho2Direction() <br /> AbstractConfig::getMinDopplerSpeed() <br /> AbstractConfig::getMaxDopplerSpeed() <br /> AbstractConfig::getMaxDopplerAccel()
AOS 669    | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBalWindowMin() <br /> AbstractConfig::getBalWindowPermil()
AOS 675    | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getFirstToSecondWarnCurveDelay() <br /> AbstractConfig::getSecondToSBCurveDelay() <br /> AbstractConfig::getDefaultBrakeDelay() <br /> AbstractConfig::getSbToEbDelay() <br /> AbstractConfig::getEffectiveEbDelay() <br /> AbstractConfig::getTractionCutoffDelay()
AOS 682    | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getRollAwayMargin()
AOS 683    | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getRevSupMargin() <br /> AbstractConfig::getRevSupMargForShuntRoute() <br /> AbstractConfig::getRevSupMargForFreeRolling()
AOS 758    | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getRadioTimeOut() <br /> AbstractConfig::getRadioTimeOutSb() <br /> AbstractConfig::getRadioTimeOutYardMode()
AOS 1671   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getCabinConfiguration()
AOS 1737 S | \ref Initialization                     | AbstractConfig::init()
AOS 1742   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getTrainCfgTicTimeout()
AOS 1821   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBalSearchDistance()
AOS 1944   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getSecondBaliseSearchDistance()
AOS 1960   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBalSearchSpeed()
AOS 2121   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBalSearchSpeed()
AOS 2178   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getDefaultBrakeability() <br /> AbstractConfig::getDefaultBrakeDelay()
AOS 2186   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getSiteId() <br /> AbstractConfig::getRadioId()
AOS 2260   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getVehicleType1() <br /> AbstractConfig::getVehicleType2() <br /> : <br /> AbstractConfig::getVehicleType20()
AOS 2282   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getMinSpeedMarginWarn() <br /> AbstractConfig::getMinSpeedMarginSB() <br /> AbstractConfig::getMinSpeedMarginEB() <br /> AbstractConfig::getMaxSpeedMarginWarn() <br /> AbstractConfig::getMaxSpeedMarginSB() <br /> AbstractConfig::getMaxSpeedMarginEB() <br /> AbstractConfig::getSpeedMarginWarn() <br /> AbstractConfig::getSpeedMarginSB() <br /> AbstractConfig::getSpeedMarginEB()
AOS 2301   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getSbExpectedDecLimit() <br /> AbstractConfig::getEbExpectedDecLimit()
AOS 2605   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getLocoType()
AOS 2615 S | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getLastBrakeTestTime()
AOS 2623   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBrakeTestMandatoryTime()
AOS 2626   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getLastBrakeTestTime()
AOS 2628   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBrakeTestNotifyTime()
AOS 2645   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBrakeTestExecTime()
AOS 2690   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getAtoEnable()
AOS 2694   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getESAInputAvail()
AOS 2705   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getTimsSensorTimeDiff()
AOS 2706   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getTimsManualConfTime()
AOS 2714   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getSbAvailable()
AOS 2738 S | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBrakeParameter()
AOS 2739 S | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBrakeParameter()
AOS 2740 S | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBrakeLambdaMax()
AOS 2741 S | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBrakeLambdaMin()
AOS 2755   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getEbPrPropagationSpeed()
AOS 2821   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBTMTestNotify()
AOS 2827   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBTMTestMandatory()
AOS 2915 S | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getBrakeParameter()
AOS 2956   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getEventKeepActiveTime() <br /> AbstractConfig::getStandstillEventKeepActiveTime()
AOS 3170   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getEBMarginAdded()
AOS 3195   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getReleaseSpeed()
AOS 3227   | \ref Initialization                     | AbstractConfig::init() <br /> AbstractConfig::getDeleteTrackMargin()

\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP Core.

Only the architectural requirements traced explicitly to this component are included in the table below.
Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

Req          | Chapter                               | Function
------------ | ------------------------------------- | --------
AOS_AS 653 S | \ref Initialization                   | AbstractConfig::getTachoPulsesPer10Revolutions1() <br /> AbstractConfig::getTachoPulsesPer10Revolutions2() <br /> AbstractConfig::getDopplerPulsesPerKm() <br /> AbstractConfig::getDopplerPulsePrecision()
AOS_AS 775 S | \ref Initialization                   | AbstractConfig::init() <br /> AbstractConfig::getVNomMargin()

*/

}
