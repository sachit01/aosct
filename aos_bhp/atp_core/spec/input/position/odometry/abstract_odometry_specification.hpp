namespace ATP::Pos::AbstractOdometry
{

  /**
  \if AsMainPage
  \mainpage Odometry Component Specification
  @anchor odo
  \endif

  \ifnot AsMainPage
  \class AbstractOdometry
  \endif

  \section Purpose Purpose
  This document specifies the software design of the AbstractOdometry class, the core part of the Odometry component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview
  \subsection GeneralFunctionality General Functionality
  The Odometry component is an interface between the COD and the AOS software. 
  COD measures speed and distance and provides that information to AOS software.
  Refer to [IFS_COD] for details about the interface between Odometry and COD.
  The Odometry component must configure the COD before it can start reading data from COD.
  The raw data read from COD is converted to AOS reference system and accessible by other components via getter function.
  
  In addition to being the interface towards COD it also implements important functionality such as 
  calculating the balise window, stand-still condition and checking the slip and slide status from COD. 
  It also monitors tachometer errors and doppler errors and creates log events when such errors are detected.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  AbstractOdometry depends on the following components:
  
  + \a Position provides current odometer offset correction value, expected balise information and current position accuracy state.
  + \a Targets provides information such as supposed travel direction, current gradient, free rolling status and safety margin.
  + \a Mode Control indicates current driving direction.

  Other components have dependencies to this component because they use its public types
  and methods, see \ref AbstractOdometry Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection preInitialization Pre-initialization
  Pre-initialization is done in the function preInit(). It creates three VFW channels towards COD as follows:
  - Channel to send configuration data.
  - Channel to receive configuration response from COD. 
  - Channel to receive measurement data.

  \subsection initialization Initialization
  The function init() handles the remaining initialization of the component. 
  It is implemented within a state machine in which it performs the static and dynamic configuration of COD. 
  See \ref odometerStates for details about the state machine's states and their respective operations.
  After a successful initialization, persistent variables are registered for cross compare and selected 
  measurement variables are registered to Analyzer IF.
  
  \subsubsection odometerStates Odometer Configuration States
  The communication with COD can have delays which is why the initialization is implemented as a state machine 
  with certain states specific for waiting for responses.
  Below is the list of states in the same order as executed.
  - \a SendStaticConfig: See \ref sendStaticConfig.
  - \a WaitForStaticConfigResponse: Waits for the response for the static configuration and validates it.
  - \a SendDynamicConfig: See \ref sendDynamicConfig.
  - \a WaitForDynamicConfigResponse: Waits for the response for the dynamic configuration and validates it.
  - \a ConfigComplete: COD configuration is done. Register variables for cross compare and to Analyzer IF.
  - \a ConfigFailed: Handles if validation of a configuration response fails.

  \subsection ModeDependentOperation Mode Dependent Operation
  The Odometry component is not mode dependent.

  \subsection scheduling Scheduling
  The core component has run() function, that must be called each execution cycle.

  \subsubsection run run
  The run() executes all functions related to below listed information that is maintained by this component. 
  It also updates the dynamic configuration including checking that the response is received within 
  1 second after it is sent and the validity of response message. If the response message is not received
  within the expected time a safety halt event it trigged.
  + \ref monitorSensorsAndFilters
  + \ref directionMultiplier
  + Odometer values
  + \ref baliseWindow
  + \ref processSlipSlideCheck
  + \ref StandStillCheck

  @image html abstract_odometry_run.png "run() flow chart"
  @image latex abstract_odometry_run.png "run() flow chart"

  \subsection configCOD Configuration of COD
  For the AOS software to be able to retrieve necessary data from COD it has to configure COD first which is done 
  in two parts in the initialization phase. The configuration is split into two parts, the static configuration and the 
  dynamic configuration. The two types have different telegrams, thus different content. The static configuration 
  telegram is sent once during initialization while the dynamic configuration telegram is sent once during initialization 
  but also while running when configuration parameters are updated. 
  See [IFS_COD] for detailed information.

  \subsubsection sendStaticConfig Static Configuration
  The static configuration consists of parameters that are not expected to change during the course of one power cycle, 
  such as parameters related the two tachometers and Doppler radar related parameters. 
  The function sendStaticConfig() implements the writing of the static configuration data telegram according to [IFS_COD] 
  to COD on the configuration channel.
  Most configuration parameters are provided by the Config component.

  \subsubsection sendDynamicConfig Dynamic Configuration
  Dynamic configuration, as opposed to static configuration, consists of parameters that may change during runtime. 
  Such parameters may be maximum expected acceleration and deceleration, gradient information, wheel size and wheel
  size errors etc. 
  The function sendDynamicConfig() implements the writing of the dynamic configuration data telegram according to [IFS_COD]. 
  Wheel sizes are calculated according to the following formula if any of the two is less than the allowed limit by COD.
  \f[wheelSize = \frac{(minAcceptableWheelSizebyCOD * wheelSize)}{minWheelSize}\f] 
  minWheelSize being the size of the wheel that is the smallest.
  
  \subsection monitorSensorsAndFilters Monitoring safety critical variables and sensors
  The Odometry component is responsible for implementing monitors for safety critical variables related to readings
  from COD. The variables are the maximum and the minimum speed reported by COD. For these a filter is implemented and
  the filtered value is monitored. If for longer then 20 seconds, the filtered value is higher than the nominal value + a
  configurable error margin, AOS issues a service brake event and informs the driver about the speed sensor failure
  under the duration of the error.

  \subsection baliseWindow Balise Window
  The balise window is the confidence interval since the last balise passage. It indicates with what confidence AOS has 
  calculated the position of the train. The larger the balise window the more uncertain the actual position of the train 
  compared to the calculated position. 
  The balise window is reset each time the train passes an expected balise and it is never less than what is the minimum 
  limit given by the Config component. Config also provides a safety margin to limit how large the balise window may 
  become before appropriate measures are taken. If the safety margin is exceeded the Odometry component will raise a
  safe brake to stop event. 

  The Odometry component manages two balise windows being the \a last \a car balise window and the \a locomotive \a end 
  balise window. How they are calculated depends on the value of the \ref directionMultiplier or if the train is in free rolling.
  In the normal case, the balise window depends on two values based on max, min and nominal distance values given by COD. See below:
  \f[upperRange = (accumulated D_{MAX} - accumulated D_{NOM})\f]
  \f[lowerRange = (accumulated D_{NOM} - accumulated D_{MIN})\f]
  The accumulated distance values mentioned above are like the two balise windows, reset when the train passes a balise.
  The principle is that the \a upperRange value is used to tell the balise window on the front end of the train in
  the driving direction and the \a lowerRange value indicates the balise window on the rear end of the train in the 
  driving direction.

  Otherwise when the train is in free rolling, the balise window is calculated based on a percentage of how much D_NOM has 
  increased since the last balise passage.
  
  \subsection StandStillCheck Standstill monitoring
  The Odometry component maintains a status flag that indicated if the train is in stand-still or not. The vehicle is 
  considered to be in stand-still if the vehicle speed is 0 for at least 10 cycles and considered to be moved out of 
  stand-still if the speed greater than 0 for 5 cycles.

  \subsection processSlipSlideCheck Slip and Slide Detection
  The following flowchart details the processSlipSlideCheck() function which check is there has been any slipping or sliding.
  @image html abstract_odometry_processslipslidecheck.png "Slip and Slide Detection"
  @image latex abstract_odometry_processslipslidecheck.png "Slip and Slide Detection"

  \subsection directionMultiplier Direction Multiplier
  The Direction Multiplier is a variable that is given values -1, 0  and 1 depending on the direction
  given by the train setup.
  It is implemented as a means to do decide the odometer based direction and to calculate the traveled 
  distance based on the delta between the raw odometer readings. E.g. depending on if it is positive or 
  negative it will have the effect of increasing or decreasing the traveled distance.
  This is done in the function processMeasurementData().

  \section ClassDiagram Class Diagram

  @image html abstract_odometry_class_diagram.png "Class diagram"
  @image latex abstract_odometry_class_diagram.png "Class diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console Commands
  The following component-specific console commands are implemented:
  - chstat   : Print list of all channels describing each channel name, type, number of messages sent/received and number of bytes read/written
  - odoVal   : Print the calculated odometry values.
  - odoRaw   : Print the last raw values used for odometry calculation, same values as 'trace odoRaw' would print.
  - odoStat  : Print the odometry status values

  \subsection Analyze Analyze
  Following variables are added at initialization for analysis. These variables are to be accessed by an external analyzer tool.

  Variable             | Unit   | Description
  ---------------------|--------|------------
  resCurSpeedAIF       | cm/s   | current vehicle speed
  resCurAccAIF         | cm/s   | current vehicle acceleration
  resCurOdoAIF         | cm/s^2 | current odometer value
  resFrontBwRaw        | cm     | raw positive balise window
  resRearBwRaw         | cm     | raw negative balise window
  resFrontBw           | cm     | positive balise window
  resRearBw            | cm     | negative balise window
  resSlipAIF           | byte   | slip
  resSlideAIF          | byte   | slide
  resdNom              | cm     | nominal distance
  resdMin              | cm     | minimum distance
  resdMax              | cm     | maximum distance
  travDistNom          | cm     | traveled distance since last passed balise in cm (based on dNom)
  travDistMax          | cm     | traveled distance since last passed balise in cm (based on dMax)
  travDistMin          | cm     | traveled distance since last passed balise in cm (based on dMin)
  resvMax              | cm/s   | maximum speed
  resvMin              | cm/s   | minimum speed
  resRawdDoppler       | cm/s   | dDoppler calculations
  resRawdTacho1        | cm     | dTacho1 calculations
  resRawdTacho2        | cm     | dTacho2 calculations
  resRawvDoppler       | cm/s   | vDoppler calculations
  resRawvTacho1        | cm     | vTacho1 calculations
  resRawvTacho2        | cm     | vTacho2 calculations
  resRawSlipSide1      | byte   | SlipSlideStatus for Slip
  resRawSlipSide2      | byte   | SlipSlideStatus for Slide

  \section CoreAdaptation Core / Adaptation
  The Odometry component is split in core and adaptation part. The adaptation is required for instantiation of the component.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives are available for this component.

  \section Traceability Traceability

  \subsection SSRS Functional Requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP Core.

  The requirements relevant for this component are:

  Req        | Chapter                    | Function
  ---------- | -------------------------- | --------
  AOS 172    | \ref processSlipSlideCheck | processSlipSlideCheck()
  AOS 183 S  | \ref directionMultiplier   | processMeasurementData()
  AOS 185 S  | \ref directionMultiplier   | processMeasurementData()
  AOS 187    | \ref processSlipSlideCheck | processSlipSlideCheck()
  AOS 190    | \ref baliseWindow          | updateBaliseWindow()
  AOS 191    | \ref run                   | readMeasurementTelegrams()
  AOS 212    | \ref baliseWindow          | updateBaliseWindow()
  AOS 213    | \ref baliseWindow          | updateBaliseWindow()
  AOS 218 S  | \ref baliseWindow          | updateBaliseWindow()
  AOS 656    | \ref initialization        | startupAndHealthSupTest()
  AOS 1039   | \ref Console               | consoleCall()
  AOS 2094   | \ref baliseWindow          | updateBaliseWindow()
  AOS 2104   | \ref processSlipSlideCheck | processSlipSlideCheck()
  AOS 2105   | \ref processSlipSlideCheck | processSlipSlideCheck()
  AOS 2625 S | \ref initialization        | init()
  AOS 3301   | \ref baliseWindow          | updateBaliseWindow()

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP Core.

  Only the architectural requirements traced explicitly to this component are included in the table below.

  Fulfillment of other architectural requirements allocated to the ATP is described in [SWAS].

  Req          | Chapter                | Function
  ------------ | ---------------------- | --------
  AOS_AS-47 S  | \ref run               | readMeasurementTelegrams()
  AOS_AS-59 S  | \ref initialization    | validateSdpVersion()
  AOS_AS-70 S  | \ref preInitialization | preInit()
  AOS_AS-71 S  | \ref run               | readMeasurementTelegrams()
  AOS_AS-72 S  | \ref run               |        -
  AOS_AS-88 S  | \ref preInitialization | preInit()
  AOS_AS-89 S  | \ref preInitialization | preInit()
  AOS_AS-162   | \ref run               |        -
  AOS_AS-393 S | \ref initialization    | init()
  AOS_AS-394 S | \ref run               | readMeasurementTelegrams()
  AOS_AS-428 S | \ref run               | readMeasurementTelegrams()
  AOS_AS-429 S | \ref run               | readMeasurementTelegrams()
  AOS_AS-615 S | \ref initialization    | init()
  AOS_AS-633 S | \ref run               | sendDynamicConfig()
  AOS_AS-653 S | \ref run               | sendStaticConfig()
  AOS_AS-764 S | \ref monitorSensorsAndFilters | readMeasurementTelegrams()
  AOS_AS-767 S | \ref monitorSensorsAndFilters | readMeasurementTelegrams()
  AOS_AS-775 S | \ref monitorSensorsAndFilters | readMeasurementTelegrams()
  AOS_AS-778 S | \ref monitorSensorsAndFilters | readMeasurementTelegrams()
  AOS_AS-766   | \ref run               | readMeasurementTelegrams()
  AOS_AS-765 S | \ref run               | readMeasurementTelegrams()

  */

}
