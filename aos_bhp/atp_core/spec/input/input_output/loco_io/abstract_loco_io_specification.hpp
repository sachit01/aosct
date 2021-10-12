namespace ATP::IO::AbstractLocoIO
{

  /**

  \if AsMainPage
  @anchor lio
  \mainpage LocoIO Component Specification


  \endif

  \ifnot AsMainPage
  \class Abstract LocoIO
  \endif
  

  \section Purpose Purpose
  This document specifies the software design for the LocoIO component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality
  The AbstractLocoIO component handles reading of inputs and setting outputs. AbstractLocoIO performs
  the following:
  - read the digital/analog inputs and write digital inputs.
  - periodically trigger the Vital Driver, while reading and writing the outputs.
  - supervise the health state of I/Os and monitor the feedback value of the outputs.
  - trigger the VIOH cross compare every hour to check the VIOH client data is intact.
   
  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  The AbstractLocoIO component has dependencies to the following other components:
  + Brake - for retrieving service and emergency brake application status and status of any of emergency brakes
             applied during BrakeTest.
  + TSetup - to set the locomotive direction from the locomotive orientation.
  + ATPApplication - to acquire the VIOH Client Handle and get ATP Application cycle time.
  + ModeControl - to get information on startup, retrieve current mode, free rolling status and status of location mode.
  + Supervise - to retrieve buzzer information.
  + EventHandler - to report events.

  Other components have dependencies to this component for the public types
  and methods, see \ref AbstractLocoIO Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  AbstractLocoIO constructor and init() functions together handle the initialization of AbstractLocoIO component.
  Constructor initializes total number of IOs signals of the locomotive.</br>
  init() performs the following:
  + checks that the IOs and GPIO are registered with VIOH and wait until the VIOH registration is successful.
  + checks for the revision IDs, HW configuration and device state of the VIO, VOU, AIOU.
  + registers Vital Driver, checks GPIO display.
  + checks and validates VIOH client version. 
  + on completing the initialization of signals, it registers the measurement values to AnalyzerIF.

  \subsection ModeDependentOperation Mode Dependent Operation
  LocoIO is depends on modes to reads values from IO and sets values to IO.
  To read and write outputs, AbstractLocoIO checks that ATP has passed startup.
  For the inputs, before the startup all the IOs are set to invalid except for LCSReady and ATPOff as they are read continuously even if the startup is not completed.
  After the startup is completed, all the inputs are read. 
  For outputs, before the startup SB, EB and ATPOk are set with their default values while other signals are set as per the 
  internal values of AOS. After startup, all values are set as per the internal values of AOS.
  
  \subsection Scheduling Scheduling
  The AbstractLocoIO component has two functions runIn() and runOut() scheduled for every cycle.

  \subsubsection runIn runIn()
  The runIn() function is responsible to read and update values through \ref ReadingInputs, supervise the Vital Driver health states and trigger Vital Driver.
  Before startup, LcsReady and ATPOff are read then after startup it reads input values and Isolation switch else default values are set and Vital Driver is set active.
  The output feedback values are read to verify the outputs. A local counter is maintained and validated to trigger SVD.
  It triggers VIOH cross compare every hour to ensure that the VIOH client internally stored configuration and states are intact.

  @image html abstract_loco_io_run_in.png "runIn() flow chart"
  @image latex abstract_loco_io_run_in.png "runIn() flow chart"
 
  \subsubsection runOut runOut()
  The runOut() function fetches core digital output values from different components and sets the outputs values using \ref WritingOutputs.
  runOut() performs the following:
  + when the Vital Driver is \a active, the output values are written through writeOutputs(). Refer \ref WritingOutputs.
  + If brake test is in progress then EB values are set as Brake test conditions for EB states else set to EB brakes applied from brake component.
  + manages the buzzer status depending on its priority level.
  + updates the brake status information to \a AnalyzerIF component.

  \subsubsection ReadingInputs Reading Inputs
  The function readInputs() calls readDigitalInput() and readAnalogInput() to read core digital and analog inputs.<br>
  readInputs() performs the following:
  + It verifies the health states of inputs and updates the Validity state to valid or invalid.
  + Consecutive invalidated health states of each input value are kept track of and events are raised.
  + If any inputs failed to update, the status is checked to report errors to EventHandler.
  + It calculates loco travel direction and calculates ATO mode based on the ATO Mode Switch Position.

  @image html abstract_loco_io_readdigital_readanalog_inputs.png     "Read Digital Inputs flowchart"
  @image latex abstract_loco_io_readdigital_readanalog_inputs.png    "Read Digital Inputs flowchart"
  
  For the analog Inputs, the flowchart is similar to digital inputs but with no vital check involved.

  \subsubsection WritingOutputs Writing Outputs
  The function writeOutputs() updates the output values depending on the validity state.
  It updates \a newValue, \a validityState and \a timestamp of output value and validates them in \ref ReadOutputFeedback before the value can be used.
  Any failure invalidates the values and event is logged to RU.

  \subsubsection ReadOutputFeedback Read Output Feedback
  This function reads output feedback values and validates the values if it is same as written as output before they can be used.
  For all outputs, it waits until output lag time to set the validity states. Then it validates them.
  It monitors health state error and set to Invalid if Vital Driver is deactivated.
  Safety halt events are raised if any \a healthState error occurs.
     
  \subsubsection SuperviseVitalDriverHealth Supervise Vital Driver Health State
   This function checks for SVD state and sets the vital driver Validity State.
   It checks for current health state to set the vital Driver Active feedback.
   In case of  Validity state is Pending state, it checks vital driver time stamp to 
   set Validity state to Valid. It raises safety halt if any other health state other than valid states occur.
   In case of  Validity state is Valid, it verifies for vital driver active order and vital driver feedback. 
   If they match, it resets vital driver time stamp. If any health state or feedback are not verified, It sets Validity state to Invalid.
   In case of  Validity state is Invalid,it reports safety halt as incorrect vital driver health state.

  

  \section ClassDiagram Class Diagram
  @image html abstract_loco_io_class_diagram.png "Abstract LocoIO Class Diagram"
  @image latex abstract_loco_io_class_diagram.png "Abstract LocoIO Class Diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console-commands
  The command "locoio" prints the following values to the console <br>
  Inputs :
  + DigitalInputs
  + VitalStatus
  + validity
  + Value <br>

  Outputs :
  + DigitalOutputs
  + VFW_Side
  + isVital
  + DefaultValue
  + CurrentValue
  + RelayFB
  + NewValue
  + IsValid
  + Timestamp

  \subsection Analyze Analyze
  The AbstractLocoIO reports the emergency and service brake status to AnalyzerIF component.
  + Indication for Service Brake applied
  + Indication for Emergency Brake applied

  \section CoreAdaptation Core / Adaptation
  Adaptation is responsible to provide the mapping of the IOs to the physical signal number on hardware.
  Adaptation handles TCO functionality and puts adaptation IOs into the data structure provided by the core.
  AbstractLocoIO defines the following pure virtual function that has to be implemented by adaptation.
  + validateViohVersion() - return true if the VIOH version is validated
  
  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component.

  \section Traceability Traceability
  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP Core.

  The requirements relevant for this component are:

  Req        |  Chapter                              | Function
  ---------- | -------------------------------------| --------------
  AOS 1809 S | \ref WritingOutputs                  | writeOutputs()
  AOS 1811 S | \ref WritingOutputs                  | writeOutputs()
  AOS 1173 S | \ref WritingOutputs                  | writeOutputs()
  AOS 1174 S | \ref ReadingInputs                   | readInputs()
  AOS 1176   | \ref ReadingInputs                   | readInputs()
  AOS 1772 S | \ref SuperviseVitalDriverHealth      | superviseVitalDriverHealthState()
  AOS 1773 S | \ref ReadOutputFeedback              | readOutputFeedback()
  AOS 2427   |                -                     | runIn()
  AOS 1717 S | \ref runOut                          | runOut()
  AOS 1188 S | \ref runOut                          | runOut()
  AOS 1175   | \ref runOut                          | setCoreDigitalOutputValue()
  AOS 2688   | \ref runOut                          | runOut()
  AOS 1031   | \ref ReadingInputs                   | calculateATOModeSwitchPosition()
  AOS 2953   | \ref ReadingInputs                   | calculateDrivingDirection()
  AOS 2425   | \ref ReadingInputs                   | calculateDrivingDirection()
  AOS 2426 S | \ref ReadingInputs                   | calculateATOModeSwitchPosition()
  AOS 2749 S | \ref runIn                           | runIn()
  AOS 2685 S | \ref ReadOutputFeedback              | readOutputFeedback()
  AOS 1040   | \ref Console                         | consoleCall()
  AOS 1799 S | \ref runOut                          | runOut()
  AOS 2825   |                -                     | getSleepingSignal()
  AOS 431    |                -                     | getCoreDigitalInputValue()
  AOS 432    |                -                     | getCoreDigitalInputValue()
  AOS 680    |                -                     | manageBuzzerStatus()
  AOS 2695   |                -                     | getEmergencyStopActiveAlert()
  AOS 3150   |                -                     | getCoreDigitalInputValue()
  AOS 783 S  |                -                     | runIn(), runOut(), getCoreDigitalInputValue(), getCoreDigitalOutputValue(), readOutputFeedback()

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].
  
  Common requirements are described in SCDS ATP Core.

  Only the architectural requirements traced explicitly to this component are included in the table below.
  Fulfillment of other architectural requirements allocated to the ATP is described in [SWAS].

  Req          |   Chapter                             | Function
  ------------ | ------------------------------------- | --------------
  AOS_AS-478 S |                 -                     | AbstractLocoIO()
  AOS_AS-111 S | \ref ReadOutputFeedback               | readOutputFeedback()
  AOS_AS-431 S |                 -                     | readDigitalInput()
  AOS_AS-433 S | \ref SuperviseVitalDriverHealth       | superviseVitalDriverHealthState()
  AOS_AS-113 S | \ref ReadOutputFeedback               | readOutputFeedback()
  AOS_AS-115 S | \ref ReadOutputFeedback               | readOutputFeedback()
  AOS_AS-119 S | \ref SuperviseVitalDriverHealth       | superviseVitalDriverHealthState()
  AOS_AS-130 S | \ref runIn                            | runIn()
  AOS_AS-58 S  | \ref Initialization                   | setInitDone()
  AOS_AS-59 S  | \ref Initialization                   | setInitDone()



  */

}
