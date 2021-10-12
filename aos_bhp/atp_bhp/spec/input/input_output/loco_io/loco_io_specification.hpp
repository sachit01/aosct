namespace ATP::IO::LocoIO
{

/** 
\if AsMainPage
@anchor lio
\mainpage LocoIO Component Specification
\endif

\ifnot AsMainPage
\class  ATP::IO::LocoIO
\endif

\section Purpose Purpose
This document specifies the software design for the BHP adaptation for the Loco IO component. 

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
The LocoIO class is the BHP adaptation of the core AbstractLocoIO class.
It is responsible for the following:
+ mapping the I/Os to Vital IO board pin out sequence.
+ read inputs and provide interface for other components to read values. 
+ to process the HiRail input signal and sleeping signal.
+ read TCO feedback input value and set TCO order value. 
+ sets digital and analog output values for adaptation inputs and VIOH version

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
The LocoIO component is dependent on following components
+ Brake        : To check the status of brake tests, TCO status
+ ModeControl  : To check the ATP startup status.
+ EventHandler : To report events.

Other components are dependent on this components functionality by its public functions. <br>
Refer to Public Member Functions from \ref ATP::IO::LocoIO.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization
The initialization of LocoIO is done in its constructor and the \ref init() function.
+ The constructor initializes in a sequence of core and adaptation inputs followed by core and adaptation outputs. It is responsible for adding and mapping the adaptation I/Os into the same arrays of core I/Os.
+ The init() function initializes the base class and registers brake pressure value measurement variables to AnalyzerIF.

\subsection ModeDependentOperation Mode Dependent Operation
The LocoIO component depends on startup mode operation. Before startup it sets 1 all the time.
After startup, It checks for brake test in progress status to set the values and handle the TCO value. 

\subsection Scheduling Scheduling
The LocoIO component has two functions runIn() and runOut() scheduled for every cycle.

\subsubsection runIn runIn()
The runIn() function executes the base class's runIn() function and handles the Rail/Road mode signals for HiRail. See \ref handleRailRoadsignal.

\subsubsection runOut runOut()
The runOut() function handles two functionalities 
- If TCO feedback is configured to be used, it retrieves TCO value from Brake depending on the brake test status.
- It executes the base class's runOut() function.

\subsection handleRailRoadsignal Handle Rail/Road Signals
The function handleRailRoadSignal() is valid for locomotive type HiRail. 
Safety halt is raised if any signal other than \a RoadMode, \a RailMode occur.

\subsection HandlingInputs  Handling Inputs
In this adaptation component, the initialization of adaptation inputs are done after the initialization of core inputs. 
The getAdapAnalogInputValue() function reads the raw value of given analog input signal and scales it based on associated range defined by configuration parameters.
The scaled analog value is calculated as below:

\f[ value = (min range of brake pressure)+ \frac{(raw value-min analog raw value )*(delta of min and max range of brake pressure)}{(max analog raw value - min analog raw value)}  \f]

\section ClassDiagram Class Diagram
@image html loco_io_class_diagram.png "LocoIO Class Diagram"
@image latex loco_io_class_diagram.png "LocoIO Class Diagram"

\section Diagnostics Diagnostics

\subsection Console Console-commands
The console command "locoio" shall add the following analog inputs values to the core values .
- AnalogInputs 
- Validity 
- RawValue 
- ScaledValue 

\subsection Analyze Analyze
The LocoIO reports the following to Analyzer.
- brakePressure 1
- brakePressure 2
- TCO feedback

\section CoreAdaptation Core / Adaptation
Adaptation component is responsible to add the I/Os into the I/O array provided by the core and maps the I/Os.
In the adaptation component, these virtual functions overrides the functions of core component
- runIn()
- init()
- runOut()
- getSleepingSignal()
- getSbApplied()
- consoleCall()
- initCrossCompare()
- validateViohVersion()

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component.
  
\section Traceability Traceability
\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in the SCDS ATP BHP.

The requirements relevant for this component are:
   
Req            | Chapter              | Function
---------------| --------------       | --------------
AOS_BHPB 2635  | \ref Initialization  | LocoIO()
AOS_BHPB 2807  | \ref Initialization  | LocoIO()
AOS_BHPB 2808  | \ref Initialization  | LocoIO()
AOS_BHPB 2682 S| \ref handleRailRoadsignal  | handleRailRoadSignal()
AOS_BHPB 2683 S| \ref handleRailRoadsignal  | handleRailRoadSignal()
AOS_BHPB 2630  | \ref Initialization  | LocoIO()
AOS_BHPB 2631  | \ref Initialization  | LocoIO()
AOS_BHPB 2814 S| \ref runOut          | runOut()
AOS_BHPB 2646 S| \ref runOut          | runOut()
AOS_BHPB 2809 S| \ref runOut          | runOut()
AOS_BHPB 2632  |          -           | getSleepingSignal()
AOS_BHPB 2636  |          -           | getSleepingSignal()
AOS_BHPB 2847 S|          -           | getAdapDigitalOutputValue()
AOS_BHPB 2768 S|          -           | LocoIO()
AOS_BHPB 2962 S|          -           | getAdapDigitalOutputValue()
AOS_BHPB 2963 S|          -           | getAdapDigitalOutputValue()

\subsection SSAS Architectural requirements
The architectural requirements are defined in [SSAS-APP] .

Common requirements are specified in SCDS ATP BHP.

Fulfillment of other architectural requirements allocated to the ATP is described in [SWAS].

*/
}

