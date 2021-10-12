/**
\if AsMainPage
\mainpage Checkpoints in ATP 
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-10-24 | Checkpoints in ATP                            | akushwah


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | Automatic Train Protection and Automatic Train Operation on-board system
ELF            | Executable Linkable Format
CRC            | Cyclic redundancy check

\section Introduction Introduction

\subsection Checkpoints Checkpoints Overview

Checkpoints are used to ensure that diversified applications have taken the same execution path.
The checkpoints shall have equal names in the diversified applications. When a checkpoint is visited, an ELF hash tag of the checkpoint name is calculated.
The hash tag is appended to a list of hash tags for the previously visited checkpoints. For each visited checkpoint, the CRC of the hash tags is recalculated.
For each cross-comparison this CRC will be compared between the diversified applications. If a mismatch is detected the cross-comparison will fail.
If the checkpoints are not visited in the same order in the diversified applications, the CRCs will not be equal and the cross-comparison will result 
in the system entering HALT state. After each cross-comparison the CRC and the list of hash tags for visited checkpoints are reset.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

There is no requirement present for checkpoint in ATP now. Some requirements are expected in the later phase of project. 

\subsection Guidelines Guidelines for Checkpoints

The following guidelines shall be followed while creating checkpoints:
- All call back functions shall have checkpoints as it will ensure that these functions are used as intended and in the same order on both CPU-A and CPU-B.
- Checkpoints shall be inserted at the beginning of runIn(), runOut() and run() functions in the adaptation for all the components.
- Functions where the most commonly used data is stored for use in the succeeding cycles are also strong candidates for inserting checkpoints.
- A pragmatic choice needs to be made if adding a checkpoint at both the beginning and the end of function add value (on a case-by-case basis). 
  Functions implementing complex logic and performing critical calculations will gain from having two checkpoints.
- Checkpoints shall also be added in a few selected places, deep inside the program logic.


\section SystemArchitecturalDesign System Architectural Design

\subsection ChosenSystemArchitecture Chosen System Architecture

Checkpoints shall be inserted with the intention of verifying that the program flow of the ATP Software running on the diversified hardware (CPU-A and CPU-B)
have taken the same execution path.
Each checkpoint must be initialized with a unique string within the process. (The string will be the same on both CPU-A and CPU-B).
Initialization of the checkpoints will be done by using VFW function vfwInitCheckPoints(). This function will be called in initApplication(void) function of atp_main.cpp.
Once initialization is done, need to call vfwVisitCheckPoint() function with a unique string for each called function.

The unique string shall be assembled in one of the two ways as follows:
- single point checkpoint: functions with a single checkpoint at the beginning as ComponentShortName_FunctionName

\code
//Pseudo code Start
Assuming, the short name for MyAOSComponent is AC:
void MyAOSComponent::checkPointExample(void)
{
   static uint32_t cp = 0U; // Must be initialized to 0
   vfwVisitCheckPoint(&cp, "AC_checkPointExample");
   //Add code here
}

//Pseudo code End
\endcode

- Multiple point checkpoint: functions with a checkpoint at the beginning and one at the end as ComponentShortName_FunctionName_Begin and ComponentShortName_FunctionName_End

\code
//Pseudo code Start
Assuming, the short name for MyAOSComponent is AC:
void MyAOSComponent::checkPointExample(void)
{
   static uint32_t beginCp = 0U; // Must be initialized to 0
   static uint32_t endCp = 0U;   // Must be initialized to 0

   vfwVisitCheckPoint(&beginCp, AC_checkPointExample_begin");
   // Add code here
   vfwVisitCheckPoint(&endCp, AC_checkPointExample_end");
}

//Pseudo code End
\endcode

NOTE: code for vfwInitCheckPoints() needs to be removed from AbstractCrossCompare::preInit() in abstract_cross_compare.cpp as it needs to be added in atp_main.cpp.
Also, the unique string of vfwVisitCheckPoint() needs to be updated as per the new naming convention(described above) in abstract_cross_compare.cpp.

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

<Describe if there are any alternatives to the chosen design.>

\subsection ExternalInterfaceDescription External Interface Description

<Describe if there's any changes to external systems.>

\section DetailedDescriptionOfComponents Detailed Description of Components

Following the checkpoints guidelines in the coding conventions document, checkpoints insertion in ATP Software needs to be done as mentioned below.
- Each callback function will have a single point checkpoint insertion.
- Single point checkpoint need to be inserted for each run function (run(), runIn(), runOut()) of the component whereas applicable.
The first choice of checkpoints insertion will be in the adaptation code, if run function is not present in adaptation then it need to be added in core part.
Also, one checkpoint need  to be added for AbstractATPApplication::run() function as it is responsible for scheduling run functionality of ATP software.
- Single point checkpoint need to be inserted for the functions which store the most commonly used data.
  + Examples:
   - updateTargets() in Targets component.
   - updateTracks() in Tracks component
   - setNextMode() in ModeControl component.

- Multiple point checkpoint need to be inserted for the functions implementing complex logic and performing critical calculations .
  + Examples:
    - isSlipDetected(), isSlideDetected() and updateBaliseWindow() in Odometry component.
    - calcCurveDistance(), calcEBCurveDistance(),calcSBCurveDistance(), calcSecondWarningCurveDistance(), calcFirstWarningCurveDistance(), calcCurveSpeed(),
      calcSecondWarningCurveSpeed(), calcDistanceBCA(), calcPredictedSpeedAtTarget(), calcTimeToIntervention() in Brake curve Calculations.
    - updatePermittedSpeed(), updateGBC(), calculateCeilingSpeed() in Target Calculations.
    - findClosestSupervisableTarget(), superviseTargets(), superviseCeilingSpeed() in Supervise Components.

- Single point checkpoint need to be inserted deep inside the program logic to ensure the flow of ATP Software have taken the same execution path.
  + Examples:
    - getApproximatePosition() in the Message Handler component.
    - getDMIButtonStatus() in DMI Handler component.
    - runBTMAntennaPowerStateMachine() in BTM Handler component.

\section UserInterfaceDesign User Interface Design

\subsection DescriptionOfTheUserInterface Description of the User Interface

<If applicable, describe the design/re-design of the UI.>

\subsubsection ScreenImages Screen Images

<If applicable, include screen-shoots or other images to better describe the UI.>

\subsubsection ObjectsAndActions Objects and Actions

<If applicable, describe the actions trigged by certain UI events.>

\section ImplementationDistribution Task Distribution

There will be one task for the implementation where all the above mentioned coding needs to be done.

\section AdditionalMaterial Additional Material

Refer Documents 1DOC-1015360 ( C/C++ Programming Conventions Interflo 150 AOS BHPB) V1.5 section 7 Checkpoints.

*/