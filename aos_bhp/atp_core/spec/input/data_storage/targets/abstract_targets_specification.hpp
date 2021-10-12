namespace ATP::DS
{
  /**
  \if AsMainPage
  \mainpage Targets Component Specification
  @anchor ta
  \endif

  \ifnot AsMainPage
  \class AbstractTargets
  \endif

  \section Purpose Purpose
  This document specifies the software design for the core part of Targets component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview
  \subsection GeneralFunctionality General Functionality
  The Targets component implements the data storage for all targets within the AOS.
  The target storage consists of MA target list and supervised target list accessible for other components.
  After the MA is received from the TCC then the new targets are pushed into the MA target list by Message Handler.
  The supervised targets are calculated based on the MA targets received and are inserted into the supervised target list by the
  Target Calculation component. The main functionality of the Targets component is to store and delete  MA targets and supervised targets.
  The component provides functions to create targets by other components.

  The component provides the following functionality:
  + Adding target objects.
  + Removal of target objects.
  + Sorting of targets in the expected travel direction.
  + Iterating over targets in both travel directions.


  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  This component has dependencies to the following components:
  + Mode Control: To fetch current 'ATP mode', 'idle state' and 'Free Rolling status'.
  + Train Setup: To get current ceiling speed.
  + Brake: To get emergency brake applied.
  + Odometry: To get standstill status
  + Loco IO: To get emergency stop active signal.

  Other components have dependencies to this component because they use its public types
  and methods, see \ref AbstractTargets Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design
  \subsection Initialization Initialization
  The AbstractTargets class is responsible for initializing the attributes of class and allocate the memory needed to
  instantiate Primary Target, Gradient Target, Speed Target, TrackDataItem Target and Supervised Target using "Fixed-Size Memory Pool technique".
  This technique creates a memory-pool for a fixed number of objects and allocates memory
  for the entire pool at initialization. If memory is not allocated for any target, Safety halt event is raised.  

  \subsection ModeDependentOperation Mode dependent operation

  The following table shows the deletion of targets on mode transitions.
  The table setup is as follows:
  - Rows define the new mode after the transition
  - Columns define original, or from, mode

  Mode (to below)      |PU|PD|TC|BS|NO|SR|LH|SH|SE|SL|PO|YD|UR|SS|ST|
  ---------------------|:-|:-|:-|:-|:-|:-|:-|:-|:-|:-|:-|:-|:-|:-|:-|
  PowerUp (PU)         |x |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
  PowerDown (PD)       |D |x |D |D |D |D |D |D |D |D |D |D |D |D |D |
  TrainConfig (TC)     |D |- |x |- |- |- |- |D |U |D |D |D |D |U |- |
  BaliseSearch (BS)    |- |- |U |x |- |- |- |- |- |- |- |- |- |U |- |
  Normal (NO)          |- |- |U |U |x |U |U |- |U |- |- |- |- |U |- |
  StaffResponsible (SR)|- |- |U |- |U |x |- |- |U |- |- |- |- |- |- |
  LocationHandling (LH)|- |- |- |- |U |- |x |- |- |- |- |- |- |- |- |
  Shunting (SH)        |- |- |D |D |D |D |- |x |D |- |- |- |- |D |- |
  ShuntingRoute (SE)   |- |- |- |- |U |U |- |- |x |- |- |- |- |- |- |
  Sleeping (SL)        |D |- |D |- |D |- |- |D |D |x |D |D |D |D |- |
  Possession (PO)      |D |- |D |D |D |D |- |D |D |D |x |D |D |D |- |
  Yard (YD)            |D |- |D |D |D |D |- |D |D |D |D |x |D |D |- |
  Unregistered (UR)    |D |- |D |D |D |D |D |D |D |D |D |D |x |D |- |
  SafeBrakeToStop (SS) |D |- |U |U |U |U |U |D |U |D |D |D |D |x |- |
  SafetyHalt (ST)      |D |D |D |D |D |D |D |D |D |D |D |D |D |D |x |

  - U: All targets are unchanged
  - D: All targets are deleted
  - -: Mode Transition not possible
  - x: Not Applicable (Mode in column and row are same.)


  \subsection Scheduling Scheduling
  The functionality in the component is mainly data driven because targets are added or deleted based on external calls
  to interface functions. The run() function is executed once every cycle to allow
  for internal house-keeping routines to be performed.

  \subsubsection run run()
  AbstractTargets::run() function is structured as follows:
  - Resets flags to 'get deleted in target list', 'target list changed' and 'target list reversed in Location mode'.
  - Remove all targets from the MA target list and supervised target list according to below conditions.
   + 'train is standstill', 'emergency stop active signal is active' and 'EB is not applied'.
   + ATP mode transition according to \ref ModeDependentOperation
  - Reset Supervise status of the target changed for conditional targets. This status is being used to re-visit the supervised targets.
  - Manage free memory-blocks counters of primary target, gradient target, speed target and TrackDataItems target.

  \subsection MATargetListHandling  MA Target List Handling
  MA target list is populated when the MA is received. The MA target list contains the pointers to all currently stored MA targets.
  The MA target list will be kept in valid order at all times thus any insertion or deletion of a target will immediately
  rearrange the target list accordingly. In case direction get changed, the target list is reordered as per the expected travel direction.
  The different types of MA targets are:
  - Primary target: The target is created to reach out end of the MA, when MA is received.
  - Location targets: The Location Start Target and Location End Target are created when location data is received in MA.

  \subsubsection MATargetInsertion MA Target Insertion
  AbstractTargets overloads AbstractTargets::addTarget() function to add various targets (\ref BaseTarget::CoreTargetType and \ref BaseTarget::LocationTargetType ) in MA target list, when MA message is accepted from TCC.
  Message Handler creates the MA targets and created targets are added by AbstractTargets::addTarget(). AbstractTargets::insertMaTargetInList() function inserts new target in MA target list.
  It validates target and inserts it at suitable position based on the current defined travel direction. If new target already exists in MA target list, a safety halt event will be raised. 

  \subsubsection TargetDeletion  Deletion of target
  When MA target is deleted, the MA target list is kept in order by deleting invalid pointer from this list.
  The supervised targets related to the deleted parent MA target are also deleted along with target pointers from the supervised target list.
  The supervised targets are also deleted when the MA targets are out of the scope or are modified.


  \subsection SuperVisedTargetListHandling Supervised Target List Handling
  Pointers to supervised targets are inserted and arranged in the supervised target list.
  The different types of supervised targets(\ref SupervisedTarget::SupervisedTargetType) are:
  - \b Gradient \b Supervise \b Target : The target is created for gradient changes in the MA.
  - \b Emergency \b Brake \b Speed \b Target: The target is created for ceiling speed change in the MA which reduces the ceiling speed.
  - \b Service \b Brake \b Speed \b Target : The target is created for ceiling speed change in the MA which reduces the ceiling speed.
  - \b Second \b Warning \b Speed \b Target: The target is created for ceiling speed change in the MA which reduces the ceiling speed.
  - \b First \b Warning \b Speed \b Target : The target is created for ceiling speed change in the MA which reduces the ceiling speed.
  - \b EB \b Primary \b Target : The target is created for the MA end or location borders in the MA.
  - \b SB \b Primary \b Target : The target is created for the MA end or location borders in the MA.
  - \b First \b Warning \b Primary \b Target : The target is created for the MA end or location borders in the MA.
  - \b Second \b Warning \b Primary \b Target : The target is created for the MA end or location borders in the MA.
  - \b SpeedIncrease \b Target: The target is created for the ceiling speed increase in the MA.
  - \b SMSpeedRestriction \b Target: Safety Margin SpeedRestriction target is created to restrict the speed, when the Safety Margin is higher than the Safety Margin at MA end.

  \subsubsection SupervisedTargetInsertion Supervised Target Insertion
  The supervised targets are created by the Target Calculation component based on the MA targets received in the MA and the pointers of created targets are inserted in
  supervised target list. There should not be duplicate targets at the same position. Otherwise, a safety halt event will be raised. Two gradient supervised targets
  should not overlap. Otherwise a safety halt event will be raised. It is validated while adding the target. If a new supervised gradient target
  is added to the list in the already existing position of a supervised target in the list then only the gradient data will be updated of
  the existing target and new target would not be added in the list. If a new supervised target added in the already existing position of a gradient supervised
  target in the list then only the new target will be updated with the gradient data and old data will be removed.

  \subsection removePassedTargets Remove Passed Targets
  When leading position of the train passes a target position in the same direction of target,
  the target is deleted and its pointer is also removed from the target list. If primary target has passed, all targets will be deleted.
  If any other MA target (except primary target) has passed, the MA target and related supervised targets will be deleted.

  \subsection PrimaryTargte Primary Target
  Message Handler creates Primary target and it is inserted in MA target list, when MA is received from TCC,
  When primary target is handled and train passes this target ref \ref removePassedTargets. Primary target and related supervised targets will be deleted.
  The class PrimaryTarget is used to store the End of the MA or the location borders.

  \subsection SpeedTarget Speed Target
  Speed targets will be created or updated when current ceiling speed doesn't match with received ceiling speed in MA.
  Related supervised targets (\ref SupervisedTarget::SupervisedTargetType) are also created along with Speed target by Target Calculation component.
  The class SpeedTarget is used to store ceiling speed change in the MA.

  \subsection GradientTarget Gradient Target
  When MA is received from TCC, gradient targets are created or updated for gradient change in MA.
  Gradient supervised targets are also created by Target Calculation component.
  The GradientTarget class will be used to store gradient changes in the MA.

  \subsection SupervisedTarget Supervised Target
  Supervised targets are created by Target Calculation component and the Targets Component inserts created targets in supervised target list.
  These targets are used to supervise the MA targets. When these targets are handled and passed, targets are deleted and
  pointers are also deleted from the supervised targets list. The SupervisedTarget class is used to store these targets that are not
  received from MA but are created in AOS.

  \subsection TrackDataItem TrackDataItem Target
  When track data items are received in MA from TCC, TrackDataItem target is created and added in MA target list by Message Handler component.
  The TrackDataItemTarget class is used to store track data items received in the MA.

  \section ClassDiagram Class Diagram
  \subsection ComponentClassDiagram Component Class Diagram
  @image html abstract_targets_class_diagram.png  "AbstractTargets Class diagram"
  @image latex abstract_targets_class_diagram.png  "AbstractTargets Class diagram"

  \subsection TargetsClassesDiagram Targets Classes Diagram
  The BaseTarget class is the ancestor for all targets managed by AbstractTargets class. The BaseTarget class
  stores the common attributes required to manage targets in a valid travel order and additional
  common attributes. The BaseTarget can not be instantiated. All derived classes of BaseTarget class are
  required to override new and delete operators using pre-allocated memory pools.
  The target classes are inherited from BaseTarget class as below:
  @image html targets_classes.png  "Targets Classes diagram"
  @image latex targets_classes.png  "Targets Classes diagram"

  Each target class is fully defined and contains both attributes and functions to access the
  stored data. The target objects will be accessed by other components with a need to use
  the target lists.

  \section Diagnostics Diagnostics

  \subsection Console Console Commands
  - maTarg : Print all targets of MA target list.
  - supvTarg : Prints all supervised targets of Supervised target list
  - targets : Prints all MA targets and supervised targets.
  - curCS :  Prints all current ceiling speeds.
  - curGrad : Prints all current gradient.

  \subsubsection Analyze Analyze
  Following variables are added at initialization for analysis. These variables are to be accessed by an external Analyzer tool,
  Variable                  | Unit     |
  ---------------------     |--------  |
  curTrackGradient          | int32_t  |
  curGradient               | int32_t  |
  curFWCeilingSpeed         | cm/s     |
  curSWCeilingSpeed         | cm/s     |
  curSWCeilingSpeed         | cm/s     |
  curEBCeilingSpeed         | cm/s     |
  freeBlocksPrim            | uint32_t |
  freeBlocksGrad            | uint32_t |
  freeBlocksSpeed           | uint32_t |
  freeBlocksTDI             | uint32_t |
  freeBlocksSupervised      | uint32_t |

  \section CoreAdaptation Core / Adaptation
  Major functionality of Targets component is implemented in core part and Adaptation part creates the singleton instance of the component.
  The Adaptation class is inherited from AbstractTargets class of core part.

  \section PreProcessor Pre-Processor Directives

  No pre-processor directives available for this component

  \section Traceability Traceability

  \subsection SSRS Functional Requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP Core.

  The requirements relevant for this component are:

  Req        | Chapter                        | Function
  ---------- | ------------------------------ | --------
  AOS 120 S  | -                              | AbstractTargets::getLocationEndTarget() <br /> AbstractTargets::getLocationStartTarget()
  AOS 212    | -                              | AbstractTargets::getPrimaryTarget()
  AOS 227 S  | \ref SpeedTarget               | SpeedTarget::isPassedTargetHandled()
  AOS 305    | \ref run                       | AbstractTargets::run()
  AOS 402    | \ref run                       | AbstractTargets::run()
  AOS 460 S  | \ref run                       | AbstractTargets::run()
  AOS 1038   | \ref MATargetInsertion         | AbstractTargets::insertMaTargetInList()
  AOS 1039   | \ref SupervisedTargetInsertion | AbstractTargets::insertSupvTargetInList()
  AOS 1120 S | \ref MATargetInsertion         | AbstractTargets::insertMaTargetInList()
  AOS 1132   | \ref run                       | AbstractTargets::run()
  AOS 2121   | -                              | AbstractTargets::setCurCeilingSpeed()
  AOS 2122   | -                              | AbstractTargets::setCurCeilingSpeed()
  AOS 2142   | \ref MATargetInsertion         | AbstractTargets::insertMaTargetInList()
  AOS 2143   | \ref removePassedTargets       | AbstractTargets::removePassedTargets()
  AOS 2144   | \ref ModeDependentOperation    | AbstractTargets::removeAll()
  AOS 2197   | \ref ModeDependentOperation    | AbstractTargets::delTarget()
  AOS 2199   | \ref ModeDependentOperation    | AbstractTargets::delTarget()
  AOS 2255   | \ref removePassedTargets       | AbstractTargets::removePassedTargets()
  AOS 2416   | \ref ModeDependentOperation    | AbstractTargets::removeAll(
  AOS 2695   | \ref run                       | AbstractTargets::removeAll()
  AOS 2982   | \ref removePassedTargets       | AbstractTargets::removePassedTargets()
  AOS 2983   | \ref removePassedTargets       | AbstractTargets::removePassedTargets()
  AOS 3165   | -                              | AbstractTargets::addTarget() <br /> AbstractTargets::delTarget() <br /> AbstractTargets::delTargetsInRange()
  AOS 3208   | \ref run                       | AbstractTargets::removeAll()
  AOS 3292 S | \ref MATargetInsertion         | AbstractTargets::insertMaTargetInList()

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP Core.

  Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

  */
}

