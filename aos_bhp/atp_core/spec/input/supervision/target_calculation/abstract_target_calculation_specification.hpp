namespace ATP::Supv
{
  /**
  \if AsMainPage
  \mainpage Abstract Target Calculation Component Specification
  @anchor tc
  \endif

  \ifnot AsMainPage
  \class Template
  \endif

  \section Purpose Purpose
  This document specifies the software design of the AbstractTargetCalculation class, the core part of the Target Calculation component.

  \subsection Abbreviations Abbreviations

  Term             | Definition
  ---------------- | ------------------------------------------------------------------
  CS<SUB>EB</SUB>  | Ceiling speed for emergency brake curve.
  CS<SUB>SB</SUB>  | Ceiling speed for service brake curve.
  CS<SUB>SW</SUB>  | Ceiling speed for second warning curve.
  CS<SUB>FW</SUB>  | Ceiling speed for first warning curve.
  G                | Current gradient cm/s^2
  M<SUB>cw</SUB>   | Warning speed margin
  M<SUB>cs</SUB>   | Service brake speed margin
  M<SUB>ce</SUB>   | Emergency brake speed margin
  r                | Current brakeability cm/s^2
  SL               | Section Length
  STP              | Supervised Target Point
  t<SUB>fw</SUB>   | first warning to second warning curve delay (Obtained from config)
  t<SUB>sw</SUB>   | second warning to service brake order delay (Obtained from config)
  t<SUB>sb</SUB>   | SB order to EB order delay (Obtained from config)
  t<SUB>eb</SUB>   | Effective EB delay (Obtained from config)
  t<SUB>sbr</SUB>  | Brake reaction time for service brake.
  t<SUB>ebr</SUB>  | Brake reaction time for emergency brake.
  v<SUB>a</SUB>    | Current speed cm/s
  v<SUB>fw</SUB>   | First warning curve speed
  v<SUB>sw</SUB>   | Second warning curve speed
  v<SUB>sb</SUB>   | Service brake curve speed
  v<SUB>eb</SUB>   | Emergency brake curve speed
  
  \latexonly \newpage \endlatexonly

  \section Overview Overview
  Target Calculation component is responsible for creating the supervised targets from the MA targets. It will then calculate
  the ceiling speed, gradient and permitted speed for the supervised targets.
  The component is responsible to provide supervise component with all the information it needs to effectively supervise
  only the closest supervised target without having to consider other targets. Target Calculation component shall propagate all restrictions in speed and 
  gradient in the MA of all targets starting from the primary target to the closest target to the train front.

  \subsection GeneralFunctionality General Functionality

  All supervised targets contain the following to facilitate calculation of permitted speed and speed limits:
  - ceiling speed each for first warning, second warning, SB and EB.
  - permitted speed each for first warning, second warning, SB and EB.
  - gradient.

  Note that gradient for a supervised target is different from Track Gradient that is a member of gradient targets class.
  Track Gradient shows the gradient of the track from the target's point to the next Gradient target in the
  direction of the MA, while gradient for a supervised target is the effective gradient on the train from the current target to the next
  target, considering train length and gradient regions that it covers.

  The AbstractTargetCalculation class is derived from the base class ProcComp. The component's main functionalities include:
  - Providing ceiling speed information to Supervise:
  Mode dependent ceiling speed is calculated by Target Calculation component and is used by Supervise component.
  -  Create Supervised targets
    - Create and add supervised gradient targets.
      - Calculate the gradients (effective gradient) for all supervised gradient targets.
    - Create and add supervised targets for ceiling speed changes and primary targets.
    - Calculate the ceiling speeds and gradients for all supervised targets.
  -  Calculate the curve speeds for supervised targets starting from the primary target and iterating back.
  -  Calculate the release speed SB (R<SUB>SB</SUB>) and release speed EB (R<SUB>EB</SUB>) position. This is the position where the SB/EB curve equals the configured release speed.
  
  Refer to Targets component SCDS for a description of the target types in AOS.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  
  This component is dependent on the following components for the required functionality
   - AbstractTargets : To get target list, current CS, current gradient, safety margin speed restriction and change in target list.
   - AbstractTracks : To convert Track and Position to odometer value.
   - AbstractMessageHandler: To get the MA received status.
   - AbstractModeControl: To get the current Mode.
   - AbstractTSetup: To check if brakeability or brake response time has changed.
   - AbstractOdometry: To get if the train is at standstill.

  Other components have dependencies to this component because they use its public types
  and methods, see \ref AbstractTargetCalculation Class Reference.
   

  \latexonly \newpage \endlatexonly

  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  During initiation the abstract class shall initialize internal variables.
  No dynamic memory allocation is needed.
  The init function also registers the variables for the Analyzer interface and initializes the cross compare.

  \subsection ModeDependentOperation Mode Dependent Operation
  \subsubsection calculateCeilingSpeed Mode Dependant Ceiling Speed
  The component determines the mode dependant ceiling speed to be used in supervision as follows:

  ATP mode                            | Speed                                        |
  ------------------------------------| ---------------------------------------------|
  Power Down                          | 0 (zero)                                     |
  Power Up                            | 0 (zero)                                     |
  Unregistered                        | 0 (zero)                                     |
  Safety Halt                         | 0 (zero)                                     |
  Registration                        | 0 (zero)                                     |
  Configuration                       | 0 (zero)                                     |
  Sleeping                            | 0 (zero)                                     |
  Balise search                       | baliseSearchSpeed, defined by Config.   |
  Safe Brake to Stop                  | Current ceiling speed from Targets component |
  Yard                                | Current ceiling speed from Targets component |
  Possession                          | Current ceiling speed from Targets component |
  Shunting                            | Current ceiling speed from Targets component |
  Split                               | Current ceiling speed from Targets component |
  Staff Responsible                   | Current ceiling speed from Targets component |
  Normal                              | Current ceiling speed from Targets component |
  Shunting Route                      | Current ceiling speed from Targets component |
  Location                            | Current ceiling speed from Targets component |
  Join                                | Current ceiling speed from Targets component |

  If the speed is restricted as the SMSpeedRestrictionTarget, described in \ref smSpeedRest is passed, the ceiling speed is the minimum of the mode dependant speed calculated and the speed of the Safety Margin restriction (which is equal to configured release speed).
  
  \subsection Scheduling Scheduling
  The component has a function (inherited from the base-class ProcComponent), run(), that must be called
  each execution-cycle.

  \subsubsection run run()
  The class method run() will calculate the Mode Dependant Ceiling Speed defined in \ref calculateCeilingSpeed.
  If an MA from scratch is received, the current FW, SW, SB and EB Ceiling Speeds and current Gradient is calculated and updated in the Targets component.

  The run function will update the supervised targets and the speed and gradient of supervised targets if:
  - a new MA is received
  - target list has changed without any new MA
  - brake response time has changed
  - brakeability of the train has changed
  - condition for conditional target becomes true

  Depending on the above conditions, the run function will take one or more of following actions:
  - Remove all supervised targets.
  - Add gradient supervised targets based on train length and config parameters and calculate the gradient.
  - Add supervised targets ceiling speed changes and primary target.
  - Calculate ceiling speed and gradient at all supervised targets.
  - Calculate curve speeds at all supervised targets.

  @image html abstract_target_calculation_run.png  "run"
  @image latex abstract_target_calculation_run.png "run"


  \subsection supv_grad_target Supervised Gradient Target
  A supervised gradient target is created in AbstractTargetCalculation class and will be pushed in the supervised target list based
  on its odometer position.
  These targets are created to facilitate the handling of gradient changes, taking into account
  the possibility of having long trains which means that it is possible to have only a small portion of the train
  affected by gradient change at some points. The supervised gradient targets distribute the
  change in gradient over a region and thus avoid the need to take the worst case gradient into account
  and using the lowest gradient for all the regions.

  \subsubsection AddSupvGradientTargets Adding Supervised Gradient Targets
  This method is responsible for adding supervised gradient targets.
  The function is described in the following process diagram:

  @image html abstract_target_calculation_add_helper_gradient_targets.png "Adding supervised gradient targets"
  @image latex abstract_target_calculation_add_helper_gradient_targets.png "Adding supervised gradient targets"

  The number of supervised gradient targets for each gradient change in MA is determined by the following parameters.
  - MaxSupvGradientTarget (config):  The Maximum number of supervised gradient targets
  - MinSupvGradientTargetDistance (config): Minimum distance between supervised gradient targets.
  - TrainLength (Train Setup): The length of the train received from TCC.

  The section length (SL) is the distance between each supervised gradient target.
  SL is calculated as below:

  \f[ SL = Min(Max(\frac{TrainLength}{Max (MaxSupvGradientTarget , 2)}, MinSupvGradientTargetDistance), TrainLength)\f]

  For any gradient target, the minimum number of supervised gradient targets is 2, one at the gradient target location
  and one at the distance of train length from it.

  \subsubsection findGradTargetToProcess Find the Gradient Target to Process
  The function finds out the first gradient target for which the supervised gradient targets has not been created.
  Based on the gradient target, the position of the first supervised gradient target is also calculated.

  For an MA from scratch, the first gradient target to process is:
  - If there are gradient targets under the train, it is the last gradient target under the train footprint in the MA direction.
  The position of the first supervised target is at the train leading position.
  - If there are NO gradient target under the train, it is the first gradient target ahead of the train leading position.
  The position of the first supervised target is at the position of the gradient target.

  For an MA extension, the first gradient target to process is:
  - The last gradient change in the previous MA, if the distance from gradient target to MA end was not enough to add all supervised targets.
  The position of the first supervised target is at the MA start position.
  - The first gradient target in the MA extension, if all gradient targets in the previous MA was processed.
  The position of the first supervised target is at the position of the gradient target.

  \subsubsection createSupvGradTargets Create Supervised Gradient Targets

  Starting from the first gradient target to process and the position for the first supervised gradient target,
  create supervised gradient targets each with the distance of SL to the previous supervised gradient targets.
  For gradient targets after the first gradient target to process, the supervised gradient targets are created starting from the
  gradient target position.

  For each gradient target create supervised gradient targets unless:
  - the position of the supervised gradient target is after the end of MA, OR
  - the position of the supervised gradient target is after the next gradient target, OR
  - the distance between supervised gradient target and its parent gradient target is more than train length.

  If any of the above conditions are true, stop creating additional supervised targets for this gradient target.

  The function is described in the following process diagram:
  @image html abstract_target_calculation_create_supv_gradient_targets.png "Create supervised gradient targets"
  @image latex abstract_target_calculation_create_supv_gradient_targets.png "Create supervised gradient targets

  See the following examples for creating supervised gradient targets close to MA end:
  
  \paragraph exampleNoRoom There is not enough distance in MA to add all supervised targets:
  For some gradient targets it will not be possible to add all supervised targets as supervised targets will end up
  after the MA.
  In this example it is possible to add only one supervised target.
  AbstractTargetCalculation needs to remember to create the rest of supervised targets, if it is possible, when the MA is extended.

  @image html abstract_target_calculation_add_helper_target1.png "Only some Supervised Targets added"
  @image latex abstract_target_calculation_add_helper_target1.png "Only some Supervised Targets added"

  \paragraph exampleRoom The MA is extended enough to create all supervised targets:
  Upon receiving a new MA, check if all supervised targets for the last gradient target were created. If not create the first
  supervised target at MA start and then continue to create as many as possible.
  With every new MA, repeat this process until all supervised targets are created or we reach a new
  gradient target.

  @image html abstract_target_calculation_add_helper_target2.png "The rest of Supervised Targets are added"
  @image latex abstract_target_calculation_add_helper_target2.png "The rest of Supervised Targets are added"

  \subsubsection calculateGradient Calculating the Gradient
  Gradient is a value that is used for supervision instead of TrackGradient value.
  This value is based on dividing the track to sections that are affected by different gradient regions and
  different parts of the train. The train in the following figure, based on its location and length, is affected
  by different gradients.
  @image html abstract_target_calculation_gradient_on_the_track.png "Different gradient regions within the track"
  @image latex abstract_target_calculation_gradient_on_the_track.png "Different gradient regions within the track"

  The gradient is calculated and the result is set in the “gradient” or G in the target, based on the following
  formula:
  \f[ G = \frac{(L_1* G_1 )+...+(L_n* G_n)}{TrainLength }\f]

  Where:
  - L<SUB>1</SUB>,...,L<SUB>n</SUB>  are train section lengths that are on top of each gradient
  - G<SUB>1</SUB>,...,G<SUB>n</SUB>  are the gradient regions

  Each gradient region starts from the gradient target and ends at the next gradient target with the TrackGradient
  value of the starting gradient target. The sub-section gradient calculation shall consider
  the worst case scenario (lowest value) for gradient.

  G or Gradient is the gradient that is used for the brake curve calculation and for each supervised gradient target starts from the
  target point odometer and ends on the next supervised or MA target. TrackGradient is not used in any brake curve calculation.

  @image html abstract_target_calculation_gradient_member.png "Gradient region"
  @image latex abstract_target_calculation_gradient_member.png "Gradient region"

  This region is calculated considering the front of the train is starting to move from G<SUB>x</SUB> covering all
  the way up to G<SUB>x+1</SUB>. L<SUB>region</SUB> is the length of this area.
  Assuming the train has a length of L, it can be divided to 3 main sections.
  - R<SUB>2</SUB> is the area that cover from G<SUB>x</SUB> to G<SUB>x+1</SUB>. Based on the method that supervised gradient targets were created, this area is
  always covering only one gradient region.
  - R<SUB>1</SUB>, that is marked with red colour in the example, can be associated with one or several gradients. Associated gradient(s) will remain same regardless of
  train movement.
  - R<SUB>0</SUB> is an area that covers the end of the train and can be associated with one or several gradients.

  When the train front is at G<SUB>x</SUB>, L<SUB>R0</SUB> = L<SUB>region</SUB> and L<SUB>R2</SUB> = 0.
  As the train moves, size of L<SUB>R2</SUB> increases and L<SUB>R0</SUB> decreases. Eventually when the train
  reaches G<SUB>x+1</SUB>, L<SUB>R0</SUB> = 0 and L<SUB>R2</SUB> = L<SUB>region</SUB>.

  Gradient for target at G<SUB>x</SUB> can be obtained by
  \f[ G = \frac{(L_{region} * Min(minimum\ gradient\ of\ R_0,\ gradient\ of\ R_2 ))\ +\ (Cumulative\ gradient\ length\ product\ of\ R_1)}{TrainLength }\f]
  \f[where\ Cumulative\ gradient\ length\ product\ of\ R_1 = (L_1* G_1 )+...+(L_n* G_n)\ for\ all\ gradients\ in\ R_1 \f]

  The function is described in the following process diagram:
  @image html abstract_target_calculation_calculate_gradient.png "Calculating Gradient"
  @image latex abstract_target_calculation_calculate_gradient.png "Calculating Gradient"

  See the following examples:
  \paragraph exampleMaStart MA start from scratch with no gradient targets
  MA start from scratch can contain a starting gradient and also gradient targets. In this example, there are no
  targets, so the whole region under the train will have the same gradient as the MA start gradient. This Ma
  gradient value is used by MessageHandler to update current gradient.
  @image html abstract_target_calculation_calculate_gradient_ma_start_scratch1.png "MA Start from scratch with no gradient targets"
  @image latex abstract_target_calculation_calculate_gradient_ma_start_scratch1.png "MA Start from scratch with no gradient targets"

  \paragraph exampleMaStartUnderTrain MA start from scratch with targets below the train area
  In this example, the first supervised target for G<SUB>1</SUB> is added at train front as it is under the train footprint.
  The G value is then calculated starting from G<SUB>1</SUB>S<SUB>1</SUB>.
  @image html abstract_target_calculation_calculate_gradient_ma_start_scratch2.png "MA Start from scratch with gradient targets below the train"
  @image latex abstract_target_calculation_calculate_gradient_ma_start_scratch2.png "MA Start from scratch with gradient targets below the train"


  \paragraph example3NonHelper Supervised Gradient Targets examples
  In this example, a supervised gradient is added at the gradient target position for G<SUB>1</SUB> and G<SUB>2</SUB>.
  As the distance between G<SUB>1</SUB> G<SUB>2</SUB> and G<SUB>2</SUB> G<SUB>3</SUB> is less than SL, it is not possible
  to add additional gradient targets.

  The gradient for G<SUB>3</SUB>S<SUB>1</SUB>, is the gradient for the region that is limited to G<SUB>3</SUB>S<SUB>1</SUB> and
  G<SUB>3</SUB>S<SUB>2</SUB>:
  This region is calculated based on the front of the train starting to move from G<SUB>3</SUB>S<SUB>1</SUB> covering all the
  way up to G<SUB>3</SUB>S<SUB>2</SUB>. This means that an area of the train that is marked with red colour will
  always be over G<SUB>1</SUB> and yellow colour will always be over G<SUB>2</SUB> region with the length of L<SUB>1</SUB> and L<SUB>2</SUB>
  respectively. L<SUB>3</SUB> is the length of the region between G<SUB>3</SUB>S<SUB>1</SUB> and G<SUB>3</SUB>S<SUB>2</SUB>,
  and will be equal to SL. Also L<SUB>3</SUB> = L<SUB>0</SUB>

  When the train moves from G<SUB>3</SUB>S<SUB>1</SUB> to G<SUB>3</SUB>S<SUB>2</SUB>, at different times an area of the train
  to the size of L<SUB>3</SUB> will be over G<SUB>0</SUB>, G<SUB>1</SUB> and G<SUB>3</SUB> regions (either partial
  or completely). For simplicity the minimum of these gradients will be used as the gradient for L<SUB>3</SUB>.
  This is equivalent of minimum of the gradient that is below L<SUB>3</SUB>  (L<SUB>3</SUB> cannot have more than
  one gradient) and minimum of all gradient that are below L<SUB>0</SUB>:

  \f[ G_{L3} = min(G_3,min(G_0,G_1))\f]
  \f[ Gradient\;for\;  G_3S_1 = \frac{(L_1* G_1 )+(L_2* G_2 )+(L_3*G_{L3})}{TrainLength }\f]
  @image html abstract_target_calculation_calculate_gradient_non_helper.png "Gradient Calculation for first supervised gradient target"
  @image latex abstract_target_calculation_calculate_gradient_non_helper.png "Gradient Calculation for first supervised gradient target"


  For supervised gradient target G<SUB>3</SUB>S<SUB>2</SUB>:
  \f[ G_{L4} = min(G_3,min(G_1))\f]
  \f[ Gradient\;for\; G_3S_2 = \frac{(L_1* G_1)+(L_2* G_2)+(L_3* G_3)+(L_4*G_{L4})}{TrainLength}\f]
  @image html abstract_target_calculation_calculate_gradient_helper1.png "Gradient Calculation for the second supervised gradient target"
  @image latex abstract_target_calculation_calculate_gradient_helper1.png "Gradient Calculation for the second supervised gradient target"


  For supervised gradient target G<SUB>3</SUB>S<SUB>3</SUB>:
  \f[ G_{L4} = min(G_3,min(G_1,G_2))\f]
  \f[ Gradient\;for\; G_3S_3 = \frac{(L_2* G_2)+(L_3* G_3)+(L_4*G_{L4})}{TrainLength}\f]
  @image html abstract_target_calculation_calculate_gradient_helper2.png "Gradient Calculation for the third supervised gradient target"
  @image latex abstract_target_calculation_calculate_gradient_helper2.png "Gradient Calculation for the third supervised gradient target"


  For supervised gradient target G<SUB>3</SUB>S<SUB>4</SUB>:
  \f[ G_{L2} = min(G_3,min(G_2))\f]
  \f[ Gradient\;for\; G_3S_4 = \frac{(L_1* G_3)+(L_2*G_{L2})}{TrainLength}\f]
  @image html abstract_target_calculation_calculate_gradient_helper4.png "Gradient Calculation for the forth supervised gradient target"
  @image latex abstract_target_calculation_calculate_gradient_helper4.png "Gradient Calculation for the forth supervised gradient target"


  For supervised gradient target G<SUB>3</SUB>S<SUB>5</SUB>:


  The last supervised target (if creating of the supervised gradient targets were not interrupted by a new target i.e. Number of
  created targets is equal to MaxSupvGradientTarget) will include the whole train within the region so the calculated
  gradient will be equal to the track gradient of the MA gradient target or the original target, if the distance from
  the supervised target G<SUB>3</SUB>S<SUB>5</SUB> to the original gradient target (G<SUB>3</SUB>) is more than or
  equal to TrainLength.

  \f[ if\; Distance\; to\; original\; Target \geq TrainLength \f]
  \f[ Gradient\;for\; G_3S_5 = Track\ Gradient\ of\ G_3 \f]
  @image html abstract_target_calculation_calculate_gradient_helper5.png "Gradient Calculation for the fifth supervised gradient target"
  @image latex abstract_target_calculation_calculate_gradient_helper5.png "Gradient Calculation for the fifth supervised gradient target"

  \subsection supv_speed_target Supervised Primary and Speed Target
  The Target Calculation component will create the supervised targets for each Primary Target (MA end and location borders) and
  Ceiling speed changes received in the MA.

  \subsubsection primarySupv Primary Target
  For a Primary Target, 4 supervised targets with the following type are created:
  - EBPrimaryTarget: The CS<SUB>EB</SUB> is 0 ahead of this target.
  - SBPrimaryTarget: The CS<SUB>SB</SUB> is 0 ahead of this target. 
  - SWPrimaryTarget: The CS<SUB>SW</SUB> is 0 ahead of this target.
  - FWPrimaryTarget: The CS<SUB>FW</SUB> is 0 ahead of this target.

  The positions of the supervised targets are calculated based on the distance it will take the train to accelerate due to gradient during the delay time and 
  the distance to stop the train once the brakes are applied. It is to be noted that this distance can include several gradients hence the calculation 
  of this distance is an iterative algorithm as shown below.
 
  @image html abstract_target_calculation_primary_target_supv.png "Primary Supervised target Calculation"
  @image latex abstract_target_calculation_primary_target_supv.png "Primary Supervised target Calculation"
    
  If the Primary target is a location border, the gradient in the location is fixed. In that case the iterative procedure is not needed. The distance for supervised targets for location borders is calculated using the fixed gradient in location according to calculation described in \ref calcAccDeaccDist.
  
  \paragraph primSupvCalc Calculation of Supervised Target Position
  The position of the supervised targets for a Primary target is calculated as follows. The target speed is 0 for Primary targets. 
  
  The calculation is implemented in BrakeCalculations::findSupvTargetOdo()
  - Step 1: Iterate over the supervised target list in reverse direction.
  - Step 2: Find the gradient supervised target or supervised target with valid gradient before primary target.
  - Step 3: If it is the first target, calculate the curve speed at the primary target based on gradient and delay. It is allowed for the curve speed to be negative. See \ref curvespdDecspd.
  - Step 4: If the gradient of the next target is different to the last processed target, calculate the curve speed at the last target using the deceleration curve speed at that target and the new gradient. See \ref curvespdDecspd for the calculation. Update the curve speed to the minimum of the value calculated using new gradient and old gradient. 
  - Step 5: If the curve speed is greater than target speed, exit the loop.
  - Step 6: Calculate the deceleration curve speed (\ref calcDecspd) and curve speed (\ref calcCurvSpd) at the supervised gradient target using the curve speed at the last processed target.
  - Step 7: Repeat Step 2 to 6 until the loop is terminated in Step 5 or all targets are processed.
  - Step 8: If all the supervised targets were processed, update the calculated curve speed based on current gradient.
  - Step 9: If the curve speed is less than target speed, use current gradient to calculate the distance before the last processed target where the curve speed is 0. See \ref calcCurvDist.
  - Step 10: If the curve speed is target speed, the position of last processed target is the position of supervised primary target.
  - Step 11: If the curve speed is greater than target speed, use gradient from last target and calculate the distance after the last processed target where the curve speed is 0. See \ref calcCurvDist.
  
  Below is an example with steps for the calculation for the position of supervised target for Primary target. The deceleration curve speed and the curve speed are indicated in the figures using blue and black circles respectively.
  
  The curve speed for the primary target is calculated using gradient G1.
  @image html abstract_target_calculation_primSupvCalc_1-Step3.png "First iteration - Step 3"
  @image latex abstract_target_calculation_primSupvCalc_1-Step3.png "First iteration - Step 3"
  
  As the curve speed is less than 0, proceed to Step 6 to calculate deceleration curve speed and curve speed at gradient supervised target.
  @image html abstract_target_calculation_primSupvCalc_1-Step6.png "First iteration - Step 6"
  @image latex abstract_target_calculation_primSupvCalc_1-Step6.png "First iteration - Step 6"
  
  Fetch the next target in from the list in reverse. Check and update the curve speed according to Step 4.
  In this case, G2 < G1.
  @image html abstract_target_calculation_primSupvCalc_2-Step4.png "Second iteration - Step 4"
  @image latex abstract_target_calculation_primSupvCalc_2-Step4.png "Second iteration - Step 4"
  
  As the curve speed is less than 0, proceed to Step 6 to calculate deceleration curve speed and curve speed at gradient supervised target.
  @image html abstract_target_calculation_primSupvCalc_2-Step6.png "Second iteration - Step 6"
  @image latex abstract_target_calculation_primSupvCalc_2-Step6.png "Second iteration - Step 6"
  
  Fetch the next target in from the list in reverse. Check and update the curve speed according to Step 4.
  In this case, G3 > G2.
  @image html abstract_target_calculation_primSupvCalc_3-Step4.png "Third iteration - Step 4"
  @image latex abstract_target_calculation_primSupvCalc_3-Step4.png "Third iteration - Step 4"
  
  As the curve speed is less than 0, proceed to Step 6 to calculate deceleration curve speed and curve speed at gradient supervised target.
  @image html abstract_target_calculation_primSupvCalc_2-Step6.png "Third iteration - Step 6"
  @image latex abstract_target_calculation_primSupvCalc_2-Step6.png "Third iteration - Step 6"
  
  Fetch the next target in from the list in reverse. Check and update the curve speed according to Step 4. 
  In this case, G4 < G3.
  @image html abstract_target_calculation_primSupvCalc_4-Step4.png "Fourth iteration - Step 4"
  @image latex abstract_target_calculation_primSupvCalc_4-Step4.png "Fourth iteration - Step 4"
  
  Exit the loop as the curve speed is greater than 0. Proceed to Step 11 as curve speed is greater than 0.
  @image html abstract_target_calculation_primSupvCalc_Step11.png "Step 11"
  @image latex abstract_target_calculation_primSupvCalc_Step11.png "Step 11"
  
  \subsubsection ceilSupv Ceiling Speed Target

  For a Ceiling Speed Target, the supervised targets are created based on if the speed change increases or decreases the ceiling speed.

  A Ceiling Speed Target which increases the ceiling speed, will result in 1 supervised target of type:
  - SpeedIncreaseTarget: This supervised target is placed at the position of the ceiling speed target. When this target is passed, the ceiling speed for
  FW, SW, SB and EB is increased if they are not restricted by any other speed target.

  A Ceiling Speed Target which decreases the ceiling speed to v<SUB>t</SUB>, will result in 4 supervised targets of type:
  - EBSpeedTarget: This supervised target is placed at <b>EBSpeedTargetOffset</b>
  before the Speed target. <b>t<SUB>delay</SUB> = t<SUB>eb</SUB> + t<SUB>ebr</SUB></b> for EBSpeedTargetOffset calculation.
  The <b>CS<SUB>EB</SUB> &le; v<SUB>t</SUB> + M<SUB>ce</SUB></b> from this target forward.

  - SBSpeedTarget: This supervised target is placed at <b>SBSpeedTargetOffset</b>
  before the Speed target. <b>t<SUB>delay</SUB> = t<SUB>eb</SUB> + t<SUB>ebr</SUB> + t<SUB>sb</SUB> + t<SUB>sbr</SUB></b> for SBSpeedTargetOffset calculation.
  The <b>CS<SUB>SB</SUB> &le; v<SUB>t</SUB> + M<SUB>cs</SUB></b> from this target forward.

  - SWSpeedTarget: This supervised target is placed at <b>SWSpeedTargetOffset</b>
  before the Speed target. <b>t<SUB>delay</SUB> = t<SUB>eb</SUB> + t<SUB>ebr</SUB> + t<SUB>sb</SUB> + t<SUB>sbr</SUB> + t<SUB>sw</SUB></b> for SWSpeedTargetOffset calculation.
  The <b>CS<SUB>SW</SUB> &le; v<SUB>t</SUB> + M<SUB>cw</SUB></b> from this target forward.

  - FWSpeedTarget: This supervised target is placed at <b>FWSpeedTargetOffset</b>
  before the Speed target. <b>t<SUB>delay</SUB> = t<SUB>eb</SUB> + t<SUB>ebr</SUB> + t<SUB>sb</SUB> + t<SUB>sbr</SUB> + t<SUB>sw</SUB> + t<SUB>fw</SUB></b> for FWSpeedTargetOffset calculation.
  The <b>CS<SUB>FW</SUB> &le; v<SUB>t</SUB></b> from this target forward.

  The following formula is used to calculate speed limit margins M<SUB>cw</SUB>, M<SUB>cs</SUB> and M<SUB>ce</SUB>:
  \f[ SpeedLimitMargin = Min(Max(\frac{v_t*PermilValue}{1000}, MinSpeedLimit),MaxSpeedLimit)\f]
  Where the following values are obtained from config component:
  - MaxSpeedLimit = MaxSpeedMarginWarn or MaxSpeedMarginSB or MaxSpeedMarginEB
  - MinSpeedLimit = MinSpeedMarginWarn or MinSpeedMarginSB or MinSpeedMarginEB
  - PermilValue = SpeedMarginWarn or SpeedMarginSB or SpeedMarginEB

  The following formula is used to calculate EBSpeedTargetOffset, SBSpeedTargetOffset, SWSpeedTargetOffset and FWSpeedTargetOffset:
  \f[ SpeedTargetOffset = (CS_x) * t_{delay} + \frac{(CS_x - G * t_{delay})^2 - CS_x^2}{2*(r+G)} - \frac{1}{2} * G * t_{delay}^2\f]

  where
  \f[ CS_x = CS_{EB},\ CS_{SB},\ CS_{SW}\ or\ CS_{FW}\f]
  \f[ t_{delay} = first\ warning,second\ warning,\ SB\ or\ EB\ delay\f]
  \f[ G = Worst\ gradient\ from\ MA\ start\ to\ speed\ target \f]

  The SpeedTargetOffset includes the distance travelled by the train due to acceleration from gradient and the distance to reduce the speed to the target speed.

  The following figure shows the positions of the supervised targets for ceiling speed and primary targets. The figure also shows how the ceiling speeds for FW, SW, SB and EB change based on the supervised targets.
  @image html abstract_target_calculation_add_cs_supv_targets.png "Ceiling speed and Primary Supervised targets"
  @image latex abstract_target_calculation_add_cs_supv_targets.png "Ceiling speed and Primary Supervised targets"

  \subsection UpdatingCSGradient Update Ceiling Speed and Gradient
  The functions to add supervised gradient, supervised ceiling speed and supervised primary target has added all the respective supervised targets.


  The next step is to update the following parameters for each supervised target:
  - Ceiling speed (CS)
  - First Warning ceiling speed (CS<SUB>FW</SUB>)
  - Second Warning ceiling speed (CS<SUB>SW</SUB>)
  - Service Brake ceiling speed (CS<SUB>SB</SUB>)
  - Emergency Brake ceiling speed (CS<SUB>EB</SUB>)
  - Gradient (G)

  The gradient for supervised gradient targets will remain the same as the calculated value when the target was added.

  The first step is to find the value of these parameters at the start of MA.
  - For an MA from scratch: The values are calculated based on the speed and the gradient at the start of MA.
  - For an MA extension: the values are the same as the last supervised target in the previous MA before the release speed SB position. Note that the primary target of the previous MA and hence the supervised targets for that primary target are already deleted. If there were no supervised targets, the value is the same as the current value of the parameters.

  The function then finds the next ceiling speed target (NextMATarget<SUB>CS</SUB>) ahead of MA start position. If there is no ceiling speed target, then Primary target (MA end) is used. This is used to update the current ceiling speed value.
  Also 4 vectors one for each FW, SW, SB and EB are maintained, which contain the pointer to the ceiling speed target. These are used when a speed increase supervised target is found. The speeds are increased only if they are not restricted by any other target.

  The function iterates over all targets in supervised list starting from MA start and keeps record of the current value of above mentioned parameters.
  The values of these parameters are updated based on the supervised target in the iteration as described below:
  - Any supervised target: If supervised target is ahead of NextMATarget<SUB>CS</SUB>, <b>CS = ceiling speed of NextMATarget<SUB>CS</SUB></b>. Remove the NextMATarget<SUB>CS</SUB> pointer from the vectors of FW, SW, SB and EB. Find the next NextMATarget<SUB>CS</SUB>.
  - Supervised gradient target: <b>G = gradient value of the target</b>.
  - Supervised FW speed target: <b>CS<SUB>FW</SUB> = MIN (v<SUB>t</SUB>, CS<SUB>FW</SUB>)</b>, where v<SUB>t</SUB> is parent target ceiling speed. Add the parent target pointer to FW vector.
  - Supervised SW speed target: <b>CS<SUB>SW</SUB> = MIN (v<SUB>t</SUB> + M<SUB>cw</SUB>, CS<SUB>SW</SUB>)</b>, where v<SUB>t</SUB> is parent target ceiling speed. Add the parent target pointer to SW vector.
  - Supervised SB speed target: <b>CS<SUB>SB</SUB> = MIN (v<SUB>t</SUB> + M<SUB>cs</SUB>, CS<SUB>SB</SUB>)</b>, where v<SUB>t</SUB> is parent target ceiling speed. Add the parent target pointer to SB vector.
  - Supervised EB speed target: <b>CS<SUB>EB</SUB> = MIN (v<SUB>t</SUB> + M<SUB>ce</SUB>, CS<SUB>EB</SUB>)</b>, where v<SUB>t</SUB> is parent target ceiling speed. Add the parent target pointer to EB vector.
  - Supervised FW Primary target: <b>CS<SUB>FW</SUB> = 0</b>
  - Supervised SW Primary target: <b>CS<SUB>FW</SUB> = CS<SUB>SW</SUB> = 0</b>
  - Supervised SB Primary target: <b>CS<SUB>FW</SUB> = CS<SUB>SW</SUB> = CS<SUB>SB</SUB> = 0</b>
  - Supervised EB Primary target: <b>CS<SUB>FW</SUB> = CS<SUB>SW</SUB> = CS<SUB>SB</SUB> = CS<SUB>EB</SUB> = 0</b>
  - Supervised Speed Increase target:
      - <b>CS<SUB>FW</SUB> = MIN (v<SUB>t</SUB>, minimum CS in FW vector)</b>
      - <b>CS<SUB>SW</SUB> = MIN (v<SUB>t</SUB>, minimum CS in SW vector) + M<SUB>cw</SUB></b>
      - <b>CS<SUB>SB</SUB> = MIN (v<SUB>t</SUB>, minimum CS in SB vector) + M<SUB>cs</SUB></b>
      - <b>CS<SUB>EB</SUB> = MIN (v<SUB>t</SUB>, minimum CS in EB vector) + M<SUB>ce</SUB></b>
  - If the supervised target has a valid gradient <b>G = gradient value of the target</b>. This is for the case if another supervised target is placed at the same place as a gradient supervised target. In that case the gradient value is updated for the supervised target and gradient supervised target is removed.

  Once the values of the parameters are updated based on the supervised target in the iteration, the updated values are stored in the supervised target.

  The figure below shows how the various ceiling speeds change with respect to targets.
  @image html abstract_target_calculation_calc_cs.png "FW, SW, SB and EB Ceiling speeds"
  @image latex abstract_target_calculation_calc_cs.png "FW, SW, SB and EB Ceiling speeds"

  At STP<SUB>fw2</SUB>, the target FW speed is CS2. But the current ceiling speed is less than CS2, so the FW ceiling speed at the target is same as current FW ceiling speed.

  At STP<SUB>sw2</SUB>, the target SW speed is CS2 + M<SUB>cw</SUB>, but this is more than the current SW ceiling speed, so the SW ceiling is same as current SW ceiling speed.

  At STP<SUB>speedIncrease</SUB> the ceiling speed is increased to CS1. The SB and EB ceiling speed are increased based on CS1. But FW and SW are limited by CS2 as the FW and SW supervised target of CS2 has passed. So the maximum value FW and SW ceiling speed can have is based on CS2.

  At STP<SUB>sb2</SUB>, the SB ceiling speed is limited by CS2 target, and is reduced to CS2 + M<SUB>cs</SUB>.

  At STP<SUB>gradient</SUB>, the FW, SW, SB and EB ceiling speed remain the same as the previous supervised target. The current gradient is changed to G1.

  At STP<SUB>fw3</SUB>, the target FW speed is limited by CS3 which is less than CS2 so the FW ceiling speed is reduced. Note that at this target the FW ceiling speed is limited by CS3, SW and SB ceiling speed are limited by CS2 and EB ceiling speed it not limited by any speed target.

  At STP<SUB>eb2</SUB>, the EB ceiling speed is limited by CS2 target, and is reduced to CS2 + M<SUB>ce</SUB>. Note that the FW ceiling speed at this point is limited by the CS3 target.

  At STP<SUB>sw3</SUB>, STP<SUB>sb3</SUB> and STP<SUB>eb3</SUB>, the SW, SB and EB ceiling speed become limited by CS3 and gets reduced.
  
  At STP<SUB>fwp</SUB>, the FW ceiling speed drops to 0 as this is the FW target point for primary target. Note that the SW, SB and EB ceiling speed are not changed.
  
  At STP<SUB>swp</SUB>, the FW and SW ceiling speed drop to 0 as this is the SW target point for primary target. Note that the SB and EB ceiling speed are not changed.
    
  At STP<SUB>sbp</SUB>, the FW, SW and SB ceiling speed drop to 0 as this is the SB target point for primary target. Note that the EB ceiling speed is not changed.

  At STP<SUB>ebp</SUB>, the EB ceiling speed is reduced to 0. The train is not allowed to exceed this point.

  \subsection UpdateCurveSpeed Update Curve Speeds
  When train receives new MA and after all the supervised targets are added and the ceiling speeds calculated,
  AbstractTargetCalculation needs to calculate the curve speeds for the supervised targets.
  The following curve speeds are calculated for each supervised target:
  - EB Speed : Speed of Emergency brake curve at this supervised target with ceiling speed of CS<SUB>EB</SUB>.
  - SB Speed : Speed of Service brake curve at this supervised target with ceiling speed of CS<SUB>SB</SUB>.
  - Second Warning Speed : Speed of Second Warning curve at this supervised target with ceiling speed of CS<SUB>SW</SUB>.
  - First Warning Speed : Speed of First Warning curve at this supervised target with ceiling speed of CS<SUB>FW</SUB>.

  The calculation starts from the Supervised EB Primary target which is the last target in the supervised target list
  and all the curve speeds are set to zero at the target.
  
  @image html abstract_target_calculation_update_curve_speed.png "Update Curve Speeds"
  @image latex abstract_target_calculation_update_curve_speed.png "Update Curve Speeds"

  The function then reverse iterates the supervised target list and the curve speeds at a supervised target is calculated
  using the curve speeds of the last processed supervised target using the formula described in \ref calcCurvSpd.
  The deceleration curve speed at the target is calculated using the deceleration curve speed at the last processed supervised target according to \ref calcDecspd.

  The curve speed should never exceed the corresponding ceiling speed. Hence:
  \f[ v_{curve} = min(v_{calc}, v_c) \f]
  where
  \f[ v_c= CS_{EB}, CS_{SB}, CS_{SW}\ or\ CS_{FW}\f]
  
  If the gradient of the current target is different to the gradient of last processed target, the curve speed at the last processed target is updated to the minimum of the value calculated using the gradient of the current target and the gradient of the last processed target. The curve speed at the target is calculated using the deceleration curve speed at the target as described in \ref curvespdDecspd.

  It is to be noted that curve speeds are calculated for all new supervised targets and as well as recalculated
  for the previous supervised targets.

  The figure below shows the curve speeds at the supervised targets. Note that the curve speed is set to
  minimum of curve speed and the corresponding ceiling speed. The curve to previous supervised target is then calculated from
  this speed.
  @image html abstract_target_calculation_calc_curve_speeds.png "FW, SW, SB and EB Curve speeds"
  @image latex abstract_target_calculation_calc_curve_speeds.png "FW, SW, SB and EB Curve speeds"
  
  \subsection releaseSpEBPos Release Speed EB Position
  This is the position for a Primary target ahead of which movement is not allowed at the release speed. If the EB is applied at this point, the train will accelerate due to gradient from the release speed during the delay and then stop with emergency brake application. 
  The position is calculated when iterating over targets to calculate curve speed as follows:
  - If ATP mode is Location, this is the same as the supervised EB Primary target as release speed is not supervised in Location mode.
  - If the EB curve speed of the last processed target was less than release speed and EB curve speed of the current target is greater than or equal to release speed, calculate the distance from the last target where the EB curve speed equals release speed. See \ref calcCurvDist.
  - If all targets are processed and release speed EB position is not found, calculate the distance from the last target where the EB curve speed equals release speed. See \ref calcCurvDist.

  \subsection releaseSpSBPos Release Speed SB Position
  This is the position for a Primary target ahead of which movement is not allowed at the release speed. If the SB is applied at this point, the train will accelerate due to gradient from the release speed during the delay and then stop with emergency brake application. 
  The position is calculated when iterating over targets to calculate curve speed as follows:
  - If ATP mode is Location, this is the same as the supervised SB Primary target as release speed is not supervised in Location mode.
  - If the SB curve speed of the last processed target was less than release speed and SB curve speed of the current target is greater than or equal to release speed, calculate the distance from the last target where the SB curve speed equals release speed. See \ref calcCurvDist.
  - If all targets are processed and release speed SB position is not found, calculate the distance from the last target where the SB curve speed equals release speed. See \ref calcCurvDist.
  
  \subsection smSpeedRest Safety Margin Change Speed Restriction
  If the Movement authority includes a Track Data Item target of type Safety Margin Change such that the Safety Margin at the end of MA is less than the Safety Margin at any other point in the MA, the ATP needs to restrict the speed of the curves to release speed at a point before the FW curve crosses the release speed. This point is placed at a distance which is equal to the difference between max safety margin in MA and the safety margin at MA end. 
  
  To implement this, a supervised target of type SMSpeedRestrictionTarget is placed at the calculated point and the curve speeds and CS at that point are set based on the release speed. The difference between the maximum Safety Margin in the MA and the Safety Margin at MA end is calculated during the iteration to add supervised primary and ceiling speed targets in \ref AbstractTargetCalculation::addSupvSpeedAndPrimaryTargets.
  
  The calculation of the position to place the SMSpeedRestrictionTarget is done when calculating the curve speeds for targets as follows:
  - If the maximum Safety Margin in the MA is the same as Safety Margin at MA end, no SMSpeedRestrictionTarget is created.
  - If ATP mode is Location, no SMSpeedRestrictionTarget is created.
  - If the FW curve speed of the last processed target was less than release speed and FW curve speed of the current target is greater than or equal to release speed then:
    - calculate the distance from the last target where the FW curve speed equals release speed. See \ref calcCurvDist. 
    - Create supervised target of type SMSpeedRestrictionTarget at the calculated position
    - Set the FW, SW, SB and EB curve speed and ceiling speed to release speed plus the margin.
    - Update the deceleration curve speed at the current supervised target based on the newly created target.
  - If all targets are processed and position is not found, 
    - Calculate the distance from the last target where the FW curve speed equals release speed. See \ref calcCurvDist.
    - Create supervised target of type SMSpeedRestrictionTarget at the calculated position
    - Set the FW, SW, SB and EB curve speed and ceiling speed to release speed plus the margin.

  \subsection resTarget Most Restrictive Target
  The most restrictive target is a supervised target closest to train front for which the first warning curve
  speed is same as the first warning ceiling speed.
  The curve speeds at this supervised target are not restricted by other target but by the ceiling speed at the target.

  The most restrictive target can never be a supervised target of type:
  - Speed increase supervised target
  - Gradient supervised target

  \subsection curveCalcFn Curve Calculation Utilities
  The following are the utilities used to calculate the curve speeds or deceleration curve speeds. 
  They are used to calculate curve speeds at targets or finding the positions of supervised targets. 
  
  \subsubsection curvespdDecspd Calculate Allowed Curve Speed using Deceleration Curve Speed
  This function is used to calculate the FW, SW, SB or EB curve speed from the deceleration curve speed at the position of the deceleration curve speed.
  
  @image html abstract_target_calculation_curveCalcFn_curvespdDecspd.png "Allowed Curve speed using Deceleration curve speed"
  @image latex abstract_target_calculation_curveCalcFn_curvespdDecspd.png "Allowed Curve speed using Deceleration curve speed"
  
  This is calculated by equating the distance travelled during the delay due to gradient with the distance it takes to reduce the speed from deceleration curve.

  \f[ v_c * t_{delay} - \frac{1}{2} * G * t_{delay}^2 = \frac{v_d^2 - (v_{c} - G * t_{delay})^2}{2*r_m}\f]
  Solving we get:
  \f[ v_{c} = - t_{delay}*(r_m - G) + \sqrt{v_d^2 + t_{delay}^2 * r_m * (r_m - G)}\f]
  where
  \f[ r_m = r + G \f]
  \f[ v_d = deceleration\ curve\ speed\f]
  \f[ t_{delay} = first\ warning,second\ warning,\ SB\ or\ EB\ delay\f]
  \f[ v_c = calculated\ curve\ speed\f]
  
  \subsubsection calcDecspd Calculate Deceleration Curve Speed at a distance from Curve Speed at Target
  This function is used to calculate the deceleration curve speed at a distance from the target using the curve speed at the target.
  
  @image html abstract_target_calculation_curveCalcFn_calcDecspd.png "Deceleration curve speed at a distance from curve speed at target"
  @image latex abstract_target_calculation_curveCalcFn_calcDecspd.png "Deceleration curve speed at a distance from curve speed at target"
  
  It is calculated by subtracting the distance for the deceleration curve to target with the distance travelled during delay from the target speed and equating it to the distance to target.
  \f[ \frac{v_d^2 -(v_{c} - G * t_{delay})^2}{2*r_m} - (v_c * t_{delay} - \frac{1}{2} * G * t_{delay}^2) = d_t\f]
  Solving we get:
  \f[ v_{d} = \sqrt{v_c^2 + 2* t_{delay} * d_t + t_{delay} * (r_m - G) * (2 * v_c - t_{delay} * G)}\f]
  where
  \f[ r_m = r + G \f]
  \f[ v_d = calculated\ deceleration\ curve\ speed\f]
  \f[ t_{delay} = first\ warning,second\ warning,\ SB\ or\ EB\ delay\f]
  \f[ v_c = curve\ speed\ at\ target\f]
  \f[ d_t = distance\ to\ supervised\ target\ position\f]
  
  \subsubsection calcCurvSpd Calculate Curve speed at a distance from Curve Speed at Target
  This function is used to calculate the curve speed at a distance from the target using the curve speed at the target.
  
  @image html abstract_target_calculation_curveCalcFn_calcCurvSpd.png "Calculate Curve speed at a distance from curve speed at target"
  @image latex abstract_target_calculation_curveCalcFn_calcCurvSpd.png "Calculate Curve speed at a distance from curve speed at target"
  
  Equating the distance to target to the distance of deceleration curve and the distance travelled during delay at target speed and at the calculated speed, we get:  
  \f[ (v_{calc} - v_t) * t_{delay} + \frac{(v_{calc} - G * t_{delay})^2 - (v_t - G * t_{delay})^2}{2*r_m} = d_{t}\f]

  Solving for v<SUB>calc</SUB> we get:
  \f[ v_{calc} = \sqrt{w} - v_2 \f]
  Where:
  \f[ r_m = r + G \f]
  \f[ v_2= t_{delay} (r_m - G) \f]
  \f[ w = (v_t - v_1)^2 - v_1^2 + v_2^2 + 2 r_m (d_{t}+ d_1) \f]\f[ v_1 = t_{delay} G \f]
  \f[ d_1 = t_{delay} v_t \f]
  \f[ r_m = r + G \f]
  \f[ v_t = first\ warning, second\ warning, SB\ or\ EB\ curve\ speed\ at\ next\ supervised\ target \f]
  \f[ t_{delay} = first\ warning,second\ warning,\ SB\ or\ EB\ delay\f]
  \f[ d_t = distance\ from\ supervised\ target\ position\ to\ next\ supervised\ target \f]
  
  \subsubsection calcCurvDist Calculate Curve Distance
  This function is used to calculate the curve distance based on the curve speed at target and current speed. 
  It is calculated by adding the deceleration curve distance from current speed to target speed and the difference of distance travelled during delay at current speed and target speed.
  \f[ d_{t} = (v_{a} - v_t) * t_{delay} + \frac{(v_{a} - G * t_{delay})^2 - (v_t - G * t_{delay})^2}{2*r_m}\f]
  
  Where:
  \f[ r_m = r + G \f]
  \f[ v_t = first\ warning, second\ warning, SB\ or\ EB\ curve\ speed\ at\ next\ supervised\ target \f]
  \f[ t_{delay} = first\ warning,second\ warning,\ SB\ or\ EB\ delay\f]
  \f[ d_t = distance\ from\ supervised\ target\ position\f]
  
  \subsubsection calcAccDeaccDist Calculate Acceleration Deceleration Distance
  This function calculates the distance travelled from current speed to a deceleration curve speed. It is calculated by adding the distance travelled during delay at current speed and then the distance travelled when the brakes are applied to reduce the speed to deceleration curve speed.
  
  \f[ d_{t} = v_{a} * t_{delay} - \frac{1}{2} * G * t_{delay}^2 + \frac{(v_{a} - G * t_{delay})^2 - v_t^2}{2*r_m}\f]
  
  Where:
  \f[ r_m = r + G \f]
  \f[ v_t = deceleration\ curve\ speed\ at\ the\ target \f]
  \f[ t_{delay} = first\ warning,second\ warning,\ SB\ or\ EB\ delay\f]
  \f[ d_t = distance\ from\ target\ position\f]
  
  \section ClassDiagram Class Diagram
  @image html abstract_target_calculation_class_diagram.png "AbstractTargetCalculation Class Diagram"
  @image latex abstract_target_calculation_class_diagram.png "AbstractTargetCalculation Class Diagram"

  \section Diagnostics Diagnostics
  \subsection Console Console-commands
  NA

  \subsection Analyze Analyze
  References to variables to be accessed by an external Analyzer tool are prepared at initialization.
  Some of the statistics values may be of such interest.
  
  Variable         | Unit | Description
  ---------------- | ---- | -----------------------
  calcCeilingSpeed | cm/s | Mode dependant ceiling speed.

  \section CoreAdaptation Core / Adaptation
  The Target Calculation component is split in a core and one adaptation part. The adaptation implements the instantiation of the core class. The adaptation can override the virtual functions in order to create a different behaviour.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component.
  
  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP Core.
  
  The requirements relevant for this component are:

  Req          | Chapter                     | Function
  ------------ | --------------------------- | --------
  AOS 3292 S   | \ref smSpeedRest            | AbstractTargetCalculation::updateCurveSpeedsForTargets
  AOS 674 S    | \ref UpdateCurveSpeed       | AbstractTargetCalculation::updateCurveSpeedsForTargets
  AOS 3190 S   | \ref releaseSpSBPos         | AbstractTargetCalculation::updateCurveSpeedsForTargets
  AOS 3191 S   | \ref releaseSpEBPos         | AbstractTargetCalculation::updateCurveSpeedsForTargets
  AOS 2122     | \ref calculateCeilingSpeed  | AbstractTargetCalculation::calcModeDependentCeilingSpeed

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP Core.
  Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].
  
  */
}
