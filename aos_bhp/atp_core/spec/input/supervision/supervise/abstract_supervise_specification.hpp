namespace ATP::Supv
{
  /**
  \if AsMainPage
  \mainpage Abstract Supervise Component Specification
  @anchor sup
  \endif

  \ifnot AsMainPage
  \class AbstractSupervise
  \endif

  \section Purpose Purpose
  This document specifies the software design for the Abstract Supervise component.

  \subsection supAbbreviations Abbreviations

  Term             | Definition
  ---------------- | ------------------------------------------------------------------
  V<SUB>p</SUB>    | Permitted Speed
  t<SUB>fw</SUB>   | first warning to second warning curve delay (Obtained from config)
  t<SUB>sw</SUB>   | second warning to service brake order delay (Obtained from config)
  t<SUB>sb</SUB>   | SB order to EB order delay (Obtained from config)
  t<SUB>eb</SUB>   | Effective EB delay (Obtained from config)
  t<SUB>sbr</SUB>  | Brake reaction time for service brake.
  t<SUB>ebr</SUB>  | Brake reaction time for emergency brake.
  M<SUB>cw</SUB>   | Warning speed margin
  M<SUB>cs</SUB>   | Service brake speed margin
  M<SUB>ce</SUB>   | Emergency brake speed margin
  CS<SUB>EB</SUB>  | Ceiling speed for emergency brake application.
  CS<SUB>SB</SUB>  | Ceiling speed for service brake application.
  CS<SUB>SW</SUB>  | Ceiling speed for second warning.
  CS<SUB>FW</SUB>  | Ceiling speed for first warning.
  r                | Current brakeability cm/s^2
  r<SUB>a</SUB>    | Current acceleration cm/s^2
  v<SUB>a</SUB>    | Current speed cm/s
  v<SUB>max</SUB>  | Current filtered maximum speed from COD cm/s
  v<SUB>c</SUB>    | Ceiling speed cm/s
  G                | Current gradient cm/s^2
  v<SUB>fw</SUB>   | First warning curve speed
  v<SUB>sw</SUB>   | Second warning curve speed
  v<SUB>sb</SUB>   | Service brake curve speed
  v<SUB>eb</SUB>   | Emergency brake curve speed
  v<SUB>rs</SUB>   | Release speed
  STP<SUB>x</SUB>  | Supervised Target Point of type 'x'


  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality
  AbstractSupervise is responsible for the supervision of train movement to ensure the train never violates the Movement Authority.
  The ATP intervenes to prevent a violation and ensure safe operation. The component does the following:
  - \ref ceilingSpeed
  - \ref targetSupervision
  - \ref standstillsup
  - Supervise \ref releasespeed
  - \ref timsSup
  - \ref RollAwaySupv
  - \ref RevSupv
  - \ref LocationSupv

  AbstractSupervise also calculates the permitted speed, target speed and the remaining distance to the target point to be shown to the driver.
  The AbstractSupervise class is derived from the base class ProcComp.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  The AbstractSupervise shall be inherited by the adaptation layer.
  This component is dependent on the following other components 
  - Position : for the train leading position. 
  - Targets : for the targets to supervise
  - Odometry : for the odometer value, speed and acceleration. 
  - TargetCalculation : for the calculated ceiling and the position for brake application at MA end.
  - TIMS : for TIMS status.
  - Message Handler: to get if MA is received and if its MA from scratch.

  Other components have dependencies to this component because they use its public types
  and methods, see \ref AbstractSupervise Class Reference.

  \latexonly \newpage \endlatexonly

  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  During initiation the abstract class shall initialize internal variables.
  No dynamic memory allocation is needed.
  The init function also registers the variables for the Analyzer interface and initializes the cross compare.

  \subsection ModeDependentOperation Mode dependent operation
  
  The table below indicates the supervisions that are enabled (indicated by "X") in an ATP mode.

  ATP mode               |Ceiling Speed Supv. | Target Supv. |Standstill Supv. | Release Speed Supv. | Reverse Supv. | Roll Away Supv. | Location Supv. |
  -----------------------|--------------|--------------|--------------|--------------|--------------|--------------|--------------|
  Power Up Mode          |              |              | X            |              |              | X            |              |
  Configuration Mode     |              |              | X            |              |              | X            |              |
  Registration Mode      |              |              | X            |              |              | X            |              |
  BaliseSearch Mode      | X            |X             | X            | X            | X            | X            |              |
  Normal Mode            | X            |X             | X            |X             | X            | X            |              |
  Shunting Mode          | X            |              | X            |X             |              | X            |              |
  Location Mode          | X            |X             | X            |              |              | X            | X            |
  Yard Mode              | X            |              | X            |X             |              | X            |              |
  Unregistered Mode      |              |              | X            |              |              | X            |              |
  Powering Down Mode     |              |              | X            |              |              | X            |              |
  SafetyHalt Mode        |              |              | X            |              |              | X            |              |
  Sleeping Mode          |              |              | X (unless brakes are inhibited)            |              |              |               |              |
  StaffResponsible Mode  | X            |X             | X            |X             | X            | X            |              |
  Shunting Route Mode    | X            |X             | X            |X             | X            | X            |              |
  Possession Mode        | X            |              | X            |X             |              | X            |              |
  Split Mode             | X            |X             | X            |X             | X            | X            |              |
  Join Mode              | X            |X             | X            |X             | X            | X            |              |
  SafeBrakeToStop Mode   | X            |X             | X            |X             |              | X            |              |

  The ATP mode state ATPModeUndefined is an invalid mode so a Safety Halt event is triggered.


  \subsection Scheduling Scheduling

  The component has a function (inherited from the base-class ProcComponent ), run(), that must be called
  each execution-cycle.

  \subsubsection run run()

  The run() method takes the following actions:
  - Decide based on the ATP mode whether to supervise ceiling speed, targets, standstill, Release speed and/or location borders
  - Supervise ceiling speed 
  - Supervise targets
  - Prepare information for DMI
  - Verify Brakeability is better than the gradient in the MA
  - Supervise Standstill
  - Supervise TIMS
  - Supervise Reverse and Roll away movement
  - Supervise location borders

  @image html abstract_supervise_run.png "run"
  @image latex abstract_supervise_run.png "run"
  

  \subsection findClosestSupervisableTarget Get Next supervise target
  The method finds the next supervised target ahead of the train front in the direction of the Movement authority. 
  The Target Calculation component pre calculates the curve speeds at each target.
  So during runtime, the supervise only needs to look at the next supervised target to calculate the curve speeds at the current position.

  One short buzzer beep (Buzzer = BuzzerTypeOneBeep) is issued when a new target is available and the target list was empty previously.
  This means that the train was idling and now an MA is received.

  \subsection ceilingSpeed Ceiling speed supervision
  The ceiling speed is continuously monitored in all ATP operation modes. Four different speeds are supervised:
  - Ceiling speed or First Warning Speed
  - Second Warning Speed
  - Service Brake Speed
  - Emergency Brake Speed

  \subsubsection csCalc Calculation

  The Abstract Supervise component will fetch the current ceiling speed from the Target Calculation component.
  If the ATP mode allows Release speed supervision, the ceiling speed to supervise is updated as:
  \f[ v_c = Max(v_c,Release\ Speed)\f]
  where,
  Release speed is a configurable value which corresponds to the minimum allowed ceiling speed of the train in modes where Release speed is supervised.

  The Warning and Intervention speeds are calculated as follows:

  - First Warning Speed: v<SUB>c</SUB>
  - Second Warning Speed: v<SUB>c</SUB> + M<SUB>cw</SUB>
  - Service brake speed: v<SUB>c</SUB> + M<SUB>cs</SUB>
  - Emergency brake speed: v<SUB>c</SUB> + M<SUB>ce</SUB> 

  The following formula is used to calculate Speed limit margins M<SUB>cw</SUB>, M<SUB>cs</SUB> and M<SUB>ce</SUB>:
  \f[ SpeedLimitMargin = min(Max(\frac{v_c*PermilValue}{1000}, MinSpeedLimit),MaxSpeedLimit)\f]

  Where the following values are obtained from config component:
  - MaxSpeedLimit = MaxSpeedMarginWarn or MaxSpeedMarginSB or MaxSpeedMarginEB
  - MinSpeedLimit = MinSpeedMarginWarn or MinSpeedMarginSB or MinSpeedMarginEB
  - PermilValue = SpeedMarginWarn or SpeedMarginSB or SpeedMarginEB 

  \subsubsection csSupv Supervision

  The ceiling speed supervision evaluates the ATP Intervention and ATP Warning condition on each cycle. This means that the
  ATP Intervention and ATP Warning shall be cleared in the beginning of each run and evaluated again.
  The ceiling speed supervision will sound the buzzer or issue a brake event if the speed is violated as described in the following sections.
  It is to be noted that the buzzer or brake event might remain active even though the condition which caused it is no longer valid.
  The sections below describe separately the conditions to clear the Buzzer or brake event.
  The status ATPWarning and ATPIntervention are set to false each cycle and then updated based on conditions below.

  If the ceiling speed is lower than the Release speed, the Release speed supersedes the ceiling speeds, meaning the maximum of ceiling speed and
  Release speed will be supervised.

  \paragraph FWLimit First Warning Limit  
  - Entry condition:\f[v_a \geq v_c\f]
  - Entry Actions:
      - Buzzer = BuzzerTypeOneBeep
      - ATPWarning = true
  - Clear condition: \f[v_a < v_c\f]
  - Clear action: Deactivate the buzzer.
  

  \paragraph SwLimit Second Warning limit
  - Entry condition:\f[v_a \geq v_c+M_{cw}\f]
  - Entry Actions:
      - Buzzer = BuzzerTypeConstantBeep
      - ATPWarning = true
  - Clear condition: \f[v_a < v_c\f]
  - Clear action: Deactivate the buzzer.
  

  \paragraph SbLimit Service brake limit
  - Entry conditions: \f[v_a \geq v_c+M_{cs}\f]
  - Entry Actions:
      - Raise a service brake event for "Service brake on ceiling speed supervision"
      - ATPIntervention = true
  - Clear condition: \f[v_a < v_c\f]
  - Clear action:
      - Stop raising the service brake event. Service brake can be released by the Driver once the event is cleared.
  

  \paragraph EbLimit Emergency brake limit
  - Entry conditions: \f[v_a \geq v_c+M_{ce}\f]
  - Entry Actions:
      - Raise an emergency brake event for "Emergency brake on ceiling speed supervision"
      - ATPIntervention = true
  - Clear condition:\f[v_a < v_c\f]
  - Clear action:
      - Stop raising emergency brake event. Emergency brake can only be released at standstill.

      
  \subsection targetSupervision Target Supervision

  When there is change in the target list, the Target Calculation component will create supervised targets for all targets in the MA.
  For each supervised target, the Target Calculation component will calculate the first warning, second warning, service brake (SB) and emergency brake (EB) speed at the supervised target odometer value.
  The Abstract Supervise component, supervises the closest supervised target's brake curve.

  The AbstractSupervise will calculate the first warning, second warning, SB and EB speed at the current odometer position based on
  the first warning, second warning, service brake (SB) and emergency brake (EB) speed at the supervised target odometer position.

  If the calculated speeds are lower than the Release speed they are increased to be at least Release speed unless in ATP mode Location.
  The calculated speeds are then compared to the current speed to decide target supervision zone.

  The target supervision is divided into different zones:
  - Zone A: Default indication
  - Zone B: First Warning zone
  - Zone C: Second Warning zone
  - Zone D: Service brake applied zone
  - Zone E: Emergency brake applied zone

  Each zone is delimited with a curve.
  - curve ab: First warning curve
  - curve bc: Second warning curve
  - curve cd: Service brake curve
  - curve de: Emergency brake curve 

  The following figures depict the brake curves for targets with zero speed with positive and negative gradients. 
  For the positive gradient all the curves point to the Primary target as the train does not accelerate from the standstill position.
  
  If the Release speed is supervised, the FW and SW curve speeds cannot be lower than the Release speed and Release speed plus warning margin.
  The curve speed for SB and EB drops to 0 at the point where the SB or EB curve speed becomes equal to Release speed. 
  These points are referred as Release speed SB (R<SUB>sb</SUB>) and Release speed EB (R<SUB>eb</SUB>) positions respectively.

  @image html abstract_supervise_brake_curve_target_speed_zero.png  "Brake and warning curves, with a target speed equal to zero for gradient < 0"
  @image latex abstract_supervise_brake_curve_target_speed_zero.png "Brake and warning curves, with a target speed equal to zero for gradient < 0"
  
  @image html abstract_supervise_brake_curve_target_speed_zero_g1.png  "Brake and warning curves, with a target speed equal to zero for gradient >= 0"
  @image latex abstract_supervise_brake_curve_target_speed_zero)g1.png "Brake and warning curves, with a target speed equal to zero for gradient >= 0"
  
  The following figure depict the brake curves for targets with speed greater than zero
  @image html abstract_supervise_brake_curve_target_speed_non_zero.png  "Brake and warning curves, with a target speed greater than zero"
  @image latex abstract_supervise_brake_curve_target_speed_non_zero.png "Brake and warning curves, with a target speed greater than zero"
  

  @image html abstract_supervise_brake_curve_st_pt.png  "Brake and warning curves, with a ceiling speed target and primary target"
  @image latex abstract_supervise_brake_curve_st_pt.png "Brake and warning curves, with a ceiling speed target and primary target"
  

  \subsubsection tsCalc Calculation
  When Target supervision is in progress, the closest supervised target is used to calculate the following speeds at the current position:
  - v<SUB>eb</SUB> based on Emergency brake curve
  - v<SUB>sb</SUB> based on Service brake curve
  - v<SUB>sw</SUB> based on Second warning curve
  - v<SUB>fw</SUB> based on First warning curve

  These speed values will then be compared to current speed to determine the current target supervision zone.

  The various speeds at current position is calculated solving the formula below for v<SUB>calc</SUB>:
  \f[ (v_{calc} - v_t) * t_{delay} + \frac{(v_{calc} - G' * t_{delay})^2 - (v_t - G' * t_{delay})^2}{2*r_m} = d_{t}\f]

  Solving for v<SUB>calc</SUB> we get:
  \f[ v_{calc} = \sqrt{w} - v_2\ if\ d_t>0\f]
  \f[ v_{calc} = v_t\ if\ d_t\leq0\f]
  
  v<SUB>calc</SUB> can never be less than the target speed v<SUB>t</SUB>. So,
  \f[ v_{calc} = Max(v_{calc},\ v_{t})\f]
  
  Where:
  \f[ G' = G\  if\ \ G\leq0 \f]
  \f[ G' = 0\  if\ \ G>0 \f]
  \f[ r_m = r + G \f]
  \f[ v_2= t_{delay} (r_m - G') \f]
  \f[ w = (v_t - v_1)^2 - v_1^2 + v_2^2 + 2 r_m (d_{t}+ d_1) \f]
  \f[ v_1 = t_{delay} G' \f]
  \f[ d_1 = t_{delay} v_t \f]
  \f[ v_t = first\ warning, second\ warning, SB\ or\ EB\ speed\ at\ target \f]
  \f[ t_{delay} = first\ warning,second\ warning,\ SB\ or\ EB\ delay\f]
  \f[ d_t = distance\ to\ supervised\ target\ position \f]

  \paragraph FirstWc First Warning Speed

  \f[ v_t = first\ warning\ speed\ at\ supervised\ target\ point\f]
  \f[ t_{delay} = t_{fw} + t_{sw} + t_{sb} + t_{sbr} + t_{eb} + t_{ebr} \f]
  The fw curve speed cannot exceed ceiling speed, so:
  \f[ v_{fw} = Min(v_{calc}, v_c )\f]
  If supervising Release speed:
  \f[ v_{fw} = Max(v_{fw}, v_{rs} )\f]
  \f[ v_{fw} = 0 \  if\ train\ is\ ahead\ of\ Release\ speed\ SB\ position\ (R_{sb})\f]

  \paragraph SecondWc Second Warning Speed

  \f[ v_t = second\ warning\ speed\ at\ supervised\ target\ point\f]
  \f[ t_{delay} = t_{sw} + t_{sb} + t_{sbr} + t_{eb} + t_{ebr} \f]
  The sw curve speed cannot exceed ceiling speed + warning margin, so:
  \f[ v_{sw} = Min(v_{calc}, v_c + M_{cw} )\f]
  If supervising Release speed:
  \f[ v_{sw} = Max(v_{sw}, v_{rs} + M_{cw} )\f]
  \f[ v_{sw} = 0 \  if\ train\ is\ ahead\ of\ Release\ speed\ SB\ position\ (R_{sb})\f]

  \paragraph SbWc Service Brake Speed

  \f[ v_t = service\ brake\ speed\ at\ supervised\ target\ point\f]
  \f[ t_{delay} = t_{sb} + t_{sbr} + t_{eb} + t_{ebr} \f]
  The SB curve speed cannot exceed ceiling speed + SB margin, so:
  \f[ v_{sb} = Min(v_{calc}, v_c + M_{cs} )\f]
  If supervising Release speed:
  \f[ v_{sb} = 0 \  if\ train\ is\ ahead\ of\ Release\ speed\ SB\ position\ (R_{sb})\f]
  \f[ v_{sb} = Max(v_{sb}, v_{rs} + M_{cw} )\  if\ target\ is\ before\ Release\ speed\ SB\ position\ (R_{sb})\f]
  \f[ v_{sb} = Max(v_{sb}, v_{rs})\  if\ target\ is\ ahead\ of\ Release\ speed\ SB\ position\ (R_{sb})\f]
  
  \paragraph EbWc Emergency Brake Speed

  \f[ v_t = emergency\ brake\ speed\ at\ supervised\ target\ point\f]
  \f[ t_{delay} = t_{eb} + t_{ebr} \f]
  The EB curve speed cannot exceed ceiling speed + EB margin, so:
  \f[ v_{eb} = Min(v_{calc}, v_c + M_{ce} )\f]
  If supervising Release speed:
  \f[ v_{eb} = 0 \  if\ train\ is\ ahead\ of\ Release\ speed\ EB\ position\ (R_{eb})\f]
  \f[ v_{eb} = Max(v_{eb}, v_{rs} + M_{cw} )\  if\ target\ is\ before\ Release\ speed\ EB\ position\ (R_{eb})\f]
  \f[ v_{eb} = Max(v_{eb}, v_{rs})\  if\ target\ is\ ahead\ of\ Release\ speed\ EB\ position\ (R_{eb})\f]

  \subsubsection tsSupv Supervision
  When entering a zone, a certain number of actions are to be taken on each zone. Depending on the previous zone status, actions might need to be
  activated or deactivated. A number of conditions need to be valid normally for each action. In the following sections specific conditions for each
  zone action are defined. The determined zone is stored in the internal field zone Status. The value of zone status determined the previous execution
  cycle must be saved before the current zone determination begins, as the previous zone status is needed in the current zone determination.

  \paragraph ZoneA Zone A, Default Indication Zone, or “No Indication Zone”
  This is the normal state when supervising a target. It finishes where Zone B starts, when speed is at the ab curve.

  Entry conditions:

  \f[v_a < v_{fw} \f]

  Entry Actions:

  Disable the buzzer.
  -\f$ Buzzer = BuzzerTypeNone \f$ 

  \paragraph ZoneB Zone B, First Warning Zone
  Zone B begins when speed is at the ab curve and finishes when the speed is at the bc Curve. The driver is alerted and a log event is issued when Zone B is entered from Zone A or no zone for the first time.

  Entry conditions:

  \f[v_a \geq v_{fw} \f]

  Entry Actions:

  Request alert signal if entering from no zone or Zone A and if this is a new target.
  -\f$ Buzzer = BuzzerTypeOneBeep \f$
  -\f$ ATPWarning = true \f$ 
  
  If SB is requested as SB curve was breached:
  - Raise a service brake event for "Service brake on target supervision!"
  - \f$ ATPIntervention = true \f$ 

  \paragraph ZoneC Zone C, Second Warning Zone
  Zone C begins when speed is at the bc curve and finishes when the speed is at the cd Curve. The driver is alerted  and a log event to the TCC is issued when Zone C is entered from Zone B, Zone A or no zone for the first time.
  The Driver is also alerted if the SB speed is below the second warning speed. This can happen close to primary target as the SB speed is allowed to drop to Release speed while SW speed is limited to Release speed plus warning margin.

  Entry conditions:

  \f[v_a \geq v_{sw}\ OR\ v_{sb} \leq v_{sw}\f]

  Entry Actions:

  Request alert signal if entering from no zone or Zone A or Zone B and if this is a new target.
  -\f$ Buzzer = BuzzerTypeConstantBeep \f$
  -\f$ ATPWarning = true \f$  

  If SB is requested as SB curve was breached:
  - Raise a service brake event for "Service brake on target supervision!"
  - \f$ ATPIntervention = true \f$ 
  
  \paragraph ZoneD Zone D, Service Brake Applied Zone
  Zone D begins when speed is at the cd Curve and finishes when speed is at the de Curve. A service brake event will be issued.
  The AOS shall continue to raise the brake event till the speed is below the first warning curve speed.

  Entry conditions:

  \f[v_a \geq v_{sb} \f]

  Entry Actions:

  - Raise a service brake event for "Service brake on target supervision!"
  - \f$ ATPIntervention = true \f$ 

  \paragraph ZoneE Zone E, Emergency Brake Applied Zone
  Zone E begins when maximum speed is at the de curve and has no end. A emergency brake event will be issued.
  It is to be noted that the EB curve speed is supervised with the filtered maximum speed (\f$ v_{max} \f$) reported from COD rather than the nominal speed.

  Entry conditions:

  \f[v_{max} \geq v_{eb} \f]

  Entry Actions:

  - Raise a emergency brake event for "Emergency brake activated over speeding Brake curve"
  - \f$ ATPIntervention = true \f$
   
  \subsection pspeed Permitted speed
  The permitted speed is the maximum speed that is allowed. Exceeding the permitted speed will result in warning or intervention by ATP. This is also the speed that is indicated to the Driver on the DMI.
  
  <b>Permitted Speed, Target Supervision Not in Progress</b>
  
  When target supervision is not in progress, the Permitted Speed v<SUB>p</SUB> is set equal to the Ceiling Speed v<SUB>c</SUB>.
  \f[v_p= v_c\f]
  
  <b>Permitted Speed, Target Supervision in Progress</b>
  
  When target supervision is in progress the Permitted Speed v<SUB>p</SUB> is set equal to the First Warning curve Speed v<SUB>fw</SUB>.


  \subsection calcDMIData Calculate Data for DMI
  The function first finds the most restrictive and next restrictive targets which are defined as follows:
  - Most Restrictive Target : This is defined as the supervised target ahead of the train front, for which
    - the supervised target is not Gradient supervised target, AND
    - the first warning speed is the same as the first warning ceiling speed AND
    - the first warning ceiling speed is lower than the ceiling speed at target OR the ceiling speed at target is zero.
  - Next Restrictive Target : This is defined as the supervised target ahead of the train front and before Most Restrictive Target for which the first warning speed is less than the first warning ceiling speed. If there is no such target, then the Next Restrictive target is the same as Most Restrictive Target.
  

  The MRT and NRT are then used to calculate the following data which is shown on the DMI:
  - Decide if train is in BCA
  - The distance to BCA
  - Distance to target
  - Distance to stand still
  - Predicted speed at target
  - Time to intervention 

  @image html abstract_supervise_calc_dmi_data.png "Calculate DMI Data"
  @image latex abstract_supervise_calc_dmi_data.png "Calculate DMI Data"
  

  \subsubsection bca Distance to Brake Curve Area
  The Brake Curve Area (BCA) is defined as the area where the first warning curve (ab) is below the ceiling speed v<SUB>c</SUB> see the figure below:

  @image html abstract_supervise_brake_curve_area.png "Brake Curve Area"
  @image latex abstract_supervise_brake_curve_area.png "Brake Curve Area"

  The brake curve area is calculated from the next restrictive target, as this is the first target ahead of the train front where the first warning speed is lower than the first warning ceiling speed.

  The remaining distance from current location to BCA, d<SUB>BCA</SUB> is calculated according to the following equations:
  \f[ d_{BCA} = d_{nrtp}-d_{lbca} \f]
  Where
  - d<SUB>nrtp</SUB> is the distance from current location to next restrictive target point.
  - d<SUB>lbca</SUB> is the Length of BCA

  d<SUB>lbca</SUB> is calculated based on the equation for First warning curve. In the following equations v<SUB>c</SUB> or the ceiling speed
  is used as the train speed v<SUB>a</SUB>.
  \f[d_{lbca} = \frac{v_{cm}^2-v_{tm}^2}{2r_m} + v_c t_{bca} - v_t t_{bca} \f]
  When the target speed is 0, that is when driving towards primary target, the distance to the brake curve area is
  added by the distance a stationary train moves during brake delay due to gradient and the distance it takes the
  train to stop after brake application.

  \f[ if\ v_t = 0,\ d_{lbca} = d_{lbca} + (\frac{v_1^2}{2*r_m} - \frac{1}{2}*v_1*t_{bca})\f]

  Where:
  \f[ G' = G\  if\ \ G\leq0 \f]
  \f[ G' = 0\  if\ \ G>0 \f]
  \f[ v_1 = t_{bca} G' \f]
  \f[v_{cm} = v_c - v_1 \f]
  \f[v_{tm} = v_t - v_1 \f]
  \f[ t_{bca} = t_{fw} + t_{sw} + t_{sb} + t_{sbr} + t_{eb} + t_{ebr} \f]
  \f[ r_m = r + G \f]
  \f[ v_t = first\ warning\ speed\ at\ target \f]

  As mentioned above we have:
  \f$ d_{BCA} = d_{nrtp}-d_{lbca} \f$
  - if \f$ d_{lbca} < 0 \;\; or \;\; d_{BCA}<0 \f$ ;
  - We are in BCA
  - \f$ d_{BCA} = 0 \f$
  - \f$ inBCA = true \f$ 

  \subsubsection perDisStand Predicted distance to stand still location
  The predicted distance to stand still location is calculated using the following equation:
  \f[d_{pds} = -\frac{v_a^2}{2r_{curr}}\f]
  Where:
  - d<SUB>pds</SUB> = Predicted distance to standstill location
  - v<SUB>a</SUB> = Train speed
  - r<SUB>curr</SUB> = Current acceleration value.

  If the train is accelerating the result of the formula will be negative. So:
  \f[ d_{pds} = MaxValue \;\;if\;\; r_{curr} \geq 0 \f]

  The condition should be checked before applying the formula to avoid division by zero.

  @image html abstract_supervise_predicted_distance_to_standstill.png "Predicted distance to stand still location"
  @image latex abstract_supervise_predicted_distance_to_standstill.png "Predicted distance to stand still location"
  

  \subsubsection timeToIntervention Time to intervention
  The time to intervention is calculated using the service brake curve to the Most Restrictive Target.
  Intervention takes place when the two curves of service brake curve (cd-curve) and train driving curve intersect.
  
  @image html abstract_supervise_time_to_intervention.png "Time to intervention"
  @image latex abstract_supervise_time_to_intervention.png "Time to intervention"
  
  d<SUB>mrt</SUB> is the distance to the most restrictive target point odometer. 
  
  If in the Release speed area for a Primary Target OR if the MRT is ahead of the Release speed SB position, the Release speed SB position (R<SUB>sb</SUB>) is used as target position with target speed as Release speed.

  If \f$ v_a = 0 \;\;\&\;\; r_a = 0 \f$ ; Current speed and current acceleration are zero
  \f[ t_i = MaxValue \f]

  If \f$ d_{mrt} < 0 \f$ ; the train has passed the service brake target point odometer
  \f[ t_i = MaxValue \f]

  If \f$ r_a + r \leq 0 \f$ ; We decelerate faster or same as if the service brake is active; the SB curve will not be hit.
  \f[ t_i = MaxValue \f]

  If \f$ v_a \leq Max(v_t, v_{rs}) \;\;\&\;\; r_a \leq 0 \f$ ; Current speed is less than or equal target speed and current acceleration is not positive
  \f[ t_i = MaxValue \f]

  If \f$ r_a = 0 \;\;\&\;\; v_a \ge Max(v_t, v_{rs}) \f$ ; The current acceleration is 0 and the speed greater than target speed
  \f[ d_i = d_{tmrt} - SB\ curve\ distance\ at\ v_a \f]
  \f[ t_i = \frac{d_i}{v_a} \f]

  \paragraph inreleasespeedarea While In Release speed area supervising Primary Target
  If \f$ r_a = 0 \;\;\&\;\; v_a \ge 0 \f$ ; when not accelerating nor decelerating
  \f[ t_i = \frac{d_i}{v_a} \f]

  \f$ Otherwise \f$
  \f[ v_{mrt} = \sqrt{v_a^2 + (2*r_a * d_{mrt})} \f]


  \f[if\ the\ speed\ at\ the\ MRT\ will\ be\ greater\ than\ the\ Release\ speed, \ v_{mrt} \ge v_{rs}\ :\ t_i = \frac{v_{rs} - v_a}{r_a}\f]
  \f[if\ the\ train\ will\ stop\ before\ MRT,\ v_{mrt} = 0\ :\ t_i = MaxValue \f]
  \f[if\ v_{mrt} \ge 0\ and\ v_{mrt} \le v_{rs}\ intervention\ is\ when\ train\ passes\ the\ MRT\ :\ t_i = \frac{v_{mrt} - v_a}{r_a}\f]

  \paragraph noconditionsaretrue If none of the above conditions are true
  Time to intervention is calculated by solving the equation below:
  \f[ v_a * t_i + \frac{1}{2} * r_a * t_i^2 = \frac{(v_{sb} - t_{delay} *G )^2 - (v_a + r_a*t_i - t_{delay} *G)^2}{2*r_m} + v_{sb} * t_{delay} - (v_a + r_a*t_i)*t_{delay}\f]

  Solving for t<SUB>i</SUB>:
  \f[ t_i = \frac{- v_a * (r_a + r_m) - r_a * v_1 + \sqrt{square}}{r_a^2 + r_a * r_m} \ \ if\ square >= 0 \f]
  \f[ t_i = MaxValue\ if\ square < 0\f]
  where:
  \f[ r_m = r + G \f]
  \f[ v_1 = t_{delay} * (r_m - G)\f]
  \f[ t_{delay} = t_{sb} + t_{sbr} + t_{eb} + t_{ebr} \f]
  \f[ v_{sb} = current\ permitted\ SB\ speed\ (cd\ curve\ value\ at\ current\ position)\f]
  \f[ v_{sbm} = v_{sb} + v_1\f]
  \f[ square = r_a^2 * v_{sbm}^2 + r_a * r_m * (v_{sb}^2 + v_a^2 + 2 * v_1 * v_{sb}) + v_a^2 * r_m^2\f]

  \subsection releasespeed Release Speed
  The Release speed is a speed limit that is intended to represent the lowest possible speed for a train with the certain configuration.
  Thus the Release speed is configurable with the unit cm/s.
  When supervising a primary target which has target speed 0 cm/s, the brake curve speeds are calculated to reduce to 0. In modes where
  Release speed is supervised, when FW or SW brake curve speed reaches the Release speed limit it will continue at Release speed instead of reducing
  the curve speed further. See figure "Brake and warning curves, with a target speed equal to zero while considering Release speed"
  This means that the train can travel at a decent speed when it is close to the target.
  
  When the SB or EB curve speed drops below the Release Speed, SB or EB is applied respectively. This is to ensure that the train will stop within the MA when travelling at Release speed.

  There may be a ceiling speed target with target speed lower than the Release speed. In these cases the train will be allowed to travel at
  maximum the Release speed. eg. the first warning speed would be
  \f[ v_c = Max(v_c, v_{rs} )\ ,\ if\ v_c\ > 0, v_{rs}\ being\ the\ Release\ speed \f]

  The definition of "in Release speed area" is implemented and used for time to intervention calculations. What it means is that the
  brake curve speed is below the Release speed and the brake curve speed is no longer the speed to supervise. The decision is done as follows
  \f[ if\ v_{fw} < v_{rs},\ inReleaseSpeedArea = true\ otherwise\ inReleaseSpeedArea = false \f] 


  \subsection standstillsup Standstill Supervision
  This function is to prevent the train from moving.  Abstract Supervise component supervises the train standstill condition in each cycle if the standstill event is active.
  When a movement is detected, the brake event will be triggered after a distance specified by the configured margin. 
  The service brake can be released only after train comes to a standstill, where the supervision will be re-initialized. The Standstill supervision remains active as long as the Standstill event is active. 

  @image html abstract_supervise_standstill_supervision.png "Standstill Supervision"
  @image latex abstract_supervise_standstill_supervision.png "Standstill Supervision"
  

  \subsection timsSup TIMS Supervision
  The Abstract Supervise component will raise a brake event if the TIMS is supervised and Broken.

  \subsection RevSupv Reverse Movement Supervision

  The Reverse Movement Protection will prevent the train from moving in the opposite direction to the permitted one. The permitted movement direction of a train shall be the one of the currently valid MA,
  if available on-board. When a reverse movement is detected, the brake command is triggered after a distance specified by the configured margins.
  - Service brake(SB) event when SB margin is exceeded.
  - Emergency brake(EB) event when EB margin is exceeded.

  The train is considered to be reversing if:
  - Movement authority is in Forward AND odometer direction is Reverse
  OR
  - Movement authority is in Reverse AND odometer direction is Forward 

  The AOS will set the reversing distance to 0 when:
  - an MA from scratch is received
  - at standstill after movement in the direction of the MA.
  - standstill due to a brake event triggered by the Reversing supervision.
  - ATP mode is other than Normal, Staff Responsible, Shunting Route, Split, Join or Balise Search and the train is not free rolling.
  
  @image html abstract_supervise_reverse_supervision.png "Reversing supervision"
  @image latex abstract_supervise_reverse_supervision.png "Reversing supervision"

  \subsection RollAwaySupv Roll Away Supervision
  The Roll Away Supervision shall prevent the train from moving in a direction, which conflicts with the current driving direction.
  When the system recognizes a movement exceeding the configured margin for the allowed roll away distance a brake event shall be triggered.
  - Service brake(SB) event when SB margin is exceeded.
  - Emergency brake(EB) event when EB margin is exceeded.

  The train is considered to be rolling away if:
  - Driver direction is Forward OR Neutral AND odometer direction is Reverse
  OR
  - Driver direction is Reverse OR Neutral AND odometer direction is Forward
  
  @image html abstract_supervise_rollaway_supervision.png "Roll away supervision"
  @image latex abstract_supervise_rollaway_supervision.png "Roll away supervision"

  \subsection LocationSupv Location Border Supervision

  This form of supervision checks if the train rear position is outside the location borders.
  If this happens, a service brake event is raised.

  This check is only performed in ATP mode Location.
  Please note that train front position is supervised by \ref targetSupervision.

  \subsection brakeFunc Summary of Brake Functions

  A brake event is raised by AbstractSupervise to apply service brake if one of the following is true:
  - The Ceiling Speed is exceeded by M<SUB>cs</SUB>
  - The current speed exceeds the service brake curve (cd) speed. i.e. we have entered Zone D
  - The current speed exceeds the Release speed by M<SUB>cs</SUB> at Release speed, if ceiling speed lower than Release speed
  - Distance moved in during standstill supervision exceeds the service brake margin.
  - TIMS is supervised and reported broken.
  - Distance moved during reverse supervision exceeds the service brake margin.
  - Distance moved during roll away supervision exceeds the service brake margin.

  A brake event is raised by AbstractSupervise to apply Emergency brake if one of the following is true:
  - The Ceiling Speed is exceeded by M<SUB>ce</SUB>
  - The current maximum speed exceeds the emergency brake curve (de) speed i.e. we have entered Zone E
  - Distance moved in during standstill supervision exceeds the emergency brake margin.
  - Distance moved in during reverse supervision exceeds the emergency brake margin.
  - Distance moved in during roll away supervision exceeds the emergency brake margin.
  
  \section ClassDiagram Class Diagram

  @image html abstract_supervise_class_diagram.png "AbstractSupervise class diagram"
  @image latex abstract_supervise_class_diagram.png "AbstractSupervise class diagram"

  @image html brake_calculations_class_diagram.png "BrakeCalculations class diagram"
  @image latex brake_calculations_class_diagram.png "BrakeCalculations class diagram"

  
  \section Diagnostics Diagnostics

  \subsection Console Console-commands
  The following component-specific Console-commands are implemented:

  + `brakeab`       Brakeability for current speed and brake system type in use
  + `brkDelay`      Brake delay for SB and EB for the brake system type in use
  + `dist2BCA`      Distance to Brake curve area (BCA)
  + `curmrt`        Current most restrictive target position
  + `rap`           Roll Away Protection information
  + `rmp`           Reverse Movement Protection information

  \subsection Analyze Analyze
  References to variables to be accessed by an external Analyzer tool are prepared at initialization.

  Variable    | Unit | Description
  ----------- | ---- | -----------------------
  firstWarningSpeed | cm/s | First Warning Speed at current position
  secondWarningSpeed | cm/s | Second Warning Speed at current position
  sbSpeed | cm/s | SB Speed at current position
  ebSpeed | cm/s | EB Speed at current position
  timeToIntervention | s | Time to intervention

  \section CoreAdaptation Core / Adaptation
  The Supervise component is split in a core and one adaptation part. The adaptation implements the instantiation of the core class. The adaptation can override the virtual functions in order to create a different behaviour.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component.

  \section Traceability Traceability

  \subsection SSRS Functional Requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP Core.

  The requirements relevant for this component are:

  Req          | Chapter                | Function
  ------------ | ---------------------- | --------
  AOS 120 S    | \ref LocationSupv      | AbstractSupervise::locationSupervision
  AOS 308      | \ref calcDMIData       | AbstractSupervise::findRestrictiveTarget
  AOS 309 S    | \ref calcDMIData       | AbstractSupervise::findRestrictiveTarget
  AOS 314 S    | \ref targetSupervision | AbstractSupervise::superviseTargets
  AOS 326 S    | \ref ZoneD             | AbstractSupervise::superviseTargets
  AOS 328 S    | \ref ZoneE             | AbstractSupervise::superviseTargets
  AOS 343 S    | \ref SbLimit           | AbstractSupervise::superviseCeilingSpeed
  AOS 345 S    | \ref EbLimit           | AbstractSupervise::superviseCeilingSpeed
  AOS 382      | \ref RollAwaySupv      | AbstractSupervise::rollAwaySupervision
  AOS 388 S    | \ref RevSupv           | AbstractSupervise::revSupervision
  AOS 390      | \ref RevSupv           | AbstractSupervise::revSupervision
  AOS 504 S    | \ref timsSup           | AbstractSupervise::superviseTims
  AOS 673      | \ref tsCalc            | AbstractSupervise::calcBrakeCurveSpeedAtCurPosition
  AOS 674 S    | \ref tsCalc            | AbstractSupervise::calcBrakeCurveSpeedAtCurPosition
  AOS 676      | \ref ceilingSpeed      | AbstractSupervise::superviseCeilingSpeed
  AOS 678      | \ref ZoneB             | AbstractSupervise::superviseTargets
  AOS 679      | \ref ZoneC             | AbstractSupervise::superviseTargets
  AOS 681 S    | \ref RollAwaySupv      | AbstractSupervise::rollAwaySupervision
  AOS 835      | \ref calcDMIData       | AbstractSupervise::calcDMIDisplayData
  AOS 840      | \ref calcDMIData       | AbstractSupervise::calcDMIDisplayData
  AOS 844      | \ref bca               | AbstractSupervise::calcDMIDisplayData
  AOS 1147     | \ref RollAwaySupv      | AbstractSupervise::rollAwaySupervision
  AOS 1707     | \ref standstillsup     | AbstractSupervise::superviseTrainAtStandstill
  AOS 1710     | \ref standstillsup     | AbstractSupervise::superviseTrainAtStandstill
  AOS 2123     | \ref FWLimit           | AbstractSupervise::superviseCeilingSpeed
  AOS 2160 S   | \ref targetSupervision | AbstractSupervise::superviseTargets
  AOS 2249     | \ref SwLimit           | AbstractSupervise::superviseCeilingSpeed
  AOS 2597     | \ref standstillsup     | AbstractSupervise::superviseTrainAtStandstill
  AOS 2833     | \ref standstillsup     | AbstractSupervise::superviseTrainAtStandstill
  AOS 3169 S   | \ref RollAwaySupv      | AbstractSupervise::rollAwaySupervision
  AOS 3174 S   | \ref RevSupv           | AbstractSupervise::revSupervision
  AOS 3188     | \ref tsCalc            | AbstractSupervise::calcBrakeCurveSpeedAtCurPosition
  AOS 3190 S   | \ref targetSupervision | AbstractSupervise::superviseTargets
  AOS 3191 S   | \ref targetSupervision | AbstractSupervise::superviseTargets
  AOS 3216 S   | \ref RevSupv           | AbstractSupervise::revSupervision
  AOS 3221 S   | \ref standstillsup     | AbstractSupervise::superviseTrainAtStandstill
  AOS 3267     | \ref ceilingSpeed      | AbstractSupervise::run
  AOS 3285 S   | \ref run               | AbstractSupervise::verifyBrakeabilityVsGradient
  AOS 3001     | \ref pspeed            | AbstractSupervise::findRestrictiveTarget

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP Core.

  Only the architectural requirements traced explicitly to this component are included in the table below.
  Fulfillment of other architectural requirements allocated to the ATP is described in [SWAS].

  Req          | Chapter                | Function
  ------------ | ---------------------- | --------
  AOS_AS-765 S | \ref targetSupervision | AbstractSupervise::superviseTargets
  AOS_AS-766   | \ref ceilingSpeed      | AbstractSupervise::superviseCeilingSpeed

  */
}
