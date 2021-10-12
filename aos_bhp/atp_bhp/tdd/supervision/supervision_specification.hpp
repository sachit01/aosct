/**
\if AsMainPage
\mainpage Technical Design Description for Supervision
\endif

\section VersionLog Version Log
Version | Date       | Description                                            | Signature
------- | --------   | -------------------------------------------------------|---------------
1.0     | 2017-12-06 | Document creation                                      | skothiya
2.0     | 2017-01-10 | Document updated for peer review comments              | skothiya
3.0     | 2018-02-16 | Document updated for the final comments                | skothiya
4.0     | 2018-02-21 | Document updated for the further comments              | skothiya

\section Abbreviations Abbreviation
Abbreviation   | Definition
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
MA             | Movement Authority
DMI            | Driver Machine Interface

\section Introduction Introduction
This document describes the technical design description to implement the requirements related to supervision functionality in AOS BHP.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix
Below table describes the requirements which are in scope of this document.

Req      | Short requirement description                                                                                                                        | AOS Implementation Status
------   | -------------------------------------------------------------------------------------- ---------------------------------------------------           | ----------------------
AOS 2361 | The AOS shall set the current driving direction as the following, except when in ATP Mode Location of type Unload AND train state FreeRolling is active: The direction selected by the Driver in ATO Manual. The direction selected by the ATO in ATO Automatic OR ATO Supervised. | Partially Implemented
AOS 2362 | In ATP Mode Location of type Unload with train state FreeRolling active, the AOS shall set the current driving direction as: The Forward direction when at standstill, OR The direction of movement. | Not Implemented
AOS 681 S| The AOS shall issue a Brake event if the vehicle is moved more than configurable Roll-Away Margin in a direction different to the current Driving direction. | Partially Implemented
AOS 382  | Once the vehicle is at standstill the AOS shall clear the Brake event. | Partially Implemented
AOS 388 S| The AOS shall issue a Brake event if: ATP mode is Normal OR Staff Responsible OR Shunting Route OR Automatic Unload OR Split OR Join (with MA) AND The vehicle is moved more than configurable Reversing Supervision distance opposite the direction given by the MA. | Partially Implemented
AOS 390  | Once the vehicle is at standstill the AOS shall clear the Brake event. | Not Implemented
AOS 2160S| The AOS shall ensure that the current train speed is below or equal to the target speed before the train front end has reached the target position. | Implemented
AOS 309 S| The AOS shall select the most restrictive target, which is the closest target whose brake curve is not restricted by any other targets brake curve. | Implemented
AOS 308  | The AOS shall in ATP mode Location, set the most restrictive target to Location End/Location Start Primary Target when the current Driving direction has changed if: The train is at standstill, OR The train state Free Rolling is active. | Not Implemented
AOS 673  | The ATP shall calculate the following brake curves for selected most restrictive target: First warning curve, Second warning curve, Service brake application curve, Emergency brake application curve | Implemented
AOS 2736S| AOS shall define Emergency brake margin as the distance between the Emergency brake target point and the supervised target point. The emergency brake application curve shall be calculated based on the Emergency brake target point. Emergency brake margin is set to 200 cm unless overridden by adaptation. |  Partially implemented
AOS 2737 | AOS shall define service brake margin as the distance between Service brake target point and the Emergency brake target point. The Service brake application curve shall be calculated based on Service brake target point. Service brake margin is set to 100 cm unless overridden by adaptation. |  Partially implemented
AOS 314 S| The AOS shall supervise the selected most restrictive target if: The current position state is Known, AND At the current train position, the ceiling speed is limited by the First Warning curve of the target. | Not implemented
AOS 678  | When the speed exceeds the First Warning curve for the most restrictive target the AOS shall:Inform the Driver with visual indication AND a beep sound, AND Issue a Log event. | Not Implemented
AOS 679  | When the speed exceeds the Second Warning curve the AOS shall:Inform the Driver with visual indication AND a continuous beep till the speed is below the Second Warning curve speed, AND Issue a Log event. | Not Implemented
AOS 326 S| The AOS shall issue a Brake event when the speed exceeds the Service brake application curve for the most restrictive target. The AOS shall continue to raise the Brake event till the speed is below the first warning curve speed. | Partially Implemented
AOS 328 S| When the speed exceeds the Emergency brake application curve for the most restrictive target the AOS shall issue a Brake event with EB application. | Partially Implemented
AOS 226  | If an MA from scratch is accepted, the AOS shall set the current ceiling speed to the speed at the beginning of MA. | Not Implemented
AOS 2119 | In mode Yard, Possession OR Shunting the AOS shall set the current ceiling speed to the value defined in the YardAcknowledge, PossessionAcknowledge and ShuntingAcknowledge message respectively. | Not Implemented
AOS 2120 | In Yard mode if there is no radio communication, the AOS shall set the current ceiling speed to the value defined by the configuration parameter. | Partially Implemented
AOS 2121 | In Balise search mode, the AOS shall set the current ceiling speed to the value defined by the configuration parameter. | Implemented
AOS 229  | The AOS shall in ATP mode Location, set the current ceiling speed to the speed defined for current supervised Location. | Not Implemented
AOS 227 S| When the train position is Known, the AOS shall update the current ceiling speed when a ceiling speed target is passed. A ceiling speed target is passed when: The train front is ahead of the ceiling speed target without train length delay OR The train rear end is ahead of the train length delayed ceiling speed target. | Not Implemented
AOS 2122 | The AOS shall supervise the ceiling speed which is the minimum of: The current ceiling speed AND The maximum permitted train speed (V_SPEED) in the Train Setup. | Partially Implemented
AOS 676  | The AOS shall calculate following speed limits for the current supervised ceiling speed: Warning speed limit, Service brake speed limit, if configured for SB access, Emergency brake speed limit.The AOS shall provide configurable parameters to calculate the above limits. | Partially Implemented
AOS 2123 | If the current speed exceeds the current supervised ceiling speed the AOS shall: Inform the Driver with visual indication AND a beep sound, AND Issue a Log Event. | Partially Implemented
AOS 2249 | If the current speed exceeds the Warning speed limit, the AOS shall: Inform the Driver with visual indication AND continuous beeps till the speed is below the ceiling speed, AND Issue a Log Event. | Partially Implemented
AOS 343 S| The AOS shall raise a Brake event when the current speed exceeds the Service Brake speed limit. The AOS shall continue to raise the event as long as the current speed is more than the current supervised ceiling speed. | Implemented
AOS 345 S| The AOS shall raise a Brake event with EB application when the current speed exceeds the Emergency Brake speed limit. | Implemented



\subsection ExternalInterfaceDescription External Interface Description | Partially Implemented
Following external interface description is used to implement the requirement described above.
Seq No | Document Name                                      | Version
-------|----------------------------------------------------|----------
1.     | Interface Specification ATP - DMI                  | 2.14
2      | FFFIS TCC-AOS                                      | 5.8

\section Assumption Assumption
Pseudo code used in the document is only to understand the logic, please ignore lint and  syntax errors in pseudo codes.
During the implementation please do not use same code.
Also comments used in pseudo codes are not according to the coding guideline it is more descriptive to understand the logic
and to map the requirement with pseudo code.

\section DetailedDescriptionOfDesign Detailed Description of Components Design/Changes for requirements
This section describes affected components and function with logic need to add/change to implement the different requirements of supervision.
This document is covering requirement mentioned in section 5.3.22.1 to 5.3.22.6 in DOORS.

\subsection DrivingDirection Driving Direction
  - Requirement Covered: AOS 2361 & AOS 2362
  - Affected component : Mode Control
    + Description of Changes: In current implementation getter method IO::AbstractLocoIO::corePtr()->getLocoDirection is used for driving direction.
                              But I think we need to make getter in AbstractModeControl class.
                              + Define new getter method in AbstractModeControl class i.e.AbstractModeControl::getDrivingDirection(), it will return value fetched from mode classes.
                              + Define new virtual method in AbstractMode class getDrivingDirectionFrmMode().
                                  - This method will return getLocoDirection() {Direction defined by driver in all the mode except location).
                              + Override the method getDrivingDirectionFrmMode() in LocationMode class.
                                   - In LocationMode class driving direction return by this method will depend on the condition mentioned in AOS 2362.
                                     Complete implementation of Req AOS 2362 will done when location mode will be completely implemented.
                              + Also we need to replace AbstractModeControl::getDrivingDirection() in all the places where AbstractLocoIO::getLocoDirection() is used.

\subsection RollAwaySuperv Roll Away Supervision
  - Requirement Covered: AOS681S, AOS382
  - Affected component :  Odometry
     + Description of Changes:  In current implementation same service brake event is getting raised for roll-away and reverse supervision.
                                To raise separate events below changes are required:
                                + Create a new service brake event to indicate max roll away distance has been crossed.
                                + Below Changes need to be done in  AbstractOdometry::revAndRollSupervision() method for Roll Away supervision:

  +Pseudo Code:
  \code
   if((rollAwayDistance > maxRollAwayDistance) && !isTrainStandStill())
     {
      // Raise service brake if train is rolling away and exceeded configured max roll away
      // And train is not in stand still
      ATC::AbstractEventHandler::corePtr()->reportEvent(RollAwayError, __FILE__,__LINE__);
     }
  \endcode

\subsection ReversingSuperv Reversing Supervision
  - Affected component: Odometry & Mode Control
  - Requirement Covered: AOS388 S, AOS390
    + Description of Changes:   Create a new service brake event to indicate max reversing supervision distance has been crossed.
                              + Create a new flag (boolean variable) in CommonDataForModes structure to indicate if reverse supervision is allowed or not.
                                 Possible name could be reverseSupervAllowedInMode
                                 This flag will be set from Normal Mode, Staff Responsible, Shunting Route, Automatic Unload, Split and Join.
                               + Flag will be reset in each cycle in AbstractModeControl class (run method)and will be set in particular mode (if applicable) in each cycle.
                               + Define a new public method getReverseSupervAllowedInMode() in AbstractModeControl class, this method will return the current value of reverseSupervAllowedInMode.
                               + Below Changes need to be done in  AbstractOdometry::revAndRollSupervision() method for reverse supervision:

  +Pseudo Code:
  \code
   bool isReverseSupervAllowed = Kernel::AbstractModeControl::corePtr()->getReverseSupervAllowedInMode();
   if((reverseDistance > maxReverseDistance) && !isTrainStandStill() && isReverseSupervAllowed)
   {
      // Raise service brake if train has exceeded max reverse supervision distance
     // And train is not in stand still and ATP mode is Normal OR Staff Responsible
    // OR Shunting Route OR Automatic Unload OR Split OR Join
    ATC::AbstractEventHandler::corePtr()->reportEvent(ReverseSupvExceed, __FILE__,__LINE__);
    }
  \endcode

\subsection TargetSupvReqAOS2160 Target Supervision Req AOS 2160S
  - Affected component:  Supervision & Target Calculation
  - Requirement Covered: 2160 S
     + Description of Changes: This requirement is already implemented in supervision component.
                               + Target Supervision ensures that train speed should be under the next target speed.
                               + The implementation for this requirement is done in AbstractTargetCalculation::updatePermittedSpeed() and AbstractSupervise::superviseTargets() method.
                               + AbstractTargetCalculation::updatePermittedSpeed() method sets the permitted speed for the targets from the end.
                                 It means target will have permitted speed according to the next target ceiling speed.
                               + AbstractSupervise::superviseTargets() checks continuously if current speed is under the permitted target speed.

\subsection TargetSupvReqAOS309 Target Supervision Req AOS 309 S
  - Affected component:  Target Calculation
  - Requirement Covered: 309 S
     + Description of Changes: This requirement is already implemented in Target Calculation component.
                               + AbstractTargetCalculation::findRestrictiveTargetId() method is finding the most restrictive target.

\subsection TargetSupvReqAOS308 Target Supervision Req AOS 308
  - Affected component & Method:  Target Calculation & AbstractTargetCalculation::findRestrictiveTargetId()
  - Requirement Covered: 308
      + Description of Changes: Need to make below changes in AbstractTargetCalculation::findRestrictiveTargetId() method:
                                + Need to check current mode if current mode is location and if train is
+Pseudo Code:
  \code
  void AbstractTargetCalculation::findRestrictiveTargetId()
    {
      restrictiveTargetId = 0U;
      TravelDir tDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();

      //Check if current mode is location for Req AOS 308
    ATPMode atpCurrentMode  ATP::Kernel::AbstractModeControl::corePtr()->getCurrentMode();
    bool isTrainStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
    bool isTrainFreeRolling = ATP::Kernel::AbstractModeControl::corePtr()->getFreeRolling();

     // Traverse through the speed, conditional, and primary targets from the closest target to the train front to the furthest one.
      for (DS::AbstractTargetIterator it = DS::AbstractTargets::corePtr()->getTargetIter();
        it != DS::AbstractTargets::corePtr()->getTargetIterEnd(); ++it)
      {
        // If the travel direction matches target direction
        if (tDir == (*it)->getDirection())
        {
          DS::BaseTarget::CoreTargetType targetType = (*it)->getTargetType();
          // if target type is speed or conditional
          if ((DS::BaseTarget::SpeedTarget == targetType) || (DS::BaseTarget::ConditionalTarget == targetType) || (DS::BaseTarget::PrimaryTarget == targetType))
          {
            // target is restrictive when ceilingSpeed and permitted speed are the same, which means that it is not restricted by any other target
            if ((*it)->getPermSpeed() == (*it)->getCeilingSpeed())
            {
              restrictiveTargetId = (*it)->getTargetId();
              break;
            }
          }
        }
      }
    }

  \endcode

\subsection TargetSupvReqAOS673 Target Supervision Req AOS 673
  - Affected component & Method:  Supervision & AbstractSupervise::superviseTargets()
  - Requirement Covered: 673
     + Description of Changes: This requirement is already implemented in AbstractSupervise::superviseTargets() method.
     + AOS is calculating the distance according to the current speed for:
     - first warning curve
     - second warning curve
     - service brake application curve
     -  emergency brake curve
     + Only Change need to be done for this requirement is:
     - Need not to calculate the distance for service brake curve if SB access is not configured
     +Pseudo Code:
     \code

     //Calculate the SB curve distance to target only if SB access is configured
     if( SB access configured)
     {
     // SB curve distance to target
     cdDist2Targ = BrakeCalculations::instance().calcSBCurveDistance(currentSpeed, brakeResponseTime, gradient, deceleration, targetSpeed, calcCeilingSpeed);
     }

     //Check Zone D entry (service brake curve speed) criteria if access is configured
     //Code is shown in pseudo code of req
     \endcode

\subsection TargetSupvReqAOS2736 Target Supervision Req AOS 2736 S & AOS 2737
  - Affected component & Method:  Supervision & AbstractSupervise::superviseTargets()
  - Requirement Covered: AOS 2736 S & AOS 2737
      + Description of Changes: This requirement is already implemented in AbstractSupervise::superviseTargets() method.

\subsection TargetSupvReqAOS314 Target Supervision Req AOS 314 S
  - Affected component:  Supervision & AbstractSupervise::run()
  - Requirement Covered: 314 S
     + Description of Changes: Below changes need to be done in AbstractSupervise::run() method:

    +Pseudo Code:
     \code
     void AbstractSupervise::run(void)
     {
     .
     .
     .
     handleModes(superviseCeiling, superviseTarget);
     // Get the current position accuracy state
     bool isPosValid = (Pos::PosKnown == Pos::AbstractPosition::corePtr()->getAccuracyState());
     // Calculate First warning curve speed at current position for the target
     uint32_t firtsWarningCurvespeed = BrakeCalculations::instance().calcFirstWarningCurveSpeed(currfrontPos, targetSbOdo, confidenceMargin, stDir, brkRespTime, gradient,
     deceleration, targetSpeed, calcCeilingSpeed);
     //if Position state is Known and if first warning curve speed is lower then current ceiling speed then only supervise the target
     if (isPosValid && (firtsWarningCurvespeed < calcCeilingSpeed)) //
     {
     superviseTargets(currentSpeed, currfrontPos, tDir, brakeResponseTime, gradient, deceleration, targetSpeed,
     targetSbOdo, targetEbOdo, calcCeilingSpeed, newTarget);
     }
     \endcode

\subsection TargetSupvReqAOS678 Target Supervision Req AOS 678
  - Affected component & Method:  Supervision & AbstractSupervise::superviseTargets()
  - Requirement Covered: 678
     + Description of Changes: Below changes are required in AbstractSupervise::superviseTargets() method at line number 600 in abstract_supervise.cpp file:

     +Pseudo Code:
     \code
     .
     .
     .
     bool passBC_BelowAlarm = passBC && (!spHigherAlarm); // only valid for targetSpeed non-zero

     if ((targetSpZero && betweenAB_BC) ||
     ((!targetSpZero) && (betweenAB_BC || passBC_BelowAlarm)))
     {
     // If new target and the previous zone was noZone or ZoneA
     if (newTarget && ((ZoneA == currentZone) || (NoZone == currentZone)))
     {
     alarm = (alarm < AlarmOneBeep) ? AlarmOneBeep : alarm;   // Making sure an alarm with higher priority is not overwritten
     //Raise a log event as speed exceeds the first warning curve
     }
     currentZone = ZoneB;
     }
     \endcode

     - To raise beep (single or continuous), AOS has to activate Buzzer output signal, for this below changes are required in abstract_loco_io class in void AbstractLocoIO::runOut(void) method:
     +Pseudo Code:
     \code
     void AbstractLocoIO::runOut(void)
     {
     .
     .
     .
     //set value for Buzzer output signal
     bool buzzerActivation = Kernel::AbstractModeControl::corePtr()->getBuzzerStatus() | Supv::AbstractSupervise::corePtr()->getAlarm();
     static_cast<void>(setCoreDigitalOutputValue(Buzzer, buzzerActivate));
     }
     \endcode

\subsection TargetSupvReqAOS679 Target Supervision Req AOS 679
  - Affected component:  Supervision
  - Requirement Covered: 679
    + Description of Changes: Below changes are required in AbstractSupervise::superviseTargets() method at line number 600 in abstract_supervise.cpp file:

    +Pseudo Code:
    \code
    bool betweenBC_CD_AboveAlarm = betweenBC_CD && spHigherAlarm;
    bool passCD_AboveAlarmBelowSB = passCD && spHigherAlarm && (!spHigherSB);

    if ((targetSpZero && betweenBC_CD) ||
    ((!targetSpZero) && (betweenBC_CD_AboveAlarm || passCD_AboveAlarmBelowSB)))
    {
    if ((NoZone == currentZone) || (ZoneA == currentZone) || (ZoneB == currentZone))
    {
    alarm = (alarm < AlarmTwoBeep) ? AlarmTwoBeep : alarm;   // Making sure an alarm with higher priority is not overwritten
    atpWarning = true;
    //Raise log event here
    }
    currentZone = ZoneC;
    }
    \endcode


\subsection TargetSupvReqAOS326 Target Supervision Req AOS 326 S & 328 S
  - Affected component:  Supervision & AbstractSupervise::superviseTargets()
  - Requirement Covered: AOS 326 S & AOS 328 S
     + Description of Changes: These requirements are already implemented. But need to rearrange the condition checks in AbstractSupervise::superviseTargets() method.

     +Pseudo Code:
\code
// Zone E entry condition

if ((targetSpZero && (passDE || passedEBTP)) ||
((!targetSpZero) && passDE_AboveEB))    // Requirement AOS 328
{
  ATC::AbstractEventHandler::corePtr()->reportEvent(emergencyBrakeTargetSupervision, __FILE__, __LINE__);
  atpIntervention = true;
  currentZone = ZoneE;
}
else if ((SB access configured) && ((targetSpZero && (betweenCD_DE || passedSBTP)) ||  // Requirement AOS 326 & AOS 673
((!targetSpZero) && (betweenCD_DE_AboveSB || passDE_AbovSBBelowEB)))     // Zone D entry condition
{
  ATC::AbstractEventHandler::corePtr()->reportEvent(serviceBrakeTargetSupervision, __FILE__, __LINE__);
  atpIntervention = true;
  currentZone = ZoneD;
}
else if ((targetSpZero && betweenBC_CD) ||                                    // Requirement AOS 679
((!targetSpZero) && (betweenBC_CD_AboveAlarm || passCD_AboveAlarmBelowSB)))    // Zone C entry condition
{
  if ((NoZone == currentZone) || (ZoneA == currentZone) || (ZoneB == currentZone))
  {
    alarm = (alarm < AlarmTwoBeep) ? AlarmTwoBeep : alarm;   // Making sure an alarm with higher priority is not overwritten
    atpWarning = true;
  }
  currentZone = ZoneC;
}
else if ((targetSpZero && betweenAB_BC) ||                                      // Requirement AOS 678
((!targetSpZero) && (betweenAB_BC || passBC_BelowAlarm)))                      //Zone B entry condition
{
                                                                                 // If new target and the previous zone was noZone or ZoneA
  if (newTarget && ((ZoneA == currentZone) || (NoZone == currentZone)))
  {
    alarm = (alarm < AlarmOneBeep) ? AlarmOneBeep : alarm;   // Making sure an alarm with higher priority is not overwritten
  }
  currentZone = ZoneB;
}
else if (!passAB)                                           // Zone A entry condition
{
  currentZone = ZoneA;
}
else
{
  //Nothing to do
}
\endcode


\subsection CeilingSpeedSupvReqAOS2119 Ceiling Speed Supervision Req AOS 2119
  - Affected component : Mode Control
  - Requirement Covered: 2119
     + Description of Changes: This changes will be done in mode control.
                               + When mode will be changed to Shunting/Yard/Possession after receiving ShuntingAcknowledge/PossessionAcknowledge/YardAcknowledge message.
                                 Need to set current ceiling speed with received speed in message.
                                Below changes are required

                                + In YardModeRequestSeq class
                                 +Pseudo Code:
                                   \code
                                   void YardModeRequestSeq::runYardDmiButtonWaitForAck()
                                   {
                                   YardAcknowledge yardAck;

                                   if (RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus())
                                   {
                                   seqState = yardDmiButtonTCCTimeout;
                                   ATC::AbstractEventHandler::corePtr()->reportEvent(modTCCNotAvail, __FILE__, __LINE__);
                                   }

                                   else
                                   {
                                   //apply standstill event
                                   ATC::AbstractEventHandler::corePtr()->reportEvent(waitForModAckStandStill, __FILE__, __LINE__);

                                   if (AbstractMessageHandler::corePtr()->getYardAcknowledge(yardAck))
                                   {
                                   if (RequestAcknowledged == yardAck.yardAcknowledge)
                                   {
                                   //Code for requirement AOS 2119 set current ceiling speed to speed received in YardkAcknowledge message
                                    uint32_t curCeilingSpeed = yardAck.allowedSpeedInYard;
                                     DS::AbstractTargets::corePtr()->setCurCeilingSpeed(curCeilingSpeed)
                                    ..
                                    ..

                                   \endcode

                                + In ShuntModeRequestSeq class
                                   +Pseudo Code:
                                   \code
                                    void ShuntModeRequestSeq::runShuntDmiButtonWaitForAck()
                                    {
                                      ShuntingAcknowledge shuntingAck;

                                      if (RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus())
                                      {
                                         seqState = shuntDmiButtonTCCTimeout;
                                         ATC::AbstractEventHandler::corePtr()->reportEvent(modTCCNotAvail, __FILE__, __LINE__);
                                       }
                                      else
                                       {
                                         //apply standstill event
                                          ATC::AbstractEventHandler::corePtr()->reportEvent(waitForModAckStandStill, __FILE__, __LINE__);

                                          if (AbstractMessageHandler::corePtr()->getShuntingAcknowledge(shuntingAck))
                                           {
                                             if (RequestAcknowledged == shuntingAck.shuntingAcknowledge)
                                              {
                                                //Code for requirement AOS 2119 set current ceiling speed to speed received in ShuntingAcknowledge  message
                                                uint32_t curCeilingSpeed = shuntingAck.allowedSpeedInShunting;
                                                DS::AbstractTargets::corePtr()->setCurCeilingSpeed(curCeilingSpeed)
                                     ..
                                     ..
                                     \endcode

                                     + In PosModeRequestSeq class
                                     +Pseudo Code:
                                     \code
                                        void PosModeRequestSeq::runPosDmiButtonWaitForAck()
                                        {
                                          //temp variable for Possession ACK
                                            PossessionAcknowledge possessionAck;

                                            if (RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus())
                                             {
                                              seqState = posDmiButtonTCCTimeout;
                                              ATC::AbstractEventHandler::corePtr()->reportEvent(modTCCNotAvail, __FILE__, __LINE__);
                                             }
                                            else
                                            {
                                             //standstill event
                                                ATC::AbstractEventHandler::corePtr()->reportEvent(waitForModAckStandStill, __FILE__, __LINE__);

                                            if (AbstractMessageHandler::corePtr()->getPossessionAcknowledge(possessionAck))
                                            {
                                              if (RequestAcknowledged == possessionAck.possAcknowledge)
                                              {
                                                  //Code for requirement AOS 2119 set current ceiling speed to speed received in PosModeRequestSeq  message
                                                  uint32_t curCeilingSpeed = possessionAck.allowedSpeedInPossession;
                                                 DS::AbstractTargets::corePtr()->setCurCeilingSpeed(curCeilingSpeed)
                                     ..
                                     ..
                                     \endcode


\subsection CeilingSpeedSupvReqAOS2120 Ceiling Speed Supervision Req AOS 2120
  - Affected component & Method: Mode Control & YardModeRequestSeq:YardModeRequestSeq::runYardDmiButtonTCCTimeout()
  - Requirement Covered: 2120
     + Description of Changes: Below changes are required in YardModeRequestSeq class for this requirement

  +Pseudo Code:
  \code
  void YardModeRequestSeq::runYardDmiButtonTCCTimeout()
    {
      //Yard mode expiry
      if (RadioCom::AbstractRadioHandler::corePtr()->isYardModeTimerExpired())
      {
        seqState = yardDmiButtonConfirmed;
        //Code for requirement AOS 2120 set current ceiling speed with configured yard mode speed (TCC is not connected)
        uint32_t curCeilingSpeed = AbstractConfig::corePtr()->getYardSpeed();
        DS::AbstractTargets::corePtr()->setCurCeilingSpeed(curCeilingSpeed)
      }
      else
      {
        if (!RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus())
        {
          seqState = yardDmiButtonPressed;
        }
      }
    }
  \endcode

\subsection CeilingSpeedSupvReqAOS2121 Ceiling Speed Supervision Req AOS 2121
  - Affected component & Method: Mode Control & TrainRegistrationMode::runTrainRegistrationSetupBSTarget()
  - Requirement Covered: 2121
    + Description of Changes: This requirement is already implemented.
                             + in Registration Mode current ceiling speed is getting set from config parameter.
     +Pseudo Code:
      \code
      void TrainRegistrationMode::runTrainRegistrationSetupBSTarget()
      {
      ..
      ..
      ..
      //set the current ceiling speed to the balise search speed.
      DS::AbstractTargets::corePtr()->setCurCeilingSpeed(
      static_cast<uint32_t>(AbstractConfig::corePtr()->getBalSearchSpeed()));
      ..
      ..

      }
      \endcode

\subsection CeilingSpeedSupvReqAOS229 Ceiling Speed Supervision Req AOS 229
  - Affected component: Targets
  - Requirement Covered: 229
     + Description of Changes: This requirement should be implemented in Targets.
                               + In handlePassedTarget we need to set current ceiling speed with received speed in location target.
                                But this requirement will be implemented completely when location mode requirement will be implemented.

\subsection CeilingSpeedSupvReqAOS227 Ceiling Speed Supervision Req AOS 227 S
  - Affected component & Method: Target & SpeedTarget::handlePassedTarget()
  - Requirement Covered: 227 S
    + Description of Changes: Need to do below changes in SpeedTarget::handlePassedTarget() method:

    +Pseudo Code:
    \code
    bool SpeedTarget::handlePassedTarget()
    {
    bool handleTarget = true;

    if(Pos::PosKnown == Pos::AbstractPosition::corePtr()->getAccuracyState())
    {
    //setting current ceiling speed with speed target
    AbstractTargets::corePtr()->setCurCeilingSpeed(getCeilingSpeed());
    }

    return handleTarget;
    }
    \endcode

\subsection CeilingSpeedSupvReqAOS2122 Ceiling Speed Supervision Req AOS 2122
  - Affected component: Target Calculation
  - Requirement Covered: AOS 2122
     + Description of Changes: This requirement is already implemented in target calculation in AbstractTargetCalculation::calculateCeilingSpeed() method.

\subsection CeilingSpeedSupvReqAOS676 Ceiling Speed Supervision Req AOS 676
  - Affected component: Target Calculation
  - Requirement Covered: AOS 676
     + Description of Changes: This requirement is already implemented in Brake Calculation.

\subsection CeilingSpeedSupvReqAOS2123 Ceiling Speed Supervision Req AOS 2123, AOS 2249, AOS 343 S & AOS 345 S
  - Affected component: Supervision & AbstractSupervise::superviseCeilingSpeed()
  - Requirement Covered: AOS 2123, AOS 2249, AOS 343 S & AOS 345 S
     + Description of Changes: Following changes need to be done in AbstractSupervise::superviseCeilingSpeed() method:

     +Pseudo Code:
     \code

     void AbstractSupervise::superviseCeilingSpeed(const uint32_t currentSpeed, const uint32_t calcCeilingSpeed)
     {
     .
     .
     .
     uint32_t alarmLimit = BrakeCalculations::instance().calcAlarmLimitMargin(calcCeilingSpeed);
     uint32_t sbLimit = BrakeCalculations::instance().calcServiceBrakeLimitMargin(calcCeilingSpeed);
     uint32_t ebLimit = BrakeCalculations::instance().calcEmergencyBrakeLimitMargin(calcCeilingSpeed);

     if (0U != calcCeilingSpeed)
     {

     if (currentSpeed >= (calcCeilingSpeed + ebLimit)) //If current speed exceeds the emergence brake speed AOS 345S
     {
     ATC::AbstractEventHandler::corePtr()->reportEvent(emergencyBrakeCeilingSpeed, __FILE__, __LINE__);
     alarm = (alarm < AlarmTwoBeepsPerSec) ? AlarmTwoBeepsPerSec : alarm;   // Making sure an alarm with higher priority is not overwritten
     atpIntervention = true;
     }
     else if (currentSpeed >= (calcCeilingSpeed + sbLimit)) //if current speed exceeds the service brake speed limit AOS 343 S
     {
     ATC::AbstractEventHandler::corePtr()->reportEvent(serviceBrakeCeilingSpeed, __FILE__, __LINE__);
     alarm = (alarm < AlarmTwoBeepsPerSec) ? AlarmTwoBeepsPerSec : alarm;   // Making sure an alarm with higher priority is not overwritten
     atpIntervention = true;
     }
     else if (currentSpeed >= (calcCeilingSpeed + alarmLimit)) //If current speed exceeds the warning speed limit AOS 2249
     {
     //Raise continuous beeps
     alarm = (alarm < AlarmTwoBeepsPerSec) ? AlarmTwoBeepsPerSec : alarm;   // Making sure an alarm with higher priority is not overwritten
     //Raise Log event
     atpWarning = true;
     }
     else if (currentSpeed > calcCeilingSpeed) // If current speed exceeds the current supervised ceiling speed AOS 2123
     {
     //Only a beep sound
     alarm = (alarm < AlarmTwoBeepsPerSec) ? AlarmTwoBeepsPerSec : alarm;   // Making sure an alarm with higher priority is not overwritten
     //Raise Log event
     atpWarning = true;
     }
     else
     {
     //Do nothing
     }

     }

     \endcode
     */