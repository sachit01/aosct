/**
\if AsMainPage
\mainpage Technical Design Description for Train State
\endif

\section VersionLog Version Log
Version | Date       | Description                                            | Signature
------- | --------   | -------------------------------------------------------|---------------
1.0     | 2017-05-09 | Document creation                                      | skothiya
2.0     | 2017-05-19 | Document updated for peer review comments              | skothiya
3.0     | 2017-05-25 | Document updated after requirement clarification       | skothiya
4.0     | 2017-05-29 | Document updated for review comment                    | skothiya
5.0     | 2017-05-30 | Document updated for the second review comments        | skothiya
5.1     | 2017-05-31 | MAtimeout logic is updated                             | skothiya
6.0     | 2017-06-02 | Document updated for redesigning                       | skothiya
7.0     | 2017-06-06 | Document updated for the review comments               | skothiya
8.0     | 2017-06-07 | Document updated for the review comments               | skothiya
9.0     | 2017-06-12 | Doucment updated for the review comments               | skothiya

\section Abbreviations Abbreviation
Abbreviation   | Definition
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
MA             | Movement Authority
DMI            | Driver Machine Interface

\section Introduction Introduction
This document describes the technical design description of Train State for AOS BHP.
Document contains the description of new design or changes in existing design to implement below mentioned requirements.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix
Req      | Short requirement description                                                                                                                        | Description
------   | -------------------------------------------------------------------------------------- ---------------------------------------------------           | ----------------------
AOS297 S | The AOS shall set the Idle status when: Primary Target doesn't exist AND Mode is one of: Normal, Staff Responsible, Split, Join OR Shunting Route    | Partially Implemented
AOS1723 S| When the Idle status is set the AOS shall: Issue a Brake event (continuously) AND Clear the Free Rolling status......................................| Partially Implemented
AOS1595 S| The AOS shall clear the Idle status when: Primary Target exists AND mode is one of: Normal, Staff Responsible, Split, Join ..........................| Partially Implemented
AOS672 S | The AOS shall set the MA timeout status when vehicle is at standstill AND vehicle front end is NOT within the margin at the end of an MA ............| Partially Implemented
AOS2152  | The AOS shall clear the MA timeout status when: Primary Target exists AND mode is one of: Normal, Staff Responsible, Split, Join OR Shunting Route...| Not Implemented
AOS458   | The AOS shall accept StopTrain message from the TCC and on acceptance set the Stop Train status in the following ATP modes: Normal, StaffResponsible | Partially Implemented
AOS461   | The AOS shall inform the Driver about the reason for Stop Train..                                                                                    | Not Implemented
AOS398   | The AOS shall offer the Driver the possibility to set the Emergency Alert status in the following ATP modes: Balise search, Normal,..................| Partially Implemented
AOS1604  | When passing OR already passed (when the MA is accepted) the position indicating start of the Free Rolling Area AND the ATP mode is Normal...........| Not Implemented
AOS1147  | When the Free Rolling status is set the AOS shall: Disable the Roll-Away Supervision.                                                                | Not Implemented
AOS1605  | When passing OR already passed (when the MA is accepted) the position indicating end of the Free Rolling Area AND the ATP mode is Normal OR          | Not Implemented
AOS269   | When passing OR already passed (when the MA is accepted) the position indicating start of the Odometer Invalid Area AND the ATP mode is Normal.......| Not Implemented
AOS1603  | When passing OR already passed (when the MA is received) the position indicating end of the Odometer Invalid Area AND the ATP mode is Normal.........| Not Implemented
AOS273   | If the Odometer Invalid status is set AND the Free Rolling status in NOT set the AOS shall issue a Safe Brake To Stop event.                         | Not Implemented
AOS187   | The AOS shall set the Slip status when: Odometry system indicates Slip OR The rear end confidence interval in the odometry system increments.........| Partially Implemented
AOS190   | The AOS shall clear the Slip status when an expected balise telegram is received.                                                                    | Partially Implemented
AOS2104  | The AOS shall set the Slide status when: Odometry system indicates Slide OR The front end confidence interval in the odometry system increments......| Partially Implemented
AOS2105  | The AOS shall clear the Slide status when an expected balise telegram is received.                                                                   | Not Implemented
AOS2126  | When passing OR already passed the position indicating start of the Keep Track Data Area the AOS shall:Set the Keep Track Data status.               | Not Implemented
AOS2128  | The AOS shall clear the Keep Track Data status when: Leaving ATP mode Normal, OR Emergency Alert status is set, OR Stop Train Status is set.         | Not Implemented
AOS2254  | The AOS shall set the Handling Done status when: The ATP Mode is Location, AND The ATO Mode is ATO Manual, AND A request is received from the driver.| Not Implemented
AOS2255  | The AOS shall delete all targets, when the Handling Done status is set.                                                                              | Not Implemented
AOS2256  | The AOS shall clear the Handling Done status when: New MA has been accepted OR Stop Train status is set OR Emergency Alert status is set.            | Not Implemented


\subsection ExternalInterfaceDescription External Interface Description
Following external interface description is used to implement the requirement described above.
Seq No | Document Name                                      | Version
-------|----------------------------------------------------|----------
1.     | Interface Specification ATP - MMI                  | 2.4
2      | FFFIS TCC-AOS                                      | 5.8

\subsection TrainStateWorking Design Change Overview:
  - Different train states will be implemented in different components. Below is the list of train state and corresponding component
    + Mode Control: Below train states will be handled in mode control:
       + Idle
       + MA timeout
       + Stop Train
       + Handling Done\n
       \n
      According to current design, train status (states) handled in mode control component are defined as a separate functions in AbstractModeControl class.
      To organize (manage) the train status related functionality in mode control component state related function will be implemented in corresponding mode class.
      Below will be the design
      Other then above states Emergency Alert is managed as a separate class EmergencyAlertSeq in Mode Control.
      Below is the design to handle train states in modecontrol component:
       @image html modecontrol_train_state_hanlding.png
       @image latex modecontrol_train_state_hanlding.png

       Methods related to handling train states will be override in particular mode classes where different handling is required for the states.
       For example manageMATimout() and manageTrainIdling() methods will be overloaded in YardMode class to clear the TrainIdle and MATimeout states.
       \n
    + Odometry: Below two Train Status (States) will be handled in Odometry component
      + Slip
      + Slide
      \n
    + Target: Below Train Status (states) will be handled in Target component
       + Free Rolling
       + Odometer Invalid
       + KeepTrackData
       \n
       \n
         Below changes are required in Target component to handle the above states.
         + Rename function removePassed(const OdoPosition frontPos) to handlePassedTargets() in AbstractTarget class. 
          handlTargetPassed() will implement below logic:
          + Pseudo code for the handlePassedTargets:\n
          \code
          void AbstractTargets::handlePassedTargets()
          {
           for (abstractTargetIterator it = targetList.begin(); (it != targetList.end());)
           {
            if(pTarget->isTargetPassed())
            {
                         // handlePassedTarget() returns true if target shall be deleted
              if (pTarget->handlePassedTarget())
              { 
                delete(*it);
                it = targetList.erase(it);
              } 
            }
           }
         }
          \endcode
          
         + Define two new virtual methods bool handlePassedTarget() and bool isTargetPassed() in BaseTarget class.
         + Need to override the handlePassedTarget()and isTargetPassed() methods in child target class if different checks need to be done for the target.
          + Override isTargetPassed() in PrimaryTarget class to check if train passed the MAMargin and also override method handlePassedTarget() to check condition if primary target can be deleted.
            Like Primary targets can not be deleted in ATPModeLocation.
          + For TrackDataItemTarget class, method handlePassedTarget() will be overridden to set OdometerInvalid and FreeRolling status.
            OdometerInvalid and FreeRolling status will be define in AbstractTarget class and the references will be set in TrackDataItemTarget class during the constructor call.
          + For SpeedTarget, GradientTarget and BrakeabilityTarget classes will call BaseTarget::handlePassedTarget() method which will also call setCurCeilingSpeed, setCurGradient, setCurBrakeability methods of 
            AbstractTargets class to set current ceiling speed, gradient and brakeability.
          + For KeepTrackData please refer to section Keep Track Data State.
          + Need to change AbstractTargets::getPrimaryTarget() logic to find primary target.
            The traversing of the target list will be start from the end to find primary target, as primary target usually stored at the last position in list.
         
            \code
            BaseTarget* AbstractTargets::getPrimaryTarget()
            {
             BaseTarget* pTarget = static_cast<BaseTarget*>(NULL);
             for (abstractTargetIterator targetIt = targetList.end(); targetIt != targetList.begin(); --targetIt)
             {
              if ((*targetIt)->getTargetType() == BaseTarget::PrimaryTarget)
              {
               pTarget = (*targetIt);
              }
             }
             return pTarget;
            }
            \endcode
         
\section DetailedDescriptionOfDesign Detailed Description of Components Design/Changes for requirements
  This section describes affected components and function with logic need to add/change to implement the different requirements of Train Setup in ATP.

\subsection IdleTrainState Idle Train State
  - Requirement Covered: AOS297S, AOS1723S, AOS1595
  - Affected component :  Mode Control
    + Description of Changes: New method manageTrainIdling() need to be implemented in AbstractMode class.
      Call manageTrainIdling() from handleMode() function of below classes:
       + NormalMode
       + StaffResponsibleMode
       + ShuntingRouteMode
       + SplitMode
       + JoinMode

      Below is the logic for the manageTrainIdling function:
      @image html idle_train_state_flow.png
      @image latex idle_train_state_flow.png

      In below modes manageTrainIdling() function need to be override in particular mode class to clear the isTrainInIdleState.
      Power Up, Powering Down, Unregister, Registration, Automatic unload, Location, Safety Halt, Safe Brake to Stop, Configuration,
      Basile Search, Sleeping, Yard, Possession or shunting.
      \n
      + Pseudo code for one of above mode
      \code
      void YardMode::manageTrainIdling(ModeRefTrainStates & trainState)
      {
        //clear TrainIdle state in Yard Mode
       trainState.isTrainInIdleState = false;
      }
      \endcode

      + For Requirement AOS1723 S, to clear Odometer Invalid and Free Rolling train states, below code need to add in AbstractTargets::run() method.
       \code
       If (AbstractModeControl::corPtr()->isIdleState())
       {
       OdometerInvalid = false;
       FreeRolling = false;
       }
       \endcode
\subsection MAtimeout MA Timeout
  - Requirement Covered: AOS672S, AOS305, AOS2152
  - Affected component :  Mode Control
     + Description of Changes: Need to define a new method manageMaTimeout() in new AbstractMode class.
       This function will be called from handleMode() function of below classes:
       + NormalMode
       + StaffResponsibleMode
       + ShuntingRouteMode
       + SplitMode
       + JoinMode

       Below is the logic for the manageMaTimeout function:
       @image html matimeout_train_state.png
       @image latex matimeout_train_state.png

       In below modes manageMaTimeout() function need to be override in particular mode class to clear the isTrainInIdleState.
       Power Up, Powering Down, Unregister, Registration, Automatic unload, Location, Safety Halt, Safe Brake to Stop, Configuration,
       Basile Search, Sleeping, Yard, Possession or shunting.
          \n
       + Pseudo code for one of above mode
       \code
        void YardMode::manageMaTimeout(ModeRefTrainStates & trainState)
        {
          //Clear MATimoute state in yard mode
          trainState.isTrainInMATimeoutState = false;
        }
        \endcode

\subsection StopTrain Stop Train
  - Affected component: Mode Control
    + Description of Changes: Move checkForStopTrain() method from AbstractModeControl to AbstractMode class and rename this method to manageStopTrain().
      And call this function from the handleMode() method of below classes:
      + NormalMode
      + StaffResponsibleMode
      + ShuntingRouteMode
      + SplitMode
      + JoinMode
      + LocationMode
      + AutomaticUnload

  - Requirement: AOS458
  - Affected component & Method: DMI Handler & RadioMessageInStopTrain::validateMode()
    + Description of Changes: Need to change RadioMessageInStopTrain::validateMode() method to allow StopTrain message in
      Normal, Staff Responsible, Shunting Route, Location, Automatic Unload, Split and Join modes.

  - Requirement: AOS461
  - Affected component & Method: DMI Handler & DMIMessageOutTextMessage::collectData()
    + Description of Changes: Need to add logic to fetch StopTrain Reason from MessageHandler and add reason in textCommandMessage.\n
                              Add else if (Kernel::AbstractMessageHandler::corePtr()->getStopTrain(stopTrainReason)) at line number 142 of dmi_message_out_text_message.
    + Pseudo Code:\n
    \code
    Kernel::StopTrainReason stopTrainReason;
    else if (Kernel::AbstractMessageHandler::corePtr()->getStopTrain(stopTrainReason))
      {
        //Text-message received from TCC in Command Message
        char_t indexInstring[maxDmiTextMsgIndexlen] = {'\0' };
        const int32_t retValue = 0;
          switch (stopTrainReason)
          {
           case Kernel::StopTrainUndefined:
            //fetch DMI event code for StopTrainUndefined reason
           //  retValue = snprintf(&indexInstring[0], maxDmiTextMsgIndexlen, "%u", DMI event code for Stop Train Undefined);
            break;

           case Kernel::StopTrainCancelled:
            //fetch DMI event code for StopTrainCancelled reason
            ////retValue = snprintf(&indexInstring[0], maxDmiTextMsgIndexlen, "%u", DMI event code for Stop Train Cancelled);
            break;

          default:
            break;\n
          }

          ////check if read value is greater than zero
          if (retValue >= 0)
          {
            ////Add the specifier:$#$
            static_cast<void>(vfw_strlcpy(&dmiTextMsgIndex[0], formatIndex, sizeof(dmiTextMsgIndex)));
            ////Concatenate the id with $#$
            static_cast<void>(vfw_strlcat(&dmiTextMsgIndex[0], &indexInstring[0], sizeof(dmiTextMsgIndex)));
            dmiDataProcessState = DMIDataAvailable;
          }

      }
      \endcode
      + Need to update language .ini file (English.ini) for the dmi event code for StopTrainUndefined and StopTrainCancelled reason.\n

\subsection EmergencyAlert Emergency Alert
    Emergency Alert is handled as a separate class in mode control. This implementation will be remain same.
  - Requirement: AOS398
  - Affected component & Method: Mode Control & runEmergencyAlertInactive()
    + Description of Changes: As mentioned in Doors for this requirement implementation is done already in DMI so need to add
      a condition in EmergencyAlertSeq::runEmergencyAlertInactive() method.
      Condition to show Emergency Alert status in Balise search, Normal, Staff Responsible, Shunting Route, Location, Automatic Unload Split and Join mode.
    + Pseudo Code:
    \code
    /////Set bit for Emergency Alert and Emergency Alert Active
    if((DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus()
          == DMICom::DMIButtonAlertButton)
           && (currentMode == ATPModeNormal) || (currentMode == ATPModeStaffResponsible)
        || (currentMode == ATPModeShuntingRoute) || (currentMode == ATPModeSplit)
        || (currentMode == ATPModeJoin) || (currentMode == ATPModeAutomaticUnload)
        || (currentMode == ATPModeLocation) || (currentMode == ATPModeBaliseSearch))
        {
         seqState = emergencyAlertReqDriver;
    }
    \endcode
\subsection FreeRolling Free Rolling & Odometer Invalid State
  - Requirement Covered: AOS1604, AOS1147, AOS1605, AOS269, AOS1603, AOS723
  - Affected component & function:  Message Handler & RadioMessageInMovementAuthority::publishData()
      + Description of Changes: Need to add a new method publishTrackDataItemTarget in RadioMessageInMovementAuthority class.
        This method will store the Track Data Item data received in MA into TrackDataItem type target.
        publishTrackDataItemTarget() will be get called from RadioMessageInMovementAuthority::publishData().

   - Affected component: Target
        + Description of Changes: Below two new private variable will be defined in AbstractTargets class:
            + istrainStatusOdometerInvalid : bool
            + istrainStatusFreeRolling: bool
          Two getter function for these variable will also be defined in AbstractTargets class.
            + bool isTrainInOdometerInvalidState()
            + bool isTrainInFreeRollingState()

   - Affected component: ModeControl
      + Description of Changes: AbstractTargets::handlePassedTarget() method will be called from the handleMode of NormalMode/LocationMode class.
                                As described in Genernal Design section handlePassedTarget() will activate/deactivate istrainStatusFreeRolling/istrainStatusOdometerInvalid
                                flags if condition met.


  - Affected component: ModeControl
      + Description of Changes: New method manageFreeRollingState() need to be defined in AbstractMode class.
                                This method will be called from the handleMode() method of NormalMode class.
        @image html freerolling_state_flow.png
        @image latex freerolling_state_flow.png

  - Affected component: Mode Control
     + Description of Changes:New method manageOdometerInvalid() will be implemented in AbstractMode class.
                              This method will be called from handleMode() method of NomalMode/LocationMode class.
     Below logic will be implemented in the method:
     @image html odometerinvalid_state_flow.png
     @image latex odometerinvalid_state_flow.png

\subsection SlipStatus Slip Status
  - Requirement Covered: AOS187
  - Affected component: Odometry & readMeasurementData()
    + Description of Changes: Need to create a new method isSlipDetected() in AbstractOdometry class and this method will be called from readMeasurementData().
     The slip status will be reset when balise window will be received. It will be implemented in void AbstractOdometry::updateBaliseWindow().
      Following logic will be implemented in method isSlipDetected():
      @image html slip_status_flow.png
      @image latex slip_status_flow.png

\subsection SlideStatus Slide Status
  - Requirement Covered: AOS2104
  - Affected component: Odometry & readMeasurementData()
    + Description of Changes: Need to create a new method isSlideDetected() in AbstractOdometry class and this method will be called from readMeasurementData().
     The slide status will be reset when balise window will be received. It will be implemented in void AbstractOdometry::updateBaliseWindow().
     Following logic will be implemented in method isSlideDetected():
     @image html slide_status_flow.png
     @image latex slide_status_flow.png

\subsection KeepTrackData Keep Track Data State
  - Requirement Covered: AOS2127
  - Affected component Targets
    + Description of Changes: Need to create a new Target type KeepTrackDataTarget class.
                              This class will override handlePassedTarget() and isTargetPassed() method.

     + Pseudo Code for isTargetPassed() & handlePassedTarget()
     \code

     bool KeepTrackDataTarget::isTargetPassed()
     {
      bool targetpassed = false;
      OdoPosition frontOdoPos = Pos::AbstractPosition::corePtr()->getCurrFrontPosOdo();
      OdoPosition targetPos = getOdometer();
      if (supposedTravelDir == DirForward)
      {
       //Check if front end of train passed the position
       if(targetPos <= frontOdoPos)
       {
        targetpassed = true;
       }
       else
       {
        //Do nothing
       }
     }
     else if (supposedTravelDir == DirReverse)
     {
     //Check if front end of train passed the position
      if (targetPos >= frontOdoPos)
      {
       targetpassed = true;
      
      }
      else
      {
       //Do nothing
      }
     }
     else
     {
     //do nothing
     }
      return targetpassed;
    }

    bool KeepTrackDataTarget::handlePassedTarget()
    {
    bool handleTarget = true;
    //Set keep track data
     DS::AbstractTracks::corePtr()->setKeepTrackData(true);
    return targetpassed;
    }
   \endcode

  - Affected component MessageHandler
    + Description of Changes: Need to create publishKeepTractData() method to store keepTrackDataInfo in KeepTrackDataTarget class.

  - Affected component ModeControl
    + Description of Changes: AbstractTargets::handlePassedTarget() method will be called from the handleMode of NormalMode class.
                                As described in General Design section handlePassedTarget() will activate Tracks::keepTrackDat

  - Requirement Covered: AOS2128
  - Affected component: Mode Control
    + Description of Changes: In NormalMode::resetMode() method of NormalMode class, disable KeepTrackData flag by calling DS::AbstractTracks::corePtr()->setKeepTrackData(false).
                              In EmergencyAlertSeq::runEmergencyAlertReqTCC() and runEmergencyAlertReqDriver() disable keepTrackData.
                              In manageTrainStop when stopTrainStatus is set disable the keepTrackData.

\subsection HandlingDone Handling Done
  - Requirement Covered: AOS2254, AOS2255
  - Affected component: Mode Control
    + Description of Changes: New method manageHanldingDone() will be implemented in AbstractMode class.
                              This method will be called from LocationMode::runLocationFinishOK().
       Below logic will gitbe implemented in method:
       @image html handling_done_state.png
       @image latex handling_done_state.png

  - Requirement Covered: AOS2254, AOS2255
  - Affected component: Mode Control
      + Description of Changes: Override method manageHanldingDone() in NormalMode class and will be called from handleMode() method.
                                In NormalMode::manageHanldingDone(), check if MA received and then set istTrainInHandlingDoneState = false.
                                Also need to reset IsTrainInHandlingDoneState = false from NormalMode::resetMode() method and from EmergencyAlertSeq::runEmergencyAlertReqTCC() and runEmergencyAlertReqDriver().
*/