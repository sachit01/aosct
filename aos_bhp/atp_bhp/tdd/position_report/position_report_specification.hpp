/**
\if AsMainPage
\mainpage Technical Design Description for Position Report
\endif

\section VersionLog Version Log
Version | Date       | Description                                            | Signature
------- | --------   | -------------------------------------------------------|---------------
1.0     | 2017-08-02 | Document creation                                      | skothiya
2.0     | 2017-08-14 | Document updated for the peer review comments          | skothiya
3.0     | 2017-08-21 | Document updated for the final review comments         | skothiya
4.0     | 2017-08-23 | Document updated for the requirement AOS 172 redesign  | skothiya 
5.0     | 2017-08-29 | Document updated for the review comments               | skothiya

\section Abbreviations Abbreviation
Abbreviation   | Definition
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
MA             | Movement Authority
DMI            | Driver Machine Interface

\section Introduction Introduction
This document describes the technical design description of Position Report requirements for AOS BHP.
Document contains the description of new design or changes in existing design to implement below mentioned requirements.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix
Req      | Short requirement description                                                                                          | Description
------   | -------------------------------------------------------------------------------------- --------------------------------| ----------------------
AOS 637  | The AOS shall when receiving a PositionReportRequest message from the TCC respond by sending a PositionReport message. | Implemented
AOS 757  | The AOS shall replace the PositionReport message with any of the following when pending to be transmitted: AbortSetup, DriverInformation, MessageAcknowledge, StartUpMessage, TrainRegistrationInformation. | Partially Implemented
AOS 1246 | The AOS shall send to the TCC information on last passed balise when: ATP mode is Yard OR Staff Responsible OR Current position is Doubtful. | Not Implemented
AOS 1157 | If AOS is in Idling state then the speed indication in the position report shall always be zero. | Not Implemented
AOS 173  | All events shall be transmitted to the TCC in PositionReport message. | Partially Implemented
AOS 1156 | The AOS shall indicate Not ready to drive in PositionReport message if: Emergency Brake is active OR No active cabin OR Signal Locomotive Ready is NOT active. | Not Implemented
AOS 157  | The AOS shall indicate that it needs reset in PositionReport messages if the AOS has been running continously without reset for a time period longer than allowed running time. | Not Implemented
AOS 2081 | As long as the ATP reset status is set the AOS shall include AOS version block in the PositionReport message. | Not Implemented
AOS 2285 | The AOS shall, if the position is Known or Approximate, indicate in the D_BRAKE_DISTANCE field of Position Report message, the distance from train front end to the location where the Service brake curve for the Primary target restricts the current speed, if the current speed is less than the ceiling speed leading to the Primary Target. the ceiling speed leading to the Primary Target, if the current speed is more than the ceiling speed leading to the Primary Target. | Not Implemented
AOS 2578 | The AOS shall report the maximum value of D_BRAKE_DISTANCE in the Position Report message if: the position is Unknown OR Doubtful, OR the train is at standstill, OR no Primary Target exists. | Not Implemented
AOS 2448 | If the Train Name has been changed by the Driver, the AOS shall in the PositionReport message, include the TRAIN_NAME block with the new train name. | Not Implemented
AOS 92S  | When the ATP mode Shunting Route is entered the AOS shall report the position as if the complete registered train is present until a new TrainSetup message is accepted from the TCC in ATP mode Configuration. | Not Implemented
AOS 2216 | In ATP mode Balise Search the AOS shall report the current train position as Unknown. | Implemented
AOS 158  | The AOS shall send to the TCC in PositionReport message cleared positions (D_POSITION=0 AND NID_TRACK=0) if the current position is Unknown. | Partially Implemented
AOS 163  | The AOS shall use the MA Track data as reference in the position reports. All positions shall be given by Track Id and the distance, in cm, from the defined start of the Track. | Implemented
AOS 160S | The AOS shall send to the TCC in PositionReport message the train front end and rear end positions in travelling direction of the MA if the current position is NOT Unknown. | Implemented
AOS 172  | Following a balise passage that clears the slip indication (even if the slip indication is set again directly after the passage), the AOS shall ensure to send a PositionReport message to the TCC with the following data: Cleared slip indication, AND Updated rear end position. | Not Implemented
AOS 2076S| If the train integrity is NOT supervised, then the positions in the position report shall be based on configured train length. | Not Implemented
AOS 2078S| The AOS shall update the rear end position when: The train integrity is supervised AND confirmed by the TIMS OR The train integrity supervision is inhibited AND confirmed by the Driver. | Partially Implemented
AOS 2181 | The AOS shall update the rear end position continuously when: The train integrity is NOT supervised AND The train integrity supervision is NOT inhibited. | Not Implemented



\subsection ExternalInterfaceDescription External Interface Description
Following external interface description is used to implement the requirement described above.
Seq No | Document Name                                      | Version
-------|----------------------------------------------------|----------
1.     | Interface Specification ATP - MMI                  | 2.4
2      | FFFIS TCC-AOS                                      | 5.8

\section Assumption Assumption
Pseudo code used in the document is only to understand the logic, please ignore lint and  syntax errors in peudo codes.
Also comments used in pseudo codes are not according to the coding guidelines, it is more descriptive to understand the logic.

\section DetailedDescriptionOfDesign Detailed Description of Components Design/Changes for requirements
  This section describes affected components and methods with logic need to add/change to implement the different requirements of Position Report in ATP.

\subsection PositionReportReqAOS757 Requirement AOS 757
  This requirement is already implemented.
  AOS is sending PositionReport only when there is no other message in queue.

\subsection PositionReportReqAOS1246 Requirement AOS 1246
  This requirement is already implemented, AOS is sending passed balise information in all the cases.
  Also in Yard or Staff Responsible mode and if current position is Doubtfull.

\subsection PositionReportReqAOS1157 Requirement AOS 1157
  - Affected component & Method : Message Handler & RadioMessageOutPositionReport::collectData()
    + Description of Changes: Modify collectData() method in RadioMessageOutPositionReport class to fill positionReport.trainSpeed = 0U if train is in idle state.
    + Pseudo code:
     \code
      if(AbstractModeControl::corePtr()->isIdleState())
      {
       //In idle state train speed reported should be 0
       positionReport.trainSpeed = 0U;
      }
      else
      {
       // Get current train speed
       positionReport.trainSpeed = Pos::AbstractOdometry::corePtr()->getSpeed();
      }
     \endcode

\subsection PositionReportReqAOS173 Requirement AOS 173
  According to current implementation AOS is reporting all the events to TCC.
  AOS is only reporting event numbers not the event texts to TCC.

\subsection PositionReportReqAOS1156 Requirement AOS 1156
  - Affected component & Method: Message Handler & RadioMessageOutPositionReport::collectTrainStatusInfo()
     + Description of Changes: Modify collectTrainStatusInfo() method in RadioMessageOutPositionReport class to set positionReport.trainCoreStatus 
       (bit 17, Not Ready to drive (ATO operation only) if Emergency Brake is active OR No cabin is active OR Locomotive Ready signal is not active.
     + Pseudo code:
      \code
        bool isCabAActive;
      bool isCabBActive;

      //Get cab active input from LocoIO
      if (!(IO::AbstractLocoIO::corePtr()->
        getCoreDigitalInputValue(IO::AbstractLocoIO::Cab1, &isCabAActive)))
      {
        //error
        isCabAActive = false;
      }

      if (!(IO::AbstractLocoIO::corePtr()->
        getCoreDigitalInputValue(IO::AbstractLocoIO::Cab1, &isCabAActive)))
      {
        //error
        isCabBActive = false;
      }

      //Checking for LCS ready 
      bool isLcsReady;
      if (!(IO::AbstractLocoIO::corePtr()->
        getCoreDigitalInputValue(IO::AbstractLocoIO::LCSRdy,&isLcsReady)))
      {
        //error
        isLcsReady = false;
      }

      //Check if Emergency brake applied
      bool isEBApplied = Supv::AbstractBrake::corePtr()->getEbApplied();

      if (!(isCabAActive || isCabBActive)
          || (!isLcsReady) || isEBApplied)
      {
        //set Bit 17, Not ready to drive
        trainCoreStatus |= trainStatusNotReadyToDriveATO;
      }
      \endcode

\subsection PositionReportReqAOS175 Requirement AOS 175
  - Affected component & Method: Message Handler & RadioMessageOutPositionReport::collectTrainStatusInfo()
     + Description of Changes: Modify collectTrainStatusInfo() method in RadioMessageOutPositionReport class to set 9th bit(ATP needs to be reset) of positionReport.trainCoreStatus.
                               To set 9th bit(ATP needs to be reset) ATP need to check if current up time of AOS/ATP is more then configured allowed running time.
                               This requirement will be fully implemented with the implementation of new requirement AOS 666.
                               In scope of this TDD, only the bit 9 will be set.
      + Pseudo code:
      \code
      //TODO: Need to check condition, condition will be implemented after implementation of requirement AOS 666
      //if(AOS running time > Configured allowed running time)
      //{
       // Bit 9, Not ready to drive
       //trainCoreStatus |= trainStatusATPNeedsRestart;
      //}
      \endcode

\subsection PositionReportReqAOS2081 Requirement AOS 2081
  - Affected component & Method: Message Handler & RadioMessageOutPositionReport::collectData()
     + Description of Changes: Modify collectData() method in RadioMessageOutPositionReport class to fill positionReport.aosVersionVec only when ATP reset status is set.
      + Pseudo code:
      \code
      if(AbstractModeControl::corePtr()->getATPReset())
      {
        // Get AOS version
        AosVersion aosVersion;
        getAosVersion(aosVersion);
        positionReport.aosVersionVec.push_back(aosVersion);
      }
      \endcode

\subsection PositionReportReqAOS2285 Requirement AOS 2285 & AOS 2578
  - Affected component & Method: Message Handler & RadioMessageOutPositionReport::collectData()
     + Description of Changes: Modify collectData() method in RadioMessageOutPositionReport class to fill positionReport.brakeDistance according to below pseudo code.
      + Pseudo code:
      \code
      bool primaryTargetNotExist = false;
      if (NULL == DS::AbstractTargets::corePtr()->getPrimaryTarget())
      {
        primaryTargetNotExist = true;
      }

      bool isTrainStatndStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      Pos::PosAccuracyState currentPos = Pos::AbstractPosition::corePtr()->getAccuracyState();
      if ((Pos::PosUnknown == currentPos || Pos::PosDoubtfull == currentPos)
      || isTrainStatndStill || primaryTargetNotExist)
      {
      //set brake distance to maximum value
      positionReport.brakeDistance = maxBrakeDistance;
      }
      else if (Pos::PosKnown == currentPos || Pos::PosApprox == currentPos)
      {
        // Get Brake Distance
        positionReport.brakeDistance = static_cast<uint32_t>(Supv::AbstractSupervise::corePtr()->getDistanceToBCA());
      }
      else
      {
        //do nothing 
      }
      \endcode

\subsection PositionReportReqAOS2448 Requirement 2448
  - Affected component & Method: Message Handler & RadioMessageOutPositionReport::collectData()
     + Description of Changes: Modify collectData() method in RadioMessageOutPositionReport class to fill positionReport.trainNameVec when train name is changed by Driver.
      + Pseudo code:
      \code
      //TODO:: currently Train Name Changed by DMI is not implemented so this requirement will be implemented fully when requirement related to change train name from DMI will be implemented
      if(DS::AbstractTSetup::corePtr()->trainNameChangedByDMI())
      {
       if (DS::AbstractTSetup::corePtr()->getTrainName(&trainNameData.trainName[0]))
        {
          positionReport.trainNameVec.push_back(trainNameData);
        }
        }

      \endcode

\subsection PositionReportReqAOS158 Requirement AOS 158
  - Affected component & Method: Message Handler & RadioMessageOutPositionReport::collectData()
    + Description of Changes: Modify collectData() method in RadioMessageOutPositionReport class to fill positionReport.leadingTrackAndPosition & positionReport.trailingTrackAndPosition according to below pseudo code.
      + Pseudo code:
      \code
        if (positionReport.positionClassification == Pos::PosUnknown)
      {
        //set front and rear position to clear (D_POSITION=0 AND NID_TRACK=0) if the current position is UnKnown
        positionReport.trailingTrackAndPosition.track = 0U;
        positionReport.trailingTrackAndPosition.position = 0;
        positionReport.leadingTrackAndPosition.track = 0U;
        positionReport.leadingTrackAndPosition.position = 0;
      }
      else
      {
        // Get rear position
        positionReport.trailingTrackAndPosition = Pos::AbstractPosition::corePtr()->getCurrRearPos();

        // Get front position
        positionReport.leadingTrackAndPosition = Pos::AbstractPosition::corePtr()->getCurrFrontPos();
      }
      \endcode

\subsection PositionReportReqAOS92 Requirement AOS 92 S
          The current implementation is according to the requirement.
          AOS is using the existing train setup to calculate the rear end position of train in shunting route mode.

\subsection PositionReportReqAOS172 Requirement AOS 172
  -1 Affected component & Method: Message Handler & AbstractMessageHandler::runOut()
    + Description of Changes: According to requirement AOS has to report the slip clear status to TCC, so to protect the condition where slip is cleared in T1+1 cycle.
                              But due to other message in queue, PR message is not sent to TCC in T1+1 cycle and in T1+2 cycle slip is again detected then to report slip clear status.
                              Modify collectData() method in AbstractMessageHandler class as mention in below steps:
                              + Define a new private data member isSlipClearStatusReportedToTCC (bool) in AbstractMessageHandler class with default value true.
                              + ackDefaultPositionReportReceived defined in AbstractMessageHandler class is used to check if PR is sent to TCC or not.
                              + Need to add below mentioned pseudo code in collectData() method to resend front/rear end position and clear slip status. 
                              + Front/rear end position data is not getting invalidated at each cycle therefore no need to store the value in separate variable.
                                Only current value of font and rear end of train will not be fetched till PR is not sent to TCC. 
    + Pseudo code:
     \code
      void RadioMessageOutPositionReport::collectData()
    {
 
      if (!isSlipClearStatusReportedToTCC && ackDefaultPositionReportReceived)
      {
        isSlipClearStatusReportedToTCC = true;
      }

      DS::TrainSetup trainSetup;
      Pos::PosAccuracyState currentPos = Pos::AbstractPosition::corePtr()->getAccuracyState();
      if (Pos::PosUnknown == currentPos)
      {
        //set front and rear position to clear (D_POSITION=0 AND NID_TRACK=0) if the current position is UnKnown
        positionReport.trailingTrackAndPosition.track = 0U;
        positionReport.trailingTrackAndPosition.position = 0;
        positionReport.leadingTrackAndPosition.track = 0U;
        positionReport.leadingTrackAndPosition.position = 0;
      }
      else if(isSlipClearStatusReportedToTCC)
      {
        // Get rear position
        positionReport.trailingTrackAndPosition = Pos::AbstractPosition::corePtr()->getCurrRearPos();

        // Get front position
        positionReport.leadingTrackAndPosition = Pos::AbstractPosition::corePtr()->getCurrFrontPos();
      }
      else
      {
        //in case of slip status clear AOS will send front/rear position of slip status clear
      }
      .
      .
      .
      .
      .
      // Get train-status
      collectTrainStatusInfo(positionReport.trainCoreStatus);

      if (!isSlipClearStatusReportedToTCC)
      {
        //Masking the value of bit 13 (slip status)
        positionReport.trainCoreStatus &= 0XFFFDFFF;
      }

      \endcode

      + To make isSlipClearStatusReportedToTCC false below pseudo code need to be added in RadioMessageOutPositionReport::validate() method:
      \code
      bool RadioMessageOutPositionReport::validate()
      {
      
      if ((isSlipClearStatusReportedToTCC) && (Pos::AbstractOdometry::corePtr()->isSlipStatusClear()))
      {
      isSlipClearStatusReportedToTCC = false;
      }

      }
      }

      return (DataValidated == dataProcessState);
      }
     \endcode

  -2  Affected component: AbstractOdometry
    + Description of Changes: 
                             + Need to add new private parameter isSlipClear in AbstractOdometry class.
                             + Need to add new public method AbstractOdometry::isSlipStatusClear() in AbstractOdometry class, it will return value of isSlipClear flag.
                             + isSlipClear will be set by AOS when slip status gets clear on balise passes.
                             + isSlipClear will be reset in run() method of AbstractOdometry class in each cycle.
                             + Need to modify AbstractOdometry::updateBaliseWindow() method to set isSlipClear flag.
                             \code
                             //Clearing slip/slid status as expected balise window found
                             odoData.isSlipping = false;
                             //Below code will be added in AbstractOdometry::updateBaliseWindow() method.
                             if (odoData.isSlipping)
                             {
                              //Setting slip clear
                              isSlipClear = true;
                             }
                             \endcode
                             
\subsection PositionReportReqAOS2076 Requirement AOS 2076 S, AOS 2078 S, AOS 2181
  According to the current implementation TIMS is not providing position to AOS.
  Also it is not mentioned in requirement that TIMS will provide the position.
  So these 3 requirement will be implemented once requirement related to TIMS will be clear.

      */