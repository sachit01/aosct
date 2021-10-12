/**
\if AsMainPage
\mainpage Technical Design Description for Train Position
\endif

\section VersionLog Version Log
Version | Date       | Description                                            | Signature
------- | --------   | -------------------------------------------------------|---------------
1.0     | 2017-07-03 | Document creation                                      | skothiya
2.0     | 2017-07-14 | Document updated for the peer review comments          | skothiya
3.0     | 2017-07-19 | Document updated for the 2nd peer review comments      | skothiya
4.0     | 2017-07-24 | Document updated for the final review comments         | skothiya
5.0     | 2017-07-26 | Document updated for the second final review comments  | skothiya
6.0     | 2017-07-27 | Document updated for the final review comments         | skothiya

\section Abbreviations Abbreviation
Abbreviation   | Definition
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
MA             | Movement Authority
DMI            | Driver Machine Interface

\section Introduction Introduction
This document describes the technical design description of Train Position requirements for AOS BHP.
Document contains the description of new design or changes in existing design to implement below mentioned requirements.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix
Req      | Short requirement description                                                                                                                        | Description
------   | -------------------------------------------------------------------------------------- ---------------------------------------------------           | ----------------------
AOS 2266 | An ApproximatePosition message shall be checked for consistency. The message shall be rejected if it is not consistent. An ApproximatePosition message is consistent if: The TRACK_DATA in the message are continuous, AND The train footprint is within the tracks defined in TRACK_DATA. | Not Implemented
AOS 2268 | When the current position state is Unknown OR Doubtful, the AOS shall accept an ApproximatePosition Message if:The ATP mode is Registration AND following Re-Position procedure, OR The ATP Mode is Safe Brake To Stop AND the train is at standstill. | Not Implemented
AOS 183S | The AOS shall determine the motion direction and update the odometry position based on the defined train orientation.                                | Partially Implemented
AOS 185S | The AOS shall increment the odometry position in the trains's defined forward direction                                                              | Implemented
AOS 186S | The AOS shall use configured antenna position as the reference to front end and rear end of the vehicle for odometry position                        | Not Implemented
AOS 188S | The AOS shall adjust the position of the vehicle based on the reference position of the balise if: An expected balise is received withing allowed balise window AND current postion is Known. | Partially Implemented
AOS 189S | The AOS shall adjust the position of the vehicle based on the reference position of the balise if: An expected balise is received AND Slip is active AND Current position is Known OR Approximate. | Partially Implemented
AOS 2082 | The AOS shall adjust the position of the vehicle based on the position received from the TCC in ApproximatePosition message if the current position is Unknown OR Doubtful. | Not Implemented
AOS 2391 | The AOS shall issue a SafetyHalt event if: The current position is NOT Unknown, AND The train front AND rear positions are outside the stored tracks. | Partially Implemented
AOS 104  | The AOS shall classify its train position as Unknown on entry to ATP mode: Power Up,Balise Search, Yard, Shunting, Possession, Sleeping, Unregistered, Safety Halt, Powering Down. | Partially Implemented
AOS1305  | The AOS shall change the classification of the train position to Approximate when: Current position is Unknown OR Doubtful AND An ApproximatePosition message from TCC is accepted. | Not Implemented
AOS1304  | The AOS shall change the classification of the train position to Known when: Current position is Unknown AND ATP mode is Balise Search AND An expected balise is received (Re-registration procedure with 1 balise).| Implemented
AOS1313  | The AOS shall change the classification of the train position to Known when: Current position is Unknown AND ATP mode is Balise Search AND A MovementAuthority message from TCC is accepted AND The reported first balise is present in the received movement authority (Registration procedure with 2 balises).| Implemented
AOS 1306 | The AOS shall change the classification of the train position to Known when: Current position is Approximate AND An expected balise is received.     |Not Implemented
AOS 1307 | The AOS shall change the classification of the train position to Doubtful when: Current position is Known OR Approximate AND An unexpected balise is received. | Partially Implemented
AOS 1308 | The AOS shall change the classification of the train position to Doubtful when: Current position is Known AND Two consecutive expected balises were missed. | Implemented
AOS 1309 | The AOS shall change the classification of the train position to Doubtful when: Current position is Known AND An expected balise is received outside the allowed balise window AND Slip is NOT active AND Slide is NOT active. | Not Implemented
AOS 2094 | The AOS shall change the classification of the train position to Doubtful when: Current position is Known AND Calculated balise window is larger than the vehicle SAFETY_MARGIN. | Not Implemented
AOS 2390 | The AOS shall change the classification of the train position to Doubtful and issue a SafeBrakeToStop event if: The current position is Known OR Approximate, AND The current train front OR rear position is outside the stored tracks. | Partially Implemented


\subsection ExternalInterfaceDescription External Interface Description
Following external interface description is used to implement the requirement described above.
Seq No | Document Name                                      | Version
-------|----------------------------------------------------|----------
1.     | Interface Specification ATP - MMI                  | 2.4
2      | FFFIS TCC-AOS                                      | 5.8

\section Assumption Assumption
Pseudo code used in the document is only to understand the logic, please ignore lint and  syntax errors in peudo codes.
During the implementation please do not use same code correct code according to logic need to used.
Also comments used in pseudo codes are not according to the coding guideline it is more descriptive to understand the logic.
Comments also should not be used as it is during the implementation.

\section DetailedDescriptionOfDesign Detailed Description of Components Design/Changes for requirements
  This section describes affected components and function with logic need to add/change to implement the different requirements of Train Position in ATP.

\subsection ApproximatePosition Approximate Position
  - Requirement : AOS 2266, AOS 2268, AOS2082 & AOS1305

  - Affected component : Message Handler & RadioMessageInApproximatePosition::validateMode()
  + Description of Changes: Modify validateMode() method in RadioMessageInApproximatePosition class to accept the message only in Registration and Safe Brake To Stop modes.
                            + This method will also check if QSetupReason is RePosition in Registration mode.
                            + Method will reject the message if any condition does not meet.
  + Pseudo code:
  \code
  bool RadioMessageInApproximatePosition::validateMode() const
  {
  bool modeValid = false;
  ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
  switch (mode)
  {
  // Accept message in these modes
  case ATPModeStaffResponsible:
  case ATPModeSplit:
  case ATPModeUndefined:
  case ATPModePowerUp:
  case ATPModeConfiguration:
  case ATPModeBaliseSearch:
  case ATPModeNormal:
  case ATPModeLocation:
  case ATPModeJoin:
  case ATPModeShuntingRoute:
  case ATPModeAutomaticUnload:
  case ATPModePoweringDown:
  case ATPModeShunting:
  case ATPModeYard:
  case ATPModeUnregistered:
  case ATPModeSafetyHalt:
  case ATPModeSleeping:
  case ATPModePossession:
  case ATPModesCount:
  break;
  case ATPModeRegistration:
  {
  //ToDo: Need to check condition that train is in reposition procedure, the requirements are not yet implemented
  modeValid = true;
  break;
  }
  case ATPModeSafeBrakeToStop:
  modeValid = true;
  break;
  default:
  {
  trace->write(briefTrace, "Error in ATP Mode, No Such ATP Mode defined");
  writeToLog(ATC::BriefLog, "Error in ATP Mode, No Such ATP Mode defined");
  break;
  }
  }

  traceValidateMode(modeValid);

  return modeValid;
  }
  \endcode

  - Affected component : Message Handler
                         + Description of Changes: Add a new method validateApproxPosMessage() in RadioMessageInApproximatePosition class to check below conditions:
                                                   + If train is not in stand still then reject the message.
                                                   + If track data received in message are not continues or same track information received again then reject the message.
                                                   + This method will also check if track length behind received front end position of train is greater then train length or not.
                                                   + This method will also accept message only if current position is unkonwn or doubtfull, otherwise message will be rejected.
                                                   + If there will be any condition failed method will reject the message.
  + Pseudo code:
  \code
   bool RadioMessageInApproximatePosition::validateApproxPosMessage()
    {
      bool dataValid = true;
      std::vector<TrackData>::iterator trackIt;
      uint16_t lastTrackID = 0U;
      TravelDir supposedTravelDir = DirUndefined;
      bool isTrainStartFound = false;
      uint32_t trackLenTillFrontPos = 0U;
      //Check for Stand Still
      if (!Pos::AbstractOdometry::corePtr()->isTrainStandStill())
      {
        trace->write(detailedTrace, "Train is not stand still");
        dataValid = false;
      }

      if (dataValid)
      {
        //Check for Current Position
        Pos::PosAccuracyState currentPos = Pos::AbstractPosition::corePtr()->getAccuracyState();
        if ((currentPos == Pos::PosUnknown) || (currentPos == Pos::PosDoubtfull))
        {
          //Do nothing
        }
        else
        {
          trace->write(detailedTrace, "Current Pos is not Unknown or Doubtful ");
          dataValid = false;
        }

      }

      if (dataValid)
      {
        // Remove pure duplicates that are located next to each other
        static_cast<void>(unique(approxPosData.approxTrackDataVec.begin(), approxPosData.approxTrackDataVec.end()));
        for (trackIt = approxPosData.approxTrackDataVec.begin(); trackIt != approxPosData.approxTrackDataVec.end(); ++trackIt)
        {
          if (!dataValid)
          {
            break;
          }

          // Each track ID shall only exist once
          if (count_if(approxPosData.approxTrackDataVec.begin(), approxPosData.approxTrackDataVec.end(), TrackDataTrackIdComp(trackIt->track)) != 1)
          {
            trace->write(detailedTrace, "Multiple tracks with same ID");
            dataValid = false;
          }

          //check the list is continuous.. i.e. previous track id of the track is correct.
          if (trackIt != approxPosData.approxTrackDataVec.begin())
          {
            if (trackIt->previousTrack != lastTrackID)
            {
              trace->write(detailedTrace, "Added tracks are not continuous");
              dataValid = false;
            }

            // Each track should have the same travel direction.. not necessarily the orientation
            if ((trackIt)->trvDir != supposedTravelDir)
            {
              trace->write(detailedTrace, "Added tracks have different direction");
              dataValid = false;
            }
          }
          else
          {
            //setting the direction of first track to check it with further tracks direction
            supposedTravelDir = trackIt->trvDir;
            if (trackIt->previousTrack != 0U)
            {
              trace->write(detailedTrace, "First tracks do not have previous track 0");
              dataValid = false;
            }

          }

          lastTrackID = trackIt->track;

          //Checking if train footprints are within the tracks received
          //Calculating the length of tracks till train front position received in Approx Pos
          //Also assuming that tracks information received in Approx Pos message is from the rear end.

          //if current track is the track where front end of train exist
          if ((lastTrackID == approxPosData.approxTrackAndpos.track) && !isTrainStartFound)
          {
            isTrainStartFound = true;
            uint32_t trackLen = 0U;

            int32_t approxPos = approxPosData.approxTrackAndpos.position;

            if (DirForward == trackIt->trvDir)
            {
              if (OdoPositive == trackIt->odoDir)
              {
                //Moving in Forward direction and Locomotive is close to leg 1
                trackLen = static_cast<uint32_t> (ATC::ATCMath::instance().absolute((approxPos - trackIt->distanceLeg0), __FILE__, __LINE__));
              }
              else
              {
                //Moving in Forward direction and locomotive is close to leg 0
                trackLen = static_cast<uint32_t> (ATC::ATCMath::instance().absolute((approxPos - trackIt->distanceLeg1), __FILE__, __LINE__));
              }
            }
            else if (DirReverse == DS::AbstractTracks::corePtr()->getTravelDirection())
            {
              if (OdoNegative == trackIt->odoDir)
              {
                //Moving in reverse direction and loco is close to leg 1
                trackLen = static_cast<uint32_t> (ATC::ATCMath::instance().absolute((approxPos - trackIt->distanceLeg1), __FILE__, __LINE__));
              }
              else
              {
                //Moving in reverse direction and loco is close to leg 0
                trackLen = static_cast<uint32_t> (ATC::ATCMath::instance().absolute((approxPos - trackIt->distanceLeg0), __FILE__, __LINE__));
              }
            }
            else
            {
              //should never enter here
            }

            trackLenTillFrontPos += trackLen;
            DS::TrainSetup trainSetup;
            if (DS::AbstractTSetup::corePtr()->getTrainSetup(trainSetup))
            {
              uint32_t trainLen = trainSetup.length;
              //if Track behind the Approximate Position is less then the train length then train is not within the
              if (trackLenTillFrontPos < trainLen)
              {
                trace->write(detailedTrace, "Train FootPrints are not within the tracks");
                dataValid = false;
              }
            }
          }
          else if (!isTrainStartFound)
          {
            uint32_t trackLen = static_cast<uint32_t> (ATC::ATCMath::instance().absolute((trackIt->distanceLeg1 - trackIt->distanceLeg0), __FILE__, __LINE__));
            trackLenTillFrontPos += trackLen;
          }
          else
          {
            //Do nothing only used to remove lint error
          }
        }//en d of for loop
      }

      return dataValid;
    }
      \endcode

  - Affected component : Message Handler
     + Description of Changes: Add a new method publishTracks() data in RadioMessageInApproximatePosition class.
                              + This Method will remove the existing tracks and balise information stored.
                              + This Method add the tracks data received in message to Data Storage (Tracks).
                              + This method will also update the odo values for tracks according to the front position received in ApproximatePosition message.
     + Pseudo Code:
      \code
      bool RadioMessageInApproximatePosition::publishTracks()
       {
         bool publishTracksValid = true;
         if (DS::AbstractTracks::corePtr()->removeAll())
         {
           // Add data to Tracks, abort if something went wrong
           for (std::vector<TrackData>::iterator it = approxPosData.approxTrackDataVec.begin();
           (it != approxPosData.approxTrackDataVec.end()) && publishTracksValid; ++it)
           {
            DS::Track track(it->track,
            it->trvDir,
            it->previousTrack,
            it->distanceLeg0,
            it->distanceLeg1,
            it->odoDir);

            publishTracksValid = DS::AbstractTracks::corePtr()->addTrack(track);
           }

           //updating the tracks according to current train front odo position
            OdoPosition currentFrontOdoPos = Pos::AbstractPosition::corePtr()->getCurrFrontPosOdo();
           DS::AbstractTracks::corePtr()->updateTracks(approxPosData.approxTrackAndpos, currentFrontOdoPos);
         }
        else
        {
         publishTracksValid = false;
        }
        return publishTracksValid;
       }
      \endcode



  - Affected component : Message Handler & validate()
     + Description of Changes: After successful parsing, validation of mode and validation of Approx message, a flag approxPosReceived will be set to true.
                               As mentioned in below pseudo code.
                               + Pseudo Code:
                               \code
                                 // Parse, validate and publish data
                                 if (DataAvailable == dataProcessState)
                                  {
                                    if (parseMessageData())
                                     {
                                       if (validateMode())
                                       {
                                         if (validateApproxPosMessage())
                                          {
                                            if (publishTracks())
                                             {
                                                 dataProcessState = DataValidated;
                                                 ret = true;
                                             }
                                          }
                                       }
                                  }
                                  \endcode

  - Affected component & Method :  Position & AbstractPosition::run()
  + Description of Changes:  In run() method of AbstractPosition class add below  code to set the current position to PosApprox.
                             + Pseudo Code:
                               \code
                               if (Kernel::AbstractMessageHandler::corePtr()->isApproxPosReceived())
                               {
                                //Set current position to approx
                                accuracyState = PosApprox;
                               }
                               \endcode

\subsection TrainPosition Train Position General
  - Requirement : AOS 183S, AOS 185S, AOS186S are already implemented.

\subsection RequirementAOS188 Requirement AOS188 & AOS189
  -  Affected component & Method :  Position & readBalises(void)
      + Description of Changes: Modify the implementation of readBalises method from line number 331 onward in abstract_position.cpp according to below pseudo code.
                                + Pseudo code:
                                 \code
                                  //calculation of odoOffset
                                  OdoPosition offsetLocal = balise.getOdoPosition() - baliseObj.odometerPos;
                                  //absolute the value to avoid the lint error
                                  uint32_t offsetLocalAbs = static_cast<uint32_t>(abs(offsetLocal));

                                   //Logic for Requirement AOS1306
                                   if (PosApprox == accuracyState)
                                    {
                                      odoOffset = offsetLocal;
                                      //Setting Position to Known
                                      accuracyState = PosKnown;
                                    }
                                     //check whether the offset is greater than balise window
                                    else if (offsetLocalAbs <= AbstractOdometry::corePtr()->getBaliseWindow())
                                     {
                                        //Logic for Requirement AOS 188S
                                        if (PosKnown == accuracyState)
                                           {
                                              odoOffset = offsetLocal;
                                           }
                                     }
                                     else if (offsetLocalAbs > AbstractOdometry::corePtr()->getBaliseWindow())
                                     {
                                       // Logic for the requirement AOS189S
                                       //if slip found or state is known
                                       if ((PosKnown == accuracyState)
                                            && isSlip)
                                       {
                                          odoOffset = offsetLocal;
                                        }
                                        else if(PosKnown == accuracyState && (!isSlipSlideFound )) //Condition for Requirement AOS1309
                                        {
                                          //Set position to doubtful
                                           accuracyState = PosDoubtfull;
                                           ATC::debugInfo("Balise expected pos = %d, actual pos %d. Window = %d\n", balise.getOdoPosition(), baliseObj.odometerPos, AbstractOdometry::corePtr()->getBaliseWindow());
                                           ATC::AbstractEventHandler::corePtr()->reportEvent(outSideBaliseFound, __FILE__
                                           , __LINE__);
                                        }
                                        else
                                        {
                                           //Do nothing
                                        }
                                     }
                                     else
                                     {
                                           //Do nothing
                                     }
                                 \endcode


\subsection UnkownPosition Unknown Position
  - Requirement Covered: AOS104
  - Affected component & Method :  Position & run()
    + Description of Changes: In run() method set accuracyState = PosUnknown in below modes:
        + ATPModePowerUp
        + ATPModePoweringDown
        + ATPModeSleeping
        + ATPModeUnregistered
        + ATPModeSafetyHalt
        + ATPModePossession
        + ATPModeYard
        + ATPModeShunting
        + ATPModeBaliseSearch
        For ATPModeBaliseSearch mode below pseudo code need to be added in run() method 
        + Pseudo Code:
        \code
        //Setting Position to Unknown at starting of balise search start
        if (bsModeState == Kernel::BaliseSearchMode::baliseSearchStart)
        {
          accuracyState = PosUnknown;
        }
        \endcode

\subsection KnownPosition Known Position
  - Requirement Covered: AOS1306
  - Affected component & Method :  Position & readBalises()
    + Description of Changes: Please refer Pseudo code section of \ref RequirementAOS188, please find the corresponding logic by AOS1306 comments.

\subsection KnownPositionAOS1313 Known Position Requirement AOS 1313
    - Requirement Covered: AOS1313
    - Affected component & Method :  Position & run()
    + Description of Changes: Need to add below logic (mentiond in pseudo code) at line number 238 bstract_position.cpp file.
    + Pseudo code:
       \code
       //Here considering that MA will be accepted only when first found balise is present in MA
       //So not need to check again here
       else if ((bsModeState == Kernel::BaliseSearchMode::baliseSearchWaitMA)
          && (Kernel::AbstractMessageHandler::corePtr()->getMAHead(mHead))
          && isNewConf)
          {
          //Setting Position to Known
          accuracyState = PosKnown;
          }
       \endcode

\subsection DoubtfulPosition Doubtful Position
  - Requirement Covered: AOS1307
  - Affected component & Method :  Position & AbstractPosition::readBalises(void)
      + Description of Changes: Condition implemented at line number 394 of abstract_position.cpp need to be modified as mentioned below:
        + Pseudo code:
        \code
          if ((PosKnown == accuracyState) || (PosApprox == accuracyState))
            {
              //detected balise is not in MA list
              accuracyState = PosDoubtfull;
              //safe brake
              ATC::AbstractEventHandler::corePtr()->reportEvent(unKnownBaliseFound, __FILE__
                , __LINE__);
            }
        \endcode

\subsection DoubtfulPositionAOS1309 Doubtful Position Requirement AOS1309
  - Affected component & Method :  Position & AbstractPosition::readBalises(void)
      + Description of Changes: At line number 364 of in abstract_position.cpp, change the implementation as mentioned in pseudo code of section \ref RequirementAOS188.
                                Please identify the implementation changes in pseudo code by comments related to requirement number (AOS1309).


\subsection DoubtfulPositionAOS2094 Doubtful Position Requirement AOS2094
  -1 Affected component :  Odometry
      + Description of Changes:
                            + Add a new bool variable isSaftyrMarginCrossed in AbstractOdometry class.
                            + Set isSafteyMarginCrossed = true at line number 899 of abstract_odometry.cpp ,when calculated balise window is more then vehicle SAFETY_MARGIN.
                            + Add a new getter method in isSafterMarginCrossed() AbstractOdometry class
                               + Pseudo code:
                               \code
                                bool AbstractOdometry::isSafterMarginCrossed()
                                {
                                  return isSafteyMarginCrossed;
                                }
                               \endcode

  -2 Affected component & Method :  Position & AbstractPosition::run()
      + Description of Changes:  In run() method of AbstractPosition class add below  code to set the current position to doubtful.
                             + Pseudo code:
                             \code
                                //Checking if SafteyMarginCrossed then setting current position to Doubtful
                                //Safe brake is already applied in Odometery, only changing the position in Position
                                //Get the current mode of ATP
                                bool isSafetyMarginCrossed = AbstractOdometry::corePtr()->isSafterMarginCrossed();

                                if (isSafetyMarginCrossed)
                                {
                                accuracyState = PosDoubtfull;
                                }
                             \endcode

\subsection DoubtfulPositionAOS2390 Doubtful Position Requirement AOS2390 & AOS2391

- Affected component & Method :  Position & AbstractPosition::updateCurrentPos()
 + Description of Changes: Need to modify updateCurrentPos() of AbstractPosition class according to below pseudo code.
      + Pseudo code:
      \code
     void AbstractPosition::updateCurrentPos(void)
    {

      //Check if safe front or rear position are within the track
      const TrackAndPos calSafeFrontPos = DS::AbstractTracks::corePtr()->calculateTrackAndPos(getSafeFrontPosOdo());
      const TrackAndPos calSafeRearPos = DS::AbstractTracks::corePtr()->calculateTrackAndPos(getSafeRearPosOdo());

      if (((0U == calSafeFrontPos.track) &&
        (0U == calSafeRearPos.track)) &&
        (accuracyState != PosUnknown))
      {
         //Front and rear end train position are outside the stored track and position is not unknown
          accuracyState = PosDoubtfull;
          //Raising Safety Halt event
          ATC::AbstractEventHandler::corePtr()->reportEvent(outOfLimitBoundaries, __FILE__
            , __LINE__);
      }
      else if (((0U == calSafeFrontPos.track) ||
        (0U == calSafeRearPos.track)) &&
        ((PosKnown == accuracyState) || (PosApprox == accuracyState)))
      {
        //if either front or rear end of train are outside of stored tracks  and current position is known or approx
          accuracyState = PosDoubtfull;
          //safe brake to stop
          ATC::AbstractEventHandler::corePtr()->reportEvent(rearOrFrontEndoutOfLimit, __FILE__
            , __LINE__);
      }
      else
      {
        //check for current odo value present in track list
        TrackAndPos calPos = DS::AbstractTracks::corePtr()->calculateTrackAndPos(currOdoPos);
        const TrackAndPos calFrontPos = DS::AbstractTracks::corePtr()->calculateTrackAndPos(getCurrFrontPosOdo());
        //check for current rear odo in MA track list
        const TrackAndPos calRearPos = DS::AbstractTracks::corePtr()->calculateTrackAndPos(getCurrRearPosOdo());
        if (0U != calPos.track)
        {   //setting antenna position 
          currPosAntenna = calPos;
          //check for Current Front odo in MA track list 
          if ((0U != calFrontPos.track) &&
            (0U != calRearPos.track))
          {
            //set the front position
            currentPosNominalFront = calFrontPos;
            //set the rear position
            currentPosNominalRear = calRearPos;
          }

        }
      }
    }//end of updateLastPos

      \endcode

*/