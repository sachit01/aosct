/****************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (TCC->AOS) has an associated parser class inherited from AbstractRadioMessageIn.
* This file implements the parser for the MovementAuthority message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-26    marlundg    Created
* 2016-10-03    arastogi    Added special handling for balise search mode.
*                           Fixed bugs in validating balise and track data.
* 2016-10-07    hidaji      Added handling of current Gradient, Brakeability, CeilingSpeed
* 2016-10-11    hidaji      Added TODO and a commented out implementation for considering
*                           max train speed for ceiling speed
* 2016-10-12    arastogi    Added possibility to receive MA in balise search and registration
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include <algorithm>
#include "radio_message_in_movement_authority.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tracks.hpp"
#include "abstract_targets.hpp"
#include "abstract_tsetup.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_radio_handler.hpp"
#include "abstract_supervise.hpp"
#include "abstract_tims.hpp"
#include "atc_math.hpp"
#include "abstract_brake.hpp"
#include "abstract_message_handler_event_ids.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    /******************************************************************************
    * RadioMessageInMovementAuthority constructor
    ******************************************************************************/
    RadioMessageInMovementAuthority::RadioMessageInMovementAuthority() : AbstractRadioMessageIn(MTypeMovementAuthority),
      firstBaliseNotInMA(ATC::Event::createSafeBrakeSBEvent(atpMessageHandlerId,
        ATC::CoreContainer, eventIdFirstBaliseNotInMA, ATC::NoSB, DMICom::msgHdlrFirstBalNotFoundInMA, "First balise not found in MA")),
      invalidMaInReposition(ATC::Event::createSafeBrakeSBEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdInvalidMaInReposition,
        ATC::NoSB, DMICom::msgHdlrInvalidMaInReposition, "Invalid MA received during Reposition")),
      invalidMaInReregistration(ATC::Event::createSafeBrakeSBEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdInvalidMaInReregistration,
        ATC::NoSB, DMICom::msgHdlrInvalidMaInReregistration, "Invalid MA received during Reregistration")),
      criticalInvalidStateSafetyHalt(ATC::Event::createSafetyHaltEvent(atpMessageHandlerId, ATC::CoreContainer,
        eventIdATPMessageHandlerCriticalState, ATC::NoEB, DMICom::msgHdlrCriticalState, "Message handler detected critical state"))
    {
      implemented = true;

      // Setup Fixed Sizes for msg-block vectors.
      maData.departureWarningReceived = false;
      maData.partlyMaReceived = false;
      maData.maxSearchDistReceived = false;
      maData.locationDataReceived = false;
      maData.atoStopPositionReceived = false;
      maData.trackDataVec.reserve(DS::AbstractTracks::maxNumberOfStoredTracks);
      maData.baliseDataVec.reserve(DS::AbstractTracks::maxNumberOfStoredBalises);
      maData.gradientDataVec.reserve(maxNumberOfGradientTargets);
      maData.ceilingSpeedDataVec.reserve(maxNumberOfSpeedTargets);
      maData.trackDataItemVec.reserve(maxNumberOfTrackDataItemTargets);
      memset(&maData.locationData.locationName[0], 0, sizeof(maData.locationData.locationName));
      maData.locationData.locationType = UndefinedLocationType;
      maData.locationBordersReceived = false;
      maHead.maID = 0U;
      maHead.timeout = 0U;
      maHead.ceilingSpeed = 0U;
      maHead.gradient = 0;
      maHead.trainLoadStatus = TrainIsEmpty;
      maHead.adhesionValue = 50U;      // Minimum Value of N_ADHESION is 50
      maHead.trainDirection = 0U;
      maHead.routeType = RtUndefined;
      maHead.endOfMATrackAndPos.position = 0U;
      maHead.endOfMATrackAndPos.track = 0U;
      maHead.startOfMATrackAndPos.position = 0U;
      maHead.startOfMATrackAndPos.track = 0U;
      maHead.maMarginCm = 0U;
      maHead.overlapValue = 0U;
      
      isValidPartlyMAInPrevMa = false;
      isValidPartlyMAInCurrMa = false;
      
      maReceived = false;
      maPartlyHead = maHead;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::getMAHead
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::getMAHead(MAHead & head) const
    {
      bool ret = false;
      if ((DataValidated == dataProcessState) && (!isValidPartlyMAInCurrMa))
      {
        head = maHead;
        ret = true;
      }

      return ret;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::getMAReceived
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::getMAReceived(uint8_t& id, uint16_t& replyChannelId) const
    {
      replyChannelId = messageData.channelId;
      if (isValidPartlyMAInPrevMa)
      {
        id = maPartlyHead.maID;
      }
      else
      {
        id = maHead.maID;
      }

      return maReceived;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validate
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validate()
    {
      trace->write(ATC::briefTrace, "Validating MovementAuthority");

      bool valid = AbstractRadioMessageIn::validate();

      if (valid)
      {
        valid = false;

        // Parse, validate and publish data
        if (DataAvailable == dataProcessState)
        {
          if (parseMessageData())
          {
            if (validateNotReadyToDriveStatus())
            {
              if (validateEmergencyAlertStatus())
              {
                if (validateMode())
                {
                  if (validateTrackData())
                  {
                    if (validateBaliseData())
                    {
                      if (validatePrimaryTargetData())
                      {
                        if (validateTargetsData(maData.gradientDataVec))
                        {
                          if (validateTargetsData(maData.ceilingSpeedDataVec))
                          {
                            if (validateTargetsData(maData.trackDataItemVec))
                            {
                              if (validateLocationTargetData())
                              {
                                if (validateMaxSearchDistance())
                                {
                                  if (publishData())
                                  {
                                    dataProcessState = DataValidated;
                                    valid = true;
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }

        // Flag that MA is received and process-state is updated
        maReceived = true;
      }

      if (!valid)
      {
        // Reset that the partly MA was received in current MA if it is rejected.
        isValidPartlyMAInCurrMa = false;

        // Raise a SafeBreakToStop event if an MA is rejected in Reposition or Re-registration
        const ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();

        if (currentMode == ATPModeRegistration)
        {
          const TrainRegistrationModeState modeState = AbstractModeControl::corePtr()->getTrainRegistrationModeState();

          if (modeState == TrainRegistrationMode::trainRegistrationRePosition)
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidMaInReposition, __FILE__, __LINE__);
          }

          if (modeState == TrainRegistrationMode::trainRegistrationWaitReRegMA)
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidMaInReregistration, __FILE__, __LINE__);
          }
        }
      }

      return valid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::publishData
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::publishData()
    {
      bool publishDataValid = true;
      if (!isValidPartlyMAInCurrMa)
      {
        // Publish all data
        publishDataValid = publishTracks();

        if (publishDataValid)
        {
          publishDataValid = publishBalises();
          if (publishDataValid)
          {
            if (isValidPartlyMAInPrevMa)
            {
              // Copy the data of partly ma according to FFFIS.
              maHead.ceilingSpeed = maPartlyHead.ceilingSpeed;
              maHead.gradient = maPartlyHead.gradient;
              maHead.trainLoadStatus = maPartlyHead.trainLoadStatus;
              maHead.adhesionValue = maPartlyHead.adhesionValue;
              maHead.overlapValue = maPartlyHead.overlapValue;
              maHead.startOfMATrackAndPos = maPartlyHead.startOfMATrackAndPos;
              // reset the flag
              isValidPartlyMAInPrevMa = false;
            }

            publishDataValid = publishPrimaryTarget();
            publishSpeedTarget();
            publishGradientTarget();
            publishTrackDataItemTarget();
            publishLocationTarget();
          }
        }
      }
      else
      {
        
        // Remember and store the valid PARTLY_MA. This information is to be used when processing next MA
        // from another TCC.
        maPartlyHead = maHead;
        isValidPartlyMAInPrevMa = true;

        publishDataValid = true;
      }
      tracePublishData(publishDataValid);

      return publishDataValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::publishTracks
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::publishTracks()
    {
      bool publishTracksValid = true;

      // Add data to Tracks, abort if something went wrong
      for (std::vector<TrackData>::const_iterator it = maData.trackDataVec.begin();
        (it != maData.trackDataVec.end()) && publishTracksValid; ++it)
      {
        DS::Track track(
          it->track,
          it->trvDir,
          it->previousTrack,
          it->length,
          it->odoDir);

        publishTracksValid = DS::AbstractTracks::corePtr()->addTrack(track);
      }

      if (!publishTracksValid)
      {
        setInvalidationReason("addTrack failed");
      }

      return publishTracksValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::publishBalises
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::publishBalises()
    {
      bool publishBalisesValid = true;

      // Add data to Balises, abort if something went wrong
      for (std::vector<BaliseData>::const_iterator it = maData.baliseDataVec.begin();
        ((it != maData.baliseDataVec.end()) && publishBalisesValid); ++it)
      {
        DS::Balise balise(it->baliseId, it->baliseTrackAndPosition.track, it->baliseTrackAndPosition.position);

        publishBalisesValid = DS::AbstractTracks::corePtr()->addBalise(balise);
      }

      if (!publishBalisesValid)
      {
        setInvalidationReason("Failed adding balise!");
      }

      return publishBalisesValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::publishSafetyMargin
    ******************************************************************************/
    void RadioMessageInMovementAuthority::publishSafetyMargin()
    {
      // Care must be taken not to shrink the safety margin immediately based on the D_OVERLAP given in the MA head
      // Instead also the initial safety-margin change shall be handled by inserting an extra track-data item 
      // for the safety margin change. In this way the passed track-data items will be correctly handled along with
      // the initial value of D_OVERLAP (in an MA from scratch)
      // In this way we avoid setting a low value of D_OVERLAP which will cause SafeBrakeToStop in Odometry
      // because of BaliseWindow > Safety-margin, when the balise-window is already greater than D_OVERLAP
      uint16_t prevSafetyMargin = DS::AbstractTargets::corePtr()->getSafetyMargin();
      if (maHead.overlapValue >= prevSafetyMargin)
      { 
        // Increase of Safety-margin to be done immediately.
        // Especially considering that Safety-margin is 0 at startup
        DS::AbstractTargets::corePtr()->setSafetyMarginChangeTargetActive(maHead.overlapValue);
      }
      else
      {
        // Decrease Safety-margin by inserting a track-data item at start of MA to be processed along with other track-data items
        // This is to avoid setting a low SafetyMargin (for one cycle) that is already passed (supposed to be overridden) by another passed track-data item
        // In this way we avoid setting a low value of Safety margin which will cause SafeBrakeToStop in Odometry
        // because of BaliseWindow > Safety-margin, when the balise-window is already greater than the arriving D_OVERLAP

        // Create a track-data item corresponding with start of MA and the D_OVERLAP as Safety margin
        // Set valid direction of the track-data item to the same as the MA-direction  
        // MA-direction is either Forward or Reverse.
        const TravelDir travelDir = dirAndOrientation2TravelDir(maHead.trainDirection);
        ValidDirection validDirection;
        if (DirForward == travelDir)
        {
          validDirection = ValidDirectionForward;
        }
        else
        {
          validDirection = ValidDirectionReverse;
        }

        TrackDataItem trackDataItem;
        trackDataItem.trackDataType = TrackDataTypeSafetyMarginChange;
        trackDataItem.trackAndPosition = maHead.startOfMATrackAndPos;
        trackDataItem.validDir = validDirection;
        trackDataItem.optionalValue = maHead.overlapValue;
        // Insert first among the track-data items
        if (maData.trackDataItemVec.size() < maxNumberOfTrackDataItemTargets)
        {
          static_cast<void>(maData.trackDataItemVec.insert(maData.trackDataItemVec.begin(), trackDataItem));
        }
        else
        {
          writeToLog(ATC::BriefLog, "No more track-data-items available. Safety margin decreased immediately!", __FILE__, __LINE__);
          // Set the SafetyMargin immediately if not enough space for another track-data item
          DS::AbstractTargets::corePtr()->setSafetyMarginChangeTargetActive(maHead.overlapValue);
        }
      }
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::publishPrimaryTarget
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::publishPrimaryTarget()
    {
      bool publishPrimaryTargetValid = true;
      if (ATPModeLocation != Kernel::AbstractModeControl::corePtr()->getCurrentMode())
      {
        OdoPosition odoPosition;
        bool idling = Kernel::AbstractModeControl::corePtr()->getIdleState();

        // In train registration mode, train is not idling but the target list is empty
        idling = ((DS::AbstractTargets::corePtr()->isMATargetListEmpty()) || idling);

        // Tracks are published before targets, so odoPosition shall be calculated by now.
        if (DS::AbstractTracks::corePtr()->getOdoPos(maHead.endOfMATrackAndPos, odoPosition))
        {
          if (!idling)
          {
            const DS::BaseTarget* const pTarget = DS::AbstractTargets::corePtr()->getPrimaryTarget(
              dirAndOrientation2TravelDir(maHead.trainDirection));

            if (pTarget != static_cast<DS::BaseTarget*>(NULL))
            {

              const RouteType targetTypeInList = static_cast<RouteType>((pTarget)->getRouteType());

              if ((RtNormal == maHead.routeType) && (RtStaffResponsible == targetTypeInList))
              {
                static_cast<void>(DS::AbstractTargets::corePtr()->delTarget(pTarget));
              }
              else if ((RtNormal == maHead.routeType) && (RtReRegistration == targetTypeInList))
              {
                static_cast<void>(DS::AbstractTargets::corePtr()->delTarget(pTarget));
              }
              else if (maHead.routeType == targetTypeInList)
              {
                static_cast<void>(DS::AbstractTargets::corePtr()->delTarget(pTarget));
              }
              else
              {
                // do nothing
              }
            }
          }

          // Create Primary Target
          DS::PrimaryTarget primaryTarget(
            static_cast<uint32_t>(maHead.maID),
            static_cast<uint8_t>(maHead.routeType),
            static_cast<uint32_t>(maHead.maMarginCm),
            maHead.timeout,
            maHead.endOfMATrackAndPos,
            dirAndOrientation2TravelDir(maHead.trainDirection),
            odoPosition);

          DS::AbstractTargets::corePtr()->addTarget(primaryTarget);

          if (isMAFromScratch())
          {
            DS::AbstractTargets::corePtr()->setCurTrackGradient(static_cast<int32_t>(maHead.gradient));
            DS::AbstractTargets::corePtr()->setCurCeilingSpeed(static_cast<uint32_t>(maHead.ceilingSpeed));
            // Set the travel direction
            DS::AbstractTargets::corePtr()->setSupposedTravelDir(primaryTarget.getDirection());
            // Publish the D_OVERLAP 
            publishSafetyMargin();

            // Publish the Loaded status and Adhesion
            DS::AbstractTSetup::corePtr()->setTrainLoadStatus(maHead.trainLoadStatus);
            publishAdhesion();
          }
          else
          {
            //remove any old SM speed restrictions when MA extension is received.
            DS::AbstractTargets::corePtr()->removeSMSpeedRestrictionTarget();
          }
        }
        else
        {
          publishPrimaryTargetValid = false;
          setInvalidationReason("Failed publishPrimaryTarget!");
        }
      }

      return publishPrimaryTargetValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::publishSpeedTarget
    ******************************************************************************/
    void RadioMessageInMovementAuthority::publishSpeedTarget() const
    {
      bool idling = Kernel::AbstractModeControl::corePtr()->getIdleState();
      // In train registration mode, train is not idling but the target list is empty
      idling = ((DS::AbstractTargets::corePtr()->isMATargetListEmpty()) || idling);
      const TravelDir travelDir = dirAndOrientation2TravelDir(maHead.trainDirection);
      // Add Speed target 
      const OdoPosition frontOdoPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();
      //To check if last current target is ahead of last passed target
      OdoPosition lastUpdatedTargetPos = 0;
      for (uint8_t i = 0U; i < maData.ceilingSpeedDataVec.size(); i++)
      {
        uint32_t speed = maData.ceilingSpeedDataVec[i].ceilingSpeed;
        SpeedChangeReason reasonSpeedChange = maData.ceilingSpeedDataVec[i].speedChangeReason;

        const TrackAndPos& trackPosition = maData.ceilingSpeedDataVec[i].trackAndPosition;

        OdoPosition targetOdoPosition;
        const bool isOdoPositionOK = DS::AbstractTracks::corePtr()->getOdoPos(trackPosition, targetOdoPosition);
        const bool isTargetAheadCurrentPos = (((DirForward == travelDir) && (frontOdoPos < targetOdoPosition))
          || ((DirReverse == travelDir) && (frontOdoPos > targetOdoPosition)));

        if (isOdoPositionOK)
        {
          if ((!idling) || isTargetAheadCurrentPos)
          {
            DS::SpeedTarget speedTarget(speed, static_cast<uint32_t>(reasonSpeedChange), trackPosition,
              dirAndOrientation2TravelDir(maHead.trainDirection),
              targetOdoPosition);
            DS::AbstractTargets::corePtr()->addTarget(speedTarget);
          }
          else
          {
            if (((DirForward == travelDir) && (targetOdoPosition > lastUpdatedTargetPos))
              || ((DirReverse == travelDir) && (targetOdoPosition < lastUpdatedTargetPos))
              || (0 == lastUpdatedTargetPos))
            {
              lastUpdatedTargetPos = targetOdoPosition;
              //If Speed target is behind the train front then do not add the target in target list, update the current ceiling speed
              DS::AbstractTargets::corePtr()->setCurCeilingSpeed(speed);
            }
          }

        }

      }

    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::publishGradientTarget
    ******************************************************************************/
    void RadioMessageInMovementAuthority::publishGradientTarget() const
    {
      // Add Gradient target
      for (uint8_t i = 0U; i < maData.gradientDataVec.size(); i++)
      {
        const int8_t gradient = maData.gradientDataVec[i].gradient;
        const TrackAndPos& trackPosition = maData.gradientDataVec[i].trackAndPosition;

        OdoPosition targetOdoPosition;
        const bool isOdoPositionOK = DS::AbstractTracks::corePtr()->getOdoPos(trackPosition, targetOdoPosition);

        if (isOdoPositionOK)
        {
          DS::GradientTarget gradTarget(gradient, trackPosition,
            dirAndOrientation2TravelDir(maHead.trainDirection),
            targetOdoPosition);
          DS::AbstractTargets::corePtr()->addTarget(gradTarget);
        }
      }
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::publishTrackDataItemTarget
    ******************************************************************************/
    void RadioMessageInMovementAuthority::publishTrackDataItemTarget() const
    {
      // Add Track Data Item 
      for (uint8_t i = 0U; i < maData.trackDataItemVec.size(); i++)
      {
        TrackDataType trackDataType = maData.trackDataItemVec[i].trackDataType;
        TrackAndPos trackPosition;
        trackPosition.track = maData.trackDataItemVec[i].trackAndPosition.track;
        trackPosition.position = maData.trackDataItemVec[i].trackAndPosition.position;

        OdoPosition odoPosition;
        const bool isOdoPositionOK = DS::AbstractTracks::corePtr()->getOdoPos(trackPosition, odoPosition);

        if (isOdoPositionOK)
        {
          if ((TrackDataTypeSafetyMarginChange == trackDataType) || (TrackDataTypeAdhesionChange == trackDataType) ||
            (TrackDataTypeAcousticSignal == trackDataType))
          {
            uint16_t nValue = maData.trackDataItemVec[i].optionalValue;
            DS::TrackDataItemTarget TrackDataItemTarget(static_cast<uint8_t>(trackDataType), trackPosition,
              dirAndOrientation2TravelDir(maHead.trainDirection),
              odoPosition, nValue);
            DS::AbstractTargets::corePtr()->addTarget(TrackDataItemTarget);
          }
          else
          {
            DS::TrackDataItemTarget TrackDataItemTarget(static_cast<uint8_t>(trackDataType), trackPosition,
              dirAndOrientation2TravelDir(maHead.trainDirection),
              odoPosition);
            DS::AbstractTargets::corePtr()->addTarget(TrackDataItemTarget);
          }
        }
      }
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::publishAdhesion()
    ******************************************************************************/
    void RadioMessageInMovementAuthority::publishAdhesion() const
    {
      DS::AbstractTargets::corePtr()->setAdhesionValue(maHead.adhesionValue);
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validateMode
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateMode()
    {
      const bool modeValid = AbstractModeControl::corePtr()->isValidQRouteType(maHead.routeType);
      if (!modeValid)
      {
        setInvalidationReason("Invalid Q_ROUTE_TYPE");
      }

      traceValidateMode(modeValid);
      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::parseMessageData
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::parseMessageData()
    {
      bool parseDataValid = true;
      uint8_t tmpValU8;
      const ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();

      isValidPartlyMAInCurrMa = false;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &(messageData.message.data[0]), sizeof(messageData.message.data));
      vfwSetReadBuffer(&buffer, sizeof(messageData.message.data));

      // Read & validate NID_MESSAGE_TYPE
      if (vfwGetU8(&buffer) != static_cast<uint8_t>(messageType))
      {
        parseDataValid = false;
        setInvalidationReason("NID_MESSAGE_TYPE invalid");
      }

      // Read NID_MSG
      maHead.maID = vfwGetU8(&buffer);

      // Read & validate T_VALID
      maHead.timeout = vfwGetU8(&buffer);
      if (!validateT_VALID(maHead.timeout))
      {
        parseDataValid = false;
        setInvalidationReason("T_VALID invalid");
      }

      // Read V_SPEED
      maHead.ceilingSpeed = vfwGetU16(&buffer);

      // Read G_GRADIENT
      maHead.gradient = vfwGetI8(&buffer);

      // Read & validate M_LOADED
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateM_LOADED(tmpValU8))
      {
        parseDataValid = false;
        setInvalidationReason("M_LOADED invalid");
      }
      else
      {
        maHead.trainLoadStatus = (tmpValU8 == 1U) ? TrainIsLoaded : TrainIsEmpty;
      }

      // Read & validate N_ADHESION
      maHead.adhesionValue = vfwGetU8(&buffer);
      if (!validateN_ADHESION(maHead.adhesionValue))
      {
        parseDataValid = false;
        setInvalidationReason("N_ADHESION invalid");
      }

      // Read & validate B_DIRECTION
      maHead.trainDirection = vfwGetU8(&buffer);
      if (!validateB_DIRECTION(maHead.trainDirection))
      {
        parseDataValid = false;
        setInvalidationReason("B_DIRECTION invalid #1");
      }

      // Read & validate Q_ROUTE_TYPE
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateQ_ROUTE_TYPE(tmpValU8))
      {
        parseDataValid = false;
        setInvalidationReason("Q_ROUTE_TYPE invalid");
      }
      else
      {
        maHead.routeType = static_cast<RouteType>(tmpValU8);
      }

      // Read NID_TRACK (End of MA position)
      const uint16_t nidTrackEndOfTrack = vfwGetU16(&buffer);
      if (nidTrackEndOfTrack == 0U)
      {
        parseDataValid = false;
        setInvalidationReason("NID_TRACK end of MA invalid: 0");
      }

      maHead.endOfMATrackAndPos.track = nidTrackEndOfTrack;

      // Read D_POSITION (End of MA position)
      maHead.endOfMATrackAndPos.position = vfwGetU32(&buffer);

      // Read NID_TRACK (Start of MA position)
      maHead.startOfMATrackAndPos.track = vfwGetU16(&buffer);
      if (maHead.startOfMATrackAndPos.track == 0U)
      {
        parseDataValid = false;
        setInvalidationReason("NID_TRACK start of MA invalid: 0");
      }

      // Read D_POSITION (Start of MA position)
      maHead.startOfMATrackAndPos.position = vfwGetU32(&buffer);

      // Read D_MA_MARGIN
      maHead.maMarginCm = vfwGetU16(&buffer);
      if (maHead.maMarginCm == 0U)
      {
        parseDataValid = false;
        setInvalidationReason("D_MA_MARGIN invalid: 0");
      }

      // Read D_OVERLAP
      maHead.overlapValue = vfwGetU16(&buffer);

      // BlockData
      uint8_t nextMsgIdentifier = vfwGetU8(&buffer);

      // Fetch next data-block until M_END_OF_MESSAGE
      while ((nextMsgIdentifier != M_END_OF_MESSAGE) && (parseDataValid))
      {
        switch (nextMsgIdentifier)
        {
        case BTypeDepartureWarning:
        {
          const AcousticSignal tempDepatureWarning = vfwGetU8(&buffer);

          // Departure Warning 
          if (!maData.departureWarningReceived)
          {
            maData.departureWarning.signalAsDepartureWarning = tempDepatureWarning;
            maData.departureWarningReceived = true;
          }
          else
          {
            parseDataValid = false;
            setInvalidationReason("BTypeDepartureWarning overflow");
          }

          break;
        }

        case BTypePartlyMa:
        {
          bool isValidModeForPartlyMA = false;

          bool idleStatus = Kernel::AbstractModeControl::corePtr()->getIdleState();
          idleStatus = ((DS::AbstractTargets::corePtr()->isMATargetListEmpty()) || idleStatus);
          TrainRegistrationModeState regSubMode = AbstractModeControl::corePtr()->getTrainRegistrationModeState();
          
          const bool isSubModeRegValid = (TrainRegistrationMode::trainRegistrationWaitReRegMA == regSubMode) ||
                                         (TrainRegistrationMode::trainRegistrationRePosition == regSubMode);      
          
          if ((ATPModeRegistration == currentMode) && isSubModeRegValid)
          {
            isValidModeForPartlyMA = true;
          }

          if (!maData.partlyMaReceived)
          {
            // Conditions for accepting a PARTLY_MA block.
            if ((1U < RadioCom::AbstractRadioHandler::corePtr()->getNumOfTCCConnected()) && 
              (idleStatus || isValidModeForPartlyMA))
            {
              isValidPartlyMAInCurrMa = true;
            }
            else
            {
              parseDataValid = false;
              setInvalidationReason("Invalid conditions for PARTLY_MA (<2 TCC:s && (Idle || ReReg || RePos)");
            }
            maData.partlyMaReceived = true;
          }
          else
          {
            parseDataValid = false;
            setInvalidationReason("BTypePartlyMa overflow");
          }
          break;
        }

        case BTypeMaxSearchDist:
        {
          if (!maData.maxSearchDistReceived)
          {
            maData.maxSearchDistReceived = true;
            // maxDistBalise is a 24-bit value
            maData.maxSearchDist.maxDistBalise = 0U;
            maData.maxSearchDist.maxDistBalise += (static_cast<uint32_t>(vfwGetU8(&buffer)) << static_cast<uint32_t>(16U));
            maData.maxSearchDist.maxDistBalise += (static_cast<uint32_t>(vfwGetU8(&buffer)) << static_cast<uint32_t>(8U));
            maData.maxSearchDist.maxDistBalise += vfwGetU8(&buffer);
          }
          else
          {
            parseDataValid = false;
            setInvalidationReason("BTypeMaxSearchDist overflow");
          }
          break;
        }

        case BTypeLocationBorder:
        {
          if (!maData.locationBordersReceived)
          {
            maData.locationBordersReceived = true;
            maData.locationBorders.startOfLocation.track = vfwGetU16(&buffer);
            maData.locationBorders.startOfLocation.position = vfwGetU32(&buffer);
            maData.locationBorders.endOfLocation.track = vfwGetU16(&buffer);
            maData.locationBorders.endOfLocation.position = vfwGetU32(&buffer);
            maData.locationBorders.allowedSpeed = vfwGetU16(&buffer);
            maData.locationBorders.gradTwrdsLocStart = vfwGetI8(&buffer);
            maData.locationBorders.gradTwrdsLocEnd = vfwGetI8(&buffer);

            if (RtNormal != maHead.routeType)
            {
              parseDataValid = false;
              setInvalidationReason("Q_ROUTE_TYPE is invalid");
            }
          }
          else
          {
            parseDataValid = false;
            setInvalidationReason("BTypeLocationBorder overflow");
          }

          break;
        }

        case BTypeLocationData:
        {
          for (uint8_t i = 0U; i < maxLocationNameLength; i++)
          {
            maData.locationData.locationName[i] = static_cast<char_t>(vfwGetI8(&buffer));
          }

          tmpValU8 = vfwGetU8(&buffer);

          maData.locationData.locationType = static_cast<LocationType>(tmpValU8);

          if (!maData.locationDataReceived)
          {
            maData.locationDataReceived = true;
          }
          else
          {
            parseDataValid = false;
            setInvalidationReason("BTypeLocationData overflow");
          }
          break;
        }

        case BTypeATOStopPosition:
        {
          maData.atoStopPosition.trainStopTrackAndPosition.track = vfwGetU16(&buffer);
          maData.atoStopPosition.trainStopTrackAndPosition.position = vfwGetU32(&buffer);

          // ATO Stop Position
          if (!maData.atoStopPositionReceived)
          {
            maData.atoStopPositionReceived = true;
          }
          else
          {
            parseDataValid = false;
            setInvalidationReason("BTypeATOStopPosition overflow");
          }

          break;
        }

        case BTypeTrackData:
        {
          TrackData trackData;
          readTRACK_DATA(buffer, trackData);

          if (trackData.track == 0U)
          {
            parseDataValid = false;
            setInvalidationReason("Track has invalid NID_TRACK: 0");
          }

          if (!validateB_DIRECTION(trackData.bdirection))
          {
            parseDataValid = false;
            setInvalidationReason("B_DIRECTION invalid #2");
          }

          if (maData.trackDataVec.size() < DS::AbstractTracks::maxNumberOfStoredTracks)
          {
            maData.trackDataVec.push_back(trackData);
          }
          else
          {
            parseDataValid = false;
            setInvalidationReason("BTypeTrackData overflow");
          }

          if (trackData.length == 0U)
          {
            parseDataValid = false;
            setInvalidationReason("Track has length 0");
          }
          break;
        }

        case BTypeBaliseData:
        {
          BaliseData baliseData;
          baliseData.baliseTrackAndPosition.track = vfwGetU16(&buffer);
          baliseData.baliseTrackAndPosition.position = vfwGetU32(&buffer);
          baliseData.baliseId = vfwGetU16(&buffer);

          if (maData.baliseDataVec.size() < DS::AbstractTracks::maxNumberOfStoredBalises)
          {
            if (!validateNID_BG(baliseData.baliseId))
            {
              trace->write(ATC::detailedTrace, "NID_BG invalid");
              parseDataValid = false;
            }
            maData.baliseDataVec.push_back(baliseData);
          }
          else
          {
            parseDataValid = false;
            setInvalidationReason("BTypeBaliseData overflow");
          }
          break;
        }

        case BTypeGradientData:
        {
          GradientData gradientData;

          gradientData.trackAndPosition.track = vfwGetU16(&buffer);
          gradientData.trackAndPosition.position = vfwGetU32(&buffer);
          gradientData.gradient = vfwGetI8(&buffer);
          
          const bool isGradientTargetsWithinMax = (maData.gradientDataVec.size() < maxNumberOfGradientTargets);
          const bool isAvailableMemoryGradientTargets = (ATP::DS::AbstractTargets::corePtr()->getFreeBlocksGrad() > maData.gradientDataVec.size() );

          if (currentMode != ATPModeLocation)
          {
            if (isGradientTargetsWithinMax && isAvailableMemoryGradientTargets)
            {
              maData.gradientDataVec.push_back(gradientData);
            }
            else
            {
              parseDataValid = false;
              setInvalidationReason("BTypeGradientData overflow");
            }
          }
          else
          {
            char_t logStr[120];

            //lint -e{586} snprintf is needed here
            const int32_t res = snprintf(&logStr[0], sizeof(logStr),
              "LocationMode --> Ignoring GRADIENT_DATA: NID_TRACK=%u, D_POSITION=%u, G_GRADIENT=%d",
              static_cast<uint32_t>(gradientData.trackAndPosition.track),
              static_cast<uint32_t>(gradientData.trackAndPosition.position),
              static_cast<int32_t>(gradientData.gradient));

            if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
            {
              traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
            }
          }
          break;
        }

        case BTypeCeilingSpeedData:
        {
          CeilingSpeedData ceilingSpeedData;

          ceilingSpeedData.trackAndPosition.track = vfwGetU16(&buffer);
          ceilingSpeedData.trackAndPosition.position = vfwGetU32(&buffer);
          ceilingSpeedData.ceilingSpeed = vfwGetU16(&buffer);

          tmpValU8 = vfwGetU8(&buffer);
          if (!validateQ_SPEED(tmpValU8))
          {
            parseDataValid = false;
            setInvalidationReason("Q_SPEED invalid");
          }

          ceilingSpeedData.speedChangeReason = static_cast<SpeedChangeReason>(tmpValU8);

          const bool isSpeedTargetsWithinMax = maData.ceilingSpeedDataVec.size() < maxNumberOfSpeedTargets;
          const bool isAvailableMemorySpeedTargets = ATP::DS::AbstractTargets::corePtr()->getFreeBlocksSpeed() > maData.ceilingSpeedDataVec.size();

          if (isSpeedTargetsWithinMax && isAvailableMemorySpeedTargets)
          {
            if (currentMode != ATPModeLocation)
            {
              maData.ceilingSpeedDataVec.push_back(ceilingSpeedData);
            }
            else
            {
              char_t logStr[120];

              //lint -e{586} snprintf is needed here
              const int32_t res = snprintf(&logStr[0], sizeof(logStr),
                "LocationMode --> Ignoring CEILING_SPEED_DATA: NID_TRACK=%u, D_POSITION=%u, V_SPEED=%u, Q_SPEED=%u",
                static_cast<uint32_t>(ceilingSpeedData.trackAndPosition.track),
                static_cast<uint32_t>(ceilingSpeedData.trackAndPosition.position),
                static_cast<uint32_t>(ceilingSpeedData.ceilingSpeed),
                static_cast<uint32_t>(ceilingSpeedData.speedChangeReason));

              if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
              {
                traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
              }
            }
          }
          else
          {
            parseDataValid = false;
            setInvalidationReason("BTypeCeilingSpeedData overflow");
          }
          break;
        }

        case BTypeTrackDataItem:
        {
          TrackDataItem trackDataItem;

          tmpValU8 = vfwGetU8(&buffer);

          if (!validateQ_TRACK_DATA_TYPE(tmpValU8))
          {
            parseDataValid = false;
            setInvalidationReason("Q_TRACK_DATA_TYPE invalid");
          }
          trackDataItem.trackDataType = static_cast<TrackDataType>(tmpValU8);

          trackDataItem.trackAndPosition.track = vfwGetU16(&buffer);
          trackDataItem.trackAndPosition.position = vfwGetU32(&buffer);

          tmpValU8 = vfwGetU8(&buffer);

          if (!validateQ_DIRECTION(tmpValU8))
          {
            parseDataValid = false;
            setInvalidationReason("Q_DIRECTION invalid");
          }
          trackDataItem.validDir = static_cast<ValidDirection>(tmpValU8);

          trackDataItem.optionalValue = vfwGetU16(&buffer);

          const bool isTDITargetsWithinMax = maData.trackDataItemVec.size() < maxNumberOfTrackDataItemTargets;
          const bool isAvailableMemoryTDITargets = ATP::DS::AbstractTargets::corePtr()->getFreeBlocksTDI() > maData.trackDataItemVec.size();

          if (isTDITargetsWithinMax && isAvailableMemoryTDITargets)
          {
            maData.trackDataItemVec.push_back(trackDataItem);
          }
          else
          {
            parseDataValid = false;
            setInvalidationReason("BTypeTrackDataItem overflow");
          }
          break;
        }

        default:
          parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
          break;
        }

        // Fetch next msg-type (or M_END_OF_MESSAGE)
        nextMsgIdentifier = vfwGetU8(&buffer);
      }

      if ((!validateSizeOfParsedBytes(&buffer)) && (parseDataValid))
      {
        parseDataValid = false;
        setInvalidationReason("Message size incorrect");
      }

      const size_t fullTargetSize = (maData.trackDataItemVec.size() + maData.ceilingSpeedDataVec.size() + maData.gradientDataVec.size() + maxNumberOfPrimaryTargets);

      if (!ATP::DS::AbstractTargets::corePtr()->targetListFitsVolume(fullTargetSize))
      {
        parseDataValid = false;
        setInvalidationReason("Too many targets in MA");
      }

      const uint32_t fullSupvTargetSize = (static_cast<uint32_t>(maData.ceilingSpeedDataVec.size() * maxNrSupvSpeedTargets) +
                                           static_cast<uint32_t>(maData.gradientDataVec.size() * maxNrSupvGradTargets) +
                                           static_cast<uint32_t>(maxNumberOfPrimaryTargets * maxNrSupvPrimaryTargets));

      if (!ATP::DS::AbstractTargets::corePtr()->supvTargetListFitsVolume(fullSupvTargetSize))
      {
        parseDataValid = false;
        setInvalidationReason("Too many supervised targets in MA");
      }

      // Log according to trace-level
      detailedLog();
      veryDetailedLog();

      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validateTrackData
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateTrackData()
    {
      bool trackDataValid = true;
      std::vector<TrackData>::const_iterator trackIt;
      // Fetch start-values for last ID
      uint16_t lastTrackID = 0U;
      bool isMAStartFound = false;
      bool isMAEndFound = false;

      if (maData.trackDataVec.size() > 0U)
      {
        if (maData.trackDataVec[0U].previousTrack != 0U)
        {
          // If the track is not in track-list and the previous track is also not in track list.. it is error.
          // The track is compared later that it is same as the stored track.
          if (DS::AbstractTracks::corePtr()->getTrack(maData.trackDataVec[0U].track) == static_cast<const DS::Track*>(NULL))
          {
            if (DS::AbstractTracks::corePtr()->getTrack(maData.trackDataVec[0U].previousTrack) == static_cast<const DS::Track*>(NULL))
            {
              trackDataValid = false;
              setInvalidationReason("MA extension: Previous track not found in storage");
            }
          }
        }

        // check if the end of MA from scratch with PARTLY_MA block is equal to the start of new MA extension.
        if ((!isValidPartlyMAInCurrMa) && isValidPartlyMAInPrevMa)
        {
          if ((maPartlyHead.endOfMATrackAndPos.track != maHead.startOfMATrackAndPos.track) ||
            (maPartlyHead.endOfMATrackAndPos.position != maHead.startOfMATrackAndPos.position))
          {
            trackDataValid = false;
            setInvalidationReason("Invalid start track or position of new MA");
          }
        }

        // Remove pure duplicates that are located next to each other
        static_cast<void>(maData.trackDataVec.erase(unique(maData.trackDataVec.begin(), maData.trackDataVec.end()), maData.trackDataVec.end()));

        // Start Iterator
        DS::AbstractTracks::ConstTrackListIteratorType trackListItr = DS::AbstractTracks::corePtr()->getTracksIter();
        // End Iterator
        DS::AbstractTracks::ConstTrackListIteratorType trackListItrEnd = DS::AbstractTracks::corePtr()->getTracksIterEnd();

        //iterate over stored tracks
        for (; (trackListItr != trackListItrEnd) && (!isMAStartFound); ++trackListItr)
        {
          if ((!isValidPartlyMAInCurrMa) && isValidPartlyMAInPrevMa)
          {
            if (maPartlyHead.startOfMATrackAndPos.track == (*trackListItr)->getTrackId())
            {
              isMAStartFound = true;
            }
          }
          else
          {
            if (maHead.startOfMATrackAndPos.track == (*trackListItr)->getTrackId())
            {
              isMAStartFound = true;
            }
          }
        }

        for (trackIt = maData.trackDataVec.begin(); (trackIt != maData.trackDataVec.end()) && trackDataValid; ++trackIt)
        {
          const TrackData& track = *trackIt;
          // Each track ID shall only exist once
          if (count_if(maData.trackDataVec.begin(), maData.trackDataVec.end(), TrackDataTrackIdComp(track.track)) != 1)
          {
            trackDataValid = false;
            setInvalidationReason("Multiple tracks with same ID");
          }

          // Each track should have the same travel direction.. not necessarily the orientation
          if (track.trvDir != dirAndOrientation2TravelDir(maHead.trainDirection))
          {
            trackDataValid = false;
            setInvalidationReason("Added tracks have different direction");
          }

          // Check if the list is continuous.. i.e. previous track id of the track is correct.
          if (trackIt != maData.trackDataVec.begin())
          {
            if (track.previousTrack != lastTrackID)
            {
              trackDataValid = false;
              setInvalidationReason("Added tracks are not continuous");
            }
          }

          if (!isMAStartFound)
          {
            if ((!isValidPartlyMAInCurrMa) && (isValidPartlyMAInPrevMa))
            {
              if (maPartlyHead.startOfMATrackAndPos.track == track.track)
              {
                isMAStartFound = true;
              }
            }
            else
            {
              if (maHead.startOfMATrackAndPos.track == track.track)
              {
                isMAStartFound = true;
              }
            }
          }

          // look for end of MA, after start of MA
          if (isMAStartFound)
          {
            if (maHead.endOfMATrackAndPos.track == track.track)
            {
              isMAEndFound = true;
            }
          }

          lastTrackID = track.track;
        }

        if (trackDataValid)
        {

          const Pos::PosAccuracyState currAccuracyState = Pos::AbstractPosition::corePtr()->getAccuracyState();

          const bool isValidPos = ((currAccuracyState == Pos::PosKnown) || (currAccuracyState == Pos::PosApprox));
          const bool isIdle = AbstractModeControl::corePtr()->getIdleState();
          const bool isTargetListEmpty = DS::AbstractTargets::corePtr()->isMATargetListEmpty();

          // Verify as an MA-scratch if no targets are stored and valid position and prev MA was not Partly MA
          // PARTLY_MA is checked if the train is Idling and has valid position.
          if (isTargetListEmpty && isValidPos && (!isValidPartlyMAInPrevMa))
          {
            trackDataValid = validateTrackDataIdleMode();
          }
          // Verify as a MA-extension if non-idling or if a PARTLY_MA has been accepted in previous MA.
          else if ((!isIdle) || (isValidPartlyMAInPrevMa))
          {
            trackDataValid = validateTrackDataNonIdleMode();
          }
          else
          {
            trackDataValid = false;
            setInvalidationReason("Train position not valid when Idling.");
          }

          if (!isMAStartFound)
          {
            trackDataValid = false;
            setInvalidationReason("Start of MA not found in tracks");
          }
          else if (!isMAEndFound)
          {
            trackDataValid = false;
            setInvalidationReason("End of MA not found beyond start of MA");
          }
          else
          {
            // Do nothing
          }
        }
      }
      else
      {
        const bool isTargetListEmpty = DS::AbstractTargets::corePtr()->isMATargetListEmpty();
        // If there are no tracks in the MA, it should be accepted only if there are existing targets (not idling) or
        // partly MA was received and the track was already in partly MA
        if (isTargetListEmpty && (!isValidPartlyMAInPrevMa))
        {
          trackDataValid = false;
        }
      }

      return trackDataValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validateTrackDataIdleMode
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateTrackDataIdleMode()
    {
      bool trackDataValid = true;

      // The first track must have previous TrackID = 0
      if (maData.trackDataVec.begin()->previousTrack != 0U)
      {
        trackDataValid = false;
        setInvalidationReason("MA extension received in idle");
      }
      else
      {
        std::vector<TrackData>::const_iterator trackIt = maData.trackDataVec.begin();
        bool startFound = false;
        bool maStartFound = false;
        bool maEndFound = false;
        DS::AbstractTracks::ConstTrackListIteratorType trackListItr = DS::AbstractTracks::corePtr()->getTracksIter();
        const DS::AbstractTracks::ConstTrackListIteratorType trackListItrBegin = DS::AbstractTracks::corePtr()->getTracksIter();
        const DS::AbstractTracks::ConstTrackListIteratorType trackListItrEnd = DS::AbstractTracks::corePtr()->getTracksIterEnd();

        const TravelDir tracksTrvDir = DS::AbstractTracks::corePtr()->getTravelDirection();
        const TravelDir tDir = dirAndOrientation2TravelDir(maHead.trainDirection);

        //initialize the track list iterator to last track if the direction of new MA is different to stored tracks.
        if (tracksTrvDir != tDir)
        {
          DS::AbstractTracks::ConstTrackListIteratorType tListItrEnd = DS::AbstractTracks::corePtr()->getTracksIterEnd();
          --tListItrEnd;
          trackListItr = tListItrEnd;
        }

        //try to find the first stored track in the MA if there are tracks in storage.
        if(trackListItr != trackListItrEnd)
        {
          while (trackIt != maData.trackDataVec.end())
          {
            if (maHead.startOfMATrackAndPos.track == trackIt->track)
            {
              maStartFound = true;
            }
            if (maHead.endOfMATrackAndPos.track == trackIt->track)
            {
              maEndFound = true;
            }
            if ((*trackListItr)->getTrackId() == trackIt->track)
            {
              startFound = true;
              break;
            }
            ++trackIt;
          }
        }

        if (startFound)
        {
          if (TG::AbstractTIMS::corePtr()->isRearPositionValid())
          {
            const OdoPosition leadingOdoPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();
            const OdoPosition trailingOdoPos = TG::AbstractTIMS::corePtr()->getRearPositionOdo();
            const TrackAndPos leadingPos = DS::AbstractTracks::corePtr()->calculateTrackAndPos(leadingOdoPos);
            const TrackAndPos trailingPos = DS::AbstractTracks::corePtr()->calculateTrackAndPos(trailingOdoPos);

            if ((leadingPos.track != 0U) && (trailingPos.track != 0U))
            {
              // the track(s) in the stored tracks must exist in MA
              // and these tracks must cover the train footprint
              bool isLeadingPosFound = false;
              bool isTrailingPosFound = false;
              bool breakLoop = false;
              while ((trackIt != maData.trackDataVec.end()) && (!breakLoop))
              {
                if (trackIt->track != (*trackListItr)->getTrackId())
                {
                  break;
                }
                if (maHead.endOfMATrackAndPos.track == trackIt->track)
                {
                  maEndFound = true;
                }

                if (trackIt->track == leadingPos.track)
                {
                  isLeadingPosFound = true;
                  
                  //If leading end is in the same track as the MA end
                  if (maHead.endOfMATrackAndPos.track == leadingPos.track)
                  {
                    
                    if (!isValidPartlyMAInCurrMa)
                    {
                      TravelDir trkTrvDir = getTrackTravelDir(trackIt->trvDir, trackIt->odoDir);

                      //If Leading end is ahead of the MA end
                      if (((DirForward == trkTrvDir) && (maHead.endOfMATrackAndPos.position < leadingPos.position)))
                      {
                        trackDataValid = false;
                        setInvalidationReason("MA end is before the leading end of the Loco #1");
                      }
                      else if ((DirReverse == trkTrvDir) && (maHead.endOfMATrackAndPos.position > leadingPos.position))
                      {
                        trackDataValid = false;
                        setInvalidationReason("MA end is before the leading end of the Loco #2");
                      }
                      else
                      {
                        //For Lint
                      }
                    }
                    else
                    {
                      //Partly_MA
                    }

                  }
                  else
                  {
                    //If leading end is in the different track as the MA end
                    if (maEndFound)
                    {
                      trackDataValid = false;
                      setInvalidationReason("MA end is before the leading end of the Loco in a different track");
                    }
                  }
   
                }

                if (maHead.startOfMATrackAndPos.track == trackIt->track)
                {
                  maStartFound = true;
                }

                if (trackIt->track == trailingPos.track)
                {
                  isTrailingPosFound = true;
                  //if start of MA track is not found, it means the MA start is ahead of the trailing end
                  if (!maStartFound)
                  {
                    trackDataValid = false;
                    setInvalidationReason("MA start after trailing position #1");
                  }
                  else
                  {
                    //if MA start is found on this track, check on position should be done
                    if (maHead.startOfMATrackAndPos.track == trackIt->track)
                    {
                      //Check the position on the track to verify that MA start is before trailing end.
                      const OdoDir trackDir = trackIt->odoDir;
                      uint32_t trailingPosition = trailingPos.position;

                      // To handle MA in the reverse direction
                      if (tracksTrvDir != tDir)
                      {
                        // If the driving direction is switched in MA then trailing train position will be the leading position.
                        trailingPosition = leadingPos.position;
                      }

                      if (tDir == DirReverse)
                      {
                        if (((trackDir == OdoPositive) && (maHead.startOfMATrackAndPos.position < trailingPosition)) ||
                          ((trackDir == OdoNegative) && (maHead.startOfMATrackAndPos.position > trailingPosition)))
                        {
                          trackDataValid = false;
                          setInvalidationReason("MA start after trailing position #2");
                        }
                      }
                      else if (tDir == DirForward)
                      {
                        if (((trackDir == OdoPositive) && (maHead.startOfMATrackAndPos.position > trailingPosition)) ||
                          ((trackDir == OdoNegative) && (maHead.startOfMATrackAndPos.position < trailingPosition)))
                        {
                          trackDataValid = false;
                          setInvalidationReason("MA start after trailing position #3");
                        }
                      }
                      else
                      {
                        // Satisfy lint
                      }
                    }
                  }
                }

                // if new MA direction is same as track direction, increment the iterator, else decrement
                if (DS::AbstractTracks::corePtr()->getTravelDirection() == (trackIt->trvDir))
                {
                  ++trackListItr;
                  if (trackListItr == trackListItrEnd)
                  {
                    breakLoop = true;
                  }
                }
                else
                {
                  if (trackListItr == trackListItrBegin)
                  {
                    breakLoop = true;
                  }
                  else
                  {
                    --trackListItr;
                  }
                }

                ++trackIt;
              }

              // The footprint doesn't need to be checked if it is the first part of PARTLY_MA.
              if (((!isLeadingPosFound) || (!isTrailingPosFound)) && (!isValidPartlyMAInCurrMa))
              {
                trackDataValid = false;
                setInvalidationReason("MA tracks don't cover train footprint");
              }
            }
            else
            {
              trackDataValid = false;
              setInvalidationReason("Train position not in stored tracks");
              ATC::AbstractEventHandler::corePtr()->reportEvent(criticalInvalidStateSafetyHalt, __FILE__, __LINE__);
            }
          }
          else
          {
            trackDataValid = false;
            setInvalidationReason("Rear position is not valid");
          }
        }
        else
        {
          trackDataValid = false;
          setInvalidationReason("First stored track not found in MA");
        }
      }

      return trackDataValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validateTrackDataNonIdleMode
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateTrackDataNonIdleMode()
    {
      bool trackDataValid = true;
      std::vector<TrackData>::const_iterator trackIt = maData.trackDataVec.begin();
      const TravelDir receivedTravelDirectionTrack = trackIt->trvDir;
      bool startPtFoundInPartMA = false;
      bool endPtFoundInPartMA = false;
      uint32_t trackLen = 0U;
      const ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
      const TrainRegistrationModeState regState = AbstractModeControl::corePtr()->getTrainRegistrationModeState();
      const bool isReRegistration = regState == TrainRegistrationMode::trainRegistrationWaitReRegMA;
      const bool isRePosition = regState == TrainRegistrationMode::trainRegistrationRePosition;

      if (maData.trackDataVec[0U].previousTrack == 0U) // MA from scratch
      {
        if ((currentMode == ATPModeBaliseSearch) ||
          ((currentMode == ATPModeRegistration) && isReRegistration) ||
          ((currentMode == ATPModeRegistration) && isRePosition) ||
          isValidPartlyMAInPrevMa)
        {
          // Do nothing - MA from scratch is allowed here
        }
        else
        {
          trackDataValid = false;
          setInvalidationReason("MA from scratch received in wrong mode");
        }
      }

      // Loop through the TRACK_DATA blocks as long as track data is still valid
      while ((trackIt != maData.trackDataVec.end()) && trackDataValid)
      {
        const TrackData& track = *trackIt;

        // Check total length of PARTLY_MA:s (to make sure it together will cover the train-footprint)
        if ((!isValidPartlyMAInCurrMa) && isValidPartlyMAInPrevMa)
        {
          if (maHead.endOfMATrackAndPos.track == track.track)
          {
            endPtFoundInPartMA = true;
            // Special case if start/end track is the same --> Overwrite trackLen with diff from start/end of MA.
            if (maPartlyHead.startOfMATrackAndPos.track == maHead.endOfMATrackAndPos.track)
            {
              startPtFoundInPartMA = true;
              trackLen = ATC::ATCMath::absDiff(maPartlyHead.startOfMATrackAndPos.position, maHead.endOfMATrackAndPos.position);
            }
            else
            {
              if (DirForward == getTrackTravelDir(track.trvDir, track.odoDir))
              {
                trackLen += maHead.endOfMATrackAndPos.position;
              }
              else if (DirReverse == getTrackTravelDir(track.trvDir, track.odoDir))
              {
                trackLen += ATC::ATCMath::absDiff(maHead.endOfMATrackAndPos.position, track.length);
              }
              else
              {
                trackDataValid = false;
                setInvalidationReason("TRACK_DATA has incorrect direction #2");
              }
            }
          }
          else if (maPartlyHead.startOfMATrackAndPos.track == track.track)
          {
            startPtFoundInPartMA = true;

            if (DirForward == getTrackTravelDir(track.trvDir, track.odoDir))
            {
              trackLen += ATC::ATCMath::absDiff(maPartlyHead.startOfMATrackAndPos.position, track.length);
            }
            else if (DirReverse == getTrackTravelDir(track.trvDir, track.odoDir))
            {
              trackLen += maPartlyHead.startOfMATrackAndPos.position;
            }
            else
            {
              trackDataValid = false;
              setInvalidationReason("TRACK_DATA has incorrect direction #1");
            }
          }
          // Add the whole track-length for actual track if not track for MA-end is found yet.
          else if (startPtFoundInPartMA && (!endPtFoundInPartMA))
          {
            trackLen += track.length;
          }
          else
          {
            // Do nothing
          }
        }

        // Does track already exist in storage? -> Must be equal to received track.
        // This check does not apply for PARTLY_MA:s since first PARTLY_MA is starting from Idle
        // and doesn't store any tracks until this MA is processed.
        if (!isValidPartlyMAInPrevMa)
        {
          const DS::Track* pStoredTrack = DS::AbstractTracks::corePtr()->getTrack(track.track);

          if ((pStoredTrack != static_cast<DS::Track*>(NULL)) && trackDataValid)
          {
            TrackData storedTrackData;

            // Fetch the values from the stored track and save in TrackData to be able to compare
            // received and already stored track.
            // reset the directionAndOrientation of storedTrackData

            storedTrackData.trvDir = pStoredTrack->getTravelDirection();
            storedTrackData.odoDir = pStoredTrack->getOdoDirection();
            storedTrackData.length = pStoredTrack->getTrackLength();
            storedTrackData.previousTrack = pStoredTrack->getIncomingTrackId();
            storedTrackData.track = pStoredTrack->getTrackId();

            // Check consistency with already stored track
            if (track == storedTrackData)
            {
              // Track already exists in storage, and it is same.
            }
            else
            {
              trackDataValid = false;
              setInvalidationReason("Track mismatch compared to stored track");
            }
          }
          else
          {
            if (DS::AbstractTracks::corePtr()->getTracksIter() != DS::AbstractTracks::corePtr()->getTracksIterEnd())
            {
              // Track didn't exist in storage -> Validate that received travel direction is the same as in storage
              if (!(DS::AbstractTracks::corePtr()->getTravelDirection() == receivedTravelDirectionTrack))
              {
                trackDataValid = false;
                setInvalidationReason("Received travel direction mismatch with storage");
              }
            }
          }
        }

        // Get next TRACK_DATA
        ++trackIt;
      }

      if ((!isValidPartlyMAInCurrMa) && isValidPartlyMAInPrevMa && trackDataValid)
      {
        if (startPtFoundInPartMA && endPtFoundInPartMA)
        {
          const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

          if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
          {
            if (pTrainSetup->length > trackLen)
            {
              trackDataValid = false;

              char_t buffer[200];
              //lint -e{586} snprintf is needed here
              const int32_t res = snprintf(&buffer[0], sizeof(buffer),
                "Insufficient length of MA to accommodate the train (length = %u, trackLen= %u)",
                pTrainSetup->length, trackLen);

              if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(buffer)))
              {
                setInvalidationReason(&buffer[0]);
              }
            }
          }
          else
          {
            trackDataValid = false;
            setInvalidationReason("Unable to get TrainSetup");
          }
        }
        else
        {
          trackDataValid = false;

          char_t buffer[200];
          //lint -e{586} snprintf is needed here
          const int32_t res = snprintf(&buffer[0], sizeof(buffer),
            "Unable to find start/end of MA startPtFoundInPartMA: %s endPtFoundInPartMA: %s",
            startPtFoundInPartMA ? "True" : "False", endPtFoundInPartMA ? "True" : "False");

          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(buffer)))
          {
            setInvalidationReason(&buffer[0]);
          }
        }
      }

      return trackDataValid;
    }


    /******************************************************************************
    * RadioMessageInMovementAuthority::validateBaliseData
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateBaliseData()
    {
      bool baliseDataValid = true;
      if (!maData.baliseDataVec.empty())
      {
        // Remove pure duplicates that are located next to each other
        static_cast<void>(maData.baliseDataVec.erase(unique(maData.baliseDataVec.begin(), maData.baliseDataVec.end()), maData.baliseDataVec.end()));

        // if in balise search mode and waiting for MA to check Second Balise after first balise
        std::vector<BaliseData>::const_iterator registrationBaliseIt = maData.baliseDataVec.end();
        const ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
        BaliseSearchModeState bsModeState = AbstractModeControl::corePtr()->getBaliseSearchModeState();
        if ((currentMode == ATPModeBaliseSearch) && (bsModeState == BaliseSearchMode::baliseSearchWaitMA))
        {
          // get the first balise info from Position
          Pos::AbstractDecode::BaliseInfo registrationBalise = {0U, 0};
          if (Pos::AbstractPosition::corePtr()->getFirstBaliseInfo(registrationBalise))
          {
            registrationBaliseIt = std::find_if(
              maData.baliseDataVec.begin(),
              maData.baliseDataVec.end(),
              BaliseDataBaliseIdComp(registrationBalise.nidBG));

            if (registrationBaliseIt == maData.baliseDataVec.end())
            {
              baliseDataValid = false;
              // Raise the safe brake to stop
              ATC::AbstractEventHandler::corePtr()->reportEvent(firstBaliseNotInMA,
                __FILE__, __LINE__);
            }
          }
        }

        bool secondBaliseFound = false;
        std::vector<BaliseData>::const_iterator baliseIt = maData.baliseDataVec.begin();
        for (; (baliseIt != maData.baliseDataVec.end()) && (baliseDataValid); ++baliseIt)
        {
          const TrackAndPos balisePos = baliseIt->baliseTrackAndPosition;

          // Internal BaliseList check -> Each balise ID shall only exist once
          if (count_if(maData.baliseDataVec.begin(), maData.baliseDataVec.end(), BaliseDataBaliseIdComp(baliseIt->baliseId)) != 1)
          {
            baliseDataValid = false;
            setInvalidationReason("Multiple balises with same ID");
          }

          // There can be only one balise at one position.. as the balise list is sorted,
          // the consecutive balises should not have same track and position.
          std::vector<BaliseData>::const_iterator baliseItNext = baliseIt;
          ++baliseItNext;
          if (baliseItNext != maData.baliseDataVec.end())
          {
            const bool isTrackEqual = (baliseItNext->baliseTrackAndPosition.track == balisePos.track);
            const bool isPositionEqual = (baliseItNext->baliseTrackAndPosition.position == balisePos.position);
            if (isTrackEqual && isPositionEqual)
            {
              baliseDataValid = false;
              setInvalidationReason("Multiple balises at same track and position");
            }
          }

          TrackAndPos startOfMATrackAndPos = maHead.startOfMATrackAndPos;
          if (isValidPartlyMAInPrevMa && (!isValidPartlyMAInCurrMa))
          {
            startOfMATrackAndPos = maPartlyHead.startOfMATrackAndPos;
          }

          // Is Track for Balise present in MA?
          std::vector<TrackData>::const_iterator trackIt =
            find_if(maData.trackDataVec.begin(), maData.trackDataVec.end(), TrackDataTrackIdComp(balisePos.track));

          if (trackIt != maData.trackDataVec.end())
          {
            const TrackData& track = *trackIt;

            // Check if the position is within the track
            bool baliseBeforeEnd = balisePos.position < track.length;

            if (!baliseBeforeEnd)
            {
              baliseDataValid = false;
              setInvalidationReason("Balise position not within TRACK_DATA");
            }

            const TravelDir trkTrvDir = getTrackTravelDir(track.trvDir, track.odoDir);

            // If Balise is received on starting track and position of the Balise is found before start of in MA.
            // when Balise track is not stored in tracks and balised is found before start of MA.
            if (startOfMATrackAndPos.track == balisePos.track)
            {
              const bool isInvalidBalisePosInForwardDir = startOfMATrackAndPos.position > balisePos.position;
              const bool isInvalidBalisePosInReverseDir = startOfMATrackAndPos.position < balisePos.position;

              if (((trkTrvDir == DirForward) && isInvalidBalisePosInForwardDir) 
                || ((trkTrvDir == DirReverse) && isInvalidBalisePosInReverseDir))
              {
                baliseDataValid = false;
                setInvalidationReason("Balise found before start of MA #1");
              }
            }

            // When Balise track is not stored in tracks and balised is found after end of MA.
            if (maHead.endOfMATrackAndPos.track == balisePos.track)
            {
              const bool isInvalidBalisePosInForwardDir = maHead.endOfMATrackAndPos.position < balisePos.position;
              const bool isInvalidBalisePosInReverseDir = maHead.endOfMATrackAndPos.position > balisePos.position;

              if (((trkTrvDir == DirForward) && isInvalidBalisePosInForwardDir) 
                || ((trkTrvDir == DirReverse) && isInvalidBalisePosInReverseDir))
              {
                baliseDataValid = false;
                setInvalidationReason("Balise found after end of MA");
              }     
            }

            if (registrationBaliseIt != maData.baliseDataVec.end())
            {
              const TrackAndPos registrationBalisePos = registrationBaliseIt->baliseTrackAndPosition;
              std::vector<TrackData>::const_iterator registrationBaliseTrackIt =
                find_if(maData.trackDataVec.begin(), maData.trackDataVec.end(), TrackDataTrackIdComp(registrationBalisePos.track));

              if (balisePos.track == registrationBalisePos.track)
              {
                // This balise is on the same track as the registration balise

                if ( ((trkTrvDir == DirForward) && (balisePos.position > registrationBalisePos.position))
                  || ((trkTrvDir == DirReverse) && (balisePos.position < registrationBalisePos.position)) )
                {
                  secondBaliseFound = true;
                } 
              }
              else
              {
                // This balise is not on the same track as the registration balise

                if (trackIt > registrationBaliseTrackIt)
                {
                  secondBaliseFound = true;
                }
              }
            }
          }
          else
          {
            // Actual Track for Balise is not found in MA -> Look in DataStorage 
            // (Note: Only for MA extension, MA from scratch need all Tracks in MA. ValidateTrackData() makes sure the footprint is included in MA.)
            const DS::Track* const pStoredTrack = DS::AbstractTracks::corePtr()->getTrack(balisePos.track);

            if (pStoredTrack != static_cast<DS::Track*>(NULL))
            {
              // Check if the position is within the track
              bool baliseBeforeEnd = balisePos.position < pStoredTrack->getTrackLength();

              if (!baliseBeforeEnd)
              {
                baliseDataValid = false;
                setInvalidationReason("Balise position not within track");
              }
              else
              {
                // See if track is stored on board, and if the Balise is before start of MA.
                OdoPosition odoValueOfBalise;
                const bool isValidStoredOnboardBalisePosition = DS::AbstractTracks::corePtr()->getOdoPos(balisePos, odoValueOfBalise);

                OdoPosition odoStartOfMaPos;
                const bool isvalidOdoStartOfMAPos = DS::AbstractTracks::corePtr()->getOdoPos(startOfMATrackAndPos, odoStartOfMaPos);

                if (isValidStoredOnboardBalisePosition && isvalidOdoStartOfMAPos)
                {
                  const TravelDir travelDir = pStoredTrack->getTravelDirection();

                  if (((travelDir == DirForward) && (odoValueOfBalise < odoStartOfMaPos)) ||
                    ((travelDir == DirReverse) && (odoValueOfBalise > odoStartOfMaPos)))
                  {
                    baliseDataValid = false;
                    setInvalidationReason("Balise found before start of MA #2");
                  }
                }
                else
                {
                  baliseDataValid = false;
                  setInvalidationReason("Balise not found within MA");
                }
              }
            }
            else
            {
              baliseDataValid = false;
              setInvalidationReason("Balise track not valid in Storage or TRACK_DATA.");
            }
          }

          if (baliseDataValid)
          {
            baliseDataValid = validateBaliseDataInStorage(*baliseIt);
          }

        }// End, Loop through the BALISE_DATA blocks

        if ((registrationBaliseIt != maData.baliseDataVec.end()) && (!secondBaliseFound))
        {
          baliseDataValid = false;
          setInvalidationReason("No balise found in front of registration balise");
        }

        const bool isIdleState = AbstractModeControl::corePtr()->getIdleState();
        if (baliseDataValid && isIdleState)
        {
          baliseDataValid = validateBaliseDataIdleMode();
        }
      }

      return baliseDataValid;
    }


    /******************************************************************************
    * RadioMessageInMovementAuthority::validateBaliseDataInStorage
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateBaliseDataInStorage(const BaliseData& baliseData)
    {
      // Valid as default
      bool baliseDataValid = true;

      // Only needed if tracks exists in storage i e Extension MA
      if (DS::AbstractTracks::corePtr()->getTracksIter() != DS::AbstractTracks::corePtr()->getTracksIterEnd())
      {
        const DS::Balise* const foundBaliseId = DS::AbstractTracks::corePtr()->getBalise(baliseData.baliseId);
        const DS::Balise* const foundBalisePos = DS::AbstractTracks::corePtr()->getBalise(baliseData.baliseTrackAndPosition);

        if (foundBaliseId != static_cast<const DS::Balise* const>(NULL))
        {
          // The balise Id is already present in storage, check if it has the same data

          const BaliseData storedBaliseData(foundBaliseId->getBaliseId(), foundBaliseId->getPosition());
          if (baliseData == storedBaliseData)
          {
            // Balise already exists in storage, and is same as the new balise.
          }
          else
          {
            baliseDataValid = false;
            setInvalidationReason("Balise mismatch compared to stored balise");
          }
        }
        else if (foundBalisePos != static_cast<const DS::Balise* const>(NULL))
        {
          // The balise Id wasn't found in storage but the position is the same as another stored Balise -> Error

          baliseDataValid = false;
          setInvalidationReason("Balise already exists at same track and position");
        }
        else
        {
          // A new balise
        }
      }

      return baliseDataValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validateBaliseDataIdleMode
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateBaliseDataIdleMode()
    {
      bool baliseDataValid = true;

      if (TG::AbstractTIMS::corePtr()->isRearPositionValid())
      {
        const OdoPosition leadingOdoPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();
        const OdoPosition trailingOdoPos = TG::AbstractTIMS::corePtr()->getRearPositionOdo();
        const OdoPosition lowPosition = (leadingOdoPos > trailingOdoPos) ? trailingOdoPos : leadingOdoPos;
        const OdoPosition highPosition = (leadingOdoPos > trailingOdoPos) ? leadingOdoPos : trailingOdoPos;

        // Iterate over the stored balises
        DS::AbstractTracks::ConstBaliseListIteratorType baliseItr = DS::AbstractTracks::corePtr()->getBaliseIter();
        DS::AbstractTracks::ConstBaliseListIteratorType baliseEndItr = DS::AbstractTracks::corePtr()->getBaliseIterEnd();
        for (; (baliseItr != baliseEndItr) && baliseDataValid; ++baliseItr)
        {
          // If PARTLY_MA is received in MA scratch from TCC1 then Wait for next MA Extension from another TCC2 with the balises list.
          // Check if the balises in train footprint against MA received from TCC2.
          if ((!isValidPartlyMAInCurrMa) && isValidPartlyMAInPrevMa)
          {
            const DS::Balise* balise = *baliseItr;
            const OdoPosition baliseOdoPosition = balise->getOdoPosition();
            // Check only balises in train footprint against new MA
            if ((baliseOdoPosition >= lowPosition)
              && (baliseOdoPosition <= highPosition))
            {
              const std::vector<BaliseData>::const_iterator maBaliseItr = std::find_if(
                maData.baliseDataVec.begin(),
                maData.baliseDataVec.end(),
                BaliseDataBaliseIdComp(balise->getBaliseId()));

              if (maBaliseItr == maData.baliseDataVec.end())
              {
                baliseDataValid = false;
                setInvalidationReason("Stored balise not found in MA");
              }
            }
          }
          else
          {
            baliseDataValid = true;
          }
        }
      }
      return baliseDataValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validateTargetsData
    ******************************************************************************/
    template<typename TargetData>
    bool RadioMessageInMovementAuthority::validateTargetsData(std::vector<TargetData> &targetVector)
    {
      bool targetDataValid = true;

      typename std::vector<TargetData>::const_iterator targetIt = targetVector.begin();
      typename std::vector<TargetData>::const_iterator targetItEnd = targetVector.end();

      TrackAndPos startOfMATrackAndPos = maHead.startOfMATrackAndPos;

      //if partly MA was received, use the MA start from the partly MA to verify the targets
      if (isValidPartlyMAInPrevMa && (!isValidPartlyMAInCurrMa))
      {
        startOfMATrackAndPos = maPartlyHead.startOfMATrackAndPos;
      }

      for (; (targetIt != targetItEnd) && (targetDataValid); ++targetIt)
      {
        TravelDir trackTrvDir = DirUndefined;
        uint32_t trackLength = 0U;
        const TargetData& target = *targetIt;

        std::vector<TrackData>::const_iterator trackItr =
          find_if(maData.trackDataVec.begin(), maData.trackDataVec.end(), TrackDataTrackIdComp(target.trackAndPosition.track));

        if (trackItr != maData.trackDataVec.end())
        {
          // If target track within MA Track Data save necessary info of track for validation
          trackTrvDir = getTrackTravelDir(trackItr->trvDir, trackItr->odoDir);
          trackLength = trackItr->length;
        }
        else
        {
          if (startOfMATrackAndPos.track == target.trackAndPosition.track)
          {
            const DS::Track* pStoredTrack = DS::AbstractTracks::corePtr()->getTrack(target.trackAndPosition.track);
            if (NULL != pStoredTrack)
            {
              // If target track not within MA Track Data but in Saved tracks save necessary info of track for validation
              trackLength = pStoredTrack->getTrackLength();
              trackTrvDir = getTrackTravelDir(pStoredTrack->getTravelDirection(), pStoredTrack->getOdoDirection());
            }
            else
            {
              // If target track not in MA track data nor stored tracks it is invalid
              targetDataValid = false;
              setInvalidationReason("Target track not valid in Storage or TRACK_DATA.");
            }
          }
          else
          {
            // If target track not in MA track data nor stored tracks it is invalid
            targetDataValid = false;
            setInvalidationReason("Target track not within MA.");
          }
        }

        if (targetDataValid)
        {
          // Check if the position is within the track
          if (target.trackAndPosition.position > trackLength)
          {
            targetDataValid = false;
            setInvalidationReason("Target position not within TRACK_DATA");
          }

          if (maHead.endOfMATrackAndPos.track == target.trackAndPosition.track)
          {
            //verify the target position is not after MA end considering the travel direction
            if (((trackTrvDir == DirForward) && (maHead.endOfMATrackAndPos.position < target.trackAndPosition.position)) ||
              ((trackTrvDir == DirReverse) && (maHead.endOfMATrackAndPos.position > target.trackAndPosition.position)))
            {
              targetDataValid = false;
              setInvalidationReason("Target position after MA end position.");
            }
          }
          else if (startOfMATrackAndPos.track == target.trackAndPosition.track)
          {
            //verify the target position is not before MA start considering the travel direction
            if (((trackTrvDir == DirForward) && (startOfMATrackAndPos.position > target.trackAndPosition.position)) ||
              ((trackTrvDir == DirReverse) && (startOfMATrackAndPos.position < target.trackAndPosition.position)))
            {
              targetDataValid = false;
              setInvalidationReason("Target position before MA start position.");
            }
          }
          else
          {
            //target located in a track between start and end tracks, thus position is valid
          }
        }
      }

      return targetDataValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validatePrimaryTargetData
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validatePrimaryTargetData()
    {
      bool primaryTargetDataValid = true;
      bool maEndTrackValidated = false;

      // Validate MA start and end towards the first and last track in MA track data if there are any
      if (!maData.trackDataVec.empty())
      {
        // MA end validation
        if (maData.trackDataVec.back().track == maHead.endOfMATrackAndPos.track)
        {
          if (maHead.endOfMATrackAndPos.position > maData.trackDataVec.back().length)
          {
            //if MA end track is same as last MA track data and position value is larger than length of track, end position is invalid.
            primaryTargetDataValid = false;
            setInvalidationReason("Invalid position of MA end");
          }
          else
          {
            maEndTrackValidated = true;
          }
        }

        // MA start validation
        if (maData.trackDataVec.front().track == maHead.startOfMATrackAndPos.track)
        {
          if (maHead.startOfMATrackAndPos.position > maData.trackDataVec.front().length)
          {
            //if MA start track is same as first MA track data and position value is larger than length of track, start position is invalid.
            primaryTargetDataValid = false;
            setInvalidationReason("Invalid position of MA start");
          }
        }
      }

      const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

      if (pTrainSetup == static_cast<const DS::TrainSetup*>(NULL))
      {
        primaryTargetDataValid = false;
        setInvalidationReason("Unable to fetch train-setup");
      }

      const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      // if not in idleMode,  Registration-> The target must also be validated towards the stored tracks. 
      if ((!AbstractModeControl::corePtr()->getIdleState()) && 
        (ATPModeRegistration != currentMode) && 
        primaryTargetDataValid)
      {
        const TravelDir suppTrvDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();

        // Ma direction should be same as the travel direction
        if ((dirAndOrientation2TravelDir(maHead.trainDirection) != suppTrvDir) && (ATPModeLocation != currentMode))
        {
          if ((suppTrvDir != DirUndefined) && (suppTrvDir != DirNone))
          {
            primaryTargetDataValid = false;
            setInvalidationReason("Direction mismatch of Primary target");
          }
        }

        // Validate MA end track in stored track list except if in balise search mode
        if ((ATPModeBaliseSearch != currentMode) && primaryTargetDataValid)
        {
          const DS::Track* const pMAEndTrack = DS::AbstractTracks::corePtr()->getTrack(maHead.endOfMATrackAndPos.track);

          if (pMAEndTrack != static_cast<const DS::Track*>(NULL))
          {
            // compare MA end position with primary target position to check if MA end is ahead of the current primary target
            const DS::BaseTarget* pPrimaryTarget =
              DS::AbstractTargets::corePtr()->getPrimaryTarget(dirAndOrientation2TravelDir(maHead.trainDirection));
            if (pPrimaryTarget != static_cast<DS::BaseTarget*>(NULL))
            {
              const TrackAndPos pPrimaryTargetPos = pPrimaryTarget->getPosition();
              TravelDir trkTrvDir = getTrackTravelDir(pMAEndTrack->getTravelDirection(), pMAEndTrack->getOdoDirection());

              // If new MA end is within the current MA /location MA end position same as normal target position
              if ((pPrimaryTargetPos.track != maHead.endOfMATrackAndPos.track) ||
                ((trkTrvDir == DirForward) && (maHead.endOfMATrackAndPos.position < pPrimaryTargetPos.position)) ||
                ((trkTrvDir == DirReverse) && (maHead.endOfMATrackAndPos.position > pPrimaryTargetPos.position)))
              {
                primaryTargetDataValid = false;
                setInvalidationReason("Invalid MA end position");
              }
            }
            else
            {
              primaryTargetDataValid = false;
              setInvalidationReason("MA extension without prior Primary target");
              ATC::AbstractEventHandler::corePtr()->reportEvent(criticalInvalidStateSafetyHalt, __FILE__, __LINE__);
            }
          }
          else if (!maEndTrackValidated)
          {
            // MA end not in MA Track data nor stored tracks
            primaryTargetDataValid = false;
            setInvalidationReason("MA end track not found");
          }
          else
          {
            // Please lint
          }
        }

        // Start position validation
        if ((ATPModeBaliseSearch != currentMode) &&
          (ATPModeSafeBrakeToStop != currentMode) &&
          primaryTargetDataValid)
        {
          const DS::BaseTarget* pPrimaryTarget = 
            DS::AbstractTargets::corePtr()->getPrimaryTarget(dirAndOrientation2TravelDir(maHead.trainDirection));

          if (pPrimaryTarget != static_cast<DS::BaseTarget*>(NULL))
          {
            const TrackAndPos primaryTargetPos = pPrimaryTarget->getPosition();
            const DS::Track* primaryTargetTrack = DS::AbstractTracks::corePtr()->getTrack(primaryTargetPos.track);

            if (primaryTargetTrack != static_cast<const DS::Track*>(NULL))
            {
              const TravelDir trkTrvDir = getTrackTravelDir(primaryTargetTrack->getTravelDirection(), primaryTargetTrack->getOdoDirection());
              primaryTargetDataValid = validateStartOfMA(primaryTargetPos.track, primaryTargetPos.position, primaryTargetTrack->getTrackLength(), trkTrvDir);
            }
            else
            {
              primaryTargetDataValid = false;
              setInvalidationReason("Primary target track not found");
            }
          }
          else
          {
            primaryTargetDataValid = false;
            setInvalidationReason("Missing primary target");
            ATC::AbstractEventHandler::corePtr()->reportEvent(criticalInvalidStateSafetyHalt, __FILE__, __LINE__);
          }
        }
      }

      return primaryTargetDataValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validateStartOfMA
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateStartOfMA(
        const uint16_t endTrack, const uint32_t endPosition, const uint32_t endTrackLength, const TravelDir trkTrvDir)
    {
      bool valid = true;

      if (endTrack == maHead.startOfMATrackAndPos.track)
      {
        if (endPosition != maHead.startOfMATrackAndPos.position)
        {
          valid = false;
          setInvalidationReason("Invalid start position of new MA #1");
        }
      }
      else
      {
        // If the previous end of MA lies between track start and track end then the coming case will not be possible
        if (((trkTrvDir == DirForward) && (endPosition != endTrackLength)) ||
            ((trkTrvDir == DirReverse) && (endPosition != 0U)))
        {
          valid = false;
          setInvalidationReason("Invalid start position of new MA #2");
        }
        else
        {
          // The previous end of MA is at the furthest end of its track, so the new MA start must be at the closest end of the next track
          std::vector<TrackData>::const_iterator startOfMATrackIt =
            find_if(maData.trackDataVec.begin(), maData.trackDataVec.end(), TrackDataTrackIdComp(maHead.startOfMATrackAndPos.track));

          if (startOfMATrackIt != maData.trackDataVec.end())
          {
            //lint -e{1960} No side effects here
            if ((endTrack != startOfMATrackIt->previousTrack) ||
              ((trkTrvDir == DirForward) && (maHead.startOfMATrackAndPos.position != 0U)) ||
              ((trkTrvDir == DirReverse) && (maHead.startOfMATrackAndPos.position != startOfMATrackIt->length)))
            {
              valid = false;
              setInvalidationReason("Invalid start position of new MA #3");
            }
          }
          else
          {
            valid = false;
            setInvalidationReason("Invalid start track of new MA");
          }
        }
      }

      return valid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validateLocationTargetData
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateLocationTargetData()
    {
      bool isLocationTargetDataValid = true;

      const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      DS::BaseTarget* const locStartTarget = DS::AbstractTargets::corePtr()->getLocationStartTarget();

      // Check if location border is empty
      if (maData.locationBordersReceived)
      {
        // location border accepted only if the ATP mode is normal or location
        if ((ATPModeNormal == currentMode) || (ATPModeLocation == currentMode))
        {
          //lint -e{1960} No side effects here
          if ((locStartTarget != static_cast<DS::BaseTarget *>(NULL)) || (maData.locationDataReceived))
          {
            if ((locStartTarget != static_cast<DS::BaseTarget *>(NULL)) && (maData.locationDataReceived))
            {
              // We need to get the location type from one of the targets (as both the target will have same type
              const DS::PrimaryTarget* const primaryTarget =
                ATC::dynamicCast<DS::BaseTarget*, DS::PrimaryTarget*>(locStartTarget, __FILE__, __LINE__);

              char_t locationName[maxLocationNameLength + 1U];
              memset(locationName, 0, sizeof(locationName));

              if (primaryTarget->getLocationName(&locationName[0]))
              {
                if ((primaryTarget->getLocationType() != maData.locationData.locationType)
                  || (strncmp(locationName, maData.locationData.locationName, maxLocationNameLength + 1U) != 0))
                {
                  isLocationTargetDataValid = false;
                  setInvalidationReason("Location data doesn't match previously received location");
                }
              }
            }

            if (isLocationTargetDataValid)
            {
              // Unless location end position same as end of MA position the location data is incorrect
              if ((maHead.endOfMATrackAndPos == maData.locationBorders.endOfLocation))
              {
                isLocationTargetDataValid = validateTrainFootprintAndLocationBorder(maData.locationBorders);
              }
              else
              {
                isLocationTargetDataValid = false;
                setInvalidationReason("Location end is not the same as MA end");
              }
            }
          }
          else
          {
            isLocationTargetDataValid = false;
            setInvalidationReason("LOCATION_BORDERS is present but LOCATION_DATA is missing");
          }
        }// end of mode check
        else
        {
          isLocationTargetDataValid = false;
          setInvalidationReason("Incorrect mode for LOCATION_BORDERS");
        }
      }// End of location vector size check
      // Check if there is any location startTarget
      else if ((static_cast<DS::BaseTarget *>(NULL) != locStartTarget))
      {
        isLocationTargetDataValid = false;
        setInvalidationReason("Location start target is present but LOCATION_BORDERS is missing");
      }
      else if (maData.locationDataReceived)
      {
        isLocationTargetDataValid = false;
        setInvalidationReason("LOCATION_DATA present but LOCATION_BORDERS is missing");
      }
      else if (ATPModeLocation == currentMode)
      {
        isLocationTargetDataValid = false;
        setInvalidationReason("LOCATION_BORDERS and LOCATION_DATA are missing in mode Location");
      }
      else
      {
        // do nothing
      }

      return isLocationTargetDataValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validateNotReadyToDriveStatus
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateNotReadyToDriveStatus()
    {
      const bool isNotReadyToDrive = AbstractModeControl::corePtr()->getNotReadyToDrive();

      if (isNotReadyToDrive)
      {
        setInvalidationReason("NotReadyToDrive is activated.");
      }

      // The validation is true if the NotReadyToDrive is NOT activated.
      return (!isNotReadyToDrive);
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validateEmergencyAlertStatus
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateEmergencyAlertStatus()
    {
      const Kernel::EmergencyAlertState currentEmergencyState = Kernel::AbstractModeControl::corePtr()->getEmergencyAlertSeqState();

      if (currentEmergencyState != Kernel::emergencyAlertInactive)
      {
        setInvalidationReason("Emergency Alert is activated.");
      }

      // The validation is true if the Emergency Alert is NOT activated.
      return (currentEmergencyState == Kernel::emergencyAlertInactive);
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validateMaxSearchDistance
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateMaxSearchDistance()
    {
      bool isMaxSearchDistance = true;
      // Check if Maximum Search distance block is empty in ReRegistration
      if (maHead.routeType == RtReRegistration)
      {
        if (!maData.maxSearchDistReceived)
        {
          isMaxSearchDistance = false;
          setInvalidationReason("Maximum Search Distance block not available in MA ReReg");
        }
      }
      return isMaxSearchDistance;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::isTrainLengthInLocationBorder
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateTrainFootprintAndLocationBorder(const LocationBorder &locBorder)
    {
      uint32_t trackLen = 0U;
      bool locationStartTrackFound = false;
      bool locationEndTrackFound = false;

      // validate the location border in storage
      bool isLocDataValid = validateLocationBorderInStorage(locationStartTrackFound, locationEndTrackFound, trackLen, locBorder);

      std::vector<TrackData>::const_iterator trackIt = maData.trackDataVec.begin();
      // Iterate over the Ma track list
      for (; ((trackIt != maData.trackDataVec.end()) && (!locationEndTrackFound) && isLocDataValid); ++trackIt)
      {
        const TrackData& trackData = *trackIt;

        // Start location
        if ((locBorder.startOfLocation.track == trackData.track) && (!locationStartTrackFound))
        {
          TravelDir tDir = getTrackTravelDir(trackData.trvDir, trackData.odoDir);

          if (locBorder.startOfLocation.position > trackData.length) // Verify the location start position is within the track
          {
            isLocDataValid = false;
            setInvalidationReason("Location start position is outside track #1");
          }
          else if ((locBorder.startOfLocation.track == maHead.endOfMATrackAndPos.track) &&
            (((tDir == DirForward) && (locBorder.startOfLocation.position > maHead.endOfMATrackAndPos.position)) ||
            ((tDir == DirReverse) && (locBorder.startOfLocation.position < maHead.endOfMATrackAndPos.position))))
          {
            // if location start track same as MA end track and location position ahead of MA end position it is invalid
            isLocDataValid = false;
            setInvalidationReason("Location start position is ahead of MA end #1");
          }
          else if (locBorder.startOfLocation.track == maHead.endOfMATrackAndPos.track)
          {
            // Handle the case when location start is in Ma end track. Location end track is verified to be MA end track before this
            // function is called.
            locationEndTrackFound = true;
            trackLen = ATC::ATCMath::absDiff(maHead.endOfMATrackAndPos.position, locBorder.startOfLocation.position);
          }
          else
          {
            if (tDir == DirForward)
            {
              trackLen += ATC::ATCMath::absDiff(trackData.length, locBorder.startOfLocation.position);
            }
            else if (tDir == DirReverse)
            {
              trackLen += locBorder.startOfLocation.position;
            }
            else
            {
              isLocDataValid = false;
              setInvalidationReason("location start track has incorrect travel direction");
            }
          }
          locationStartTrackFound = true;
        }// end of start location check
        else if ((locBorder.endOfLocation.track == trackData.track) && (locationStartTrackFound))
        {
          // Already know that locatio end track and posistion are correct as it is checked before this function call
          TravelDir tDir = getTrackTravelDir(trackData.trvDir, trackData.odoDir);

          locationEndTrackFound = true;

          if (tDir == DirForward)
          {
            trackLen += locBorder.endOfLocation.position;
          }
          else if (tDir == DirReverse)
          {
            trackLen += ATC::ATCMath::absDiff(locBorder.endOfLocation.position, trackData.length);
          }
          else
          {
            isLocDataValid = false;
            setInvalidationReason("location end track has incorrect travel direction");
          }
        }
        else
        {
          // Don't add length if the track was in storage and already added to trackLen by validateLocationBorderInStorage()        
          const bool trackLenNotAdded = (DS::AbstractTracks::corePtr()->getTrack(trackData.track) == static_cast<const DS::Track*>(NULL));

          if (locationStartTrackFound && trackLenNotAdded)
          {
            trackLen += trackData.length;
          }
        }
      } // end of for-loop

      if (isLocDataValid && 
        ((!locationStartTrackFound) || (!locationEndTrackFound)))
      {
        isLocDataValid = false;
        setInvalidationReason("location start or end track is not within TRACK_DATA nor Stored tracks");
      }

      if (isLocDataValid)
      {
        const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

        if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
        {
          if (pTrainSetup->length > trackLen) //is location distance long enough to accommodate the train length?
          {
            isLocDataValid = false;
            trace->write(ATC::detailedTrace, "Location Boundary is insufficient to accommodate the train");
            setInvalidationReason("Location Boundary is insufficient to accommodate the train");
          }
        }
      }// End of start and end position check

      return isLocDataValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::validateLocationBorderInStorage
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::validateLocationBorderInStorage(bool &locationStartTrackFound,
      bool &locationEndTrackFound, uint32_t &trackLen, const LocationBorder &locBorder)
    {
      bool validLocData = true;

      DS::AbstractTracks::ConstTrackListIteratorType storedTracksListItr = DS::AbstractTracks::corePtr()->getTracksIter();

      // Loop through the stored tracks and sum the track lenth from the location start to location end
      for (; (storedTracksListItr != DS::AbstractTracks::corePtr()->getTracksIterEnd()) && validLocData; ++storedTracksListItr)
      {
        const DS::Track *const storedTrackItr = (*storedTracksListItr);

        // check for location start track
        if ((storedTrackItr->getTrackId() == locBorder.startOfLocation.track) && (!locationStartTrackFound))
        {
          locationStartTrackFound = true;
          
          // Use MA direction as this may change from what is previously stored
          const TravelDir tDir = getTrackTravelDir(dirAndOrientation2TravelDir(maHead.trainDirection), storedTrackItr->getOdoDirection());

          if (locBorder.startOfLocation.position > storedTrackItr->getTrackLength()) // Verify the location start position is within the track
          {
            validLocData = false;
            setInvalidationReason("Location start position is outside track #2");
          }
          else if ((locBorder.startOfLocation.track == maHead.endOfMATrackAndPos.track) &&
            (((tDir == DirForward) && (locBorder.startOfLocation.position > maHead.endOfMATrackAndPos.position)) ||
            ((tDir == DirReverse) && (locBorder.startOfLocation.position < maHead.endOfMATrackAndPos.position))))
          {
            // if location start track same as MA end track and location start position ahead of MA end position it is invalid
            validLocData = false;
            setInvalidationReason("Location start position is ahead of MA end #2");
          }
          else if ((locBorder.startOfLocation.track == maHead.endOfMATrackAndPos.track))
          {
            // in this since the location start and end track is the same we must set the found end track
            trackLen = ATC::ATCMath::absDiff(maHead.endOfMATrackAndPos.position, locBorder.startOfLocation.position);
            locationEndTrackFound = true;
          }
          else
          {
            if (tDir == DirForward)
            {
              trackLen += ATC::ATCMath::absDiff(storedTrackItr->getTrackLength(), locBorder.startOfLocation.position);
            }
            else if (tDir == DirReverse)
            {
              trackLen += locBorder.startOfLocation.position;
            }
            else
            {
              validLocData = false;
              setInvalidationReason("Incorrect travel direction when validating location border #1");
              // Should never come here
            }
          }
        }
        else if ((storedTrackItr->getTrackId() == locBorder.endOfLocation.track) && locationStartTrackFound && (!locationEndTrackFound))
        {
          const TravelDir tDir = getTrackTravelDir(dirAndOrientation2TravelDir(maHead.trainDirection), storedTrackItr->getOdoDirection());
          locationEndTrackFound = true;

          if (tDir == DirForward)
          {
            trackLen += locBorder.endOfLocation.position;
          }
          else if (tDir == DirReverse)
          {
            trackLen += ATC::ATCMath::absDiff(locBorder.endOfLocation.position, storedTrackItr->getTrackLength());
          }
          else
          {
            validLocData = false;
            setInvalidationReason("Incorrect travel direction when validating location border #2");
            // should never come here
          }
        }
        else if ((locationStartTrackFound) && (!locationEndTrackFound))
        {
          trackLen += storedTrackItr->getTrackLength();
        }
        else
        {
          if ((locationEndTrackFound) && (!locationStartTrackFound))
          {
            validLocData = false;
            setInvalidationReason("Location start & end validation failed");
          }
        }
      }// End of loop 

      // if data is valid so far lets check validity against possible existing location borders.
      if (validLocData)
      {
        // Get the location start and end targets if any
        const DS::BaseTarget* const locStartTarget = DS::AbstractTargets::corePtr()->getLocationStartTarget();
        const DS::BaseTarget* const locEndTarget = DS::AbstractTargets::corePtr()->getLocationEndTarget();

        // Check if train already has location targets
        if ((static_cast<DS::BaseTarget *>(NULL) != locStartTarget) &&
          (static_cast<DS::BaseTarget *>(NULL) != locEndTarget))
        {
          // if location borders exist from previously check that new location borders are correctly specified
          TravelDir locEndTargetDir = locEndTarget->getDirection();
          TravelDir locStartTargetDir = locStartTarget->getDirection();

          // Location end target should have same direction as MA direction
          if (dirAndOrientation2TravelDir(maHead.trainDirection) != locEndTargetDir)
          {
            validLocData = false;
            setInvalidationReason("Location end target should have same direction as MA direction");
          }

          if ((ATPModeLocation == Kernel::AbstractModeControl::corePtr()->getCurrentMode()) && validLocData)
          {
            const OdoPosition trailingOdoPos = TG::AbstractTIMS::corePtr()->getRearPositionOdo();
            const TravelDir currDrivDir = AbstractModeControl::corePtr()->getCurrentDrivingDirection();

            if (!locationStartTrackFound)
            {
              // in ATP mode Location the start position must be among stored tracks otherwise we reject it.
              validLocData = false;
              setInvalidationReason("Location start is not within stored tracks in Location mode.");
            }
            else
            {
              OdoPosition newLocStartOdoPos = 0;
              if (DS::AbstractTracks::corePtr()->getOdoPos(locBorder.startOfLocation, newLocStartOdoPos))
              {
                if ((currDrivDir != locStartTargetDir)) //driving away from location start
                {
                  if (((DirForward == locStartTargetDir) && (newLocStartOdoPos < trailingOdoPos)) ||
                    ((DirReverse == locStartTargetDir) && (newLocStartOdoPos > trailingOdoPos)))
                  {
                    validLocData = false;
                    setInvalidationReason("Invalid Location start position #1");
                  }
                }
                else //driving towards location start
                {
                  const OdoPosition locStartTargetOdoPos = locStartTarget->getOdometer();
                  if (((DirForward == locStartTargetDir) && (newLocStartOdoPos < locStartTargetOdoPos)) ||
                    ((DirReverse == locStartTargetDir) && (newLocStartOdoPos > locStartTargetOdoPos)))
                  {
                    validLocData = false;
                    setInvalidationReason("Invalid Location start position #2");
                  }
                }
              }
              else
              {
                validLocData = false;
                setInvalidationReason("Invalid Location start position #3");
              }
            }

            if (locationEndTrackFound && validLocData)
            {
              OdoPosition newLocEndOdoPos = 0;
              if (DS::AbstractTracks::corePtr()->getOdoPos(locBorder.endOfLocation, newLocEndOdoPos))
              {
                  if ((currDrivDir != locEndTargetDir)) //driving away from location end
                  {
                    if (((DirForward == locEndTargetDir) && (newLocEndOdoPos < trailingOdoPos)) ||
                      ((DirReverse == locEndTargetDir) && (newLocEndOdoPos > trailingOdoPos)))
                    {
                      validLocData = false;
                      setInvalidationReason("Invalid Location end position #1");
                    }
                  }
                  else //driving towards location start
                  {
                    const OdoPosition locEndTargetOdoPos = locEndTarget->getOdometer();
                    if (((DirForward == locEndTargetDir) && (newLocEndOdoPos < locEndTargetOdoPos)) ||
                      ((DirReverse == locEndTargetDir) && (newLocEndOdoPos > locEndTargetOdoPos)))
                    {
                      validLocData = false;
                      setInvalidationReason("Invalid Location end position #2");
                    }
                  }
              }
              else
              {
                validLocData = false;
                setInvalidationReason("Invalid Location end position #3");
              }
            }
          }
        }
      }
      return validLocData;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::getTrackTravelDir
    ******************************************************************************/
    TravelDir RadioMessageInMovementAuthority::getTrackTravelDir(const TravelDir trainDir,
      const OdoDir trackOdoDir) const
    {
      TravelDir retVal = DirNone;

      if (((trainDir == DirForward) && (trackOdoDir == OdoPositive))
        || ((trainDir == DirReverse) && (trackOdoDir == OdoNegative)))
      {
        retVal = DirForward;
      }
      else if (((trainDir == DirReverse) && (trackOdoDir == OdoPositive))
        || ((trainDir == DirForward) && (trackOdoDir == OdoNegative)))
      {
        retVal = DirReverse;
      }
      else
      {
        // Do Nothing
      }

      return retVal;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::invalidate
    ******************************************************************************/
    void RadioMessageInMovementAuthority::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      if (!isValidPartlyMAInCurrMa)
      {
        // Clear all data-block vectors
        maData.departureWarningReceived = false;
        maData.departureWarning.signalAsDepartureWarning = acousticSignalUndefined;
        maData.partlyMaReceived = false;
        maData.maxSearchDistReceived = false;
        maData.maxSearchDist.maxDistBalise = 0U;
        maData.locationDataReceived = false;
        maData.locationData.locationType = UndefinedLocationType;
        memset(&maData.locationData.locationName[0], 0, sizeof(maData.locationData.locationName));
        maData.atoStopPositionReceived = false;
        maData.atoStopPosition.trainStopTrackAndPosition.track = 0U;
        maData.atoStopPosition.trainStopTrackAndPosition.position = 0U;
        maData.trackDataVec.clear();
        maData.baliseDataVec.clear();
        maData.gradientDataVec.clear();
        maData.ceilingSpeedDataVec.clear();
        maData.trackDataItemVec.clear();
        maData.locationBordersReceived = false;
        maData.locationBorders.allowedSpeed = 0U;
        maData.locationBorders.endOfLocation.position = 0U;
        maData.locationBorders.endOfLocation.track = 0U;
        maData.locationBorders.startOfLocation.position = 0U;
        maData.locationBorders.startOfLocation.track = 0U;
        maData.locationBorders.gradTwrdsLocEnd = 0;
        maData.locationBorders.gradTwrdsLocStart = 0;
        isValidPartlyMAInPrevMa = false;
      }
      dataProcessState = NoDataAvailable;
      maReceived = false;

    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::getPartlyMaReceived
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::getPartlyMaReceived() const
    {
      return (isValidPartlyMAInCurrMa && (dataProcessState == DataValidated));
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::publishLocationTarget
    ******************************************************************************/
    void RadioMessageInMovementAuthority::publishLocationTarget() const
    {
      bool publishLocationValid = false;

      // Temp variable
      LocationData locationData;
      memset(&locationData.locationName[0], 0, sizeof(locationData.locationName));
      locationData.locationType = UndefinedLocationType;
      // Do we have location data?
      if (maData.locationDataReceived)
      {
        locationData = maData.locationData;
        publishLocationValid = true;
      }
      else
      {
        DS::BaseTarget* bTarget = DS::AbstractTargets::corePtr()->getLocationStartTarget();

        if(bTarget != static_cast<DS::BaseTarget*>(NULL))
        {
          // We need to get the location type from one of the targets (as both the target will have same type
          const DS::PrimaryTarget* const locStartTarget =
            ATC::dynamicCast<DS::BaseTarget*, DS::PrimaryTarget*>(bTarget, __FILE__, __LINE__);

          if (locStartTarget->getLocationName(&locationData.locationName[0]))
          {
            publishLocationValid = true;
          }
          locationData.locationType = locStartTarget->getLocationType();
        }
      }

      if (maData.locationBordersReceived)
      {
        if (publishLocationValid)
        {
          DS::BaseTarget* bLocStartTarget = DS::AbstractTargets::corePtr()->getLocationStartTarget();
          DS::BaseTarget* bLocEndTarget = DS::AbstractTargets::corePtr()->getLocationEndTarget();

          if((bLocStartTarget != static_cast<DS::BaseTarget*>(NULL)) && (bLocEndTarget != static_cast<DS::BaseTarget*>(NULL)))
          {
            // First check if we have an old location border...
            const DS::PrimaryTarget* const locStartTarget =
              ATC::dynamicCast<DS::BaseTarget*, DS::PrimaryTarget*>(bLocStartTarget, __FILE__, __LINE__);
            const DS::PrimaryTarget* const locEndTarget =
              ATC::dynamicCast<DS::BaseTarget*, DS::PrimaryTarget*>(bLocEndTarget, __FILE__, __LINE__);

            DS::AbstractTargets::corePtr()->delTargetsInRange(locStartTarget->getOdometer(), locEndTarget->getOdometer());
            // delete the target
            static_cast<void>(DS::AbstractTargets::corePtr()->delTarget(locStartTarget));
            static_cast<void>(DS::AbstractTargets::corePtr()->delTarget(locEndTarget));
          }

          publishLocationValid = publishLocationBorder(maData.locationBorders, locationData);
        }

        if (!publishLocationValid)
        {
          writeToLog(ATC::DetailedLog, "Publishing of location target failed!", __FILE__, __LINE__);
        }
      }
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::publishLocationTarget
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::publishLocationBorder(const LocationBorder& locationBorder, const LocationData& locData) const
    {
      OdoPosition odoPositionAtStart = 0;
      OdoPosition odoPositionAtEnd = 0;

      bool publishLocationValid = true;

      if (DS::AbstractTracks::corePtr()->getOdoPos(locationBorder.startOfLocation, odoPositionAtStart))
      {
        TravelDir startLocationDir = DirUndefined;
        if (DirForward == dirAndOrientation2TravelDir(maHead.trainDirection))
        {
          startLocationDir = DirReverse;
        }
        else if (DirReverse == dirAndOrientation2TravelDir(maHead.trainDirection))
        {
          startLocationDir = DirForward;
        }
        else
        {
          publishLocationValid = false;
        }

        if (publishLocationValid)
        {
          // Create Primary Target for location start
          DS::PrimaryTarget primaryTarget(
            static_cast<uint32_t>(maHead.maID),
            static_cast<uint8_t>(DS::BaseTarget::LocationStartTargetType),
            static_cast<uint32_t>(maHead.maMarginCm),
            maHead.timeout,
            locationBorder.startOfLocation,
            startLocationDir,
            odoPositionAtStart,
            &locData.locationName[0],
            locData.locationType,
            locationBorder.gradTwrdsLocStart);

          DS::AbstractTargets::corePtr()->addTarget(primaryTarget);

          // Save the maximum allowed speed in mode control
          AbstractModeControl::corePtr()->setMaxAllowedSpeedInLoc(locationBorder.allowedSpeed);
          const bool isLocationMode = (ATPModeLocation == AbstractModeControl::corePtr()->getCurrentMode());

          if ((LocationTypeYardMode == locData.locationType) && (!isLocationMode))
          {
            const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

            if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
            {
              OdoPosition odoPositionAtSpeed = 0;
              TrackAndPos speedTrkAndPos = { 0U,0U };
              const TravelDir trvlDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
              //Calculate the Odo position of speed target in location border 
              if (DirForward == trvlDir)
              {
                odoPositionAtSpeed = odoPositionAtStart + static_cast<OdoPosition>(pTrainSetup->length);
              }
              else if (DirReverse == trvlDir)
              {
                odoPositionAtSpeed = odoPositionAtStart - static_cast<OdoPosition>(pTrainSetup->length);
              }
              else
              {
                // do nothing
              }
              speedTrkAndPos = DS::AbstractTracks::corePtr()->calculateTrackAndPos(odoPositionAtSpeed);

              // Add speed target 
              DS::SpeedTarget speedTarget(locationBorder.allowedSpeed, static_cast<uint32_t>(ScrLocation), speedTrkAndPos,
                dirAndOrientation2TravelDir(maHead.trainDirection),
                odoPositionAtSpeed);

              DS::AbstractTargets::corePtr()->addTarget(speedTarget);
            }
          }
        }
      }
      else
      {
        publishLocationValid = false;
      }

      if (DS::AbstractTracks::corePtr()->getOdoPos(locationBorder.endOfLocation, odoPositionAtEnd) &&
        publishLocationValid)
      {
        // Create Primary Target for location end
        DS::PrimaryTarget primaryTarget(
          static_cast<uint32_t>(maHead.maID),
          static_cast<uint8_t>(DS::BaseTarget::LocationEndTargetType),
          static_cast<uint32_t>(maHead.maMarginCm),
          maHead.timeout,
          locationBorder.endOfLocation,
          dirAndOrientation2TravelDir(maHead.trainDirection),
          odoPositionAtEnd,
          &locData.locationName[0],
          locData.locationType,
          locationBorder.gradTwrdsLocEnd);

        DS::AbstractTargets::corePtr()->addTarget(primaryTarget);
      }
      else
      {
        publishLocationValid = false;
      }

      return publishLocationValid;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::getMaxSearchDistInReReg
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::getMaxSearchDistInReReg(uint32_t& maxDist) const
    {
      bool retflag = false;

      if ((DataValidated == dataProcessState) &&
          (RtReRegistration == maHead.routeType) &&
          (maData.maxSearchDistReceived))
      {
        maxDist = maData.maxSearchDist.maxDistBalise;
        retflag = true;
      }
      else
      {
        maxDist = 0U;
      }

      return retflag;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::isMAFromScratch
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::isMAFromScratch(void) const
    {
      bool bMaScratch = false;
      if(maData.trackDataVec.size() > 0U)
      { 
        // The first track must have previous TrackID = 0
        if (maData.trackDataVec.begin()->previousTrack == 0U)
        {
          bMaScratch = true;
        }
      }

      return bMaScratch;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::getDepartureSignal
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::getDepartureSignal(AcousticSignal& acousticSignal) const
    {
      bool retValue = false;

      if ((DataValidated == dataProcessState)  && maData.departureWarningReceived)
      {
        acousticSignal = maData.departureWarning.signalAsDepartureWarning;
        retValue = true;
      }
      else
      {
        acousticSignal = acousticSignalUndefined;
      }

      return retValue;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::parseAdditionalBlocks
    ******************************************************************************/
    bool RadioMessageInMovementAuthority::parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType)
    {
      // Writing the below trace for removing warning
      trace->write(ATC::detailedTrace, "No Adaptation block included in MA", static_cast<uint32_t>(adapBlockType));
      trace->write(ATC::detailedTrace, "Size of Buffer passed", buffer->b_s);
      setInvalidationReason("No Adaptation block included in MA");
      return false;
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::detailedLog
    ******************************************************************************/
    void RadioMessageInMovementAuthority::detailedLog(void) const
    {
      uint8_t currentLevel;
      bool isEnabled;

      trace->getTraceDetails(currentLevel, isEnabled);

      if (isEnabled && (currentLevel >= ATC::detailedMessageTrace))
      {   // No reason to assemble logStr if trace not enabled
        char_t logStr[120];

        //lint -e{586} snprintf is needed here
        int32_t res = snprintf(&logStr[0], sizeof(logStr),
          "NID_MSG=%u, T_VALID=%u, V_SPEED=%u, G_GRADIENT=%d, M_LOADED=%u, N_ADHESION=%u, B_DIRECTION=%X, Q_ROUTE_TYPE=%u",
          static_cast<uint32_t> (maHead.maID),
          static_cast<uint32_t> (maHead.timeout),
          static_cast<uint32_t> (maHead.ceilingSpeed),
          static_cast<int32_t>  (maHead.gradient),
          static_cast<uint32_t> (maHead.trainLoadStatus),
          static_cast<uint32_t> (maHead.adhesionValue),
          static_cast<uint32_t> (maHead.trainDirection),
          static_cast<uint32_t> (maHead.routeType));

        if ((res > 0) && (static_cast<size_t>(res) < sizeof(logStr)))
        {
          traceLog(ATC::detailedMessageTrace, ATC::DetailedLog, &logStr[0]);
        }

        //lint -e{586} snprintf is needed here
        res = snprintf(&logStr[0], sizeof(logStr),
          "EndMA=(%u,%u), StartMa=(%u,%u), MA_MARGIN=%u, D_OVERLAP=%u",
          static_cast<uint32_t> (maHead.endOfMATrackAndPos.track),
          static_cast<uint32_t> (maHead.endOfMATrackAndPos.position),
          static_cast<uint32_t> (maHead.startOfMATrackAndPos.track),
          static_cast<uint32_t> (maHead.startOfMATrackAndPos.position),
          static_cast<uint32_t> (maHead.maMarginCm),
          static_cast<uint32_t> (maHead.overlapValue));

        if ((res > 0) && (static_cast<size_t>(res) < sizeof(logStr)))
        {
          traceLog(ATC::detailedMessageTrace, ATC::DetailedLog, &logStr[0]);
        }
      }
    }

    /******************************************************************************
    * RadioMessageInMovementAuthority::veryDetailedLog
    ******************************************************************************/
    void RadioMessageInMovementAuthority::veryDetailedLog(void) const
    {
      uint8_t currentLevel;
      bool isEnabled;

      trace->getTraceDetails(currentLevel, isEnabled);

      if (isEnabled && (currentLevel >= ATC::veryDetailedMessageTrace))
      {       
        char_t logStr[200];
        int32_t res;

        // DEPARTURE_WARNING
        //lint -e{586} snprintf is needed here
        if (maData.departureWarningReceived)
        {
          res = snprintf(&logStr[0], sizeof(logStr), "DEPARTURE_WARNING: Q_SIGNAL=%u",
            static_cast<uint32_t>(maData.departureWarning.signalAsDepartureWarning));

          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
          }
        }
        else
        {
          res = snprintf(&logStr[0], sizeof(logStr), "DEPARTURE_WARNING:-");

          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
          }
        }      
        
        // PARTLY_MA
        //lint -e{586} snprintf is needed here
        res = snprintf(&logStr[0], sizeof(logStr), "PARTLY_MA:%s", (maData.partlyMaReceived) ? "True" : "-");
        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
        {
          traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
        }

        // MAX_SEARCH_DIST
        if (maData.maxSearchDistReceived)
        {
          //lint -e{586} snprintf is needed here
          res = snprintf(&logStr[0], sizeof(logStr), "MAX_SEARCH_DIST: D_MAX_DIST=%u",
            static_cast<uint32_t>(maData.maxSearchDist.maxDistBalise));

          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
          }
        }
        else
        {
          traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, "MAX_SEARCH_DIST:-");
        }

        // LOCATION_BORDERS
        if (maData.locationBordersReceived)
        {
          //lint -e{586} snprintf is needed here
          res = snprintf(&logStr[0], sizeof(logStr),
            "LOCATION_BORDERS: NID_TRACK=%u, D_POSITION=%u, NID_TRACK=%u, D_POSITION=%u, V_SPEED=%u, G_GRADIENT=%d, G_GRADIENT=%d",
            static_cast<uint32_t>(maData.locationBorders.startOfLocation.track),
            static_cast<uint32_t>(maData.locationBorders.startOfLocation.position),
            static_cast<uint32_t>(maData.locationBorders.endOfLocation.track), 
            static_cast<uint32_t>(maData.locationBorders.endOfLocation.position),
            static_cast<uint32_t>(maData.locationBorders.allowedSpeed),
            static_cast<int32_t>(maData.locationBorders.gradTwrdsLocStart),
            static_cast<int32_t>(maData.locationBorders.gradTwrdsLocEnd));

          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
          }
        }
        else
        {
          //lint -e{586} snprintf is needed here
          res = snprintf(&logStr[0], sizeof(logStr), "LOCATION_BORDERS:-");
          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
          }
        }

        // LOCATION_DATA
        if (maData.locationDataReceived)
        {
          //lint -e{586} snprintf is needed here
          res = snprintf(&logStr[0], sizeof(logStr), "LOCATION_DATA: TID_LOCATION=%s, NID_LOCATION_TYPE=%u",
            maData.locationData.locationName,
            static_cast<uint32_t>(maData.locationData.locationType));

          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
          }
        }
        else
        {
          traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, "LOCATION_DATA:-");
        }

        // ATO_STOP_POSITION
        if (maData.atoStopPositionReceived)
        {
          //lint -e{586} snprintf is needed here
          res = snprintf(&logStr[0], sizeof(logStr), "ATO_STOP_POSITION: NID_TRACK=%u, D_POSITION=%u",
            static_cast<uint32_t>(maData.atoStopPosition.trainStopTrackAndPosition.track),
            static_cast<uint32_t>(maData.atoStopPosition.trainStopTrackAndPosition.position));
          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
          }
        }
        else
        {
          //lint -e{586} snprintf is needed here
          res = snprintf(&logStr[0], sizeof(logStr), "ATO_STOP_POSITION:-");
          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
          }
        }

        // TRACK_DATA
        if (maData.trackDataVec.size() > 0U)
        {
          std::vector<TrackData>::const_iterator trackIt;
          for (trackIt = maData.trackDataVec.begin(); trackIt != maData.trackDataVec.end() ; ++trackIt)
          {
            //lint -e{586} snprintf is needed here
            res = snprintf(&logStr[0], sizeof(logStr), "TRACK_DATA: NID_TRACK=%u, L_TRACK=%u, B_DIRECTION=%X, NID_PREVIOUS_TRACK=%u",
              static_cast<uint32_t>(trackIt->track), 
              static_cast<uint32_t>(trackIt->length),
              static_cast<uint32_t>(trackIt->bdirection),
              static_cast<uint32_t>(trackIt->previousTrack));

            if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
            {
              traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
            }
          }
        }
        else
        {
          traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, "TRACK_DATA:-");
        }

        // BALISE_DATA
        if (maData.baliseDataVec.size() > 0U)
        {
          std::vector<BaliseData>::const_iterator baliseIt;
          for (baliseIt = maData.baliseDataVec.begin(); baliseIt != maData.baliseDataVec.end(); ++baliseIt)
          {
            //lint -e{586} snprintf is needed here
            res = snprintf(&logStr[0], sizeof(logStr), "BALISE_DATA: NID_TRACK=%u, D_POSITION=%u, NID_BG=%u",
              static_cast<uint32_t>(baliseIt->baliseTrackAndPosition.track),
              static_cast<uint32_t>(baliseIt->baliseTrackAndPosition.position),
              static_cast<uint32_t>(baliseIt->baliseId));

            if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
            {
              traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
            }
          }
        }
        else
        {
          traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, "BALISE_DATA:-");
        }

        // GRADIENT_DATA
        if (maData.gradientDataVec.size() > 0U)
        {
          std::vector<GradientData>::const_iterator gradientIt;
          for (gradientIt = maData.gradientDataVec.begin(); gradientIt != maData.gradientDataVec.end(); ++gradientIt)
          {
            //lint -e{586} snprintf is needed here
            res = snprintf(&logStr[0], sizeof(logStr), "GRADIENT_DATA: NID_TRACK=%u, D_POSITION=%u, G_GRADIENT=%d",
              static_cast<uint32_t>(gradientIt->trackAndPosition.track),
              static_cast<uint32_t>(gradientIt->trackAndPosition.position),
              static_cast<int32_t>(gradientIt->gradient));

            if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
            {
              traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
            }
          }
        }
        else
        {
          traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, "GRADIENT_DATA:-");
        }

        // CEILING_SPEED_DATA
        if (maData.ceilingSpeedDataVec.size() > 0U)
        {
          std::vector<CeilingSpeedData>::const_iterator ceilingSpeedIt;
          for (ceilingSpeedIt = maData.ceilingSpeedDataVec.begin(); ceilingSpeedIt != maData.ceilingSpeedDataVec.end(); ++ceilingSpeedIt)
          {
            //lint -e{586} snprintf is needed here
            res = snprintf(&logStr[0], sizeof(logStr), "CEILING_SPEED_DATA: NID_TRACK=%u, D_POSITION=%u, V_SPEED=%u, Q_SPEED=%u",
              static_cast<uint32_t>(ceilingSpeedIt->trackAndPosition.track),
              static_cast<uint32_t>(ceilingSpeedIt->trackAndPosition.position),
              static_cast<uint32_t>(ceilingSpeedIt->ceilingSpeed),
              static_cast<uint32_t>(ceilingSpeedIt->speedChangeReason));

            if ((res > 0) && (static_cast<size_t>(res) < sizeof(logStr)))
            {
              traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
            }
          }
        }
        else
        {
          traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, "CEILING_SPEED_DATA:-");
        }

        // TRACK_DATA_ITEM
        if (maData.trackDataItemVec.size() > 0U)
        {
          std::vector<TrackDataItem>::const_iterator trackDataItemIt;
          for (trackDataItemIt = maData.trackDataItemVec.begin(); trackDataItemIt != maData.trackDataItemVec.end(); ++trackDataItemIt)
          {
            //lint -e{586} snprintf is needed here
            res = snprintf(&logStr[0], sizeof(logStr),
              "TRACK_DATA_ITEM: Q_TRACK_DATA_TYPE=%u, NID_TRACK=%u, D_POSITION=%u, Q_DIRECTION=%u, N_VALUE:%u",
              static_cast<uint32_t>(trackDataItemIt->trackDataType),
              static_cast<uint32_t>(trackDataItemIt->trackAndPosition.track),
              static_cast<uint32_t>(trackDataItemIt->trackAndPosition.position),
              static_cast<uint32_t>(trackDataItemIt->validDir),
              static_cast<uint32_t>(trackDataItemIt->optionalValue));
            if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
            {
              traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
            }
          }
        }
        else
        {
          traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, "TRACK_DATA_ITEM:-");
        }
      }
    }
  }
}
