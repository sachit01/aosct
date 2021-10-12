/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (TCC->AOS) has an associated parser class inherited from AbstractRadioMessageIn.
* This file implements the parser for the ApproximatePosition message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-02-28    akushwah    Created
* 2017-07-17    skothiya    Updated according to requirements
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include <algorithm>

#include "radio_message_in_approximate_position.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_odometry.hpp"
#include "abstract_tracks.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_targets.hpp"
#include "atc_math.hpp"
#include <vfw_checkpoints.h>

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
    * RadioMessageInApproximatePosition constructor
    ******************************************************************************/
    RadioMessageInApproximatePosition::RadioMessageInApproximatePosition() : AbstractRadioMessageIn(MTypeApproximatePosition)
    {
      implemented = true;

      approxPosData.approxPosId = 0U;
      approxPosData.approxTrackAndpos.position = 0U;
      approxPosData.approxTrackAndpos.track = 0U;
      approxPosData.approxTrackDataVec.reserve(approxPosTrackDataSize);
      approxPosReceived = false;
      approxPosData.approxPartlyTrackData = false;
      partialApproxMessageAcceptedEarlier = false;
    }

    /******************************************************************************
    * RadioMessageInApproximatePosition::validate
    ******************************************************************************/
    bool RadioMessageInApproximatePosition::validate()
    {
      trace->write(ATC::briefTrace, "Validating Approximate Position");

      // Flag that Approximate position message is received and process-state is updated
      approxPosReceived = true;

      bool ret = AbstractRadioMessageIn::validate();

      if (ret)
      {
        ret = false;

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
                  writeToLog(ATC::BriefLog, "TRACK_DATA is continuous for Approximate Message", __FILE__, __LINE__);
                }
              }
            }
          }
        }

      }

      return ret;
    }

    /******************************************************************************
    * RadioMessageInApproximatePosition::validateMode
    ******************************************************************************/
    bool RadioMessageInApproximatePosition::validateMode()
    {
      bool modeValid = false;

      const ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
        case ATPModePossession:
        case ATPModeShunting:
        case ATPModeYard:
        case ATPModeStaffResponsible:
        case ATPModeSplit:
        case ATPModePowerUp:
        case ATPModeConfiguration:
        case ATPModeBaliseSearch:
        case ATPModeNormal:
        case ATPModeLocation:
        case ATPModeJoin:
        case ATPModeShuntingRoute:
        case ATPModePoweringDown:
        case ATPModeUnregistered:
        case ATPModeSafetyHalt:
        case ATPModeSleeping:
          invalidIncmgMsgTCC.setDynamicText("Invalid ATP mode");
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
          break;

        case ATPModeRegistration:
        {
          const TrainRegistrationModeState trainRegistrationState = AbstractModeControl::corePtr()->getTrainRegistrationModeState();
          const bool isRePositionSubState = (TrainRegistrationMode::trainRegistrationWaitForApprxMesg == trainRegistrationState) ||
                                            (TrainRegistrationMode::trainRegistrationRePosition == trainRegistrationState);
          const bool isPosUnknown = (Pos::PosUnknown == Pos::AbstractPosition::corePtr()->getAccuracyState());

          if (isRePositionSubState && isPosUnknown)
          {
            modeValid = true;
          }
          else if (!isPosUnknown)
          {
            setInvalidationReason("Position not unknown");
          }
          else
          {
            setInvalidationReason("Position substate not correct");
          }

          break;
        }

        case ATPModeSafeBrakeToStop:
        {
          Pos::PosAccuracyState currentPositionState = Pos::AbstractPosition::corePtr()->getAccuracyState();

          const bool curPosUnKnownOrDoubtful = ((currentPositionState == Pos::PosUnknown) || ((currentPositionState == Pos::PosDoubtfull)));

          if (curPosUnKnownOrDoubtful)
          {
            //Check for Stand Still
            if (!Pos::AbstractOdometry::corePtr()->isTrainStandStill())
            {
              setInvalidationReason("Train is not stand still");
            }
            else
            {
              modeValid = true;
            }
          }
          else
          {
            setInvalidationReason("Position known");
          }

          break;
        }

        case ATPModeUndefined:
        case ATPModesCount:
        default:
          ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
          break;
      }

      traceValidateMode(modeValid);

      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInApproximatePosition::parseAdditionalBlocks
    ******************************************************************************/
    bool RadioMessageInApproximatePosition::parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType)
    {
      // Writing the below trace for removing warning
      trace->write(ATC::detailedTrace, "No Adaptation block included in RadioMessageInApproximatePosition", static_cast<uint32_t>(adapBlockType));
      trace->write(ATC::detailedTrace, "Size of Buffer passed", buffer->b_s);
      return false;
    }

    /******************************************************************************
    * RadioMessageInApproximatePosition::parseMessageData
    ******************************************************************************/
    bool RadioMessageInApproximatePosition::parseMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));
      vfwSetReadBuffer(&buffer, sizeof(messageData.message.data));

      // Read & validate NID_MESSAGE_TYPE
      if (vfwGetU8(&buffer) != static_cast<uint8_t>(messageType))
      {
        setInvalidationReason("NID_MESSAGE_TYPE invalid");
        parseDataValid = false;
      }
      else
      {
        // Read NID_MSG
        approxPosData.approxPosId = vfwGetU8(&buffer);

        // Read NID_TRACK
        const uint16_t nidTrack = vfwGetU16(&buffer);
        if (nidTrack == 0U)
        {
          parseDataValid = false;
          setInvalidationReason("NID_TRACK invalid (0)");
        }

        approxPosData.approxTrackAndpos.track = nidTrack;

        // Read D_POSITION
        approxPosData.approxTrackAndpos.position = vfwGetU32(&buffer);

        // BlockData
        uint8_t nextMsgIdentifier = vfwGetU8(&buffer);

        // Fetch next data-block until M_END_OF_MESSAGE
        while ((nextMsgIdentifier != M_END_OF_MESSAGE) && (parseDataValid))
        {
          switch (nextMsgIdentifier)
          {
          case BTypeTrackData:
          {
            TrackData trackData;
            readTRACK_DATA(buffer, trackData);

            if (!validateB_DIRECTION(trackData.bdirection))
            {
              setInvalidationReason("B_DIRECTION invalid");
              parseDataValid = false;
            }

            if (approxPosData.approxTrackDataVec.size() < approxPosTrackDataSize)
            {
              approxPosData.approxTrackDataVec.push_back(trackData);
            }
            else
            {
              setInvalidationReason("Approximate Position TrackData overflow");
              parseDataValid = false;
            }
            break;
          }
          case BTypePartlyTrackData:
          {
            approxPosData.approxPartlyTrackData = true;
            break;
          }
          default:
            parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
            break;
          } // end switch
          // Fetch next msg-type (or M_END_OF_MESSAGE)
          nextMsgIdentifier = vfwGetU8(&buffer);
        }
      }

      // Set the partialApproxMessageAcceptedEarlier to false, so that it can clear the Approximate Position buffer,
      // if Final Approximate position message is rejected
      if (!approxPosData.approxPartlyTrackData)
      {
        partialApproxMessageAcceptedEarlier = false;
      }

      if ((approxPosData.approxTrackDataVec.size() == 0U) && parseDataValid)
      {
        setInvalidationReason("No tracks included");
        parseDataValid = false;
      }

      if ((!validateSizeOfParsedBytes(&buffer)) && parseDataValid)
      {
        setInvalidationReason("Incorrect number of parsed bytes");
        parseDataValid = false;
      }

      // Log according to trace-level
      detailedLog();
      veryDetailedLog();

      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInApproximatePosition::invalidate
    ******************************************************************************/
    void RadioMessageInApproximatePosition::invalidate()
    {
      AbstractRadioMessageIn::invalidate();

      //Don't clear the track data vector if partial approximate message received
      if (!partialApproxMessageAcceptedEarlier)
      {
        approxPosData.approxTrackDataVec.clear();
      }
      approxPosData.approxPosId = 0U;
      approxPosData.approxTrackAndpos.position = 0U;
      approxPosData.approxTrackAndpos.track = 0U;
      dataProcessState = NoDataAvailable;
      approxPosReceived = false;
      approxPosData.approxPartlyTrackData = false;


    }

    /******************************************************************************
    * RadioMessageInApproximatePosition::getApproximatePosition
    ******************************************************************************/
    bool RadioMessageInApproximatePosition::getApproximatePosition() const
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "MH_getApproximatePosition");
      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageInApproximatePosition::getApproxFrontPos
    ******************************************************************************/
    bool RadioMessageInApproximatePosition::getApproxFrontPos(TrackAndPos& tnp) const
    {
      tnp = approxPosData.approxTrackAndpos;
      return ((DataValidated == dataProcessState) && (!approxPosData.approxPartlyTrackData));
    }

    /******************************************************************************
    * RadioMessageInApproximatePosition::getApproxPosReceived
    ******************************************************************************/
    bool RadioMessageInApproximatePosition::getApproxPosReceived(uint8_t & id, uint16_t &replyChannelId) const
    {
      replyChannelId = messageData.channelId;
      id = approxPosData.approxPosId;

      return approxPosReceived;
    }


    /******************************************************************************
    * RadioMessageInApproximatePosition::publishTracks
    ******************************************************************************/
    bool RadioMessageInApproximatePosition::publishTracks()
    {
      bool publishTracksValid = true;
      //Publish the tracks only if Final Approximate message is received
      if (!approxPosData.approxPartlyTrackData)
      {
        TravelDir supposedTravelDir = DirUndefined;

        DS::AbstractTracks::corePtr()->removeAll();

        // Add data to Tracks, abort if something went wrong
        for (std::vector<TrackData>::iterator it = approxPosData.approxTrackDataVec.begin();
          (it != approxPosData.approxTrackDataVec.end()) && publishTracksValid; ++it)
        {
          DS::Track track(
            it->track,
            it->trvDir,
            it->previousTrack,
            it->length,
            it->odoDir);

          publishTracksValid = DS::AbstractTracks::corePtr()->addTrack(track);

          supposedTravelDir = it->trvDir;
        }

        if (publishTracksValid)
        {
          if (supposedTravelDir != DirUndefined)
          {
            DS::AbstractTargets::corePtr()->setSupposedTravelDir(supposedTravelDir);
          }
        }
        else
        {
          setInvalidationReason("Error adding a track!");
        }
      }
      else
      {
        partialApproxMessageAcceptedEarlier = true;
      }

      return publishTracksValid;
    }

    /******************************************************************************
    * RadioMessageInApproximatePosition::validateApproxPosMessage
    ******************************************************************************/
    bool RadioMessageInApproximatePosition::validateApproxPosMessage()
    {
      bool dataValid = true;
      std::vector<TrackData>::iterator trackIt;
      uint16_t lastTrackID = 0U;
      TravelDir supposedTravelDir = DirUndefined;
      bool isTrainStartFound = false;
      uint32_t trackLenTillFrontPos = 0U;
      uint32_t approxPos = approxPosData.approxTrackAndpos.position;

      // Remove pure duplicates that are located next to each other
      static_cast<void>(unique(approxPosData.approxTrackDataVec.begin(), approxPosData.approxTrackDataVec.end()));
      for (trackIt = approxPosData.approxTrackDataVec.begin(); (trackIt != approxPosData.approxTrackDataVec.end()) && dataValid; ++trackIt)
      {
        // Each track ID shall only exist once
        if (count_if(approxPosData.approxTrackDataVec.begin(), approxPosData.approxTrackDataVec.end(), TrackDataTrackIdComp(trackIt->track)) != 1)
        {
          dataValid = false;
          setInvalidationReason("Multiple tracks with same ID");
        }

        //check the list is continuous.. i.e. previous track id of the track is correct.
        if (trackIt != approxPosData.approxTrackDataVec.begin())
        {
          if (trackIt->previousTrack != lastTrackID)
          {
            dataValid = false;
            setInvalidationReason("Added tracks are not continuous");
          }

          // Each track should have the same travel direction.. not necessarily the orientation
          if ((trackIt)->trvDir != supposedTravelDir)
          {
            dataValid = false;
            setInvalidationReason("Added tracks have different direction");
          }
        }
        else
        {
          //setting the direction of first track to check it with further tracks direction
          supposedTravelDir = trackIt->trvDir;
          if ((trackIt->previousTrack != 0U) && (!partialApproxMessageAcceptedEarlier))
          {
            dataValid = false;
            setInvalidationReason("First tracks do not have previous track 0");
          }

        }

        lastTrackID = trackIt->track;

        // Check for new train footprint for final Approximate Position message
        if (!approxPosData.approxPartlyTrackData)
        {
          // Checking if train footprints are within the tracks received
          // Calculating the length of tracks till train front position received in Approx Position
          // Also assuming that tracks information received in Approx Position message is from the rear end.

          // If current track is the track where front end of train exist
          if ((lastTrackID == approxPosData.approxTrackAndpos.track) && (!isTrainStartFound))
          {
            isTrainStartFound = true;
            uint32_t trackLen = 0U;

            if (DirForward == trackIt->trvDir)
            {
              if (OdoPositive == trackIt->odoDir)
              {
                // Moving in Forward direction and Locomotive is close to leg 1
                trackLen = approxPos;
              }
              else
              {
                // Moving in Forward direction and locomotive is close to leg 0
                trackLen = ATC::ATCMath::absDiff(approxPos, trackIt->length);
              }
            }
            else if (DirReverse == trackIt->trvDir)
            {
              if (OdoPositive == trackIt->odoDir)
              {
                // Moving in reverse direction and loco is close to leg 1
                trackLen = ATC::ATCMath::absDiff(approxPos, trackIt->length);
              }
              else
              {
                // Moving in reverse direction and loco is close to leg 0
                trackLen = approxPos;
              }
            }
            else
            {
              // Should never enter here
            }

            trackLenTillFrontPos += trackLen;
          }
          else if (!isTrainStartFound)
          {
            uint32_t trackLen = trackIt->length;
            trackLenTillFrontPos += trackLen;
          }
          else
          {
            // Do nothing only used to remove lint error
          }
        }
      } // End of for loop

      const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

      if ((pTrainSetup != static_cast<const DS::TrainSetup*>(NULL)) && (!approxPosData.approxPartlyTrackData))
      {
        uint32_t trainLen = pTrainSetup->length;
        //if Track behind the Approximate Position is less then the train length then train is not within the
        if ((!isTrainStartFound) || (trackLenTillFrontPos < trainLen))
        {
          dataValid = false;
          setInvalidationReason("Train FootPrints are not within the tracks");
        }
      }

      return dataValid;
    }

    /******************************************************************************
    * RadioMessageInApproximatePosition::detailedLog
    ******************************************************************************/
    void RadioMessageInApproximatePosition::detailedLog(void) const
    {
      uint8_t currentLevel;
      bool isEnabled;

      trace->getTraceDetails(currentLevel, isEnabled);

      if (isEnabled && (currentLevel >= ATC::detailedMessageTrace))
      {   // No reason to assemble logStr if trace not enabled
        char_t logStr[120];

        //lint -e{586} snprintf is needed here
        const int32_t res = snprintf(&logStr[0], sizeof(logStr),
          "NID_MSG=%u, NID_TRACK=%u, D_POSITION=%u",
          static_cast<uint32_t> (approxPosData.approxPosId),
          static_cast<uint32_t> (approxPosData.approxTrackAndpos.track),
          static_cast<uint32_t> (approxPosData.approxTrackAndpos.position));

        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
        {
          traceLog(ATC::detailedMessageTrace, ATC::DetailedLog, &logStr[0]);
        }
      }
    }

    /******************************************************************************
    * RadioMessageInApproximatePosition::veryDetailedLog
    ******************************************************************************/
    void RadioMessageInApproximatePosition::veryDetailedLog(void) const
    {
      uint8_t currentLevel;
      bool isEnabled;

      trace->getTraceDetails(currentLevel, isEnabled);

      if (isEnabled && (currentLevel >= ATC::veryDetailedMessageTrace))
      {
        char_t logStr[120];

        //lint -e{586} snprintf is needed here
        int32_t res = snprintf(&logStr[0], sizeof(logStr), "PARTLY_TRACK_DATA: %s", approxPosData.approxPartlyTrackData ? "True" : "-");

        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
        {
          traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
        }

        // TRACK_DATA
        if (approxPosData.approxTrackDataVec.size() > 0U)
        {
          std::vector<TrackData>::const_iterator trackIt;
          for (trackIt = approxPosData.approxTrackDataVec.begin(); trackIt != approxPosData.approxTrackDataVec.end(); ++trackIt)
          {
            //lint -e{586} snprintf is needed here
            res = snprintf(&logStr[0], sizeof(logStr),
              "TRACK_DATA: NID_TRACK=%u, L_TRACK=%u, B_DIRECTION=%X, NID_PREVIOUS_TRACK=%u",
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
      }
    }
  }
}
