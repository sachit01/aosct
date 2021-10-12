/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (TCC->AOS) has an associated parser class inherited from AbstractRadioMessageIn.
* This file implements the parser for the unconditional Shortening message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-08-30    spandita    Created

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "radio_message_in_unconditional_shortening.hpp"
#include "abstract_message_handler_event_ids.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_tracks.hpp"
#include "abstract_targets.hpp"
#include "abstract_tims.hpp"
#include "brake_calculations.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_mode_control.hpp"
#include "dmi_event_codes.hpp"
#include "atc_math.hpp"
#include "dmi_event_codes.hpp"

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
    * RadioMessageInUnconditionalShortening constructor
    ******************************************************************************/
    RadioMessageInUnconditionalShortening::RadioMessageInUnconditionalShortening() :
      AbstractRadioMessageIn(MTypeUnconditionalShortening),
      unCondDiscardedSafeBrakeToStop(ATC::Event::createSafeBrakeSBEvent(atpMessageHandlerId, ATC::CoreContainer,
        eventIdDiscardUnCondSafeBrakeToStopInBaliseSearch, ATC::NoSB, DMICom::uncondDiscardedSafeBrakeToStopInBaliseSearch,
        "Discarded Unconditional Shortening, received in balise search"))
    {
      implemented = true;
      uncondShortMsg.allowedMargin = 0U;
      uncondShortMsg.endTrack.position = 0U;
      uncondShortMsg.endTrack.track = 0U;
      activeEmergencyAlert = false;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool RadioMessageInUnconditionalShortening::validate()
    {
      trace->write(ATC::briefTrace, "Validating unconditional Shortening");

      bool ret = AbstractRadioMessageIn::validate();

      if (ret)
      {
        ret = false;

        if (DataAvailable == dataProcessState)
        {
          if (parseMessageData())
          {
            if (validateMode())
            {
              if (validateTargetInStorage())
              {
                if (validatePositionInStorage())
                {
                  if (publishData())
                  {
                    dataProcessState = DataValidated;
                    ret = true;
                  }
                }
              }
            }
          }
        }
      }

      // if discarded
      if (!ret)
      {
        const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

        if (currentMode == ATPModeBaliseSearch)
        {
          // The AOS shall raise a SafeBrakeToStop event when an UnconditionalShortening message is received in ATP mode Balise Search.
          ATC::AbstractEventHandler::corePtr()->reportEvent(unCondDiscardedSafeBrakeToStop,
            __FILE__, __LINE__);
        }
        else
        {
          // Set the emergency alert, this will raise a Brake event, AND Delete all targets at standstill.
          activeEmergencyAlert = true;
          //Reject the incoming message from TCC 
          invalidIncmgMsgTCC.setDynamicText("Uncond Shortening");
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
        }
      }
      return ret;
    }

    /******************************************************************************
    * parseMessageData
    ******************************************************************************/
    bool RadioMessageInUnconditionalShortening::parseMessageData()
    {
      bool parseDataValid = true;
      //VFW buffer
      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));
      vfwSetReadBuffer(&buffer, sizeof(messageData.message.data));

      // Validate NID_MESSAGE_TYPE
      if (vfwGetU8(&buffer) != static_cast<uint8_t>(messageType))
      {
        trace->write(ATC::detailedTrace, "NID_MESSAGE_TYPE invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("NID_MESSAGE_TYPE");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

      // Fetch NID_TRACK
      const uint16_t nidTrack = vfwGetU16(&buffer);
      if (nidTrack == 0U)
      {
        parseDataValid = false;
        setInvalidationReason("NID_TRACK invalid (0)");
        invalidDataInTCCMessage.setDynamicText("NID_TRACK = 0");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

      uncondShortMsg.endTrack.track = nidTrack;

      // Fetch D_POSITION
      uncondShortMsg.endTrack.position = vfwGetU32(&buffer);

      // Fetch D_MA_MARGIN
      uncondShortMsg.allowedMargin = vfwGetU16(&buffer);
      if (uncondShortMsg.allowedMargin == 0U)
      {
        setInvalidationReason("D_MA_MARGIN (0)");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("D_MA_MARGIN = 0");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

      // Find M_END_Of_MESSAGE or read adaptation blocks
      uint8_t nextMsgIdentifier = vfwGetU8(&buffer);
      while ((nextMsgIdentifier != M_END_OF_MESSAGE) && (parseDataValid))
      {
        parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
        nextMsgIdentifier = vfwGetU8(&buffer);
      }

      if ((!validateSizeOfParsedBytes(&buffer)) && (parseDataValid))
      {
        invalidDataInTCCMessage.setDynamicText("Msg size incorrect");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
        parseDataValid = false;
      }

      return parseDataValid;
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void RadioMessageInUnconditionalShortening::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      dataProcessState = NoDataAvailable;
      uncondShortMsg.allowedMargin = 0U;
      uncondShortMsg.endTrack.position = 0U;
      uncondShortMsg.endTrack.track = 0U;
      activeEmergencyAlert = false;
    }

    /******************************************************************************
    * validatePosInStorage
    ******************************************************************************/
    bool RadioMessageInUnconditionalShortening::validateTargetInStorage() const
    {
      bool isValidData = false;
      //End position in stored tracks?
      if (DS::AbstractTracks::corePtr()->checkTrackAndPos(uncondShortMsg.endTrack))
      {
        DS::BaseTarget *pTarget = DS::AbstractTargets::corePtr()->getPrimaryTarget();

        if (pTarget != static_cast<DS::BaseTarget*>(NULL))
        {
          //End position is ahead of primary target
          if (uncondShortMsg.endTrack.track == pTarget->getPosition().track)
          {
            const TravelDir trvlDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();

            if (DirForward == trvlDir)
            {
              if (pTarget->getPosition().position > uncondShortMsg.endTrack.position)
              {
                isValidData = true;
              }
            }
            else if (DirReverse == trvlDir)
            {
              if (pTarget->getPosition().position < uncondShortMsg.endTrack.position)
              {
                isValidData = true;
              }
            }
            else
            {
              trace->write(ATC::detailedTrace, "invalid direction for validation targets");
            }

          }
          else
          {
            isValidData = handleTargetsInStorage(pTarget->getPosition().track);
          }
        }
      }

      return isValidData;
    }

    /******************************************************************************
    * handleTargetsInStorage
    ******************************************************************************/
    bool RadioMessageInUnconditionalShortening::handleTargetsInStorage(const uint16_t trackId) const
    {
      bool validTarget = false;
      bool isPrimaryTargetFound = false;

      // Start Iterator
      DS::AbstractTracks::ConstTrackListIteratorType trackListItr = DS::AbstractTracks::corePtr()->getTracksIter();
      // End Iterator
      DS::AbstractTracks::ConstTrackListIteratorType trackListItrEnd = DS::AbstractTracks::corePtr()->getTracksIterEnd();


      while ((trackListItr != trackListItrEnd) &&
        (!validTarget) && (!isPrimaryTargetFound))
      {
        if (uncondShortMsg.endTrack.track == (*trackListItr)->getTrackId())
        {
          validTarget = true;
        }
        else if (trackId == (*trackListItr)->getTrackId())
        {
          isPrimaryTargetFound = true;
        }
        else
        {
          //do nothing
        }
        ++trackListItr;
      }

      return validTarget;

    }

    /******************************************************************************
    * validatePositionInStorage
    ******************************************************************************/
    bool RadioMessageInUnconditionalShortening::validatePositionInStorage() const
    {
      bool isValidPos = false;
      //Get the current speed
      const uint32_t currentSpeed = Pos::AbstractOdometry::corePtr()->getSpeed();

      //Get the current gradient
      const int32_t currentGrad = DS::AbstractTargets::corePtr()->getCurGradient();
      //Calculate delay
      const uint32_t delay = Supv::BrakeCalculations::instance().getSBCurveDelay();
      //In this case Target speed will be zero
      int32_t distanceToStandstill = static_cast<int32_t>(Supv::BrakeCalculations::instance().calcAccDeaccDistance(currentSpeed, 0U, delay, currentGrad));
      //Get th travel direction
      const TravelDir trvlDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();

      OdoPosition odoVal = 0;

      if (DS::AbstractTracks::corePtr()->getOdoPos(uncondShortMsg.endTrack, odoVal))
      {
        int32_t odoValOfDistToStandStill = 0;

        if (DirForward == trvlDir)
        {
          //Calculate the odo value from the calculated distance
          odoValOfDistToStandStill = distanceToStandstill + Pos::AbstractPosition::corePtr()->getLeadingPosOdo();

          if (odoValOfDistToStandStill < odoVal)
          {
            isValidPos = true;
          }
        }
        else if (DirReverse == trvlDir)
        {
          odoValOfDistToStandStill = Pos::AbstractPosition::corePtr()->getLeadingPosOdo() - distanceToStandstill;

          if (odoValOfDistToStandStill > odoVal)
          {
            isValidPos = true;
          }
        }
        else
        {
          //do nothing
          trace->write(ATC::detailedTrace, "invalid direction for validation of position");
        }
      }
      return isValidPos;
    }

    /******************************************************************************
    * validateMode
    ******************************************************************************/
    bool RadioMessageInUnconditionalShortening::validateMode() const
    {
      const bool validMode = AbstractModeControl::corePtr()->isValidUncondShorteningMsg();
      traceValidateMode(validMode);
      return validMode;
    }

    /******************************************************************************
    * publishData
    ******************************************************************************/
    bool RadioMessageInUnconditionalShortening::publishData() const
    {
      bool publishDone = true;

      // Get the location start target if any
      const DS::BaseTarget* const locStartTarget = DS::AbstractTargets::corePtr()->getLocationStartTarget();
      // Check if there is any location start targets
      if (static_cast<DS::BaseTarget *>(NULL) != locStartTarget)
      {
        publishDone = DS::AbstractTargets::corePtr()->delTarget(locStartTarget);
      }

      // location end target if any
      const DS::BaseTarget* const locEndTarget = DS::AbstractTargets::corePtr()->getLocationEndTarget();
      if (static_cast<DS::BaseTarget *>(NULL) != locEndTarget)
      {
        publishDone = DS::AbstractTargets::corePtr()->delTarget(locEndTarget);
      }

      DS::BaseTarget *pTarget = DS::AbstractTargets::corePtr()->getPrimaryTarget();

      if (pTarget != static_cast<DS::BaseTarget*>(NULL))
      {
        if (publishDone)
        {
          OdoPosition shortenedMaEndPos = 0;
          if (DS::AbstractTracks::corePtr()->getOdoPos(uncondShortMsg.endTrack, shortenedMaEndPos))
          {
            //Ma timeout
            uint8_t storedTimeOut = pTarget->getMATimeout();
            //MA direction
            TravelDir storedTrvlDir = pTarget->getDirection();
            //Q route type
            uint8_t routeType = pTarget->getRouteType();

            DS::AbstractTargets::corePtr()->delTargetsInRange(shortenedMaEndPos, pTarget->getOdometer());
            //Delete the target
            publishDone = DS::AbstractTargets::corePtr()->delTarget(pTarget);

            DS::PrimaryTarget primaryTarget(
              static_cast<uint32_t>(0U),
              routeType,
              static_cast<uint32_t>(uncondShortMsg.allowedMargin),
              storedTimeOut,
              uncondShortMsg.endTrack,
              storedTrvlDir,
              shortenedMaEndPos);

            if (publishDone)
            {
              DS::AbstractTargets::corePtr()->addTarget(primaryTarget);

              if (TG::AbstractTIMS::corePtr()->isRearPositionValid())
              {
                const OdoPosition safeTrailingPos = TG::AbstractTIMS::corePtr()->getSafePositionToDeleteTrack();

                if (!DS::AbstractTracks::corePtr()->removeNotCovered(safeTrailingPos, shortenedMaEndPos))
                {
                  // No tracks to delete
                }
              }
            }
          }
        }
      }
      return publishDone;
    }

    /******************************************************************************
    * getActiveEmergencyAlert
    ******************************************************************************/
    bool RadioMessageInUnconditionalShortening::getActiveEmergencyAlert() const
    {
      return activeEmergencyAlert;
    }
  }
}
