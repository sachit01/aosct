/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->LCS) has an associated creator class inherited from AbstractLCSMessageOut.
* This file implements the creator for the Status message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-24    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_types.h>
#include <vfw_buffer.h>

#include "atp_types.hpp"
#include "atc_math.hpp"
#include "abstract_atp_application.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_position.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_loco_io.hpp"
#include "abstract_tracks.hpp"
#include "abstract_odometry.hpp"
#include "abstract_tims.hpp"
#include "lcs_message_out_status.hpp"
#include "lcs_message_common.hpp"
#include "atc_util.hpp"

#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#endif

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
  namespace TG
  {
    /******************************************************************************
    * LCSMessageOutStatus constructor
    ******************************************************************************/
    LCSMessageOutStatus::LCSMessageOutStatus() : AbstractLCSMessageOut(LCSMTypeStatusMessage, 1U, true),
      sendStatusMessageCounter(0U)
    {
    }

    /******************************************************************************
    * LCSMessageOutStatus::collectData
    ******************************************************************************/
    void LCSMessageOutStatus::collectData()
    {
      if (sendStatusMessageCounter > 0U)
      {
        --sendStatusMessageCounter;
      }

      // Check if it is time to send status-data
      if (sendStatusMessageCounter == 0U)
      {
        sendStatusMessageCounter = timeoutSendStatusMessage / ATP::Kernel::AbstractATPApplication::atpAppCycleTime;

        // Get Cabin Selector
        getCabinSelector(aosStatus.atoModeCabinSelectorStatus);

        // Get current mode to determine Idle status
        ATPMode currMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

        switch (currMode)
        {
          // When movement is controlled by MA's, fetch idle-status from Mode Control
          // No MA's to process before Normal Mode.
        case ATPModeConfiguration:
        case ATPModeRegistration:
        case ATPModeBaliseSearch:
          aosStatus.trainIdling = TrainIdlingNotAsserted;
          break;

        case ATPModeNormal:
        case ATPModeLocation:
        case ATPModeStaffResponsible:
        case ATPModeShuntingRoute:
        case ATPModeSplit:
        case ATPModeJoin:
          aosStatus.trainIdling = Kernel::AbstractModeControl::corePtr()->getIdleState() ? TrainIdlingTrainIsIdling : TrainIdlingMaExists;
          break;

          // Idle status set to 'TrainIdlingNotAsserted' where movement is not controlled by MA's.
        case ATPModePowerUp:
        case ATPModeUnregistered:
        case ATPModeShunting:
        case ATPModeYard:
        case ATPModePoweringDown:
        case ATPModeSafetyHalt:
        case ATPModeSleeping:
        case ATPModePossession:
        case ATPModeSafeBrakeToStop:
          aosStatus.trainIdling = TrainIdlingNotAsserted;
          break;
        case ATPModesCount:
        case ATPModeUndefined:
        default:
          // Unknown mode
          aosStatus.trainIdling = TrainIdlingNotAsserted;
          ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        }

        const Pos::PosAccuracyState currentAccuracyState = Pos::AbstractPosition::corePtr()->getAccuracyState();

        if (Pos::PosUnknown == currentAccuracyState)
        {
          // Set front and rear position to clear (D_POSITION=0 AND NID_TRACK=0) if the current position is UnKnown
          aosStatus.trackIdRearOfTrain = 0U;
          aosStatus.positionOnTrackRearOfTrain = 0U;
          aosStatus.trackIdFrontOfTrain = 0U;
          aosStatus.positionOnTrackFrontOfTrain = 0U;
        }
        else
        {
          const TrackAndPos trackAndPosFront = DS::AbstractTracks::corePtr()->calculateTrackAndPos(
            Pos::AbstractPosition::corePtr()->getActualLeadingPosOdo());
          aosStatus.trackIdFrontOfTrain = trackAndPosFront.track;
          aosStatus.positionOnTrackFrontOfTrain = trackAndPosFront.position;

          const TrackAndPos trackAndPosRear = DS::AbstractTracks::corePtr()->calculateTrackAndPos(
            Pos::AbstractPosition::corePtr()->getActualTrailingPosOdo());
          aosStatus.trackIdRearOfTrain = trackAndPosRear.track;
          aosStatus.positionOnTrackRearOfTrain = trackAndPosRear.position;
        }

        if (Kernel::AbstractModeControl::corePtr()->getBaliseSearchModeState() <= Kernel::BaliseSearchMode::baliseSearchWaitMA)
        {
          OdoDir odoDir = Kernel::AbstractModeControl::corePtr()->getOdoDirInNewRegistration();

          if (OdoUndefined != odoDir)
          {
            // Orientation in track is '1' if Odometer direction is set to negative.
            if (OdoNegative == odoDir)
            {
              aosStatus.trainOrientationFrontTrack = TrainOrientationFrontTrackLeadTowardsLeg1;
            }
            else
            {
              aosStatus.trainOrientationFrontTrack = TrainOrientationFrontTrackLeadTowardsLeg0;
            }
          }
          else
          {
            // Unknown orientation, use default value.
            getTracer().write(ATC::briefTrace, "Unknown orientation in track");
          }
        }
        else
        {
          const DS::Track* const pFoundTrack = DS::AbstractTracks::corePtr()->getTrack(aosStatus.trackIdFrontOfTrain);

          if (static_cast<const DS::Track*>(NULL) != pFoundTrack)
          {
            if (OdoUndefined != pFoundTrack->getOdoDirection())
            {
              if (OdoPositive == pFoundTrack->getOdoDirection())
              {
                aosStatus.trainOrientationFrontTrack = TrainOrientationFrontTrackLeadTowardsLeg1;
              }
              else
              {
                aosStatus.trainOrientationFrontTrack = TrainOrientationFrontTrackLeadTowardsLeg0;
              }
            }
          }

          const DS::Track* const pFoundRearTrack = DS::AbstractTracks::corePtr()->getTrack(aosStatus.trackIdRearOfTrain);

          if (static_cast<const DS::Track*>(NULL) != pFoundRearTrack)
          {
            if (OdoUndefined != pFoundRearTrack->getOdoDirection())
            {
              if (OdoPositive == pFoundRearTrack->getOdoDirection())
              {
                aosStatus.trainOrientationRearTrack = TrainOrientationRearTrackLeadTowardsLeg1;
              }
              else
              {
                aosStatus.trainOrientationRearTrack = TrainOrientationRearTrackLeadTowardsLeg0;
              }
            }
          }
        }

        // Travel Direction
        TravelDir travelDir = Kernel::AbstractModeControl::corePtr()->getCurrentDrivingDirection();

        aosStatus.travelDirection = (DirReverse == travelDir) ? TravelDirectionLocoTrailing : TravelDirectionLocoLeading;

        // Vehicle Speed, Convert unit from cm/sec to 0.1km/h
        aosStatus.vehicleSpeed = static_cast<uint16_t>(ATC::ATCMath::convCmpsTo100mph(Pos::AbstractOdometry::corePtr()->getSpeed()));

        // TODO: Get Blue-flag status
        aosStatus.blueFlagStatus = BlueFlagInactive;

        // Fetch system time 
        ATC::getUTCTime(aosStatus.messageTime);

        const ATPMode mode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

        // Limited Supervised Mode (Yard/Shunting/Possession and not monitoring train position)
        if ((ATPModeShunting == mode) || (ATPModeYard == mode) || (ATPModePossession == mode))
        {
          aosStatus.limitedSupervisedMode = LimitedSupervisedModeActive;
        }
        else
        {
          aosStatus.limitedSupervisedMode = LimitedSupervisedModeNotActive;
        }

        // Status Message Data shall always be available (and sent to LCS)
        setDataProcessState(DataAvailable);

        uint8_t lev;
        bool enabled;
        getTracer().getTraceDetails(lev, enabled);
        if (lev == 5U)
        {
          getTracer().write(5U, "AOSStatus:atoModeCabinSelectorStatus:", static_cast<uint32_t>(aosStatus.atoModeCabinSelectorStatus));
          getTracer().write(5U, "AOSStatus:trainIdling:", static_cast<uint32_t>(aosStatus.trainIdling));
          getTracer().write(5U, "AOSStatus:trackIdFrontOfTrain:", static_cast<uint32_t>(aosStatus.trackIdFrontOfTrain));
          getTracer().write(5U, "AOSStatus:positionOnTrackFrontOfTrain:", aosStatus.positionOnTrackFrontOfTrain);
          getTracer().write(5U, "AOSStatus:trainOrientationFrontTrack:", static_cast<uint32_t>(aosStatus.trainOrientationFrontTrack));
          getTracer().write(5U, "AOSStatus:trackIdRearOfTrain:", static_cast<uint32_t>(aosStatus.trackIdRearOfTrain));
          getTracer().write(5U, "AOSStatus:positionOnTrackRearOfTrain:", aosStatus.positionOnTrackRearOfTrain);
          getTracer().write(5U, "AOSStatus:trainOrientationRearTrack:", static_cast<uint32_t>(aosStatus.trainOrientationRearTrack));
          getTracer().write(5U, "AOSStatus:travelDirection:", static_cast<uint32_t>(aosStatus.travelDirection));
          getTracer().write(5U, "AOSStatus:Vehicle Speed:", static_cast<uint32_t>(aosStatus.vehicleSpeed));
          getTracer().write(5U, "AOSStatus:blueFlagStatus:", static_cast<uint32_t>(aosStatus.blueFlagStatus));
          getTracer().write(5U, "AOSStatus:messageTime:", aosStatus.messageTime);
          getTracer().write(5U, "AOSStatus:limitedSupervisedMode:", static_cast<uint32_t>(aosStatus.limitedSupervisedMode));
          getTracer().setTraceDetails(0U, enabled);
        }
      }
    }

    /******************************************************************************
    * LCSMessageOutStatus::validate
    ******************************************************************************/
    bool LCSMessageOutStatus::validate(EmpMsg* const mData, uint16_t& length)
    {
      // Parse, validate and publish data
      if (getDataProcessState() == DataAvailable)
      {
        getTracer().write(ATC::veryDetailedTrace, "Validating Status");

        if (assembleMessageData(mData, length))
        {
          setDataProcessState(DataValidated);
        }
      }

      return (getDataProcessState() == DataValidated);
    }

    /******************************************************************************
    * LCSMessageOutStatus::assembleMessageData
    ******************************************************************************/
    bool LCSMessageOutStatus::assembleMessageData(EmpMsg* const messageData, uint16_t& appDataLength) const
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, messageData->getEMPBodyBuffer(), messageData->getEMPMessageMaxBodyLen());

      vfwPutU8(&buffer, static_cast<uint8_t>(aosStatus.atoModeCabinSelectorStatus));
      vfwPutU8(&buffer, static_cast<uint8_t>(aosStatus.trainIdling));

      vfwPutU16(&buffer, aosStatus.trackIdFrontOfTrain);
      vfwPutU32(&buffer, aosStatus.positionOnTrackFrontOfTrain);
      vfwPutU8(&buffer, static_cast<uint8_t>(aosStatus.trainOrientationFrontTrack));

      vfwPutU16(&buffer, aosStatus.trackIdRearOfTrain);
      vfwPutU32(&buffer, aosStatus.positionOnTrackRearOfTrain);
      vfwPutU8(&buffer, static_cast<uint8_t>(aosStatus.trainOrientationRearTrack));

      vfwPutU8(&buffer, static_cast<uint8_t>(aosStatus.travelDirection));
      vfwPutU16(&buffer, aosStatus.vehicleSpeed);
      vfwPutU8(&buffer, static_cast<uint8_t>(aosStatus.blueFlagStatus));

      vfwPutU32(&buffer, aosStatus.messageTime);
      vfwPutU8(&buffer, static_cast<uint8_t>(aosStatus.limitedSupervisedMode));

      traceAssembleData(parseDataValid);

      // Total length of Application-message
      appDataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));

      return parseDataValid;
    }

    /******************************************************************************
    *  LCSMessageOutStatus::invalidate
    ******************************************************************************/
    void LCSMessageOutStatus::invalidate()
    {
      setDataProcessState(NoDataAvailable);
    }

    /******************************************************************************
    * LCSMessageOutStatus::logToRU
    ******************************************************************************/
    void LCSMessageOutStatus::logToRU(const EmpMsg* const mData) const
    {
      static int64_t lastLogTime = 0L; // seconds
      static LCSAOSStatusType lastAosStatus;

      int64_t timeNow = vfwGetReferenceTime() / 1000; // millis to seconds
      int64_t logPeriod = static_cast<int64_t>(ATC::AbstractConfigBase::basePtr()->getRuLogDynValuePeriod()); // seconds

      if (((timeNow - lastLogTime) >= logPeriod) ||
        (aosStatus.atoModeCabinSelectorStatus != lastAosStatus.atoModeCabinSelectorStatus) ||
        (aosStatus.trainIdling != lastAosStatus.trainIdling) ||
        (aosStatus.trainOrientationFrontTrack != lastAosStatus.trainOrientationFrontTrack) ||
        (aosStatus.trainOrientationRearTrack != lastAosStatus.trainOrientationRearTrack) ||
        (aosStatus.travelDirection != lastAosStatus.travelDirection) ||
        (aosStatus.blueFlagStatus != lastAosStatus.blueFlagStatus) ||
        (aosStatus.limitedSupervisedMode != lastAosStatus.limitedSupervisedMode))
      {
        lastLogTime = timeNow;
        lastAosStatus = aosStatus;

        AbstractLCSMessageOut::logToRU(mData);
      }
    }

    /******************************************************************************
    * LCSMessageOutStatus::getCabinSelector
    ******************************************************************************/
    void LCSMessageOutStatus::getCabinSelector(ATOModeCabinSelectorType &selectorType) const
    {
      // ATO Mode Cabin Selector
      switch (IO::AbstractLocoIO::corePtr()->getATOModeSwitchPosition())
      {
      case ATOModeManualPos:
      {
        selectorType = ATOModeCabinManual;
        break;
      }
      case ATOModeSupervisedPos:
      {
        selectorType = ATOModeCabinSupervised;
        break;
      }
      case ATOModeAutomaticPos:
      {
        selectorType = ATOModeCabinAutomatic;
        break;
      }
      case ATOModeUnknownPos:
      default:
      {
        selectorType = ATOModeCabinNotAsserted;
        break;
      }
      }
    }

    /******************************************************************************
    * LCSAOSStatusType::LCSAOSStatusType()
    ******************************************************************************/
    LCSAOSStatusType::LCSAOSStatusType() :
      atoModeCabinSelectorStatus(ATOModeCabinNotAsserted),
      trainIdling(TrainIdlingNotAsserted),
      trackIdFrontOfTrain(0U),
      positionOnTrackFrontOfTrain(0U),
      trainOrientationFrontTrack(TrainOrientationFrontTrackLeadTowardsLeg0),
      trackIdRearOfTrain(0U),
      positionOnTrackRearOfTrain(0U),
      trainOrientationRearTrack(TrainOrientationRearTrackLeadTowardsLeg0),
      travelDirection(TravelDirectionLocoLeading),
      vehicleSpeed(0U),
      blueFlagStatus(BlueFlagInactive),
      messageTime(0U),
      limitedSupervisedMode(LimitedSupervisedModeNotAsserted)
    {
    }
  }
}
