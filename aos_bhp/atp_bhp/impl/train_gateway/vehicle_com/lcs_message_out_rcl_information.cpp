/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2020
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->LCS) has an associated creator class inherited from AbstractLCSMessageOut.
* This file implements the creator for the RCL Information message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
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
#include "lcs_message_out_rcl_information.hpp"
#include "lcs_message_common.hpp"
#include "atc_util.hpp"
#include "abstract_brake.hpp"
#include "abstract_targets.hpp"
#include "abstract_supervise.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_target_calculation.hpp"

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
    * LCSMessageOutRclInformation constructor
    ******************************************************************************/
    LCSMessageOutRclInformation::LCSMessageOutRclInformation() : AbstractLCSMessageOut(LCSMTypeRclInformation, 1U, true)
    {
      sendRclInfoMessageCounter = 0U;
    }

    /******************************************************************************
    * LCSMessageOutRclInformation::collectData
    ******************************************************************************/
    void LCSMessageOutRclInformation::collectData()
    {

      if (sendRclInfoMessageCounter > 0U)
      {
        --sendRclInfoMessageCounter;
      }

      // Check if it is time to send status-data
      if (sendRclInfoMessageCounter == 0U)
      {
        sendRclInfoMessageCounter = timeoutSendRclInfoMessage / ATP::Kernel::AbstractATPApplication::atpAppCycleTime;


        // Get current mode to determine Idle status
        ATPMode currMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

        DS::BaseTarget *pTarget = DS::AbstractTargets::corePtr()->getPrimaryTarget();

        const int32_t currLeadPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();
        const int32_t currTrailPos = Pos::AbstractPosition::corePtr()->getTrailingPosOdo();

        const TravelDir direction = IO::AbstractLocoIO::corePtr()->getLocoDirection();

        const uint16_t numberOfEvents = ATC::AbstractEventHandler::corePtr()->getNumberOfStandstillEvents();

        const bool activeStandStillEventOtherThanNeutral = (numberOfEvents > 1U)  || ((numberOfEvents == 1U) && (direction != DirNone));
        
        switch (currMode)
        {
        case ATPModePowerUp:
        case ATPModeConfiguration:
        case ATPModeRegistration:
        case ATPModeUnregistered:
        case ATPModePoweringDown:
        case ATPModeSafetyHalt:
        case ATPModeSleeping:
        case ATPModeSafeBrakeToStop:
        case ATPModeStaffResponsible:
        case ATPModeShuntingRoute:
        case ATPModeSplit:
        case ATPModeJoin:
        case ATPModeBaliseSearch:
        case ATPModeShunting:
        case ATPModeYard:
        case ATPModePossession:

          rclInfo.aosOperationalMode = AosOperationalModeOther;
          rclInfo.allowedTrainMovement = AllowedTrainMovementUndefined;
          rclInfo.distanceToGoReverse = -1;
          rclInfo.distanceToGoForward = -1;
          break;

        case ATPModeNormal:    
        {
          rclInfo.aosOperationalMode = AosOperationalModeNormal;

          if (activeStandStillEventOtherThanNeutral || (pTarget == static_cast<DS::BaseTarget*>(NULL)))
          {
            // None when no MA is present 
            // Or standstill  has been raised for reasons other than direction controller in Neutral.
            rclInfo.allowedTrainMovement = AllowedTrainMovementNone;
          }
          else
          {
            rclInfo.allowedTrainMovement = (
              DirReverse == pTarget->getDirection()) ? AllowedTrainMovementReverse : AllowedTrainMovementForward;
          }

          // Set forward and reverse Distanse to Go 
          // Distance to Go is calculated from SB primary target/Curve. 
          // Since SB primary and EB primary will be same, if SB curve is not configured.
          const int32_t releaseSpeed = Supv::AbstractTargetCalculation::corePtr()->getReleaseSpSBPos();
          if (AllowedTrainMovementReverse == rclInfo.allowedTrainMovement)
          {
            rclInfo.distanceToGoReverse = ((currLeadPos - releaseSpeed) > 0) ? (currLeadPos - releaseSpeed) : 0;
            rclInfo.distanceToGoForward = 0;
          }
          else if (AllowedTrainMovementForward == rclInfo.allowedTrainMovement)
          {
            rclInfo.distanceToGoReverse = 0;
            rclInfo.distanceToGoForward = ((releaseSpeed - currLeadPos) > 0) ? (releaseSpeed - currLeadPos) : 0;
          }
          else
          {
            rclInfo.distanceToGoReverse = 0;
            rclInfo.distanceToGoForward = 0;
          }
          break;
        }
        case ATPModeLocation:
        {
          rclInfo.aosOperationalMode = AosOperationalModeLocation;

          // Set None when no MA is present Or standstill  has been raised for reasons other than direction controller in Neutral.
          if (activeStandStillEventOtherThanNeutral || (pTarget == static_cast<DS::BaseTarget*>(NULL)))
          {
            rclInfo.allowedTrainMovement = AllowedTrainMovementNone;
          }
          else
          {
            rclInfo.allowedTrainMovement = AllowedTrainMovementBoth;
          }

          int32_t currFwPos = currLeadPos;
          int32_t currRevPos = currTrailPos;
          //when direction is reverse, Fowrwd, Distance to go will be calculated 
          //from trailing end of train and Reverse, Distance to go will be calculated from leading end.
          if (DirReverse == DS::AbstractTargets::corePtr()->getSupposedTravelDir())
          {
            currFwPos = currTrailPos;
            currRevPos = currLeadPos;
          }
          // Forward and Reverse Distance to Go is calculated based on sb primary target.
          DS::SupervisedTargetIterator it = DS::AbstractTargets::corePtr()->getSupervisedTargetIter();
          while (it != DS::AbstractTargets::corePtr()->getSupervisedTargetIterEnd())
            {
              DS::SupervisedTarget* sTarget = *it;
              if (DS::BaseTarget::SupervisedTarget == sTarget->getTargetType()) 
              {
                // SB curve is not configured. EB primary target and SB target will be same.
                if (DS::SupervisedTarget::SBPrimaryTarget == sTarget->getSupervisedTargetType())
                {
                  if ((DirForward == sTarget->getDirection()))
                  {
                    rclInfo.distanceToGoForward = sTarget->getOdometer() - currFwPos;
                  }
                  else
                  {
                    rclInfo.distanceToGoReverse = currRevPos - sTarget->getOdometer();
                  }
                }
              }
              ++it;
            }
          break;
        }
        case ATPModesCount:
        case ATPModeUndefined:
        default:
          // Unknown mode
          rclInfo.aosOperationalMode = AosOperationalModeUndefined;
          ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        }

        //AOS intervention applied
        bool ebApplied = Supv::AbstractBrake::corePtr()->getEbApplied();
        bool sbApplied = Supv::AbstractBrake::corePtr()->getSbApplied();
        rclInfo.aosInterventionApplied = static_cast<AosInterventionType>(ebApplied || sbApplied);

        //Train orientation
        const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();
        if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
        {         
          rclInfo.trainOrientation = (pTrainSetup->orientation == trainOrientationCarConnectedToAEnd) ?
            TrainOrientationTrainForwardLocomotiveReverse : TrainOrientationTrainForwardLocomotiveForward;
        }
        else
        {
          rclInfo.trainOrientation = TrainOrientationUndefined;
        }

        // ceiling Speed, Convert unit from cm/sec to 0.1km/h
        rclInfo.currentCeilingSpeed = 
          static_cast<uint16_t>(ATC::ATCMath::convCmpsTo100mph(DS::AbstractTargets::corePtr()->getCurCeilingSpeed()));

        setDataProcessState(DataAvailable);

        uint8_t currentLevel;
        bool isEnabled;
        getTracer().getTraceDetails(currentLevel, isEnabled);
        if (isEnabled && (currentLevel >= ATC::veryDetailedMessageTrace))
        {
          getTracer().write(ATC::veryDetailedMessageTrace, "RclInformation:aosOperationalMode:", static_cast<uint32_t>(rclInfo.aosOperationalMode));
          getTracer().write(ATC::veryDetailedMessageTrace, "RclInformation:aosInterventionApplied:", static_cast<uint32_t>(rclInfo.aosInterventionApplied));
          getTracer().write(ATC::veryDetailedMessageTrace, "RclInformation:allowedTrainMovement:", static_cast<uint32_t>(rclInfo.allowedTrainMovement));
          getTracer().write(ATC::veryDetailedMessageTrace, "RclInformation:distanceToGoForward:", static_cast<uint32_t>(rclInfo.distanceToGoForward));
          getTracer().write(ATC::veryDetailedMessageTrace, "RclInformation:dtgReverse:", static_cast<uint32_t>(rclInfo.distanceToGoReverse));
          getTracer().write(ATC::veryDetailedMessageTrace, "RclInformation:trainOrientation:", static_cast<uint32_t>(rclInfo.trainOrientation));
          getTracer().write(ATC::veryDetailedMessageTrace, "RclInformation:currentCeilingSpeed:", static_cast<uint32_t>(rclInfo.currentCeilingSpeed));
        }
      }

    }

    /******************************************************************************
    * LCSMessageOutRclInformation::validate
    ******************************************************************************/
    bool LCSMessageOutRclInformation::validate(EmpMsg* const mData, uint16_t& length)
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
    * LCSMessageOutRclInformation::assembleMessageData
    ******************************************************************************/
    bool LCSMessageOutRclInformation::assembleMessageData(EmpMsg* const messageData, uint16_t& appDataLength) const
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, messageData->getEMPBodyBuffer(), messageData->getEMPMessageMaxBodyLen());

      vfwPutU8(&buffer, static_cast<uint8_t>(rclInfo.aosOperationalMode));
      vfwPutU8(&buffer, static_cast<uint8_t>(rclInfo.aosInterventionApplied));

      vfwPutU8(&buffer, static_cast<uint8_t>(rclInfo.allowedTrainMovement));
      vfwPutI32(&buffer, rclInfo.distanceToGoForward);
      vfwPutI32(&buffer, rclInfo.distanceToGoReverse);

      vfwPutU8(&buffer, static_cast<uint8_t>(rclInfo.trainOrientation));
      vfwPutU16(&buffer, static_cast<uint8_t>(rclInfo.currentCeilingSpeed));

      traceAssembleData(parseDataValid);

      // Total length of Application-message
      appDataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));

      return parseDataValid;
    }

    /******************************************************************************
    *  LCSMessageOutRclInformation::invalidate
    ******************************************************************************/
    void LCSMessageOutRclInformation::invalidate()
    {
      setDataProcessState(NoDataAvailable);
    }

    /******************************************************************************
    * LCSAOSStatusType::LCSAOSStatusType()
    ******************************************************************************/
    LCSRclInformationType::LCSRclInformationType() :
      aosOperationalMode(AosOperationalModeUndefined),
      aosInterventionApplied(AosInterventionNotApplied),
      allowedTrainMovement(AllowedTrainMovementUndefined),
      distanceToGoForward(-1),
      distanceToGoReverse(-1),
      trainOrientation(TrainOrientationUndefined),
      currentCeilingSpeed(0U)
    {
    }
  }
}
