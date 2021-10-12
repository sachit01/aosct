/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the AbstractMode class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-19    arastogi    Created
* 2017-06-07    skothiya    Updated for train state implementation
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_math.hpp"
#include "abstract_mode.hpp"
#include "abstract_odometry.hpp"
#include "abstract_log_handler.hpp"
#include "abstract_position.hpp"
#include "abstract_targets.hpp"
#include "abstract_tracks.hpp"
#include "abstract_tims.hpp"
#include "abstract_loco_io.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_mode_control.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include <vfw_checkpoints.h>
#include "abstract_tsetup.hpp"
#include "abstract_radio_handler.hpp"
#include "abstract_config.hpp"
#include "abstract_mode_control_event_ids.hpp"
#include "abstract_dmi_handler.hpp"

#ifdef WIN32
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
  namespace Kernel
  {

    /********************************************************************
    *static variables
     ********************************************************************/
     //variables used to communicate Q_SETUP between Configuration and Registration mode
     //Q_SETUP will be received in TrainSetup message in configuration mode and same will be used in registration mode
    TrainSetupReason AbstractMode::qSetupFromTSetup = TrainSetupRegistration;
    bool AbstractMode::isValidQSetupReceived = false;
    uint32_t AbstractMode::maxSearchDistanceInReReg = 0U;
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractMode::AbstractMode() :
      initDone(false),
      idleTrainStateStandstill(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdStandstillInIdleTrainState,
        ATC::NoSB, 0x0U, "Applying Standstill in Idle state")),
      odometerInvalidSB(ATC::Event::createSafeBrakeSBEvent(atpModeControlId, ATC::CoreContainer, eventIdOdometerInvalid,
        ATC::NoSB, DMICom::modOdometerInvalid, "Applying Service Brake in Odometer Invalid state")),
      tccConnectionLost(ATC::Event::createSBReqEvent(atpModeControlId, ATC::CoreContainer, eventIdTCClost,
        ATC::NoSB, DMICom::tccConnectionLost, "TCC Connection Lost !")),
      tccConnectionLostInStandstill(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdTCClostInStandstill, 
        ATC::NoSB, DMICom::tccConnectionLost, "TCC Connection Lost in standstill!")),
      maTimeout(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer, eventIdMATimeout, DMICom::maTimeout,
        "MA Timeout")),
      stopTrainRequested(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer, eventIdStopTrainRequested, DMICom::stopTrainRequested,
        "Stop Train requested"))
    {
      trace = AbstractModeControl::corePtr()->getTrace();
      nextMode = ATPModeUndefined;
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractMode::init(void)
    {
      //If init has not been done successfully.
      if (!initDone)
      {
        initDone = true;

        initCrossCompare();
      }

      return initDone;
    }

    /******************************************************************************
    * setNextMode
    ******************************************************************************/
    void AbstractMode::setNextMode(const ATPMode mode)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "MC_setNextMode");

      nextMode = mode;
    }

    /******************************************************************************
    * getNextMode
    ******************************************************************************/
    ATPMode AbstractMode::getNextModeId() const
    {
      return nextMode;
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void AbstractMode::resetMode()
    {
      nextMode = ATPModeUndefined;
    }

    /******************************************************************************
    * ~AbstractMode
    ******************************************************************************/
    AbstractMode::~AbstractMode()
    {
      trace = static_cast<ATC::TraceInterface*>(NULL);
    }


    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool AbstractMode::getCurrentModeStateString(char_t* const /*str*/)
    {
      return false;
    }


    /******************************************************************************
    * manageTrainIdling
    ******************************************************************************/
    void AbstractMode::manageTrainIdling(CommonDataForModes &commonData)
    {
      //Check if primary target exist
      if (static_cast<DS::PrimaryTarget *>(NULL)
        == DS::AbstractTargets::corePtr()->getPrimaryTarget())
      {
        //if train was not idling before.. write to log that the train is idling now.
        if (!commonData.idling)
        {
          trace->write(3U, "Train Idling !!!");
          commonData.idling = true;
        }
        //Applying Standstill event in Idle state
        ATC::AbstractEventHandler::corePtr()->reportEvent(idleTrainStateStandstill, __FILE__, __LINE__);
        //Clearing Free Rolling and Odometer Invalid status in Idle state
        commonData.freeRolling = false;
        commonData.odometerInvalid = false;
        //removing tracks and balise not covered by the vehicle footprint 
        if (TG::AbstractTIMS::corePtr()->isRearPositionValid())
        {
          if (!(DS::AbstractTracks::corePtr()->removeNotCovered(
            TG::AbstractTIMS::corePtr()->getSafePositionToDeleteTrack(),
            Pos::AbstractPosition::corePtr()->getSafeLeadingPosOdo())))
          {
            // No tracks or balises to remove.
          }
        }
      }
      else
      {
        commonData.idling = false;
      }

    }


    /******************************************************************************
    * manageMATimeout
    ******************************************************************************/
    void AbstractMode::manageMATimeout(CommonDataForModes &commonData)
    {

      //if train stand still
      if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
      {
        bool maTimeoutExpired = false;
        DS::BaseTarget* bTarget = DS::AbstractTargets::corePtr()->getPrimaryTarget();

        // Check if MA timeout expired if any Primary target exists
        if(bTarget != static_cast<DS::BaseTarget*>(NULL))
        {
          const DS::PrimaryTarget* const pTarget
            = ATC::dynamicCast<DS::BaseTarget*, DS::PrimaryTarget*>(bTarget, __FILE__, __LINE__);

          int64_t currentTime = vfwGetReferenceTime();
          int64_t maTimeoutTimeStamp = 0;

          if (pTarget->getMATimeout() != 0U)
          {
            maTimeoutTimeStamp = pTarget->getMATimeoutTimeStamp();
          }

          //check if MA timeout exceeded
          if ((maTimeoutTimeStamp != 0) &&
            (currentTime > maTimeoutTimeStamp))
          {
            maTimeoutExpired = true;
          }
        }

        // Set MA Timeout if timeout expired 
        // OR Train is in Stop Train state and not reached Primary target
        // OR Train is in Emergency alert state and not reached Primary Target
        const bool reachedPrimaryTarget = DS::AbstractTargets::corePtr()->isPrimaryTargetReached();
        EmergencyAlertState emergencyAlertSeq = AbstractModeControl::corePtr()->getEmergencyAlertSeqState();
        if (maTimeoutExpired
          || 
          ((!reachedPrimaryTarget) &&
          (commonData.stopTrainActive
          || (emergencyAlertSeq != emergencyAlertInactive))))
        {
          if (!commonData.maTimeOut)
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(maTimeout, __FILE__, __LINE__);
            commonData.maTimeOut = true;
            DS::AbstractTargets::corePtr()->removeAll();
          }
        }
        else if (bTarget != static_cast<DS::BaseTarget*>(NULL))
        { 
          // Reset MA timeout when no condition for timeout and primary target exists again
          commonData.maTimeOut = false;
        } 
        else
        {
          // No action (to satisfy lint)
        }
      }
    }

    /******************************************************************************
    * manageStopTrain
    ******************************************************************************/
    void AbstractMode::manageStopTrain(CommonDataForModes &commonData)
    {
      if (AbstractMessageHandler::corePtr()->getStopTrain())
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(stopTrainRequested, __FILE__, __LINE__);
        commonData.stopTrainActive = true;
        commonData.handlingDone = false;
      }
      else
      {
        //if train comes in standstill after receiving Stop Train
        const bool isTrainStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
        if (commonData.stopTrainActive && isTrainStandStill)
        {
          commonData.stopTrainActive = false;
          //delete all targets
          DS::AbstractTargets::corePtr()->removeAll();
        }

      }
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractMode::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&initDone));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TrainSetupReason>(&qSetupFromTSetup));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&isValidQSetupReceived));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&maxSearchDistanceInReReg));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<ATPMode>(&nextMode));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&idleTrainStateStandstill));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&odometerInvalidSB));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&tccConnectionLost));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&tccConnectionLostInStandstill));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&maTimeout));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&stopTrainRequested));
    }

    /******************************************************************************
    * manageFreeRolling
    ******************************************************************************/
    void AbstractMode::manageFreeRolling(CommonDataForModes &commonData)
    {
      if (DS::AbstractTargets::corePtr()->getFreeRollingTargetActive())
      {
        commonData.freeRolling = true;
      }
      else
      {
        commonData.freeRolling = false;
      }
    }


    /******************************************************************************
    * manageOdometerInvalid
    ******************************************************************************/
    void AbstractMode::manageOdometerInvalid(CommonDataForModes &commonData)
    {
      if (DS::AbstractTargets::corePtr()->getOdometerInvalidTargetActive())
      {
        commonData.odometerInvalid = true;
        if (!commonData.freeRolling)
        {
          //Apply service brake if odometer invalid is set and free rolling is not set 
          ATC::AbstractEventHandler::corePtr()->reportEvent(odometerInvalidSB, __FILE__
            , __LINE__);
        }
      }
      else
      {
        commonData.odometerInvalid = false;
      }

    }

    /******************************************************************************
    * manageHandlingDone
    ******************************************************************************/
    void AbstractMode::manageHandlingDone(CommonDataForModes &commonData)
    {
      if (commonData.handlingDone)
      {
        if (AbstractMessageHandler::corePtr()->isValidMAReceived())
        {
          commonData.handlingDone = false;
        }
      }
    }

    /******************************************************************************
    * manageLocationModeValidity
    ******************************************************************************/
    void AbstractMode::manageLocationModeValidity(CommonDataForModes &commonData)
    {
      const DS::BaseTarget* const locStartTarget = DS::AbstractTargets::corePtr()->getLocationStartTarget();

      if (static_cast<DS::BaseTarget *>(NULL) != locStartTarget)
      {
        const TravelDir trvlDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();

        const OdoPosition rearPosOdo = TG::AbstractTIMS::corePtr()->getRearPositionOdo();
        const OdoPosition locStartOdo = locStartTarget->getOdometer();

        if (DirForward == trvlDir)
        {
          if (rearPosOdo > locStartOdo)
          {
            commonData.isAllowedToEnterLocationMode = true;
          }
        }
        else if (DirReverse == trvlDir)
        {
          if (rearPosOdo < locStartOdo)
          {
            commonData.isAllowedToEnterLocationMode = true;
          }
        }
        else
        {
          //do nothing
        }
      }
    }

    /******************************************************************************
    * isModeChangedByMA
    ******************************************************************************/
    bool AbstractMode::isModeChangedByMA(ATPMode& mode) const
    {
      bool modeChanged = false;

      MAHead maHead;
      if (AbstractMessageHandler::corePtr()->getMAHead(maHead))
      {
        switch (maHead.routeType)
        {
          case RtShuntingRoute:
            modeChanged = true;
            mode = ATPModeShuntingRoute;
            break;

          case RtStaffResponsible:
            modeChanged = true;
            mode = ATPModeStaffResponsible;
            break;

          case RtJoin:
            modeChanged = true;
            mode = ATPModeJoin;
            break;

          case RtSplit:
            modeChanged = true;
            mode = ATPModeSplit;
            break;

          case RtNormal:
          case RtUndefined:
          case RtReRegistration:
          default:
            modeChanged = false;
            break;
        }
      }
      return modeChanged;
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    ModeState AbstractMode::getModeState() const
    {
      return undefinedModeState;
    }

    /******************************************************************************
    * isValidQRouteType
    ******************************************************************************/
    bool AbstractMode::isValidQRouteType(const RouteType routeType) const
    {
      trace->write(ATC::detailedTrace, "Invalid Mode for MA", static_cast<uint32_t>(routeType));
      return false;
    }

    /******************************************************************************
    * setMaxAllowedSpeedInLocation
    ******************************************************************************/
    void AbstractMode::setMaxAllowedSpeedInLocation(const uint16_t speed)
    {
      trace->write(ATC::detailedTrace, "Invalid Mode for maximum allowed speed", static_cast<uint32_t>(speed));
    }

    /******************************************************************************
    * removePassedObjects
    ******************************************************************************/
    void AbstractMode::removePassedObjects(const CommonDataForModes &commonData) const
    {
      //remove passed target
      DS::AbstractTargets::corePtr()->removePassedTargets();

      //Do not Remove tracks when in normal mode and entering in Location mode 
      if (!(commonData.isAllowedToEnterLocationMode))
      {
        if (TG::AbstractTIMS::corePtr()->isRearPositionValid())
        {
          // Remove passed tracks and balises
          if (!DS::AbstractTracks::corePtr()->removePassed(
            TG::AbstractTIMS::corePtr()->getSafePositionToDeleteTrack()))
          {
            trace->write(1U, "Failed to remove passed tracks");
          }
        }
      }
    }
    
    /******************************************************************************
    * handleUnRegMessage
    ******************************************************************************/
    bool AbstractMode::handleUnRegMessage()
    {
      bool isUnRegMsgProcessed = false;
      //Check for Un-registration message
      UnregInfo UnRegInfo;
      if (AbstractMessageHandler::corePtr()->getUnregInfo(UnRegInfo))
      {
        //set the next mode
        setNextMode(ATPModeUnregistered);
        isUnRegMsgProcessed = true;
      }
      return isUnRegMsgProcessed;
    }


    /******************************************************************************
    * handleAbortSetup
    ******************************************************************************/
    void AbstractMode::handleAbortSetup(CommonDataForModes &commonData)
    {
      if (DMICom::DMIButtonAbortSetup == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus())
      {
        commonData.trainSetupAborted = true;
        commonData.abortSetupReason = AbortedByDriver;
        setNextMode(ATPModeConfiguration);
      }
    }

    /******************************************************************************
    * clear
    ******************************************************************************/
    void CommonDataForModes::clear()
    {
      stopTrainActive = false;
      idling = false;
      maTimeOut = false;
      handlingDone = false;
      odometerInvalid = false;
      freeRolling = false;
      atpOkStatus = false;
      atpLampStatus = false;
      isATPReset = false;
      isNewConfig = false;
      modeReqByDMI = ATPModeUndefined;
      buzzer = BuzzerTypeNone;
      isAbortSetupActive = false;
      odoDirectionNewReg = OdoUndefined;
      currentDrivDirection = DirForward;
      isAllowedToEnterLocationMode = false;
      freeRollingButton.freeRollingConfirmToDMI = false;
      freeRollingButton.freeRollingDisplayToDMI = false;
      trainSetupAborted = false;
    }

    /******************************************************************************
    * manageTCCTimeOut
    ******************************************************************************/
    void AbstractMode::manageTCCTimeOut()
    {
      if (RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus())
      {
        const int64_t timeDiffFrmTCCTimeout = vfwGetReferenceTime() - RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutVal();
        const int64_t radioTimeOutSBConfigValInMilliSec = static_cast<int64_t>(AbstractConfig::corePtr()->getRadioTimeOutSb()) *
          (static_cast<int64_t>(ATC::secToMSec));
        const bool isRadioTimeOutNonZero = (0 != radioTimeOutSBConfigValInMilliSec);
        const bool isTimeGreaterThanConfigValue = (timeDiffFrmTCCTimeout > radioTimeOutSBConfigValInMilliSec);

        if (isRadioTimeOutNonZero && isTimeGreaterThanConfigValue)
        {
          //raise event depending on if in standstill or not
          if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
          {
            // raise a log event since the service brake should not be triggered in this case when in stand still
            ATC::AbstractEventHandler::corePtr()->reportEvent(tccConnectionLostInStandstill, __FILE__, __LINE__);
          }
          else
          {
            // raise a brake event
            ATC::AbstractEventHandler::corePtr()->reportEvent(tccConnectionLost, __FILE__, __LINE__);
          }

          //delete the target at standstill if the targetlist is not already empty
          if (!ATP::DS::AbstractTargets::corePtr()->isMATargetListEmpty())
          {
            DS::AbstractTargets::corePtr()->setDelTargetAtStandStill(true);
          }
        }
      }
    }

    /******************************************************************************
    * handleCurrentDrivDirection
    ******************************************************************************/
    void AbstractMode::handleCurrentDrivDirection(CommonDataForModes &commonData)
    {
      TravelDir direction = IO::AbstractLocoIO::corePtr()->getLocoDirection();

      if (ATOModeManual == AbstractModeControl::corePtr()->getATOMode())
      {
        // Only care about actual direction
        if ((DirForward == direction) || (DirReverse == direction))
        {
          commonData.currentDrivDirection = direction;
        }
      }
    }

    /******************************************************************************
    * isValidUncondShorteningMsg
    ******************************************************************************/
    bool AbstractMode::isValidUncondShorteningMsg() const
    {
      return false;
    }
  }
}
