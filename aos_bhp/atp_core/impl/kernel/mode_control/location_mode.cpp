/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the Location mode class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-04-10   spandita    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "location_mode.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_targets.hpp"
#include "abstract_odometry.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_tsetup.hpp"
#include <vfw_string.h>

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
    * Constructor
    ******************************************************************************/
    LocationMode::LocationMode() : AbstractMode(),
    modeState(locationStart)
    {
      maxAllowedSpeeInLoc = 0U;
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void LocationMode::resetMode()
    {
      modeState = locationStart;
      // !FT3
      AbstractMode::resetMode();
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void LocationMode::handleMode(CommonDataForModes &commonData)
    {
      //Remove passed targets
      DS::AbstractTargets::corePtr()->removePassedTargets();
      //manage TCC timeout
      manageTCCTimeOut();
      //Checking for stop train
      manageStopTrain(commonData);
      //Checking for Odometer Invalid status
      manageOdometerInvalid(commonData);
      //Handle driving direction in location
      handleCurrentDrivDirection(commonData);
      //Handle standstill condition in location
      handleStandStillCondition();
      //Handle Handling done condition
      manageHandlingDone(commonData);
      //Handle Mode change condition
      handleModeChangeCondition();
      //Handle gradient values in location
      handleGradInLocation(commonData);
      LocationModeState oldModeState = modeState;
      //run the function corresponding to the modeState.
      switch (modeState)
      {
      case locationStart:
        runLocationStart(commonData);
        break;

      case locationWaitToConfirmType:
        runLocationWaitToConfirmType();
        break;

      case locationYardModeHandlingStart:
        runLocYardModeHandlingStart();
        break;

      case locationYardModeHandlingDone:
        runLocYardModeHandlingDone();
        break;

      case locationYardModWaitAck:
        runLocYardModWaitAck();
        break;

      case locationUnloadLocation:
        runLocUnloadLocation(commonData);
        break;

      case locationFinishOk:
        runLocFinishOK(commonData);
        break;
      default:
        break;
      }

      //If Mode state has changed
      if (oldModeState != modeState)
      {
        char_t buffer[maxModeStateNameLength];
        getModeStateString(modeState, &buffer[0]);

        AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
        AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
      }
    }

    /******************************************************************************
    * getModeId
    ******************************************************************************/
    ATPMode LocationMode::getModeId()
    {
      return ATPModeLocation;
    }

    /******************************************************************************
    * runLocationStart
    ******************************************************************************/
    void LocationMode::runLocationStart(CommonDataForModes &commonData)
    {
      //Clearing MA Timeout and Train Idling 
      manageMATimeout(commonData);
      manageTrainIdling(commonData);
      //Delete normal target
      DS::AbstractTargets::corePtr()->removeNormalPrimaryTargetInLocation();
      //change the submode 
      modeState = locationWaitToConfirmType;
    }

    /******************************************************************************
    * handleModeChangeCondition
    ******************************************************************************/
    void LocationMode::handleModeChangeCondition()
    {
      bool isValidSubMode = (locationFinishOk != modeState);
      if ((DS::AbstractTargets::corePtr()->isMATargetListEmpty()) &&
        isValidSubMode)
      {
        //Set the next mode to Normal
        modeState = locationFinishOk;
      }
    }

    /******************************************************************************
    * runLocationWaitToConfirmType
    ******************************************************************************/
    void LocationMode::runLocationWaitToConfirmType()
    {
      DS::BaseTarget* bTarget = DS::AbstractTargets::corePtr()->getLocationStartTarget();
      if(bTarget != static_cast<DS::BaseTarget*>(NULL))
      {
        const DS::PrimaryTarget* const locTarget =
          ATC::dynamicCast<DS::BaseTarget*, DS::PrimaryTarget*>(bTarget,__FILE__, __LINE__);

        //Type of location?
        switch (locTarget->getLocationType())
        {
        case LocationtypeUnloadLocation:
          modeState = locationUnloadLocation;
          break;

        case LocationTypeYardMode:
          modeState = locationYardModeHandlingStart;
          break;

        case UndefinedLocationType:
        case LocationTypeRemoteLoad:
        case LocationTypeFutureCoreMin:
        case LocationTypeFutureCoreMax:
        case LocationTypeFutureAdapMin:
        case LocationTypeFutureAdapMax:
        case LocationTypeManualHandling:
        default:
          break;
        }
      }
    }

    /******************************************************************************
    * runLocFinishOK
    ******************************************************************************/
    void LocationMode::runLocFinishOK(CommonDataForModes &commonData)
    {
      //To satisfy lint
      const bool temp = commonData.freeRolling;
      commonData.freeRolling = temp;
      //Set the next mode
      setNextMode(ATPModeNormal);
    }

    /******************************************************************************
    * runLocUnloadLocation
    ******************************************************************************/
    void LocationMode::runLocUnloadLocation(CommonDataForModes &commonData)
    {
      manageFreeRolling(commonData);
    }

    /******************************************************************************
    * setMaxAllowedSpeedInLocation
    ******************************************************************************/
    void LocationMode::setMaxAllowedSpeedInLocation(const uint16_t speed)
    {
      maxAllowedSpeeInLoc = speed;
    }

    /******************************************************************************
    * manageHandlingDone
    ******************************************************************************/
    void LocationMode::manageHandlingDone(CommonDataForModes &commonData)
    {
      if (!commonData.handlingDone)
      {
        //Get the button status
        const bool isDmiButtonPressed = (DMICom::DMIButtonHandlingDone == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus());
        //is ATO mode Manual?
        const bool isManualAtoMode = (ATOModeManual == AbstractModeControl::corePtr()->getATOMode());
        if ((Pos::AbstractOdometry::corePtr()->isTrainStandStill())
          && isDmiButtonPressed && isManualAtoMode)
        {
          setHandlingDone(commonData);
        }
      }
    }

    /******************************************************************************
    * setHandlingDone
    ******************************************************************************/
    void LocationMode::setHandlingDone(CommonDataForModes& commonData)
    {
      //Delete all the targets
      DS::AbstractTargets::corePtr()->removeAll();
      //Set the next mode
      modeState = locationFinishOk;
      commonData.handlingDone = true;
    }

    /******************************************************************************
    * runLocYardModWaitAck
    ******************************************************************************/
    void LocationMode::runLocYardModWaitAck()
    {
      YardAcknowledge yardAck;
      if (AbstractMessageHandler::corePtr()->getYardAcknowledge(yardAck))
      {
        if (RequestAcknowledged == yardAck.yardAcknowledge)
        {
          maxAllowedSpeeInLoc = yardAck.allowedSpeedInYard;
          modeState = locationYardModeHandlingDone;
        }
        else
        {
          modeState = locationYardModeHandlingStart;
        }
      }
    }

    /******************************************************************************
    * runLocYardModeHandlingStart
    ******************************************************************************/
    void LocationMode::runLocYardModeHandlingStart()
    {
      //Yard button pressed
      if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonEnterYardMode)
      {
        if (!Pos::AbstractOdometry::corePtr()->isTrainStandStill())
        {
          modeState = locationYardModWaitAck;
        }
      }
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool LocationMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void LocationMode::getModeStateString(const LocationModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case locationStart:
        static_cast<void>(vfw_strlcpy(buffer, "locationStart", maxModeStateNameLength));
        break;

      case locationWaitToConfirmType:
        static_cast<void>(vfw_strlcpy(buffer, "locationWaitToConfirmType", maxModeStateNameLength));
        break;

      case locationYardModeHandlingStart:
        static_cast<void>(vfw_strlcpy(buffer, "locationYardModeHandlingStart", maxModeStateNameLength));
        break;

      case locationYardModeHandlingDone:
        static_cast<void>(vfw_strlcpy(buffer, "locationYardModeHandlingDone", maxModeStateNameLength));
        break;

      case locationYardModWaitAck:
        static_cast<void>(vfw_strlcpy(buffer, "locationYardModWaitAck", maxModeStateNameLength));
        break;

      case locationUnloadLocation:
        static_cast<void>(vfw_strlcpy(buffer, "locationUnloadLocation", maxModeStateNameLength));
        break;

      case locationFinishOk:
        static_cast<void>(vfw_strlcpy(buffer, "locationFinishOk", maxModeStateNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
        break;
      }
    }

    /******************************************************************************
    * manageTrainIdling
    ******************************************************************************/
    void LocationMode::manageTrainIdling(CommonDataForModes &commonData)
    {
      commonData.idling = false;
    }

    /******************************************************************************
    * handleCurrentDrivDirection
    ******************************************************************************/
    void LocationMode::handleCurrentDrivDirection(CommonDataForModes &commonData)
    {
      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

      DS::BaseTarget* bTarget = DS::AbstractTargets::corePtr()->getLocationEndTarget();

      if(bTarget != static_cast<DS::BaseTarget*>(NULL))
      {
        const DS::PrimaryTarget* const locTarget =
          ATC::dynamicCast<DS::BaseTarget*, DS::PrimaryTarget*>(bTarget,__FILE__, __LINE__);

        if (LocationtypeUnloadLocation == locTarget->getLocationType())
        {
          if (commonData.freeRolling)
          {
            if (isStandStill)
            {
              commonData.currentDrivDirection = DirForward;
            }
            else
            {
              //get the data from odometry
              commonData.currentDrivDirection = Pos::AbstractOdometry::corePtr()->getOdoDirection();
            }
          }
          else
          {
            // Do normal direction handling.
            AbstractMode::handleCurrentDrivDirection(commonData);
          }
        }
        else
        {
          AbstractMode::handleCurrentDrivDirection(commonData);
        }
        if ((commonData.currentDrivDirection == DirForward) || (commonData.currentDrivDirection == DirReverse))
        {
          DS::AbstractTargets::corePtr()->setSupposedTravelDir(commonData.currentDrivDirection);
        }
      }

    }

    /******************************************************************************
    * runLocationYardModeHandling
    ******************************************************************************/
    void LocationMode::runLocYardModeHandlingDone()
    {
      if (Pos::AbstractOdometry::corePtr()->getSpeed() < maxAllowedSpeeInLoc)
      {
        //Set the ceiling speed 
        DS::AbstractTargets::corePtr()->setCurCeilingSpeed(maxAllowedSpeeInLoc);
        DS::AbstractTSetup::corePtr()->removeTrainSetup();
        //change the mode to yard
        setNextMode(ATPModeYard);
      }
    }

    /******************************************************************************
    * manageMATimeoutState
    ******************************************************************************/
    void LocationMode::manageMATimeout(CommonDataForModes &commonData)
    {
      commonData.maTimeOut = false;
    }

    /******************************************************************************
    * handleStandStillCondition
    ******************************************************************************/
    void LocationMode::handleStandStillCondition() const
    {
      const bool isYardButtonPressed = (YardModeRequestSeq::yardConfirmed == AbstractModeControl::corePtr()->getYardModReqSeqState());
      const bool isPosButttonPressed = (PosModeRequestSeq::posDmiButtonConfirmed == AbstractModeControl::corePtr()->getPosModReqSeqState());
      const bool isShuntButtonPressed = (ShuntModeRequestSeq::shuntDmiButtonConfirmed == AbstractModeControl::corePtr()->getShuntModReqSeqState());
      //Any DMI pressed?
      const bool isModeButtonAlreadyPressed = (isYardButtonPressed || isPosButttonPressed || isShuntButtonPressed);
      //If standstill and no DMI button has been pressed
      if (Pos::AbstractOdometry::corePtr()->isTrainStandStill() && (!isModeButtonAlreadyPressed))
      {
        DS::AbstractTargets::corePtr()->removeTargetsAtStandStillInLocation();
        DS::AbstractTargets::corePtr()->setCurCeilingSpeed(maxAllowedSpeeInLoc);
      }
    }
    /******************************************************************************
    * isValidQRouteType
    ******************************************************************************/
    bool LocationMode::isValidQRouteType(const RouteType routeType) const
    {
      bool retFlag = false;

      if (RtNormal == routeType)
      {
        retFlag = true;
      }

      return retFlag;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void LocationMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&modeState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&maxAllowedSpeeInLoc));
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    LocationModeState LocationMode::getModeState() const
    {
      return modeState;
    }

    /******************************************************************************
    * handleGradInLocation
    ******************************************************************************/
    void LocationMode::handleGradInLocation(CommonDataForModes &commonData) const
    {
      //gradient should be updated only at standstill
      if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
      {
        const DS::BaseTarget* const locStartTarget = DS::AbstractTargets::corePtr()->getLocationStartTarget();
        const DS::BaseTarget* const locEndTarget = DS::AbstractTargets::corePtr()->getLocationEndTarget();

        if ((static_cast<DS::BaseTarget *>(NULL) != locStartTarget) &&
          (static_cast<DS::BaseTarget *>(NULL) != locEndTarget))
        {
          if (commonData.currentDrivDirection == locStartTarget->getDirection())
          {
            DS::AbstractTargets::corePtr()->setCurTrackGradient(locStartTarget->getLocGradValue());
            DS::AbstractTargets::corePtr()->setCurGradient(locStartTarget->getLocGradValue());
          }
          else if (commonData.currentDrivDirection == locEndTarget->getDirection())
          {
            DS::AbstractTargets::corePtr()->setCurTrackGradient(locEndTarget->getLocGradValue());
            DS::AbstractTargets::corePtr()->setCurGradient(locEndTarget->getLocGradValue());
          }
          else
          {
            // do nothing
          }
        }
      }
    }

  }
}
