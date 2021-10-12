/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the NormalMode class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-03    arastogi    Created
* 2017-06-14    skothiya    Updated for train states implementation
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "normal_mode.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tracks.hpp"
#include "abstract_targets.hpp"
#include "abstract_odometry.hpp"
#include "abstract_message_handler.hpp"

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
    NormalMode::NormalMode() : AbstractMode()
    {

    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void NormalMode::handleMode(CommonDataForModes &commonData)
    {
      //Check if un-registration message is received
      if (!handleUnRegMessage())
      {
        //check if the train is in location
        manageLocationModeValidity(commonData);
        //Remove passed tracks and targets
        removePassedObjects(commonData);
        //manage the TCC time out
        manageTCCTimeOut();
        //check for MA timeout
        manageMATimeout(commonData);
        //check for Idling
        manageTrainIdling(commonData);
        //Checking for stop train
        manageStopTrain(commonData);
        //Check for FreeRolling status
        manageFreeRolling(commonData);
        //Check for OdometerIvalid status
        manageOdometerInvalid(commonData);
        //Check for handling status
        manageHandlingDone(commonData);
        //To manage direction
        handleCurrentDrivDirection(commonData);
        //To deactivate the Abort Setup on DMI
        commonData.isAbortSetupActive = false;
        // Check if MA has received then Change Mode according to Q_ROUTE_TYPE
        ATPMode newModeToChange = ATPModeUndefined;

        if (handleModeChangeRequest(newModeToChange, commonData))
        {
          //Clearing the train states and other related flags
          commonData.freeRolling = false;
          commonData.handlingDone = false;
          commonData.isAllowedToEnterLocationMode = false;
          // Change to next mode
          setNextMode(newModeToChange);
        }

      }
    }

    /******************************************************************************
     * getModeId
     ******************************************************************************/
    ATPMode NormalMode::getModeId()
    {
      return ATPModeNormal;
    }

    /******************************************************************************
    * handleModeChangeRequest
    ******************************************************************************/
    bool NormalMode::handleModeChangeRequest(ATPMode& mode, CommonDataForModes& commonData) const
    {
      bool modeChanged = true;

      const bool joinCommandReceived = AbstractMessageHandler::corePtr()->getJoinCommand();
      const ATOMode atoMode = AbstractModeControl::corePtr()->getATOMode();

      if (isModeChangedByMA(mode))
      {
        // Do nothing (handled already)
      }
      else if (commonData.idling && joinCommandReceived && (ATOModeManual == atoMode))
      {
        mode = ATPModeJoin;
      }
      else if (commonData.isAllowedToEnterLocationMode)
      {
        commonData.currentDrivDirection = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
        mode = ATPModeLocation;
      }
      else
      {
        modeChanged = false;
      }

      return modeChanged;
    }

    /******************************************************************************
    * isValidQRouteType
    ******************************************************************************/
    bool NormalMode::isValidQRouteType(const RouteType routeType) const
    {
      bool retFlag = false;

      switch (routeType)
      {
      case RtNormal:
      {
        retFlag = true;
        break;
      }
      case RtStaffResponsible:
      case RtShuntingRoute:
      case RtSplit:
      case RtJoin:
      {
        if (AbstractModeControl::corePtr()->getIdleState())
        {
          retFlag = true;
        }
        break;
      }
      case RtReRegistration:
      case RtUndefined:
      {
        retFlag = false;
        break;
      }
      default:
      {
        retFlag = false;
        AbstractModeControl::corePtr()->getTrace()->write(2U, "Invalid Q Route Type !");
        break;
      }
      }
      return retFlag;
    }

    /******************************************************************************
    * isValidUncondShorteningMsg
    ******************************************************************************/
    bool NormalMode::isValidUncondShorteningMsg() const
    {
      return true;
    }
  }
}
