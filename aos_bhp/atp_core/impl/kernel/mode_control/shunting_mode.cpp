/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the Shunting mode class.
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
#include <vfw_string.h>

#include "shunting_mode.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_tracks.hpp"
#include "abstract_targets.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_cross_compare.hpp"
#include "abstract_tsetup.hpp"

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
    ShuntingMode::ShuntingMode() : AbstractMode(),
      modeState(shuntingStart)
    {
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void ShuntingMode::resetMode()
    {
      modeState = shuntingStart;
      AbstractMode::resetMode();
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void ShuntingMode::handleMode(CommonDataForModes &commonData)
    {
      handleCurrentDrivDirection(commonData);
      ShuntingModeState oldModeState = modeState;

      // Run the function corresponding to the modeState.
      switch (modeState)
      {
      case shuntingStart:
        runShuntingStart(commonData);
        break;
      case shuntingWaitSleep:
        runShuntingWaitSleep();
        break;
      case shuntingWaitSleepConfirmDMI:
        runShuntingWaitSleepConfirmDMI();
        break;
      case shuntingFinishSleep:
        runShuntingFinishSleep();
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
    * runShuntingStart
    ******************************************************************************/
    void ShuntingMode::runShuntingStart(CommonDataForModes &commonData)
    {
      //Clearing MA Timeout and Train Idling 
      manageMATimeout(commonData);
      manageTrainIdling(commonData);
      //To deactivate the Abort Setup on DMI
      commonData.isAbortSetupActive = false;
      // Clear any existing track data
      DS::AbstractTracks::corePtr()->removeAll();
      //Copy the ceiling speed to local variable before deletion of target
      const uint32_t recCeilingSpeed = DS::AbstractTargets::corePtr()->getCurCeilingSpeed();

      // Clear any existing target data
      DS::AbstractTargets::corePtr()->removeAll();

      //Set the ceiling speed 
      DS::AbstractTargets::corePtr()->setCurCeilingSpeed(recCeilingSpeed);

      // Switch sub-mode for next cycle
      modeState = shuntingWaitSleep;
    }

    /******************************************************************************
    * runShuntingWaitSleep
    ******************************************************************************/
    void ShuntingMode::runShuntingWaitSleep()
    {
      /* Detect when to leave ShuntingMode */
      if (AbstractModeControl::corePtr()->isAllowedToEnterSleepMode())
      {
        modeState = shuntingWaitSleepConfirmDMI;
      }
    }

    /******************************************************************************
    * runShuntingWaitSleepConfirmDMI
    ******************************************************************************/
    void ShuntingMode::runShuntingWaitSleepConfirmDMI()
    {
      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonConfirmSleep)
      {
        // Sleep mode is confirmed from DMI -> Proceed to next state
        modeState = shuntingFinishSleep;
      }
      else if (!(IO::AbstractLocoIO::corePtr()->getSleepingSignal() && isStandStill))
      {
        // Continue in Shunting mode if sleeping signal is deactivated before driver confirmed mode change to Sleeping
        modeState = shuntingWaitSleep;
      }
      else
      {
        // Do Nothing
      }
    }
    /******************************************************************************
    * runShuntingFinishSleep
    ******************************************************************************/
    void ShuntingMode::runShuntingFinishSleep()
    {
      setNextMode(ATPModeSleeping);
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    ShuntingModeState ShuntingMode::getModeState() const
    {
      return modeState;
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool ShuntingMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void ShuntingMode::getModeStateString(const ShuntingModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case shuntingStart:
        static_cast<void>(vfw_strlcpy(buffer, "shuntingStart", maxModeStateNameLength));
        break;

      case shuntingWaitSleep:
        static_cast<void>(vfw_strlcpy(buffer, "shuntingWaitSleep", maxModeStateNameLength));
        break;

      case shuntingWaitSleepConfirmDMI:
        static_cast<void>(vfw_strlcpy(buffer, "shuntingWaitSleepConfirmDMI", maxModeStateNameLength));
        break;

      case shuntingFinishSleep:
        static_cast<void>(vfw_strlcpy(buffer, "shuntingFinishSleep", maxModeStateNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
        break;
      }
    }

    /******************************************************************************
    * getModeName
    ******************************************************************************/
    ATPMode ShuntingMode::getModeId()
    {
      return ATPModeShunting;
    }

    /******************************************************************************
    * manageIdling
    ******************************************************************************/
    void ShuntingMode::manageTrainIdling(CommonDataForModes &commonData)
    {
      commonData.idling = false;
    }

    /******************************************************************************
    * manageMATimeoutState
    ******************************************************************************/
    void ShuntingMode::manageMATimeout(CommonDataForModes &commonData)
    {
      commonData.maTimeOut = false;
    }

    /******************************************************************************
    * handleCurrentDrivDirection
    ******************************************************************************/
    void ShuntingMode::handleCurrentDrivDirection(CommonDataForModes &commonData)
    {
      AbstractMode::handleCurrentDrivDirection(commonData);
    
      DS::AbstractTargets::corePtr()->setSupposedTravelDir(commonData.currentDrivDirection);
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void ShuntingMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&modeState));
    }
  }
}

