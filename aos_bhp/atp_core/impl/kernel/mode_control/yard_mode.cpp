/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the Yard mode class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-29    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_string.h>

#include "yard_mode.hpp"
#include "abstract_config.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_odometry.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_config_base.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_tracks.hpp"
#include "abstract_targets.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_radio_handler.hpp"
#include "abstract_cross_compare.hpp"


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
    YardMode::YardMode() : AbstractMode(),
      modeState(yardStart),
      prevTravelDirection(DirUndefined)
    {
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void YardMode::resetMode()
    {
      modeState = yardStart;
      AbstractMode::resetMode();
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void YardMode::handleMode(CommonDataForModes &commonData)
    {
      YardModeState oldModeState = modeState;
      handleCurrentDrivDirection(commonData);
      // Run the function corresponding to the modeState.
      switch (modeState)
      {
      case yardStart:
        runYardStart(commonData);
        break;
      case yardWaitConfigOrSleep:
        runYardWaitConfigOrSleep();
        break;
      case yardWaitSleepConfirmDMI:
        runYardWaitSleepConfirmDMI();
        break;
      case yardFinishConfig:
        runYardFinishConfig();
        break;
      case yardFinishSleep:
        runYardFinishSleep();
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
    * runYardStart
    ******************************************************************************/
    void YardMode::runYardStart(CommonDataForModes &commonData)
    {
      //To deactivate the Abort Setup on DMI
      commonData.isAbortSetupActive = false;
      // Clearing MA Timeout and Train Idling 
      manageMATimeout(commonData);
      manageTrainIdling(commonData);
      // Clear any existing track data
      DS::AbstractTracks::corePtr()->removeAll();
      //Copy the ceiling speed to local variable before deletion of target
      const uint32_t recCeilingSpeed = DS::AbstractTargets::corePtr()->getCurCeilingSpeed();

      // Clear any existing target data
      DS::AbstractTargets::corePtr()->removeAll();

      DS::AbstractTargets::corePtr()->setCurCeilingSpeed(recCeilingSpeed);

      // Switch sub-mode for next cycle
      modeState = yardWaitConfigOrSleep;
    }

    /******************************************************************************
    * runYardWaitConfigOrSleep
    ******************************************************************************/
    void YardMode::runYardWaitConfigOrSleep()
    {
      /* Detect when to leave YardMode */
      if (AbstractModeControl::corePtr()->isAllowedToEnterSleepMode())
      {
        modeState = yardWaitSleepConfirmDMI;
      }
      else if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonTrainConfig)
      {
        modeState = yardFinishConfig;
      }
      else
      {
        // Do Nothing
      }
    }

    /******************************************************************************
    * runYardWaitSleepConfirmDMI
    ******************************************************************************/
    void YardMode::runYardWaitSleepConfirmDMI()
    {
      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonConfirmSleep)
      {
        // Sleep mode is confirmed from DMI -> Proceed to next state
        modeState = yardFinishSleep;
      }
      else if (!(IO::AbstractLocoIO::corePtr()->getSleepingSignal() && isStandStill))
      {
        // Continue in Yard mode if sleeping signal is deactivated before driver confirmed mode change to Sleeping
        modeState = yardWaitConfigOrSleep;
      }
      else
      {
        // Do Nothing
      }
    }


    /******************************************************************************
    * runYardFinishConfig
    ******************************************************************************/
    void YardMode::runYardFinishConfig()
    {
      setNextMode(ATPModeConfiguration);
    }

    /******************************************************************************
    * runYardFinishSleep
    ******************************************************************************/
    void YardMode::runYardFinishSleep()
    {
      setNextMode(ATPModeSleeping);
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    YardModeState YardMode::getModeState() const
    {
      return modeState;
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool YardMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void YardMode::getModeStateString(const YardModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case yardStart:
        static_cast<void>(vfw_strlcpy(buffer, "yardStart", maxModeStateNameLength));
        break;

      case yardWaitConfigOrSleep:
        static_cast<void>(vfw_strlcpy(buffer, "yardWaitConfigOrSleep", maxModeStateNameLength));
        break;

      case yardWaitSleepConfirmDMI:
        static_cast<void>(vfw_strlcpy(buffer, "yardWaitSleepConfirmDMI", maxModeStateNameLength));
        break;

      case yardFinishConfig:
        static_cast<void>(vfw_strlcpy(buffer, "yardFinishConfig", maxModeStateNameLength));
        break;

      case yardFinishSleep:
        static_cast<void>(vfw_strlcpy(buffer, "yardFinishSleep", maxModeStateNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
        break;
      }
    }


    /******************************************************************************
    * getModeName
    ******************************************************************************/
    ATPMode YardMode::getModeId()
    {
      return ATPModeYard;
    }

    /******************************************************************************
    * manageIdling
    ******************************************************************************/
    void YardMode::manageTrainIdling(CommonDataForModes &commonData)
    {
      commonData.idling = false;
    }

    /******************************************************************************
    * manageMATimeoutState
    ******************************************************************************/
    void YardMode::manageMATimeout(CommonDataForModes &commonData)
    {
      commonData.maTimeOut = false;
    }

    /******************************************************************************
    * handleCurrentDrivDirection
    ******************************************************************************/
    void YardMode::handleCurrentDrivDirection(CommonDataForModes &commonData)
    {
      AbstractMode::handleCurrentDrivDirection(commonData);

      DS::AbstractTargets::corePtr()->setSupposedTravelDir(commonData.currentDrivDirection);
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void YardMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&modeState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TravelDir>(&prevTravelDirection));
    }
  }
}

