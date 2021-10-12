/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the Sleeping mode class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-08   spandita    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_string.h>

#include "sleeping_mode.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_targets.hpp"
#include "abstract_loco_io.hpp"
#include "abstract_odometry.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_brake.hpp"
#include "abstract_cross_compare.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_tracks.hpp"
#include "abstract_mode_control_event_ids.hpp"
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
    * Constructor
    ******************************************************************************/
    SleepingMode::SleepingMode() : AbstractMode(),
        sleepingSignalInactiveStandstill(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdSleepingSignalInactive,
          ATC::NoSB, DMICom::sleepingSignalInActive, "Applying Standstill as ATP is sleeping and sleeping signal is inactive")),
        modeState(sleepingStart)
    {
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void SleepingMode::resetMode()
    {
      modeState = sleepingStart;
      AbstractMode::resetMode();
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void SleepingMode::handleMode(CommonDataForModes &commonData)
    {
      SleepingModeState oldModeState = modeState;
      handleCurrentDrivDirection(commonData);
      switch (modeState)
      {
      case sleepingStart:
        runSleepingStart(commonData);
        break;
      case sleepingWaitSleepingSigDeactiv:
        runSleepingWaitSleepingSigDeactive();
        break;
      case sleepingNotSleepingWaitStandstill:
          runSleepingNotSleepingWaitStandstill();
       break;
      case sleepingWaitNotSleeping:
          runSleepingWaitNotSleeping();
        break;
      case sleepingFinishConfig:
        runSleepingFinishConfig();
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
    * getModeName
    ******************************************************************************/
    ATPMode SleepingMode::getModeId()
    {
      return ATPModeSleeping;
    }

    /******************************************************************************
    * runSleepingStart
    ******************************************************************************/
    void SleepingMode::runSleepingStart(CommonDataForModes &commonData)
    {

      // Clearing MA Timeout and Train Idling 
      manageMATimeout(commonData);
      manageTrainIdling(commonData);

      // Clear any existing target data
      DS::AbstractTargets::corePtr()->removeAll();

      // Clear any existing track data
      DS::AbstractTracks::corePtr()->removeAll();

      // Remove train setup 
      DS::AbstractTSetup::corePtr()->removeTrainSetup();

      // Activate the AOS OK signal/status when in sleeping mode.
      commonData.atpOkStatus = true;

      modeState = sleepingWaitSleepingSigDeactiv;
    }

    /******************************************************************************
    * runSleepingWaitSleepingSigDeactive
    ******************************************************************************/
    void SleepingMode::runSleepingWaitSleepingSigDeactive()
    {
      if (!IO::AbstractLocoIO::corePtr()->getSleepingSignal())
      {
        modeState = sleepingNotSleepingWaitStandstill;
      }
    }

    /******************************************************************************
    * runSleepingNotSleepingWaitStandstill
    ******************************************************************************/
    void SleepingMode::runSleepingNotSleepingWaitStandstill()
    {
      //Check for standstill if the sleeping signal is Inactive
      if (!IO::AbstractLocoIO::corePtr()->getSleepingSignal())
      {
        if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
        {
          modeState = sleepingWaitNotSleeping;
        }
        else
        {
          // To please lint
        }
      }
      else
      {
        modeState = sleepingWaitSleepingSigDeactiv;
      }
    }

    /******************************************************************************
    * runSleepingWaitNotSleeping
    ******************************************************************************/
    void SleepingMode::runSleepingWaitNotSleeping()
    {
      // Sleeping Signal is not active and stand still is already detected in previous mode state
      ATC::AbstractEventHandler::corePtr()->reportEvent(sleepingSignalInactiveStandstill, __FILE__, __LINE__);

      if (IO::AbstractLocoIO::corePtr()->getSleepingSignal())
      {
        // Sleeping signal activated again
        modeState = sleepingWaitSleepingSigDeactiv;
      }
      else if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonTrainConfig)
      {
        // Configuration mode is confirmed from DMI -> Proceed to next state
        modeState = sleepingFinishConfig;
      }
      else
      {
        // Keep Lint happy
      }
    }

    /******************************************************************************
    * runSleepingFinishConfig
    ******************************************************************************/
    void SleepingMode::runSleepingFinishConfig()
    {
      setNextMode(ATPModeConfiguration);
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    SleepingModeState SleepingMode::getModeState() const
    {
      return modeState;
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool SleepingMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void SleepingMode::getModeStateString(const SleepingModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case sleepingStart:
        static_cast<void>(vfw_strlcpy(buffer, "sleepingStart", maxModeStateNameLength));
        break;

      case sleepingWaitSleepingSigDeactiv:
        static_cast<void>(vfw_strlcpy(buffer, "sleepingWaitSleepingSigDeactiv", maxModeStateNameLength));
        break;
      
      case sleepingNotSleepingWaitStandstill:
        static_cast<void>(vfw_strlcpy(buffer, "sleepingNotSleepingWaitStandstill", maxModeStateNameLength));
        break;

      case sleepingWaitNotSleeping:
        static_cast<void>(vfw_strlcpy(buffer, "sleepingWaitNotSleeping", maxModeStateNameLength));
        break;

      case sleepingFinishConfig:
        static_cast<void>(vfw_strlcpy(buffer, "sleepingFinishConfig", maxModeStateNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
        break;
      }
    }

    /******************************************************************************
    * manageIdling
    ******************************************************************************/
    void SleepingMode::manageTrainIdling(CommonDataForModes &commonData)
    {
      commonData.idling = false;
    }


    /******************************************************************************
    * manageMATimeoutState
    ******************************************************************************/
    void SleepingMode::manageMATimeout(CommonDataForModes &commonData)
    {
      commonData.maTimeOut = false;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void SleepingMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&modeState));
    }
  }
}

