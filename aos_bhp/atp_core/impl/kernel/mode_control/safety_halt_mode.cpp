/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the SafetyHalt class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-12-02    nsyed    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "safety_halt_mode.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_tracks.hpp"
#include "abstract_targets.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_loco_io.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_mode_control_event_ids.hpp"
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
    SafetyHaltMode::SafetyHaltMode() : AbstractMode(),
      safetyHaltModeEvent(ATC::Event::createSafetyHaltEvent(atpModeControlId, ATC::CoreContainer, eventIdSafetyHaltEvent, ATC::NoEB,
        DMICom::modSafetyHaltEB, "Safety Halt in Safety Halt mode.")),
      modeState(safetyHaltStart)
    {
      toggleWaitCycle = 0U;
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void SafetyHaltMode::resetMode()
    {
      modeState = safetyHaltStart;
      toggleWaitCycle = 0U;
      AbstractMode::resetMode();
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void SafetyHaltMode::handleMode(CommonDataForModes &commonData)
    {
      //raise sb event to restrict movement
      ATC::AbstractEventHandler::corePtr()->reportEvent(safetyHaltModeEvent,
        __FILE__, __LINE__);

      //Clearing MA Timeout and Train Idling 
      manageMATimeout(commonData);
      manageTrainIdling(commonData);
      handleCurrentDrivDirection(commonData);
      SafetyHaltModeState oldModeState = modeState;
      // Run the function corresponding to the modeState.
      switch (modeState)
      {
      case safetyHaltStart:
        runSafetyHaltStart(commonData);
        break;

      case safetyHaltWaitForPowerDownOrRestart:
        runSafetyHaltWaitForPowerDownOrRestart(commonData);
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
    * runSafetyHaltStart
    ******************************************************************************/
    void SafetyHaltMode::runSafetyHaltStart(CommonDataForModes &commonData)
    {
      //To deactivate the Abort Setup on DMI
      commonData.isAbortSetupActive = false;

      // Clear any existing target data
      DS::AbstractTargets::corePtr()->removeAll();

      // Clear any existing track data
      DS::AbstractTracks::corePtr()->removeAll();

      //Setting buzzer according to priority
      commonData.buzzer = (commonData.buzzer > BuzzerTypeFor30Sec) ? commonData.buzzer : BuzzerTypeFor30Sec;

      modeState = safetyHaltWaitForPowerDownOrRestart;
    }

    /******************************************************************************
    * runSafetyHaltWaitForPowerDownOrRestart
    ******************************************************************************/
    void SafetyHaltMode::runSafetyHaltWaitForPowerDownOrRestart(CommonDataForModes &commonData)
    {
      if (maxTimeToggleAOSStatus > toggleWaitCycle)
      {
        //Toggle AOS Status output after 10 cycle (0.5hz and 50% duty cycle)
        if (0U == (toggleWaitCycle % toggleFrequency))
        {
          //Toggle the AOS Status 
          commonData.atpLampStatus = !(commonData.atpLampStatus);
        }
          //increment the toggle cycle
        ++toggleWaitCycle;
      }
      else
      {
        //After 5 min AOS status should be off
        commonData.atpLampStatus = false;
      }
    }

    /******************************************************************************
    * getModeName
    ******************************************************************************/
    ATPMode SafetyHaltMode::getModeId()
    {
      return ATPModeSafetyHalt;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void SafetyHaltMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&safetyHaltModeEvent));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&toggleWaitCycle));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<SafetyHaltModeState>(&modeState));
    }

    /******************************************************************************
    * manageIdling
    ******************************************************************************/
    void SafetyHaltMode::manageTrainIdling(CommonDataForModes &commonData)
    {
      commonData.idling = false;
    }

    /******************************************************************************
    * manageMATimeoutState
    ******************************************************************************/
    void SafetyHaltMode::manageMATimeout(CommonDataForModes &commonData)
    {
      commonData.maTimeOut = false;
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool SafetyHaltMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void SafetyHaltMode::getModeStateString(const SafetyHaltModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case safetyHaltStart:
        static_cast<void>(vfw_strlcpy(buffer, "safetyHaltStart", maxModeStateNameLength));
        break;

      case safetyHaltWaitForPowerDownOrRestart:
        static_cast<void>(vfw_strlcpy(buffer, "safetyHaltWaitForPowerDownOrRestart", maxModeStateNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
        break;
      }
    }
  }
}
