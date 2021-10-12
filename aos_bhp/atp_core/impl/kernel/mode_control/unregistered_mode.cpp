/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the UnregisteredMode class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-03    arastogi    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_string.h>
#include "unregistered_mode.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_tracks.hpp"
#include "abstract_targets.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_mode_control_event_ids.hpp"
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
    UnregisteredMode::UnregisteredMode() : AbstractMode(),
      // creating different set of objects for different type of events
      standstillInUnregisteredMode(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdStandstillInUnregistered,
        ATC::NoSB, DMICom::modChangeToUnreg, "Stand Still in unregistered Mode!"))
    {
      modeState = unregisteredStart;
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void UnregisteredMode::resetMode()
    {
      modeState = unregisteredStart;
      AbstractMode::resetMode();
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void UnregisteredMode::handleMode(CommonDataForModes &commonData)
    {
      UnregisteredModeState oldModeState = modeState;

      //Clearing MA Timeout and Train Idling 
      manageMATimeout(commonData);
      manageTrainIdling(commonData);
      handleCurrentDrivDirection(commonData);
      //To deactivate the Abort Setup on DMI
      commonData.isAbortSetupActive = false;
      //raise standstill event to restrict movement
      ATC::AbstractEventHandler::corePtr()->reportEvent(standstillInUnregisteredMode, __FILE__, __LINE__);

      // Run the function corresponding to the modeState.
      switch (modeState)
      {
        case unregisteredStart:
          runUnregisteredStart();
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
    ATPMode UnregisteredMode::getModeId()
    {
      return ATPModeUnregistered;
    }

    /******************************************************************************
    * manageIdling
    ******************************************************************************/
    void UnregisteredMode::manageTrainIdling(CommonDataForModes &commonData)
    {
      commonData.idling = false;
    }

    /******************************************************************************
    * manageMATimeoutState
    ******************************************************************************/
    void UnregisteredMode::manageMATimeout(CommonDataForModes &commonData)
    {
      commonData.maTimeOut = false;
    }

    /******************************************************************************
    * runUnregisteredStart
    ******************************************************************************/
    void UnregisteredMode::runUnregisteredStart()
    {
      //Removing train setup 
      if (DS::AbstractTSetup::corePtr()->isTrainSetupValid())
      {
        DS::AbstractTSetup::corePtr()->removeTrainSetup();
      }
      
      // Clear any existing track data
      if (!DS::AbstractTracks::corePtr()->isTrackListEmpty())
      {
        DS::AbstractTracks::corePtr()->removeAll();
      }
      
      if (!DS::AbstractTargets::corePtr()->isMATargetListEmpty())
      {
        // Clear any existing target data
        DS::AbstractTargets::corePtr()->removeAll();
      }
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool UnregisteredMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void UnregisteredMode::getModeStateString(const UnregisteredModeState state, char_t* const buffer) const
    {
      switch (state)
      {
        case unregisteredStart:
          static_cast<void>(vfw_strlcpy(buffer, "unregisteredStart", maxModeStateNameLength));
          break;

        default:
          static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
          break;
      }
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void UnregisteredMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&standstillInUnregisteredMode));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<UnregisteredModeState>(&modeState));
    }
  }
}
