/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the Possession mode class.
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
#include "possession_mode.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_tracks.hpp"
#include "abstract_targets.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_cross_compare.hpp"
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
    PossessionMode::PossessionMode() : AbstractMode(),
      modeState(possessionModeStart)
    {

    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void PossessionMode::handleMode(CommonDataForModes &commonData)
    {
      PossessionModeState oldModeState = modeState;
      handleCurrentDrivDirection(commonData);
      
      // Run the function corresponding to the modeState.
      switch (modeState)
      {
      case possessionModeStart:
        runPossessionModeStart(commonData);
        break;

      case possessionModeFinish:
        runPossessionModeFinish();
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
    * resetMode
    ******************************************************************************/
    void PossessionMode::resetMode()
    {
      char_t buffer[maxModeStateNameLength];
      getModeStateString(modeState, &buffer[0]);

      modeState = possessionModeStart;
      AbstractMode::resetMode();
      AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
      AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
    }

    /******************************************************************************
    * getModeName
    ******************************************************************************/
    ATPMode PossessionMode::getModeId()
    {
      return ATPModePossession;
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    PossessionModeState PossessionMode::getModeState() const
    {
      return modeState;
    }
    /******************************************************************************
    * manageIdling
    ******************************************************************************/
    void PossessionMode::manageTrainIdling(CommonDataForModes &commonData)
    {
      commonData.idling = false;
    }

    /******************************************************************************
    * handleCurrentDrivDirection
    ******************************************************************************/
    void PossessionMode::handleCurrentDrivDirection(CommonDataForModes &commonData)
    {
      AbstractMode::handleCurrentDrivDirection(commonData);

      DS::AbstractTargets::corePtr()->setSupposedTravelDir(commonData.currentDrivDirection);
    }

    /******************************************************************************
    * runPossessionModeStart
    ******************************************************************************/
    void PossessionMode::runPossessionModeStart(CommonDataForModes &commonData)
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
      
      //Set the ceiling speed of possession
      DS::AbstractTargets::corePtr()->setCurCeilingSpeed(recCeilingSpeed);
      modeState = possessionModeFinish;
    }

    /******************************************************************************
    * runPossessionModeFinish
    ******************************************************************************/
    void PossessionMode::runPossessionModeFinish()
    {
      const ModeRequestSeqState configModeRequestSeqState = AbstractModeControl::corePtr()->getConfigModReqSeqState();
      const ModeRequestSeqState shuntModeRequestSeqState = AbstractModeControl::corePtr()->getShuntModReqSeqState();
      const ModeRequestSeqState yardModeRequestSeqState = AbstractModeControl::corePtr()->getYardModReqSeqState();

      const bool isConfigButtonValid = ConfigModeRequestSeq::configDmiButtonConfirmed == configModeRequestSeqState;
      const bool isShuntButtonValid = ShuntModeRequestSeq::shuntDmiButtonConfirmed == shuntModeRequestSeqState;
      const bool isYardButtonValid = (YardModeRequestSeq::yardConfirmed == yardModeRequestSeqState)
        || (YardModeRequestSeq::YardDmiAckManualConfirm == yardModeRequestSeqState);

      if (isConfigButtonValid || isShuntButtonValid || isYardButtonValid)
      {
        DS::AbstractTracks::corePtr()->clearPossessionBaliseList();
      }
    }
    /******************************************************************************
    * manageMATimeoutState
    ******************************************************************************/
    void PossessionMode::manageMATimeout(CommonDataForModes &commonData)
    {
      commonData.maTimeOut = false;
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool PossessionMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void PossessionMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&modeState));
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void PossessionMode::getModeStateString(const PossessionModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case possessionModeStart:
        static_cast<void>(vfw_strlcpy(buffer, "possessionModeStart", maxModeStateNameLength));
        break;

      case possessionModeFinish:
        static_cast<void>(vfw_strlcpy(buffer, "possessionModeFinish", maxModeStateNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
        break;
      }
    }

  }
}

