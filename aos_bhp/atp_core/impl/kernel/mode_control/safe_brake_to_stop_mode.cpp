/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the SafeBrakeToStopMode class.
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

#include "safe_brake_to_stop_mode.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_event_handler.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_targets.hpp"
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
    SafeBrakeToStopMode::SafeBrakeToStopMode() : AbstractMode(),
      modeState(safeBrakeToStopStart),
      sbInSafeBrakeToStopMode(ATC::Event::createSBReqEvent(atpModeControlId, ATC::CoreContainer, eventIdSBinSafeBrake,
        ATC::NoSB, DMICom::sbInSafeBrake, "Service Brake in Safe Brake to Stop mode.")),
      safetyHaltInvalidTSetup(ATC::Event::createSafetyHaltEvent(atpModeControlId, ATC::CoreContainer, eventIdInvalidTSetupInSafeBrkToStp,
        ATC::NoEB, DMICom::modSBToStopInvalidTSetup, "Safety Halt From Safe Brake To Stop mode, train setup not present"))
    {
    
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void SafeBrakeToStopMode::handleMode(CommonDataForModes &commonData)
    {

      //To deactivate the Abort Setup on DMI
      commonData.isAbortSetupActive = false;

      SafeBrakeToStopModeState oldModeState = modeState;

      // Check if the Unregistration message is received from TCC.
      if(!handleUnRegMessage())
      {
        // Clearing MA Timeout and Train Idling 
        manageMATimeout(commonData);
        manageTrainIdling(commonData);
        handleCurrentDrivDirection(commonData);

        // Remove passed tracks and targets
        removePassedObjects(commonData);

        switch (modeState)
        {
        case safeBrakeToStopStart:
          runSafeBrakeToStopStart();
          break;
        case safeBrakeToStopWaitMA:
          runSafeBrakeToStopWaitMA();
          break;
        case safeBrakeToStopFinishOk:
          runSafeBrakeToStopFinishOk();
          break;
        default:
          break;
        }

        // If Mode state has changed
        if (oldModeState != modeState)
        {
          char_t buffer[maxModeStateNameLength];
          getModeStateString(modeState, &buffer[0]);

          AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
          AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
        }
        
        // Raise SB event to restrict movement
        ATC::AbstractEventHandler::corePtr()->reportEvent(sbInSafeBrakeToStopMode,
          __FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * getModeName
    ******************************************************************************/
    ATPMode SafeBrakeToStopMode::getModeId()
    {
      return ATPModeSafeBrakeToStop;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void SafeBrakeToStopMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&sbInSafeBrakeToStopMode));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&safetyHaltInvalidTSetup));     
    }

    /******************************************************************************
    * manageMATimeoutState
    ******************************************************************************/
    void SafeBrakeToStopMode::manageMATimeout(CommonDataForModes &commonData)
    {
      commonData.maTimeOut = false;
    }

    /******************************************************************************
    * runSafeBrakeToStopStart
    ******************************************************************************/
    void SafeBrakeToStopMode::runSafeBrakeToStopStart()
    {
      // Remove all the targets when the train is at standstill
      DS::AbstractTargets::corePtr()->setDelTargetAtStandStill(true);

      // We have to have a valid train setup.
      if (DS::AbstractTSetup::corePtr()->getTrainSetup() == static_cast<const DS::TrainSetup*>(NULL))
      {
        // Raise safety halt event
        ATC::AbstractEventHandler::corePtr()->reportEvent(safetyHaltInvalidTSetup,
          __FILE__, __LINE__);
      }
      modeState = safeBrakeToStopWaitMA;
    }

    /******************************************************************************
    * runSafeBrakeToStopWaitMA
    ******************************************************************************/
    void SafeBrakeToStopMode::runSafeBrakeToStopWaitMA()
    {
      bool isSafeBrkToStopEvntActive = ATC::AbstractEventHandler::corePtr()->isModeChangeToSafeBrakeToStop();

      MAHead mHead;
      if ((AbstractMessageHandler::corePtr()->getMAHead(mHead)) && (!isSafeBrkToStopEvntActive))
      {
        if (RtStaffResponsible == mHead.routeType)
        {
          modeState = safeBrakeToStopFinishOk;
        }
      }
    }

    /******************************************************************************
    * runSafeBrakeToStopFinishOk
    ******************************************************************************/
    void SafeBrakeToStopMode::runSafeBrakeToStopFinishOk()
    {
      setNextMode(ATPModeStaffResponsible);
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool SafeBrakeToStopMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void SafeBrakeToStopMode::getModeStateString(const SafeBrakeToStopModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case safeBrakeToStopStart:
        static_cast<void>(vfw_strlcpy(buffer, "safeBrakeToStopStart", maxModeStateNameLength));
        break;

      case safeBrakeToStopFinishOk:
        static_cast<void>(vfw_strlcpy(buffer, "safeBrakeToStopFinishOk", maxModeStateNameLength));
        break;

      case safeBrakeToStopWaitMA:
        static_cast<void>(vfw_strlcpy(buffer, "safeBrakeToStopWaitMA", maxModeStateNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
        break;
      }
    }

    /******************************************************************************
    * isValidQRouteType
    ******************************************************************************/
    bool SafeBrakeToStopMode::isValidQRouteType(const RouteType routeType) const
    {
      bool retFlag = false;
      if (RtStaffResponsible == routeType)
      {
        bool isSafeBrkToStopEvntActive = ATC::AbstractEventHandler::corePtr()->isModeChangeToSafeBrakeToStop();
        
        // Check if Approximate message received and safe brake to stop is not active
        if ((AbstractMessageHandler::corePtr()->isApproxPosReceivedEarlier()) && (!isSafeBrkToStopEvntActive))
        {
          retFlag = true;
        }       
      }
      return retFlag;
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    SafeBrakeToStopModeState SafeBrakeToStopMode::getModeState() const
    {
      return modeState;
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void SafeBrakeToStopMode::resetMode()
    {
      char_t buffer[maxModeStateNameLength];
      getModeStateString(modeState, &buffer[0]);
      modeState = safeBrakeToStopStart;
      AbstractMode::resetMode();
      AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
      AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
    }
  }
}
