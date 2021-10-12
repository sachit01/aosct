/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the JoinMode class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-06-08    skothiya    Document create
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_string.h>

#include "join_mode.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_targets.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_mode_control_event_ids.hpp"

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
    JoinMode::JoinMode() : AbstractMode(),
      modeState(joinWaitJoinConfirmDMI),
      standstillWaitJoinConfirm(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdStandStillInJoin,
        ATC::NoSB, 0U, "Waiting for confirmation in Join mode")),
      joinCommandReceived(false)
    {
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void JoinMode::handleMode(CommonDataForModes &commonData)
    {
      JoinModeState oldModeState = modeState;

      // Check if un-registration message is received from TCC.
      if (!handleUnRegMessage())
      {
        //Remove passed tracks and targets
        removePassedObjects(commonData);
        manageTCCTimeOut();
        manageMATimeout(commonData);
        manageTrainIdling(commonData);
        manageStopTrain(commonData);
        handleCurrentDrivDirection(commonData);
        // Run the function corresponding to the modeState.
        switch (modeState)
        {
        case joinWaitJoinConfirmDMI:
          runJoinWaitJoinConfirmDMI();
          break;
        case joinInProgress:
          runJoinInProgress();
          break;
        case joinWaitSleepConfirmDMI:
          runJoinWaitSleepConfirmDMI();
          break;
        case joinFinishSleep:
          runJoinFinishSleep();
          break;
        case joinFinishConfig:
          runJoinFinishConfig();
          break;
        case joinWaitMAScratch:
          runJoinWaitMAScratch();
          break;
        case joinConfirmMAScratch:
          runJoinConfirmMAScratch();
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
      }
    }

    /******************************************************************************
    * runJoinWaitJoinConfirmDMI
    ******************************************************************************/
    void JoinMode::runJoinWaitJoinConfirmDMI()
    {
      // Raise Standstill event until confirmed from DMI.
      ATC::AbstractEventHandler::corePtr()->reportEvent(standstillWaitJoinConfirm, __FILE__, __LINE__);

      // If Join mode is confirmed from DMI -> Proceed to next state
      if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonConfirmJoin)
      {
        modeState = joinInProgress;
      }
    }

    /******************************************************************************
    * runJoinInProgress
    ******************************************************************************/
    void JoinMode::runJoinInProgress()
    {
      // If all sleep-criteria is full-filled -> Proceed to next state waiting for sleep confirmation
      if (AbstractModeControl::corePtr()->isAllowedToEnterSleepMode())
      {
        modeState = joinWaitSleepConfirmDMI;
      }
      // If Configuration mode is confirmed from DMI -> Proceed to configuration
      else if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonTrainConfig)
      {
        modeState = joinFinishConfig;
      }
      else if (AbstractModeControl::corePtr()->getIdleState())
      {
        modeState = joinWaitMAScratch;
      }
      else
      {
        // Do nothing
      }
    }

    /******************************************************************************
    * runJoinWaitSleepConfirmDMI
    ******************************************************************************/
    void JoinMode::runJoinWaitSleepConfirmDMI()
    {
      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonConfirmSleep)
      {
          // Sleep mode is confirmed from DMI -> Proceed to next state
        modeState = joinFinishSleep;
      }
      else if (!(IO::AbstractLocoIO::corePtr()->getSleepingSignal() && isStandStill))
      {
          // Continue in join mode if sleeping signal is deactivated before mode changed to Sleeping
        modeState = joinInProgress;
      }
      else
      {
        // Do Nothing
      }
    }

    /******************************************************************************
    * runJoinFinishSleep
    ******************************************************************************/
    void JoinMode::runJoinFinishSleep()
    {
      setNextMode(ATPModeSleeping);
    }

    /******************************************************************************
    * runJoinFinishConfig
    ******************************************************************************/
    void JoinMode::runJoinFinishConfig()
    {
      setNextMode(ATPModeConfiguration);
    }

    /******************************************************************************
    * runJoinWaitMAScratch
    ******************************************************************************/
    void JoinMode::runJoinWaitMAScratch()
    {
      // Check if the MA received is with Q_ROUTE_TYPE Join
      MAHead mHead;

      if (AbstractMessageHandler::corePtr()->getMAHead(mHead))
      {
        if (RtJoin == mHead.routeType)
        {
          // Switch to next state for confirmation on DMI
          modeState = joinConfirmMAScratch;
          
          // Standstill event issued to avoid one cycle delay until driver confirms 
          ATC::AbstractEventHandler::corePtr()->reportEvent(standstillWaitJoinConfirm,
            __FILE__, __LINE__);
        }
      }
      else if (AbstractModeControl::corePtr()->isAllowedToEnterSleepMode())
      {
        modeState = joinWaitSleepConfirmDMI;
      }
      else
      {
        // Nothing
      }
    }

    /******************************************************************************
    * runJoinConfirmMAScratch
    ******************************************************************************/
    void JoinMode::runJoinConfirmMAScratch()
    {
      // Issue a standstill event until driver confirms
      ATC::AbstractEventHandler::corePtr()->reportEvent(standstillWaitJoinConfirm,
        __FILE__, __LINE__);

      if (DMICom::DMIButtonConfirmJoinMA == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus())
      {
        modeState = joinInProgress;
      }
    }

    /******************************************************************************
    * getModeName
    ******************************************************************************/
    ATPMode JoinMode::getModeId()
    {
      return ATPModeJoin;
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool JoinMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    JoinModeState JoinMode::getModeState() const
    {
      return modeState;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void JoinMode::getModeStateString(const JoinModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case joinWaitJoinConfirmDMI:
        static_cast<void>(vfw_strlcpy(buffer, "joinWaitJoinConfirmDMI", maxModeStateNameLength));
        break;

      case joinInProgress:
        static_cast<void>(vfw_strlcpy(buffer, "joinInProgress", maxModeStateNameLength));
        break;

      case joinWaitSleepConfirmDMI:
        static_cast<void>(vfw_strlcpy(buffer, "joinWaitSleepConfirmDMI", maxModeStateNameLength));
        break;

      case joinFinishSleep:
        static_cast<void>(vfw_strlcpy(buffer, "joinFinishSleep", maxModeStateNameLength));
        break;

      case joinFinishConfig:
        static_cast<void>(vfw_strlcpy(buffer, "joinFinishConfig", maxModeStateNameLength));
        break;

      case joinWaitMAScratch:
        static_cast<void>(vfw_strlcpy(buffer, "joinWaitMAScratch", maxModeStateNameLength));
        break;

      case joinConfirmMAScratch:
        static_cast<void>(vfw_strlcpy(buffer, "joinConfirmMAScratch", maxModeStateNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
        break;
      }
    }

    /******************************************************************************
    * isValidQRouteType
    ******************************************************************************/
    bool JoinMode::isValidQRouteType(const RouteType routeType) const
    {
      bool retFlag = false;

      switch (routeType)
      {
      case RtJoin:
        // The MA shall only be accepted if a Join Command has not been received.
        retFlag = !joinCommandReceived;
        break;

      case RtNormal:
      case RtShuntingRoute:
      case RtStaffResponsible:
      case RtSplit:
      case RtReRegistration:
      case RtUndefined:
        break;

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
    * initCrossCompare
    ******************************************************************************/
    void JoinMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&modeState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&standstillWaitJoinConfirm));
    }
    
    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void JoinMode::resetMode()
    {
      char_t buffer[maxModeStateNameLength];
      getModeStateString(modeState, &buffer[0]);
      modeState = joinWaitJoinConfirmDMI;
      joinCommandReceived = AbstractMessageHandler::corePtr()->getJoinCommand();
      AbstractMode::resetMode();
      AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
      AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
    }

    /******************************************************************************
    * isValidUncondShorteningMsg
    ******************************************************************************/
    bool JoinMode::isValidUncondShorteningMsg() const
    {
      return true;
    }
  }
}
