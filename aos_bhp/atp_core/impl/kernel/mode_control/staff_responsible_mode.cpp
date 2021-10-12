/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the Staff Responsible mode class.
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
#include "staff_responsible_mode.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_targets.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_dmi_handler.hpp"
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
    StaffResponsibleMode::StaffResponsibleMode() : AbstractMode(),
      standStillWaitSRConfirm(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdStandStillInStaffResp,
        ATC::ATOSB, 0x0U, "Waiting for confirmation in Staff Responsible mode"))
    {
      modeState = staffResponsibleStart;
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void StaffResponsibleMode::handleMode(CommonDataForModes &commonData)
    {
      //Check if unregistration message is received from TCC.
      if (!handleUnRegMessage())
      {
        //Remove passed tracks and targets
        removePassedObjects(commonData);

        manageTCCTimeOut();
        //check for MA timeout
        manageMATimeout(commonData);
        //check for Idling
        manageTrainIdling(commonData);
        //Checking for stop train
        manageStopTrain(commonData);
        handleCurrentDrivDirection(commonData);
        StaffResponsibleModeState oldModeState = modeState;

        //run the function corresponding to the modeState.
        switch (modeState)
        {
        case staffResponsibleStart:
          runStaffResponsibleStart(commonData);
          break;
        case staffResponsibleWaitConfirmDMI:
          runStaffResponsibleWaitConfirmDMI(commonData);
          break;
        case staffResponsibleWaitMA:
          runStaffResponsibleWaitMA();
          break;
        case staffResponsibleConfirmMAScratch:
          runStaffResponsibleConfirmMAScratch();
          break;
        case staffResponsibleWaitMAScratch:
          runStaffResponsibleWaitMAScratch();
          break;
        case staffResponsibleFinishOk:
          runStaffResponsibleFinishOk();
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
    }

    /******************************************************************************
    * getModeName
    ******************************************************************************/
    ATPMode StaffResponsibleMode::getModeId()
    {
      return ATPModeStaffResponsible;
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool StaffResponsibleMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void StaffResponsibleMode::getModeStateString(const StaffResponsibleModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case staffResponsibleStart:
        static_cast<void>(vfw_strlcpy(buffer, "staffResponsibleStart", maxModeStateNameLength));
        break;

      case staffResponsibleWaitConfirmDMI:
        static_cast<void>(vfw_strlcpy(buffer, "staffResponsibleWaitConfirmDMI", maxModeStateNameLength));
        break;

      case staffResponsibleWaitMA:
        static_cast<void>(vfw_strlcpy(buffer, "staffResponsibleWaitMA", maxModeStateNameLength));
        break;

      case staffResponsibleConfirmMAScratch:
        static_cast<void>(vfw_strlcpy(buffer, "staffResponsibleConfirmMAScratch", maxModeStateNameLength));
        break;

      case staffResponsibleWaitMAScratch:
        static_cast<void>(vfw_strlcpy(buffer, "staffResponsibleWaitMAScratch", maxModeStateNameLength));
        break;

      case staffResponsibleFinishOk:
        static_cast<void>(vfw_strlcpy(buffer, "staffResponsibleFinishOk", maxModeStateNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
        break;
      }
    }

    /******************************************************************************
    * runStaffResponsibleStart
    ******************************************************************************/
    void StaffResponsibleMode::runStaffResponsibleStart(CommonDataForModes &commonData)
    {
            //To deactivate the Abort Setup on DMI
      commonData.isAbortSetupActive = false;
      //keep reporting event to apply SB
      ATC::AbstractEventHandler::corePtr()->reportEvent(standStillWaitSRConfirm,
        __FILE__, __LINE__);
      // Need to request driver
      modeState = staffResponsibleWaitConfirmDMI;

    }

    /******************************************************************************
    * runStaffResponsibleWaitConfirmDMI
    ******************************************************************************/
    void StaffResponsibleMode::runStaffResponsibleWaitConfirmDMI(CommonDataForModes &commonData)
    {
      //Issue a standstill event until driver confirms
      ATC::AbstractEventHandler::corePtr()->reportEvent(standStillWaitSRConfirm,
        __FILE__, __LINE__);
      if (DMICom::DMIButtonConfirmStaffResponsible == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus())
      {
        if (commonData.idling)
        {
          modeState = staffResponsibleWaitMAScratch;
        }
        else
        {
          modeState = staffResponsibleWaitMA;
        }
      }
    }

    /******************************************************************************
    * runStaffResponsibleWaitMA
    ******************************************************************************/
    void StaffResponsibleMode::runStaffResponsibleWaitMA()
    {
      MAHead mHead;
      if (AbstractMessageHandler::corePtr()->getMAHead(mHead))
      {
        if ((AbstractMessageHandler::corePtr()->isMAFromScratch()) && (RtStaffResponsible == mHead.routeType))
        {
          modeState = staffResponsibleConfirmMAScratch;
        }
        else if (RtNormal == mHead.routeType)
        {
          modeState = staffResponsibleFinishOk;
        }
        else
        {
          // do nothing
        }
      }
    }

    /******************************************************************************
    * runStaffResponsibleConfirmMAScratch
    ******************************************************************************/
    void StaffResponsibleMode::runStaffResponsibleConfirmMAScratch()
    {
      //Issue a standstill event until driver confirms
      ATC::AbstractEventHandler::corePtr()->reportEvent(standStillWaitSRConfirm,
        __FILE__, __LINE__);
      if (DMICom::DMIButtonConfirmStaffResponsibleMA == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus())
      {
        modeState = staffResponsibleWaitMA;
      }
    }

    /******************************************************************************
    * runStaffResponsibleWaitMAScratch
    ******************************************************************************/
    void StaffResponsibleMode::runStaffResponsibleWaitMAScratch()
    {
      MAHead mHead;
      if (AbstractMessageHandler::corePtr()->getMAHead(mHead))
      {
        if (RtStaffResponsible == mHead.routeType)
        {
          //Switch to next mode state
          modeState = staffResponsibleConfirmMAScratch;

          //Issue a standstill event to avoid one cycle delay until driver confirms
          ATC::AbstractEventHandler::corePtr()->reportEvent(standStillWaitSRConfirm,
            __FILE__, __LINE__);
        }
        else if (RtNormal == mHead.routeType)
        {
          modeState = staffResponsibleFinishOk;
        }
        else
        {
          //do nothing
        }
      }
    }

    /******************************************************************************
    * runStaffResponsibleFinishOk
    ******************************************************************************/
    void StaffResponsibleMode::runStaffResponsibleFinishOk()
    {
      setNextMode(ATPModeNormal);
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void StaffResponsibleMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&modeState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&standStillWaitSRConfirm));
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    StaffResponsibleModeState StaffResponsibleMode::getModeState() const
    {
      return modeState;
    }

    /******************************************************************************
    * isValidQRouteType
    ******************************************************************************/
    bool StaffResponsibleMode::isValidQRouteType(const RouteType routeType) const
    {
      bool retFlag = false;

      Pos::PosAccuracyState currAccuracyState = Pos::AbstractPosition::corePtr()->getAccuracyState();
      switch (routeType)
      {
      case RtNormal:

        if (Pos::PosKnown == currAccuracyState)
        {
          retFlag = true;
        }
        break;

      case RtStaffResponsible:
        
        if ((Pos::PosApprox == currAccuracyState) || (Pos::PosKnown == currAccuracyState))
        {
          retFlag = true;
        }
        break;
      case RtSplit:
      case RtJoin:
      case RtReRegistration:
      case RtShuntingRoute:
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
    * resetMode
    ******************************************************************************/
    void StaffResponsibleMode::resetMode()
    {
      char_t buffer[maxModeStateNameLength];
      getModeStateString(modeState, &buffer[0]);
      modeState = staffResponsibleStart;
      AbstractMode::resetMode();
      AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
      AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
    }

    /******************************************************************************
    * isValidUncondShorteningMsg
    ******************************************************************************/
    bool StaffResponsibleMode::isValidUncondShorteningMsg() const
    {
      return true;
    }
  }
}

