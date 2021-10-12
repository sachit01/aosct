/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of ShuntingRouteMode class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-04-27    skothiya    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "shunting_route_mode.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_mode_control_event_ids.hpp"
#include "dmi_event_codes.hpp"

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
    ShuntingRouteMode::ShuntingRouteMode() : AbstractMode(),
      standStillShuntingRouteConfirm(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer,
        eventIdStandStillShuntingRouteConfirmation, ATC::DriverSB, 0x0U, "Waiting for confirmation in Shunting Route mode")),
      unKnownStateInShuntRoute(ATC::Event::createEBReqEvent(atpModeControlId, ATC::CoreContainer,
        eventIdUnknownStateInShuntRoute, ATC::NoEB, DMICom::unknownStateInShunting, "Unknown State in Train Shunting Route Mode"))
    {
      subModeState = shuntingRouteStart;
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void ShuntingRouteMode::handleMode(CommonDataForModes &commonData)
    {
      //Remove passed tracks and targets
      removePassedObjects(commonData);
      //Handle current driving direction
      handleCurrentDrivDirection(commonData);
      // Check if unregistration message is received from TCC.
      if (!handleUnRegMessage())
      {
        manageTCCTimeOut();
        //check for MA timeout
        manageMATimeout(commonData);
        //check for Idling
        manageTrainIdling(commonData);
        //Checking for stop train
        manageStopTrain(commonData);
        //run the function corresponding to the modeState.
        switch (subModeState)
        {
        case shuntingRouteStart:
          runShuntingRouteStart();
          break;

        case shuntingRouteWaitConfirmFromDMI:
          runShuntingRouteWaitConfirmFromDMI();
          break;

        case shuntingRouteAcceptedFromDMI:
          runShuntingRouteAcceptedFromDMI(commonData);
          break;

        case shuntingRouteWaitMAScratch:
          runShuntingRouteWaitMAScratch();
          break;

        case shuntingRouteConfirmMAScratch:
          runShuntingRouteConfirmMAScratch();
          break;

        default:
          //Safety Halt
          ATC::AbstractEventHandler::corePtr()->reportEvent(unKnownStateInShuntRoute,
            __FILE__, __LINE__);
          break;
        }
      }


    }

    /******************************************************************************
    * getModeId
    ******************************************************************************/
    ATPMode ShuntingRouteMode::getModeId()
    {
      return ATPModeShuntingRoute;
    }


    /******************************************************************************
    * runShuntingRouteStart
    ******************************************************************************/
    void ShuntingRouteMode::runShuntingRouteStart()
    {
      ATC::AbstractEventHandler::corePtr()->reportEvent(standStillShuntingRouteConfirm,
        __FILE__, __LINE__);
      subModeState = shuntingRouteWaitConfirmFromDMI;
    }


    /******************************************************************************
    * runShuntingRouteWatingConfrimationFromDMI
    ******************************************************************************/
    void ShuntingRouteMode::runShuntingRouteWaitConfirmFromDMI()
    {
      //Issue standstill event until driver confirms on DMI
      ATC::AbstractEventHandler::corePtr()->reportEvent(standStillShuntingRouteConfirm,
        __FILE__, __LINE__);

      if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonConfirmShuntingRoute)
      {
        subModeState = shuntingRouteAcceptedFromDMI;
      }
    }


    /******************************************************************************
    * runShuntingRouteAcceptedFromDMI
    ******************************************************************************/
    void ShuntingRouteMode::runShuntingRouteAcceptedFromDMI(CommonDataForModes &commonData)
    {
      if (commonData.idling)
      {
        subModeState = shuntingRouteWaitMAScratch;
      }
    }

    /******************************************************************************
    * runShuntingRouteWaitMAScratch
    ******************************************************************************/
    void ShuntingRouteMode::runShuntingRouteWaitMAScratch()
    {
      MAHead mHead;
      if (AbstractMessageHandler::corePtr()->getMAHead(mHead))
      {
        if (RtShuntingRoute == mHead.routeType)
        {
          subModeState = shuntingRouteConfirmMAScratch;

          //Issue a standstill event to avoid one cycle delay until driver confirms
          ATC::AbstractEventHandler::corePtr()->reportEvent(standStillShuntingRouteConfirm,
            __FILE__, __LINE__);
        }
      }
    }

    /******************************************************************************
    * runShuntingRouteConfirmMAScratch
    ******************************************************************************/
    void ShuntingRouteMode::runShuntingRouteConfirmMAScratch()
    {
      //Issue a standstill event until driver confirms
      ATC::AbstractEventHandler::corePtr()->reportEvent(standStillShuntingRouteConfirm,
        __FILE__, __LINE__);
      if (DMICom::DMIButtonConfirmShuntingRouteMA == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus())
      {
        subModeState = shuntingRouteAcceptedFromDMI;
      }
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void ShuntingRouteMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&subModeState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&standStillShuntingRouteConfirm));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&unKnownStateInShuntRoute));
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    TrainShuntingRouteModeState ShuntingRouteMode::getModeState() const
    {
      return subModeState;
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool ShuntingRouteMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(subModeState, str);
      return true;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void ShuntingRouteMode::getModeStateString(const TrainShuntingRouteModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case shuntingRouteStart:
        static_cast<void>(vfw_strlcpy(buffer, "shuntingRouteStart", maxModeStateNameLength));
        break;

      case shuntingRouteWaitConfirmFromDMI:
        static_cast<void>(vfw_strlcpy(buffer, "shuntingRouteWaitConfirmFromDMI", maxModeStateNameLength));
        break;

      case shuntingRouteAcceptedFromDMI:
        static_cast<void>(vfw_strlcpy(buffer, "shuntingRouteAcceptedFromDMI", maxModeStateNameLength));
        break;

      case shuntingRouteWaitMAScratch:
        static_cast<void>(vfw_strlcpy(buffer, "shuntingRouteWaitMAScratch", maxModeStateNameLength));
        break;

      case shuntingRouteConfirmMAScratch:
        static_cast<void>(vfw_strlcpy(buffer, "shuntingRouteConfirmMAScratch", maxModeStateNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
        break;
      }
    }

    /******************************************************************************
    * isValidQRouteType
    ******************************************************************************/
    bool ShuntingRouteMode::isValidQRouteType(const RouteType routeType) const
    {
      bool retFlag = false;

      switch (routeType)
      {
      case RtShuntingRoute:
      {
        retFlag = true;
        break;
      }
      case RtNormal:
      case RtStaffResponsible:
      case RtSplit:
      case RtJoin:
      case RtReRegistration:
      case RtUndefined:
      {
        break;
      }
      default:
      {
        AbstractModeControl::corePtr()->getTrace()->write(2U, "Invalid Q Route Type !");
        break;
      }
      }
      return retFlag;
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void ShuntingRouteMode::resetMode()
    {
      char_t buffer[maxModeStateNameLength];
      getModeStateString(subModeState, &buffer[0]);
      subModeState = shuntingRouteStart;
      AbstractMode::resetMode();
      AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
      AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
    }

    /******************************************************************************
    * isValidUncondShorteningMsg
    ******************************************************************************/
    bool ShuntingRouteMode::isValidUncondShorteningMsg() const
    {
      return true;
    }
  }
}
