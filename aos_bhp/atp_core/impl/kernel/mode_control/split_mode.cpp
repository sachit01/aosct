/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the SplitMode class.
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

#include "split_mode.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_targets.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
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
    SplitMode::SplitMode() : AbstractMode(),
      modeState(splitStart),
      standstillWaitForConfirm(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdStandStillInSplit,
        ATC::NoSB, 0U, "Standstill when waiting for Split confirmation."))
    {
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void SplitMode::handleMode(CommonDataForModes &commonData)
    {
      SplitModeState oldModeState = modeState;

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
        case splitStart:
          runSplitStart();
          break;
        case splitWaitConfirmDMI:
          runSplitWaitConfirmDMI();
          break;
        case splitProcessing:
          runSplitProcessing();
          break;
        case splitFinishConfig:
          runSplitFinishConfig();
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
    * runSplitStart
    ******************************************************************************/
    void SplitMode::runSplitStart()
    {
      // when MA from scratch is received from TCC.
      const bool isMATargetListEmpty = DS::AbstractTargets::corePtr()->isMATargetListEmpty();
      if (isMATargetListEmpty == false)
      {
        // Need to request driver
        modeState = splitWaitConfirmDMI;
      }
    }
       

    /******************************************************************************
    * runSplitWaitConfirmDMI
    ******************************************************************************/
    void SplitMode::runSplitWaitConfirmDMI()
    {

      // Raise Standstill event until confirmed from DMI.
      ATC::AbstractEventHandler::corePtr()->reportEvent(standstillWaitForConfirm, __FILE__, __LINE__);

      // If Split mode is confirmed from DMI -> Proceed to next state
      if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonConfirmSplit)
      {
        modeState = splitProcessing;
      }

    }

    /******************************************************************************
    * runSplitProcessing
    ******************************************************************************/
    void SplitMode::runSplitProcessing()
    {
      // if driver is logged in DMI
      if (Kernel::DriverLoginSeq::driverLoggedIn == Kernel::AbstractModeControl::corePtr()->getDriverLoginSeqState())
      {
        // If Configuration mode is confirmed from DMI -> Proceed to next state
        if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonTrainConfig)
        {
          modeState = splitFinishConfig;
        }
      }
    }

    /******************************************************************************
    * runSplitFinishConfig
    ******************************************************************************/
    void SplitMode::runSplitFinishConfig()
    {
      setNextMode(ATPModeConfiguration);
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void SplitMode::resetMode()
    {
      char_t buffer[maxModeStateNameLength];
      getModeStateString(modeState, &buffer[0]);
      modeState = splitStart;
      AbstractMode::resetMode();
      AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
      AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
    }

    /******************************************************************************
    * getModeName
    ******************************************************************************/
    ATPMode SplitMode::getModeId()
    {
      return ATPModeSplit;
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool SplitMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    SplitModeState SplitMode::getModeState() const
    {
      return modeState;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void SplitMode::getModeStateString(const SplitModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case splitStart:
        static_cast<void>(vfw_strlcpy(buffer, "splitStart", maxModeStateNameLength));
        break;

      case splitWaitConfirmDMI:
        static_cast<void>(vfw_strlcpy(buffer, "splitWaitConfirmDMI", maxModeStateNameLength));
        break;

      case splitProcessing:
        static_cast<void>(vfw_strlcpy(buffer, "splitProcessing", maxModeStateNameLength));
        break;

      case splitFinishConfig:
        static_cast<void>(vfw_strlcpy(buffer, "splitFinishConfig", maxModeStateNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
        break;
      }
    }

    /******************************************************************************
    * isValidQRouteType
    ******************************************************************************/
    bool SplitMode::isValidQRouteType(const RouteType routeType) const
    {
      bool retFlag = false;

      switch (routeType)
      {
      case RtSplit:
        retFlag = true;
        break;
      case RtNormal:
      case RtShuntingRoute:
      case RtStaffResponsible:
      case RtJoin:
      case RtReRegistration:
      case RtUndefined:
        break;
      default:
      {
        AbstractModeControl::corePtr()->getTrace()->write(2U, "Invalid Q Route Type !");
        break;
      }
      }
      return retFlag;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void SplitMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&modeState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&standstillWaitForConfirm));
    }

    /******************************************************************************
    * isValidUncondShorteningMsg
    ******************************************************************************/
    bool SplitMode::isValidUncondShorteningMsg() const
    {
      return true;
    }
  }
}

