/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the BaliseSearchMode class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-25    arastogi    Created
* 2017-03-21    spandita    Updated the balise search mode with balise search requirements
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "balise_search_mode.hpp"
#include "abstract_config.hpp"
#include "abstract_position.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tracks.hpp"
#include "abstract_odometry.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_targets.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_log_handler.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_mode.hpp"
#include "abstract_tsetup.hpp"
#include "atc_types.hpp"
#include <vfw_string.h>
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "emergency_alert_seq.hpp"
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
    BaliseSearchMode::BaliseSearchMode() : AbstractMode(),
      // creating different set of objects for different type of events
      logFirstBalise(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer, eventIdSBinBALFirstBalise,
        DMICom::noDmi, "First balise found, Balise ID:", true)),
      logSecondBalise(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer, eventIdSecondBaliseFoundinBS,
        DMICom::noDmi, "Second balise found, Balise ID:", true)),
      unknownState(ATC::Event::createSafetyHaltEvent(atpModeControlId, ATC::CoreContainer, eventIdUnknownStateInBAL,
        ATC::NoEB, DMICom::modUnknownModeState, "Unknown state in Balise Search mode")),
      baliseNotFoundInBS(ATC::Event::createSBReqEvent(atpModeControlId, ATC::CoreContainer, eventIdBaliseNotFoundinBS,
        ATC::ATOSB, DMICom::modFirstBaliseError, "Exceeded the max search distance for First Balise ", true)),
      exceededBalSearchDistForSecBal(ATC::Event::createSBReqEvent(atpModeControlId, ATC::CoreContainer, eventIdExceededMAWaitForSecBal,
        ATC::ATOSB, DMICom::exceededMAWaitForSecBal, "Exceeded the MA search distance(cm) for second balise:", true)),
      inCorrectRouteTypeInBS(ATC::Event::createSafeBrakeSBEvent(atpModeControlId, ATC::CoreContainer, eventIdInCorrectRouteTypeInBS,
        ATC::ATOSB, DMICom::modReRefInvalidMA, "Invalid Q Route Type In BS")),
      secondBaliseFoundBeforeMA(ATC::Event::createSBReqEvent(atpPositionId, ATC::CoreContainer, eventIdSecBaliseFoundInBS,
        ATC::NoSB, DMICom::secBaliseFoundInBS, "Mode dependent brake for 2nd balise, Balise ID:", true)),
      emergencyAlertInBS(ATC::Event::createSafeBrakeSBEvent(atpModeControlId, ATC::CoreContainer, eventIdEmergencyAlertInBS,
        ATC::NoSB,  DMICom::modEmergencyAlertBS, "Emergency Alert set In BS")),
      unConditionalShortInBS(ATC::Event::createSafeBrakeSBEvent(atpModeControlId,ATC::CoreContainer, eventIdUnconditionalInBS,
        ATC::ATOSB, DMICom::modUnconditionalMA, "Unconditional Msg Received In BS")),
      unRegMsgSBEvent(ATC::Event::createSBReqEvent(atpModeControlId, ATC::CoreContainer, eventIdUnRegMsg,
        ATC::NoSB, DMICom::unregMsg, "Unregistration message received in balise search before any MA"))
    {
      modeState = baliseSearchStart;
      secondBaliseInfo.nidBG = 0U;
      secondBaliseInfo.odometerPos = 0;
      baliseSearchOdometerReReg = 0;
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void BaliseSearchMode::resetMode()
    {
      modeState = baliseSearchStart;
      AbstractMode::resetMode();
      secondBaliseInfo.nidBG = 0U;
      secondBaliseInfo.odometerPos = 0;
      char_t buffer[maxModeStateNameLength];
      getModeStateString(modeState, &buffer[0]);

      AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
      AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void BaliseSearchMode::handleMode(CommonDataForModes &commonData)
    {
      
      handleAbortSetup(commonData);//Check if the setup is aborted and switch the mode state
      handleCurrentDrivDirection(commonData);

      // Check if the Unregistration message is received from TCC.
      if (!handleUnRegMessage())
      {
        BaliseSearchModeState oldModeState = modeState;

        //run the function corresponding to the modeState.
        switch (modeState)
        {
        case baliseSearchStart:
          runBaliseSearchStart(commonData);
          break;

        case baliseSearchWaitForBaliseReg:
          runBaliseSearchWaitForBaliseReg();
          break;

        case baliseSearchWaitForBaliseReReg:
          runBaliseSearchWaitForBaliseReReg(commonData);
          break;

        case baliseSearchWaitMA:
          runBaliseSearchWaitMA(commonData);
          break;

        case baliseSearchWaitBalise2:
          runBaliseSearchWaitBalise2(commonData);
          break;

        case baliseSearchFinishOK:
          runBaliseSearchFinishOK();
          break;

        case baliseSearchFinishNOK:
          runBaliseSearchFinishNOK();
          break;

        case baliseSearchConfigAborted:
          runBaliseSearchConfigAborted(commonData);
          break;

        case baliseSearchHandleUnregWithoutMA:
          runbaliseSearchHandleUnregWithoutMA();
          break;

        default:
          //Safety Halt
          ATC::AbstractEventHandler::corePtr()->reportEvent(unknownState, __FILE__, __LINE__);
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

        // Check if Emergency Alert is raised in Balise Search mode
        if ((AbstractModeControl::corePtr()->getEmergencyAlertSeqState() != emergencyAlertInactive))
        {
          //Raise a Safe Brake To Stop 
          ATC::AbstractEventHandler::corePtr()->reportEvent(emergencyAlertInBS,
            __FILE__, __LINE__);
        }
      }
    }

    /******************************************************************************
    * getModeName
    ******************************************************************************/
    ATPMode BaliseSearchMode::getModeId()
    {
      return ATPModeBaliseSearch;
    }

    /******************************************************************************
    * runBaliseSearchStart
    ******************************************************************************/
    void BaliseSearchMode::runBaliseSearchStart(CommonDataForModes &commonData)
    {
      //Clearing MA Timeout and Train Idling 
      manageMATimeout(commonData);
      manageTrainIdling(commonData);
      
      //check for new configuration
      if (AbstractModeControl::corePtr()->isNewTrainConfiguration())
      {
        modeState = baliseSearchWaitForBaliseReg;
      }
      else
      {
        modeState = baliseSearchWaitForBaliseReReg;
        DS::BaseTarget *pTarget = DS::AbstractTargets::corePtr()->getPrimaryTarget();

        if(pTarget != static_cast<DS::BaseTarget*>(NULL))
        {
          baliseSearchOdometerReReg = (DirForward == pTarget->getDirection()) ?
              (Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo() + static_cast<int32_t>(maxSearchDistanceInReReg)) :
              (Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo() - static_cast<int32_t>(maxSearchDistanceInReReg));
        }
      }
    }

    /******************************************************************************
    * runBaliseSearchWaitForBaliseReg
    ******************************************************************************/
    void BaliseSearchMode::runBaliseSearchWaitForBaliseReg()
    {
      Pos::AbstractDecode::BaliseInfo bInfo = {0U,0};
      //Get the primary target from target list
      DS::BaseTarget *pTarget = DS::AbstractTargets::corePtr()->getPrimaryTarget();

      TravelDir targetDir = DirUndefined;
      TrackAndPos targetPos = { 0U,0U };
      OdoPosition primaryTargetOdo = 0;

      if (static_cast<DS::BaseTarget*>(NULL) != pTarget)
      {
        //Get the direction
        targetDir = pTarget->getDirection();
        //Get the track and position
        targetPos = pTarget->getPosition();
        //get target odoposition
        primaryTargetOdo = pTarget->getOdometer();
      }

      // Check if first balise has been detected
      if (Pos::AbstractPosition::corePtr()->getFirstBaliseInfo(bInfo))
      {
        bool isTargetValid = true;
        OdoPosition targetOdo = 0;

        //prepare the dynamic text to be send while reporting event.
        logFirstBalise.setDynamicText(static_cast<uint32_t>(bInfo.nidBG));
        ATC::AbstractEventHandler::corePtr()->reportEvent(logFirstBalise, __FILE__, __LINE__);

        if (DirForward == targetDir)
        {
          //calculate the Updated Odo value for Wait for MA second balise search
          targetOdo = bInfo.odometerPos + static_cast<OdoPosition>(AbstractConfig::corePtr()->getSecondBaliseSearchDistance());
        }
        else if (DirReverse == targetDir)
        {
          //calculate the Updated Odo value for Wait for MA second balise search
          targetOdo = bInfo.odometerPos - static_cast<OdoPosition>(AbstractConfig::corePtr()->getSecondBaliseSearchDistance());
        }
        else
        {
          modeState = baliseSearchFinishNOK;
          isTargetValid = false;
        }

        if (isTargetValid)
        {
          //Delete the primary target
          if (DS::AbstractTargets::corePtr()->delTarget(pTarget))
          {
            //Create primary target object.
            DS::PrimaryTarget newPrimaryTarget(0U, //No MA id for virtual target
              static_cast<uint8_t>(RtNormal), // Normal route
              maMarginBaliseSearch, // MA Margin
              0U, // No Timeout
              targetPos, // Target Position
              targetDir, // travel direction
              targetOdo); // odometer of target

            DS::AbstractTargets::corePtr()->addTarget(newPrimaryTarget);
            //Change the mode to baliseSearchWaitMA
            modeState = baliseSearchWaitMA;
          }
        }
      }
      else
      {
        //check the train movement should not exceed the odo value of target
        if (DirForward == targetDir)
        {
          if (Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo() >= primaryTargetOdo)
          {
            //prepare the dynamic text to be send while reporting event.
            baliseNotFoundInBS.setDynamicText("1st Bal Missed");
            //Raise  brake  and send DMI text
            ATC::AbstractEventHandler::corePtr()->reportEvent(baliseNotFoundInBS,
              __FILE__, __LINE__);
            //remove the target
            DS::AbstractTargets::corePtr()->setDelTargetAtStandStill(true);
          }
        }
        else if (DirReverse == targetDir)
        {
          if (Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo() <= primaryTargetOdo)
          {
            //prepare the dynamic text to be send while reporting event.
            baliseNotFoundInBS.setDynamicText("1st Bal Missed");
            //Raise  brake  and send DMI text 
            ATC::AbstractEventHandler::corePtr()->reportEvent(baliseNotFoundInBS,
              __FILE__, __LINE__);
            //remove the target
            DS::AbstractTargets::corePtr()->setDelTargetAtStandStill(true);
          }
        }
        else
        {
           //do nothing
        }
      }
    }


    /******************************************************************************
    * runBaliseSearchWaitForBaliseReReg
    ******************************************************************************/
    void BaliseSearchMode::runBaliseSearchWaitForBaliseReReg(CommonDataForModes &commonData)
    {
      
      Pos::AbstractDecode::BaliseInfo bInfo = {0U,0};

      //Get the primary target from target list
      const DS::BaseTarget *pTarget = DS::AbstractTargets::corePtr()->getPrimaryTarget();
      // Check if first balise has been detected
      if (Pos::AbstractPosition::corePtr()->getFirstBaliseInfo(bInfo))
      {
        // Check if balise is in balise list
        if (DS::AbstractTracks::corePtr()->getBalise(bInfo.nidBG) != static_cast<const DS::Balise* const>(NULL))
        {
          modeState = baliseSearchFinishOK;

          //prepare the dynamic text to be send while reporting event.
          logFirstBalise.setDynamicText(static_cast<uint32_t>(bInfo.nidBG));
          ATC::AbstractEventHandler::corePtr()->reportEvent(logFirstBalise, __FILE__, __LINE__);
          // Remove passed tracks and targets
          removePassedObjects(commonData);
        }
        else
        {
          modeState = baliseSearchFinishNOK;
        }
      }
      else
      {
        if (static_cast<DS::BaseTarget*>(NULL) != pTarget)
        {
          bool missed1StBal = (DirForward == pTarget->getDirection()) ?
            (Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo() >= static_cast<int32_t>(baliseSearchOdometerReReg)) :
            (Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo() <= static_cast<int32_t>(baliseSearchOdometerReReg));
          if (missed1StBal)
          {
            //prepare the dynamic text to be send while reporting event.
            baliseNotFoundInBS.setDynamicText("1st Bal Missed");
            //Raise  brake  and send DMI text 
            ATC::AbstractEventHandler::corePtr()->reportEvent(baliseNotFoundInBS,
              __FILE__, __LINE__);
            DS::AbstractTargets::corePtr()->setDelTargetAtStandStill(true);
          }
        }
      }
    }

    /******************************************************************************
    * runBaliseSearchWaitMA
    ******************************************************************************/
    void BaliseSearchMode::runBaliseSearchWaitMA(CommonDataForModes &commonData)
    {
      MAHead mHead;
      //Get the primary target
      const DS::BaseTarget *pTarget = DS::AbstractTargets::corePtr()->getPrimaryTarget();
      //Check if the train is at standstill
      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      //To deactivate the Abort Setup on DMI
      commonData.isAbortSetupActive = false;
      //check if new MA is received
      if (AbstractMessageHandler::corePtr()->getMAHead(mHead))
      {
        //Only Normal MA is allowed.
        if (mHead.routeType == RtNormal)
        {
          modeState = baliseSearchWaitBalise2;
        }
        else
        {
          modeState = baliseSearchFinishNOK;
          //No other MA allowed during balise search.. Error
        }
      }
      else
      {
        Pos::AbstractDecode::BaliseInfo bInfo = { 0U,0};
        // Check if any balise received
        if (Pos::AbstractPosition::corePtr()->getBaliseInfo(bInfo))
        {
          if (0U == secondBaliseInfo.nidBG)
          {
             secondBaliseInfo = bInfo;
             ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "Second balise found before MA", "MC", __FILE__, __LINE__);
          }// if third balise is found, event will be raised by position
        }
        // check if second balise found before MA and train is not at standstill
        if (0U != secondBaliseInfo.nidBG)
        {
          if (!isStandStill)
          {
            //prepare the dynamic text to be send while reporting event.
            secondBaliseFoundBeforeMA.setDynamicText(static_cast<uint32_t>(secondBaliseInfo.nidBG));
            ATC::AbstractEventHandler::corePtr()->reportEvent(secondBaliseFoundBeforeMA, __FILE__
            , __LINE__);
           }
          else 
          {
            //delete the targets if target list is not empty.
            if(!(DS::AbstractTargets::corePtr()->isMATargetListEmpty()))
            {
              // Clear target data
              DS::AbstractTargets::corePtr()->removeAll();
            }
          }
        }

        //If MA has not received 
        if (static_cast<DS::BaseTarget*>(NULL) != pTarget)
        {
          //check the train odometry should not exceed the ODO value of target for second balise
          bool exceedMaWaitDis = (DirForward == pTarget->getDirection()) ?
            (Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo() >= pTarget->getOdometer()) :
            (Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo() <= pTarget->getOdometer());

          // check if MA search distance exceeded and train is not at standstill
          if (exceedMaWaitDis) 
          {
             if (!isStandStill)
             {
              //prepare the dynamic text to be send while reporting event.
              exceededBalSearchDistForSecBal.setDynamicText(static_cast<uint32_t>(AbstractConfig::corePtr()->getSecondBaliseSearchDistance()));
              //Raise brake
              ATC::AbstractEventHandler::corePtr()->reportEvent(exceededBalSearchDistForSecBal,
              __FILE__, __LINE__);
             }
             else
             {
               // Clear target data
               DS::AbstractTargets::corePtr()->removeAll();
             }
          }
        }
      }
    }

    /******************************************************************************
    * runBaliseSearchWaitBalise2
    ******************************************************************************/
    void BaliseSearchMode::runBaliseSearchWaitBalise2(CommonDataForModes &commonData)
    {
      //Do not raise event.. Allow movement
      Pos::AbstractDecode::BaliseInfo bInfo = { 0U,0 };
      //Remove passed tracks and targets
      removePassedObjects(commonData);
      // check for second balise
      if (0U != secondBaliseInfo.nidBG)
      {
        // Check if balise is in balise list
        if (DS::AbstractTracks::corePtr()->getBalise(secondBaliseInfo.nidBG) != static_cast<const DS::Balise* const>(NULL))
        {
          //Reset the value
          secondBaliseInfo.nidBG = 0U;
          secondBaliseInfo.odometerPos = 0;
          modeState = baliseSearchFinishOK;
         }
         else
         {
            modeState = baliseSearchFinishNOK;
         }
      }
      //check if balise is received
      else if (Pos::AbstractPosition::corePtr()->getBaliseInfo(bInfo))
      {
          //check if balise is in balise list
          if (DS::AbstractTracks::corePtr()->getBalise(bInfo.nidBG) != static_cast<const DS::Balise* const>(NULL))
          {
            secondBaliseInfo.nidBG = bInfo.nidBG;
            secondBaliseInfo.odometerPos = bInfo.odometerPos;

            modeState = baliseSearchFinishOK;

            //prepare the dynamic text to be send while reporting event.
            logSecondBalise.setDynamicText(static_cast<uint32_t>(bInfo.nidBG));
            ATC::AbstractEventHandler::corePtr()->reportEvent(logSecondBalise, __FILE__, __LINE__);
          }
          else
          {
            modeState = baliseSearchFinishNOK;
          }
      }
      else
      {
        //do nothing
      }
    }

    /******************************************************************************
    * runBaliseSearchFinishOK
    ******************************************************************************/
    void BaliseSearchMode::runBaliseSearchFinishOK()
    {
      //change mode to Normal
      setNextMode(ATPModeNormal);
    }

    /******************************************************************************
    * runBaliseSearchFinishNOK
    ******************************************************************************/
    void BaliseSearchMode::runBaliseSearchFinishNOK()
    {
      setNextMode(ATPModeSafeBrakeToStop);
    }

    /******************************************************************************
    * runBaliseSearchConfigAborted
    ******************************************************************************/
    void BaliseSearchMode::runBaliseSearchConfigAborted(CommonDataForModes &commonData)
    {
      // Disable Abort button on DMI
      commonData.isAbortSetupActive = false;

      // Remove the train setup when abort button is pressed in Balise search
      DS::AbstractTSetup::corePtr()->removeTrainSetup();

      // Remove targets when configuration is rejected
      DS::AbstractTargets::corePtr()->setDelTargetAtStandStill(true);
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    BaliseSearchModeState BaliseSearchMode::getModeState() const
    {
      return modeState;
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool BaliseSearchMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void BaliseSearchMode::getModeStateString(const BaliseSearchModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case baliseSearchStart:
        static_cast<void>(vfw_strlcpy(buffer, "baliseSearchStart", maxModeStateNameLength));
        break;

      case baliseSearchWaitForBaliseReg:
        static_cast<void>(vfw_strlcpy(buffer, "baliseSearchWaitForBaliseReg", maxModeStateNameLength));
        break;

      case baliseSearchWaitForBaliseReReg:
        static_cast<void>(vfw_strlcpy(buffer, "baliseSearchWaitForBaliseReReg", maxModeStateNameLength));
        break;

      case baliseSearchWaitMA:
        static_cast<void>(vfw_strlcpy(buffer, "baliseSearchWaitMA", maxModeStateNameLength));
        break;

      case baliseSearchWaitBalise2:
        static_cast<void>(vfw_strlcpy(buffer, "baliseSearchWaitBalise2", maxModeStateNameLength));
        break;

      case baliseSearchFinishOK:
        static_cast<void>(vfw_strlcpy(buffer, "baliseSearchFinishOK", maxModeStateNameLength));
        break;

      case baliseSearchFinishNOK:
        static_cast<void>(vfw_strlcpy(buffer, "baliseSearchFinishNOK", maxModeStateNameLength));
        break;

      case baliseSearchConfigAborted:
        static_cast<void>(vfw_strlcpy(buffer, "baliseSearchConfigAborted", maxModeStateNameLength));
        break;

      case baliseSearchHandleUnregWithoutMA:
        static_cast<void>(vfw_strlcpy(buffer, "baliseSearchHandleUnregWithoutMA", maxModeStateNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
        break;
      }
    }

    /******************************************************************************
    * manageIdling
    ******************************************************************************/
    void BaliseSearchMode::manageTrainIdling(CommonDataForModes &commonData)
    {
      commonData.idling = false;
    }


    /******************************************************************************
    * manageMATimeoutState
    ******************************************************************************/
    void BaliseSearchMode::manageMATimeout(CommonDataForModes &commonData)
    {
      commonData.maTimeOut = false;
    }

    /******************************************************************************
    * isValidQRouteType
    ******************************************************************************/
    bool BaliseSearchMode::isValidQRouteType(const RouteType routeType) const
    {
      bool retFlag = false;

      switch (routeType)
      {

      case RtNormal:
      {
        if (baliseSearchWaitMA == modeState)
        {
          retFlag = true;
        }
        break;
      }
      case RtShuntingRoute:
      case RtStaffResponsible:
      case RtSplit:
      case RtJoin:
      case RtReRegistration:
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
      if (!retFlag)
      {
        //raise safe brake to stop
        ATC::AbstractEventHandler::corePtr()->reportEvent(inCorrectRouteTypeInBS,
          __FILE__, __LINE__);
      }
      return retFlag;
    }

    /******************************************************************************
    * handleUnRegMessage
    ******************************************************************************/
    bool BaliseSearchMode::handleUnRegMessage()
    {
      bool isUnRegMsgProcessed = false;
      UnregInfo UnRegInfo;
      if (AbstractMessageHandler::corePtr()->getUnregInfo(UnRegInfo))
      {
        modeState = baliseSearchHandleUnregWithoutMA;
        isUnRegMsgProcessed = true;
      }
      return isUnRegMsgProcessed;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void BaliseSearchMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&modeState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&baliseSearchOdometerReReg));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&logFirstBalise));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&logSecondBalise));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&unknownState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&baliseNotFoundInBS));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&exceededBalSearchDistForSecBal));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&inCorrectRouteTypeInBS));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&secondBaliseFoundBeforeMA));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&emergencyAlertInBS));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&unConditionalShortInBS));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&unRegMsgSBEvent));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&secondBaliseInfo.nidBG));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&secondBaliseInfo.odometerPos));
    }

    /******************************************************************************
    * isValidUncondShorteningMsg
    ******************************************************************************/
    bool BaliseSearchMode::isValidUncondShorteningMsg() const
    {
      //raise the safe brake to stop
      ATC::AbstractEventHandler::corePtr()->reportEvent(unConditionalShortInBS,
        __FILE__, __LINE__);
      return false;
    }

    /******************************************************************************
    * runbaliseSearchHandleUnregWithoutMA
    ******************************************************************************/
    void BaliseSearchMode::runbaliseSearchHandleUnregWithoutMA()
    {
      const bool isTrainStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      if (isTrainStandStill)
      {
        //In standstill changing mode to Unregistration
        setNextMode(ATPModeUnregistered);
        //Removing train setup 
        DS::AbstractTSetup::corePtr()->removeTrainSetup();
      }
      else
      {
        //Raising brake event till train comes to stand still
        ATC::AbstractEventHandler::corePtr()->reportEvent(unRegMsgSBEvent, __FILE__, __LINE__);
      }
    }
    
  }
}
