/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the PowerDownMode class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-03    arastogi    Created
* 2017-11-06    keisele     implement powering down sequence
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "power_down_mode.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_loco_io.hpp"
#include "abstract_odometry.hpp"
#include "abstract_targets.hpp"
#include "abstract_radio_handler.hpp"
#include "abstract_log_handler.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_mode_control_event_ids.hpp"

#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#include <cstdio>
#endif

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
    PowerDownMode::PowerDownMode() : AbstractMode(),
      // creating different set of objects for different type of events
      standstillInPowerDownMode(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdSBinPowerDown,
        ATC::NoSB, 0x0U, "Standstill in Power Down mode.")),
      powerDownSequenceStarted(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer, eventIdPowerDownSeqStarted,
        0U, "PoweringDown sequence started")),
      modeState(powerDownStateSendLog),
      offInPressTimerStart(0),
      offInPressTimerActive(false),
      lastOffIn(false),
      lastOffInValid(false),
      lastNumPositionMessagesTime(0),
      lastNumPositionMessages(0U),
      fPowerDownRequested(false),
      blinkLastToggle(0),
      statusSent(false)
    {
    }

    /******************************************************************************
    * powerDownRequested
    ******************************************************************************/
    bool PowerDownMode::powerDownRequested()
    {
      bool offIn = false;
      bool atoModeManual = false;
      bool timerExpired = false;
      char_t buf[512];
      char_t modeName[AbstractMode::maxModeStateNameLength + 1U];

      /* once condition is met continue with powerdown sequencing, AOS 2229 */
      if (!fPowerDownRequested)
      {
        if (!IO::AbstractLocoIO::corePtr()->getCoreDigitalInputValue(IO::AbstractLocoIO::OFFIn, &offIn))
        {
          /* should not happen */
          AbstractModeControl::corePtr()->getTrace()->write(ATC::briefTrace, "getCoreInputValue(): IO::AbstractLocoIO::CoreInputs::OFFIn failed");
          offIn = false;
        }

        // 5 second offIn press handling
        int64_t cur = vfwGetReferenceTime();
        if (lastOffInValid)
        {
          /* detect rising edge and start timer */
          if ((!lastOffIn) && offIn)
          {
            offInPressTimerActive = true;
            offInPressTimerStart = cur;
          }
        }
        else
        {
          if (offIn)
          {
            offInPressTimerActive = true;
            offInPressTimerStart = cur;
          }
        }

        lastOffIn = offIn;
        lastOffInValid = true;

        /* is offIn release stop timer*/
        if (!offIn)
        {
          if (offInPressTimerActive && (!statusSent))
          {
            AbstractModeControl::corePtr()->getTrace()->write(ATC::briefTrace, "powering down OffIn button request was too short");
          }

          offInPressTimerActive = false;
          statusSent = false;
        }

        if (offInPressTimerActive)
        {
          int64_t passsec = (cur - offInPressTimerStart) / 1000;

          /* timeout for offIn signal is 5 seconds*/
          if (passsec > 5)
          {
            timerExpired = true;
          }
        }

        if (timerExpired)
        {

          // gather information to test powerDown condition
          bool tccCommLost = !RadioCom::AbstractRadioHandler::corePtr()->getConnected();
          bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
          ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
          DS::BaseTarget* pTarget = DS::AbstractTargets::corePtr()->getPrimaryTarget();
          bool primaryTargetExists = false;
          if (pTarget != static_cast<DS::BaseTarget*>(NULL))
          {
            primaryTargetExists = true;
          }

          atoModeManual = (AbstractModeControl::corePtr()->getATOMode() == ATOModeManual);

          switch (currentMode) {
          case ATPModeNormal:
          case ATPModeStaffResponsible:
          case ATPModeShuntingRoute:
          case ATPModeSplit:
          case ATPModeJoin:
          case ATPModeLocation:   // Shall be possible to powerdown also in Location when communication with TCC lost 

            if (isStandStill  && atoModeManual)
            {
              if (primaryTargetExists)
              {
                if (tccCommLost)
                {
                  DS::AbstractTargets::corePtr()->removeAll();

                  fPowerDownRequested = true;
                }
              }
              else
              {
                fPowerDownRequested = true;
              }
            }
            break;

          case ATPModePowerUp:
          case ATPModeConfiguration:
          case ATPModeRegistration:
          case ATPModeBaliseSearch:
          case ATPModePossession:
          case ATPModeShunting:
          case ATPModeYard:
          case ATPModeSleeping:
          case ATPModeSafeBrakeToStop:
          case ATPModeUnregistered:
          case ATPModeSafetyHalt:

            // once powerdown is requested dont exit mode
            if (isStandStill && atoModeManual)
            {
              fPowerDownRequested = true;
            }
            break;

          case ATPModePoweringDown:
            break;
          case ATPModeUndefined:
          case ATPModesCount:
          default:
            ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
            break;
          }

          /* show message showing why the powerdown request was rejected */
          if ((!fPowerDownRequested) && (!statusSent))
          {
            AbstractModeControl::corePtr()->getModeString(currentMode, &modeName[0]);

            //lint -e{586} snprintf is needed here
            const int32_t result = snprintf(&buf[0], sizeof(buf), "powering down requested but not entered because of: "
              "state: %s, "
              "isStandStill: %d, "
              "atoModeManual: %d, "
              "primaryTargetExists: %d, "
              "tccCommLost: %d",
              modeName,
              isStandStill ? 1 : 0,
              atoModeManual ? 1 : 0,
              primaryTargetExists ? 1 : 0,
              tccCommLost ? 1 : 0);

            if ((result > 0) && (static_cast<size_t>(result) < sizeof(buf)))
            {
              AbstractModeControl::corePtr()->getTrace()->write(ATC::briefTrace, &buf[0]);
            }
            statusSent = true;
          }
        }
      }
      return fPowerDownRequested;
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void PowerDownMode::handleMode(CommonDataForModes &commonData)
    {
      int64_t curTime = vfwGetReferenceTime();
      int64_t elapsedSec;
      uint32_t curPosMessages;
      uint32_t positionMessageTimeout;
      uint32_t positionMessageNum;

      // Raise standstill event to restrict movement
      ATC::AbstractEventHandler::corePtr()->reportEvent(standstillInPowerDownMode,
        __FILE__, __LINE__);

      //Clearing MA Timeout and Train Idling 
      manageMATimeout(commonData);
      manageTrainIdling(commonData);
      handleCurrentDrivDirection(commonData);
      PowerDownModeState oldModeState = modeState;
      commonData.isAbortSetupActive = false;
      switch (modeState)
      {
      case powerDownStateSendLog:

        ATC::AbstractEventHandler::corePtr()->reportEvent(powerDownSequenceStarted, __FILE__, __LINE__);
        //clear the Possession Balise List
        DS::AbstractTracks::corePtr()->clearPossessionBaliseList();
        //clear all tracks and balises
        DS::AbstractTargets::corePtr()->removeAll();
        DS::AbstractTracks::corePtr()->removeAll();
        /* start timer for for waiting for positional messages to be sent */
        lastNumPositionMessagesTime = curTime;
        lastNumPositionMessages = RadioCom::AbstractRadioHandler::corePtr()->getNumPositionMessages();
        modeState = powerDownStateWaitPositionSent;

        break;

      case powerDownStateWaitPositionSent:

        elapsedSec = (curTime - lastNumPositionMessagesTime) / 1000;
        curPosMessages = RadioCom::AbstractRadioHandler::corePtr()->getNumPositionMessages();
        positionMessageTimeout = AbstractModeControl::corePtr()->getPoweringDownPosMessageTimeoutSec();
        positionMessageNum = AbstractModeControl::corePtr()->getPoweringDownNumPosMessages();

        if (((curPosMessages - lastNumPositionMessages) >= positionMessageNum) ||
          (elapsedSec >= static_cast<int32_t>(positionMessageTimeout)))
        {
          modeState = powerDownStateTurnOffLight;
        }
        break;

      case powerDownStateTurnOffLight:
        /* last state, powerOffSignalValue() returns true, power is removed */
        break;

      default:
        break;
      }

      //If Mode state has changed
      if (oldModeState != modeState)
      {
        AbstractModeControl::corePtr()->getTrace()->write
        (2U, "Current PowerDown State :", static_cast<uint32_t>(modeState));
      }

      /* blink while powering down mode is active */
      blink(commonData);
    }

    /******************************************************************************
    * getModeName
    ******************************************************************************/
    ATPMode PowerDownMode::getModeId()
    {
      return ATPModePoweringDown;
    }

    /******************************************************************************
    * manageIdling
    ******************************************************************************/
    void PowerDownMode::manageTrainIdling(CommonDataForModes &commonData)
    {
      commonData.idling = false;
    }

    /******************************************************************************
    * manageMATimeoutState
    ******************************************************************************/
    void PowerDownMode::manageMATimeout(CommonDataForModes &commonData)
    {
      commonData.maTimeOut = false;
    }

    /******************************************************************************
    * powerOffSignalValue
    ******************************************************************************/
    bool PowerDownMode::powerOffSignalValue() const
    {
      return (modeState == powerDownStateTurnOffLight);
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void PowerDownMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<PowerDownModeState>(&modeState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&standstillInPowerDownMode));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&powerDownSequenceStarted));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&offInPressTimerStart));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&offInPressTimerActive));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&lastOffIn));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&lastOffInValid));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&lastNumPositionMessagesTime));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&lastNumPositionMessages));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&fPowerDownRequested));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&blinkLastToggle));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&statusSent));
    }

    /******************************************************************************
    * blink
    ******************************************************************************/
    void PowerDownMode::blink(CommonDataForModes &commonData)
    {
      // Implement blinking while powering down, AOS 431
      uint32_t duration = AbstractModeControl::corePtr()->getPoweringDownBlinkDutyMs();
      int64_t curTime = vfwGetReferenceTime();

      if ((curTime - blinkLastToggle) > static_cast<int64_t>(duration))
      {
        blinkLastToggle = curTime;
        commonData.atpLampStatus = !(commonData.atpLampStatus);
      }
    }
  }
}
