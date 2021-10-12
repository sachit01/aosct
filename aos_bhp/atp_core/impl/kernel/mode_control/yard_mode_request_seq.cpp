/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the YardModeRequestSeq class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-12-20   spandita      Yard mode button sequence
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "yard_mode_request_seq.hpp"
#include "abstract_odometry.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_radio_handler.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_targets.hpp"
#include "abstract_dmi_handler.hpp"

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
    YardModeRequestSeq::YardModeRequestSeq() :ModeRequestSeq()
    {
      seqState = yardWaitDmiButtonPress;
    }


    /******************************************************************************
    * run
    ******************************************************************************/
    void YardModeRequestSeq::run(CommonDataForModes &commonData)
    {

      ModeRequestSeqState oldState = seqState;
      ATPMode atpMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      bool atpOkStatus = Kernel::AbstractModeControl::corePtr()->getATPOKStatus();
      bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      const bool isCabActive = (Kernel::AbstractModeControl::corePtr()->getActiveCab() != NoCabActive);
      const bool isSleepingSignalActive = IO::AbstractLocoIO::corePtr()->getSleepingSignal();

      // Reset mode State and wait for button press
      if ( (!atpOkStatus) || (!isStandStill) || (!isCabActive) || ((ATPModeSleeping == atpMode) && (isSleepingSignalActive)))
      {
        seqState = yardWaitDmiButtonPress;
      }

      //run the function corresponding to the sequence state.
      switch (seqState)
      {
      case yardWaitDmiButtonPress:
        runYardWaitDmiButtonPress();
        break;

      case yardDmiButtonSendReq:
        seqState = YardWaitForTCCAck;
        lastNumPositionMessages = RadioCom::AbstractRadioHandler::corePtr()->getNumPositionMessages();
        break;

      case YardWaitForTCCAck:
        runYardWaitForTCCAck();
        break;

      case yardConfirmed:
        runYardConfirmed(commonData);
        break;

      case YardDmiAckManualConfirm:
        runYardDmiAckManualConfirm();
        break;

      default:
        break;
      }

      //If sequence state has changed
      if (oldState != seqState)
      {
        trace->write
        (2U, "Current DMI Button State:", static_cast<uint32_t>(seqState));
      }
    }

    /******************************************************************************
    * runYardWaitDmiButtonPress
    ******************************************************************************/
    void YardModeRequestSeq::runYardWaitDmiButtonPress()
    {
      bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

      if ((DMICom::DMIButtonEnterYardMode == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus()) &&
        isStandStill)
      {
        if (RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus())
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(modTCCNotAvail, __FILE__, __LINE__);
          seqState = YardDmiAckManualConfirm;
        }
        else
        {
          seqState = yardDmiButtonSendReq;
        }
      }
    }

    /******************************************************************************
    * runYardWaitForTCCAck
    ******************************************************************************/
    void YardModeRequestSeq::runYardWaitForTCCAck()
    {
      YardAcknowledge yardAck;
      if (RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus())
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(modeAckRejectNotify, __FILE__, __LINE__);
        seqState = yardWaitDmiButtonPress;
      }
      else
      {
        //apply standstill event
        ATC::AbstractEventHandler::corePtr()->reportEvent(waitForModAckStandStill, __FILE__, __LINE__);

        if (AbstractMessageHandler::corePtr()->getYardAcknowledge(yardAck))
        {
          if (RequestAcknowledged == yardAck.yardAcknowledge)
          {
            DS::AbstractTargets::corePtr()->setCurCeilingSpeed(yardAck.allowedSpeedInYard);
            seqState = yardConfirmed;
            ATC::AbstractEventHandler::corePtr()->reportEvent(modAckReceived, __FILE__, __LINE__);
          }
          else
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(modeAckRejectNotify, __FILE__, __LINE__);
            seqState = yardWaitDmiButtonPress;
          }
        }
        else
        {
          // Check if it has taken to many polls to receive the YardAck message
          if (isAckTimeout())
          {
            // Reset to start-state
            seqState = yardWaitDmiButtonPress;
          }
        }
      }
    }

    /******************************************************************************
    * runYardDmiAckManualConfirm
    ******************************************************************************/
    void YardModeRequestSeq::runYardDmiAckManualConfirm()
    {
      //TCC Timeout expiry
      if (RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus())
      {
        if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonConfirmYard)
        {
          DS::AbstractTargets::corePtr()->setCurCeilingSpeed(AbstractConfig::corePtr()->getYardSpeed());
          seqState = yardConfirmed;
        }
        else
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(waitForModAckDriver, __FILE__, __LINE__);
        }
      }
      else
      {
        seqState = yardWaitDmiButtonPress;
      }
    }

    /******************************************************************************
    * runYardConfirmed
    ******************************************************************************/
    void YardModeRequestSeq::runYardConfirmed(CommonDataForModes &commonData)
    {
      commonData.modeReqByDMI = ATPModeYard;
      DS::AbstractTSetup::corePtr()->removeTrainSetup();
      commonData.isATPReset = false;
      seqState = yardWaitDmiButtonPress;
    }
   
    /******************************************************************************
    * isDMIButtonNeeded
    ******************************************************************************/
    bool YardModeRequestSeq::isDMIButtonNeeded()
    {
      bool enableYardButtonOnDMI = false;

      //Radio link with TCC is reported lost for longer than a configurable time to allow Yard mode 
      const bool isRadioTimedOutToEnterYard = RadioCom::AbstractRadioHandler::corePtr()->isYardModeTimerExpired();
      //vehicle is standstill
      const bool isTrainStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      //Current ATP Mode
      const ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
      const LocationModeState locationModeState = AbstractModeControl::corePtr()->getLocationModeState();

      if (isRadioTimedOutToEnterYard)
      {
        const bool isValidMode = validateModes();
        //ATO mode is manual
        const bool isATOModeManual = (ATOModeManual == AbstractModeControl::corePtr()->getATOMode());
        //cabin is active
        const bool isAnyCabinActive = (NoCabActive != AbstractModeControl::corePtr()->getActiveCab());
        //Safe brake to stop event active
        const bool isSafeBrkToStopEvntActive = ATC::AbstractEventHandler::corePtr()->isModeChangeToSafeBrakeToStop();
        //idle state is set if in normal mode
        const bool isTrainidle = (ATPModeNormal == currentMode) ? AbstractModeControl::corePtr()->getIdleState() : true;

        enableYardButtonOnDMI = isValidMode && isAnyCabinActive && isTrainStandStill && isATOModeManual && 
                                isTrainidle && !isSafeBrkToStopEvntActive;
      }
      else if ((ATPModeLocation == currentMode) 
        && (LocationMode::locationYardModeHandlingStart == locationModeState))
      {
        if (Pos::AbstractOdometry::corePtr()->getSpeed() < AbstractModeControl::corePtr()->geMaxAllowedSpeedInLoc())
        {
          enableYardButtonOnDMI = true;
        }
      }
      else
      {
        enableYardButtonOnDMI = ModeRequestSeq::isDMIButtonNeeded();
      }

      return enableYardButtonOnDMI;
    }

    /******************************************************************************
    * validateModes
    ******************************************************************************/
    bool YardModeRequestSeq::validateModes()
    {

      bool validMode = false;
      ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
      DriverLoginState driverLoginState = AbstractModeControl::corePtr()->getDriverLoginSeqState();
      const bool isTCCTimedOut = RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus();

      if (ATPModeSleeping == currentMode)
      {
        if (!IO::AbstractLocoIO::corePtr()->getSleepingSignal())
        {
          validMode = true;
        }
      }
      else if ((ATPModeSafetyHalt != currentMode) && (ATPModePoweringDown != currentMode) && (ATPModeYard != currentMode))
      {
        validMode = true;
      }
      else if ((ATPModeYard == currentMode) && (driverLoginState == DriverLoginSeq::driverLoggedOut) && (isTCCTimedOut))
      {
        validMode = true;
      }
      else
      {
        //Nothing
      }

      return validMode;
    }

  }
}
