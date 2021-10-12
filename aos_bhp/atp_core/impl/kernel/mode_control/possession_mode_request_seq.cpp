/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the PosModeRequestSeq class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-12-20   spandita    Created for possession mode DMI button
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "possession_mode_request_seq.hpp"
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
    PosModeRequestSeq::PosModeRequestSeq() :ModeRequestSeq()
    {
      seqState = posIsDmiButtonPressed;
    }


    /******************************************************************************
    * run
    ******************************************************************************/
    void PosModeRequestSeq::run(CommonDataForModes &commonData)
    {
      const ModeRequestSeqState oldState = seqState;

      //run the function corresponding to the sequence state.
      switch (seqState)
      {
      case posIsDmiButtonPressed:
        runPosButtonPressed();
        break;
      
      case posDmiButtonSendReq:
        seqState = posDmiButtonWaitForAck;
        lastNumPositionMessages = RadioCom::AbstractRadioHandler::corePtr()->getNumPositionMessages();
        break;

      case posDmiButtonWaitForAck:
        runPosDmiButtonWaitForAck();
        break;

      case posDmiButtonConfirmed:
        runPosDmiButtonConfirmed(commonData);
        break;

      case posDmiButtonTCCTimeout:
        runPosDmiButtonTCCTimeout();
        break;

      default:
        break;
      }

      // Trace if sequence state has changed
      if (oldState != seqState)
      {
        trace->write(ATC::detailedMessageTrace, "Current DMI Button State:", static_cast<uint32_t>(seqState));
      }
    }

    /******************************************************************************
    * runPosButtonPressed
    ******************************************************************************/
    void PosModeRequestSeq::runPosButtonPressed()
    {
      bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

      if ((DMICom::DMIButtonEnterPossessionMode == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus())
        && isStandStill)
      {
        seqState = posDmiButtonSendReq;
      }
    }

   /******************************************************************************
    * runPosDmiButtonWaitForAck
    ******************************************************************************/
    void PosModeRequestSeq::runPosDmiButtonWaitForAck()
    {
      if (RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus())
      {
        seqState = posDmiButtonTCCTimeout;
        ATC::AbstractEventHandler::corePtr()->reportEvent(modTCCNotAvail, __FILE__, __LINE__);
      }
      else
      {
        //standstill event
        ATC::AbstractEventHandler::corePtr()->reportEvent(waitForModAckStandStill, __FILE__, __LINE__);

        const PossessionAcknowledge* const possessionAck = AbstractMessageHandler::corePtr()->getPossessionAcknowledge();

        if (possessionAck != static_cast<PossessionAcknowledge*>(NULL))
        {
          if (RequestAcknowledged == possessionAck->possAcknowledge)
          {
            //set the state to confirmed
            seqState = posDmiButtonConfirmed;
            DS::AbstractTargets::corePtr()->setCurCeilingSpeed(possessionAck->allowedSpeedInPossession);
            ATC::AbstractEventHandler::corePtr()->reportEvent(modAckReceived, __FILE__, __LINE__);
          }
          else
          {
            seqState = posIsDmiButtonPressed;
            
            // Log to driver
            ATC::AbstractEventHandler::corePtr()->reportEvent(modeAckRejectNotify, __FILE__, __LINE__);
          }
        }
        else
        {
          // Check if it has taken to many polls to receive the PossesionAck message
          if (isAckTimeout())
          {
            // Reset to start-state
            seqState = posIsDmiButtonPressed;
          }
        }
      }
    }

    /******************************************************************************
    * runPosDmiButtonConfirmed
    ******************************************************************************/
    void PosModeRequestSeq::runPosDmiButtonConfirmed(CommonDataForModes &commonData)
    {
      commonData.modeReqByDMI = ATPModePossession;
      DS::AbstractTSetup::corePtr()->removeTrainSetup();
      commonData.isATPReset = false;
      seqState = posIsDmiButtonPressed;
    }

    /******************************************************************************
    * runPosDmiButtonTCCTimeout
    ******************************************************************************/
    void PosModeRequestSeq::runPosDmiButtonTCCTimeout()
    {
      seqState = posIsDmiButtonPressed;
    }

    /******************************************************************************
    * validateModes
    ******************************************************************************/
    bool PosModeRequestSeq::validateModes()
    {
      bool validMode = false;
      ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
      if ((ATPModeSafetyHalt != currentMode) && (ATPModePoweringDown != currentMode) && (ATPModePossession != currentMode))
      {
        validMode = true;
      }
      return validMode;
    }
  }
}
