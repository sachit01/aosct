/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the ShuntModeRequestSeq class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-12-20   spandita    Updated for shunting DMI button implementation
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "shunting_mode_request_seq.hpp"
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
    ShuntModeRequestSeq::ShuntModeRequestSeq() :ModeRequestSeq()
    {
      seqState = shuntIsDmiButtonPressed;
    }


    /******************************************************************************
    * run
    ******************************************************************************/
    void ShuntModeRequestSeq::run(CommonDataForModes &commonData)
    {
  
      ModeRequestSeqState oldState = seqState;

      //run the function corresponding to the sequence state.
      switch (seqState)
      {
      case shuntIsDmiButtonPressed:
        runShuntButtonPressed();
        break;

      case shuntDmiButtonSendReq:
        seqState = shuntDmiButtonWaitForAck;
        lastNumPositionMessages = RadioCom::AbstractRadioHandler::corePtr()->getNumPositionMessages();
        break;

      case shuntDmiButtonWaitForAck:
        runShuntDmiButtonWaitForAck();
        break;

      case shuntDmiButtonConfirmed:
        runShuntDmiButtonConfirmed(commonData);
        break;

      case shuntDmiButtonTCCTimeout:
        runShuntDmiButtonTCCTimeout();
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
    * runShuntButtonUnavailable
    ******************************************************************************/
    void ShuntModeRequestSeq::runShuntButtonPressed()
    {
      bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

      if ((DMICom::DMIButtonShuntingMode == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus()) 
        && isStandStill)
      {
        seqState = shuntDmiButtonSendReq;
      }
    }

    /******************************************************************************
    * runShuntDmiButtonWaitForAck
    ******************************************************************************/
    void ShuntModeRequestSeq::runShuntDmiButtonWaitForAck()
    {
      ShuntingAcknowledge shuntingAck;
    
      if (RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus())
      {
        seqState = shuntDmiButtonTCCTimeout;
        ATC::AbstractEventHandler::corePtr()->reportEvent(modTCCNotAvail, __FILE__, __LINE__);
      }
      else
      {
        //apply standstill event
        ATC::AbstractEventHandler::corePtr()->reportEvent(waitForModAckStandStill, __FILE__, __LINE__);

        if (AbstractMessageHandler::corePtr()->getShuntingAcknowledge(shuntingAck))
        {
          if (RequestAcknowledged == shuntingAck.shuntingAcknowledge)
          {
            seqState = shuntDmiButtonConfirmed;
            DS::AbstractTargets::corePtr()->setCurCeilingSpeed(shuntingAck.allowedSpeedInShunting);
            ATC::AbstractEventHandler::corePtr()->reportEvent(modAckReceived, __FILE__, __LINE__);
          }
          else

          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(modeAckRejectNotify, __FILE__, __LINE__);
            seqState = shuntIsDmiButtonPressed;
          }

        }
        else
        {
          // Check if it has taken to many polls to receive the ShuntingAck message
          if (isAckTimeout())
          {
            // Reset to start-state
            seqState = shuntIsDmiButtonPressed;
          }
        }
      }
    }

    /******************************************************************************
    * runShuntDmiButtonConfirmed
    ******************************************************************************/
    void ShuntModeRequestSeq::runShuntDmiButtonConfirmed(CommonDataForModes &commonData)
    {
      commonData.modeReqByDMI = ATPModeShunting;
      DS::AbstractTSetup::corePtr()->removeTrainSetup();
      commonData.isATPReset = false;
      seqState = shuntIsDmiButtonPressed;
    }

    /******************************************************************************
    * runShuntDmiButtonTCCTimeout
    ******************************************************************************/
    void ShuntModeRequestSeq::runShuntDmiButtonTCCTimeout()
    {
      seqState = shuntIsDmiButtonPressed;
    }
    /******************************************************************************
    * validateModes
    ******************************************************************************/
    bool ShuntModeRequestSeq::validateModes()
    {
      bool validMode = false;
      ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
      if ((ATPModeSafetyHalt != currentMode) && (ATPModePoweringDown != currentMode) && (ATPModeShunting != currentMode))
      {
        validMode = true;
      }
      return validMode;
    }

  }
}
