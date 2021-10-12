/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (TCC->AOS) has an associated parser class inherited from AbstractRadioMessageIn.
* This file implements the parser for the Unregistration message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-26    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "abstract_event_handler.hpp"
#include "radio_message_in_unregistration.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_odometry.hpp"
#include "balise_search_mode.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_message_handler_event_ids.hpp"

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
    * RadioMessageInUnregistration constructor
    ******************************************************************************/
    RadioMessageInUnregistration::RadioMessageInUnregistration() :
      AbstractRadioMessageIn(MTypeUnregister), 
      localUnregInfo(unregUnknownReason),
      // creating different set of objects for different type of events
      unRegMsgReceivedLogEvent(ATC::Event::createLogEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdUnRegMsgRcvdLogEvent,
        0U, "Unregistration message received. ")),
      unRegMsgUnknownReason(ATC::Event::createLogEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdUnknownReasonInUnRegMsg,
        DMICom::noDmi, "Unknown reason unregistration message received. ", true)),
      unRegMsgSafetyHalt(ATC::Event::createSafetyHaltEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdUnRegMsgSafetyHalt,
        ATC::NoEB, DMICom::msgHdlrUnregistrationMsgReceived, "Unregistration message received."))
    {
      implemented = true;
    }

    /******************************************************************************
    * RadioMessageInUnregistration::getUnregInfo
    ******************************************************************************/
    bool RadioMessageInUnregistration::getUnregInfo(UnregInfo & unregInfo) const
    {
      if (DataValidated == dataProcessState)
      {
        unregInfo = localUnregInfo;
      }

      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageInUnregistration::validate
    ******************************************************************************/
    bool RadioMessageInUnregistration::validate()
    {
      trace->write(ATC::briefTrace, "Validating Unregistration");

      bool ret;

      ret = AbstractRadioMessageIn::validate();

      if (ret)
      {
        ret = false;

        // Parse, validate and publish data
        if (DataAvailable == dataProcessState)
        {
          if (parseMessageData())
          {
            if (validateMode())
            {
              dataProcessState = DataValidated;
              ret = true;
            }
          }
        }
      }

      return ret;
    }

    /******************************************************************************
    * RadioMessageInUnregistration::validateMode
    ******************************************************************************/
    bool RadioMessageInUnregistration::validateMode() const
    {
      bool modeValid = false;
      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
        case ATPModeYard:
        case ATPModePossession:
        case ATPModeShunting:
        case ATPModeSleeping:
        case ATPModePowerUp:
        case ATPModePoweringDown:
        case ATPModeSafetyHalt:
        case ATPModeUnregistered:
          // Raise a Log event if unregistration message is received.
          ATC::AbstractEventHandler::corePtr()->reportEvent(unRegMsgReceivedLogEvent, __FILE__, __LINE__);
          break;

        case ATPModeLocation:
          // Raise a Safety Halt event if unregistration message is received.
          ATC::AbstractEventHandler::corePtr()->reportEvent(unRegMsgSafetyHalt, __FILE__, __LINE__);
          break;

        case ATPModeSafeBrakeToStop:
          if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
          {
            modeValid = true;
          }
          else
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(unRegMsgSafetyHalt, __FILE__, __LINE__);
          }
          break;

        case ATPModeBaliseSearch:
        {
          //Fetch the current Balise search mode 
          const BaliseSearchModeState baliseSearchModeState = AbstractModeControl::corePtr()->getBaliseSearchModeState();
          const bool invalidModeState = ((BaliseSearchMode::baliseSearchWaitBalise2 == baliseSearchModeState)
            || (BaliseSearchMode::baliseSearchWaitForBaliseReReg == baliseSearchModeState)
            || (BaliseSearchMode::baliseSearchFinishOK == baliseSearchModeState));

          if (invalidModeState)
          {
            // In balise search if unregistration message received with an active MA raise safety halt event
            ATC::AbstractEventHandler::corePtr()->reportEvent(unRegMsgSafetyHalt, __FILE__, __LINE__);
          }
          else
          {
            // In balise search if unregistration message received before receiving MA
            modeValid = true;
          }
          break;
        }

        case ATPModeNormal:
        case ATPModeStaffResponsible:
        case ATPModeShuntingRoute:
        case ATPModeSplit:
        case ATPModeJoin:
          if (AbstractModeControl::corePtr()->getIdleState())
          {
            modeValid = true;
          }
          else
          {
            // if the train is not in Idle state, raise Safety Halt event.
            ATC::AbstractEventHandler::corePtr()->reportEvent(unRegMsgSafetyHalt, __FILE__, __LINE__);
          }
          break;

        case ATPModeConfiguration:
        case ATPModeRegistration:
          modeValid = true;
          break;

        case ATPModesCount:
        case ATPModeUndefined:
        default:
          ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
          break;
      }
      traceValidateMode(modeValid);
      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInUnregistration::parseMessageData
    ******************************************************************************/
    bool RadioMessageInUnregistration::parseMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));
      vfwSetReadBuffer(&buffer, sizeof(messageData.message.data));

      // Validate NID_MESSAGE_TYPE
      if (vfwGetU8(&buffer) != static_cast<uint8_t>(messageType))
      {
        trace->write(ATC::detailedTrace, "NID_MESSAGE_TYPE invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("NID_MESSAGE_TYPE");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

      // Read & validate Q_UNREGISTRATION
      localUnregInfo = vfwGetU8(&buffer);
      if (isQ_UNREGISTRATIONUnknown(localUnregInfo))
      {
        trace->write(ATC::detailedTrace, "Q_UNREGISTRATION, unknown reason");
        unRegMsgUnknownReason.setDynamicText("Q_UNREGISTRATION unknown");
        ATC::AbstractEventHandler::corePtr()->reportEvent(unRegMsgUnknownReason, __FILE__, __LINE__);
      }

      // Find M_END_Of_MESSAGE or read adaptation blocks
      uint8_t nextMsgIdentifier = vfwGetU8(&buffer);
      while ((nextMsgIdentifier != M_END_OF_MESSAGE) && (parseDataValid))
      {
        parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
        nextMsgIdentifier = vfwGetU8(&buffer);
      }

      if ((!validateSizeOfParsedBytes(&buffer)) && (parseDataValid))
      {
        invalidDataInTCCMessage.setDynamicText("Msg size incorrect");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
        parseDataValid = false;
      }

      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInUnregistration::invalidate
    ******************************************************************************/
    void RadioMessageInUnregistration::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      localUnregInfo = unregUnknownReason;
      dataProcessState = NoDataAvailable;
    }

    /******************************************************************************
    * RadioMessageInUnregistration::isQ_UNREGISTRATIONUnknown
    ******************************************************************************/
    bool RadioMessageInUnregistration::isQ_UNREGISTRATIONUnknown(const UnregInfo val) const
    {
      return (val >= unregUnknownReason);
    }
  }
}
