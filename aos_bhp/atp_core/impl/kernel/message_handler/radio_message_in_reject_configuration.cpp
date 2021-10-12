/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
* Each messageType (TCC->AOS) has an associated parser class inherited from AbstractRadioMessageIn.
* This file implements the parser for the RejectConfiguration message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-02-28    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_mode_control.hpp"
#include "radio_message_in_reject_configuration.hpp"
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
    * RadioMessageInRejectConfiguration constructor
    ******************************************************************************/
    RadioMessageInRejectConfiguration::RadioMessageInRejectConfiguration() 
      : AbstractRadioMessageIn(MTypeRejectConfiguration),
      rejectConfigReason(rejectConfigUnknownReason),
      unknownReasonRejectConfigByTCC(ATC::Event::createLogEvent(atpMessageHandlerId, ATC::CoreContainer,
        eventIdUnknownReasonInRejectConfigMsg, DMICom::noDmi, "Unknown reason reject configuration message from TCC:", true))
    {
      implemented = true;
    }

    /******************************************************************************
    * RadioMessageInRejectConfiguration::validate
    ******************************************************************************/
    bool RadioMessageInRejectConfiguration::validate()
    {
      trace->write(ATC::briefTrace, "Validating RejectConfiguration");

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
    * RadioMessageInRejectConfiguration::validateMode
    ******************************************************************************/
    bool RadioMessageInRejectConfiguration::validateMode() const
    {
      bool modeValid = false;

      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
        case ATPModeConfiguration:
          modeValid = true;
          break;

        case ATPModeYard:
        case ATPModeShunting:
        case ATPModePossession:
        case ATPModeSleeping:
          //Reject the incoming message from TCC
          invalidIncmgMsgTCC.setDynamicText("RejectConfiguration");
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
          break;

        case ATPModePowerUp:
        case ATPModeBaliseSearch:
        case ATPModeRegistration:
        case ATPModeNormal:
        case ATPModeSafetyHalt:
        case ATPModeLocation:
        case ATPModeStaffResponsible:
        case ATPModeSplit:
        case ATPModeJoin:
        case ATPModeShuntingRoute:
        case ATPModePoweringDown:
        case ATPModeUnregistered:
        case ATPModeSafeBrakeToStop:
          break;

        case ATPModeUndefined:
        case ATPModesCount:
        default:
          ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
          break;
      }
      traceValidateMode(modeValid);
      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInRejectConfiguration::parseMessageData
    ******************************************************************************/
    bool RadioMessageInRejectConfiguration::parseMessageData()
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

      // Read & validate Q_REJECT_CONFIGURATION
      rejectConfigReason = vfwGetU8(&buffer);
      if (isQ_REJECTCONFIGURATIONUnknown(rejectConfigReason))
      {
        trace->write(ATC::detailedTrace, "Q_REJECT_CONFIGURATION, unknown reason");
        unknownReasonRejectConfigByTCC.setDynamicText("Q_REJECT_CONFIGU...");
        ATC::AbstractEventHandler::corePtr()->reportEvent(unknownReasonRejectConfigByTCC, __FILE__, __LINE__);
      }

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
    * RadioMessageInRejectConfiguration::invalidate
    ******************************************************************************/
    void RadioMessageInRejectConfiguration::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      rejectConfigReason = rejectConfigUnknownReason;
      dataProcessState = NoDataAvailable;
    }

    /******************************************************************************
    * RadioMessageInRejectConfiguration::getRejectConfigurationReason
    ******************************************************************************/
    bool RadioMessageInRejectConfiguration::getRejectConfigurationReason(RejectConfigInfo & rejectConfigurationReason) const
    {
      if (DataValidated == dataProcessState)
      {
        rejectConfigurationReason = rejectConfigReason;
      }

      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageInRejectConfiguration::isQ_REJECTCONFIGURATIONUnknown
    ******************************************************************************/
    bool RadioMessageInRejectConfiguration::isQ_REJECTCONFIGURATIONUnknown(const RejectConfigInfo val) const
    {
      return (val >= rejectConfigUnknownReason);
    }
  }
}
