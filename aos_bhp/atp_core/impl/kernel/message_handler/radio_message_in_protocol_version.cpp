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
* This file implements the parser for the DriverLogonStatus message.
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
#include "radio_message_in_protocol_version.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_mode_control.hpp"
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
    * RadioMessageInProtocolVersion constructor
    ******************************************************************************/
    RadioMessageInProtocolVersion::RadioMessageInProtocolVersion()
      : AbstractRadioMessageIn(MTypeProtocolVersion),
      protocolVerUnrecovMismatch(ATC::Event::createLogEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdProtocolVersionUnrecovMismatch,
        0x0U, "TCC-AOS Unrecoverable mismatch in Major Protocol Version."))
    {
      implemented = true;
      incomingProtocolVersionData.incomingProtocolVersion = ProtocolVersion();
      incomingProtocolVersionData.incomingProtocolResponse = TCCRequest;
    }

    /******************************************************************************
    * RadioMessageInProtocolVersion::validate
    ******************************************************************************/
    bool RadioMessageInProtocolVersion::validate()
    {
      trace->write(ATC::briefTrace, "Validating Protocol Version");

      // Parse, validate and publish data.
      // No call to base class method to check for protocol version in AbstractRadioMessageIn::validate(), 
      // since the ProtocolVersion message itself shall always be processed.
      if (DataAvailable == dataProcessState)
      {
        if (parseMessageData())
        {
          if (validateMode())
          {
            publishData();
            dataProcessState = DataValidated;
          }
        }
      }

      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageInProtocolVersion::parseMessageData
    ******************************************************************************/
    bool RadioMessageInProtocolVersion::parseMessageData()
    {
      bool parseDataValid = true;
      uint8_t tmpValU8;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));
      vfwSetReadBuffer(&buffer, sizeof(messageData.message.data));

      // Read & validate NID_MESSAGE_TYPE
      if (vfwGetU8(&buffer) != static_cast<uint8_t>(messageType))
      {
        trace->write(ATC::detailedTrace, "NID_MESSAGE_TYPE invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("NID_MESSAGE_TYPE");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

      // Read & validate Q_PROTOCOL_RESPONSE
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateIncomingQ_PROTOCOL_RESPONSE(tmpValU8))
      {
        trace->write(ATC::detailedTrace, "Q_PROTOCOL_RESPONSE invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("Q_PROTOCOL_RESPONSE");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }
      else
      {
        incomingProtocolVersionData.incomingProtocolResponse = static_cast<ProtocolResponse>(tmpValU8);
      }

      // Read & validate PROTOCOL_VERSION Block
      if (vfwGetU8(&buffer) != BTypeProtocolVersion)
      {
        trace->write(ATC::detailedTrace, "PROTOCOL_VERSION invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("PROTOCOL_VERSION");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }
      else
      {
        //Major version
        incomingProtocolVersionData.incomingProtocolVersion.majorVersion = vfwGetU8(&buffer);
        //Minor version
        incomingProtocolVersionData.incomingProtocolVersion.minorVersion = vfwGetU8(&buffer);
      }

      // Fetch M_END_OF_MESSAGE
      uint8_t nextMsgIdentifier = vfwGetU8(&buffer);
      while ((nextMsgIdentifier != M_END_OF_MESSAGE) && (parseDataValid))
      {
        parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
        nextMsgIdentifier = vfwGetU8(&buffer);
      }

      if ((!validateSizeOfParsedBytes(&buffer)) && (parseDataValid))
      {
        trace->write(ATC::detailedTrace, "Invalid size of parsed bytes");
        invalidDataInTCCMessage.setDynamicText("Msg size incorrect");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
        parseDataValid = false;
      }

      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInProtocolVersion::invalidate
    ******************************************************************************/
    void RadioMessageInProtocolVersion::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      // Reset
      incomingProtocolVersionData.incomingProtocolVersion = ProtocolVersion();
      incomingProtocolVersionData.incomingProtocolResponse = TCCRequest;
      dataProcessState = NoDataAvailable;
    }

    /******************************************************************************
    * RadioMessageInProtocolVersion::getProtocolVersionFromTCC
    ******************************************************************************/
    bool RadioMessageInProtocolVersion::getProtocolVersionFromTCC(ProtocolVersion &protocolVersionFromTCC,
      ProtocolResponse &protocolVersionRequest) const
    {
      if (DataValidated == dataProcessState)
      {
        //Major version
        protocolVersionFromTCC.majorVersion = incomingProtocolVersionData.incomingProtocolVersion.majorVersion;
        //Minor version
        protocolVersionFromTCC.minorVersion = incomingProtocolVersionData.incomingProtocolVersion.minorVersion;

        //Request for check
        protocolVersionRequest = incomingProtocolVersionData.incomingProtocolResponse;
      }

      return (DataValidated == dataProcessState);
    }


    /******************************************************************************
    * RadioMessageInProtocolVersion::validateMode
    ******************************************************************************/
    bool RadioMessageInProtocolVersion::validateMode() const
    {
      bool modeValid = false;

      // Fetch the current  ATP mode 
      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
        case ATPModeYard:
        case ATPModePossession:
        case ATPModeShunting:
        case ATPModePowerUp:
        case ATPModeConfiguration:
        case ATPModeRegistration:
        case ATPModeBaliseSearch:
        case ATPModeNormal:
        case ATPModeLocation:
        case ATPModeUnregistered:
        case ATPModePoweringDown:
        case ATPModeSafetyHalt:
        case ATPModeStaffResponsible:
        case ATPModeShuntingRoute:
        case ATPModeSplit:
        case ATPModeJoin:
        case ATPModeSafeBrakeToStop:
          modeValid = true;
          break;
        case ATPModeSleeping:
        {
          if (IO::AbstractLocoIO::corePtr()->getSleepingSignal())
          {
            //Reject the incoming message from TCC when the sleep signal is active
            modeValid = false;
            invalidIncmgMsgTCC.setDynamicText("Protocol Version");
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
          }
          else
          {
            // Accept the incoming message from TCC when the sleep signal is inactive
            modeValid = true;
          }
          break;
        }
        case ATPModeUndefined:
        case ATPModesCount:
        default:
        {
          ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
          break;
        }
      }
      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInProtocolVersion::publishData
    ******************************************************************************/
    void RadioMessageInProtocolVersion::publishData() const
    {
      // Check if the protocol version message received has unrecoverable mismatch
      if (TCCResponseUnrecoverableMisMatch == incomingProtocolVersionData.incomingProtocolResponse)
      {
        // Remember Mismatch status
        Kernel::AbstractMessageHandler::corePtr()->setProtocolVersionStatus(false, messageData.channelId);

        // Issue a log event
        ATC::AbstractEventHandler::corePtr()->reportEvent(protocolVerUnrecovMismatch, __FILE__, __LINE__);
      }
      // Protocol Check Request
      else
      {
        // Get the AOS version
        const ProtocolVersion& currentAOSProtocolVersion = Kernel::AbstractMessageHandler::corePtr()->getProtocolVersion();

        const bool protocolVersionIsOk = (currentAOSProtocolVersion == incomingProtocolVersionData.incomingProtocolVersion);
        // Remember Mismatch status
        Kernel::AbstractMessageHandler::corePtr()->setProtocolVersionStatus(protocolVersionIsOk, messageData.channelId);
      }
    }

  }
}
