/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->TCC) has an associated creator class inherited from AbstractRadioMessageOut.
* This file implements the creator for the StartupMessage message.
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
#include "radio_message_out_protocol_version.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_radio_message_common.hpp"


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
    * RadioMessageOutProtocolVersion constructor
    ******************************************************************************/
    RadioMessageOutProtocolVersion::RadioMessageOutProtocolVersion() : AbstractRadioMessageOut(MTypeProtocolVersion)

    {
      implemented = true;
      outgoingProtocolVersionData.outgoingProtocolVersion = ProtocolVersion();
      outgoingProtocolVersionData.outgoingProtocolResponse = TCCRequest;
    }

    /******************************************************************************
    * RadioMessageOutProtocolVersion::collectData
    ******************************************************************************/
    void RadioMessageOutProtocolVersion::collectData()
    {
      ProtocolVersion protocolVersionFromTCC;
      ProtocolResponse protocolVersionRequest;

      // Check if a ProtocolVersion is received from TCC and a response shall be generated.
      if (Kernel::AbstractMessageHandler::corePtr()->getIncomingProtocolVersionFromTCC(protocolVersionFromTCC, protocolVersionRequest))
      {
        // A request from TCC shall be responded.
        if (protocolVersionRequest == TCCRequest)
        {
          const ProtocolVersion& currentAOSProtocolVersion = Kernel::AbstractMessageHandler::corePtr()->getProtocolVersion();

          outgoingProtocolVersionData.outgoingProtocolVersion.majorVersion = currentAOSProtocolVersion.majorVersion;
          outgoingProtocolVersionData.outgoingProtocolVersion.minorVersion = currentAOSProtocolVersion.minorVersion;

          // Protocol Match or Mismatch?
          if (currentAOSProtocolVersion == protocolVersionFromTCC)
          {
            outgoingProtocolVersionData.outgoingProtocolResponse = AOSResponseMatch;
          }
          else
          {
            outgoingProtocolVersionData.outgoingProtocolResponse = AOSResponseMisMatch;
          }

          dataProcessState = DataAvailable;
        }
        else
        {
          // Don't generate a response if TCCResponseUnrecoverableMisMatch is received.
        }
      }
    }

    /******************************************************************************
    * RadioMessageOutProtocolVersion::validate
    ******************************************************************************/
    bool RadioMessageOutProtocolVersion::validate()
    {
      // assemble, validate and publish data
      if (DataAvailable == dataProcessState)
      {
        trace->write(ATC::briefTrace, "Validating Outgoing ProtocolVersion");

        if (assembleMessageData())
        {
          dataProcessState = DataValidated;
        }
      }

      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageOutProtocolVersion::assembleMessageData
    ******************************************************************************/
    bool RadioMessageOutProtocolVersion::assembleMessageData()
    {
      bool assembleDataValid = true;

      // Sanity check of certain (that can be checked) parameter values: Q_PROTOCOL_RESPONSE
      if (!validateOutgoingQ_PROTOCOL_RESPONSE(static_cast<uint8_t>(outgoingProtocolVersionData.outgoingProtocolResponse)))
      {
        trace->write(ATC::detailedTrace, "Q_PROTOCOL_RESPONSE invalid");
        assembleDataValid = false;
      }

      if (assembleDataValid)
      {
        VFW_Buffer buffer;

        // Initialize buffer to first byte of Application level message
        vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));

        // Assemble all data into net-message format
        vfwPutU8(&buffer, static_cast<uint8_t>(messageType));

        // Assemble Q_PROTOCOL_RESPONSE into net-message format
        vfwPutU8(&buffer, static_cast<uint8_t>(outgoingProtocolVersionData.outgoingProtocolResponse));

        // Assemble PROTOCOL_VERSION into net-message format
        vfwPutU8(&buffer, BTypeProtocolVersion);
        vfwPutU8(&buffer, outgoingProtocolVersionData.outgoingProtocolVersion.majorVersion);
        vfwPutU8(&buffer, outgoingProtocolVersionData.outgoingProtocolVersion.minorVersion);

        // Assemble M_END_OF_MESSAGE into net-message format
        vfwPutU8(&buffer, M_END_OF_MESSAGE);

        // Total length of message
        messageData.message.dataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));
      }

      traceAssembleData(assembleDataValid);

      return assembleDataValid;
    }

    /******************************************************************************
    * RadioMessageOutProtocolVersion::invalidate
    ******************************************************************************/
    void RadioMessageOutProtocolVersion::invalidate()
    {
      outgoingProtocolVersionData.outgoingProtocolVersion = ProtocolVersion();
      outgoingProtocolVersionData.outgoingProtocolResponse = TCCRequest;
      dataProcessState = NoDataAvailable;
    }

  }
}
