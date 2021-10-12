/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->TCC) has an associated creator class inherited from AbstractRadioMessageOut.
* This file implements the creator for the AbortSetup message.
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
#include "atp_types.hpp"

#include "abstract_position.hpp"
#include "radio_message_out_abort_setup.hpp"
#include "abstract_mode_control.hpp"
#include "radio_channel.hpp"
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
    * RadioMessageOutAbortSetup constructor
    ******************************************************************************/
    RadioMessageOutAbortSetup::RadioMessageOutAbortSetup() :
      AbstractRadioMessageOut(MTypeAbortSetup), abortSetup(AbortedByAos)
    {
      implemented = true;
    }

    /******************************************************************************
    * RadioMessageOutAbortSetup::collectData
    ******************************************************************************/
    void RadioMessageOutAbortSetup::collectData()
    {
      AbortReason abortSetupReason;
      const bool  sendAbortSetupMessage = Kernel::AbstractModeControl::corePtr()->sendAbortSetupToTCC(abortSetupReason);
      if (sendAbortSetupMessage)
      {
        abortSetup = abortSetupReason;
        dataProcessState = DataAvailable;
      }

    }

    /******************************************************************************
    * RadioMessageOutAbortSetup::validate
    ******************************************************************************/
    bool RadioMessageOutAbortSetup::validate()
    {
      // assemble, validate and publish data
      if (DataAvailable == dataProcessState)
      {
        trace->write(ATC::briefTrace, "Validating AbortSetup");

        if (assembleMessageData())
        {
          dataProcessState = DataValidated;
        }
      }

      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageOutAbortSetup::assembleMessageData
    ******************************************************************************/
    bool RadioMessageOutAbortSetup::assembleMessageData()
    {
      bool assembleDataValid = true;

      // Sanity check of certain (that can be checked) parameter values
      if (!validateQ_ABORT(static_cast<uint8_t>(abortSetup)))
      {
        trace->write(ATC::detailedTrace, "Q_ABORT invalid");
        assembleDataValid = false;
      }

      if (assembleDataValid)
      {
        VFW_Buffer buffer;

        // Initialize buffer to first byte of Application level message
        vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));

        // Assemble all data into net-message format
        vfwPutU8(&buffer, static_cast<uint8_t>(messageType));
        vfwPutU8(&buffer, static_cast<uint8_t>(abortSetup));
        //Add M_END_OF_MESSAGE
        vfwPutU8(&buffer, M_END_OF_MESSAGE);

        // Total length of message
        messageData.message.dataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));
      }

      traceAssembleData(assembleDataValid);

      return assembleDataValid;
    }

    /******************************************************************************
    * RadioMessageOutAbortSetup::invalidate
    ******************************************************************************/
    void RadioMessageOutAbortSetup::invalidate()
    {
      dataProcessState = NoDataAvailable;
      abortSetup = AbortedByAos;
    }

    /******************************************************************************
    * RadioMessageOutDriverInformation::getChannelId
    ******************************************************************************/
    uint16_t RadioMessageOutAbortSetup::getChannelId() const
    {
      return RadioCom::radioChannelRegionBroadcast;
    }
  }
}
