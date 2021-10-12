/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->DMI) has an associated creator class inherited from AbstractDMIMessageOut.
* This file implements the creator for the outgoing Re-Registration Selected DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-25    marlundg    Created 
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_rereg_selected.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tsetup.hpp"

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
  namespace DMICom
  {
    /******************************************************************************
    * DMIMessageOutReRegSelected constructor
    ******************************************************************************/
    DMIMessageOutReRegSelected::DMIMessageOutReRegSelected() : AbstractDMIMessageOut(MTypeReconfigurationSelected)
    {
      timsRelatedData = 0U;

    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutReRegSelected::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message :ReConfigurationSelected");

        if (assembleDMIMessageData())
        {
          dmiDataProcessState = DMIDataValidated;
        }
      }

      return(DMIDataValidated == dmiDataProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageOutReRegSelected::invalidate()
    {
      // Clear all data
      timsRelatedData = 0U;
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutReRegSelected::collectData()
    {

      if (Kernel::AbstractModeControl::corePtr()->getCurrentMode() == ATPModeConfiguration)
      {
        if (Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState()
          == Kernel::TrainConfigMode::trainConfigSendReRegDataToDMI)
        {
          DS::TrainSetup trainSetup;

          bool trainSetupvalid = DS::AbstractTSetup::corePtr()->getPreliminaryTrainSetup(trainSetup);

          if (trainSetupvalid)
          {
            if (trainSetup.timsSupNeeded)
            {
              // Set the bit related to timsRequired
              timsRelatedData = timsRelatedData | 0x01U;
            }

            if ((trainSetup.orientation & trainLocoOrientation) > 0U)
            {
              // Set the bit for Car Connected as Cars connected at A side
              timsRelatedData = timsRelatedData | 0x02U;
            }

            dmiDataProcessState = DMIDataAvailable;

          }
          else
          {
            trace->write(ATC::veryDetailedTrace, "DMI Handler: Invalid Train Setup");
            writeToLog(ATC::VeryDetailedLog, "DMI Handler: Invalid Train Setup", __FILE__, __LINE__);
          }
        }
      }
    }

    /******************************************************************************
    * assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutReRegSelected::assembleDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));

      // Header Type
      messageData.headerType = dmiHeaderTypeAckMsg;
      // Message Number
      messageData.msgNumber = AbstractDMIHandler::corePtr()->getNextMessageNumber();

      // Get MSB for the acknowledged DMIMessageType 
      messageData.dmiData.msgType = static_cast<uint8_t>(static_cast<uint8_t>(messageType) | 0x80U);

      // Assemble data and write in network order
      vfwPutU8(&buffer, timsRelatedData);

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      // Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }
  }
}
