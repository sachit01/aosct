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
* This file implements the creator for the outgoing Manual Configuration Selected DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-21    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_manual_config_selected.hpp"
#include "abstract_mode_control.hpp"

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
    * DMIMessageOutManualConfigSelected constructor
    ******************************************************************************/
    DMIMessageOutManualConfigSelected::DMIMessageOutManualConfigSelected()
      : AbstractDMIMessageOut(MTypeManualConfigSelected)
    {
      messageSent = false;
      
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutManualConfigSelected::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message :Manual Configuration Selected");

        const ATPMode currentMode =
          Kernel::AbstractModeControl::corePtr()->getCurrentMode();
        const Kernel::TrainConfigModeState configState =
          Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();

        if ((currentMode == ATPModeConfiguration)
          && (configState == Kernel::TrainConfigMode::trainConfigWaitNewConfigDMI))
        {
          if (!messageSent)
          {
            if (assembleDMIMessageData())
            {
              dmiDataProcessState = DMIDataValidated;
              messageSent = true;
            }
          }
        }
      }
      return(DMIDataValidated == dmiDataProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageOutManualConfigSelected::invalidate()
    {
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutManualConfigSelected::collectData()
    {
      dmiDataProcessState = DMIDataAvailable;
    }

    /******************************************************************************
    * DMIMessageOutManualConfigSelected::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutManualConfigSelected::assembleDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));

      //Header Type
      messageData.headerType = dmiHeaderTypeAckMsg;
      //Message Number
      messageData.msgNumber = AbstractDMIHandler::corePtr()->getNextMessageNumber();

      //Get MSB for the acknowledged DMIMessageType 
      messageData.dmiData.msgType = static_cast<uint8_t>(static_cast<uint8_t>(messageType) | 0x80U);

      // Assemble data and write in network order


      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }
  }
}
