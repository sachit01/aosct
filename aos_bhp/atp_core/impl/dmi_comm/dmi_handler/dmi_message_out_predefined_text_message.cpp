/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->DMI) has an associated creator class inherited from AbstractDMIMessageOut.
* This file implements the creator for the outgoing predefined text message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-04-13   akushwah    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "dmi_message_out_predefined_text_message.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_message_handler.hpp"

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
    * DMIMessageOutErrMessage constructor
    ******************************************************************************/
    DMIMessageOutPredefinedTextMessage::DMIMessageOutPredefinedTextMessage() 
      :AbstractDMIMessageOut(MTypePredefinedTextMessage)
    {
      eaReasonToDMI = static_cast<uint8_t>(Kernel::EmAlertUndefined);
    }

    /******************************************************************************
    * validate()
    ******************************************************************************/
    bool DMIMessageOutPredefinedTextMessage::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message :Predefined Text Message");
        //NO validation check will be performed as it is just a notification to Driver
        if (assembleDMIMessageData())
        {
          dmiDataProcessState = DMIDataValidated;
        }
      }
      return(DMIDataValidated == dmiDataProcessState);
    }


    /******************************************************************************
    * invalidate()
    ******************************************************************************/
    void DMIMessageOutPredefinedTextMessage::invalidate()
    {
      //reset the values 
      eaReasonToDMI = static_cast<uint8_t>(Kernel::EmAlertUndefined);
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * assembleDMIMessageData()
    ******************************************************************************/
    bool DMIMessageOutPredefinedTextMessage::assembleDMIMessageData()
    {
      bool parseDataValid = true;
      VFW_Buffer buffer;
      //unused field for confirmation requested in Predefined Text message
      const uint8_t unUsedField = 0U;
      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));
      //Header type
      messageData.headerType = dmiHeaderTypeAckMsg;
      // Message Number
      messageData.msgNumber = AbstractDMIHandler::corePtr()->getNextMessageNumber();
      //Get MSB for the acknowledged DMIMessageType 
      messageData.dmiData.msgType = static_cast<uint8_t>(static_cast<uint8_t>(messageType) | 0x80U);
      //Put Confirmation requested
      vfwPutU8(&buffer, unUsedField);
      //Put Predefined Text message specifier for EA
      vfwPutU8(&buffer, eaReasonToDMI);
      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * collectData()
    ******************************************************************************/
    void DMIMessageOutPredefinedTextMessage::collectData()
    {
      if (Kernel::AbstractMessageHandler::corePtr()->getEmAlertReason(eaReasonToDMI))
      {
        dmiDataProcessState = DMIDataAvailable;
      }
      else
      {
        trace->write(ATC::detailedTrace, "DMI Handler: Invalid EA Reason");
      }
    }

  }
}
