/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->DMI) has an associated creator class inherited from AbstractDMIMessageOut.
* This file implements the creator for the outgoing estimated time of arrival DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-08-14    spandita    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_eta_bhp.hpp"
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
    * DMIMessageOutETABHP constructor
    ******************************************************************************/
    DMIMessageOutETABHP::DMIMessageOutETABHP() :AbstractDMIMessageOut(MTypeDMIMessageETA)
    {
      estimatedTime = 0U;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutETABHP::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
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
    void DMIMessageOutETABHP::invalidate()
    {
      //clear all data
      estimatedTime = 0U;
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutETABHP::collectData()
    {
      const Kernel::Path* const pathMessage = Kernel::AbstractMessageHandler::corePtr()->getPath();

      if (pathMessage != static_cast<Kernel::Path*>(NULL))
      {
        dmiDataProcessState = DMIDataAvailable;
        estimatedTime = pathMessage->etaRequestStruct.nextTargetArrivalTime;
      }
    }

    /******************************************************************************
    * DMIMessageOutRadioAreaBHP::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutETABHP::assembleDMIMessageData()
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
      vfwPutU64(&buffer, estimatedTime);

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }
  }
}
