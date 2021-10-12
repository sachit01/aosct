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
* This file implements the creator for the outgoing error message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-03-09   spandita    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "dmi_message_out_text_message.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_message_handler.hpp"
#include "radio_message_types.hpp"
#include "dmi_event_codes.hpp"
#include <vfw_string.h>
#include <cstdio>

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
    * Index Format for DMI
    ******************************************************************************/
    const char_t * const formatIndex = "$#$";

    /******************************************************************************
    * DMIMessageOutErrMessage constructor
    ******************************************************************************/
    DMIMessageOutTextMessage::DMIMessageOutTextMessage() :AbstractDMIMessageOut(MTypeTextMessage)
    {
      memset(&dmiTextMsgIndex[0], 0, sizeof(dmiTextMsgIndex));

    }

    /******************************************************************************
    * validate()
    ******************************************************************************/
    bool DMIMessageOutTextMessage::validate()
    {
      // Assemble, validate and publish data
      if (DMICom::DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message : Text Message");
        //NO validation check will be performed as it is just a notification to user
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
    void DMIMessageOutTextMessage::invalidate()
    {
      //reset the values 
      memset(&dmiTextMsgIndex[0], 0, sizeof(dmiTextMsgIndex));
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * assembleDMIMessageData()
    ******************************************************************************/
    bool DMIMessageOutTextMessage::assembleDMIMessageData()
    {
      bool parseDataValid = true;
      VFW_Buffer buffer;
      //unused field for confirmation requested in Text message
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
      //Put the data 
      vfwCpyFromRawBuffer(&buffer, &dmiTextMsgIndex[0], static_cast<uint32_t>(maxDmiTextMsgIndexlen));
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
    void DMIMessageOutTextMessage::collectData()
    {
        //Get the event if any for DMI    
        const ATC::Event* event = ATC::AbstractEventHandler::corePtr()->getNextDmiEvent();
        char_t indexInstring[maxDmiTextMsgIndexlen];
        //check for null event
        if (event != static_cast<ATC::Event*>(NULL))
        {
          //Convert the number to string
          //lint -e{586} snprintf is needed here
          const int32_t retValue = snprintf(&indexInstring[0], maxDmiTextMsgIndexlen, "%u", event->getDmiEventCode());
          if ((retValue > 0) && (static_cast<size_t>(retValue) < maxDmiTextMsgIndexlen))
          {
            //Add the specifier:”$#$”
            static_cast<void>(vfw_strlcpy(&dmiTextMsgIndex[0], formatIndex, sizeof(dmiTextMsgIndex)));
            //Concatenate the id with $#$
            static_cast<void>(vfw_strlcat(&dmiTextMsgIndex[0], &indexInstring[0], sizeof(dmiTextMsgIndex)));
            dmiDataProcessState = DMIDataAvailable;
          }
        }
      }
  }
}
