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
* This file implements the creator for the atp notification message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-26    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include <vfw_string.h>

#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_atp_notification.hpp"
#include "abstract_message_handler.hpp"
#include "atc_math.hpp"

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
    // The below variables are commented as they were raising Lint error and
    // needs to be uncommented when are used in the project.

    // ATP-Notifications
    // Uses # and a number to use a string to be translated by MMI

    /******************************************************************************
    * Index Format for DMI
    ******************************************************************************/
    const char_t * const formatIndex = "#";

    /******************************************************************************
    * DMIMessageOutAtpNotification constructor
    ******************************************************************************/
    DMIMessageOutAtpNotification::DMIMessageOutAtpNotification() : AbstractDMIMessageOut(MTypeATPNotification)
    {
      memset(&text[0], 0, sizeof(text));
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutAtpNotification::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message :ATP Notification");

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
    void DMIMessageOutAtpNotification::invalidate()
    {
      memset(&text[0], 0, sizeof(text));
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutAtpNotification::collectData()
    {
      Kernel::RejectConfigInfo  infoReason;
      Kernel::UnregInfo unregInfo;

      const char_t* const textCommandMessage = Kernel::AbstractMessageHandler::corePtr()->getTextMessage();
      if (textCommandMessage != static_cast<char_t *>(NULL))
      {
        //Text-message received from TCC in Command Message 
        static_cast<void>(vfw_strlcpy(&text[0], textCommandMessage, sizeof(text)));
        dmiDataProcessState = DMIDataAvailable;
      }
      else if (Kernel::AbstractMessageHandler::corePtr()->getRejectConfigurationInfo(infoReason))
      {
        createATPNoficiationText(infoReason, Kernel::rejectConfigUnknownReason);
      }
      else if (Kernel::AbstractMessageHandler::corePtr()->getUnregInfo(unregInfo))
      {
        // Numbering for Unregistration reason in DMI starts after a gap from reject configuration reasons
        const uint16_t dmiUnregInfoNo = static_cast<uint16_t>(unregInfo) + dmiScalerForUnregReason;
        const uint16_t dmiUnregUnknownReasonNo = Kernel::unregUnknownReason + dmiScalerForUnregReason;
        createATPNoficiationText(dmiUnregInfoNo, dmiUnregUnknownReasonNo);
      }
      else
      {
        //Invalid data
        trace->write(ATC::briefTrace, "DMI Handler: Invalid Data");
      }
    }

    /******************************************************************************
    * DMIMessageOutAtpNotification::createATPNoficiationText
    ******************************************************************************/
    void DMIMessageOutAtpNotification::createATPNoficiationText(const uint16_t info, const uint16_t unknownReasonValue)
    {
      char_t indexInstring[textSize];
      memset(&indexInstring[0], 0, textSize);

      //Convert the number to string
      //lint -e{586} snprintf is needed here
      int32_t retValue = snprintf(&indexInstring[0], textSize, "%u", ATC::ATCMath::minimum(info, unknownReasonValue));

      if ((retValue > 0) && (static_cast<size_t>(retValue) < textSize))
      {
        //Add the specifier:”#”
        static_cast<void>(vfw_strlcpy(&text[0], formatIndex, sizeof(text)));
        //Concatenate the id with
        static_cast<void>(vfw_strlcat(&text[0], &indexInstring[0], sizeof(text)));
        dmiDataProcessState = DMIDataAvailable;
      }
    }

    /******************************************************************************
    * DMIMessageOutTrainName::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutAtpNotification::assembleDMIMessageData()
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
      // Mode set to 0
      vfwPutU8(&buffer, 0U);

      // Message
      vfwCpyFromRawBuffer(&buffer, &text[0], static_cast<uint32_t>(textSize));
      
      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      // Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }
  }
}
