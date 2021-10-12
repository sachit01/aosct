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
* This file implements the creator for the outgoing Driver Info DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-21    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
* 2016-10-12    arastogi    Added getting direction in collect data
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "dmi_message_out_dmi_startup_history.hpp"
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_tsetup.hpp"
#include <vfw_string.h>
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
    * Constructor
    ******************************************************************************/
    DMIMessageOutDMIStartupHistory::DMIMessageOutDMIStartupHistory() : AbstractDMIMessageOut(MTypeStartupHistory)
    {
      memset(&textBuff[0], 0, sizeof(textBuff));
      isDataAvailable = false;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutDMIStartupHistory::validate()
    {
      // validate and assemble data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message :Startup History");
        writeToLog(ATC::DetailedLog, "DMI Handler: Validating DMI Message :Startup History", __FILE__, __LINE__);

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
    void DMIMessageOutDMIStartupHistory::invalidate()
    {
      // No Data available: Invalidated
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutDMIStartupHistory::collectData()
    {
      if (isDataAvailable)
      {
        dmiDataProcessState = DMIDataAvailable;
      }
    }

    /******************************************************************************
    * assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutDMIStartupHistory::assembleDMIMessageData()
    {
      bool parseDataValid = true;
      uint16_t lengthOfText;

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
      lengthOfText = static_cast<uint16_t>(strnlen(&textBuff[0], maxStartupHistoryText));
      vfwPutU16(&buffer, lengthOfText);
      vfwCpyFromRawBuffer(&buffer, &textBuff[0], lengthOfText);

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);
      //reset the buffer
      isDataAvailable = false;
      memset(&textBuff[0], 0, sizeof(textBuff));
      return parseDataValid;
    }

    /******************************************************************************
    * addStartupMsg
    ******************************************************************************/
    void DMIMessageOutDMIStartupHistory::addStartupMsg(const char_t * const str)
    {
      //Get the message length
      uint16_t strLength = static_cast<uint16_t>(strnlen(str, maxStartupHistoryText));
      if (maxStartupHistoryText == strLength)
      {
        trace->write(ATC::briefTrace, "DMI Handler: StartUp History buffer Overflow");
        writeToLog(ATC::BriefLog, "DMI Handler: StartUp History buffer Overflow", __FILE__, __LINE__);
      }
      else
      {
        static_cast<void>(vfw_strlcat(&textBuff[0], str, sizeof(textBuff)));
        isDataAvailable = true;
      }
    }

  }
}
