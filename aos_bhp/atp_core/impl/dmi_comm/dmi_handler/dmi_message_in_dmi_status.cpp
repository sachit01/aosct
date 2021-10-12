/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each DMI messageType (DMI->AOS) has an associated parser class inherited from AbstractDMIMessageIn.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-13    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_in.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_in_dmi_status.hpp"

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
    * DMIMessageInDMIStatus constructor
    ******************************************************************************/
    DMIMessageInDMIStatus::DMIMessageInDMIStatus() : AbstractDMIMessageIn(MTypeDMIStatus)
    {
      dmiStatusWord = 0U;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageInDMIStatus::validate()
    {
      trace->write(ATC::briefTrace, "Validating DMI Incoming Message :DMI Status");

      // Parse, validate and publish data
      if (DMIDataIncomingAvailable == dmiDataInProcessState)
      {
        if (parseDMIMessageData())
        {
          dmiDataInProcessState = DMIDataIncomingValidated;
        }
      }

      return (DMIDataIncomingValidated == dmiDataInProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageInDMIStatus::invalidate()
    {
      dmiStatusWord = 0U;
      dmiDataInProcessState = DMINoDataIncomingAvailable;
    }

    /******************************************************************************
    * logToRU
    ******************************************************************************/
    void DMIMessageInDMIStatus::logToRU() const
    {
      static uint16_t lastStatusWord = 0U;

      if (dmiStatusWord != lastStatusWord)
      {
        lastStatusWord = dmiStatusWord;

        AbstractDMIMessageIn::logToRU();
      }
    }

    /******************************************************************************
    * getDMIStatusWord
    ******************************************************************************/
    uint16_t DMIMessageInDMIStatus::getDMIStatusWord() const
    {
      uint16_t tempStatusWord;

      if (DMIDataIncomingValidated == dmiDataInProcessState)
      {
        tempStatusWord = dmiStatusWord;
      }
      else
      {
        tempStatusWord = 0U;
      }

      return tempStatusWord;
    }

    /******************************************************************************
    * DMIMessageInDMIStatus::parseMessageData
    ******************************************************************************/
    bool DMIMessageInDMIStatus::parseDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));
      vfwSetReadBuffer(&buffer, sizeof(messageData.dmiData.msgData));

      //Get the DMI Status Word
      dmiStatusWord = vfwGetU16(&buffer);

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

  }
}
