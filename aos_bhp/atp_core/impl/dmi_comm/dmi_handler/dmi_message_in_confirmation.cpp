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
#include "dmi_message_in_confirmation.hpp"

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
    * DMIMessageInConfirmation constructor
    ******************************************************************************/
    DMIMessageInConfirmation::DMIMessageInConfirmation() : AbstractDMIMessageIn(MTypeConfirmation)
    {
      confirmationData = DMIATPConfirmationUndefined;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageInConfirmation::validate()
    {
      trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Incoming Message :Confirmation");

      // Parse, validate and publish data
      if (DMIDataIncomingAvailable == dmiDataInProcessState)
      {
        if (parseDMIMessageData())
        {
          if (validateMode())
          {
            dmiDataInProcessState = DMIDataIncomingValidated;
          }
        }
      }

      return (DMIDataIncomingValidated == dmiDataInProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageInConfirmation::invalidate()
    {
      confirmationData = DMIATPConfirmationUndefined;
      dmiDataInProcessState = DMINoDataIncomingAvailable;
    }

    /******************************************************************************
    * getConfirmation
    ******************************************************************************/
    Confirmation DMIMessageInConfirmation::getConfirmation() const
    {
      Confirmation tempConfirmation;

      if (DMIDataIncomingValidated == dmiDataInProcessState)
      {
        tempConfirmation = confirmationData;
      }
      else
      {
        tempConfirmation = DMIATPConfirmationUndefined;
      }

      return tempConfirmation;
    }

    /******************************************************************************
    * DMIMessageInConfirmation::parseMessageData
    ******************************************************************************/
    bool DMIMessageInConfirmation::parseDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));
      vfwSetReadBuffer(&buffer, sizeof(messageData.dmiData.msgData));

      uint8_t tempConfirmationData = vfwGetU8(&buffer);

      if (!(tempConfirmationData <= static_cast<uint8_t>(DMIATPConfirmationNOK)))
      {
        trace->write(ATC::detailedTrace, "DMI Handler: Invalid Confirmation Status");
        writeToLog(ATC::DetailedLog, "DMI Handler: Invalid Confirmation Status", __FILE__, __LINE__);
        parseDataValid = false;
      }
      else
      {
        confirmationData = static_cast<Confirmation>(tempConfirmationData);
      }

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * DMIMessageInConfirmation::validateMode
    ******************************************************************************/
    bool DMIMessageInConfirmation::validateMode() const
    {
      bool modeValid = true;

      //TODO: Confirmation data is based on the text & predefined text message

      //Write the Trace regarding Mode 
      traceValidateMode(modeValid);

      return modeValid;
    }

  }
}
