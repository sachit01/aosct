/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (DMI->AOS) has an associated parser class inherited from AbstractDMIMessageIn.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-12-11    akushwah    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_in.hpp"
#include "dmi_message_in_train_name.hpp"
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
    * DMIMessageInTrainName constructor
    ******************************************************************************/
    DMIMessageInTrainName::DMIMessageInTrainName() : AbstractDMIMessageIn(MTypeTrainName)
    {
      memset(&changedTrainName[0], 0, sizeof(changedTrainName));
    }

    /******************************************************************************
     * DMIMessageInTrainName::validate
     ******************************************************************************/
    bool DMIMessageInTrainName::validate()
    {
      trace->write(ATC::briefTrace, "Validating DMI Incoming Message :Changed Train Name");

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
    * DMIMessageInTrainName::invalidate
    ******************************************************************************/
    void DMIMessageInTrainName::invalidate()
    {
      memset(&changedTrainName[0], 0, sizeof(changedTrainName));
      dmiDataInProcessState = DMINoDataIncomingAvailable;
    }

    /******************************************************************************
    * DMIMessageInTrainName::getChangedTrainName
    ******************************************************************************/
    bool DMIMessageInTrainName::getChangedTrainName(char_t* const trainName) const
    {
      if (DMIDataIncomingValidated == dmiDataInProcessState)
      {
        static_cast<void>(vfw_strlcpy(trainName, &changedTrainName[0], trainNameMaxLength + 1U));
      }

      return (DMIDataIncomingValidated == dmiDataInProcessState);
    }

    /******************************************************************************
    * DMIMessageInTrainName::parseMessageData
    ******************************************************************************/
    bool DMIMessageInTrainName::parseDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));
      vfwSetReadBuffer(&buffer, sizeof(messageData.dmiData.msgData));


      //Get the Changed Train Name, null terminate
      vfwCpyToRawBuffer(&changedTrainName[0], &buffer, static_cast<uint32_t>(trainNameMaxLength));
      changedTrainName[trainNameMaxLength] = '\0';

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

  }
}
