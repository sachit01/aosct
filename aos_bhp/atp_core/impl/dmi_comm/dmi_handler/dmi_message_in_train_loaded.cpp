/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
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
* 2018-12-05    csundin     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "dmi_message_in_train_loaded.hpp"
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
    * DMIMessageInTrainLoaded constructor
    ******************************************************************************/
    DMIMessageInTrainLoaded::DMIMessageInTrainLoaded() : AbstractDMIMessageIn(MTypeTrainName)
    {
      trainLoadedRequested = TrainIsLoaded;
    }

    /******************************************************************************
     * DMIMessageInTrainLoaded::validate
     ******************************************************************************/
    bool DMIMessageInTrainLoaded::validate()
    {
      trace->write(ATC::briefTrace, "Validating DMI Incoming Message :Train Loaded");

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
    * DMIMessageInTrainLoaded::invalidate
    ******************************************************************************/
    void DMIMessageInTrainLoaded::invalidate()
    {
      trainLoadedRequested = TrainIsLoaded;  // Loaded is the safest value
      dmiDataInProcessState = DMINoDataIncomingAvailable;
    }

    /******************************************************************************
    * DMIMessageInTrainLoaded::parseMessageData
    ******************************************************************************/
    bool DMIMessageInTrainLoaded::parseDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));
      vfwSetReadBuffer(&buffer, sizeof(messageData.dmiData.msgData));

      trainLoadedRequested = (vfwGetU8(&buffer) == 1U) ? TrainIsLoaded : TrainIsEmpty;

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * DMIMessageInTrainLoaded::getTrainLoadedStatus requested by driver
    ******************************************************************************/
    bool DMIMessageInTrainLoaded::getTrainLoadedStatusRequestedByDriver(TrainLoaded & trainLoadedStatusRequested) const
    {
 
      trainLoadedStatusRequested = trainLoadedRequested;
      return (DMIDataIncomingValidated == dmiDataInProcessState);
    }

  }
}
