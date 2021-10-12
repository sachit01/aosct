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
* This file implements the creator for the outgoing train weight DMIMessage.
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
#include "dmi_message_out_train_weight_bhp.hpp"
#include "abstract_odometry.hpp"
#include "abstract_targets.hpp"
#include "tsetup.hpp"
#include "message_handler.hpp"
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
    * DMIMessageOutTrainWeightBHP constructor
    ******************************************************************************/
    DMIMessageOutTrainWeightBHP::DMIMessageOutTrainWeightBHP() :AbstractDMIMessageOut(MTypeDMIMessageTrainWeight)
    {
      trainWeight = 0;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutTrainWeightBHP::validate()
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
    void DMIMessageOutTrainWeightBHP::invalidate()
    {
      dmiDataProcessState = DMINoDataAvailable;

      // Note: don't change trainWeight here: the old value is needed in collectData()
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutTrainWeightBHP::collectData()
    {
      uint16_t startupStatus;
      const bool dmiStartupStatus = AbstractDMIHandler::corePtr()->getDMIStartupStatus(startupStatus);
      const int32_t newTrainWeight = DS::AbstractTSetup::corePtr()->getTrainWeight();

      if ((newTrainWeight != trainWeight) || dmiStartupStatus)
      {
        trainWeight = newTrainWeight;
        dmiDataProcessState = DMIDataAvailable;
      }
    }

    /******************************************************************************
    * DMIMessageOutTrainWeightBHP::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutTrainWeightBHP::assembleDMIMessageData()
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
      //Assemble the data in network order
      vfwPutI32(&buffer, trainWeight);

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }
  }
}
