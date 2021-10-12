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
* This file implements the creator for the outgoing Train Name DMIMessage.
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
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_train_name.hpp"
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
    * DMIMessageOutTrainName constructor
    ******************************************************************************/
    DMIMessageOutTrainName::DMIMessageOutTrainName() :AbstractDMIMessageOut(MTypeTrainName)
    {
      memset(&trainName[0], 0, sizeof(trainName));

    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutTrainName::validate()
    {
      // Assemble, validate and publish data
      uint16_t startupStatus;
      const bool dmiStartupStatus = AbstractDMIHandler::corePtr()->getDMIStartupStatus(startupStatus);
      const bool isTrainNameChanged = DS::AbstractTSetup::corePtr()->trainNameChanged();

      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message :Train Name");
        if (isTrainNameChanged || dmiStartupStatus)
        {
          if (assembleDMIMessageData())
          {
            DS::AbstractTSetup::corePtr()->setTrainNameChanged(false);
            dmiDataProcessState = DMIDataValidated;
          }
        }
      }
      return(DMIDataValidated == dmiDataProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageOutTrainName::invalidate()
    {
      //clear all data
      memset(&trainName[0], 0, sizeof(trainName));
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutTrainName::collectData()
    {
      //Get Train Name
      bool trainNameValid = DS::AbstractTSetup::corePtr()->getTrainName(&trainName[0]);

      if (!trainNameValid)
      {
        trace->write(ATC::veryDetailedTrace, "DMI Handler: Invalid Train Name");
        writeToLog(ATC::VeryDetailedLog, "DMI Handler: Invalid Train Name", __FILE__, __LINE__);
      }

      dmiDataProcessState = DMIDataAvailable;
    }

    /******************************************************************************
    * DMIMessageOutTrainName::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutTrainName::assembleDMIMessageData()
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
      vfwCpyFromRawBuffer(&buffer, &trainName[0], static_cast<uint32_t>(trainNameMaxLength));

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }
  }
}
