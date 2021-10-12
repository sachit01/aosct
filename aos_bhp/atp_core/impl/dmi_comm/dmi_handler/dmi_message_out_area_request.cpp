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
* This file implements the creator for the Area request DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 19-04-2017    adgupta     Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_area_request.hpp"
#include "radio_message_types.hpp"
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
    * DMIMessageOutAreaRequest constructor
    ******************************************************************************/
    DMIMessageOutAreaRequest::DMIMessageOutAreaRequest() :AbstractDMIMessageOut(MTypeAreaRequest)
    {
      numAreaIds = 0U;
      memset(&areaId[0], 0, sizeof(areaId));
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutAreaRequest::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message : Area Request");

        if ((numAreaIds > 0U) && (numAreaIds < maxAreaIds))
        {
          if (AbstractDMIHandler::corePtr()->getDMICompatibilityVersionAccepted())
          {
            if (assembleDMIMessageData())
            {
              dmiDataProcessState = DMIDataValidated;
            }
          }
        }
      }

      return(DMIDataValidated == dmiDataProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageOutAreaRequest::invalidate()
    {
      numAreaIds = 0U;
      memset(&areaId[0], 0, sizeof(areaId));
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutAreaRequest::collectData()
    {
      // Collect data from TCC
      Kernel::TCCAreas areas;
      uint8_t selectedRegArea = 0U;

      bool isAreaRequested = Kernel::AbstractMessageHandler::corePtr()->getAreaRequested(areas);
      
      // Get information if Registration Area is already selected by driver.
      bool isRegAreaSelectedByDriver = Kernel::AbstractMessageHandler::corePtr()->getRegAreaSelectedByDriver(selectedRegArea);

      dmiDataProcessState = DMINoDataAvailable;

      // Send data to DMI only if Registration Area is not already selected by driver and there is an AreaRequest from TCC.
      if (isAreaRequested && (!isRegAreaSelectedByDriver))
      {
        if (areas.numAreas > maxAreaIds)
        {
          trace->write(ATC::briefTrace, "Number of Areas Requested, more than Max value");
          numAreaIds = 0U;  //Invalidate number of Areas
        }
        else
        {
          numAreaIds = areas.numAreas;
          for (uint8_t cnt = 0U; cnt < numAreaIds; cnt++)
          {
            areaId[cnt] = areas.areaId[cnt];  //Copy Area Ids
          }
          dmiDataProcessState = DMIDataAvailable;
        }
      }
    }

    /******************************************************************************
    * DMIMessageOutAreaRequest::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutAreaRequest::assembleDMIMessageData()
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

      vfwPutU8(&buffer, numAreaIds);

      // Assemble data and write in network order
      for (uint8_t cnt = 0U; cnt < numAreaIds; cnt++)
      {
        vfwPutU8(&buffer, areaId[cnt]);
      }

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

  }
}
