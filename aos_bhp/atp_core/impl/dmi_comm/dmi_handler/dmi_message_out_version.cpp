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
* This file implements the creator for the version DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-13    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_version.hpp"

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
    * DMIMessageOutVersion constructor
    ******************************************************************************/
    DMIMessageOutVersion::DMIMessageOutVersion() :AbstractDMIMessageOut(MTypeVersion)
    {
      
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutVersion::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message : Version");

        uint8_t dmiCompatibilityVersion;

        if (AbstractDMIHandler::corePtr()->getDMICompatibilityVersion(dmiCompatibilityVersion))
        {
          if (DMICom::AbstractDMIHandler::corePtr()->getCompatibilityVersion() == dmiCompatibilityVersion)
          {
            if (assembleDMIMessageData())
            {
              AbstractDMIHandler::corePtr()->setDMICompatibilityVersionAccepted();
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
    void DMIMessageOutVersion::invalidate()
    {
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutVersion::collectData()
    {
      dmiDataProcessState = DMIDataAvailable;
    }

    /******************************************************************************
    * DMIMessageOutVersion::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutVersion::assembleDMIMessageData()
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
      vfwPutU8(&buffer, DMICom::AbstractDMIHandler::corePtr()->getCompatibilityVersion());

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }
  }
}
