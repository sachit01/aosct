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
#include "dmi_message_in_dmi_startup.hpp"

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
    * DMIMessageInDMIStartup constructor
    ******************************************************************************/
    DMIMessageInDMIStartup::DMIMessageInDMIStartup() : AbstractDMIMessageIn(MTypeDMIStartup)
    {
      dmiStartupStatus = 0U;
      dmiCompatibilityVersion = 0U;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageInDMIStartup::validate()
    {
      trace->write(ATC::briefTrace, "Validating DMI Incoming Message :DMI StartUp");

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
    void DMIMessageInDMIStartup::invalidate()
    {
      dmiStartupStatus = 0U;
      dmiCompatibilityVersion = 0U;
      dmiDataInProcessState = DMINoDataIncomingAvailable;
    }

    /******************************************************************************
    * getDMIStartupStatus
    ******************************************************************************/
    bool DMIMessageInDMIStartup::getDMIStartupStatus(uint16_t &status) const
    {
      bool dmiStartupReceived;

      if (DMIDataIncomingValidated == dmiDataInProcessState)
      {
        status = dmiStartupStatus;
        dmiStartupReceived = true;
      }
      else
      {
        status = 0U;
        dmiStartupReceived = false;
      }

      return dmiStartupReceived;
    }

    /******************************************************************************
    * getDMICompatibilityVersion
    ******************************************************************************/
    bool DMIMessageInDMIStartup::getDMICompatibilityVersion(uint8_t &compatibilityVersion) const
    {
      bool retFlag = false;

      if (DMIDataIncomingValidated == dmiDataInProcessState)
      {
        compatibilityVersion = dmiCompatibilityVersion;
        retFlag = true;
      }

      return retFlag;
    }

    /******************************************************************************
    * DMIMessageInDMIStartup::parseMessageData
    ******************************************************************************/
    bool DMIMessageInDMIStartup::parseDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));
      vfwSetReadBuffer(&buffer, sizeof(messageData.dmiData.msgData));

      //Get the DMI Status 
      dmiStartupStatus = vfwGetU16(&buffer);

      //Get the DMI compatibility Version  
      dmiCompatibilityVersion = vfwGetU8(&buffer);

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

  }
}
