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
* 19-04-2017    adgupta     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_in.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_message_handler.hpp"
#include "dmi_message_in_registration_area.hpp"

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
    * registrationAreaId constructor
    ******************************************************************************/
    DMIMessageInRegistrationArea::DMIMessageInRegistrationArea() : AbstractDMIMessageIn(MTypeRegistrationArea)
    {
      regArea.isvalid = false;
    }

    /******************************************************************************
     * registrationAreaId::validate
     ******************************************************************************/
    bool DMIMessageInRegistrationArea::validate()
    {
      trace->write(ATC::briefTrace, "Validating DMI Incoming Message :Registration Area");

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
    * registrationAreaId::invalidate
    ******************************************************************************/
    void DMIMessageInRegistrationArea::invalidate()
    {
      dmiDataInProcessState = DMINoDataIncomingAvailable;

      // The Registration Area is only valid for one cycle (external components needs to detect changed value).
      regArea.isvalid = false;
    }

    /******************************************************************************
    * registrationAreaId::getDriverIdAndPassword
    ******************************************************************************/
    bool DMIMessageInRegistrationArea::getRegistrationArea(uint8_t &area) const
    {
      if (regArea.isvalid)
      {
        area = regArea.regAreaNidId;
      }

      return regArea.isvalid;
    }

    /******************************************************************************
    * registrationAreaId::parseMessageData
    ******************************************************************************/
    bool DMIMessageInRegistrationArea::parseDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));
      vfwSetReadBuffer(&buffer, sizeof(messageData.dmiData.msgData));

      //Get Registration Area
      regArea.isvalid = true;
      regArea.regAreaNidId = vfwGetU8(&buffer);

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

  }
}
