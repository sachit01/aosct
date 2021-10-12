/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->TCC) has an associated creator class inherited from AbstractRadioMessageOut.
* This file implements the creator for the RegistrationArea message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-26    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"

#include "abstract_message_handler.hpp"
#include "abstract_position.hpp"
#include "radio_message_out_registration_area.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_dmi_handler.hpp"
#include "radio_channel.hpp"

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
  namespace Kernel
  {
    /******************************************************************************
    * RadioMessageOutRegistrationArea constructor
    ******************************************************************************/
    RadioMessageOutRegistrationArea::RadioMessageOutRegistrationArea() :
      AbstractRadioMessageOut(MTypeRegistrationAreaMessage), registrationArea(0U)
    {
      implemented = true;
      registrationArea = 0U;
    }

    /******************************************************************************
    * RadioMessageOutRegistrationArea::collectData
    ******************************************************************************/
    void RadioMessageOutRegistrationArea::collectData()
    {
      uint8_t areaId;

      // Check if Driver selected RegArea this cycle
      if (DMICom::AbstractDMIHandler::corePtr()->getRegistrationArea(areaId))
      {
        // Remember area ID selected by driver.
        Kernel::AbstractMessageHandler::corePtr()->setRegAreaSelectedByDriver(areaId);
      }

      // Check if area ID has been selected by driver
      const bool isRegAreaSelectedByDriver =
        Kernel::AbstractMessageHandler::corePtr()->getRegAreaSelectedByDriver(areaId);

      // Data is valid is if RegArea has been selected by driver at any point.
      if (isRegAreaSelectedByDriver)
      {
        registrationArea = areaId;
        dataProcessState = DataAvailable;
      }
    }

    /******************************************************************************
    * RadioMessageOutRegistrationArea::validate
    ******************************************************************************/
    bool RadioMessageOutRegistrationArea::validate()
    {
      // assemble, validate and publish data
      if (DataAvailable == dataProcessState)
      {
        trace->write(ATC::briefTrace, "Validating RegistrationArea");

        if (assembleMessageData())
        {
          dataProcessState = DataValidated;
        }
      }

      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageOutRegistrationArea::assembleMessageData
    ******************************************************************************/
    bool RadioMessageOutRegistrationArea::assembleMessageData()
    {
      bool assembleDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));

      // Assemble all data into net-message format
      vfwPutU8(&buffer, static_cast<uint8_t>(messageType));
      vfwPutU8(&buffer, registrationArea);
      //Add M_END_OF_MESSAGE
      vfwPutU8(&buffer, M_END_OF_MESSAGE);

      // Total length of message
      messageData.message.dataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));

      traceAssembleData(assembleDataValid);

      return assembleDataValid;
    }

    /******************************************************************************
    * RadioMessageOutRegistrationArea::invalidate
    ******************************************************************************/
    void RadioMessageOutRegistrationArea::invalidate()
    {
      dataProcessState = NoDataAvailable;
    }
    /******************************************************************************
    * RadioMessageOutDriverInformation::getChannelId
    ******************************************************************************/
    uint16_t RadioMessageOutRegistrationArea::getChannelId() const
    {
      return ATP::RadioCom::radioChannelId1;
    }
  }
}
