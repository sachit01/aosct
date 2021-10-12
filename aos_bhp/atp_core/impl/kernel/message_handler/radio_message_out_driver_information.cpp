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
* This file implements the creator for the DriverInformation message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-20    bhermans    Created
* 2016-04-03    bhermans    File renamed
* 2016-08-26    marlundg    Updated for ATP-Limited
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_string.h>

#include "driver_login_seq.hpp"
#include "abstract_mode_control.hpp"
#include "radio_message_out_driver_information.hpp"
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
    * RadioMessageOutDriverInformation constructor
    ******************************************************************************/
    RadioMessageOutDriverInformation::RadioMessageOutDriverInformation() : 
      AbstractRadioMessageOut(MTypeDriverInformation), driverInfoSent(false) 
    {
      implemented = true;

      memset(&driverInfo.driver[0], 0, sizeof(driverInfo.driver));
      memset(&driverInfo.password[0], 0, sizeof(driverInfo.password));
    }

    /******************************************************************************
    * RadioMessageOutDriverInformation::collectData
    ******************************************************************************/
    void RadioMessageOutDriverInformation::collectData()
    {
      if (AbstractModeControl::corePtr()->getDriverLoginSeqState() == DriverLoginSeq::driverLoginVerification)
      {
        DMICom::DriverIdAndPassword driverIDPassword;
        if (DMICom::AbstractDMIHandler::corePtr()->getDriverIdAndPassword(driverIDPassword))
        {
          memset(&driverInfo.driver[0], 0, sizeof(driverInfo.driver));
          static_cast<void>(strncpy(&driverInfo.driver[0], &driverIDPassword.driverID[0], sizeof(driverInfo.driver)));

          memset(&driverInfo.password[0], 0, sizeof(driverInfo.password));
          static_cast<void>(strncpy(&driverInfo.password[0], &driverIDPassword.password[0], sizeof(driverInfo.password)));

          encryptPassword(&driverInfo.password[0]);
          dataProcessState = DataAvailable;
        }
      }
    }

    /******************************************************************************
    * RadioMessageOutDriverInformation::encryptPassword
    ******************************************************************************/
    void RadioMessageOutDriverInformation::encryptPassword(char_t password[tccPasswordIdMaxLength])
    {
      // Encoding password with key
      for(uint8_t i = 0U; i < tccPasswordIdMaxLength; i++)
      {
        //lint -e{1960} Cast changes signedness - we have to, since ^ is disallowed for char_t
        password[i] = static_cast<char_t>(static_cast<uint8_t>(password[i]) ^ key[i]);
      }
    }

    /******************************************************************************
    * RadioMessageOutDriverInformation::validate
    ******************************************************************************/
    bool RadioMessageOutDriverInformation::validate()
    {
      // assemble, validate and publish data
      if (DataAvailable == dataProcessState)
      {
        trace->write(ATC::briefTrace, "Validating DriverInformation");

        if (assembleMessageData())
        {
          dataProcessState = DataValidated;
        }
      }
      
      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageOutDriverInformation::assembleMessageData
    ******************************************************************************/
    bool RadioMessageOutDriverInformation::assembleMessageData()
    {
      bool assembleDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));

      vfwPutU8(&buffer, static_cast<uint8_t>(messageType));

      for(uint8_t i = 0U; i < sizeof(driverInfo.driver); i++)
      {
        vfwPutI8(&buffer, static_cast<int8_t>(driverInfo.driver[i]));
      }

      for(uint8_t i = 0U; i < sizeof(driverInfo.password); i++)
      {
        vfwPutI8(&buffer, static_cast<int8_t>(driverInfo.password[i]));
      }

      //Add M_END_OF_MESSAGE
      vfwPutU8(&buffer, M_END_OF_MESSAGE);
      
      // Total length of message
      messageData.message.dataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));

      traceAssembleData(assembleDataValid);

      return assembleDataValid;
    }

    /******************************************************************************
    * RadioMessageOutDriverInformation::invalidate
    ******************************************************************************/
    void RadioMessageOutDriverInformation::invalidate()
    {
      dataProcessState = NoDataAvailable;
    }
    
    /******************************************************************************
    * RadioMessageOutDriverInformation::getChannelId
    ******************************************************************************/
    uint16_t RadioMessageOutDriverInformation::getChannelId() const
    {
      return RadioCom::radioChannelRegionBroadcast;
    }

  }
}
