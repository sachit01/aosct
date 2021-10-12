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
* 2016-09-13    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_in.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_in_driver_id_and_password.hpp"
#include "driver_login_seq.hpp"
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
    * DMIMessageInDriverIDandPassword constructor
    ******************************************************************************/
    DMIMessageInDriverIDandPassword::DMIMessageInDriverIDandPassword() : AbstractDMIMessageIn(MTypeDriverIdAndPassword)
    {
      memset(&driverIDAndPassword, 0, sizeof(DriverIdAndPassword));
    }

    /******************************************************************************
     * DMIMessageInDriverIDandPassword::validate
     ******************************************************************************/
    bool DMIMessageInDriverIDandPassword::validate()
    {
      trace->write(ATC::briefTrace, "Validating DMI Incoming Message :Driver Id and Password");

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
    * DMIMessageInDriverIDandPassword::invalidate
    ******************************************************************************/
    void DMIMessageInDriverIDandPassword::invalidate()
    {
      memset(&driverIDAndPassword, 0, sizeof(DriverIdAndPassword));
      dmiDataInProcessState = DMINoDataIncomingAvailable;
    }

    /******************************************************************************
    * DMIMessageInDriverIDandPassword::getDriverIdAndPassword
    ******************************************************************************/
    bool DMIMessageInDriverIDandPassword::getDriverIdAndPassword(DriverIdAndPassword &driverIdAndPass) const
    {
      if (DMIDataIncomingValidated == dmiDataInProcessState)
      {
        static_cast<void>(vfw_strlcpy(&driverIdAndPass.driverID[0], &driverIDAndPassword.driverID[0], sizeof(driverIdAndPass.driverID)));
        static_cast<void>(vfw_strlcpy(&driverIdAndPass.password[0], &driverIDAndPassword.password[0], sizeof(driverIdAndPass.password)));
      }
      return (DMIDataIncomingValidated == dmiDataInProcessState);
    }

    /******************************************************************************
    * DMIMessageInDriverIDandPassword::parseMessageData
    ******************************************************************************/
    bool DMIMessageInDriverIDandPassword::parseDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));
      vfwSetReadBuffer(&buffer, sizeof(messageData.dmiData.msgData));
      //Get Driver ID, driverID[] buffer size is driverSize+1, null terminate
      vfwCpyToRawBuffer(&driverIDAndPassword.driverID[0], &buffer, static_cast<uint32_t>(driverIdMaxLength));
      driverIDAndPassword.driverID[driverIdMaxLength] = '\0';

      //Get Password, passwd[] buffer size is passwordSize+1, null terminate
      vfwCpyToRawBuffer(&driverIDAndPassword.password[0], &buffer, static_cast<uint32_t>(dmiPasswordIdMaxLength));
      driverIDAndPassword.password[dmiPasswordIdMaxLength] = '\0';

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

  }
}
