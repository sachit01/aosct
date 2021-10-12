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
* This file implements the creator for the outgoing Train Configuration Data DMIMessage.
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
#include "dmi_message_out_train_config_data.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_config.hpp"

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
    * DMIMessageOutTrainConfigData_hpp constructor
    ******************************************************************************/
    DMIMessageOutTrainConfigData::DMIMessageOutTrainConfigData() :AbstractDMIMessageOut(MTypeTrainConfigData)
    {
      memset(&trainName[0], 0, sizeof(trainName));
      trainLength = 0U;
      timsRelatedData = 0U;
      distFromBaliseToTrainFront = 0U;
      distFromBaliseToTrainEnd = 0U;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutTrainConfigData::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message :Train Configuration Data");

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
    void DMIMessageOutTrainConfigData::invalidate()
    {
      //Clear all data
      memset(&trainName[0], 0, sizeof(trainName));
      trainLength = 0U;
      timsRelatedData = 0U;
      distFromBaliseToTrainFront = 0U;
      distFromBaliseToTrainEnd = 0U;
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutTrainConfigData::collectData()
    {
      uint16_t startupStatus;
      const bool dmiStartupStatus = AbstractDMIHandler::corePtr()->getDMIStartupStatus(startupStatus);
      const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      // Fetch train-setup from preliminary Train- and VehicleSetup if processing a re-registration.
      const Kernel::TrainConfigModeState trainConfigModeState = Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();

      const bool validToSendDataInConfigMode = ((currentMode == ATPModeConfiguration)
        && ((trainConfigModeState == Kernel::TrainConfigMode::trainConfigTSetupAccepted)
          || (trainConfigModeState == Kernel::TrainConfigMode::trainConfigSendReRegDataToDMI)));

      /* Send config message to DMI when TSetup is accepted in below modes:
      - Configuration
      - Normal
      - Shunting Route
      - Staff Responsible
      */

      //local variables needed for function call
      uint8_t  id = 0U;
      uint16_t receivedChannelId = 0U;
      const bool isTsetupReceived = Kernel::AbstractMessageHandler::corePtr()->getTrainSetupReceived(id, receivedChannelId);
      const bool isTsetupRejected = Kernel::AbstractMessageHandler::corePtr()->isTrainSetupRejectedByAOS();
      const bool isTsetupAccepted = isTsetupReceived && (!isTsetupRejected);

      const bool validToSendDataInOtherModes = (((ATPModeNormal == currentMode)
        || (ATPModeShuntingRoute == currentMode)
        || (ATPModeStaffResponsible == currentMode))
        && isTsetupAccepted);

      //Get trainLength(m)

      const bool sendOnDMIRestart = (DS::AbstractTSetup::corePtr()->isTrainSetupValid() && dmiStartupStatus);
      if (validToSendDataInOtherModes || validToSendDataInConfigMode || sendOnDMIRestart)
      {
        //Get Train Name
        bool trainNameValid = DS::AbstractTSetup::corePtr()->getTrainName(&trainName[0]);

        if (!trainNameValid)
        {
          trace->write(ATC::veryDetailedTrace, "DMI Handler: Invalid Train Name");
          writeToLog(ATC::VeryDetailedLog, "DMI Handler: Invalid Train Name", __FILE__, __LINE__);
        }

        const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();
        if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
        {
          //Converting Train length from centimeter to meter
          trainLength = static_cast<uint16_t>((pTrainSetup->length + 50U) / 100U);

          if (pTrainSetup->timsSupNeeded)
          {
            //Set the bit related to timsRequired
            timsRelatedData = timsRelatedData | 0x01U;
          }

          if ((pTrainSetup->orientation & trainLocoOrientation) > 0U)
          {
            // Set the bit for Car Connected as Cars connected at A side
            timsRelatedData = timsRelatedData | 0x02U;
          }

          //Set the balise antenna position from loco front
          distFromBaliseToTrainFront = AbstractConfig::corePtr()->getBalAntennaPosFront();

          //Set the balise antenna position from loco end 
          distFromBaliseToTrainEnd = AbstractConfig::corePtr()->getBalAntennaPosEnd();
        }
        else
        {
          trace->write(ATC::veryDetailedTrace, "DMI Handler: Invalid Train Setup");
          writeToLog(ATC::VeryDetailedLog, "DMI Handler: Invalid Train Setup", __FILE__, __LINE__);
        }

        dmiDataProcessState = DMIDataAvailable;
      }
    }

    /******************************************************************************
    * DMIMessageOutTrainConfigData::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutTrainConfigData::assembleDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));

      //Header Type
      messageData.headerType = dmiHeaderTypeAckMsg;
      //Message Number
      messageData.msgNumber = AbstractDMIHandler::corePtr()->getNextMessageNumber();

      //Set MSB for the acknowledged DMIMessageType 
      messageData.dmiData.msgType = static_cast<uint8_t>(static_cast<uint8_t>(messageType) | 0x80U);

      // Assemble data and write in network order
      //Use vfwCpyFromRawBuffer instead of VfwPutString because VfwPutString will 
      //increments the offset in the buffer depending on the String Length of Train Name
      vfwCpyFromRawBuffer(&buffer, &trainName[0], static_cast<uint32_t>(trainNameMaxLength));
      vfwPutU16(&buffer, trainLength);
      vfwPutU8(&buffer, timsRelatedData);
      vfwPutU16(&buffer, distFromBaliseToTrainFront);
      vfwPutU16(&buffer, distFromBaliseToTrainEnd);

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }
  }
}
