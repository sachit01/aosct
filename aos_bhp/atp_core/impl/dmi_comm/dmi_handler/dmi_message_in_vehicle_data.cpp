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
* 2018-03-30    skothiya    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_dmi_message_in.hpp"
#include "dmi_message_in_vehicle_data.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tsetup.hpp"
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
    * DMIMessageInVehicleData constructor
    ******************************************************************************/
    DMIMessageInVehicleData::DMIMessageInVehicleData() : AbstractDMIMessageIn(MTypeVehicleData)
    {
      noOfVehicleDatablocks = 0U;
      memset(&vehicleData[0], 0, sizeof(vehicleData));
      manualTrainSetupConfirmed = false;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageInVehicleData::validate()
    {
      trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Incoming Message: Vehicle Name List");
      writeToLog(ATC::DetailedLog, "DMI Handler: Validating DMI Incoming Message: Vehicle Name List", __FILE__, __LINE__);

      // Parse, validate and publish data
      if (DMIDataIncomingAvailable == dmiDataInProcessState)
      {
        if (parseDMIMessageData())
        {
          if (validateMode())
          {
            Kernel::TrainConfigModeState configModeState =
              Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();

           // Change train-setup only in manual configuration
            if (Kernel::TrainConfigMode::trainConfigWaitNewConfigDMI == configModeState)
            {
              DS::TrainSetup  tSetup;
              DS::VehicleSetup  tVehicleSetup;

              tSetup.orientation = (AbstractDMIHandler::corePtr()->getLocoVsTrainDirData() == DMIATPcarsConnectedAtAEnd) ? trainLocoOrientation: 0U;
              uint16_t numberOfVehicle = 0U;

              // DMI does not send leading loco, so we must add a placeholder here
              tVehicleSetup.vehicleType = 0U;
              tVehicleSetup.nodeAdress = 0U;
              memset(&tVehicleSetup.vehicleName[0], 0, sizeof(tVehicleSetup.vehicleName));
              if (!(DS::AbstractTSetup::corePtr()->setPreliminaryVehicleSetup(numberOfVehicle, tVehicleSetup)))
              {
                trace->write(ATC::veryDetailedTrace, "DMI Handler: Vehicle Setup is not set properly this index:",
                  static_cast<int32_t>(numberOfVehicle));
                writeToLog(ATC::DetailedLog, "DMI Handler: Vehicle Setup is not set properly this index:",
                  static_cast<int32_t>(numberOfVehicle), __FILE__, __LINE__);
              }
              ++numberOfVehicle;

              for (uint16_t vehicleBlockIndex = 0U; vehicleBlockIndex < noOfVehicleDatablocks; ++vehicleBlockIndex)
              {
                uint16_t numberOfVehicleInBlock = vehicleData[vehicleBlockIndex].noOfVehicleInBlock;
                for (uint16_t vehicleIndex = 0U; vehicleIndex < numberOfVehicleInBlock; ++vehicleIndex)
                {
                  //TODO: Need to uncomment when car setup will be implemented
                  tVehicleSetup.vehicleType = vehicleData[vehicleBlockIndex].vehicleType;
                  tVehicleSetup.nodeAdress = vehicleData[vehicleBlockIndex].vehicleNodeId;
                  static_cast<void>(vfw_strlcpy(&tVehicleSetup.vehicleName[0], &vehicleData[vehicleBlockIndex].vehicleName[0],
                    sizeof(tVehicleSetup.vehicleName)));
                  // Store new vehicle setup
                  if (!(DS::AbstractTSetup::corePtr()->setPreliminaryVehicleSetup(numberOfVehicle, tVehicleSetup)))
                  {
                    trace->write(ATC::veryDetailedTrace, "DMI Handler: Vehicle Setup is not set properly this index:",
                      static_cast<int32_t>(numberOfVehicle));
                    writeToLog(ATC::DetailedLog, "DMI Handler: Vehicle Setup is not set properly this index:",
                      static_cast<int32_t>(numberOfVehicle), __FILE__, __LINE__);
                  }
                  ++numberOfVehicle;
                }
              }

              tSetup.vehicleCount = numberOfVehicle;
              if (!(DS::AbstractTSetup::corePtr()->setPreliminaryTrainSetup(tSetup)))
              {
                //Reset the value of Loco Vs Train direction, so that AOS will get proper value for the next received LocoVsTrain DMI message 
                AbstractDMIHandler::corePtr()->resetLocoVsTrainDirData();
              }

            }

            manualTrainSetupConfirmed = true;
            dmiDataInProcessState = DMIDataIncomingValidated;
          }
        }
      }

      return (DMIDataIncomingValidated == dmiDataInProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageInVehicleData::invalidate()
    {
      noOfVehicleDatablocks = 0U;
      memset(&vehicleData[0], 0, sizeof(vehicleData));
      manualTrainSetupConfirmed = false;
      dmiDataInProcessState = DMINoDataIncomingAvailable;
    }

    /******************************************************************************
    * parseMessageData
    ******************************************************************************/
    bool DMIMessageInVehicleData::parseDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));
      vfwSetReadBuffer(&buffer, sizeof(messageData.dmiData.msgData));

      // Get the Number of Vehicles Data Blocks
      noOfVehicleDatablocks = vfwGetU16(&buffer);

      if (noOfVehicleDatablocks <= maxVehicleCount)
      {
        // Get the Vehicle Data
        for (uint16_t i = 0U; i < noOfVehicleDatablocks; i++)
        {
          VehicleDataBlock& vehicleBlock = vehicleData[i];

          // Number of vehicle of a vehicle type
          vehicleBlock.noOfVehicleInBlock = vfwGetU16(&buffer);
          //vehicle type
          vehicleBlock.vehicleType = vfwGetU8(&buffer);
          // vehicle node address or id 
          vehicleBlock.vehicleNodeId = vfwGetU16(&buffer);
          // Vehicle Name or Road Number
          memset(&vehicleBlock.vehicleName[0], 0, sizeof(vehicleBlock.vehicleName));
          for (uint8_t pos = 0U; pos < (vehicleNameMaxLength / sizeof(vehicleBlock.vehicleName[0U])); pos++)
          {
            vehicleBlock.vehicleName[pos] = static_cast<char_t>(vfwGetI8(&buffer));
          }
        }
      }
      else
      {
        trace->write(ATC::briefTrace, "DMI Handler: Invalid Number of Vehicle data blocks");
        writeToLog(ATC::BriefLog, "DMI Handler: Invalid Number of Vehicle data blocks", __FILE__, __LINE__);
        parseDataValid = false;
      }

      // Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    *validateMode
    ******************************************************************************/
    bool DMIMessageInVehicleData::validateMode() const
    {
      bool modeValid = false;

      if (Kernel::AbstractModeControl::corePtr()->getCurrentMode() == ATPModeConfiguration)
      {
        modeValid = true;
      }

      //Write the Trace regarding Mode 
      traceValidateMode(modeValid);

      return modeValid;
    }

    /******************************************************************************
    *newTrainSetUpConfirmed
    ******************************************************************************/
    bool DMIMessageInVehicleData::getManualTrainSetupConfirmed(void) const
    {
      return manualTrainSetupConfirmed;
    }

  }
}
