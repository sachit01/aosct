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
* This file implements the creator for the StartupMessage message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-26    marlundg    Created
* 2016-10-12    arastogi    Fixed modestate check. Fixed the train length.
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "abstract_config.hpp"
#include "abstract_position.hpp"
#include "radio_message_out_startup_message.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_message_handler.hpp"
#include "dmi_event_codes.hpp"
#include <vfw_string.h>
#include "abstract_tic.hpp"
#include "abstract_tims.hpp"
#include "abstract_message_handler_event_ids.hpp"
#include "abstract_radio_handler.hpp"
#include "vehicle_com.hpp"
#include <cstdio>

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
    * RadioMessageOutStartUpMessage constructor
    ******************************************************************************/
    RadioMessageOutStartUpMessage::RadioMessageOutStartUpMessage() : AbstractRadioMessageOut(MTypeStartUpMessage),
      invalidStartupMessage(ATC::Event::createSafeBrakeSBEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdInvalidStartupMessage,
        ATC::NoSB, DMICom::msgHdlrInvalidCarCnt, "Current Car Count more than maxCarCount, Car Count:",true))
    {
      implemented = true;

      startUpMessage.trainCoreStatus = 0U;
      startUpMessage.locomotiveLength = 0U;
      startUpMessage.configSource = ManuallyEnteredByDriver;
      startUpMessage.timsStatus = TimsNotAvailable;
      startUpMessage.directionAndOrientation = 0U;
      startUpMessage.brakeSystem = BrakeSystemTypeUndefined;

      // Setup fixed size
      startUpMessage.vehicleDataVec.reserve(maxVehicleCount);
      startUpMessage.configConfirmationReceived = false;
    }

    /******************************************************************************
    * RadioMessageOutStartUpMessage::collectData
    ******************************************************************************/
    void RadioMessageOutStartUpMessage::collectData()
    {
      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      TrainConfigModeState modeState = AbstractModeControl::corePtr()->getTrainConfigModeState();

      if ((mode == ATPModeConfiguration) && ((modeState == TrainConfigMode::trainConfigSendStartUpForNewConfig) ||
        (modeState == TrainConfigMode::trainConfigSendStartUpForReReg)))
      {
        DS::TrainSetup  trainSetup;

        // Get the preliminary train setup to fetch orientation and TIC, TIMS availability
        if (DS::AbstractTSetup::corePtr()->getPreliminaryTrainSetup(trainSetup))
        {
          //Úpdating the Preliminary orientation
          startUpMessage.directionAndOrientation = trainSetup.orientation;

          startUpMessage.locomotiveLength = AbstractConfig::corePtr()->getBalAntennaPosEnd() +
            AbstractConfig::corePtr()->getBalAntennaPosFront();

          startUpMessage.configSource = (TG::AbstractTIC::corePtr()->getTICAvailable()) ? AutoCollectedByTic : ManuallyEnteredByDriver;
          startUpMessage.timsStatus = (TG::AbstractTIMS::corePtr()->getTimsAvailable()) ? TimsAvailable : TimsNotAvailable;
          startUpMessage.brakeSystem = TG::VehicleCom::instance().getBrakeSystem();

          collectTrainStatusInfo(startUpMessage.trainCoreStatus);

          // Get the Config Confirmation Acknowledge
          if (modeState == TrainConfigMode::trainConfigSendStartUpForReReg)
          {
            ConfigConfirmation configConfirmData;
            DMICom::Confirmation dmiConfirmation = DMICom::AbstractDMIHandler::corePtr()->getConfirmation();
            bool isTSConfirmByDMI = (dmiConfirmation == DMICom::DMIATPConfirmationOK);
            if (isTSConfirmByDMI)
            {
              configConfirmData.configConfirmationAcknowledge = RequestAcknowledged;
            }
            else
            {
              configConfirmData.configConfirmationAcknowledge = RequestNotAcknowledged;
            }

            if (!startUpMessage.configConfirmationReceived)
            {
              startUpMessage.configConfirmationReceived = true;
              startUpMessage.configConfirmation = configConfirmData;
              dataProcessState = DataAvailable;
            }
            else
            {
              // ConfigConfirmation already received
              invalidStartupMessage.setDynamicText("ConfigConfirmation already received");
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidStartupMessage, __FILE__, __LINE__);
            }
          }
          else
          {
            if (trainSetup.vehicleCount <= maxVehicleCount)
            {
              // Send dummy data for the leading locomotive
              VehicleData   locoData;
              memset(&locoData.vehicleName[0], 0, sizeof(locoData.vehicleName));
              locoData.vehicleNodeAddress = 0U;
              locoData.vehicleType = 0U;
              locoData.noOfVeh = 1U;
              startUpMessage.vehicleDataVec.push_back(locoData);

              // Get the Vehicle ID Data (skipping the leading loco)
              for (uint16_t i = 1U; i < (trainSetup.vehicleCount); ++i)
              {
                DS::VehicleSetup   vehicleSetup;
                VehicleData   vehicleData;
                memset(&vehicleData.vehicleName[0], 0, sizeof(vehicleData.vehicleName));

                if (DS::AbstractTSetup::corePtr()->getPreliminaryVehicleSetup(i, vehicleSetup))
                {
                  static_cast<void>(vfw_strlcpy(&vehicleData.vehicleName[0], &vehicleSetup.vehicleName[0], sizeof(vehicleData.vehicleName)));
                  vehicleData.vehicleNodeAddress = vehicleSetup.nodeAdress;
                  vehicleData.vehicleType = vehicleSetup.vehicleType;
                  vehicleData.noOfVeh = 1U;
                  startUpMessage.vehicleDataVec.push_back(vehicleData);
                }
                else
                {
                  trace->write(ATC::detailedTrace, "Unable to fetch VehicleSetup");
                }
              }

              dataProcessState = DataAvailable;
            }
            else
            {
              //Report events when car count is more than maxCarCount
              //prepare the dynamic text to be send while reporting event.
              invalidStartupMessage.setDynamicText(static_cast<uint32_t>(trainSetup.vehicleCount));
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidStartupMessage, __FILE__, __LINE__);
            }
          }
        }
        else
        {
          trace->write(ATC::detailedTrace, "Unable to fetch train Setup");
        }
      }
    }

    /******************************************************************************
    * RadioMessageOutStartUpMessage::validate
    ******************************************************************************/
    bool RadioMessageOutStartUpMessage::validate()
    {
      // assemble, validate and publish data
      if (DataAvailable == dataProcessState)
      {
        trace->write(ATC::briefTrace, "Validating StartUpMessage");

        if (assembleMessageData())
        {
          dataProcessState = DataValidated;
        }
      }

      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageOutStartUpMessage::isStartUpMessageSent
    ******************************************************************************/
    bool RadioMessageOutStartUpMessage::isStartUpMessageSent() const
    {
      bool returnValue = false;
      if (DataValidated == dataProcessState)
      {
        returnValue = true;
      }

      return returnValue;
    }

    /******************************************************************************
    * RadioMessageOutStartUpMessage::assembleMessageData
    ******************************************************************************/
    bool RadioMessageOutStartUpMessage::assembleMessageData()
    {
      bool assembleDataValid = true;

      // Sanity check of certain (that can be checked) parameter values
      if (!validateQ_CONFIG_SOURCE(static_cast<uint8_t>(startUpMessage.configSource)))
      {
        trace->write(ATC::detailedTrace, "Q_CONFIG_SOURCE invalid");
        assembleDataValid = false;
      }

      if (!validateQ_TIMS_AVAILABLE(static_cast<uint8_t>(startUpMessage.timsStatus)))
      {
        trace->write(ATC::detailedTrace, "Q_TIMS_AVAILABLE invalid");
        assembleDataValid = false;
      }

      if (!validateB_DIRECTION(startUpMessage.directionAndOrientation))
      {
        trace->write(ATC::detailedTrace, "B_DIRECTION invalid");
        assembleDataValid = false;
      }

      if (!validateM_BRAKE_SYSTEM(static_cast<uint8_t>(startUpMessage.brakeSystem)))
      {
        trace->write(ATC::detailedTrace, "M_BRAKE_SYSTEM invalid");
        assembleDataValid = false;
      }

      if (startUpMessage.configConfirmationReceived)
      {
        if (!validateQ_ACKNOWLEDGE(static_cast<uint8_t>(startUpMessage.configConfirmation.configConfirmationAcknowledge)))
        {
          trace->write(ATC::detailedTrace, "Q_ACKNOWLEDGE invalid");
          assembleDataValid = false;
        }
      }

      for (std::vector<VehicleData>::iterator it = startUpMessage.vehicleDataVec.begin();
        it != startUpMessage.vehicleDataVec.end(); ++it)
      {
        if (!validateNID_VEHICLE_TYPE(static_cast<uint8_t>(it->vehicleType)))
        {
          trace->write(ATC::detailedTrace, "NID_VEHICLE_TYPE invalid");
          assembleDataValid = false;
        }
      }

      if (assembleDataValid)
      {
        VFW_Buffer buffer;

        // Initialize buffer to first byte of Application level message
        vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));

        // Assemble all data into net-message format
        vfwPutU8(&buffer, static_cast<uint8_t>(messageType));

        vfwPutU32(&buffer, startUpMessage.trainCoreStatus);
        vfwPutU16(&buffer, startUpMessage.locomotiveLength);
        vfwPutU8(&buffer, static_cast<uint8_t>(startUpMessage.configSource));
        vfwPutU8(&buffer, static_cast<uint8_t>(startUpMessage.timsStatus));
        vfwPutU8(&buffer, startUpMessage.directionAndOrientation);
        vfwPutU8(&buffer, static_cast<uint8_t>(startUpMessage.brakeSystem));

        //Assemble data for config Confirmation block
        if (startUpMessage.configConfirmationReceived)
        {
          vfwPutU8(&buffer, BTypeConfigConfirmation);
          vfwPutU8(&buffer, static_cast<uint8_t>(startUpMessage.configConfirmation.configConfirmationAcknowledge));
        }

        // VEHICLE_DATA Block types 
        for (std::vector<VehicleData>::iterator it = startUpMessage.vehicleDataVec.begin();
          it != startUpMessage.vehicleDataVec.end(); ++it)
        {
          vfwPutU8(&buffer, BTypeVehicleData);
          vfwPutU16(&buffer, (it->noOfVeh));
          vfwPutU8(&buffer, static_cast<uint8_t>(it->vehicleType));
          vfwPutU16(&buffer, it->vehicleNodeAddress);
          for (uint8_t i = 0U; i < vehicleNameMaxLength; i++)
          {
            vfwPutI8(&buffer, static_cast<int8_t>(it->vehicleName[i]));
          }
        }

        assembleAdditionalBlocks(buffer);

        vfwPutU8(&buffer, M_END_OF_MESSAGE);

        // Total length of message
        messageData.message.dataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));
      }

      traceAssembleData(assembleDataValid);

      return assembleDataValid;
    }

    void RadioMessageOutStartUpMessage::assembleAdditionalBlocks(VFW_Buffer& /*buffer*/)
    {
      // No nothing (placeholder for adaptation)
    }

    /******************************************************************************
    * RadioMessageOutStartUpMessage::invalidate
    ******************************************************************************/
    void RadioMessageOutStartUpMessage::invalidate()
    {
      // Clear all optional data
      startUpMessage.trainCoreStatus = 0U;
      startUpMessage.locomotiveLength = 0U;
      startUpMessage.configSource = ManuallyEnteredByDriver;
      startUpMessage.timsStatus = TimsNotAvailable;
      startUpMessage.directionAndOrientation = 0U;
      startUpMessage.brakeSystem = BrakeSystemTypeUndefined;

      startUpMessage.vehicleDataVec.clear();
      startUpMessage.configConfirmationReceived = false;
      dataProcessState = NoDataAvailable;
    }

    /******************************************************************************
    * getChannelId
    ******************************************************************************/
    uint16_t RadioMessageOutStartUpMessage::getChannelId() const
    {
      uint16_t replyChannel;

      const ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
      const ATPMode previousMode = AbstractModeControl::corePtr()->getPreviousMode();
      const bool modeTranFromShuntRouteToConfig =
        (ATPModeConfiguration == currentMode) && (ATPModeShuntingRoute == previousMode);

      if (modeTranFromShuntRouteToConfig)
      {
        //broadcast startup message if ATP mode Configuration has been entered from ATP mode Shunting Route 
        replyChannel = RadioCom::radioChannelRegionBroadcast;
      }
      else
      {
        // Get proper reply-channel (where the INITATE_CONFIG was received in Position Report Request)
        AbstractMessageHandler::corePtr()->getReplyChannelInitConfig(replyChannel);
      }

      return replyChannel;
    }

  }
}
