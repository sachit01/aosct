/****************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the parser for the MovementAuthority message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-03-21    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "radio_message_types_bhp.hpp"
#include "radio_message_in_train_setup_bhp.hpp"
#include "tsetup.hpp"
#include "atc_math.hpp"
#include "mode_control.hpp"
#include "log_handler.hpp"

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
    * constructor
    ******************************************************************************/
    RadioMessageInTrainSetupBHP::RadioMessageInTrainSetupBHP() : RadioMessageInTrainSetup()
    {
      bhpConfigVersion.noOfBytesApplicationData = 0U;
      bhpConfigVersion.configurationMajorVersion = 0U;
      bhpConfigVersion.configurationMinorVersion = 0U;
      currentCarsInOrder = 0U;
      previousCarsInOrder = 0U;
    }


    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void RadioMessageInTrainSetupBHP::invalidate()
    {
      bhpConfigVersion.noOfBytesApplicationData = 0U;
      bhpConfigVersion.configurationMajorVersion = 0U;
      bhpConfigVersion.configurationMinorVersion = 0U;
      currentCarsInOrder = 0U;
      previousCarsInOrder = 0U;
      RadioMessageInTrainSetup::invalidate();
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool RadioMessageInTrainSetupBHP::validate()
    {
      bool retValue = false;

      if (RadioMessageInTrainSetup::validate())
      {
        retValue = true;
      }

      return retValue;
    }

    /******************************************************************************
    * parseAdditionalBlocks
    ******************************************************************************/
    bool RadioMessageInTrainSetupBHP::parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType)
    {
      bool retvalue = false;

      switch (adapBlockType)
      {
      case BTypeBHPBConfigVersion:
      {
        bhpConfigVersion.noOfBytesApplicationData = vfwGetU16(buffer);
        bhpConfigVersion.configurationMajorVersion = vfwGetU8(buffer);
        bhpConfigVersion.configurationMinorVersion = vfwGetU8(buffer);
        //Check for N_LENGTH

        if (bhpbConfigVersionInTSetupBlockSize == bhpConfigVersion.noOfBytesApplicationData)
        {
          retvalue = true;
        }
        else
        {
          invalidIncmgMsgTCC.setDynamicText("BHPB_CONFIG_VERSION");
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
        }
        break;
      }
      default:
      {
        trace->write(ATC::detailedTrace, "Adaptation Block not defined");
        writeToLog(ATC::DetailedLog, "Adaptation Block not defined", __FILE__, __LINE__);
        invalidBlockTypeInRejectedTCCMessage.setDynamicText(static_cast<uint32_t>(adapBlockType));
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidBlockTypeInRejectedTCCMessage, __FILE__, __LINE__);
        break;
      }
      }

      return retvalue;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::detailedLog
    ******************************************************************************/
    void RadioMessageInTrainSetupBHP::detailedLog(void) const
    {
      uint8_t currentLevel;
      bool isEnabled;

      RadioMessageInTrainSetup::detailedLog();

      trace->getTraceDetails(currentLevel, isEnabled);

      if (isEnabled && (currentLevel >= ATC::detailedMessageTrace))
      {   // No reason to assemble logStr if trace not enabled
        char_t logStr[120];

        //lint -e{586} snprintf is needed here
        const int32_t res = snprintf(&logStr[0], sizeof(logStr),
          "BHPB_CONFIG_VERSION: N_LENGTH=%u, M_VERSION=%u, M_VERSION=%u",
          static_cast<uint32_t> (bhpConfigVersion.noOfBytesApplicationData),
          static_cast<uint32_t> (bhpConfigVersion.configurationMajorVersion),
          static_cast<uint32_t> (bhpConfigVersion.configurationMinorVersion));

        if ((res != -1)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
        {
          traceLog(ATC::detailedMessageTrace, ATC::DetailedLog, &logStr[0]);
        }
      }
    }


    /******************************************************************************
    * calculateBrakeParameters
    ******************************************************************************/
    void RadioMessageInTrainSetupBHP::calculateBrakeParameters(const VehicleTypeData* const currentVehicleTypeData, const uint16_t numOfVeh)
    {
      RadioMessageInTrainSetup::calculateBrakeParameters(currentVehicleTypeData, numOfVeh);
      if (currentVehicleTypeData != static_cast<VehicleTypeData*>(NULL))
      {
        const uint16_t newVehicleTypeId = static_cast<uint16_t>(currentVehicleTypeData->vehicleType);

        // Is the vehicle type a locomotive?
        if ((newVehicleTypeId >= vehicleLocomotivesMin) && (newVehicleTypeId <= vehicleLocomotivesMax))
        {
          //If Loco found then reset and fetch the cars again
          if (previousCarsInOrder < currentCarsInOrder)
          {
            previousCarsInOrder = currentCarsInOrder;
          }
          currentCarsInOrder = 0U;
        }
        else
        {
          currentCarsInOrder += numOfVeh;
        }
      }
    }

    /******************************************************************************
    * publishData
    ******************************************************************************/
    bool RadioMessageInTrainSetupBHP::publishData()
    {
      // To set the maximum values of consecutive cars in order in the current cycle.
      uint32_t maxCurrentCarsInOrder = ATC::ATCMath::maximum(currentCarsInOrder, previousCarsInOrder);
      const uint32_t calcConsecutiveCarlength = static_cast<uint32_t>(ATC::ATCMath::instance().signMul(
        trainSetup.trainLength, maxCurrentCarsInOrder, __FILE__, __LINE__));
      const uint32_t maxConsecutiveCarLength = static_cast<uint32_t>(ATC::ATCMath::instance().unsignDiv(
        calcConsecutiveCarlength, trainSetup.noOfVehicles, __FILE__, __LINE__));
      ATP::DS::TSetup::instance().setMaxConsectiveCarLength(maxConsecutiveCarLength);
      ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "maxConsecutiveCarLength: ", maxConsecutiveCarLength, __FILE__);

      // Set and validate the static BHPB Configuration version
      ModeControl::instance().setStaticConfigurationVersion(bhpConfigVersion.configurationMajorVersion,
        bhpConfigVersion.configurationMinorVersion);

      bool publishDataValid = RadioMessageInTrainSetup::publishData();
      currentCarsInOrder = 0U;
      previousCarsInOrder = 0U;
      return publishDataValid;
    }
  }
}
