/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (TCC->AOS) has an associated parser class inherited from AbstractRadioMessageIn.
* This file implements the parser for the ConfigurationData message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-02-28    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "abstract_mode_control.hpp"
#include "radio_message_in_configuration_data.hpp"
#include "abstract_config.hpp"
#include "abstract_message_handler.hpp"
#include "base_config_item.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_message_handler_event_ids.hpp"

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
    * RadioMessageInConfigurationData constructor
    ******************************************************************************/
    RadioMessageInConfigurationData::RadioMessageInConfigurationData()
      : AbstractRadioMessageIn(MTypeConfigurationData),
      invalidConfigDataFromTCC(ATC::Event::createSafetyHaltEvent(atpMessageHandlerId, ATC::CoreContainer,
        eventIdInvalidConfigurationData, ATC::NoEB, DMICom::msgHdlrInvalidConfigDataByTCC, "Invalid Configuration Data from TCC:", true)),
      configValueReplacedByTCC(ATC::Event::createLogEvent(atpMessageHandlerId, ATC::CoreContainer,
        eventIdConfigValueReplaced, 0U, "Current values of ConfigurationData replaced with received values"))     
    {
      implemented = true;

      configData.configDataId = 0U;
      // Setup Fixed Sizes for track data vectors.
      configData.configDataVec.reserve(ConfigurationDataSize);

      configDataReceived = false;
    }

    /******************************************************************************
    * RadioMessageInConfigurationData::validate
    ******************************************************************************/
    bool RadioMessageInConfigurationData::validate()
    {
      trace->write(ATC::briefTrace, "Validating ConfigurationData");
      bool ret;

      ret = AbstractRadioMessageIn::validate();

      if (ret)
      {
        ret = false;
        // Parse, validate and publish data
        if (DataAvailable == dataProcessState)
        {
          if (parseMessageData())
          {
            if (validateData())
            {
              if (validateMode())
              {
                dataProcessState = DataValidated;
                //set the runtime configuration data from TCC
                setValidatedConfigData();
                ret = true;
              }
            }
          }
          else
          {
            setInvalidationReason("RadioMessageInConfigurationData: Parse failed!");
          }
        }
        // Flag that Configuration data is received and process-state is updated
        configDataReceived = true;
      }

      return (ret);
    }

    /******************************************************************************
    * RadioMessageInConfigurationData::validateMode
    ******************************************************************************/
    bool RadioMessageInConfigurationData::validateMode()
    {
      bool modeValid = false;
      bool isDriverNotLogedIn = false;

      //Check if the driver is logged on or not
      if (DriverLoginSeq::driverLoggedIn != AbstractModeControl::corePtr()->getDriverLoginSeqState())
      {
        isDriverNotLogedIn = true;
      }

      // Check if the configuration data is received or not
      const bool configDataStatus = AbstractMessageHandler::corePtr()->isConfigDataReceived();

      // Fetch the current mode
      const ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();

      switch (mode)
      {
      case ATPModePowerUp:
      case ATPModeConfiguration:
      case ATPModeYard:
        if ((isDriverNotLogedIn) && (!configDataStatus))
        {
          modeValid = true;
        }
        break;

      case ATPModeSleeping:
      {
        if (IO::AbstractLocoIO::corePtr()->getSleepingSignal())
        {
          //Reject the incoming message from TCC if the sleep signal is active
          invalidIncmgMsgTCC.setDynamicText("Configuration Data");
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
        }
        else if (isDriverNotLogedIn && (!configDataStatus))
        {
          modeValid = true;
        }
        else if (!isDriverNotLogedIn)
        {
          setInvalidationReason("Config data received, driver not logged in!");
        }
        else
        {
          setInvalidationReason("Config data already received!");
        }

        break;
      }

      case ATPModeRegistration:
      case ATPModeBaliseSearch:
      case ATPModeNormal:
      case ATPModeLocation:
      case ATPModeStaffResponsible:
      case ATPModeSplit:
      case ATPModeJoin:
      case ATPModeShuntingRoute:
      case ATPModePoweringDown:
      case ATPModeShunting:
      case ATPModeUnregistered:
      case ATPModeSafetyHalt:
      case ATPModeSafeBrakeToStop:
      case ATPModePossession:
        setInvalidationReason("Configuration Data message received in invalid ATP mode");
        break;

      case ATPModeUndefined:
      case ATPModesCount:
      default:
        ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        break;
      }
      traceValidateMode(modeValid);

      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInConfigurationData::parseMessageData
    ******************************************************************************/
    bool RadioMessageInConfigurationData::parseMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));
      vfwSetReadBuffer(&buffer, sizeof(messageData.message.data));

      // Read & validate NID_MESSAGE_TYPE
      if (vfwGetU8(&buffer) != static_cast<uint8_t>(messageType))
      {
        trace->write(ATC::detailedTrace, "NID_MESSAGE_TYPE invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("NID_MESSAGE_TYPE");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

      // Read NID_MSG
      configData.configDataId = vfwGetU8(&buffer);

      // BlockData
      uint8_t nextMsgIdentifier = vfwGetU8(&buffer);

      // Fetch next data-block until M_END_OF_MESSAGE
      while ((nextMsgIdentifier != M_END_OF_MESSAGE) && (parseDataValid))
      {
        switch (nextMsgIdentifier)
        {
        case BTypeConfigurationData:
        {
          ConfigDataStruct configurationDataValue;
          configurationDataValue.configurationId = ATC::maxConfigItems;

          //Read the Parameter name(TID_TEXT_STRING)
          memset(&configurationDataValue.textStringName[0], 0, sizeof(configurationDataValue.textStringName));
          for (uint8_t i = 0U; i < maxTextStringLength; i++)
          {
            configurationDataValue.textStringName[i] = static_cast<char_t>(vfwGetI8(&buffer));
          }

          //Read the Value String(TID_TEXT_STRING)
          memset(&configurationDataValue.textStringValue[0], 0, sizeof(configurationDataValue.textStringValue));
          for (uint8_t i = 0U; i < maxTextStringLength; i++)
          {
            configurationDataValue.textStringValue[i] = static_cast<char_t>(vfwGetI8(&buffer));
          }

          //Get the configuration Id of the configuration Parameter received
          for (ATC::ConfigIdType id = ATC::firstConfigItem; id < ATC::maxConfigItems; ++id)
          {
            ATC::BaseConfigItem* configItem = ATC::AbstractConfigBase::basePtr()->getConfigItem(id);

            if (configItem != NULL)
            {
              if ((strncmp(&(configurationDataValue.textStringName[0]), configItem->getGlobalName(), maxTextStringLength) == 0))
              {
                //Set the Config Id to the parameter received
                configurationDataValue.configurationId = configItem->getConfigItemId();
                break;
              }
            }
          }

          if (configurationDataValue.configurationId == ATC::maxConfigItems)
          {
            trace->write(ATC::detailedTrace, "Unknown parameter in Configuration Data");
            invalidConfigDataFromTCC.setDynamicText(&configurationDataValue.textStringName[0]);
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidConfigDataFromTCC, __FILE__, __LINE__);
            parseDataValid = false;
          }
          else if (configData.configDataVec.size() >= ConfigurationDataSize)
          {
            trace->write(ATC::detailedTrace, "Configuration Data overflow");
            parseDataValid = false;
            invalidConfigDataFromTCC.setDynamicText("Overflow");
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidConfigDataFromTCC, __FILE__, __LINE__);
          }
          else
          {
            configData.configDataVec.push_back(configurationDataValue);
          }
          break;
        }
        default:
          parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
          break;
        } // end switch
        // Fetch next msg-type (or M_END_OF_MESSAGE)
        nextMsgIdentifier = vfwGetU8(&buffer);
      }

      if (configData.configDataVec.size() == 0U)
      {
        trace->write(ATC::detailedTrace, "No Config Data");
        parseDataValid = false;
        invalidConfigDataFromTCC.setDynamicText("No Cfg Data in packet!");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidConfigDataFromTCC, __FILE__, __LINE__);
      }
      else if ((!validateSizeOfParsedBytes(&buffer)) && (parseDataValid))
      {
        invalidConfigDataFromTCC.setDynamicText("Msg size incorrect");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidConfigDataFromTCC, __FILE__, __LINE__);
        parseDataValid = false;
      }
      else
      {
        // Config data OK
      }

      // Log according to trace-level
      detailedLog();
      veryDetailedLog();

      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInConfigurationData::validateData
    ******************************************************************************/
    bool RadioMessageInConfigurationData::validateData()
    {
      return true; // The core validation is done above, in parseMessageData()
    }

    /******************************************************************************
    * RadioMessageInConfigurationData::getConfigDataReceived
    ******************************************************************************/
    bool RadioMessageInConfigurationData::getConfigDataReceived() const
    {
      return ((DataValidated == dataProcessState) && configDataReceived);
    }

    /******************************************************************************
    * RadioMessageInConfigurationData::getConfigDataReceived
    ******************************************************************************/
    bool RadioMessageInConfigurationData::getConfigDataReceived(uint8_t& id, uint16_t& replyChannelId) const
    {
      id = configData.configDataId;
      replyChannelId = messageData.channelId;

      return configDataReceived;
    }

    /******************************************************************************
    * RadioMessageInConfigurationData::invalidate
    ******************************************************************************/
    void RadioMessageInConfigurationData::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      configData.configDataId = 0U;
      configData.configDataVec.clear();
      dataProcessState = NoDataAvailable;
      configDataReceived = false;
    }

    /******************************************************************************
    * RadioMessageInConfigurationData::setvalidatedConfigData
    ******************************************************************************/
    void RadioMessageInConfigurationData::setValidatedConfigData() const
    {
      bool isConfigDataValueReplacedByTCC = true;
      for (uint8_t i = 0U; i < configData.configDataVec.size(); i++)
      {
        bool putConfigStringStatus = ATC::AbstractConfigBase::basePtr()->putConfigString(
          configData.configDataVec[i].configurationId, &configData.configDataVec[i].textStringValue[0]);

        if (!putConfigStringStatus)
        {
          //Report a safety halt event
          invalidConfigDataFromTCC.setDynamicText(configData.configDataVec[i].configurationId);
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidConfigDataFromTCC, __FILE__, __LINE__);
          isConfigDataValueReplacedByTCC = false;
        }
      }
      if (isConfigDataValueReplacedByTCC)
      {
        //Issue a log event when the configuration parameters are replaced with the received ConfigurationData msg
        ATC::AbstractEventHandler::corePtr()->reportEvent(configValueReplacedByTCC, __FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * RadioMessageInConfigurationData::detailedLog
    ******************************************************************************/
    void RadioMessageInConfigurationData::detailedLog() const
    {
      uint8_t currentLevel;
      bool isEnabled;

      trace->getTraceDetails(currentLevel, isEnabled);

      if (isEnabled && (currentLevel >= ATC::detailedMessageTrace))
      {   // No reason to assemble logStr if trace not enabled
        char_t logStr[120];

        //lint -e{586} snprintf is needed here
        const int32_t res = snprintf(&logStr[0], sizeof(logStr), "NID_MSG=%u", static_cast<uint32_t> (configData.configDataId));

        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
        {
          traceLog(ATC::detailedMessageTrace, ATC::DetailedLog, &logStr[0]);
        }
      }
    }

    /******************************************************************************
    * RadioMessageInConfigurationData::veryDetailedLog
    ******************************************************************************/
    void RadioMessageInConfigurationData::veryDetailedLog() const
    {
      uint8_t currentLevel;
      bool isEnabled;

      trace->getTraceDetails(currentLevel, isEnabled);

      if (isEnabled && (currentLevel >= ATC::veryDetailedMessageTrace))
      {   // No reason to assemble logStr if trace not enabled     
        if (configData.configDataVec.size() > 0U)
        {
          char_t logStr[120];
          std::vector<ConfigDataStruct>::const_iterator configDataIt;
          for (configDataIt = configData.configDataVec.begin(); configDataIt != configData.configDataVec.end(); ++configDataIt)
          {
            //lint -e{586} snprintf is needed here
            const int32_t res = snprintf(&logStr[0], sizeof(logStr), "CONFIGURATION_DATA: TID_TEXT_STRING=%s,TID_TEXT_STRING=%s",
              configDataIt->textStringName,
              configDataIt->textStringValue);

            if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
            {
              traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
            }
          }
        }
        else
        {
          traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, "CONFIGURATION_DATA:-");
        }
      }
    }
  }
}
