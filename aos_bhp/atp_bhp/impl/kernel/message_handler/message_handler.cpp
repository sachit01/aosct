/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the MessageHandler class
* which contains the adapted functionality of the MessageHandler
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-09    bhermans    Created
* 2016-04-03    bhermans    Files renamed
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-08-26    marlundg    Updated for ATP-Limited
* 2016-08-27    adgupta     Updated Console call to return false for non-Message Handle calls.
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "message_handler.hpp"
#include "radio_channel.hpp"
#include "abstract_cross_compare.hpp"

#include "radio_message_in_ato_remote_control.hpp"
#include "radio_message_in_possession_acknowledge.hpp"
#include "radio_message_in_shunting_acknowledge.hpp"
#include "radio_message_in_join_command.hpp"
#include "radio_message_in_external_data.hpp"
#include "radio_message_in_reject_configuration.hpp"
#include "radio_message_in_area_request.hpp"
#include "radio_message_in_em_alert.hpp"
#include "radio_message_in_position_report_request.hpp"
#include "radio_message_in_driver_logon_status.hpp"
#include "radio_message_in_revoke_em_alert.hpp"
#include "radio_message_in_stop_train.hpp"
#include "radio_message_in_unregistration.hpp"
#include "radio_message_in_approximate_position.hpp"
#include "radio_message_in_yard_acknowledge.hpp"
#include "radio_message_in_protocol_version.hpp"
#include "radio_message_in_unconditional_shortening.hpp"
#include "radio_message_in_configuration_data_bhp.hpp"
#include "radio_message_in_path_bhp.hpp"
#include "radio_message_in_movement_authority_bhp.hpp"
#include "radio_message_in_train_setup_bhp.hpp"
#include "radio_message_in_command_message_bhp.hpp"

#include "radio_message_out_driver_information.hpp"
#include "radio_message_out_registration_area.hpp"
#include "radio_message_out_abort_setup.hpp"
#include "radio_message_out_train_registration_information.hpp"
#include "radio_message_out_protocol_version.hpp"
#include "radio_message_out_position_report_bhp.hpp"
#include "radio_message_out_startup_message_bhp.hpp"


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
    * Constructor
    ******************************************************************************/
    MessageHandler::MessageHandler(void) : AbstractMessageHandler(), initDone(false)
    {
      for (uint8_t i = 0U; i < static_cast<uint8_t>(sizeof(parsers) / sizeof(parsers[0])); i++)
      {
        parsers[i] = static_cast<AbstractRadioMessageIn*>(NULL);
      }

      for (uint8_t i = 0U; i < static_cast<uint8_t>(sizeof(creators) / sizeof(creators[0])); i++)
      {
        creators[i] = static_cast<AbstractRadioMessageOut*>(NULL);
      }
    }

    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    * (This info is not included in doxygen documentation but may be useful)
    *
    ******************************************************************************/
    MessageHandler& MessageHandler::instance(void)
    {
      static MessageHandler theOnlyMessageHandlerInstance;

      return theOnlyMessageHandlerInstance;
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool MessageHandler::init(void)
    {
      //lint --e{586} 'new' is acceptable during initialization

      // Only initialize once
      if (!initDone)
      {
        // Create and fill the container with parsers for incoming messages
        parsers[MTypePositionReportRequest] = new RadioMessageInPositionReportRequest();
        parsers[MTypeDriverLogonStatus] = new RadioMessageInDriverLogonStatus();
        parsers[MTypeEmergencyAlert] = new RadioMessageInEmAlert();
        parsers[MTypeMovementAuthority] = new RadioMessageInMovementAuthorityBHP();
        parsers[MTypeTrainSetup] = new RadioMessageInTrainSetupBHP();
        parsers[MTypeUnregister] = new RadioMessageInUnregistration();
        parsers[MTypeATORemoteControl] = new RadioMessageInATORemoteControl();
        parsers[MTypeStopTrain] = new RadioMessageInStopTrain();
        parsers[MTypeRevokeEmergencyAlert] = new RadioMessageInRevokeEmAlert();
        parsers[MTypeApproximatePosition] = new RadioMessageInApproximatePosition();
        parsers[MTypePossessionAcknowledge] = new RadioMessageInPossessionAcknowledge();
        parsers[MTypeShuntingAcknowledge] = new RadioMessageInShuntingAcknowledge();
        parsers[MTypeJoinCommand] = new RadioMessageInJoinCommand();
        parsers[MTypeExternalData] = new RadioMessageInExternalData();
        parsers[MTypeConfigurationData] = new RadioMessageInConfigurationDataBHP();
        parsers[MTypeCommandMessage] = new RadioMessageInCommandMessageBHP();
        parsers[MTypePath] = new RadioMessageInPathBHP();
        parsers[MTypeRejectConfiguration] = new RadioMessageInRejectConfiguration();
        parsers[MTypeAreaRequestMessage] = new RadioMessageInAreaRequest();
        parsers[MTypeYardAcknowledge] = new RadioMessageInYardAcknowledge();
        parsers[MTypeUnconditionalShortening] = new RadioMessageInUnconditionalShortening();

        // Insert any new parsers for incoming messages before parsers for common messages
        // Start of parsers for common Incoming messages
        // Create and fill the container with parsers for Common messages
        parsers[MTypeProtocolVersionIncoming] = new RadioMessageInProtocolVersion();

        messagesInParsers = parsers;

        const uint8_t startIndex = static_cast<uint8_t>(MTypeDriverInformation);

        // Create and fill the container with creators for outgoing messages
        creators[static_cast<uint8_t>(MTypeDriverInformation) - startIndex] = new RadioMessageOutDriverInformation();
        creators[static_cast<uint8_t>(MTypeStartUpMessage) - startIndex] = new RadioMessageOutStartUpMessageBHP();
        creators[static_cast<uint8_t>(MTypeAbortSetup) - startIndex] = new RadioMessageOutAbortSetup();
        creators[static_cast<uint8_t>(MTypeTrainRegistrationInformation) - startIndex] = new RadioMessageOutTrainRegistrationInformation();
        creators[static_cast<uint8_t>(MTypePositionReportRegion1) - startIndex] = new RadioMessageOutPositionReportBHP(RadioCom::radioChannelId2);
        creators[static_cast<uint8_t>(MTypePositionReportRegion2) - startIndex] = new RadioMessageOutPositionReportBHP(RadioCom::radioChannelId3);
        creators[static_cast<uint8_t>(MTypeRegistrationAreaMessage) - startIndex] = new RadioMessageOutRegistrationArea();

        // Insert any new creators for outgoing messages before the creators for common outgoing messages
        // Start of creators for common Outgoing messages
        // Create and fill the container with creators for Common messages
        creators[static_cast<uint8_t>(MTypeProtocolVersionOutgoing) - startIndex] = new RadioMessageOutProtocolVersion();

        messagesOutCreators = creators;

        initCrossCompare();

        initDone = true;
      }

      return initDone;
    }

    /******************************************************************************
    * getBHPBSafeForBoarding
    ******************************************************************************/
    bool MessageHandler::getBHPBSafeForBoarding(void) const
    {
      RadioMessageInCommandMessageBHP *radioMessageInCommandMessageBHP =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInCommandMessageBHP*> (messagesInParsers[MTypeCommandMessage], __FILE__, __LINE__);

      const bool retVal = radioMessageInCommandMessageBHP->getSafeForBoarding();

      return retVal;
    }

   /******************************************************************************
    * getStaticConfigurationVersionInPath
    ******************************************************************************/
    bool MessageHandler::getStaticConfigurationVersionInPath(uint8_t & configMajorVersion, uint8_t & configMinorVersion) const
    {
      RadioMessageInPathBHP *radioMessageInPathBHP =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInPathBHP*> (messagesInParsers[MTypePath], __FILE__, __LINE__);

      const bool retVal = radioMessageInPathBHP->getStaticConfigurationVersionInPath(configMajorVersion, configMinorVersion);

      return retVal;
    }

    /******************************************************************************
    * getRadioChannelName
    ******************************************************************************/
    bool MessageHandler::getRadioChannelName(char_t * const radioChannel) const
    {
      RadioMessageInCommandMessageBHP *radioMessageInCommandMessageBHP =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInCommandMessageBHP*>(messagesInParsers[MTypeCommandMessage], __FILE__, __LINE__);

      const bool retVal = radioMessageInCommandMessageBHP->getRadioChannelName(radioChannel);

      return retVal;
    }

    /******************************************************************************
    * getRadioChannelEnable
    ******************************************************************************/
    bool MessageHandler::getRadioChannelEnable() const
    {
      RadioMessageInCommandMessageBHP *radioMessageInCommandMessageBHP =
        ATC::dynamicCast<AbstractRadioMessageIn*, RadioMessageInCommandMessageBHP*>(messagesInParsers[MTypeCommandMessage], __FILE__, __LINE__);

      const bool retVal = radioMessageInCommandMessageBHP->getRadioChannelEnable();
      return retVal;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void MessageHandler::initCrossCompare() const
    {
      AbstractMessageHandler::initCrossCompare();

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&initDone));
    }
  }
}
