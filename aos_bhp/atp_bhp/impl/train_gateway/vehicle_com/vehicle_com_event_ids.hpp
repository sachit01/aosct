#ifndef VehicleCom_EventIDs_hpp
#define VehicleCom_EventIDs_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*
* DESCRIPTION:
*
* The Unique Event Ids used by the Vehicle Com Component while creating events
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-07-24    akushwah    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
   namespace TG
   {
      /**
      * Event Ids to report Errors to event handler
      * By keeping them in the namespace we will not get Lint-warnings from
      * constructors accessing them.
      */
      static const uint16_t eventIdValidationIncomingMessageFailed = 0x01U;
      static const uint16_t eventIdValidationOutgoingMessageFailed = 0x02U;
      static const uint16_t eventIdInvalidMessageType = 0x03U;
      static const uint16_t eventIdParserNullPtr = 0x04U;
      static const uint16_t eventIdTimeDeviationResponseFromLCS = 0x05U;
      static const uint16_t eventIdLostConnectionWithLCSandECPB = 0x06U;
      static const uint16_t eventIdMissedMessageFromLCS = 0x07U;
      static const uint16_t eventIdParserNotImplemented = 0x08U;
      // Spare codes available here
      static const uint16_t eventIdSysFaultLossOfLeaderComm = 0x0AU;
      static const uint16_t eventIdSysFaultLossOfAirBrakeComm = 0x0BU;
      static const uint16_t eventIdEstablishedConnectionWithLCS = 0x0CU;
      static const uint16_t eventIdTimeDeviationRecoveredFromLCS = 0x0DU;
      static const uint16_t eventIdFaultyLigConfig = 0x0EU;
      static const uint16_t eventIdLostConnectionWithLCS = 0x0FU;
      // Spare codes available here
      static const uint16_t eventIdValidationCRCFailed = 0x13U;
      static const uint16_t eventIdInvalidMessageValue = 0x14U;
      static const uint16_t eventIdSysFaultRecoveryOfLeaderComm = 0x15U;
      static const uint16_t eventIdSysFaultRecoveryOfAirBrakeComm = 0x16U;
   }
}
#endif
