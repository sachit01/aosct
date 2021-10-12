#ifndef Abstract_Message_Handler_EventIDs_hpp
#define Abstract_Message_Handler_EventIDs_hpp
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
* The Unique Event Ids used by the Message Handler Component while creating events
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
   namespace Kernel
   {
      /**
      * Event Ids to report Errors to event handler
      * By keeping them in the namespace we will not get Lint-warnings from
      * constructors accessing them.
      */
      static const uint16_t eventIdReceiveQueueFullError = 0x01U;
      static const uint16_t eventIdSendQueueFullError = 0x02U;
      static const uint16_t eventIdValidationIncomingMessageFailed = 0x03U;
      static const uint16_t eventIdValidationOutgoingMessageFailed = 0x04U;
      static const uint16_t eventIdInvalidMessageType = 0x05U;
      static const uint16_t eventIdParserNotImplemented = 0x06U;
      static const uint16_t eventIdParserNullPtr = 0x07U;
      static const uint16_t eventIdNoValidPositionreport = 0x08U;
      static const uint16_t eventIdProtocolVersionMismatch = 0x09U;
      static const uint16_t eventIdNoBaliseInfoAvailable = 0x0AU;
      static const uint16_t eventIdFirstBaliseNotInMA = 0x0BU;
      //0x0CU;
      static const uint16_t eventIdInvalidStartupMessage = 0x0DU;
      static const uint16_t eventIdInvalidDriverLogonStatusSeq = 0x0EU;
      static const uint16_t eventIdInvalidQSetup = 0x0FU;
      static const uint16_t eventIdInvalidConfigurationData = 0x10U;
      static const uint16_t eventIdDiscardUnCondSafeBrakeToStopInBaliseSearch = 0x11U;
      static const uint16_t eventIdUnRegMsgRcvdLogEvent = 0x12U;
      static const uint16_t eventIdUnRegMsgSafetyHalt = 0x13U;
      static const uint16_t eventIdProtocolVersionUnrecovMismatch = 0x14U;
      static const uint16_t eventIdInvalidIncomingMsg = 0x15U;
      static const uint16_t eventIdConfigValueReplaced = 0x16U;
      static const uint16_t eventIdInvalidLambdaReceived = 0x17U;
      static const uint16_t eventIdTrainSetupRejectedByAOS = 0X18U;
      static const uint16_t eventIdInvalidMaInReposition = 0x19U;
      static const uint16_t eventIdATPMessageHandlerCriticalState = 0x1AU;
      static const uint16_t eventIdInvalidMaInReregistration = 0x1BU;
      static const uint16_t eventIdInvalidMessage = 0x1DU;
      static const uint16_t eventIdInvalidDataInMessage = 0x1EU;
      static const uint16_t eventIdTooManyBalisesInPossessionAck = 0x1FU;
      static const uint16_t eventIdInvalidBlockTypeInMessage = 0x20U;
      static const uint16_t eventIdInvalidBlockTypeInRejectedMessage = 0x21U;
      static const uint16_t eventIdUnknownReasonInRejectConfigMsg = 0x22U;
      static const uint16_t eventIdUnknownReasonInEmAlertMsg = 0x23U;
      static const uint16_t eventIdUnknownReasonInUnRegMsg = 0x24U;
   }
}
#endif
