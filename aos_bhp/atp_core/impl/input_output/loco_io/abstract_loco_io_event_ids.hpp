#ifndef Abstract_LocoIO_EventIDs_hpp
#define Abstract_LocoIO_EventIDs_hpp
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
* The Unique Event Ids used by the Loco IO Component while creating events
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
   namespace IO
   {
      /**
      * Event Ids to report Errors to event handler
      * By keeping them in the namespace we will not get Lint-warnings from
      * constructors accessing them.
      */
      static const uint16_t eventIdErrInputRegisterFail = 0x01U;
      static const uint16_t eventIdErrInputRegisterResultFail = 0x02U;
      static const uint16_t eventIdErrInputRegisterResultIncorrect = 0x03U;
      static const uint16_t eventIdErrInputDeviceStateFail = 0x04U;
      static const uint16_t eventIdErrInputRevIdIncorrect = 0x05U;
      static const uint16_t eventIdErrInputHwConfIncorrect = 0x06U;
      static const uint16_t eventIdErrOutputRegisterFail = 0x07U;
      static const uint16_t eventIdErrOutputRegisterResultFail = 0x08U;
      static const uint16_t eventIdErrOutputRegisterResultIncorrect = 0x09U;
      static const uint16_t eventIdErrOutputDeviceStateFail = 0x0AU;
      static const uint16_t eventIdErrOutputRevIdIncorrect = 0x0BU;
      static const uint16_t eventIdErrOutputHwConfIncorrect = 0x0CU;
      static const uint16_t eventIdErrWritingVIOH = 0x0DU;
      static const uint16_t eventIdFailureReadingVIOH = 0x0EU;
      static const uint16_t eventIdFeedbackFailureVIOH = 0x0FU;
      static const uint16_t eventIdErrFeedbackSVDAlreadyTriggered = 0x10U;
      static const uint16_t eventIdErrFeedbackSVDTrigger = 0x11U;
      static const uint16_t eventIdErrFeedbackGPIORegisterSVDFail = 0x12U;
      static const uint16_t eventIdErrFeedbackGPIORegisterSVDResultCallFail = 0x13U;
      static const uint16_t eventIdErrFeedbackCannotGetGPIORegisterSVDResult = 0x14U;
      static const uint16_t eventIdlcsReadyInactiveStandstill = 0x15U;
      static const uint16_t eventIdErrVIOCrossCompareFailed = 0x16U;
      static const uint16_t eventIdDigitalOutputOutOfRange = 0x17U;
      static const uint16_t eventIdNoOrMoreThanOneATOSwitchModeSelected = 0x18U;
      static const uint16_t eventIdIsolationSwitchNotInRunMode = 0x19U;
      static const uint16_t eventIdAmbiguousTravelDir = 0x1AU;
      static const uint16_t eventIdNoActiveDir = 0x1BU;
      static const uint16_t eventIdStandStillDrivingDirNotSet = 0x1CU;
      static const uint16_t eventIdVitalDriverFailure = 0x1DU;
      static const uint16_t eventIdVitalDriverOutputFailure = 0x1EU;
      static const uint16_t eventIdVitalDriverOutputError = 0x1FU;
      static const uint16_t eventIdFeedbackErrorVIOH = 0x20U;
      static const uint16_t eventIdErrorReadingVIOH = 0x21U;
      static const uint16_t eventIdVersionMismatchVIOH = 0x22U;
   }
}
#endif
