#ifndef Asbtract_BTMHandler_EventIDs_hpp
#define Asbtract_BTMHandler_EventIDs_hpp
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
* The Unique Event Ids used by the BTM Handler Component while creating events
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
      const uint16_t eventIdErrorBtmStatusMessageInvalid = 0x01U;
      const uint16_t eventIdErrorSequenceOrBaliseNumber = 0x02U;
      const uint16_t eventIdErrorRoutineTestInProgress = 0x03U;
      const uint16_t eventIdErrorRoutineTestMandatory = 0x04U;
      const uint16_t eventIdErrorRoutineTestSucceded = 0x05U;
      const uint16_t eventIdErrorRoutineTestFailed = 0x06U;
      const uint16_t eventIdErrorMandatoryRoutineTestFailed = 0x07U;
      const uint16_t eventIdErrorBtmSupervisionFailed = 0x08U;
      const uint16_t eventIdErrorBrakeUntilStandstill = 0x09U;
      const uint16_t eventIdErrorCouldBeFaulty = 0x0AU;
      const uint16_t eventIdErrorStausIsGreen = 0x0BU;
      const uint16_t eventIdErrorSporadicError = 0x0CU;
      const uint16_t eventIdErrorTelegramInvalid = 0x0DU;
      const uint16_t eventIdErrorFailedBtm = 0x0EU;
      const uint16_t eventIdErrorOpcSupervisionFailed = 0x0FU;
   }
}
#endif
