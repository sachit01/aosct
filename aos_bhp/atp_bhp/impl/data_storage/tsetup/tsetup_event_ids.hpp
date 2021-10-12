#ifndef TSetup_EventIDs_hpp
#define TSetup_EventIDs_hpp
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
* The Unique Event Ids used by the TSetup Component while creating events
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
   namespace DS
   {
      /**
      * Event Ids to report Errors to event handler
      * By keeping them in the namespace we will not get Lint-warnings from
      * constructors accessing them.
      */
      static const uint16_t eventIdDataValidTSetup = 0x01U;
      static const uint16_t eventIdOutOfIndexTSetup = 0x02U;
      static const uint16_t eventIdOutOfRangeTSetup = 0x03U;
      static const uint16_t eventIdBrakeHandlingEvent = 0x04U;
      static const uint16_t eventIdStandstillEcpbReportedChangedRunMode = 0x05U;
      static const uint16_t eventIdEcpbReportedTooLowPercentageOfWorkingBrakes = 0x06U;
      static const uint16_t eventIdStandStillTooLowPercentageOfWorkingBrakes = 0x07U;
      static const uint16_t eventIdSBLowLambdaPercentage = 0x08U;
      static const uint16_t eventIdStandStillLowLambdaPercentage = 0x09U;
      static const uint16_t eventIdSBEcpbReportedChangedRunMode = 0x0AU;
      static const uint16_t eventIdBrakeSystemUpdated = 0x0BU;
   }
}
#endif
