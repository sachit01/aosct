#ifndef Abstract_Brake_EventIDs_hpp
#define Abstract_Brake_EventIDs_hpp
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
* The Unique Event Ids used by the Brake Component while creating events
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
   namespace Supv
   {
      /**
      * Event Ids to report Errors to event handler
      * By keeping them in the namespace we will not get Lint-warnings from
      * constructors accessing them.
      */
      static const uint16_t eventIdReportEventEBSuprvision = 0x01U;
      static const uint16_t eventIdReportEventbReqSB = 0x02U;
      static const uint16_t eventIdReportEventEBRelayFb = 0x03U;
      static const uint16_t eventIdBrakeTestInProgress = 0x04U;
      static const uint16_t eventIdBrakeTestExecTimeout = 0x05U;
      static const uint16_t eventIdBrakeTestFailed = 0x06U;
      static const uint16_t eventIdBrakeTestSuccessful = 0x07U;
      static const uint16_t eventIdUnableToStartBrakeTest = 0x08U;
      static const uint16_t eventIdBrakeTestFailedEbInternal = 0x09U;
      static const uint16_t eventIdBrakeTestFailedExternal = 0x0AU;
      static const uint16_t eventIdCabinDeactivated = 0x0BU;
      static const uint16_t eventIdAdditionalBrakeOrders = 0x0CU;
      static const uint16_t eventIdEmergencyAlertActive = 0x0DU;
      static const uint16_t eventIdBrakeTestAbortedByDriver = 0x0EU;
      static const uint16_t eventIdStartingBrakeTest = 0x0FU;
      static const uint16_t eventIdBrakeTestAborted = 0x10U;
   }
}
#endif
