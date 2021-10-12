#ifndef Abstract_Supervise_EventIDs_hpp
#define Abstract_Supervise_EventIDs_hpp
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
* The Unique Event Ids used by the Supervise Component while creating events
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
      static const uint16_t eventIdServiceBrakeCeilingSpeed = 0x01U;
      static const uint16_t eventIdServiceBrakeTargetSupervision = 0x02U;
      static const uint16_t eventIdEmergencyBrakeCeilingSpeed = 0x03U;
      static const uint16_t eventIdEmergencyBrakeTargetSupervision = 0x04U;
      static const uint16_t eventIdStandstillCeilingSpeedZero = 0x05U;
      static const uint16_t eventIdStandstillNoTarget = 0x06U;
      static const uint16_t eventIdTargetListError = 0x07U;
      static const uint16_t eventIdLocationBorderExceeded = 0x08U;
      static const uint16_t eventIdServiceBrakeStandstillSupervision = 0x09U;
      static const uint16_t eventIdStandstillATPMode = 0x0AU;
      static const uint16_t eventIdServiceBrakeTIMSBroken = 0x0BU;
      static const uint16_t eventIdFirstWarnSpeedExceed = 0x0CU;
      static const uint16_t eventIdSecondWarnSpeedExceed = 0x0DU;
      static const uint16_t eventIdWarningLimitExceed = 0x0EU;
      static const uint16_t eventIdCeilingSpeedExceed = 0x0FU;
      static const uint16_t eventIdRevDistExceeded = 0x10U;
      static const uint16_t eventIdRollAwayDistExceeded = 0x11U;
      static const uint16_t eventIdRevEBMarginExceeded = 0x12U;
      static const uint16_t eventIdRollAwayEBMarginExceeded = 0x13U;
      static const uint16_t eventIdEmergencyBrakeStandstillSupervision = 0x14U;
   }
}
#endif
