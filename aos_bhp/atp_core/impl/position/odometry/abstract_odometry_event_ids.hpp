#ifndef Abstract_Odometry_EventIDs_hpp
#define Abstract_Odometry_EventIDs_hpp
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
* The Unique Event Ids used by the Odometry Component while creating events
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
   namespace Pos
   {
      /**
      * Event Ids to report Errors to event handler
      * By keeping them in the namespace we will not get Lint-warnings from
      * constructors accessing them.
      */
      static const uint16_t eventIdConfigFailure = 0x01U;
      static const uint16_t eventIdInvalidState = 0x02U;
      static const uint16_t eventIdNoConfigRespRecvFailure = 0x03U;
      static const uint16_t eventIdNoDataTelFailure = 0x04U;
      static const uint16_t eventIdBaliseWindowFailure = 0x05U;
      static const uint16_t eventIdSafetyIssueCodData = 0x06U;
      static const uint16_t eventIdVersionCODMismatch = 0x07U;
      static const uint16_t eventIdInvalidDataTelegram = 0x08U;
      static const uint16_t eventIdOdometerSpeedSensorFailureOccurred = 0x09U;
      static const uint16_t eventIdOdometerSpeedSensorFailureRecovered = 0x0AU;
      static const uint16_t eventIdTachometer1SensorFailureOccured = 0x0BU;
      static const uint16_t eventIdTachometer2SensorFailureOccured = 0x0CU;
      static const uint16_t eventIdDopplerSensorFailureOccured = 0x0DU;
      static const uint16_t eventIdTachometer1SensorFailureRecovered = 0x0EU;
      static const uint16_t eventIdTachometer2SensorFailureRecovered = 0x0FU;
      static const uint16_t eventIdDopplerSensorFailureRecovered = 0x10U;
      static const uint16_t eventIdSlipDetected= 0x11U;
      static const uint16_t eventIdSlideDetected = 0x12U;
      static const uint16_t eventIdDopplerSensorNeedMaintenance = 0x13U;
      static const uint16_t eventIdOdometerSpeedSensorDiffFailureOccurred = 0x14U;
   }
}
#endif
