#ifndef Abstract_Mode_Control_EventIDs_hpp
#define Abstract_Mode_Control_EventIDs_hpp
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
* The Unique Event Ids used by the Mode Control Component while creating events
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
      static const uint16_t eventIdSWNameAndVersion = 0x01U;
      static const uint16_t eventIdCommonConfigNameAndVersion = 0x02U;
      static const uint16_t eventIdMaintConfigNameAndVersion = 0x03U;
      static const uint16_t eventIdRuntimeConfigNameAndVersion = 0x04U;
      static const uint16_t eventIdHWDigitalInputNameAndVersion = 0x05U;
      static const uint16_t eventIdHWDigitalOutputNameAndVersion = 0x06U;
      static const uint16_t eventIdHWAnalogInputNameAndVersion = 0x07U;
      static const uint16_t eventIdVFWNameAndVersion = 0x08U;
      static const uint16_t eventIdSDPNameAndVersion = 0x09U;
      static const uint16_t eventIdBrakeToCheckOBRDFeedback = 0x0AU;
      static const uint16_t eventIdLastCarBrakePressureNotValid = 0x0BU;
      static const uint16_t eventIdLastCarBrakePressurePassed = 0x0CU;
      static const uint16_t eventIdDispatcherNameAndVersion = 0x0DU;
      static const uint16_t eventIdViohClientNameAndVersion = 0x0EU;
      static const uint16_t eventIdViohServerNameAndVersion = 0x0FU;
      static const uint16_t eventIdOpcVersion = 0x10U;
      static const uint16_t eventIdADSVersionMismatch = 0x11U;
      static const uint16_t eventIdLastCarBrakePressureTestInProgress = 0x12U;
      static const uint16_t eventIdLastCarBrakePressureTestAborted = 0x13U;
      static const uint16_t eventIdTypeConfigNameAndVersion = 0x14U;
      static const uint16_t eventIdInstanceConfigNameAndVersion = 0x15U;
   }
}
#endif
