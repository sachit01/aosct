#ifndef Targets_EventIDs_hpp
#define Targets_EventIDs_hpp
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
* The Unique Event Ids used by the Targets Component while creating events
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
      static const uint16_t eventIdInvalidMemForPrim = 0x01U;
      static const uint16_t eventIdInvalidMemForKeepTrkData = 0x02U;
      static const uint16_t eventIdInvalidMemForGrad = 0x03U;
      static const uint16_t eventIdInvalidMemForSpeed = 0x04U;
      static const uint16_t eventIdMemNotSet = 0x05U;
      static const uint16_t eventIdInvalidMemForTrkData = 0x06U;
      static const uint16_t eventIdTargetListFull = 0x07U;
      static const uint16_t eventIdInvalidMemForSupervised = 0x08U;
      static const uint16_t eventIdNullPointer = 0x09U;
      static const uint16_t eventIdUnknownDir = 0x0AU;
      static const uint16_t eventIdInconsistencyError = 0x0BU;
      static const uint16_t eventIdSameTargetId = 0x0CU;
      //                                          0x0DU;
      static const uint16_t eventIdInvalidDirection = 0x0EU;
      static const uint16_t eventIdExceededContSize = 0x10U;
      static const uint16_t eventIdTwoSupGradSamePos = 0x11U;
      static const uint16_t eventIdNullPointerAcess = 0x12U;
   }
}
#endif
