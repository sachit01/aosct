#ifndef Abstract_Position_EventIDs_hpp
#define Abstract_Position_EventIDs_hpp
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
      static const uint16_t eventIdMissedBalise = 0x01U;
      static const uint16_t eventIdYardMode = 0x02U;
      static const uint16_t eventIdOutSideBalise = 0x03U;
      static const uint16_t eventIdUnExpBalise = 0x04U;
      // 0x05U;
      static const uint16_t eventIdOutOfRange = 0x06U;
      static const uint16_t eventIdInvalidBaliseInBalisesearch = 0x07U;
      static const uint16_t eventIdFirstMissedBalise = 0x08U;
      static const uint16_t eventIdIdenticalBaliseFound = 0x09U;
      static const uint16_t eventIdFrontOrRearEndOutOfLimit = 0x0AU;
      static const uint16_t eventIdSecBaliseFoundBeforeMA = 0x0BU;
      static const uint16_t eventIdThirdBalError = 0x0CU;
      static const uint16_t eventIdInvalBalInPos = 0x0DU;
      static const uint16_t eventIdBaliseDetectedIrrelevantMode = 0x0EU;
      static const uint16_t eventIdBaliseDetected = 0x0FU;
   }
}
#endif
