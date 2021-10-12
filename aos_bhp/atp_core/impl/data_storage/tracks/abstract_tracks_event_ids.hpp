#ifndef Tracks_EventIDs_hpp
#define Tracks_EventIDs_hpp
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
* The Unique Event Ids used by the Tracks Component while creating events
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
      static const uint16_t eventIdInvalidPosInTrack = 0x01U;
      static const uint16_t eventIdTrackListFullError = 0x02U;
      static const uint16_t eventIdBaliseListFullError = 0x03U;
      static const uint16_t eventIdInitNotDone = 0x04U;
      static const uint16_t eventIdOutOfMemory = 0x05U;
      static const uint16_t eventIdPosUndef = 0x06U;
      static const uint16_t eventIdInconsistencyBaliseTrack = 0x07U;
      static const uint16_t eventIdInvalidUpdateStartPoint = 0x08U;
      static const uint16_t eventIdInvalidIncomingTrack = 0x09U;
      static const uint16_t eventIdInvalidIncomingTrackDir = 0x0AU;
      static const uint16_t eventIdModifyExistingBalise = 0x0BU;
      static const uint16_t eventIdModifyExistingTrack = 0x0CU;
      static const uint16_t eventIdRemoveExistingBaliseFailure = 0x0DU;
      static const uint16_t eventIdTrackListInternalError = 0x0EU;
   }
}
#endif
