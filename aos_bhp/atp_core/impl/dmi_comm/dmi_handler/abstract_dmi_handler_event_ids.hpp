#ifndef DMIHandler_EventIDs_hpp
#define DMIHandler_EventIDs_hpp
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
* The Unique Event Ids used by the DMI Handler Component while creating events
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
   namespace DMICom
   {
      /**
      * Event Ids to report Errors to event handler
      * By keeping them in the namespace we will not get Lint-warnings from
      * constructors accessing them.
      */
      static const uint16_t eventIdUnexpectedDMIMsg = 0x01U;
      static const uint16_t eventIdDriverInteractions = 0x02U;
      static const uint16_t eventIdDriverConfirmedTachometer1Failure = 0x03U;
      static const uint16_t eventIdDriverConfirmedTachometer2Failure = 0x04U;
      static const uint16_t eventIdDriverConfirmedDopplerFailure = 0x05U;
   }
}
#endif
