#ifndef MessageDispatcher_EventIDs__hpp
#define MessageDispatcher_EventIDs__hpp
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
* The Unique Event Ids used by the Message Dispatcher Component while creating events
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

namespace Dispatcher
{
   /**
   * Event Ids to report Errors to event handler
   * By keeping them in the namespace we will not get Lint-warnings from
   * constructors accessing them.
   */
   static const uint16_t eventIdInvalidConstructor = 0x01U;
   static const uint16_t eventIdConnecIdFailure = 0x02U;
   static const uint16_t eventIdWriteChannelFailure = 0x03U;
   static const uint16_t eventIdUdpSequenceFailure = 0x04U;
}
#endif
