#ifndef Supervise_EventIDs_hpp
#define Supervise_EventIDs_hpp
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
* 2019-04-24    pparthib    Created
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
    static const uint16_t eventIdExtdRevDistExceeded = 0x01U;
    static const uint16_t eventIdExtdRevEBMarginExceeded = 0x02U;
    static const uint16_t eventIdEBCeilingSpeed = 0x03U;
    static const uint16_t eventIdSBCeilingSpeed = 0x04U;    
  }
}
#endif
