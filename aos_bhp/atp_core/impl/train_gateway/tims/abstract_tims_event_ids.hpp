#ifndef AbstractTIMS_EventIDs_hpp
#define AbstractTIMS_EventIDs_hpp
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
* The Unique Event Ids used by the AbstractTIMS component to create events
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-12-11    csundin     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace TG
  {
    /**
    * Event Ids to report Errors to event handler
    * By keeping them in the namespace we will not get Lint-warnings from
    * constructors accessing them.
    */
    static const uint16_t eventIdIntegrityBroken = 0x01U;
    static const uint16_t eventIdIntegrityIntact = 0x02U;
  }
}
#endif
