#ifndef TIMS_EventIDs_hpp
#define TIMS_EventIDs_hpp
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
* The Unique Event Ids used by the TIMS component to create events
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2019-11-08    csundin     Created
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
    static const uint16_t eventIdObrdOutsideTrain = 0x01U;
    static const uint16_t eventIdObrdValidationIncomingMessageFailed = 0x02U;
    static const uint16_t eventIdObrdEstablishedConnection = 0x03U;
    static const uint16_t eventIdObrdLostConnection = 0x04U;
    static const uint16_t eventIdObrdWrongMessageType = 0x05U;
    static const uint16_t eventIdObrdWrongProtocolVersion = 0x06U;

  }
}
#endif
