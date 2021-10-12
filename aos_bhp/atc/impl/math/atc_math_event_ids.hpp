#ifndef ATCMath_EventIDs_hpp
#define ATCMath_EventIDs_hpp
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
* The Unique Event Ids used by the ATC Math while creating events
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

namespace ATC
{
   /**
   * Event Ids to report Errors to event handler
   * By keeping them in the namespace we will not get Lint-warnings from
   * constructors accessing them.
   */
   static const uint16_t eventIdErrSignMul = 0x01U;
   static const uint16_t eventIdErrSignMulOpACast = 0x02U;
   static const uint16_t eventIdErrSignMulOpBCast = 0x03U;
   static const uint16_t eventIdErrUnSignDivZero = 0x04U;
   static const uint16_t eventIdErrSignDivDivisorOverflow = 0x05U;
   static const uint16_t eventIdErrSignDivDividendOverflow = 0x06U;
   static const uint16_t eventIdErrAbsOverflow = 0x07U;
}
#endif
