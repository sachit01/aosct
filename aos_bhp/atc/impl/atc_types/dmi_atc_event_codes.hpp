#ifndef DMI_ATCEventCodes_hpp
#define DMI_ATCEventCodes_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
* This file defines the DMI event code for ATC components which will be used as index of text message
* to display the text on DMI
* Below mentioned event codes need to update with respective of language INI file of DMI
******************************************************************************/


/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-16   spandita    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATC
{
  namespace DMICom
  {
    /**
    * ATC related DMI events ID's 320--340
    */
    static const DMIEventCode atcMathInternalCalculationFailure = 320U;
    static const DMIEventCode ChannelFailure = 321U;
    static const ATC::DMIEventCode IncorrectConfig = 322U;
    static const ATC::DMIEventCode NullPointerError = 323U;


    /**Add here Event ID's*/

  }
}

#endif
