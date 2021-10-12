#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          OBRDSimCommon.cpp %
*
*  %version:       1 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2018-10-11 12:40 %
*
*  DESCRIPTION:
*
*  Common utility functions for the OBRD Simulator.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-11    marlundg    File created
*
*******************************************************************************/
#include "stdafx.h"
#include <time.h>


namespace OBRDSimDLL {
  
  /***********************************************************************************************************
  * Function:     getUTCTime
  * Description:  Get current UTC time
  ***********************************************************************************************************/
  unsigned long long getUTCTime()
  {
    time_t secs;
    time(&secs);

    return(static_cast<unsigned long long>(secs));
  }
}