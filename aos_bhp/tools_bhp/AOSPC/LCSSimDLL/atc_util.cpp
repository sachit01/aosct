/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          atc_util.cpp %
*
*  %version:       1 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-02-15 16:22 %
*
*  DESCRIPTION:
*  Wrapper functions for VFW calls in EMP/Class-D Layers.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-02-15    Marlundg	File created
*
*******************************************************************************/

#include "stdafx.h"

#include <time.h>
#include "atc_util.hpp"

namespace ATC
{
  extern "C"
  {
    /******************************************************************************
    * Function:     getUTCTime
    * Description:  Wrapper functions for VFW calls in EMP/Class-D Layers.
    ******************************************************************************/
    void getUTCTime(uint32_t &timeUTC)
    {
      time_t secs;
      time(&secs);

      timeUTC = static_cast<uint32_t>(secs);
    }

    /******************************************************************************
    * absDiffU32
    ******************************************************************************/
    uint32_t absDiffU32(const uint32_t a, const uint32_t b)
    {
      if (a > b)
      {
        return a - b;
      }
      else
      {
        return b - a;
      }
    }

  }
}
