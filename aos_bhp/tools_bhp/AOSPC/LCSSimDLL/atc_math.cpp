/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          atc_math.cpp %
*
*  %version:       1 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2019-02-18 16:22 %
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
* 2019-02-18    Maralund	File created
*
*******************************************************************************/
#include "stdafx.h"
#include "atc_math.hpp"
#include "atc_types.hpp"

namespace ATC
{

  /******************************************************************************
  * absDiffU32
  ******************************************************************************/
  uint32_t ATCMath::absDiff(const uint32_t a, const uint32_t b)
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
