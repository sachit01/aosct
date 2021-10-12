#pragma once

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          atc_math.hpp %
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
* 2019-02-14    Maralund	File created
*
*******************************************************************************/

#include "vfw_buffer.h"

/******************************************************************************
* Description:  Wrapper functions for VFW calls in EMP/Class-D Layers.
******************************************************************************/
namespace ATC
{
  class ATCMath;

  class ATCMath
  {
  public:
    /**
    * Calculates the absolute value of the difference between two unsigned 32-bit values.
    *
    * Note: std::abs() only supports signed values.
    *
    * @param[in] a  the first value
    * @param[in] b  the second value
    *
    * @return The absolute value of the difference between a and b
    */
    static uint32_t absDiff(const uint32_t a, const uint32_t b);
  };
}
