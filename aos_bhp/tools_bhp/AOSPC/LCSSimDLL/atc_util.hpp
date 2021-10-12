#pragma once

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          atc_util.hpp %
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

#include "vfw_buffer.h"

/******************************************************************************
* Description:  Wrapper functions for VFW calls in EMP/Class-D Layers.
******************************************************************************/
namespace ATC
{
  extern "C"
  {
    void getUTCTime(uint32_t &timeUTC);
  }

}
