/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file implements the Dispatcher/BHP specific part of the vfw-sim.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-02-23    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "atc_types.hpp"

/******************************************************************************
* vfwSimPreProcessIncomingMessage
******************************************************************************/
bool vfwSimPreProcessIncomingMessage(uint8_t channelDescr, uint8_t *inBuf, uint32_t &nrOfBytesToProcess, uint8_t *outBuf, uint32_t &outBufLen)
{
  // Do Nothing in Dispatcher built with vfw_sim

  return false;
}

/******************************************************************************
* vfwSimPostProcessOutgoingMessage
******************************************************************************/
bool vfwSimPostProcessOutgoingMessage(uint8_t channelDescr, uint8_t *buf, uint32_t &len)
{
  // Do Nothing in Dispatcher built with vfw_sim

  return false;
}
