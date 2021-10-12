#ifndef VFWSim_hpp
#define VFWSim_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This defines the basic structures for Vital Framework Simulator(VFW Sim)
* This class simulates the basic structures required for Vital framework.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-05-19    adgupta     Created
* 2016-06-20    spandita    updated with structure of sync,channel desc and timer
* 2016-07-04    spandita    updated with review comments
* 2016-01-03    spandita    Created the structure for sync timer
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
/** This implementation is not as per any requirement
 * Created for VFW sim implementation operation .
 */

 /**
 * \brief Vfw channel Structure
 * \param : desciptor
 */
struct vfw_ChannelDesc
{
  uint8_t descriptor; //!<descriptor for the channel
};

/**
* \brief   Vfw synch channel Structure
* \param : desciptor
*/
struct vfw_i_SyncChannel
{
  uint8_t descriptor; //!<descriptor for the sync channel
};

/**
* \brief Vfw timer Structure
* \param : Time out ,Start time and Stop time
*/
struct vfw_i_Timer
{
  int64_t time_out;   //!<time out of timer
  int64_t start_time; //!<Start time of timer
};

/**
* \brief Vfw Sync timer Structure
* \param : Time out ,Start time and Stop time
*/

struct vfw_i_SyncTimer
{
  uint64_t time_out;   //!<time out of timer
  uint64_t start_time; //!<Start time of timer
};



/**
* Max IP connections (This value needs to be updated if any new IP connections are added in ATP or Dispatcher)
*
*/
static const uint8_t ipConnectionMax = 35U;

// Added methods for pre/post processing of messages for vfwSim (not included in ordinary vfw)

/**
* Pre-Process a message buffer
*
* @param[in]      channelDescr        Channel descriptor for the incoming message
* @param[in]      *inBuf              Location where incoming message is located
* @param[in,out]  nrOfBytesToProcess  Number of bytes to process in inBuf, number of bytes left to process after call
* @param[out]     *outBuf             Location where preprocessed message shall be stored
* @param[out]     outBufLen           Length of preprocessed message in outBuf
* @return - true if preprocessing is done
*/
bool vfwSimPreProcessIncomingMessage(uint8_t channelDescr, uint8_t *inBuf, uint32_t &nrOfBytesToProcess, uint8_t *outBuf, uint32_t &outBufLen);

/**
* Post-Process a message buffer 
*
* @param[in]      channelDescr    Channel descriptor for the outgoing message
* @param[in,out]  *buf            Location where outgoing message is located, will be set to new position after post-processing
* @param[in,out]                  Length of outgoing message, will be set to new length after post-processing
* @return - true if post-processing is done
*/
bool vfwSimPostProcessOutgoingMessage(uint8_t channelDescr, uint8_t *buf, uint32_t &len);

#ifdef WIN32
/**
* Simulate start of cycle
*/
void simVfwStartCycle();
#endif

#endif
