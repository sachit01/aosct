#ifndef RadioChannelCentral_hpp
#define RadioChannelCentral_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-11-10    keisele     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_sync.h>
#include <vfw_crc.h>
#include "atc_base.hpp"
#include <radio_channel.hpp>

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace RadioCom
  {

    /** RadioChannelCentral overrides the crc calculation  
    */
    class RadioChannelCentral : public RadioChannel
    {
    private:

      /** Initial value for CRC64 calculation
      *   Target does not support 'ULL', therefore a workaround with shift operation.
      */
      static const uint64_t crcInitValueCentral = (static_cast<uint64_t>(0xffffffffUL) << 32) + 0xffffffffUL;

    public:

      /** RadioChannelCentral constructor
      *
      *  @param[in] readChannelName  Name of vfw channel to read messages from
      *  @param[in] writeChannelName Name of vfw channel to write messages to
      *  @param[in] radChnlID Radio Channel ID
      *  @param[in] compShtName Short Name for this Channel
      */
      RadioChannelCentral(const char_t * const readChannelName, const char_t * const writeChannelName, const uint16_t radChnlID, const char_t * const compShtName);

      /**
      * Test the predefined output specified in FFFIS TCC-AOS 4.2.2 for string "123456789"
      *
      * @return true if pass
      */
      virtual bool selfTest(void);

      /** Check if radio-channel is associated with Central TCC
      *
      *  @return true if associated with central TCC
      */
      virtual bool isCentral() const;

      /** Init cross compare registers internal persistent values for cross-compare
      *
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * Default Constructor
      * Declare constructor as private in order to prevent illegal use.
      */
      RadioChannelCentral();

      /** Calculate Cyclic Redundancy Code for Central
      *
      *  @param[in] start  Pointer to start of buffer to calculate CRC of
      *  @param[in] length Length of buffer
      *  @return the CRC
      */
      virtual uint64_t calculateCRC(const uint8_t* start, uint16_t const length);

     

    };

  }

}

#endif
