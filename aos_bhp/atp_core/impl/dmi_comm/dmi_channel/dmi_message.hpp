#ifndef DMIMessage_hpp
#define DMIMessage_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*
*  DESCRIPTION:
*   The DMIMessage class is used by the DMIChannel and DMIHandler to represent one 
*   incoming or outgoing message.              
*
******************************************************************************/


/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 16-08-2016    adgupta     Created
* 29-08-2016    adgupta     Updates for implementation of functionalities for DMI Channel
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "event.hpp"
#include "atp_types.hpp"
#include "channel_config.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  /** 
  * The DMIMessage class represents one 
  * incoming or outgoing message.
  * The attributes are represented in host byte order 
  * with the exception of the variable data part which is 
  * represented in network byte order.
  */
  class DMIMessage
  {
    public:

    /**
    * Header Size value(MS+HT+MN+DL)
    */
    static const uint16_t headerSize = 5U;

    /**
    * Trailer Size value(CRC length)
    */
    static const uint16_t trailerSize = 4U;

    /** 
    * Constructor 
    * Initialize data length to 0
    * in case object is copied 
    */
    DMIMessage()
    {
      msgLen = 0U;
      headerType = 0U;
      msgNumber = 0U;
      crc = 0U;
      dmiData.msgType = 0U;
    }

    struct DMIData
    {
      /**
      * Message Type
      * Value should be between 1 - 127
      * Refer IF spec ATP-DMI.docx section 5.4 for details
      */
      uint8_t msgType;

      /** 
      * DMI message data.
      * Variable part of the telegram
      * The bytes shall appear in network order
      */
      uint8_t  msgData[ATC::maxDMIMessageSize];
    };

    /**
    * Header Type
    * 0x00 = Unacknowledged msg
    * 0x10 = Acknowledge msg
    * 0x80 = Acknowledge
    */
    uint8_t headerType;

    /**
    * Message Number
    * 0 for unacknowledged message
    * 1 to 255 for Acknowledge message or Acknowledge
    */
    uint8_t msgNumber;

    /**
    * Length of message
    */
    uint16_t msgLen;

    /**
    * The DMI data.
    */
    DMIData dmiData;

    /**
    * CRC-32 checksum of the data received/sent from/to DMI.
    */
    uint32_t crc;

  };
}

#endif
