#ifndef BTMTelegram_hpp
#define BTMTelegram_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the BTM telegram sent from OPCAgent to AOS.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-31    arastogi    Created
* 2016-09-23    adgupta     Implementation for BTM Handler
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "btm_types.hpp"
#include "event.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace IO
  {

    /**
    * The class BtmTelegram defines the BTM telegram.
    *
    */
    class BtmTelegram
    {
    public:

      /**
      * Constructor.
      *
      */
      BtmTelegram();

      /**
      * Function to get the balise number of the message
      *
      * @return baliseNumber variable.
      */
      uint8_t getBaliseNumber() const;

      /**
      * Function to get the begin MVB time received in message
      *
      * @return beginMvbTime variable.
      */
      uint32_t getBeginMvbTime() const;

      /**
      * Function to get the end MVB time received in message
      *
      * @return endMvbTime variable.
      */
      uint32_t getEndMvbTime() const;

      /**
      * Function to get the begin time stamp in vfw reference
      *
      * @return beginTimeStamp variable.
      */
      int64_t getBeginTimeStamp() const;

      /**
      * Function to get the end time stamp in vfw reference
      *
      * @return endTimeStamp variable.
      */
      int64_t getEndTimeStamp() const;

      /**
      * Function to get final report flag
      *
      * @return finalReport value.
      */
      bool getFinalReport() const;

      /**
      * Function to unpack the BTM Status message.
      *
      * The data is unpacked from the structures of 5 ports.
      * Checks are performed to ensure the data extracted from all ports
      * is valid and all ports have the same sequence number.
      * If valid the class variables are updated.
      *
      * @param [in]  vfwParseBuffer Data from port 1-5 of MVB
      * @param [in]  sequenceNumber The sequence number received in the message
      * @param [out] telegramValid  Will indicate whether vfwParseBuffer contains a valid telegram 
      *
      * @return true if the unpacked message is unpacked without error, false otherwise.
      */
      bool unpack(VFW_Buffer& vfwParseBuffer, uint8_t& sequenceNumber, bool& telegramValid);

      /**
      * Function to get the the balise packet of this telegram
      *
      * @return The value of btmPacket variable.
      */
      const BtmTelegramPacket& getBTMTelegramPacket() const;

    private:

      /** Class type of a valid 210 telegram */
      static const uint8_t classValid210Telegram = 3U;

      /** Class type of a valid 830 telegram */
      static const uint8_t classValid830Telegram = 4U;

      /** Class type of a Balise Detect FSK */
      static const uint8_t classBaliseDetectFSK = 6U;

      /** BTM telegram port size plus */
      static const uint8_t btmTgmPortSize = 26U;

      /** BTM telegram port size safety bytes */
      static const uint8_t btmTgmPortSafetyDataSize = 6U;

      /**
      * The begin time received in the message.
      */
      uint32_t beginMvbTime;

      /**
      * The begin time converted to vfw reference
      */
      int64_t beginTimeStamp;

      /**
      * The end time received in the message.
      */
      uint32_t endMvbTime;

      /**
      * The end time converted to vfw reference
      */
      int64_t endTimeStamp;

      /**
      * The counter value from received message which increments on a balise passage.
      */
      uint8_t baliseNumber;

      /**
      * Flag final Report.
      */
      bool finalReport;

      /**
      * Telegram class
      */
      uint8_t telegramClass;

      /**
      * The balise packet retrieved from the data received.
      */
      BtmTelegramPacket btmPacket;

    };
  }
}

#endif
