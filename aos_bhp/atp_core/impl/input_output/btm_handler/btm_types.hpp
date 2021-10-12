#ifndef btmTypes_hpp
#define btmTypes_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*
* DESCRIPTION:
*
* The BTM Types declares common types used by BTM Handler component
*
*****************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-31    arastogi    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <stdint.h>

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace IO
  {
    /**
    * Enum for TIOS Failure Information A
    */
    enum TIOS_FailureInformationA
    {
      NoFailureA               = 0,
      TemporaryDisturbanceA    = 1,
      DisturbanceLevelTooHighA = 2,
      ErrorInSequenceA         = 3,
      ErrorInTimeA             = 4,
      UndefinedFailureA        = 5
    };

    /**
    * Structure for 10 byte vital data generated from ATP
    */
    struct GP_10ByteVitalSourceDataA
    {
      uint8_t safetyData[10];   /**<GP_10ByteVitalSourceDataA::safetyData*/
    };

    /**
    * Structure for 32 byte vital data generated from ATP
    */
    struct GP_32ByteVitalSourceDataA
    {
      uint8_t safetyData[32];   /**<GP_32ByteVitalSourceDataA::safetyData*/
    };

    /**
    * Structure for 10 byte vital data received by ATP
    */
    struct GP_10ByteVitalSinkDataA
    {
      uint8_t safetyData[10];   /**<GP_10ByteVitalSinkDataA::safetyData*/
    };

    /**
    * Structure for 104 byte Balise information received in Balise telegram
    * The Balise telegrams are defined according to the Eurobalise standard described by [ERTMS] in chapter 7 and 8.
    */
    struct BtmTelegramPacket
    {
      uint8_t packetData[104];   /**<BtmTelegramPacket::packetData*/
    };

    /**
    * BSA Balise Service Available
    */
    const uint8_t bsaBaliseServiceAvailable = 0U;

    /**
    * BSA BTM start up or routine test is in progress
    */
    const uint8_t bsaBtmStartUpOrRoutineTestInProgress = 1U;

    /**
    * BSA sporadic failure.
    */
    const uint8_t bsaSporadicFailure = 2U;

    /**
    * BSA permanent failure.
    */
    const uint8_t bsaPermamnentFailure = 3U;

    /** ATPCU Protocol version. */
    const uint8_t btmProtocolVersion = 0x08U;   // As described in 3NSS010889D0108 Version 2.3

    /** Preliminary BTM Status Test in progress : This is the initial state when the test becomes active after being disabled or unavailable*/
    const uint8_t btmPreliminaryBtmStatusTestInProgress = 0x03U;

    /** Preliminary BTM Status Green: The test indicates that the BTM + antenna are likely to give full performance when the train starts rolling.*/
    const uint8_t btmPreliminaryBtmStatusGreen = 0x04U;

    /**
    * 0:Enable/Disable Telegram 830 bits
    * 1:Enable/Disable Telegram 210 bits
    * 2:Enable/Disable Telegram 180 bits var 11
    * 3:Enable/Disable Telegram 180 bits var 22
    * 4:Enable/Disable Telegram 700 bits
    * 5:Enable/Disable Euroloop bits
    * 6:Unused
    * 7:Unused
    */
    const uint8_t telegramFormat = 0x03U; // Enable 830 and 210 telegram only

    /** Number of MVB ports from where data will be received. */
    const uint8_t numMvbPorts = 5U;

    /** Item set by source value for Q_ITEM_STATUS */
    static const uint8_t itemSetBySource = 1U;
  }
}

#endif
