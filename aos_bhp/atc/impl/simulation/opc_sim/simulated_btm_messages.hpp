#ifndef Simulated_Btm_Messages_hpp
#define Simulated_Btm_Messages_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This defines the interface of simulated OPC agent
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-06    spandita     Created
* 2016-09-12    spandita     updated the structure with safety code
* 2016-09-15    spandita     Added const values for balise information
* 2016-09-26    spandita     Updated with review comment
* 2016-09-26    spandita     Renamed the Baliseinfo struct to simbaliseinfo
                             Moved constants to opc_sim.hpp
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <stdint.h>
#include "atp_types.hpp"

namespace ATC
{
  namespace Sim
  {
    /** Structure for storing next detected Balise and its related parameter
     *
     **/
    struct SimBaliseInfo
    {
      uint16_t nextExpectedBaliseId;          //!< Next Detected Balise id
      ATP::OdoPosition nextExpectedBaliseOdo; //!< Odo value at next detected Balise
      ATP::OdoPosition odoAtStartDistance;    //!< Odo at start of search balise distance
      ATP::OdoPosition odoAtEndDistance;      //!< Odo at end of search Balise Distance
    };

    /** Structure for status message data (sent from OPC to ATP)
    *
    **/
    struct StatusMessage
    {
      uint8_t enableTelegram : 8;         //!< To set which type of balise to read
      uint8_t safetyMode  : 8;            //!< To set the Safetymode test
      uint8_t bsa : 3;                    //!< BSA
      uint8_t bsaCounter : 8;             //!< BSA Counter
      uint8_t prelTestStatus : 3;         //!< Preliminary Test status
      uint8_t ifKActive : 1;              //!< K interface selection
      uint8_t usedIfBtmId : 2;            //!< antenna type
      uint8_t telePowerStatus : 2;        //!< Tele powering 
      uint8_t statusandOption : 8;        //!< status and option field
      uint8_t usedProtocolVersion : 8;    //!< protocol version
      uint8_t usedEuroloopCode : 4;       //!< used euro loop code
    };

    /** Structure for telegram message data from port 1  (sent from OPC to ATP)
    *
    **/
    struct TelegramMessage
    {
      uint8_t  sequenceNo;         //!< Sequence number
      uint8_t  telegramType;       //!< Class
      uint8_t  baliseNumber;       //!< Balise Number
      uint32_t beginTimeStamp;     //!<Begin time stamp
      uint32_t endTimeStamp;       //!< End time stamp
      uint32_t telegramTimeStamp;  //!< Telegram Time stamp
      uint8_t  finalReport;        //!< Final report
    };


    /** Structure for Simulated BTM Status (sent from OPC to ATP)
    *
    **/

    struct SimulatedBtmStatus
    {
      uint8_t status;        //!< Validity of the Message
      StatusMessage msgData; //!< Message data
    };
 
    /**
     * Structure for 26 byte vital data received by ATP
     */
    struct GP_26ByteVitalSinkDataA
    {
      uint8_t safetyData[26];
    };

    /** Euro-balise Telegram Parameter
    *
    **/
    const uint8_t headerQUpdown = 1U;  /** Header Q up down*/
    const uint8_t headerMVer = 16U;   /** version of ETCS language*/
    const uint8_t headerQMedia = 0U;  /** balise */
    const uint8_t headerNPig = 0U;  /** 0 the first balise in balise group*/
    const uint8_t headerNTotal = 0U;  /** 1 balise in group */
    const uint8_t headerMDup = 0U;  /** No duplicates*/
    const uint8_t headerMCount = 255U;  /** The telegram fits with all telegrams of the same balise group */
    const uint16_t headerNidC = 530U;  /** Region ID */
    const uint8_t headerQLink = 1U;  /** linked */
    const uint8_t shInfoNidPacket = 132U;  /** Danger for Shunting Information*/
    const uint8_t shInfoQDir = 2U;  /** Both directions */
    const uint8_t shInfoLPacket = 24U;
    const uint8_t shInfoQAspect = 0U;  /** stop */
    const uint8_t endInfoNidPacket = 255U;
  }
}

#endif
