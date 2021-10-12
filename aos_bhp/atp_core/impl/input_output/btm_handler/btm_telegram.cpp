/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the BtmTelegram class.
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

#include "btm_telegram.hpp"
#include <string.h>
#include <vfw_buffer.h>
#include "abstract_btm_handler.hpp"
#include "abstract_log_handler.hpp"
#include "atc_bit_unpacker.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace IO
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    BtmTelegram::BtmTelegram()
      :
      beginMvbTime(0U),
      beginTimeStamp(0),
      endMvbTime(0U),
      endTimeStamp(0),
      baliseNumber(0U),
      finalReport(false),
      telegramClass(0U)
    {
      memset(&btmPacket.packetData[0], 0, sizeof(btmPacket.packetData));
    }

    /******************************************************************************
    * getBaliseNumber
    ******************************************************************************/
    uint8_t BtmTelegram::getBaliseNumber() const
    {
      return baliseNumber;
    }

    /******************************************************************************
    * getBeginMvbTime
    ******************************************************************************/
    uint32_t BtmTelegram::getBeginMvbTime() const
    {
      return beginMvbTime;
    }

    /******************************************************************************
    * getEndMvbTime
    ******************************************************************************/
    uint32_t BtmTelegram::getEndMvbTime() const
    {
      return endMvbTime;
    }

    /******************************************************************************
    * getBeginTimeStamp
    ******************************************************************************/
    int64_t BtmTelegram::getBeginTimeStamp() const
    {
      return beginTimeStamp;
    }

    /******************************************************************************
    * getEndTimeStamp
    ******************************************************************************/
    int64_t BtmTelegram::getEndTimeStamp() const
    {
      return endTimeStamp;
    }

    /******************************************************************************
    * getFinalReport
    ******************************************************************************/
    bool BtmTelegram::getFinalReport() const
    {
      return finalReport;
    }

    /******************************************************************************
    * unpack
    ******************************************************************************/
    bool BtmTelegram::unpack(
      VFW_Buffer& vfwParseBuffer,
      uint8_t& sequenceNumber,
      bool& telegramValid)
    {
      uint8_t  btmTelegramPort;
      const uint8_t nidBtm = vfwGetU8(&vfwParseBuffer); // NID_BTM

      // BTM telegram message
      bool errorTelegram = (nidBtm != 1U);

      /** MVB port data stored before packing into BTM data packet */
      telegramValid = true;
      uint8_t telegramStatus = 0U;
      uint8_t nextExpectedSequenceNumber = sequenceNumber + 1U;

      uint8_t newTelegramCount = 0U;

      // Combine all telegram ports into one large balise telegram of total 104 bytes of data.
      VFW_Buffer totalBuffer;
      vfwInitBuffer(&totalBuffer, &btmPacket.packetData[0], sizeof(btmPacket.packetData));

      // Read all the ports
      for (btmTelegramPort = 0U; btmTelegramPort < numMvbPorts; ++btmTelegramPort)
      {
        const uint8_t qItemStatus = vfwGetU8(&vfwParseBuffer); // Q_ITEM_STATUS

        if (qItemStatus == itemSetBySource)
        {
          // Create a sub-buffer for the port data, this will be unpacked after we checked the telegramValid flag
          VFW_Buffer portBuffer;
          vfwInitSubBuffer(&portBuffer, &vfwParseBuffer, btmTgmPortSize);

          const bool telegramPortValid = (vfwGetU8(&vfwParseBuffer) == 1U);
          const uint8_t telegramPortStatus = vfwGetU8(&vfwParseBuffer);
          vfwConsumeBuffer(&vfwParseBuffer, 4U); // Safety time stamp (not used)

          if (telegramPortValid)
          {
            // See chapter 4.5.24.3 GP_26ByteVitalSinkDataA/B in 3NSS000250S0021
            // safteyData[26]   uint8
            // telegramValid    boolA (enum boolA { trueA = 1, falseA = 0 } )
            // telegramStatus   tios_FailureInformationA
            // safetyTimeStamp  uint32

            const uint8_t newSequenceNumber = vfwGetU8(&portBuffer);

            if (newSequenceNumber == sequenceNumber)
            {
              // Telegram OK, but no new sequence number...
            }
            else if (newSequenceNumber == nextExpectedSequenceNumber)
            {
              // Telegram OK
              ++newTelegramCount;
            }
            else
            {
              errorTelegram = true; // Missed sequence number!
            }

            if (btmTelegramPort == 0U)
            {
              telegramClass = vfwGetU8(&portBuffer);

              ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::VeryDetailedLog, "sequenceNumber :",
                static_cast<uint32_t>(newSequenceNumber), "BTM");
              ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::VeryDetailedLog, "telegramClass :",
                static_cast<uint32_t>(telegramClass), "BTM");

              beginMvbTime = vfwGetU32(&portBuffer);
              endMvbTime = vfwGetU32(&portBuffer);

              // Calculate time stamp from MVB to our time offset...
              const int32_t mvbReferenceTimeOffset = AbstractBTMHandler::corePtr()->getOpcReferenceTimeOffset();
              beginTimeStamp = static_cast<int64_t>(beginMvbTime) - mvbReferenceTimeOffset;
              endTimeStamp = static_cast<int64_t>(endMvbTime) - mvbReferenceTimeOffset;

              vfwConsumeBuffer(&portBuffer, 4U); // Telegram time stamp
              baliseNumber = vfwGetU8(&portBuffer); // 8 bit for balise number

              vfwConsumeBuffer(&portBuffer, 2U); // Skip 16 bits...
                                                     // 4 unused bits left
              const uint8_t finalData = vfwGetU8(&portBuffer);
              finalReport = ((finalData & 0x8U) == 0x8U); // bit-3 for final report
              vfwConsumeBuffer(&portBuffer, 4U); // Remove unused

              ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::VeryDetailedLog, "baliseNumber :", static_cast<uint32_t>(baliseNumber), "BTM");
              ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::VeryDetailedLog, "finalReport :", static_cast<uint32_t>(finalReport), "BTM");

              vfwCpyBuffer(&totalBuffer, &portBuffer, 4U); // Balise information bits, bit 0 ... 31 (4 Bytes)

              if ((telegramClass != classValid830Telegram) &&
                (telegramClass != classValid210Telegram) &&
                (telegramClass != classBaliseDetectFSK))
              {
                errorTelegram = true;
              }
            }
            else
            {
              vfwCpyBuffer(&totalBuffer, &portBuffer, 25U); // Balise information bits, bit 32 ... 231. (25 Bytes)
            }
          }
          else
          {
            telegramValid = false;
            telegramStatus = telegramPortStatus;
          }
        }
        else
        {
          // No new data, just consume...
          vfwConsumeBuffer(&vfwParseBuffer, btmTgmPortSize + btmTgmPortSafetyDataSize);
          telegramValid = false;
        }
      }// End of for(btmTelegramPort)

      if (vfwGetValidSize(&vfwParseBuffer) != 0U)
      {
        errorTelegram = true;
        AbstractBTMHandler::corePtr()->getTrace()->write(ATC::briefTrace, "BTM tel size error");
      }

      if (!telegramValid)
      {
        ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "TelegramValid value is false, status: ",
          static_cast<int32_t>(telegramStatus), "BTM");
      }

      if (newTelegramCount == numMvbPorts)
      {
        sequenceNumber = nextExpectedSequenceNumber;
        AbstractBTMHandler::corePtr()->getTrace()->write(ATC::briefTrace, "New telegram", static_cast<uint32_t>(nextExpectedSequenceNumber));

        if (finalReport && (telegramClass == classBaliseDetectFSK))
        {
          errorTelegram = true;
        }
      }

      return !errorTelegram;
    }

    /******************************************************************************
    * getBTMTelegramPacket
    ******************************************************************************/
    const BtmTelegramPacket& BtmTelegram::getBTMTelegramPacket() const
    {
      return btmPacket;
    }
  }
}
