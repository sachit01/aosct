/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file implements the abstract (core) decode component class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-23    ljoars      Created
* 2016-04-26    lantback    Use ATC::ProcComponent, init to return bool
*                           Bugg corrections, namespace
* 2016-04-27    lantback    Corrected namespace of Decode
* 2016-06-02    arastogi    Moved it to position folder
* 2016-08-17    spandita    Added function for yard movement
* 2016-08-23    spandita    Removed the function for yardmovement
* 2016-08-24    spandita    Updated the code with doxygen comments(review comment)
* 2016-09-12    saprasad    Implemented all the function of decode as per updated SCDS
* 2016-09-20    saprasad    Fixed the review  comments
* 2016-09-20    saprasad    Remove the console call & end packet 255 check after review comment
* 2016 10-05    spandita    Bugfix for Danger for shunting and lint fix
* 2016 10-19    saprasad    Redesign of Deocode,remove test code and mode dependent code.
* 2016 10-19    saprasad    Added Standstill functionality as per redesign SCDS.
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_btm_handler.hpp"
#include "abstract_config.hpp"
#include "abstract_decode.hpp"
#include "abstract_odometry.hpp"
#include "atc_math.hpp"
#include "atc_util.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_array.hpp"
#include "cross_compare_complex.hpp"
#include "atc_bit_unpacker.hpp"
#include "abstract_decode_event_ids.hpp"

#include <vfw_checkpoints.h>

#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <cstdio>
#include <vfw_time.h>
#endif

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
  namespace Pos
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractDecode::AbstractDecode() : ATC::ProcComponent(atpDecodeId, "Decode", "DEC"),
      // creating different set of objects for different type of events
      baliseOutputQueueNotEmpty(ATC::Event::createSafeBrakeSBEvent(atpDecodeId, ATC::CoreContainer, eventIdReportEventBaliseQuenNotEmpty,
        ATC::NoSB, DMICom::decodeBalQueNotEmpty, "Balise queue is not empty ")),
      timeStampError(ATC::Event::createSafeBrakeSBEvent(atpDecodeId, ATC::CoreContainer, eventIdReportEventTimeStampError,
        ATC::NoSB,  DMICom::decodeTachErr, "BTM time stamp error (running >49 days?)")),
      redBaliseDetected(ATC::Event::createSBReqEvent(atpDecodeId, ATC::CoreContainer, eventIdReportEventRedBalise,
        ATC::NoSB, DMICom::redBaliseDetected, "Red Balise is detected")),
      baliseDetectedStandStill(ATC::Event::createSafetyHaltEvent(atpDecodeId, ATC::CoreContainer, eventIdReportEventStandStill,
        ATC::NoEB,  DMICom::decodeStandStill, "Balise detected in Standstill condition.")),
      ignoredBalisePacketFound(ATC::Event::createLogEvent(atpDecodeId, ATC::CoreContainer, eventIdReportEventUnknownPacket,
        DMICom::noDmi, "Unknown packet in balise found.", true)),
      redBaliseDetectedFlag(false),
      dangerForShunting(false),
      lastOdoHistoryValue(0),
      curStandStillCount(0U),
      historyCountStandStill(0U),
      historyCounter(0U),
      initDone(false)
    {
      if (coreDecodeInstancePtr != 0)
      {
        // Error to event handler
        ATC::aosHalt(__FILE__, __LINE__, "Decode constructor already instantiated");
      }

      memset(&historyPosition[0], 0x00, sizeof(historyPosition));
      memset(&historyTime[0], 0x00, sizeof(historyTime));
      memset(&historyOdoStandStillPosition[0], 0x00, sizeof(historyOdoStandStillPosition));

      // Setup single instance pointer for core access
      coreDecodeInstancePtr = this;
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractDecode::init(void)
    {
      if (!initDone)
      {
        vecBaliseInfo.reserve(baliseQueueSize);
        initCrossCompare();
        initDone = true;
      }

      return initDone;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void AbstractDecode::run(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "DEC_run");

      // dangerForShunting make it false as there is no telegram detected as of now  
      dangerForShunting = false;

      //check the size of Vector Balise Queue
      if (vecBaliseInfo.size() != 0U)
      {
        //If the output queue is not empty ,then baliseOutputQueueNotEmpty raised 
        ATC::AbstractEventHandler::corePtr()->reportEvent(baliseOutputQueueNotEmpty, __FILE__,
          __LINE__);
      }
      else
      {
        updatePositionHistory();

        // BTM Telegram Object which will get BTM telegram information
        IO::BtmTelegram btmTelegramObj;

        //check  Whether Balise telegrams available or not    
        while (IO::AbstractBTMHandler::corePtr()->getBTMTelegram(btmTelegramObj))
        {
          const OdoPosition  curOdoPos = Pos::AbstractOdometry::corePtr()->getOdoPosition();
          const bool standStillCondition = (curOdoPos == historyOdoStandStillPosition[0]) && (historyCountStandStill == standStillMaxCount);

          // Checking for standstill condition, compare the last Odo Position value with Oldest Value stored in History Position
          if (standStillCondition)
          {
            //Generate a fatal event
            ATC::AbstractEventHandler::corePtr()->reportEvent(baliseDetectedStandStill, __FILE__,
              __LINE__);
          }// end of "if" for standstill condition 
          else
          {
            //BaliseTelegram variable describes Balise header
            BaliseTelegram  baliseTelegram;

            // Parse the telegram Packets if the Balise telegram is detected
            bool bRetVal = parseTelegramPacket(baliseTelegram, btmTelegramObj);
            if (!bRetVal)
            {
              trace.write(1U, "Parsing Telegram Packet error!");
              writeToLog(ATC::BriefLog, "Parsing Telegram Packet error!", __FILE__, __LINE__);
            }
            else
            {
              const uint16_t cfgCountryCodeNID_C = AbstractConfig::corePtr()->getBalNidC();

              // checking Red balise detection condition  
              if (isRedBalise(baliseTelegram.header_nid_c, baliseTelegram.header_nid_bg))
              {
                redBaliseDetectedFlag = true;
              }// end of if statement for red balise detection
              else if (cfgCountryCodeNID_C == baliseTelegram.header_nid_c)
              {
                // Checking for Danger for shunting condition  
                if (baliseTelegram.dangerForShunting)
                {
                  // Set danger for shunting variable ,trace and logging of it 
                  dangerForShunting = true;
                  trace.write(1U, "Danger for shunting condition !!!");
                  writeToLog(ATC::BriefLog, "Danger for shunting !!", __FILE__, __LINE__);
                }

                // Calculate balise position. 
                const OdoPosition odoMeterVal = calculateOdometerPosition(baliseTelegram.beginTimeStamp, baliseTelegram.endTimeStamp);

                // Push the odo data and nidBG in BaliseInfo vector queue
                BaliseInfo stBalisObj;
                stBalisObj.nidBG = baliseTelegram.header_nid_bg;
                stBalisObj.odometerPos = odoMeterVal;
                vecBaliseInfo.push_back(stBalisObj);

                //TODO Zero Lobe condition in flyspray
              }
              else
              {
                // TODO: Do something here?
                writeToLog(ATC::BriefLog, "Found balise with country id",
                  static_cast<uint32_t>(baliseTelegram.header_nid_c), __FILE__, __LINE__);
                writeToLog(ATC::BriefLog, "expected country id",
                  static_cast<uint32_t>(cfgCountryCodeNID_C), __FILE__, __LINE__);
              }
            }

          }// end of "else" for standstill condition 

        }//end of while loop for new telegram message from BTM handler

      }//end of "else" when output vector queue is not empty

      //Increment the curStandStillCount by 1 in each cycle for Standstill condition 
      curStandStillCount++;

      // Maintain History Position for StandStill Condition 
      if (standStillMaxCount == curStandStillCount)
      {
        //start storing the odo values reading
        saveStandStillData();
        //reset to zero as cycle reached maximum value
        curStandStillCount = 0U;
      }

      if (redBaliseDetectedFlag)
      {
        if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
        {
          redBaliseDetectedFlag = false;
        }
        else
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(redBaliseDetected, __FILE__, __LINE__);
        }
      }
    }

    /******************************************************************************
    * getDangerForShunting
    ******************************************************************************/
    bool AbstractDecode::getDangerForShunting(void) const
    {
      return dangerForShunting;// return danger for shunting variable 
    }

    /******************************************************************************
    * getBaliseInformation
    ******************************************************************************/
    bool AbstractDecode::getBaliseInformation(BaliseInfo &info)
    {
      // bResult give the Vector dequeue operations result
      bool bResult = false;
      if (vecBaliseInfo.size() == 0U)
      {
        info.nidBG = 0U;
        info.odometerPos = 0;
        bResult = false;
      }//end of if
      else
      {
        // Dequeue the balise information from vector BaliseInfo
        BaliseInfo objBaliseInfo = vecBaliseInfo.front();
        info.nidBG = objBaliseInfo.nidBG;
        info.odometerPos = objBaliseInfo.odometerPos;
        static_cast<void>(vecBaliseInfo.erase(vecBaliseInfo.begin()));//to avoid lint
        bResult = true;

      }// end of else for dequeue operation of vector
      return bResult;
    }

    /******************************************************************************
    * updatePositionHistory
    ******************************************************************************/
    void AbstractDecode::updatePositionHistory()
    {
      // gives the current speed which will used for checking condition to maintain History List
      const uint16_t curSpeed = Pos::AbstractOdometry::corePtr()->getSpeed();
      // gives current OdoPosition which will used for checking condition to maintain History List
      const OdoPosition  curOdoPos = Pos::AbstractOdometry::corePtr()->getOdoPosition();

      const int32_t odoChangeDistance = ATC::ATCMath::instance().absolute(curOdoPos - lastOdoHistoryValue, __FILE__, __LINE__);

      if (((odoChangeDistance >= odoChangeDelta) && (historyListSaveSpeed > curSpeed))  ||
          (historyListSaveSpeed <= curSpeed))
      {
        updateHistoryDataList();
      }
    }

    /******************************************************************************
    * updateHistoryDataList
    ******************************************************************************/
    void AbstractDecode::updateHistoryDataList()
    {
      // Get the Odo position for maintaining History List 
      lastOdoHistoryValue = Pos::AbstractOdometry::corePtr()->getOdoPosition();

      // Get the odometer timestamp for maintaining History List
      const int64_t lastOdoHistoryTickTime = static_cast<int64_t>(Pos::AbstractOdometry::corePtr()->getOdoTimeStamp());
      // If historyCounter History List is full, move all entries one step forward
      if (odoHistoryMaxCount <= historyCounter)
      {
        for (uint8_t counter = 1U; counter < odoHistoryMaxCount; counter++)
        {
          //Shift array element to left when it reaches it's upper limit
          historyPosition[counter - 1U] = historyPosition[counter];
          historyTime[counter - 1U] = historyTime[counter];
        }
        historyCounter = (odoHistoryMaxCount - 1U);
      }
      // Save the current position and timestamp in History List
      historyPosition[historyCounter] = lastOdoHistoryValue;
      // Save the Odo position in History List
      historyTime[historyCounter] = lastOdoHistoryTickTime;
      // Increment the history list counter
      historyCounter++;
    }

    /******************************************************************************
    * saveStandStillData
    ******************************************************************************/
    void AbstractDecode::saveStandStillData()
    {
      // Get the current Odo position for maintaining History List for position 
      const OdoPosition curPos = Pos::AbstractOdometry::corePtr()->getOdoPosition();

      // If historyCountStandStill is greater than or equal to standStillMaxCount start maintaining Odometer reading
      if (standStillMaxCount <= historyCountStandStill)
      {
        for (uint8_t counter = 1U; counter < standStillMaxCount; counter++)
        {
          // Shift array element to left when it reaches it's upper limit
          historyOdoStandStillPosition[counter - 1U] = historyOdoStandStillPosition[counter];
        }
        historyCountStandStill = (standStillMaxCount - 1U);
      }

      // Save the Odo position in History List
      historyOdoStandStillPosition[historyCountStandStill] = curPos;
      // Increment the history list counter
      historyCountStandStill++;
    }

    /****************************************************************************
    * calculateOdometerPosition
    ****************************************************************************/
    OdoPosition AbstractDecode::calculateOdometerPosition(const int64_t beginTimeStamp, const int64_t endTimeStamp) const
    {
      // Convert a passed timeValue to a odometer value using interpolation with the formula :
      // *Yx = Y(n - 1) + ((Y(n) - Y(n - 1))*(Tx - T(n - 1))
      //  ----------------------------
      //   (T(n) - T(n - 1))
      //  * Yx = odometer
      //  * tx = baliseTimeStamp
      //  * Y(n) - Y(n - 1) = diffOdoValues
      //  * T(n) - T(n - 1) = diffTimeStamps

      bool  found = false;

      // The calculated balise position returned by this function
      OdoPosition odometer = 0;
      // Use endTimeStamp and beginTimeStamp timestamps received from BTM to find the middle of the balise
      const int64_t baliseTimeStamp = ((endTimeStamp - beginTimeStamp) / 2) + beginTimeStamp;

      // Check if baliseTimeStamp is less than HistoryList oldest value
      if (baliseTimeStamp < historyTime[0])
      {
        // Generate safe brake event
        ATC::AbstractEventHandler::corePtr()->reportEvent(timeStampError, __FILE__, __LINE__);
        writeToLog(ATC::BriefLog, "calculateOdometerPosition: BTM time stamp error (running >49 days?)", __FILE__, __LINE__);
      }
      // Check if baliseTimeStamp is found in the History List
      else
      {
        uint8_t counter;
        // Scan History list between two intermediate values 
        for (counter = 1U; (counter < historyCounter); ++counter)
        {
          if ((baliseTimeStamp >= historyTime[counter - 1U]) &&
            (baliseTimeStamp <= historyTime[counter]))
          {
            // If baliseTimeStamp is found between two intermediate values then set "found"
            found = true;
            break;
          }
        }// end of "for" loop 

        // If baliseTimeStamp is found, do the odo calculation
        if (found)
        {
          // Use linear interpolation to get the odometer position for the balise timestamp
          const OdoPosition diffTimeStamps = static_cast<OdoPosition>(historyTime[counter] - historyTime[counter - 1U]);
          // Interpolate calculations between samples for Odo Position
          const OdoPosition diffOdoValues = (historyPosition[counter] - historyPosition[counter - 1U]);
          const OdoPosition odoTimex = static_cast<OdoPosition>(baliseTimeStamp - historyTime[counter - 1U]);
          // calculate balise position
          odometer = historyPosition[counter - 1U];
          odometer += ((diffOdoValues*odoTimex) / diffTimeStamps);

          char_t bufferInfo[300];
          //lint -e{586} snprintf is needed here
          const int32_t res = snprintf(&bufferInfo[0], sizeof(bufferInfo), "calculateOdometerPosition odo = %d, %lld %lld\n",
            odometer, beginTimeStamp, endTimeStamp);

          if ((res > 0) && (static_cast<size_t>(res) < sizeof(bufferInfo)))
          {
            trace.write(1U, &bufferInfo[0]);
          }
        }
        // This is  the case when the baliseTimeStamp is not found in the history list or value is later than the last save value
        else
        {
          // Use the last Odo value as balise position 
          odometer = lastOdoHistoryValue;
          char_t bufferInfo[300];
          //lint -e{586} snprintf is needed here
          const int32_t res = snprintf(&bufferInfo[0], sizeof(bufferInfo), "calculateOdometerPosition, using lastOdo, odo = %d, %lld %lld\n",
            odometer, beginTimeStamp, endTimeStamp);

          if ((res > 0) && (static_cast<size_t>(res) < sizeof(bufferInfo)))
          {
            trace.write(1U, &bufferInfo[0]);
          }
        }
      }

      return odometer;
    }

    /****************************************************************************
    * parseTelegramPacket
    ****************************************************************************/
    bool AbstractDecode::parseTelegramPacket(BaliseTelegram& baliseTelegram, const ATP::IO::BtmTelegram& btmTelegram) const
    {
      // True if the parsing of telegram packet is done successfully
      bool resultPacket = true;
      // packetTelegram gives the BTM Telegram packets of 104 bytes (uint8_t packetData[104])

      //Reset the flag for shunting integration
      baliseTelegram.dangerForShunting = false;

      const IO::BtmTelegramPacket& packetTelegram = btmTelegram.getBTMTelegramPacket();

      ATC::BitUnpacker btmTelegramUnpacker(&packetTelegram.packetData[0], sizeof(packetTelegram.packetData));

      const uint8_t qUpdown = btmTelegramUnpacker.unpack8(1U); // Q_UPDOWN
      if (qUpdown != baliseQupdownInfo)
      {
        // Trace the Telegram header If it is not valid 
        trace.write(1U, "Balise Header Error!");
        writeToLog(ATC::BriefLog, "Balise Header Error Q_UPDOWN!", static_cast<uint32_t>(qUpdown), __FILE__, __LINE__);
        resultPacket = false;
      }

      const uint8_t mVersion = btmTelegramUnpacker.unpack8(7U); // M_VERSION
      if ((mVersion < minBaliseMVersion) || (mVersion > maxBaliseMVersion))
      {
        writeToLog(ATC::BriefLog, "Balise Header Error M_VERSION!", static_cast<uint32_t>(mVersion), __FILE__, __LINE__);
        resultPacket = false;
      }

      const uint8_t qMedia = btmTelegramUnpacker.unpack8(1U); // Q_MEDIA
      if (qMedia != baliseQMedia)
      {
        writeToLog(ATC::BriefLog, "Balise Header Error Q_MEDIA!", static_cast<uint32_t>(qMedia), __FILE__, __LINE__);
        resultPacket = false;
      }

      // Discard the unused bits that we should not validate
      btmTelegramUnpacker.skip(3U + 3U + 2U + 8U); // N_PIG, N_TOTAL, M_DUP, M_MCOUNT

      // Unpack NID_C field from the telegram packet
      baliseTelegram.header_nid_c = static_cast<uint16_t>(btmTelegramUnpacker.unpack32(10U)); 
      trace.write(ATC::detailedTrace, "NID_C = ", static_cast<int32_t>(baliseTelegram.header_nid_c));
      // Unpack the NID_BG field from telegram packet
      baliseTelegram.header_nid_bg = static_cast<uint16_t>(btmTelegramUnpacker.unpack32(14U));
      trace.write(ATC::detailedTrace, "NID_BG = ", static_cast<int32_t>(baliseTelegram.header_nid_bg));

      // Discard Q_LINK that we should not validate in the header
      btmTelegramUnpacker.skip(1U);  // Q_LINK
      
      bool stopHandlingPackets = false;

      // Check if balise group id is valid, 0 is not a valid value.
      if (baliseTelegram.header_nid_bg == 0U)
      {
        writeToLog(ATC::BriefLog, "Balise Header Error: NID_BG = 0", __FILE__, __LINE__);
        resultPacket = false;
      }

      while (!stopHandlingPackets)
      {
        // Unpack the NID_PACKET field from telegram packets
        const uint8_t nidPacket = btmTelegramUnpacker.unpack8(8U);  // NID_PACKET

        if (btmTelegramUnpacker.getStatus() != ATC::BitUnpacker::OK_STATUS)
        {
          stopHandlingPackets = true;
          resultPacket = false;
        }
        else if (baliseNidEndPackets == nidPacket)
        {
          // End packet, no more data present...
          stopHandlingPackets = true;
        }
        else if (btmTelegramUnpacker.getNumberOfRemainingBits() > packetHeaderLength) // It must be > since an end-packet must be present
        {
          // Handle the packet found
          // -| Q_DIR | 2 | -
          // -| L_PACKET | 13 | -
          btmTelegramUnpacker.skip(2U);  // Q_DIR, ignored

          const uint16_t lPacket = static_cast<uint16_t>(btmTelegramUnpacker.unpack32(13U)); // L_PACKET

          // Check packet length if too short
          if (lPacket < packetHeaderLength)
          {
            stopHandlingPackets = true;
            resultPacket = false;
          }
          else if (baliseShNidpacket == nidPacket)
          {
            // Danger for shunting packet
            if (lPacket != dangerForShuntingPacketLength)
            {
              writeToLog(ATC::BriefLog, "Balise Header Error L_PACKET!", static_cast<uint32_t>(lPacket), __FILE__, __LINE__);
              resultPacket = false;
              stopHandlingPackets = true;
            }

            // -| Q_ASPECT | 1 | = 0 (stop if in SH mode)
            // Unpack the sh_info_q_aspect from telegram packet
            const uint8_t qAspect = btmTelegramUnpacker.unpack8(1U); // Q_ASPECT

            if (qAspect != baliseInfoQAspect)
            {
              baliseTelegram.dangerForShunting = false;
              writeToLog(ATC::BriefLog, "Balise Header Error Q_ASPECT!", static_cast<uint32_t>(qAspect), __FILE__, __LINE__);
              resultPacket = false;
            }
            else
            {
              // This is the only valid value in our project
              baliseTelegram.dangerForShunting = true;
            }

            trace.write(ATC::detailedTrace, "Danger for shunting = ", static_cast<int32_t>(baliseTelegram.dangerForShunting));
          }
          else
          {
            // Ignore the packet, just jump forward the remaining bits in the packet
            btmTelegramUnpacker.skip(lPacket - packetHeaderLength);

            // Send an event to inform about the packet...
            ignoredBalisePacketFound.setDynamicText(nidPacket);
            ATC::AbstractEventHandler::corePtr()->reportEvent(ignoredBalisePacketFound, __FILE__,
              __LINE__);
          }
        }
        else
        {
          // Incorrect format of the balise, lengths are not correct!
          resultPacket = false;
          stopHandlingPackets = true;
        }
      }

      baliseTelegram.beginTimeStamp = btmTelegram.getBeginTimeStamp();
      baliseTelegram.endTimeStamp = btmTelegram.getEndTimeStamp();

      return resultPacket; // return the result of parse telegram packet
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractDecode* AbstractDecode::corePtr(void)
    {
      return coreDecodeInstancePtr;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractDecode::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&redBaliseDetectedFlag));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&dangerForShunting));

      crossCompare->addCrossCompareData(new Support::CrossCompareInt32(&lastOdoHistoryValue));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&curStandStillCount));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&historyCountStandStill));

      crossCompare->addCrossCompareData(new Support::CrossCompareArray<OdoPosition>(&historyOdoStandStillPosition[0], standStillMaxCount));
      crossCompare->addCrossCompareData(new Support::CrossCompareArray<OdoPosition>(&historyPosition[0], odoHistoryMaxCount));
      crossCompare->addCrossCompareData(new Support::CrossCompareArray<int64_t>(&historyTime[0], odoHistoryMaxCount));
      
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&historyCounter));

      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&baliseOutputQueueNotEmpty));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&timeStampError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&redBaliseDetected));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&baliseDetectedStandStill));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&ignoredBalisePacketFound));

      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&initDone));
    }

    /******************************************************************************
    * isRedBalise
    ******************************************************************************/
    bool AbstractDecode::isRedBalise(const uint16_t /* nid_c */, const uint16_t /* nid_bg */)
    {
      return false;
    }

  }
}

