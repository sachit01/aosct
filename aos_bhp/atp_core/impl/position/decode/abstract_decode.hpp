#ifndef AbstractDecode_hpp
#define AbstractDecode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file defines AbstractDecode class which contains the core decode logic
*  used by the AOS.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-23    ljoars      Created
* 2016-04-26    lantback    Use ATC::ProcComponent, init to return bool, corePtr()
* 2016-04-27    lantback    Corrected namespace of Decode
* 2016-06-02    arastogi    Moved it to position folder
* 2016-08-17    spandita    Added function for yard movement
*                           changed type odo pos from unsigned to signed
* 2016-08-22    spandita    Updated the code with review comments
* 2016-08-24    spandita    Updated the code with doxygen comments(review comment)
* 2016-09-12    saprasad    Declare events,variables for balise detection/decode component
* 2016-09-20    saprasad    Fixed the review comments
* 2016-09-29    saprasad    Remove the console call function prototype
* 2016 10-05    spandita    Restructured the class members
* 2016 10-19    saprasad    Redesign of Decode,remove test code and mode dependent code.
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "atp_types.hpp"
#include "abstract_odometry.hpp"
#include "abstract_btm_handler.hpp"
#include "abstract_config.hpp"
#include "atc_util.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace Pos
  {
    class AbstractDecode;
    /**
    * Static variable to store the single instance of AbstractDecode
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractDecode* coreDecodeInstancePtr = static_cast<AbstractDecode*>(NULL);

    // Note: Need to take care that 1st line below is the Brief description, while 2nd Line onwards is the detailed description.
    /**
    * The class AbstractDecode implements the interface defined by the ComponentBase class.
    * The AbstractDecode class implements the Core functionality of the Decode component. It is responsible for the processing of BTM packets from BTM handler
    * and making it available to other desired component. It also is responsible for the detection of Red Balise and Danger for Shunting.
    */
    class AbstractDecode : public ATC::ProcComponent
    {
    public:

      /**
      * Describes a balise group.
      */
      struct BaliseInfo
      {
        uint16_t nidBG; //!< Id of the balise group
        OdoPosition odometerPos; //!< Odometer position of the balise group
      };

      /**
      * Implements the virtual init function.
      *
      * @return True when initialization done.
      */
      virtual bool init(void);

      /**
      * Implements the virtual run function.
      */
      virtual void run(void);

      /**
      * Implements the getDangerForShunting function.
      *
      * @return true if Danger for Shunting message has been received in this cycles.
      */
      bool getDangerForShunting(void) const;

      /**
      * Retrieves the @ref BaliseInfo for the oldest balise in the queue, removes it
      * from the queue and returns true. Returns false if the queue is empty.
      *
      * @param[out] info  balise information object to copy to
      *
      * @return True if balise info was copied to 'info'.
      */
      bool getBaliseInformation(BaliseInfo &info);

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractDecode* corePtr();

    protected:

      /**
      * Constructor
      */
      AbstractDecode();

      /**
      * Determines whether a (nid_c,nid_bg) pair denotes a red balise.
      * In core false is always returned. Override in adaptation if red balise is used.
      *
      * @param[in] nid_c   country id
      * @param[in] nid_bg  balise group id
      *
      * @return false in the core implementation.
      */
      virtual bool isRedBalise(const uint16_t nid_c, const uint16_t nid_bg);

    private:

      /**
      * The data contained in a received balise telegram.
      */
      struct BaliseTelegram
      {
        /** Identity number of the country or region */
        uint16_t header_nid_c;

        /** Identity number of the balise group  */
        uint16_t header_nid_bg;

        /** Timestamp of the start of the main lobe */
        int64_t beginTimeStamp;

        /** Timestamp of the end of the main lobe */
        int64_t endTimeStamp;

        /** Aspect of danger of shunting */
        bool dangerForShunting;
      };

      /**
      * Parses a balise telegram received from BTM.
      *
      * @param[out] baliseTelegram  structure where the parsed balise information will be stored
      * @param[in]  btmTelegram  the received BTM telegram
      *
      * @return true if successful parsing is done else false.
      */
      bool parseTelegramPacket(BaliseTelegram& baliseTelegram, const ATP::IO::BtmTelegram& btmTelegram) const;

      /**
      * Updates position history to maintain history list
      */
      void updatePositionHistory();

      /**
      * Maintains history list for odometer time/position.

      */
      void updateHistoryDataList();

      /**
      * Maintains history for current positions to check for standstill condition.
      */
      void saveStandStillData();

      /**
      * Calculates odometer position of a balise using the begin and end timestamps.
      *
      * @param[in] beginTimeStamp  begin time stamp for received balise (reference time, ms)
      * @param[in] endTimeStamp    end time stamp for received balise (reference time, ms)
      *
      * @return Balise position if balise info is available else 0.
      */
      OdoPosition calculateOdometerPosition(const int64_t beginTimeStamp, const int64_t endTimeStamp) const;

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      void initCrossCompare() const;

      /**
      * Number of samples required in the standstill history
      */
      static const uint8_t standStillMaxCount = 10U;

      /**
      * Number of samples required in the time and position history
      */
      static const uint8_t odoHistoryMaxCount = 25U;

      /**
      * constant defined for delta change in Position
      */
      static const int32_t odoChangeDelta = 20;  //!< cm

      /**
      * Q_UPDOWN value for Up link telegram
      */
      static const uint8_t baliseQupdownInfo = 1U;

      /**
      * Minimum allowed M_VERSION value for ERTMS/ETCS language
      */
      static const uint8_t minBaliseMVersion = 0x10U;

      /**
      * Maximum allowed M_VERSION value for ERTMS/ETCS language
      */
      static const uint8_t maxBaliseMVersion = 0x11U;

      /**
      * Q_MEDIA value for the type of media (Balise)
      */
      static const uint8_t baliseQMedia = 0U;

      /**
      * L_PACKET value for the length of the "danger for shunting" packet
      */
      static const uint8_t dangerForShuntingPacketLength = 24U;

      /**
      * Packet header length (except for end packet)
      */
      static const uint8_t packetHeaderLength = 23U;

      /**
      * Q_ASPECT value for Danger for Shunting
      */
      static const uint8_t baliseInfoQAspect = 0U;

      /**
      * NID_PACKET value for marking the end of shunting packets
      */
      static const uint8_t baliseNidEndPackets = 255U;

      /**
      * NID_PACKET value for Danger for Shunting
      */
      static const uint8_t baliseShNidpacket = 132U;

      /**
      * Current speed when HistoryList need to be saved (cm/sec)
      */
      static const uint16_t historyListSaveSpeed = 200U;

      /**
      * Size of balise queue
      */
      static const uint16_t baliseQueueSize = 10U;

      /**
      * An event to report event when Balise output queue is not empty
      */
      const ATC::Event baliseOutputQueueNotEmpty;

      /**
      * An event to report when BTM time stamps are invalid
      */
      const ATC::Event timeStampError;

      /**
      * An event to report event when Red Balise is detected
      */
      const ATC::Event redBaliseDetected;

      /**
      * An event to report event when balise is detected in standstill condition
      */
      const ATC::Event baliseDetectedStandStill;

      /**
      * An event to report when an ignored balise packet is found
      */
      const ATC::Event ignoredBalisePacketFound;

      /**
      * bool for if red balise got detected
      */
      bool redBaliseDetectedFlag;

      /**
      * bool for if danger for shunting got detected
      */
      bool dangerForShunting;

      /**
      * value of ODO when last saved
      */
      int32_t lastOdoHistoryValue;

      /**
      * Current cycle count for run function since standstill
      */
      uint8_t curStandStillCount;

      /**
      * Counter value for Standstill condition
      */
      uint8_t historyCountStandStill;

      /**
      * Counter value for history list
      */
      uint8_t historyCounter;

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * Stores balise information. Objects are pushed to the end and popped from the beginning.
      */
      std::vector<BaliseInfo> vecBaliseInfo;

      /**
      * Position history for standStill condition
      */
      OdoPosition historyOdoStandStillPosition[standStillMaxCount];

      /**
      * Position history array
      */
      OdoPosition historyPosition[odoHistoryMaxCount];

      /**
      * Time history array (reference time, ms)
      */
      int64_t historyTime[odoHistoryMaxCount];
    };
  }
}

#endif
