#ifndef SPLHandler_hpp
#define SPLHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The SPL Handler component deals with the interface between the OPC Agent and the AOS SW.
*  AbstractSPLHandler implements the core functionality of the component.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-26    rquensel    Created for SPL Handler
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include <vfw_buffer.h>
#include <vfw_sync.h>
#include "btm_types.hpp"
#include "cross_compare_output_channel.hpp"
#include "channel_config.hpp"
#include "event.hpp"

extern "C"
{
#include "spl_api_interface.h"

}

/******************************************************************************
* Profibus_Type, it is a part of the SPL interface.
* It must look like this, be global and cannot be put in a namespace.
******************************************************************************/
//lint -e{1960} Needed as glue towards the SPL library
//lint -esym(768,Profibus_Type::recvAddress) Needed as glue towards the SPL library
//lint -esym(768,Profibus_Type::sendAddress) Needed as glue towards the SPL library
/**
* struct Profibus_Type  Needed as glue towards the SPL library
*/
typedef struct
{
  uint8_t SAP_no;             /**<Profibus_Type::SAP_no*/
  uint8_t recvAddress;        /**<Profibus_Type::recvAddress*/
  uint8_t sendAddress;        /**<Profibus_Type::sendAddress*/
  uint32_t timeStamp;         /**<Profibus_Type::timeStamp*/
  uint8_t Profibus_Data[300]; /**<Profibus_Type::Profibus_Data*/
} Profibus_Type; //lint !e1960 Needed as glue towards the SPL library


namespace ATP
{
  namespace IO
  {
    /**
    * Max number of bytes we can put in a SPL packet
    */
    const uint32_t maxSplDataSize = 242U;
    //lint -esym(551,ATP::IO::maxSplDataSize) Lint is wrong, this constant *is* used

    /**
    * Application Status is running
    */
    const uint8_t appStatusIdle = 0U;

    /**
    * OPC version Reply ID
    */
    const uint8_t opcVersionReplyId = 6U;

    /**
    * Sender dynamic transfer time as described in subset 056, 4.4.10.2, in ms, default = 50
    */
    const int32_t  senderDynamicTransferTime = 160;

    /**
    * Sender static transfer time as described in subset 056, 4.4.10.1, in ms, default = 0
    */
    const int32_t  senderStaticTransferTime = -48;

    /**
    * Bus dynamic transfer time as described in subset 056, 4.4.10.5, in ms
    */
    const int32_t  dynamicBusTransferTime = 50;

    /**
    * Bus static transfer time as described in subset 056, 4.4.10.4, in ms, default = 0 */
    const int32_t  staticBusTransferTime = 0;

    /**
    * Connection setup time limit (time to wait for Run/ReadyToRun tlg) as described in subset 056, 9.12, in ms, default = 5000 */
    const uint32_t connectionSetupTimeLimit = 5000U;

    /**
    * Acknowledgment timeout period (time to wait for Connect/Authenticate Confirm tlg. a.k.a break timeout)
    * as described in subset 057, 7.2, in ms, default = 5000 */
    const uint32_t connectConfirmTimeout = 5000U;

    /**
    * Minimum time allowed for second incorrect receive as described in subset 056, 5.2.5.8.4, in sec, default =  24*60*60 ~ 24 hours (86400U)
    */
    const uint32_t secondIncorrectReceiveTimeout = 86400U;

    /**
    * version string length in VersionReply packet
    */
    static const uint8_t versionStringLength = 20U;

    /**
    * Buffer to hold the OPC version string
    */
    struct OpcVersionString
    {
      char_t versionString[versionStringLength + 1U];   /**<OpcVersionString::versionString*/
    };

    /**
    * Class to simplify the usage of the SPL library
    */
    class SplHandler
    {
    public:


      /**
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      */
      static SplHandler& instance();

      /**
      * Implements the preInit function.
      * Initializes the vfw channels.
      *
      */
      void preInit();

      /**
      * Initializes the SPL library
      *
      */
      void init();

      /**
      * Implements the runIn() function.
      * Owner or scheduler shall call runOut() once per activation.
      */
      void runIn();

      /**
      * Implements the runOut() function.
      * Owner or scheduler shall call runOut() once per activation.
      */
      void runOut();

      /**
      * sends the SPL-output to the OPC.
      * Called from _IOsetProfibusOutputData (interface function)
      *
      * @param [in] pBuff Pointer to a Profibus_Type buffer.
      */
      void sendOutputToOpc(Profibus_Type* const pBuff);

      /**
      * Reads the SPL-input from the OPC.
      * Called from _IOgetProfibusInputData (interface function)
      *
      * @param [out] pBuff Pointer to a Profibus_Type buffer.
      * @return 1 if data is available, 0 otherwise
      */
      uint8_t getInputFromOpc(Profibus_Type* const pBuff);

      /**
      * Pack the BTM command.
      * Called from AbstractBTMHandler::populateAndSendBtmCommand
      *
      * @param [in] btmCommand BTM command packed in a buffer.
      */
      void packBtmhToBtmPacket(const IO::GP_10ByteVitalSourceDataA& btmCommand);
      
      /**
      * Pack the odometer packet.
      * Called from Abstract odometry class
      *
      * @param [in] btmCommand ODO data packed in a buffer.
      */
      void packOdohToOpcPacket(const IO::GP_32ByteVitalSourceDataA& btmCommand);

      /**
      * Packs the ATP service packet
      *
      * @param[in] calendarTime - The new calendar time.      */
      void packAtpServiceCalendarTime(const uint32_t calendarTime);

      /**
      * Check if SPL is connected
      */
      bool isConnected() const;

      /**
      * Check if OPC lifesign is running
      */
      bool opcIsSystemRunning() const;

      /**
      * OPC version string
      * @return the OPC version string if received, null terminated string otherwise
      */
      const OpcVersionString& getOpcVersionString() const;

    private:

      /**
      * Constructor
      */
      SplHandler();

      /**
      * States of the SPL handler
      */
      enum OpcSplState
      {
        OPC_SPL_CLOCK_SYNCHRONIZING,
        OPC_SPL_WAIT_FOR_LEAVING_IDLE,
        OPC_SPL_SEND_CLOCK_SYNC,
        OPC_SPL_CONNECTING,
        OPC_SPL_CONNECTED,
        OPC_SPL_FAILED
      };
      
      /**
      * Initializes cross compare
      */
      void initCrossCompare() const;

      /**
      * Initializes the profibus part of the SPL library
      */
      void initProfibus();

      /**
      * Initializes the reference clock in the SPL library
      */
      void initReferenceClock() const;

      /**
      * Initializes the SPL connection in the SPL library
      */
      void initSplConnection();

      /**
      * Runs the in part while SPL is connected
      */
      void runSplConnectedIn();

      /**
      * Packs the life sign packet
      */
      void packLifeSignPacket();

      /**
      * Packs the version request packet
      */
      void packVersionRequest();

      /**
      * Sends the packed data to the SPL library
      */
      void sendSplOutput();

      /**
      * Handle state OPC_SPL_CLOCK_SYNCHRONIZING
      *
      * @param [in] clockState SPL clock state.
      */
      void handleOpcSplClockSynchronizing(const int32_t clockState);

      /**
      * Handle state OPC_SPL_WAIT_FOR_LEAVING_IDLE
      *
      * @param [in] clockState SPL clock state.
      */
      void handleOpcSplWaitForLeavingIdle(const int32_t clockState);
    
      /**
      * Handle state OPC_SPL_SEND_CLOCK_SYNC
      *
      * @param [in] clockState SPL clock state.
      */
      void handleOpcSplSendClockSync(const int32_t clockState);

      /**
      * Handle state OPC_SPL_CONNECTING
      *
      * @param [in] clockState SPL clock state.
      */
      void handleOpcSplConnecting(const int32_t clockState);

      /**
      * Handle state OPC_SPL_CONNECTED
      *
      * @param [in] clockState SPL clock state.
      */
      void handleOpcSplConnected(const int32_t clockState);

      /**
      * Handle MVB input message
      *
      * @param [in] rawBuffer   Buffer containing the SPL-MVB packet.
      * @param [in] messageSize Message size.
      */
      void handleMvbInput(uint8_t* const rawBuffer, const uint32_t messageSize);

      /**
      * Handle the life sign packet
      *
      * @param [in] packetBuffer  Buffer containing the Life Sign packet.
      */
      void handleLifeSign(VFW_Buffer& packetBuffer);

      /**
      * Handle the version reply packet
      *
      * @param [in] packetBuffer  Buffer containing the Version Reply packet.
      */
      bool handleVersionReply(VFW_Buffer& packetBuffer);

      /**
      * Read the App status channel
      * 
      */
      void readAppStatus();

      /**
      * Read the tigris time offset
      * 
      */
      void readTigrisOffset();

      /**
      * Returns the expected time sync server version
      *
      * A version of 7.1.1 will be in number format compacted to 0x070101ff where ff indicates that this byte is not used.
      *
      * @return the time sync server version
      */
      uint32_t getExpectedTimeSyncServerVersion() const;

      /**
      * Our cycle time sent to SPL library (SPL_IsActive)
      */

      static const uint32_t cycleTime = 100U;

      /**
      * Our profibus SAP-ID
      */
      static const uint8_t mvbSapId = 19U;

      /**
      * Clock telegram SAP-ID
      */
      static const uint8_t clockSapId = 32U;

      /**
      * SPL Header size
      */
      static const uint16_t splHeaderSize = 7U;

      // See 1DOC-1002321 (ETC, OPC and VAP Interface Specification)

      /**
      * NID_ATP_PACKET for BtmH2Btm 
      */
      static const uint16_t nidBtmH2Btm = 256U;

      /**
      * NID_ATP_PACKET for LifeSign
      */
      static const uint16_t nidLifesign = 266U;

      /**
      * NID_ATP_PACKET for OdoH2Opc
      */
      static const uint16_t nidOdoH2Opc = 267U;

      /**
      * NID_ATP_PACKET for VersionRequest
      */
      static const uint16_t nidVersionRequest = 268U;

      /**
      * NID_ATP_PACKET for ATP Services
      */
      static const uint16_t nidAtpService = 269U;

      /**
      * NID_ATP_PACKET for BtmTgm2BtmH
      */
      static const uint16_t nidBtmTgm2BtmH = 513U;

      /**
      * NID_ATP_PACKET for BtmStatus2BtmH
      */
      static const uint16_t nidBtmStatus2BtmH = 514U;

      /**
      * NID_ATP_PACKET for VersionReply
      */
      static const uint16_t nidVersionReply = 522U;

      /**
      * NID_ATP_PACKET for Application Status
      */
      static const uint16_t nidAppStatus = 270U;

      /**
      * NID_ATP_PACKET for Tigris Offset
      */
      static const uint16_t nidTigrisOffset = 300U;

      /**
      * L_ATP_PACKET for BtmTgm2BtmH
      */
      static const uint16_t packetLengthBtmTgm2BtmH = 170U;

      /**
      * L_ATP_PACKET for LengthLifesign
      */
      static const uint16_t packetLengthLifesign = 14U;

      /**
      * L_ATP_PACKET for LengthOdoH2Opc
      */
      static const uint16_t packetLengthOdoH2Opc = 36U;

      /**
      * L_ATP_PACKET for VersionRequest
      */
      static const uint16_t packetLengthVersionRequest = 4U;

      /**
      * L_ATP_PACKET for ATP Service
      */
      static const uint16_t packetLengthAtpService = 11U;

      /**
      * L_ATP_PACKET for BtmH2Btm
      */
      static const uint16_t packetLengthBtmH2Btm = 16U;

      /**
      * L_ATP_PACKET for BtmStatus2BtmH
      */
      static const uint16_t packetLengthBtmStatus2BtmH = 22U;

      /**
      * L_ATP_PACKET for VersionReply
      */
      static const uint16_t packetLengthVersionReply = 26U;

      /**
      * L_ATP_PACKET for Application Status
      */
      static const uint16_t packetLengthAppStatus = 11U;

      /**
      * L_ATP_PACKET for Tigris Offset
      */
      static const uint16_t packetLengthTigrisOffset = 13U;

      /**
      * Q_ITEM_STATUS = 1, Item set by source
      */
      static const uint8_t qItemStatusSet = 1U;

      /**
      * Lifesign_unit = 2, OPC
      */
      static const uint8_t opcLifeSignUnitId = 2U;

      /**
      * Lifesign_unit = 1, ETCS Core
      */
      static const uint8_t atpLifeSignUnitId = 1U;

      /**
      * Lifesign_status = 1, Idle
      */
      static const uint8_t lifesignStatusIdle = 1U;

      /**
      * Lifesign_status = 2, Running
      */
      static const uint8_t lifesignStatusRunning = 2U;

      /**
      * Lifesign_status = 4, Uncondit. Stopping Failure
      */
      static const uint8_t lifesignStatusUnconditionalStoppingFailure = 4U;

      /**
      * NID_BTM, BTM id used for our BTM
      */
      static const uint8_t nidBtm1 = 1U;

      /**
      * Source Unit OPC
      */
      static const uint8_t appStatusSourceUnitOpc = 2U;

      /**
      * Application Status is running
      */
      static const uint8_t appStatusRunning = 3U;

      /**
      * Application Status is running
      */
      static const uint8_t appStatusError = 4U;

      /**
      * Number of cycles sending clock syncs before connecting
      */
      static const uint8_t opcSplSendConnectCounterDelay = 20U;

      /**
      * Max number of cycles in order to receive a tigris offset packet (60 seconds)
      */
      static const uint16_t tigrisOffsetCycleCounter = 600U;

      /**
      * ATP Service Source Unit DMI
      */
      static const uint8_t atpServiceSourceUnitDmi = 2U;

      /**
      * ATP Service Number for Calendar Time
      */
      static const uint8_t atpServiceNumberCalendarTime = 2U;

      /**
      * ATP Service Action for Calendar Time
      */
      static const uint8_t atpServiceActionCalendarTime = 3U;

      /**
      *  TargetProfibusAddress, the Profibus address of the remote station.
      */
      static const uint8_t opcProfibusAddress = 20U;

      /**
      * Master 1: We are master. We shall call SPL_Connect, we must wait until is_Active returns IS_CONNECTED.
      */
      static const uint8_t splMaster = 1U;

      /** 
      * IdleTime, time in ms, this time will be sent to the target.
      * If the target does not receive a data or idle telegram within this time,
      * the target will disconnect the connection, i.e. the station must send
      * data or idle telegrams frequently enough, that the target will receive
      * them within this time.
      */
      static const uint16_t idleTime = 500U;      

      /**
      * Profibus Configuration. Profibus address, this Station physical address. Unique for each station
      */
      static const uint8_t ourProfibusAdress = 2U;

      /**
      *  Profibus Configuration. Minimum Station Delay of Responders in tBit unit. tBit is the time it takes to transmit 1 bit (1/Baud-Rate)
      */
      static const uint8_t minimumStationDelayOfResponders = 22U;

      /**
      *  Profibus Configuration. Maximum Station Delay of Responders in tBit unit.
      */
      static const uint8_t maximumStationDelayOfResponders = 150U;

      /**
      *  Profibus Configuration. Slot Time in tBit unit
      */
      static const uint16_t slotTime = 300U;

      /**
      *  Profibus Configuration. Quiet Time in tBit unit
      */
      static const uint8_t quietTime = 9U;

      /**
      *  Profibus Configuration. Setup Time in tBit unit
      */
      static const uint8_t setupTime = 1U;

      /**
      *  Profibus Configuration. Nominal token cycling time in milliseconds
      */
      static const uint16_t timeTargetRotation = 30000U;

      /**
      *  Profibus Configuration. GAP Actualization Factor
      */
      static const uint8_t gapActualizationFactor = 10U;

      /**
      *  Profibus Configuration. Station address of the highest active station
      */
      static const uint8_t highestStationAddress = 126U;

      /**
      *  Profibus Configuration. Maximum number of attempts a sender will re-send a telegram
      */
      static const uint8_t maxRetryLimit = 5U;

      /**
      *  SPL Clock Management parameters. Phase 1 Frequency. Time between sending of two sync and reference time telegrams in the start interval
      *  (see FFFIS Safe Time Layer subset - 056, chapter 7.3)in ms, recommended time in subset 059, 4.3. 50 - 200 ms
      */
      static const uint16_t phase1Frequency = 300U;

      /**
      *  SPL Clock Management parameters. Phase 1 Duration. Duration for sending sync and reference time telegrams with a high frequency in ms,
      *  recommended time in subset 059, 4.3. 30s
      */
      static const uint16_t phase1Duration = 30000U;

      /**
      *  SPL Clock Management parameters. Phase 2 Frequency. Time between sending of two sync and reference time telegrams after the start interval
      *  (see FFFIS Safe Time Layer subset - 056, chapter 7.3) in ms, recommended time in subset 059, 4.3. 200 - 400 ms
      */
      static const uint16_t phase2Frequency = 300U;

      /**
      *  SPL Clock Management parameters. Local Clock Inaccuracy. Local clock inaccuracy in ms, at least Profibus board(10ms) + 20% max task interruption time(75ms)
      */
      static const uint8_t localClockInaccuracy = 48U;

      /**
      *  Token value, could be any value since we are not use tokens on this platform.
      */
      static const int32_t tokenValue = 4711;

      /**
      * Life sign timeout value in ms
      */
      static const uint32_t lifeSignTimeoutValue = 360U;

      /**
      * Event to report Error if supervision of OPC fails
      */
      const ATC::Event eventErrorOpcSupervisionFailed;

      /**
      * SPL connection handle for the OPC 
      */
      SPL_Connection splConnection;

      /**
      * SPL Handler State
      */
      OpcSplState opcSplState;

      /**
      * Output channel for the OPC
      */
      Support::CrossCompareOutputChannel atpToOpcSap19OutputChannel;

      /**
      * Output channel for the OPC-Profibus data
      */
      Support::CrossCompareOutputChannel atpToOpcClockSyncOutputChannel;

      /**
      * Input channel for the OPC
      */
      VFW_SyncChannel opcToAtpSap19SyncChannel;

      /**
      * Input channel for the OPC Application Status
      */
      VFW_SyncChannel opcToAtpAppStatusSyncChannel;

      /**
      * Input channel for the TigrisOffset
      */
      VFW_SyncChannel tigrisOffsetSyncChannel;

      /**
      * Buffer to write SPL packets to before sending them to SPL Library
      */
      VFW_Buffer splOutBuffer;

      /**
      * Raw Buffer to write SPL packets to before sending them to SPL Library
      */
      uint8_t splOutRawBuffer[maxSplDataSize];

      /**
      * OPC Application status
      */
      uint8_t opcAppStatus;

      /**
      * OPC Lifesign status
      */
      uint8_t lifesignStatus;

      /**
      * OPC Lifesign timestamp
      */
      uint32_t lifesignTimestamp;

      /**
      * Clock sync send counter
      */
      uint8_t opcSplSendClockSyncCounter;

      /**
      * Tigris offset receive counter
      */
      uint16_t tigisOffsetReceivedCycleCounter;

      /**
      * UDP input raw buffer
      */
      uint8_t udpInputRawBuffer[ATC::splMessageSizeOpcToAtp];

      /**
      * UDP input buffer
      */
      VFW_Buffer udpInputBuffer;

      /**
      * Current tigris time offset
      */
      int32_t currentTigrisTimeOffset;

      /**
      * OPC version string
      */
      OpcVersionString opcVersionString;
    };
  } // namespace IO
} // namespace ATP
#endif

