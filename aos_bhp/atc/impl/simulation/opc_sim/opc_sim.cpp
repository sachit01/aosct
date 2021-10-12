/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This simulates the functionality of opc.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-06    spandita     Created
* 2016-09-23    arastogi    Removed ATC::
* 2016-09-15    spandita    Updated the functions with required functionalities
* 2016-09-26    spandita    Updated with review comments
* 2016-09-26    spandita    Removed ATC:: and updated the logic for init()
* 2016-10-04    spandita    Modified the Length and Time copy method to buffer
* 2016-10-12    arastogi    Balise number/sequence number should be updated only
*                           when new balise is found.
*                           calculation of balise odometer was using balise id.
*                           Should check for new balise to send only when the last
*                           one is sent.
* 2016-10-03    arastogi    Added check to reset previous balise id when idling.
* 2016-10-24    arastogi    Balise simulation now takes care of the train movement
*                           against MA.
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include <string>
#include "opc_sim.hpp"
#include "abstract_event_handler.hpp"
#include <vfw_identity.h>
#include "abstract_position.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_mode.hpp"
#include "abstract_targets.hpp"
#include "abstract_tracks.hpp"
#include "abstract_odometry.hpp"
#include "dmi_atc_event_codes.hpp"
#include "opc_sim_event_ids.hpp"
#include "sim_types.hpp"
#include <vfw_string.h>
#include <vfw_timer.h>

#ifdef WIN32
extern "C" int64_t vfwGetReferenceTime();
#else
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
namespace ATC
{
  namespace Sim
  {
    /**
    * BTM status buffer
    */
    uint8_t btmStatusBuffer[packetLengthBtmStatus2BtmH];

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    OPCSim::OPCSim() : IOComponent(atcOPCSimId, "OPCSim", "OS"),
      // creating different set of objects for different type of events
      errorReadChannel(Event::createSafetyHaltEvent(atcOPCSimId, CommonContainer, eventIdReadChannelFailure,
        NoEB, DMICom::ChannelFailure, "Read channel failure")),
      errorWriteChannel(Event::createSafetyHaltEvent(atcOPCSimId, CommonContainer, eventIdWriteChannelFailure,
        NoEB, DMICom::ChannelFailure, "Write channel failure")),
      telegramValid(true),
      telegramStatus(ATP::IO::NoFailureA),
      safetyTimeStamp(0U),
      initDone(false),
      opcSimWriteChannelDesc(static_cast<VFW_ChannelDesc>(NULL)),
      opcSimAOSPCWriteChannelDesc(static_cast<VFW_ChannelDesc>(NULL)),
      syncChannelReadDesc(static_cast<VFW_SyncChannel>(NULL)),
      syncOPCSimChannelReadDesc(static_cast<VFW_SyncChannel>(NULL)),
      startOdoFlag(false),
      stopOdoFlag(false),
      updateBaliseObj(true),
      oldOdoDir(ATP::DirUndefined),
      refTimeOffset(static_cast<uint32_t>(-50000)),
      refTimeOffsetDrift(0U),
      sendVersionReply(false),
      currentRoutineTestCounter(0U),
      holdTemporaryBsaValueCounter((5U * ATC::secToMSec) / ATC::cycleCntToMsec), // Startup takes 5 seconds...
      setBalisePresent(false),
      setOverHeated(false),
      initialBaliseNo(4513U),
      regBaliseIdReceived(false),
      lifesign(1U)
    {
      if (coreOPCSimPtr != 0)
      {
        // Error handler
        aosHalt(__FILE__, __LINE__, "OPCSim Constructor already instantiated");
      }

      // Initialize balise object
      baliseObj.nextExpectedBaliseId = 0U;
      baliseObj.nextExpectedBaliseOdo = 0;
      baliseObj.odoAtEndDistance = 0;
      baliseObj.odoAtStartDistance = 0;
      // Setup single instance pointer
      coreOPCSimPtr = this;

      memset(&statusMsg, 0, sizeof(statusMsg));
      memset(&telgramMsg, 0, sizeof(telgramMsg));
      memset(&telgramMsg2, 0, sizeof(telgramMsg2));
      memset(&btmStatusBuffer[0], 0, sizeof(btmStatusBuffer));

      timerWriteToOPCSimChannel = vfwTimerAllocate();
    }

    /******************************************************************************
    * Init
    ******************************************************************************/
    void OPCSim::preInit()
    {
      // Name of channel to read messages from ATP
      char_t opcSimProfibusReadChannelName[VFW_CH_NAME_MAX_Z];

      // Name of channel to read messages from Disp
      char_t opcSimAOSReadChannelName[VFW_CH_NAME_MAX_Z];

      // Name of channel to write messages from ATP
      char_t opcSimAOSWriteChannelName[VFW_CH_NAME_MAX_Z];

      // Name of channel to write the message other than BTM telegram message
      char_t opcSimWriteChannelName[VFW_CH_NAME_MAX_Z];

      // Name of channel to write the Application status message
      char_t opcSimWriteAppStatusName[VFW_CH_NAME_MAX_Z];

      // Name of channel to write the Application status message
      char_t opcSimWriteTigrisTimeOffsetName[VFW_CH_NAME_MAX_Z];

#ifdef _SIL
      static_cast<void>(vfw_strlcpy(&opcSimProfibusReadChannelName[0], btmhChannelATPAToDisp, sizeof(opcSimProfibusReadChannelName)));
      static_cast<void>(vfw_strlcpy(&opcSimAOSReadChannelName[0], opcChannelDispToATPA, sizeof(opcSimAOSReadChannelName)));
      static_cast<void>(vfw_strlcpy(&opcSimWriteChannelName[0], btmhChannelDispToATPA, sizeof(opcSimWriteChannelName)));
      static_cast<void>(vfw_strlcpy(&opcSimWriteAppStatusName[0], btmhChannelDispAppDataToATPA, sizeof(opcSimWriteAppStatusName)));
      static_cast<void>(vfw_strlcpy(&opcSimWriteTigrisTimeOffsetName[0], tigrisOffsetChannelNameA, sizeof(opcSimWriteTigrisTimeOffsetName)));
      static_cast<void>(vfw_strlcpy(&opcSimAOSWriteChannelName[0], opcChannelATPAToDisp, sizeof(opcSimAOSWriteChannelName)));

#else
      // In HIL , messages on VFW channels need to be sent to the opposite CPU, as VFW doesn't allow you to open a read and write channel 
      // with the same name on a single process
      if (VFW_A_SIDE == vfwGetSide())
      {
        static_cast<void>(vfw_strlcpy(&opcSimProfibusReadChannelName[0], btmhChannelATPBToDisp, sizeof(opcSimProfibusReadChannelName)));
        static_cast<void>(vfw_strlcpy(&opcSimAOSReadChannelName[0], opcChannelDispToATPB, sizeof(opcSimAOSReadChannelName)));
        static_cast<void>(vfw_strlcpy(&opcSimWriteChannelName[0], btmhChannelDispToATPB, sizeof(opcSimWriteChannelName)));
        static_cast<void>(vfw_strlcpy(&opcSimWriteAppStatusName[0], btmhChannelDispAppDataToATPB, sizeof(opcSimWriteAppStatusName)));
        static_cast<void>(vfw_strlcpy(&opcSimWriteTigrisTimeOffsetName[0], tigrisOffsetChannelNameB, sizeof(opcSimWriteTigrisTimeOffsetName)));
        static_cast<void>(vfw_strlcpy(&opcSimAOSWriteChannelName[0], opcChannelATPBToDisp, sizeof(opcSimAOSWriteChannelName)));
      }
      else if (VFW_B_SIDE == vfwGetSide())
      {
        static_cast<void>(vfw_strlcpy(&opcSimProfibusReadChannelName[0], btmhChannelATPAToDisp, sizeof(opcSimProfibusReadChannelName)));
        static_cast<void>(vfw_strlcpy(&opcSimAOSReadChannelName[0], opcChannelDispToATPA, sizeof(opcSimAOSReadChannelName)));
        static_cast<void>(vfw_strlcpy(&opcSimWriteChannelName[0], btmhChannelDispToATPA, sizeof(opcSimWriteChannelName)));
        static_cast<void>(vfw_strlcpy(&opcSimWriteAppStatusName[0], btmhChannelDispAppDataToATPA, sizeof(opcSimWriteAppStatusName)));
        static_cast<void>(vfw_strlcpy(&opcSimWriteTigrisTimeOffsetName[0], tigrisOffsetChannelNameA, sizeof(opcSimWriteTigrisTimeOffsetName)));
        static_cast<void>(vfw_strlcpy(&opcSimAOSWriteChannelName[0], opcChannelATPAToDisp, sizeof(opcSimAOSWriteChannelName)));
      }
      else
      {
        // To please lint:
        opcSimProfibusReadChannelName[0] = '\0';
        opcSimAOSReadChannelName[0] = '\0';
        opcSimAOSWriteChannelName[0] = '\0';
        opcSimWriteChannelName[0] = '\0';
        opcSimWriteAppStatusName[0] = '\0';
        opcSimWriteTigrisTimeOffsetName[0] = '\0';

        aosHalt(__FILE__, __LINE__, "Invalid Side");
      }
#endif

      //Initialize the read channels
      const VFW_ChannelDesc opcSimProfibusReadChannelDesc = vfwChannelOpenRead(&opcSimProfibusReadChannelName[0],
        ATC::splQueueSize, splMessageSizeOpcToAtp, &opcSimProfibusReadChannelName[0]);
      const VFW_ChannelDesc opcSimAOSReadChannelDesc = vfwChannelOpenRead(&opcSimAOSReadChannelName[0],
        ATC::opcSimMsgInQueueSize, opcSimMessageSize, &opcSimAOSReadChannelName[0]);

      vfwChannelSetOverwritable(opcSimProfibusReadChannelDesc);
      vfwChannelSetOverwritable(opcSimAOSReadChannelDesc);

      //Initialize the write channels
      opcSimWriteChannelDesc = vfwChannelOpenWrite(&opcSimWriteChannelName[0],
        ATC::splQueueSize, splMessageSizeOpcToAtp);
      opcSimWriteAppStatusChannelDesc = vfwChannelOpenWrite(&opcSimWriteAppStatusName[0],
        ATC::appDataQueueSize, appDataMessageSize);
      opcSimWriteTigrisTimeOffsetChannelDesc = vfwChannelOpenWrite(&opcSimWriteTigrisTimeOffsetName[0],
        ATC::tigrisOffsetQueueSize, tigrisOffsetMessageSize);
      opcSimAOSPCWriteChannelDesc = vfwChannelOpenWrite(&opcSimAOSWriteChannelName[0],
        ATC::opcSimMsgOutQueueSize, opcSimMessageSize);

      if ((static_cast<VFW_ChannelDesc>(NULL) != opcSimProfibusReadChannelDesc)          &&
          (static_cast<VFW_ChannelDesc>(NULL) != opcSimAOSReadChannelDesc)               &&
          (static_cast<VFW_ChannelDesc>(NULL) != opcSimWriteChannelDesc)                 &&
          (static_cast<VFW_ChannelDesc>(NULL) != opcSimWriteAppStatusChannelDesc)        &&
          (static_cast<VFW_ChannelDesc>(NULL) != opcSimWriteTigrisTimeOffsetChannelDesc) &&
          (static_cast<VFW_ChannelDesc>(NULL) != opcSimAOSPCWriteChannelDesc))
      {
        //Sync with Diversified Channel(A/B)
        syncChannelReadDesc = vfwSyncAddChannel(opcSimProfibusReadChannelDesc, trueVfw);
        syncOPCSimChannelReadDesc = vfwSyncAddChannel(opcSimAOSReadChannelDesc, trueVfw);

        //Deactivate the event driven callback functionality.
        vfwSyncChannelDeactivate(syncChannelReadDesc);
        vfwSyncChannelDeactivate(syncOPCSimChannelReadDesc);

        vfwChannelSetOverwritable(opcSimWriteAppStatusChannelDesc);
        vfwChannelSetOverwritable(opcSimAOSPCWriteChannelDesc);
      }
      else
      {
        aosHalt(__FILE__, __LINE__, "Failed to open channels");
      }
    }

    /******************************************************************************
    * Init
    ******************************************************************************/
    bool OPCSim::init()
    {
      //Bool to check the initialization status
      if (!initDone)
      {
        //initialize the Simulated Status message with Init value
        simulatedBtmStatusMsg.status = 1U;
        simulatedBtmStatusMsg.msgData.enableTelegram = 3U;
        //used protocol version is 8 defined in ref document
        simulatedBtmStatusMsg.msgData.usedProtocolVersion = 8U; // See 3NSS010889D0108 version 2.3
        simulatedBtmStatusMsg.msgData.bsa = btmStartUpOrRoutineTestIsInProgress; //BSA 
        simulatedBtmStatusMsg.msgData.bsaCounter = 0U; //BSA counter
        simulatedBtmStatusMsg.msgData.ifKActive = 0U; //IFK field
        simulatedBtmStatusMsg.msgData.statusandOption = 0U;
        simulatedBtmStatusMsg.msgData.telePowerStatus = 0U;
        simulatedBtmStatusMsg.msgData.usedEuroloopCode = 0U;
        simulatedBtmStatusMsg.msgData.usedIfBtmId = 0U;
        simulatedBtmStatusMsg.msgData.safetyMode = 0U;

        //Initialize the Simulated BTM Telegram Messages
        simulatedBtmTelMsg.status = 1U;
        simulatedBtmTelMsg.msgData1.telegramType = btmValid830Telegram;        //class 4
        simulatedBtmTelMsg.msgData1.baliseNumber = 0U;          //balise number
        simulatedBtmTelMsg.msgData1.sequenceNo = 0U;           //Seq number
        simulatedBtmTelMsg.msgData1.endTimeStamp = 0U;
        simulatedBtmTelMsg.msgData1.beginTimeStamp = 0U;
        simulatedBtmTelMsg.msgData1.finalReport = 8U;         // final report
        initDone = true;

        vfwTimerSetTimeout(timerWriteToOPCSimChannel, timeoutWriteToOPCSimChannel);
        vfwTimerStart(timerWriteToOPCSimChannel);
        isConnected = false;
      }

      return initDone;
    }


    /******************************************************************************
     * runIn
     ******************************************************************************/
    void OPCSim::runIn()
    {
      opcSimChannelRead();

      syncChannelRead();
    }


    /******************************************************************************
    * opcSimChannelRead
    ******************************************************************************/
    void OPCSim::opcSimChannelRead()
    {
      uint8_t opcSimMsgBuffer[opcSimMessageSize];

      //Check for any byte present on 
      while (vfwSyncChannelStat(syncOPCSimChannelReadDesc) > 0U)
      {
        const int32_t numReadBytes = vfwSyncChannelRead(syncOPCSimChannelReadDesc, &opcSimMsgBuffer[0], opcSimMessageSize);

        if ((numReadBytes == static_cast<int32_t>(headerFooterLen + simOPCRegBaliseDataLen)))
        {
          VFW_Buffer buffer;
          vfwInitBuffer(&buffer, &opcSimMsgBuffer[0], static_cast<uint32_t>(numReadBytes));

          vfwSetReadBuffer(&buffer, static_cast<uint32_t>(numReadBytes));

          // converting STX from network to host byte order
          const uint8_t stx = vfwGetU8(&buffer);
          // converting VER from network to host byte order
          const uint8_t ver = vfwGetU8(&buffer);
          // converting LEN from network to host byte order
          const uint16_t len = vfwGetU16(&buffer);
          (void)len; //To remove Lint warning
          // converting nidMessageType from network to host byte order
          const uint8_t nidMessageType = vfwGetU8(&buffer);
          // converting balise ID from network to host byte order
          const uint16_t n_bid = vfwGetU16(&buffer);

          if (stx == STX)
          { //checks version received should be same as protocol Version(1)
            if (ver == Sim::simRegBaliseIDProtocolVer)
            { //checks the Simulated Balise Id data type
              if (nidMessageType == simBaliseIDNidMsgType)
              {
                if (n_bid != initialBaliseNo)
                {
                  initialBaliseNo = n_bid;
                  ATP::Kernel::BaliseSearchModeState baliseSubMode = ATP::Kernel::AbstractModeControl::corePtr()->getBaliseSearchModeState();
                  if (ATP::ATPModeBaliseSearch == ATP::Kernel::AbstractModeControl::corePtr()->getCurrentMode()
                    && ((ATP::Kernel::BaliseSearchMode::baliseSearchWaitForBaliseReg == baliseSubMode) ||
                    (ATP::Kernel::BaliseSearchMode::baliseSearchWaitForBaliseReReg == baliseSubMode)))
                  {
                    baliseObj.nextExpectedBaliseId = 0U; // needs to be reset, otherwise it will not be updated when a balise is found.
                  }
                }
                writeToLog(ATC::BriefLog, "Reg. balise ID received:", static_cast<uint32_t>(n_bid), __FILE__, __LINE__);
                regBaliseIdReceived = true;
                isConnected = true;
              }
              else
              {
                trace.write(1U, "Bad message type");
              }
            }
          }
        }
        else
        {
          trace.write(1U, "Bad message length or bad timing");
        }
      }
    }

    /******************************************************************************
    * syncChannelRead
    ******************************************************************************/
    void OPCSim::syncChannelRead()
    {
      //Input buffer 
      uint8_t  simBtmhandlerTelegram[splMessageSizeAtpToOpc];
      uint8_t  inBuffer[10];

      //Check for any byte present on 
      while (vfwSyncChannelStat(syncChannelReadDesc) > 0U)
      {
        //Read from channel
        const int32_t numReadBytes = vfwSyncChannelRead(syncChannelReadDesc, &simBtmhandlerTelegram[0], splMessageSizeAtpToOpc);
        //check if any data available
        if (numReadBytes >= static_cast<int8_t>(splHeaderSize))
        {
          const uint32_t dataLen = static_cast<uint32_t>(numReadBytes) - splHeaderSize;
          VFW_Buffer readBuffer;
          vfwInitBuffer(&readBuffer, &simBtmhandlerTelegram[splHeaderSize], dataLen);
          vfwSetReadBuffer(&readBuffer, dataLen);

          bool done = false;

          while (!done)
          {
            const uint32_t bytesLeft = vfwGetValidSize(&readBuffer);

            if (bytesLeft == 0U)
            {
              done = true;
            }
            else if (bytesLeft < (2U * sizeof(uint16_t))) // must contain the obligatory header (see below)
            {
              trace.write(1U, "Incorrect message received from BTM handler");
              done = true;
            }
            else
            {
              const uint16_t nidPacket = vfwGetU16(&readBuffer);
              const uint16_t packetLength = vfwGetU16(&readBuffer);

              //Check received message is of correct type
              if ((nidPacket == nidBtmH2Btm) && (packetLength == packetLengthBtmH2Btm))
              {
                const uint8_t nidBtm = vfwGetU8(&readBuffer); // NID_BTM
                const uint8_t qItemStatus = vfwGetU8(&readBuffer); // Q_ITEM_STATUS

                vfwCpyToRawBuffer(&inBuffer[0], &readBuffer, sizeof(inBuffer));

                //To reduce the 
                //Tele Powering status
                const uint8_t prevTelePowerStatus = simulatedBtmStatusMsg.msgData.telePowerStatus;
                simulatedBtmStatusMsg.msgData.telePowerStatus = (inBuffer[0] & 0x03U);

                if (prevTelePowerStatus != simulatedBtmStatusMsg.msgData.telePowerStatus)
                {
                  const uint32_t newHoldTemporaryBsaValueCounter = (1U * ATC::secToMSec) / ATC::cycleCntToMsec;

                  // Don't let the antenna powering mess up ongoing routine test
                  if (newHoldTemporaryBsaValueCounter > holdTemporaryBsaValueCounter)
                  {
                    holdTemporaryBsaValueCounter = newHoldTemporaryBsaValueCounter; // Number of cycles it should be active...
                  }
                }
                
                //Get the BTM ID
                simulatedBtmStatusMsg.msgData.usedIfBtmId = (inBuffer[0] & 0x0CU) >> 2;
                //Set the IfK 
                simulatedBtmStatusMsg.msgData.ifKActive = (inBuffer[0] & 0x10U) >> 4;
                //Set the Preliminary -Availability test to 0
                simulatedBtmStatusMsg.msgData.prelTestStatus = 0U;          //Set the Preliminary -Availability test to 0
                // Enable Tele Format to Euro balise 830/210
                simulatedBtmStatusMsg.msgData.enableTelegram = inBuffer[1];
                //Set the safety code to 0
                simulatedBtmStatusMsg.msgData.safetyMode = 0U;
                //Unpacking and storing the bits of option and status field in status message accordingly
                const uint8_t btmOptionsInCommandTelegram = inBuffer[4];

                // Options:
                // Bit 0
                //  0: Automatically perform a routine test at start-up.
                //  1: Do not automatically perform a routine test at start - up.
                // Bit 1
                //  0 : Set BSA to(FALSE) sporadic failure(2) if the BTM related part of a routine test fails.
                //  1 : Do not let the results of a routine test affect BSA.
                // Bit 2
                //  0 : Do not send more than one balise report per balise passage.
                //  1 : Send a balise report at least each 100 ms as long as a balise is pre - sent.
                // Bit 3
                //  0 : Run - time test failures shall give BSA = (FALSE)Permanent failure.
                //  1 : Run - time test failures shall give BSA = (FALSE)Temporary failure


                // Status and Options:
                // Bit 5:
                //   0: (FALSE)The BTM will automatically perform a routine test at start - up.
                //   1 : (TRUE)The BTM will not automatical - ly perform a routine test at start - up.
                // Bit 4:
                //   0 : (FALSE)The BTM will set BSA to(FALSE) sporadic failure(2) if a BTM function related part of the routine test fails.
                //   1 : (TRUE)The BTM will not let the results of a BTM function related part of the rou - tine test affect BSA.
                // Bit 3:
                //   0 : (FALSE)The BTM will not send more than one balise report per balise passage.
                //   1 : (TRUE)The BTM will send a balise report at least each 100 ms as long as a balise is present.
                // Bit 2:
                //   0 : (FALSE)The BTM will set BSA to(FALSE) permanent failure(3) if the BTM function related parts of a run - time test fails.
                //   1 : (TRUE)The BTM will set BSA to(FALSE) temporary failure(2) if the BTM function related parts of a run - time test fails.
                // 
                simulatedBtmStatusMsg.msgData.statusandOption = (0x00U | // 0 -1 unused
                  ((btmOptionsInCommandTelegram & 0x08U) >> 1) | // Bit 2: Equals bit 3 in the BTM Options sent in the BTM Command Telegram message
                  ((btmOptionsInCommandTelegram & 0x04U) << 1) | // Bit 3: Equals bit 2 in the BTM Options sent in the BTM Command Telegram message
                  ((btmOptionsInCommandTelegram & 0x02U) << 3) | // Bit 4: Equals bit 1 in the BTM Options sent in the BTM Command Telegram message
                  ((btmOptionsInCommandTelegram & 0x01U) << 5) | // Bit 5: Equals bit 0 in the BTM Options sent in the BTM Command Telegram message
                  ((setBalisePresent ? 0x01U : 0x00U) << 6) |  //bit 6 : equal to balise present
                  ((setOverHeated    ? 0x01U : 0x00U) << 7));  //bit 7 : overheated

                //Unused
                simulatedBtmStatusMsg.msgData.usedEuroloopCode = 0U;

                // Check if we should run a new routine test or not...
                const uint8_t newRoutineTestCounter = inBuffer[2];
                if (newRoutineTestCounter != currentRoutineTestCounter)
                {
                  currentRoutineTestCounter = newRoutineTestCounter;
                  holdTemporaryBsaValueCounter = (30U * ATC::secToMSec) / ATC::cycleCntToMsec; // Number of cycles it should be active...

                  simulatedBtmStatusMsg.msgData.bsa = btmStartUpOrRoutineTestIsInProgress;
                  ++simulatedBtmStatusMsg.msgData.bsaCounter;
                }

                if (simulatedBtmStatusMsg.msgData.telePowerStatus != 0U)
                {
                  simulatedBtmStatusMsg.msgData.prelTestStatus = btmPreliminaryBtmStatusGreen;
                }
                else
                {
                  simulatedBtmStatusMsg.msgData.prelTestStatus = btmPreliminaryBtmStatusUnavailable;
                }
              }
              else if ((nidPacket == nidLifesign) && (packetLength == packetLengthLifesign))
              {
                // Do nothing, discard life sign at the moment...
                vfwGetU8(&readBuffer); // Lifesign_unit
                lifesign = vfwGetU8(&readBuffer); // Lifesign_status
                vfwGetU32(&readBuffer); // TimeStmp
                vfwGetU32(&readBuffer); // RefTimeOffset
              }
              else if ((nidPacket == nidVersionRequest) && (packetLength == packetLengthVersionRequest))
              {
                sendVersionReply = true;
              }
              else if ((nidPacket == nidAtpService) && (packetLength == packetLengthAtpService))
              {
                trace.write(1U, "Received ATP service");
                // Do nothing, discard ATP service at the moment...
                const uint8_t sourceUnit = vfwGetU8(&readBuffer);
                const uint8_t serviceNumber = vfwGetU8(&readBuffer);
                const uint8_t action = vfwGetU8(&readBuffer);
                const uint32_t timeStamp = vfwGetU32(&readBuffer);

                trace.write(1U, "SRC unit", static_cast<uint32_t>(sourceUnit)); // Source unit
                trace.write(1U, "Service number", static_cast<uint32_t>(serviceNumber)); // Service Number
                trace.write(1U, "Action", static_cast<uint32_t>(action)); // Service Action
                trace.write(1U, "Timestamp", timeStamp); // TimeStmp
              }
              else if ((nidPacket == nidOdoH2Opc) && (packetLength == packetLengthOdoH2Opc))
              {
                trace.write(5U, "Received packetLengthOdoH2Opc");
                // Do nothing, discard packetLengthOdoH2Opc at the moment...
                for (uint8_t i = 4U; i < packetLengthOdoH2Opc; ++i)
                {
                  static_cast<void>(vfwGetU8(&readBuffer));
                }
              }
              else
              {
                trace.write(2U, "Incorrect message received from BTM handler");
              }
            }
          } // while (!done)
        }
        else
        {
          trace.write(2U, "Incorrect message received from BTM handler");
        }
      }
    }


    /******************************************************************************
    * runOut
    ******************************************************************************/
    void OPCSim::runOut()
    {
      // Update the baliseobj to reflect the new odoOffset.
      updateOdoValForBaliseObj();

      writeToBTMChannel();
      writeToOPCSimChannel();
    }

    /******************************************************************************
    * Write to channels
    ******************************************************************************/
    void OPCSim::writeToBTMChannel()
    {
      ++refTimeOffsetDrift;

      if (refTimeOffsetDrift > 150U)
      {
        refTimeOffsetDrift = 0U;
        ++refTimeOffset; // Let the time offset slowly drift.
      }

      // Verify and simulate the balise passage
      verifyBalisePassage();

      // Check whether write channel opened?
      if ((static_cast<VFW_ChannelDesc>(NULL) != opcSimWriteChannelDesc) &&
        (static_cast<VFW_SyncChannel>(NULL) != syncChannelReadDesc))
      {
        if (lifesign > 1U)
        {
          // Pack the output packet
          pack();
          // Write to channels
          writeToChannel();
        }
        writeAppStatus();

        if ((refTimeOffsetDrift % 30U) == 0U) // Do not write the offset every cycle
        {
          writeTigrisTimeOffset();
        }

        //Reset the bit 7 of Status and option field of Status message
        simulatedBtmStatusMsg.msgData.statusandOption &= 0xbfU;
      }
    }

    /******************************************************************************
    * Write to OPCSim AOS channels
    ******************************************************************************/
    void OPCSim::writeToOPCSimChannel()
    {
      if (isConnected)
      {
         // Transmit the simulated ATP Ready message cyclically at the frequency given by timeoutWriteToOPCSimChannel
         // Note !!
         // AOSPC will regard the ATP Ready message from OPC Sim as an acknowledge that the OPCSim is up and running and has received the
         // registration balise from AOSPC.
         // The connection between AOSPC and OPCSim is UDP and the ATPReady - message will not reach AOSPC 
         // (even if the ATP sends it cyclically to the dispatcher) before the Dispatcher has received 
         // at least one message (with the RegBalise) from AOSPC. 
         // The reason is that the dispatcher retrieves the IP of the UDP peer(AOSPC) from the message 
         // received from AOSPC. So it means that a full message-exchange has been performed when the ATPReady-message reaches AOSPC.
         
        if (vfwTimerCheck(timerWriteToOPCSimChannel) == ATC::trueVfw)
        {
          if (regBaliseIdReceived)
          {
            uint8_t buffer[ATC::opcSimMessageSize];
            VFW_Buffer vfwBuffer;
            vfwInitBuffer(&vfwBuffer, &buffer[0], opcSimMessageSize);

            vfwPutU8(&vfwBuffer, STX);
            vfwPutU8(&vfwBuffer, simATPReadyProtocolVer);
            vfwPutU16(&vfwBuffer, simOPCATPReadyDataLen);
            vfwPutU8(&vfwBuffer, simATPReadyIDNidMsgType);

            vfwPutU8(&vfwBuffer, ETX);

            vfwChannelWriteBuffer(opcSimAOSPCWriteChannelDesc, &vfwBuffer);
            writeToLog(ATC::BriefLog, "ATP Ready", __FILE__, __LINE__);
            regBaliseIdReceived = false;
          }

          vfwTimerStart(timerWriteToOPCSimChannel);          
        }
      }
    }

    /******************************************************************************
    * Write to channels
    ******************************************************************************/
    void OPCSim::writeToChannel()
    {
      VFW_Buffer buffer;
      uint8_t rawSplBuffer[splMessageSizeOpcToAtp];
      
      vfwInitBuffer(&buffer, &rawSplBuffer[0], sizeof(rawSplBuffer));

      uint16_t dataSize = packetLengthLifesign + packetLengthBtmTgm2BtmH;

      if (sendVersionReply)
      {
        dataSize += packetLengthVersionReply;
      }

      uint8_t newBtmStatusBuffer[packetLengthBtmStatus2BtmH];
      VFW_Buffer btmVfwStatusBuffer;
      vfwInitBuffer(&btmVfwStatusBuffer, &newBtmStatusBuffer[0], sizeof(newBtmStatusBuffer));

      packStatusPacket(btmVfwStatusBuffer);

      bool packStatusBuffer = false;

      // The timestamp (4 Bytes) is put at the end and should not be checked...
      if (memcmp(&newBtmStatusBuffer[0], &btmStatusBuffer[0], sizeof(newBtmStatusBuffer) - 4U) != 0)
      {
        memmove(&btmStatusBuffer[0], &newBtmStatusBuffer[0], sizeof(newBtmStatusBuffer));
        packStatusBuffer = true;
        dataSize += packetLengthBtmStatus2BtmH;
      }

      packSplHeader(buffer, dataSize);

      if (sendVersionReply)
      {
        sendVersionReply = false;
        packVersionReply(buffer);
      }

      packLifeSign(buffer);

      if (packStatusBuffer)
      {
        vfwCpyFromRawBuffer(&buffer, &newBtmStatusBuffer[0], sizeof(newBtmStatusBuffer));
      }

      packBtmTelegramPacket(buffer);

      const uint16_t packedSize = static_cast<uint16_t>(vfwGetValidSize(&buffer));

      if (packedSize == (dataSize + splHeaderSize)) // 7 bytes = SPL header size
      {
        vfwChannelWriteBuffer(opcSimWriteChannelDesc, &buffer);
      }
      else
      {
        // Error!
      }
    }

    /******************************************************************************
    * writeAppStatus
    ******************************************************************************/
    void OPCSim::writeAppStatus()
    {
      VFW_Buffer buffer;
      uint8_t rawAppStatusBuffer[packetLengthAppStatus];

      vfwInitBuffer(&buffer, &rawAppStatusBuffer[0], sizeof(rawAppStatusBuffer));

      // Pack the Application Status packet
      vfwPutU16(&buffer, nidAppStatus);
      vfwPutU16(&buffer, packetLengthAppStatus);
      vfwPutU8(&buffer, 2U); // Source unit, 2 = OPC
      vfwPutU8(&buffer, 3U); // Status, 3 = Running
      vfwPutU8(&buffer, 0U); // DMI Status, 0 = No DMI connected
      const uint32_t timeStmp = static_cast<uint32_t>(vfwGetReferenceTime());
      vfwPutU32(&buffer, timeStmp + refTimeOffset); // Timestamp in milliseconds set when packet created by the sender

      vfwChannelWriteBuffer(opcSimWriteAppStatusChannelDesc, &buffer);
    }


    /******************************************************************************
    * writeTigrisTimeOffset
    ******************************************************************************/
    void OPCSim::writeTigrisTimeOffset()
    {
      VFW_Buffer buffer;
      uint8_t rawBuffer[packetLengthTigrisTimeOffset];

      vfwInitBuffer(&buffer, &rawBuffer[0], sizeof(rawBuffer));

      // Pack the Application Status packet
      vfwPutU16(&buffer, nidTigirisTimeOffset);
      vfwPutU16(&buffer, packetLengthTigrisTimeOffset);
      // Status, not used today
      vfwPutU8(&buffer, 3U);
      // tigrisTimeOffset
      vfwPutU32(&buffer, refTimeOffset);
      // A version of 7.1.1 will be in number format compacted to 0x070101ff where ff indicates that this byte is not used in the numbering format.
      vfwPutU32(&buffer, 0x070101ffU);
      vfwChannelWriteBuffer(opcSimWriteTigrisTimeOffsetChannelDesc, &buffer);
    }
    

    /******************************************************************************
    * packSplHeader
    ******************************************************************************/
    void OPCSim::packSplHeader(VFW_Buffer& buffer, uint16_t dataSize)
    {
      // Fill in a fake header
      // SPL header size = 7.
      vfwPutU16(&buffer, dataSize);
      // This was hard coded 2, It this looks like it can be HW address of ATPCU CORE (because it is a number 2).
      vfwPutU8(&buffer, 2U);
      // This was hard coded 20, If the one above is source then this is destination HW address. Or is it the other way around?
      vfwPutU8(&buffer, 2U);
      vfwPutU8(&buffer, 19U);
      vfwPutU8(&buffer, 19U);
      // Or is this the HW addres of ATPCU CORE 
      vfwPutU8(&buffer, 2U);
    }


    /******************************************************************************
    * packVersionReply
    ******************************************************************************/
    void OPCSim::packVersionReply(VFW_Buffer& buffer)
    {
      // First pack the SPL packet header
      vfwPutU16(&buffer, nidVersionReply);
      vfwPutU16(&buffer, packetLengthVersionReply);

      const char_t versionString[] = "4.2.10\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
      const uint32_t versionStringSize = 20U;

      vfwPutU8(&buffer, 6U); // 6 = OPC
      vfwPutU8(&buffer, static_cast<uint8_t>(strnlen(versionString, versionStringSize)));

      /* Version text string on format :
         xxx.yyy.lll where
         xxx = Major release
         yyy = Middle release
         lll = Minor version
      */
      vfwCpyFromRawBuffer(&buffer, versionString, versionStringSize);
    }

    
    /******************************************************************************
    * packLifeSign
    ******************************************************************************/
    void OPCSim::packLifeSign(VFW_Buffer& buffer)
    {
      const int64_t currentTime = vfwGetReferenceTime();

      // First pack the SPL packet header
      vfwPutU16(&buffer, nidLifesign);
      vfwPutU16(&buffer, packetLengthLifesign);

      vfwPutU8(&buffer, 2U); // Lifesign_unit (2 = OPC)

      vfwPutU8(&buffer, lifesign); // Lifesign_status 2 = running

      /* Timestamp in milliseconds set when lifesign packet created by the sender.
          The TimeStmp is used by the receiver to verify that the packet really has
          been updated and newly sent by the receiver in a new packet send routine
      */
      const uint32_t timeStamp = static_cast<uint32_t>(vfwGetReferenceTime());
      vfwPutU32(&buffer, timeStamp + refTimeOffset);

      /* 4 byte integer value representing the time difference between the SPL ref
          time and the TigrisReftime in the OPC in milliseconds.
          Formula
          RefTimeOffset = TIGRIS Ref time - SPL Reftime
          The value is only applicable in the direction from OPC to ETCS Core.
          In the opposite direction, the parameter is set to 0.
      */
      vfwPutU32(&buffer, refTimeOffset);
    }

    
    /******************************************************************************
    * packStatusPacket
    ******************************************************************************/
    void OPCSim::packStatusPacket(VFW_Buffer& buffer)
    {
      // First pack the SPL packet header
      vfwPutU16(&buffer, nidBtmStatus2BtmH);
      vfwPutU16(&buffer, packetLengthBtmStatus2BtmH);

      vfwPutU8(&buffer, 1U); // NID_BTM, always 1                             
      // Write the Message status
      vfwPutU8(&buffer, simulatedBtmStatusMsg.status); // Q_ITEM_STATUS

      // Now pack the actual status message

      vfwCpyFromRawBuffer(&buffer, &(statusMsg.safetyData[0]), sizeof(statusMsg.safetyData));

      uint8_t populate = telegramValid ? 1U : 0U;
      vfwPutU8(&buffer, populate);
      populate = (ATP::IO::NoFailureA == telegramStatus) ? 0U : 1U;
      vfwPutU8(&buffer, populate);
      vfwPutU32(&buffer, safetyTimeStamp);
    }

    
    /******************************************************************************
    * packBtmTelegramPacket
    ******************************************************************************/
    void OPCSim::packBtmTelegramPacket(VFW_Buffer& buffer)
    {
      // First pack the SPL packet header
      vfwPutU16(&buffer, nidBtmTgm2BtmH);
      vfwPutU16(&buffer, packetLengthBtmTgm2BtmH);

      //BTM Telegram message
      vfwPutU8(&buffer, 1U); // NID_BTM, always 1                             

      //Write start value to buffer
      vfwPutU8(&buffer, simulatedBtmTelMsg.status); // Q_ITEM_STATUS

      vfwCpyFromRawBuffer(&buffer, &(telgramMsg.safetyData[0]), sizeof(telgramMsg.safetyData));

      uint8_t populate = telegramValid ? 1U : 0U;
      vfwPutU8(&buffer, populate);
      populate = (ATP::IO::NoFailureA == telegramStatus) ? 0U : 1U;
      vfwPutU8(&buffer, populate);

      vfwPutU32(&buffer, safetyTimeStamp);

      //write the Message status2
      vfwPutU8(&buffer, simulatedBtmTelMsg.status);

      vfwCpyFromRawBuffer(&buffer, &(telgramMsg2.safetyData[0]), sizeof(telgramMsg2.safetyData));

      populate = telegramValid ? 1U : 0U;
      vfwPutU8(&buffer, populate);
      populate = (ATP::IO::NoFailureA == telegramStatus) ? 0U : 1U;
      vfwPutU8(&buffer, populate);

      vfwPutU32(&buffer, safetyTimeStamp);

      //write the Message status3
      vfwPutU8(&buffer, simulatedBtmTelMsg.status);
      memset(&telgramMsg2.safetyData[1], 0, sizeof(telgramMsg2.safetyData) - 1U);

      vfwCpyFromRawBuffer(&buffer, &(telgramMsg2.safetyData[0]), sizeof(telgramMsg2.safetyData));

      populate = telegramValid ? 1U : 0U;
      vfwPutU8(&buffer, populate);
      populate = (ATP::IO::NoFailureA == telegramStatus) ? 0U : 1U;
      vfwPutU8(&buffer, populate);

      vfwPutU32(&buffer, safetyTimeStamp);

      //write the Message status4
      vfwPutU8(&buffer, simulatedBtmTelMsg.status);

      vfwCpyFromRawBuffer(&buffer, &(telgramMsg2.safetyData[0]), sizeof(telgramMsg2.safetyData));

      populate = telegramValid ? 1U : 0U;
      vfwPutU8(&buffer, populate);
      populate = (ATP::IO::NoFailureA == telegramStatus) ? 0U : 1U;
      vfwPutU8(&buffer, populate);

      vfwPutU32(&buffer, safetyTimeStamp);

      //write the Message status5
      vfwPutU8(&buffer, simulatedBtmTelMsg.status);

      vfwCpyFromRawBuffer(&buffer, &(telgramMsg2.safetyData[0]), sizeof(telgramMsg2.safetyData));

      populate = telegramValid ? 1U : 0U;
      vfwPutU8(&buffer, populate);
      populate = (ATP::IO::NoFailureA == telegramStatus) ? 0U : 1U;
      vfwPutU8(&buffer, populate);

      vfwPutU32(&buffer, safetyTimeStamp);
    }


    /******************************************************************************
    * Pack the Messages
    ******************************************************************************/
    void OPCSim::pack()
    {
      const uint32_t referenceTimeSafetyTimeStamp = static_cast<uint32_t>(vfwGetReferenceTime()) + refTimeOffset;

      //boolean for danger for shunting packet
      static bool dangerForShuntingPacket = false;
      
      // Packing of Time is done in writeToChannel() itself.
      
      // Status Message Packet (10 bytes + 6 bytes)
      statusMsg.safetyData[0] = simulatedBtmStatusMsg.msgData.enableTelegram;
      statusMsg.safetyData[1] = simulatedBtmStatusMsg.msgData.safetyMode;

      if (holdTemporaryBsaValueCounter != 0U)
      {
        --holdTemporaryBsaValueCounter;

        if (holdTemporaryBsaValueCounter == 0U)
        {
          simulatedBtmStatusMsg.msgData.bsa = (simulatedBtmStatusMsg.msgData.telePowerStatus == 1U) ? btmBaliseServiceAvailable : bsaSporadicFailure;
          ++simulatedBtmStatusMsg.msgData.bsaCounter;
        }
      }

      statusMsg.safetyData[2] = simulatedBtmStatusMsg.msgData.bsa;
      statusMsg.safetyData[3] = simulatedBtmStatusMsg.msgData.bsaCounter;
      statusMsg.safetyData[4] = simulatedBtmStatusMsg.msgData.prelTestStatus << 5 |
        simulatedBtmStatusMsg.msgData.ifKActive << 4 |
        simulatedBtmStatusMsg.msgData.usedIfBtmId << 2 |
        simulatedBtmStatusMsg.msgData.telePowerStatus;
      statusMsg.safetyData[5] = simulatedBtmStatusMsg.msgData.statusandOption;
      statusMsg.safetyData[5] |= ((setBalisePresent ? 0x01 : 0x00) << 6);
      statusMsg.safetyData[6] = simulatedBtmStatusMsg.msgData.usedProtocolVersion;
      statusMsg.safetyData[7] = 0U; //unused
      statusMsg.safetyData[8] = 0U; //unused 
      statusMsg.safetyData[9] = 0U; //unused
      telegramStatus = ATP::IO::NoFailureA;

      safetyTimeStamp = referenceTimeSafetyTimeStamp;

      //Temporary Balise information telegram 
      uint8_t baliseInfoTelegram[11] = { 0U, 0U, 0U, 0U, 0U, 0U, 0U, 0U, 0U, 0U, 0U };
      //calculate balise information 
      baliseInfoTelegram[0] = (((headerQUpdown & 0x01U) << 7) | (headerMVer & 0x7fU));
      baliseInfoTelegram[1] = (((headerQMedia & 0x01U) << 7) | (headerNPig & 0x07U) << 4 | 
        ((headerNTotal & 0x07U) << 1) | (headerMDup & 0x02U) >> 1);
      baliseInfoTelegram[2] = (((headerMDup & 0x01U) << 7) | (headerMCount & 0xfeU) >> 1);
      baliseInfoTelegram[3] = (((headerMCount & 0x01U) << 7) | (headerNidC & 0x3f8U) >> 3);
      baliseInfoTelegram[4] = (((headerNidC & 0x07U)) << 5 | (baliseObj.nextExpectedBaliseId & 0x3e00U) >> 9);
      baliseInfoTelegram[5] = static_cast<uint8_t>((baliseObj.nextExpectedBaliseId & 0x1feU) >> 1);

      if (!dangerForShuntingPacket)
      {//Normal operation
        baliseInfoTelegram[6] = (((baliseObj.nextExpectedBaliseId & 0x01U) << 7) | ((headerQLink & 0x01U) << 6) | (endInfoNidPacket & 0xfcU) >> 2);
        baliseInfoTelegram[7] = ((endInfoNidPacket & 0x03U) << 6);
      }
      else
      { //packet for danger for shunting
        baliseInfoTelegram[6] = (((baliseObj.nextExpectedBaliseId & 0x01U) << 7) | ((headerQLink & 0x01U) << 6) | (shInfoNidPacket & 0xfcU) >> 2);
        baliseInfoTelegram[7] = (((shInfoNidPacket & 0x03U) << 6) | ((shInfoQDir & 0x03U) << 4) | (shInfoLPacket & 0x1e00U) >> 9);
        baliseInfoTelegram[8] = ((shInfoLPacket & 0x1feU) >> 1);
        baliseInfoTelegram[9] = (((shInfoLPacket & 0x1U) << 7) | ((shInfoQAspect & 0x1U) << 6) | ((endInfoNidPacket & 0xfcU) >> 2));
        baliseInfoTelegram[10] = ((endInfoNidPacket & 0x03U) << 6);
      }

      VFW_Buffer messageBuffer;
      vfwInitBuffer(&messageBuffer, &telgramMsg.safetyData[0], sizeof(telgramMsg.safetyData));

      //Telegram Message Packet ( simulatedBtmStatusMsg.msgData.telePowerStatus;26 bytes + 8 bytes)
      vfwPutU8(&messageBuffer, simulatedBtmTelMsg.msgData1.sequenceNo);
      vfwPutU8(&messageBuffer, btmValid830Telegram);

      vfwPutU32(&messageBuffer, simulatedBtmTelMsg.msgData1.beginTimeStamp);
      vfwPutU32(&messageBuffer, simulatedBtmTelMsg.msgData1.endTimeStamp);
      vfwPutU32(&messageBuffer, simulatedBtmTelMsg.msgData1.telegramTimeStamp);
      //balise number
      vfwPutU8(&messageBuffer, simulatedBtmTelMsg.msgData1.baliseNumber);

      //Unused in Simulation
      vfwPutU8(&messageBuffer, 0U);
      vfwPutU8(&messageBuffer, 0U);
      //final report
      vfwPutU8(&messageBuffer, simulatedBtmTelMsg.msgData1.finalReport);
      //Unused in Simulation
      vfwPutU8(&messageBuffer, 0U);
      vfwPutU8(&messageBuffer, 0U);
      vfwPutU8(&messageBuffer, 0U);
      vfwPutU8(&messageBuffer, 0U);
      //update balise information of packet from port 1
      vfwCpyFromRawBuffer(&messageBuffer, &baliseInfoTelegram[0], 4U);

      //telegram status 
      telegramStatus = ATP::IO::NoFailureA;
      //Telegram Message from port 2-5 
      telgramMsg2.safetyData[0] = simulatedBtmTelMsg.msgData1.sequenceNo;
      //counter for telegram and message from port 2
      uint8_t telegramCount = 4U;
      uint8_t msgCount = 1U;

      for (; telegramCount <= 10U; telegramCount++)
      {
        telgramMsg2.safetyData[msgCount] = baliseInfoTelegram[telegramCount];
        ++msgCount;
      }
      //unused
      telgramMsg2.safetyData[8] = 0U;
      telgramMsg2.safetyData[9] = 0U;
      telgramMsg2.safetyData[10] = 0U;
      telgramMsg2.safetyData[11] = 0U;
      telgramMsg2.safetyData[12] = 0U;
      telgramMsg2.safetyData[13] = 0U;
      telgramMsg2.safetyData[14] = 0U;
      telgramMsg2.safetyData[15] = 0U;
      telgramMsg2.safetyData[16] = 0U;
      telgramMsg2.safetyData[17] = 0U;
      telgramMsg2.safetyData[18] = 0U;
      telgramMsg2.safetyData[19] = 0U;
      telgramMsg2.safetyData[20] = 0U;
      telgramMsg2.safetyData[21] = 0U;
      telgramMsg2.safetyData[22] = 0U;
      telgramMsg2.safetyData[23] = 0U;
      telgramMsg2.safetyData[24] = 0U;
      telgramMsg2.safetyData[25] = 0U;

    }


    /******************************************************************************
    * Verify and simulate the balise passage
    ******************************************************************************/
    void OPCSim::verifyBalisePassage()
    {
      ATP::TravelDir odoDir = ATP::Pos::AbstractOdometry::corePtr()->getOdoDirection();
      // Update odo direction only if it is fwd or rev
      if ((odoDir == ATP::DirForward) || (odoDir == ATP::DirReverse))
      {
        // Check if odometer direction has changed.
        if (odoDir != oldOdoDir)
        {
          // Reset the values of next expected balise if the direction has changed
          previousExpectedBaliseId = 0U;
          baliseObj.nextExpectedBaliseId = 0U;
          trace.write(3U, "Direction changed to", odoDir);
          trace.write(3U, "Cleared expected balise");
          startOdoFlag = false;
          stopOdoFlag = false;
          updateBaliseObj = true;
          oldOdoDir = odoDir;
        }
      }

      // Check that next balise is present in balise-list -> If not update the next expected balise.
      if (!updateBaliseObj)
      {
        if ((baliseObj.nextExpectedBaliseId != 0U) && (baliseObj.nextExpectedBaliseId != initialBaliseNo))
        {
          // Didn't find it? -> Then update next expected balise.
          if (ATP::DS::AbstractTracks::corePtr()->getBalise(baliseObj.nextExpectedBaliseId) == static_cast<const ATP::DS::Balise*>(NULL))
          {
            trace.write(3U, "Cleared expected balise", baliseObj.nextExpectedBaliseId);
            previousExpectedBaliseId = 0U;
            baliseObj.nextExpectedBaliseId = 0U;
            startOdoFlag = false;
            stopOdoFlag = false;
            updateBaliseObj = true;
          }
        }
      }

      //reset the previous expected balise id if train is idling.
      if (ATP::Kernel::AbstractModeControl::corePtr()->getIdleState())
      {
        previousExpectedBaliseId = 0U;
      }

      currentReceivedOdo = ATP::Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo();

      if (updateBaliseObj)
      {
        if (ATP::ATPModeBaliseSearch == ATP::Kernel::AbstractModeControl::corePtr()->getCurrentMode())
        { 
          ATP::Kernel::BaliseSearchModeState baliseSubMode = ATP::Kernel::AbstractModeControl::corePtr()->getBaliseSearchModeState();
          //set first balise, when the state of Balise Search mode to wait for balise for new registration or re-registration
          if ((ATP::Kernel::BaliseSearchMode::baliseSearchWaitForBaliseReg == baliseSubMode) || 
            (ATP::Kernel::BaliseSearchMode::baliseSearchWaitForBaliseReReg == baliseSubMode))
          {
            //Set the next detected Balise if not set and the odometer direction is the same as supposed direction
            if ((baliseObj.nextExpectedBaliseId == 0U) && (odoDir == ATP::DS::AbstractTargets::corePtr()->getSupposedTravelDir()))
            {
              baliseObj.nextExpectedBaliseId = initialBaliseNo;
              trace.write(3U, "New expected initial balise ", baliseObj.nextExpectedBaliseId);
              previousExpectedBaliseId = 0U;
              //Check for direction of train
              if (ATP::DirForward == (ATP::DS::AbstractTargets::corePtr()->getSupposedTravelDir()))
              {
                //Set the Initial Balise Odo
                baliseObj.nextExpectedBaliseOdo = currentReceivedOdo + initialBaliseOdo;
                //calculate the Odo at limits of balise detection range in case of  Balise Search ATP mode 
                baliseObj.odoAtStartDistance = baliseObj.nextExpectedBaliseOdo - baliseDetectionRange;
                baliseObj.odoAtEndDistance = baliseObj.nextExpectedBaliseOdo + baliseDetectionRange;
                trace.write(3U, "New expected balise at pos ", baliseObj.nextExpectedBaliseOdo);
                updateBaliseObj = false;
              }
              else if (ATP::DirReverse == (ATP::DS::AbstractTargets::corePtr()->getSupposedTravelDir()))
              {
                //Set the Initial Balise Odo
                baliseObj.nextExpectedBaliseOdo = currentReceivedOdo - initialBaliseOdo;
                baliseObj.odoAtStartDistance = baliseObj.nextExpectedBaliseOdo - baliseDetectionRange;
                baliseObj.odoAtEndDistance = baliseObj.nextExpectedBaliseOdo + baliseDetectionRange;
                trace.write(3U, "New expected balise at pos ", baliseObj.nextExpectedBaliseOdo);
                updateBaliseObj = false;
              }
              else
              {
                // do nothing
              }
            }
          }//if the ATP mode is in balise search and sub mode is in waiting for 2nd balise or searching for balise in re-registration.
          else if (ATP::Kernel::BaliseSearchMode::baliseSearchWaitBalise2 == baliseSubMode)
          {
            getNextBaliseToDetect();
            if (initialBaliseNo == baliseObj.nextExpectedBaliseId)
            {
              startOdoFlag = false;
              stopOdoFlag = false;
              updateBaliseObj = true;
            }
          }
          else
          {
            //Do nothing
          }
        }
        else
        {
          //reset the values if position is unknown.
          //movement with unknown position where the balise need to be simulated is only in Balise search mode.
          //in all other cases there is no need to simulate the balises.
          //exception is Possession mode, but that is not handled in opc sim.
          if (ATP::Pos::AbstractPosition::corePtr()->getAccuracyState() == ATP::Pos::PosUnknown)
          {
            startOdoFlag = false;
            stopOdoFlag = false;
            updateBaliseObj = true;
            previousExpectedBaliseId = 0U;
            baliseObj.nextExpectedBaliseId = 0U;
          }
          else
          {
            getNextBaliseToDetect();
          }
        }
      } // end if (updateBaliseObj)

      if (baliseObj.nextExpectedBaliseId != 0U)
      {
        if (ATP::DirForward == odoDir)
        {
          //check if the current received Odo is greater than the Odo at start limit of balise detection range
          if ((currentReceivedOdo >= baliseObj.odoAtStartDistance) &&
            (!startOdoFlag) && (!updateBaliseObj))
          {
            simulatedBtmTelMsg.msgData1.beginTimeStamp = static_cast<uint32_t>(vfwGetReferenceTime()) + refTimeOffset;
            startOdoFlag = true;
            trace.write(4U, "Balise detection started");
          }

          //check if the current received Odo is greater than the Odo at end limit of balise detection range
          if ((currentReceivedOdo >= baliseObj.odoAtEndDistance) &&
            (!stopOdoFlag) && (!updateBaliseObj))
          {
            simulatedBtmTelMsg.msgData1.endTimeStamp = static_cast<uint32_t>(vfwGetReferenceTime()) + refTimeOffset;
            stopOdoFlag = true;
            trace.write(4U, "Balise detection complete");
          }
        }
        else if (ATP::DirReverse == odoDir)
        {
          //check if the current received Odo is greater than the Odo at start limit of balise detection range
          if ((currentReceivedOdo <= baliseObj.odoAtStartDistance) &&
            (!startOdoFlag) && (!updateBaliseObj))
          {
            simulatedBtmTelMsg.msgData1.beginTimeStamp = static_cast<uint32_t>(vfwGetReferenceTime()) + refTimeOffset;
            startOdoFlag = true;
            trace.write(4U, "Balise detection started");
          }

          //check if the current received Odo is greater than the Odo at end limit of balise detection range
          if ((currentReceivedOdo <= baliseObj.odoAtEndDistance) &&
            (!stopOdoFlag) && (!updateBaliseObj))
          {
            simulatedBtmTelMsg.msgData1.endTimeStamp = static_cast<uint32_t>(vfwGetReferenceTime()) + refTimeOffset;
            stopOdoFlag = true;
            trace.write(4U, "Balise detection complete");
          }
        }
        else
        {
          //do nothing
        }

        if (startOdoFlag && stopOdoFlag)
        {
          if (previousExpectedBaliseId != baliseObj.nextExpectedBaliseId)
          {
            previousExpectedBaliseId = baliseObj.nextExpectedBaliseId;
            //update the sequence number
            simulatedBtmTelMsg.msgData1.sequenceNo = simulatedBtmTelMsg.msgData1.sequenceNo + 1U;
            //update the balise number
            simulatedBtmTelMsg.msgData1.baliseNumber = simulatedBtmTelMsg.msgData1.baliseNumber + 1U;
            //Set the bit 6 of  status and option field of status message
            simulatedBtmStatusMsg.msgData.statusandOption |= 0x40U;
            //set the telegram time stamp
            simulatedBtmTelMsg.msgData1.telegramTimeStamp = static_cast<uint32_t>(vfwGetReferenceTime()) + refTimeOffset;
            //write to console
            trace.write(1U, "Simulated Balise Passage Balise ID: ", static_cast<uint32_t>(baliseObj.nextExpectedBaliseId));
          }

          startOdoFlag = false;
          stopOdoFlag = false;
          updateBaliseObj = true;
        }
      }
    }

    /******************************************************************************
     * To get next detected balise
     ******************************************************************************/
    void OPCSim::getNextBaliseToDetect()
    {

      if (updateBaliseObj)
      {
        //init the next expected balise to 0
        if (baliseObj.nextExpectedBaliseId != 0U)
        {
          trace.write(3U, "Cleared expected balise");
        }

        baliseObj.nextExpectedBaliseId = 0U;        

        //balise start iterator
        ATP::DS::AbstractTracks::ConstBaliseListIteratorType baliseItr = ATP::DS::AbstractTracks::corePtr()->getBaliseIter();
        //balise end iterator
        ATP::DS::AbstractTracks::ConstBaliseListIteratorType baliseItrEnd = ATP::DS::AbstractTracks::corePtr()->getBaliseIterEnd();

        ATP::TravelDir trackDir = ATP::DS::AbstractTracks::corePtr()->getTravelDirection();
        ATP::TravelDir odoDir = ATP::Pos::AbstractOdometry::corePtr()->getOdoDirection();


        //only if the directions are fwd or rev
        if (((trackDir == ATP::DirForward) || (trackDir == ATP::DirReverse)) &&
          ((odoDir == ATP::DirForward) || (odoDir == ATP::DirReverse)))
        {
          //if the odo direction is same as balise list direction
          if (trackDir == odoDir)
          {
            //iterate forward in the list.
            while (baliseItr != baliseItrEnd)
            {
              ATP::OdoPosition bOdo = (*baliseItr)->getOdoPosition();

              //if the balise odo is further ahead than current odo in the movement direction.
              if (((trackDir == ATP::DirForward) && (currentReceivedOdo < bOdo)) || ((trackDir == ATP::DirReverse) && (currentReceivedOdo > bOdo)))
              {
                //Set the Next detected balise to the balise ID present in the balise list
                baliseObj.nextExpectedBaliseId = (*baliseItr)->getBaliseId();
                baliseObj.nextExpectedBaliseOdo = (*baliseItr)->getOdoPosition();
                trace.write(3U, "New expected balise ", baliseObj.nextExpectedBaliseId);
                trace.write(3U, "New expected balise at pos ", baliseObj.nextExpectedBaliseOdo);
                updateBaliseObj = false;
                break;
              }

              ++baliseItr;
            }
          }
          //if the odo direction is opposite of balise list direction
          else
          {

            //if the balise list is not empty.
            if (baliseItr != baliseItrEnd)
            {
              baliseItr = baliseItrEnd;

              //iterate reverse in the list.
              do
              {
                --baliseItr;
                ATP::OdoPosition bOdo = (*baliseItr)->getOdoPosition();

                //if the balise odo is further ahead than current odo in the movement direction.
                if (((trackDir == ATP::DirForward) && (currentReceivedOdo >= bOdo))
                  || ((trackDir == ATP::DirReverse) && (currentReceivedOdo <= bOdo)))
                {
                  //Set the Next detected balise to the balise ID present in the balise list
                  baliseObj.nextExpectedBaliseId = (*baliseItr)->getBaliseId();
                  baliseObj.nextExpectedBaliseOdo = (*baliseItr)->getOdoPosition();
                  trace.write(3U, "New expected balise ", baliseObj.nextExpectedBaliseId);
                  trace.write(3U, "New expected balise at pos ", baliseObj.nextExpectedBaliseOdo);
                  updateBaliseObj = false;
                  break;
                }
              } while (baliseItr != ATP::DS::AbstractTracks::corePtr()->getBaliseIter());
            }
          }
        }

      }

      if (baliseObj.nextExpectedBaliseId != 0U)
      {
        switch (ATP::Pos::AbstractOdometry::corePtr()->getOdoDirection())
        {
        case ATP::DirForward:
          //calculate the Odo at limits of balise detection range
          baliseObj.odoAtStartDistance = baliseObj.nextExpectedBaliseOdo - baliseDetectionRange;
          baliseObj.odoAtEndDistance = baliseObj.nextExpectedBaliseOdo + baliseDetectionRange;
          break;

        case ATP::DirReverse:
          //calculate the Odo at limits of balise detection range
          baliseObj.odoAtStartDistance = baliseObj.nextExpectedBaliseOdo + baliseDetectionRange;
          baliseObj.odoAtEndDistance = baliseObj.nextExpectedBaliseOdo - baliseDetectionRange;
          break;

        default:
          //nothing to do
          break;
        }
      }
    }

    /******************************************************************************
    * instance of OPC Sim
    *
    * ******************************************************************************/
    OPCSim& OPCSim::instance()
    {
      static OPCSim theOnlyOPCSimInstance;
      return theOnlyOPCSimInstance;
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    OPCSim* OPCSim::corePtr()
    {
      return coreOPCSimPtr;
    }
    
    /*****************************************************************************
    * consoleCall
    *******************************************************************************/
    bool OPCSim::consoleCall(const uint32_t argc, const ConsoleArguments argv)
    {
      bool retVal = false;

      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        char_t helpText[] =
          "simOPC         Shows the OPC sim values\n"
          "balpres [on|off] Turns balise present on/off\n"
          "btmvalid [true|false] Turns btm valid flag true/false\n"
          "btmtgmform[int] sets the btm telegram format\n";

        ATC::AbstractConsole::corePtr()->write(&helpText[0]);
        retVal = false;
      }
      else if (ATC::isTextMatch(&argv[0][0], "simOPC", sizeof("simOPC")) && (argc >= 1U))
      {
        char_t buffer[100];
        const int32_t res = snprintf(&buffer[0], sizeof(buffer), "Registration balise ID: %d \nNext balise id: %d odo: %d ",
          initialBaliseNo, baliseObj.nextExpectedBaliseId, static_cast<int32_t>(baliseObj.nextExpectedBaliseOdo));

        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          retVal = true;
        }
      }
      else if (ATC::isTextMatch(&argv[0][0], "balpres", sizeof("balpres")) && (argc >= 1U))
      {
        char_t buffer[100];

        if (argc == 2U)
        { 
          setBalisePresent = ATC::isTextMatch(&argv[1][0], "on", sizeof("on"));
        }
        const int32_t res = snprintf(&buffer[0], sizeof(buffer),
          "balise present %d", setBalisePresent);

        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          retVal = true;
        }
      }
      else if (ATC::isTextMatch(&argv[0][0], "btmvalid", sizeof("btmvalid")) && (argc == 2U))
      {
        telegramValid = ATC::isTextMatch(&argv[1][0], "true", sizeof("true"));

        retVal = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "btmtgmform", sizeof("btmtgmform")) && (argc >= 1U))
      {
        char_t buffer[100];

        uint8_t temp = 0U;
        if (argc == 2U)
        {
          static_cast<void>(sscanf(&argv[1][0], "%hhu", &temp));
          simulatedBtmStatusMsg.msgData.enableTelegram = temp;
        }

        const int32_t res = snprintf(&buffer[0], sizeof(buffer),
          "btm telegram format %d", simulatedBtmStatusMsg.msgData.enableTelegram);

        if ((res > 0) && (static_cast<size_t>(res) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }
        retVal = true;
      }
      else
      {
        // do nothing
      }

      return retVal;
    }

    /*****************************************************************************
    * updateOdoValForBaliseObj
    *******************************************************************************/
    void OPCSim::updateOdoValForBaliseObj()
    {
      const ATP::OdoPosition odoOffsetCorrection = ATP::Pos::AbstractPosition::corePtr()->getOdometerOffsetCorrection();

      // If offset is updated --> Update the baliseobj to reflect the new odoOffset
      if (0 != odoOffsetCorrection)
      {
        baliseObj.nextExpectedBaliseOdo += odoOffsetCorrection;
        baliseObj.odoAtStartDistance += odoOffsetCorrection;
        baliseObj.odoAtEndDistance += odoOffsetCorrection;
      }
    }

  }
}
