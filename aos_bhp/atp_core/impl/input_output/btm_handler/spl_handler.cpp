/********************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The SPL Handler component deals with the interface between the OPC Agent and the AOS SW.
*  AbstractSPLHandler implements the core functionality of the component.
*
*******************************************************************************/

/********************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-12-05   rquensel    Created
********************************************************************************/

/*******************************************************************************
* LINT DIRECTIVES
*******************************************************************************/
//lint -esym(714,_IO*) Needed as glue towards the SPL library
//lint -esym(759,_IO*) Needed as glue towards the SPL library
//lint -esym(765,_IO*) Needed as glue towards the SPL library

/*******************************************************************************
* INCLUDE FILES
*******************************************************************************/
#include <cstdio>
#include "spl_handler.hpp"
#include "pl_driver_types.h"
#include "spl_a_errorcodes.h"
#include "atc_util.hpp"
#include "atc_types.hpp"
#include "abstract_btm_handler.hpp"
#include "abstract_config.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "cross_compare_array.hpp"
#include "abstract_log_handler.hpp"
#include "abstract_mode_control.hpp"
#include "spl_handler.h"
#include <vfw_identity.h>
#include "abstract_btm_handler_event_ids.hpp"
#include "dmi_event_codes.hpp"

#ifndef __GNUG__
// Its is not possible to include <vfw_time.h> in visual studio
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#endif


/********************************************************************************
* Declarations and Definitions
********************************************************************************/


namespace ATP
{
  namespace IO
  {
    // Configuration data for the SPL connection
    const SPL_P2P_Protocol_Parameters p2pCfg =
    {
      senderDynamicTransferTime,
      senderStaticTransferTime,
      dynamicBusTransferTime,
      staticBusTransferTime,
      connectionSetupTimeLimit,
      connectConfirmTimeout,
      secondIncorrectReceiveTimeout
    };

    /*******************************************************************************
    * SplHandler constructor
    *******************************************************************************/
    SplHandler::SplHandler()
      :
      eventErrorOpcSupervisionFailed(ATC::Event::createSafetyHaltEvent(atpOPCHandlerId, ATC::CoreContainer, eventIdErrorOpcSupervisionFailed,
        ATC::NoEB, DMICom::opcFailed, "OPC Supervision failed!")),
      splConnection(static_cast<SPL_Connection>(NULL)),
      opcSplState(OPC_SPL_CLOCK_SYNCHRONIZING),
      opcToAtpSap19SyncChannel(static_cast<VFW_SyncChannel>(NULL)),
      opcToAtpAppStatusSyncChannel(static_cast<VFW_SyncChannel>(NULL)),
      tigrisOffsetSyncChannel(static_cast<VFW_SyncChannel>(NULL)),
      opcAppStatus(appStatusIdle),
      lifesignStatus(lifesignStatusIdle),
      lifesignTimestamp(0U),
      opcSplSendClockSyncCounter(0U),
      tigisOffsetReceivedCycleCounter(0U),
      currentTigrisTimeOffset(ATC::int32Max)
    {
      // Initialize buffer for outgoing SPL-messages...
      vfwInitBuffer(&splOutBuffer, &splOutRawBuffer[0], maxSplDataSize);
      memset(&splOutRawBuffer[0], 0, maxSplDataSize);

      // Initialize buffer for incoming SPL-messages...
      vfwInitBuffer(&udpInputBuffer, &udpInputRawBuffer[0], sizeof(udpInputRawBuffer));
      memset(&udpInputRawBuffer[0], 0, sizeof(udpInputRawBuffer));
      vfwSetFullBuffer(&udpInputBuffer);

      memset(&opcVersionString, 0, sizeof(opcVersionString));
    }

    /*******************************************************************************
    * instance
    *******************************************************************************/
    SplHandler& SplHandler::instance()
    {
      static SplHandler splHandlerInstance;

      return splHandlerInstance;
    }

    /*******************************************************************************
    * preInit
    *******************************************************************************/
    void SplHandler::preInit()
    {
      VFW_ChannelDesc opcToAtpChannel = static_cast<VFW_ChannelDesc>(NULL);
      VFW_ChannelDesc opcToAtpAppStatusChannel = static_cast<VFW_ChannelDesc>(NULL);
      VFW_ChannelDesc tigrisOffsetChannel = static_cast<VFW_ChannelDesc>(NULL);

#if defined (__HIL) || defined (__EMD)
      const bool divideMessage = false;
#else
      const bool divideMessage = true;
#endif
                                                                                                                                                     
      if (vfwGetSide() == VFW_A_SIDE)
      {
        opcToAtpChannel = vfwChannelOpenRead(ATC::btmhChannelDispToATPA, ATC::splQueueSize,
          ATC::splMessageSizeOpcToAtp, ATC::btmhChannelDispToATPA);

        opcToAtpAppStatusChannel = vfwChannelOpenRead(ATC::btmhChannelDispAppDataToATPA, ATC::appDataQueueSize,
          ATC::appDataMessageSize, ATC::btmhChannelDispAppDataToATPA);

        tigrisOffsetChannel = vfwChannelOpenRead(ATC::tigrisOffsetChannelNameA, ATC::tigrisOffsetQueueSize,
          ATC::tigrisOffsetMessageSize, ATC::tigrisOffsetChannelNameA);

        atpToOpcSap19OutputChannel.initChannel(ATC::btmhChannelATPAToDisp, ATC::splQueueSize,
          ATC::splMessageSizeAtpToOpc, divideMessage);

        atpToOpcClockSyncOutputChannel.initChannel(ATC::opcClockSyncChannelATPAToDisp, ATC::splQueueSize,
          ATC::splMessageSizeAtpToOpc, divideMessage);
      }
      else if (vfwGetSide() == VFW_B_SIDE)
      {
        opcToAtpChannel = vfwChannelOpenRead(ATC::btmhChannelDispToATPB, ATC::splQueueSize,
          ATC::splMessageSizeOpcToAtp, ATC::btmhChannelDispToATPB);

        opcToAtpAppStatusChannel = vfwChannelOpenRead(ATC::btmhChannelDispAppDataToATPB, ATC::appDataQueueSize,
          ATC::appDataMessageSize, ATC::btmhChannelDispAppDataToATPB);

        tigrisOffsetChannel = vfwChannelOpenRead(ATC::tigrisOffsetChannelNameB, ATC::tigrisOffsetQueueSize,
          ATC::tigrisOffsetMessageSize, ATC::tigrisOffsetChannelNameB);

        atpToOpcSap19OutputChannel.initChannel(ATC::btmhChannelATPBToDisp, ATC::splQueueSize,
          ATC::splMessageSizeAtpToOpc, divideMessage);

        atpToOpcClockSyncOutputChannel.initChannel(ATC::opcClockSyncChannelATPBToDisp, ATC::splQueueSize,
          ATC::splMessageSizeAtpToOpc, divideMessage);
      }
      else
      {
        ATC::aosHalt(__FILE__, __LINE__, "Incorrect side set!");
      }

      if ((opcToAtpChannel          != static_cast<VFW_ChannelDesc>(NULL)) &&
          (opcToAtpAppStatusChannel != static_cast<VFW_ChannelDesc>(NULL)) &&
          (tigrisOffsetChannel      != static_cast<VFW_ChannelDesc>(NULL))   )
      {
        // Synchronize with diversified channel (A/B)
        opcToAtpSap19SyncChannel = vfwSyncAddChannel(opcToAtpChannel, ATC::trueVfw);
        opcToAtpAppStatusSyncChannel = vfwSyncAddChannel(opcToAtpAppStatusChannel, ATC::trueVfw);
        tigrisOffsetSyncChannel = vfwSyncAddChannel(tigrisOffsetChannel, ATC::trueVfw);

        // Not event-driven, cyclic polled
        vfwSyncChannelDeactivate(opcToAtpSap19SyncChannel);
        vfwSyncChannelDeactivate(opcToAtpAppStatusSyncChannel);
      }
      else
      {
        ATC::aosHalt(__FILE__, __LINE__, "Failed to open OPC channels");
      }
    }


    /*******************************************************************************
    * init
    *******************************************************************************/
    void SplHandler::init()
    {
      initCrossCompare();
      initProfibus();
      initReferenceClock();
      initSplConnection();
    }

    /*******************************************************************************
    * initCrossCompare
    *******************************************************************************/
    void SplHandler::initCrossCompare() const
    {
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<OpcSplState>(&opcSplState));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&opcAppStatus));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&opcSplSendClockSyncCounter));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&tigisOffsetReceivedCycleCounter));
      crossCompare->addCrossCompareData(new Support::CrossCompareInt32(&currentTigrisTimeOffset));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventErrorOpcSupervisionFailed));
      crossCompare->addCrossCompareData(new Support::CrossCompareArray<char_t>(&opcVersionString.versionString[0],
        static_cast<uint16_t>(sizeof(opcVersionString))));
    }

    /*******************************************************************************
    * initProfibus
    *******************************************************************************/
    void SplHandler::initProfibus()
    {
      PL_Driver_Initialisation_Struct driverInitStruct;
      driverInitStruct.TS = ourProfibusAdress;
      driverInitStruct.Baud_Rate = PL_Driver_Initialisation_Struct::PL_BR_1_500_000; // Preferred value in FFFIS/ERTMS application
      driverInitStruct.Min_T_SDR = minimumStationDelayOfResponders;
      driverInitStruct.Max_T_SDR = maximumStationDelayOfResponders;
      driverInitStruct.T_SL = slotTime;
      driverInitStruct.T_QUI = quietTime;
      driverInitStruct.T_SET = setupTime;
      driverInitStruct.T_TR = timeTargetRotation;
      driverInitStruct.GAP = gapActualizationFactor;
      driverInitStruct.HSA = highestStationAddress;
      driverInitStruct.Max_Retry_Limit = maxRetryLimit;

      // Using ATPCU driver in R4 new platform
      const uint32_t splDriverType = SPL_Driver_ATPCU;

      int32_t splResult = SPL_INIT_OK;

      // Initialize the profibus library
      static_cast<void>(SPL_Initialise(ourProfibusAdress,
        splDriverType,
        &driverInitStruct,
        &splResult, tokenValue));

      if (splResult != SPL_INIT_OK)
      {
        ATC::debugInfo("SPL did not initialize.\n");
        opcSplState = OPC_SPL_FAILED;
      }
      else
      {
        // OK we did initialize!
      }
    }

    /*******************************************************************************
    * initReferenceClock
    *******************************************************************************/
    void SplHandler::initReferenceClock() const
    {
      int32_t splResult = SPL_INIT_OK;

      static_cast<void>(SPL_Reference_Clock_Master(
        phase1Frequency,
        phase1Duration,
        phase2Frequency,
        localClockInaccuracy,
        clockSapId,  // SAP
        &splResult,
        tokenValue));

      if (splResult != SPL_CLOCK_OK)
      {
        ATC::debugInfo("SPL reference clock not initialized.\n");
      }
      else
      {
        // OK
      }
    }

    /*******************************************************************************
    * initSplConnection
    *******************************************************************************/
    void SplHandler::initSplConnection()
    {
      const uint8_t profibusConnectionName[] = "ATP-OPC";

      static_cast<void>(SPL_Create_P2P(
        &profibusConnectionName[0],
        opcProfibusAddress,
        mvbSapId,
        splMaster,
        static_cast<uint32_t>(SPL_SAFETY_LEVEL_HIGH_SAFE), // Safety level 4
        idleTime,
        &p2pCfg,
        &splConnection,
        tokenValue));

      if (splConnection == static_cast<SPL_Connection>(NULL))
      {
        ATC::aosHalt(__FILE__, __LINE__, "SPL connection not initialized.");
      }
    }

    /*******************************************************************************
    * runSplConnectedIn
    *******************************************************************************/
    void SplHandler::runSplConnectedIn()
    {
      bool done = false;

      if (tigisOffsetReceivedCycleCounter >= tigrisOffsetCycleCounter)
      {
        opcSplState = OPC_SPL_FAILED;
      }
      else
      {
        ++tigisOffsetReceivedCycleCounter;
      }
      
      while (!done)
      {
        int32_t activeResult = 0;
        uint8_t profibusBuffer[maxSplDataSize];

        //lint -e{970} The external function SPL_recv() uses unsigned long
        static_cast<void>(
          SPL_recv(
            splConnection,
            &profibusBuffer[0],
            maxSplDataSize,
            static_cast<unsigned long*>(NULL),
            &activeResult,
            tokenValue));

        if (activeResult > 0)
        {
          // We have received a buffer, take care of it...
          handleMvbInput(&profibusBuffer[0], static_cast<uint32_t>(activeResult));
        }
        else
        {
          done = true;
          if (activeResult < 0)
          {
            // Oops, things have gone wrong!
            ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "SPL recv failed.", "SPL",__FILE__, __LINE__);
            opcSplState = OPC_SPL_FAILED;
          }
        }
      }

      if (opcIsSystemRunning())
      {
        const uint32_t timeStamp = static_cast<uint32_t>(vfwGetReferenceTime());
        const uint32_t timeoutTime = lifesignTimestamp + lifeSignTimeoutValue;
        if (timeStamp > timeoutTime)
        {
          ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "OPC lifesign failed ", timeStamp - timeoutTime, "SPL",__FILE__, __LINE__);
          opcSplState = OPC_SPL_FAILED;
          ATC::AbstractEventHandler::corePtr()->reportEvent(eventErrorOpcSupervisionFailed, __FILE__, __LINE__);
        }
      }

    }

    /*******************************************************************************
    * packLifeSignPacket
    *******************************************************************************/
    void SplHandler::packLifeSignPacket()
    {
      vfwPutU16(&splOutBuffer, nidLifesign);           // NID_ATP_PACKET
      vfwPutU16(&splOutBuffer, packetLengthLifesign);  // L_ATP_PACKET
      vfwPutU8(&splOutBuffer, atpLifeSignUnitId);      // Lifesign_unit

      const ATPMode mode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      uint8_t lifeSign;

      if (mode == ATPModeSafetyHalt)
      {
        lifeSign = lifesignStatusUnconditionalStoppingFailure;
      }
      else
      {
        lifeSign = lifesignStatusRunning;
      }
      
      vfwPutU8(&splOutBuffer, lifeSign);  // Lifesign_status
      vfwPutU32(&splOutBuffer, static_cast<uint32_t>(vfwGetReferenceTime()));           // TimeStmp
      vfwPutU32(&splOutBuffer, 0U);                    // RefTimeOffset, always 0 in direction ATP-OPC
    }

    /*******************************************************************************
    * packVersionRequest
    *******************************************************************************/
    void SplHandler::packVersionRequest()
    {
      vfwPutU16(&splOutBuffer, nidVersionRequest);           // NID_ATP_PACKET
      vfwPutU16(&splOutBuffer, packetLengthVersionRequest);  // L_ATP_PACKET
    }


    /*******************************************************************************
    * packAtpServiceCalendarTime
    *******************************************************************************/
    void SplHandler::packAtpServiceCalendarTime(const uint32_t calendarTime)
    {
      vfwPutU16(&splOutBuffer, nidAtpService);           // NID_ATP_PACKET
      vfwPutU16(&splOutBuffer, packetLengthAtpService);  // L_ATP_PACKET

      vfwPutU8(&splOutBuffer, atpServiceSourceUnitDmi);  // Source unit
      vfwPutU8(&splOutBuffer, atpServiceNumberCalendarTime);  // Source number
      vfwPutU8(&splOutBuffer, atpServiceActionCalendarTime);  // Service Action
      vfwPutU32(&splOutBuffer, calendarTime);  // Optional Data
    }


    /*******************************************************************************
    * packBtmhToBtmPacket
    *******************************************************************************/
    void SplHandler::packBtmhToBtmPacket(const IO::GP_10ByteVitalSourceDataA& btmCommand)
    {
      // Check status first? Can only be sent when connected...
      vfwPutU16(&splOutBuffer, nidBtmH2Btm); // NID_ATP_PACKET
      vfwPutU16(&splOutBuffer, packetLengthBtmH2Btm); // L_ATP_PACKET
      vfwPutU8(&splOutBuffer, nidBtm1); // NID_BTM
      vfwPutU8(&splOutBuffer, qItemStatusSet); // Q_ITEM_STATUS, It is always set
      vfwCpyFromRawBuffer(&splOutBuffer, &btmCommand.safetyData[0], sizeof(btmCommand.safetyData));
    }

    /*******************************************************************************
    * packOdohToOpcPacket
    *******************************************************************************/
    void SplHandler::packOdohToOpcPacket(const IO::GP_32ByteVitalSourceDataA & btmCommand)
    {
      vfwPutU16(&splOutBuffer, nidOdoH2Opc);
      vfwPutU16(&splOutBuffer, packetLengthOdoH2Opc);
      vfwCpyFromRawBuffer(&splOutBuffer, &btmCommand.safetyData[0], sizeof(btmCommand.safetyData));
    }

    /*******************************************************************************
    * isConnected
    *******************************************************************************/
    bool SplHandler::isConnected() const
    {
      return (opcSplState == OPC_SPL_CONNECTED);
    }

    /*******************************************************************************
    * opcIsSystemRunning
    *******************************************************************************/
    bool SplHandler::opcIsSystemRunning() const
    {
      return (lifesignStatus >= lifesignStatusRunning);
    }

    /*******************************************************************************
    * getOpcVersionString
    *******************************************************************************/
    const OpcVersionString& SplHandler::getOpcVersionString() const
    {
      return opcVersionString;
    }

    /*******************************************************************************
    * sendSplOutput
    *******************************************************************************/
    void SplHandler::sendSplOutput()
    {
      const uint32_t splOutputSize = vfwGetValidSize(&splOutBuffer);

      if (splOutputSize > 0U)
      {
        int32_t activeResult = 0;

        static_cast<void>(SPL_send(splConnection,
          &splOutRawBuffer[0],
          splOutputSize,
          &activeResult,
          tokenValue));

        if (activeResult != SPL_SEND_OK)
        {
          // Oops, things have gone wrong!
          ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "SPL send failed.", "SPL",__FILE__, __LINE__);
          opcSplState = OPC_SPL_FAILED;
        }

        vfwClearBuffer(&splOutBuffer);
      }
    }

    /*******************************************************************************
    * handleOpcSplClockSynchronizing
    *******************************************************************************/
    void SplHandler::handleOpcSplClockSynchronizing(const int32_t clockState)
    {
      if (clockState == SPL_CLOCK_SYNCHRONIZING)
      {
        // Keep waiting to be synchronized
      }
      else if (clockState == SPL_CLOCK_SYNCHRONIZED)
      {
        // Now our connection is closed
        opcSplState = OPC_SPL_WAIT_FOR_LEAVING_IDLE;
      }
      else
      {
        // Oops, things have gone wrong!
        ATC::debugInfo("SPL Clock Synchronizing failed.\n");
        opcSplState = OPC_SPL_FAILED;
      }
    }

    /*******************************************************************************
    * handleOpcSplWaitForLeavingIdlesetOpcReferenceTimeOffset
    *******************************************************************************/
    void SplHandler::handleOpcSplWaitForLeavingIdle(const int32_t clockState)
    {
      if (clockState != SPL_CLOCK_SYNCHRONIZED)
      {
        // Oops, things have gone wrong!
        opcSplState = OPC_SPL_FAILED;
      }
      else if (opcAppStatus != appStatusIdle)
      {
        opcSplState = OPC_SPL_SEND_CLOCK_SYNC;
      }
      else
      {
        // Wait for leaving idle...
      }
    }

    /*******************************************************************************
    * handleOpcSplSendClockSync
    *******************************************************************************/
    void SplHandler::handleOpcSplSendClockSync(const int32_t clockState)
    {
      if (clockState != SPL_CLOCK_SYNCHRONIZED)
      {
        // Oops, things have gone wrong!
        opcSplState = OPC_SPL_FAILED;
      }
      else if (opcSplSendClockSyncCounter > opcSplSendConnectCounterDelay)
      {
        int32_t activeResult = 0;
        static_cast<void>(SPL_IsActive(splConnection, cycleTime,
          &activeResult, tokenValue));

        if (activeResult == SPL_IS_CLOSED)
        {
          static_cast<void>(SPL_Connect(splConnection, &activeResult, tokenValue));

          if (activeResult == SPL_CONNECT_OK)
          {
            //  It does *NOT* imply that the connection is Connected, this must be checked with the SPL_IsActive function
            ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::DetailedLog, "SPL trying to connect.", "SPL",__FILE__, __LINE__);
            opcSplState = OPC_SPL_CONNECTING;
          }
          else
          {
            // Oops, things have gone wrong!
            opcSplState = OPC_SPL_FAILED;
          }
        }
        else
        {
          // Oops, things have gone wrong, state should be closed here!
          opcSplState = OPC_SPL_FAILED;
        }
      }
      else
      {
        // Wait until we have sent 20 clock syncs.
      }
    }

    /*******************************************************************************
    * handleOpcSplConnecting
    *******************************************************************************/
    void SplHandler::handleOpcSplConnecting(const int32_t clockState)
    {
      if (clockState != SPL_CLOCK_SYNCHRONIZED)
      {
        // Oops, things have gone wrong!
        opcSplState = OPC_SPL_FAILED;
      }
      else
      {
        int32_t activeResult = 0;
        static_cast<void>(SPL_IsActive(splConnection, cycleTime,
          &activeResult, tokenValue));

        if (activeResult == SPL_IS_CONNECTING)
        {
          // Just wait...
        }
        else if (activeResult == SPL_IS_CONNECTED)
        {
          // Now we are connected
          opcSplState = OPC_SPL_CONNECTED;
          packVersionRequest(); // Since we now are connected, send the version request.
          uint32_t currentTime = 0U;
          ATC::getUTCTime(currentTime);
          packAtpServiceCalendarTime(currentTime); // Send the calendar time to the BTM...
          runSplConnectedIn();
          ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "SPL is connected.", "SPL",__FILE__, __LINE__);
        }
        else
        {
          // Oops, things have gone wrong!
          ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "SPL connect failed.", "SPL",__FILE__, __LINE__);
          opcSplState = OPC_SPL_FAILED;
        }
      }
    }



    /*******************************************************************************
    * handleOpcSplConnected
    *******************************************************************************/
    void SplHandler::handleOpcSplConnected(const int32_t clockState)
    {
      if (clockState != SPL_CLOCK_SYNCHRONIZED)
      {
        // Oops, things have gone wrong!
        opcSplState = OPC_SPL_FAILED;
      }
      else
      {
        int32_t activeResult = 0;
        static_cast<void>(SPL_IsActive(splConnection, cycleTime,
          &activeResult, tokenValue));

        if (activeResult == SPL_IS_CONNECTED)
        {
          runSplConnectedIn();
        }
        else
        {
          // Oops, things have gone wrong!
          opcSplState = OPC_SPL_FAILED;
          ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "SPL disconnected, SPL_IsActive returned = ",
            activeResult, "SPL",__FILE__, __LINE__);
        }
      }
    }


    /*******************************************************************************
    * handleMvbInput
    *******************************************************************************/
    void SplHandler::handleMvbInput(uint8_t* const rawBuffer, const uint32_t messageSize)
    {
      VFW_Buffer mvbBuffer;
      vfwInitBuffer(&mvbBuffer, rawBuffer, messageSize);
      vfwSetReadBuffer(&mvbBuffer, messageSize);
      bool done = false;

      while (!done)
      {
        const uint32_t sizeLeft = vfwGetValidSize(&mvbBuffer);
        const uint16_t nidPacket = vfwGetU16(&mvbBuffer);
        const uint32_t lPacket = vfwGetU16(&mvbBuffer);

        if (lPacket > sizeLeft)
        {
          // Error!
          done = true;
          ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "SPL incorrect packet length received: ",
            static_cast<uint32_t>(nidPacket), "SPL",__FILE__, __LINE__);
        }
        else
        {
          // Create a buffer for our packet...
          VFW_Buffer packetBuffer;
          vfwInitSubBuffer(&packetBuffer, &mvbBuffer, lPacket - 4U); // NID_PACKET + L_PACKET (4 Bytes)

          switch (nidPacket)
          {
          case nidLifesign:
            if (lPacket == packetLengthLifesign)
            {
              handleLifeSign(packetBuffer);
            }
            else
            {
              done = true;
            }
            break;

          case nidBtmTgm2BtmH:
            if (lPacket == packetLengthBtmTgm2BtmH)
            {
              if (opcIsSystemRunning())
              {
                AbstractBTMHandler::corePtr()->processTelegramMessage(packetBuffer);
              }
            }
            else
            {
              done = true;
            }
            break;

          case nidBtmStatus2BtmH:
            if (lPacket == packetLengthBtmStatus2BtmH)
            {
              if (opcIsSystemRunning())
              {
                AbstractBTMHandler::corePtr()->processStatusMessage(packetBuffer);
              }
            }
            else
            {
              done = true;
            }
            break;

          case nidVersionReply:
            if (lPacket == packetLengthVersionReply)
            {
              done = !handleVersionReply(packetBuffer);
            }
            else
            {
              done = true;
            }
            break;

          default:
            // Error!
            ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "SPL incorrect NID packet received: ",
              static_cast<uint32_t>(nidPacket), "SPL", __FILE__, __LINE__);
            done = true;
            break;
          }

          if (sizeLeft == lPacket)
          {
            done = true;
          }
        }
      }
    }

    /*******************************************************************************
    * handleLifeSign
    *******************************************************************************/
    void SplHandler::handleLifeSign(VFW_Buffer& packetBuffer)
    {
      const uint8_t lifesignUnit = vfwGetU8(&packetBuffer);

      if (lifesignUnit == opcLifeSignUnitId)
      {
        const uint8_t newLifesignStatus = vfwGetU8(&packetBuffer);

        if (newLifesignStatus != lifesignStatus)
        {
          lifesignStatus = newLifesignStatus;

          if (lifesignStatus == lifesignStatusUnconditionalStoppingFailure)
          {
            opcSplState = OPC_SPL_FAILED;
          }
        }
      }
      else
      {
        // NOK
      }

      uint32_t newLifesignTimestamp = vfwGetU32(&packetBuffer);
      AbstractBTMHandler::corePtr()->getTrace()->write(ATC::veryDetailedTrace, "Lifesign time ", newLifesignTimestamp);
      AbstractBTMHandler::corePtr()->getTrace()->write(ATC::veryDetailedTrace, "Current time ", static_cast<uint32_t>(vfwGetReferenceTime()));

      // Note that this is a signed value, but unsigned will work anyway since we are using uint32_t in all calculations
      const uint32_t refTimeOffset = vfwGetU32(&packetBuffer);
      // Adjust with tigris offset...
      newLifesignTimestamp -= refTimeOffset;

      if (newLifesignTimestamp > lifesignTimestamp)
      {
        lifesignTimestamp = newLifesignTimestamp;
      }
      else
      {
        opcSplState = OPC_SPL_FAILED;
      }

      AbstractBTMHandler::corePtr()->getTrace()->write(ATC::veryDetailedTrace, "Offset time ", static_cast<int32_t>(refTimeOffset));
    }


    /*******************************************************************************
    * handleVersionReply
    *******************************************************************************/
    bool SplHandler::handleVersionReply(VFW_Buffer& packetBuffer)
    {
      bool versionReplyOk = false;
      const uint8_t packetId = vfwGetU8(&packetBuffer);
      const uint8_t stringLength = vfwGetU8(&packetBuffer);

      OpcVersionString versionString;
      vfwCpyToRawBuffer(&versionString.versionString[0], &packetBuffer, versionStringLength);

      // Null terminate string...
      versionString.versionString[versionStringLength] = '\0';

      if ((stringLength <= versionStringLength) &&   // Check string length in packet
          (versionString.versionString[stringLength] == '\0')) // Check NULL-termination...
      {
        if (packetId == opcVersionReplyId) // Only copy string if it has the OPC id
        {
          memmove(&opcVersionString, &versionString, sizeof(versionString));
        }

        versionReplyOk = true;
      }

      return versionReplyOk;
    }


    /*******************************************************************************
    * readAppStatus
    *******************************************************************************/
    void SplHandler::readAppStatus()
    {
      bool error = false;

      while (vfwSyncChannelStat(opcToAtpAppStatusSyncChannel) > 0U)
      {
        VFW_ChannelCheck check = { VFW_ChannelErrorNone, 0U };
        uint8_t appStatusRawBuffer[ATC::appDataMessageSize];
        // Read the data...
        const uint32_t bytesRead = static_cast<uint32_t>(vfwSyncChannelReadCheck(
          opcToAtpAppStatusSyncChannel, &appStatusRawBuffer[0], sizeof(appStatusRawBuffer), &check));

        if (check.error != VFW_ChannelErrorNone)
        {
          ATC::AbstractLogHandler::corePtr()->writeToLog(
            ATC::BriefLog, "OPC channel error:", static_cast<uint32_t>(check.error), "BH", __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "OPC channel error");
        }
        if (check.timeSinceProduced > ATC::maxChannelTransmissionTime)
        {
          ATC::AbstractLogHandler::corePtr()->writeToLog(
            ATC::BriefLog, "OPC message too old:", check.timeSinceProduced, "BH", __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "OPC message too old");
        }

        if (bytesRead == packetLengthAppStatus)
        {
          VFW_Buffer appStatusBuffer;
          vfwInitBuffer(&appStatusBuffer, &appStatusRawBuffer[0], packetLengthAppStatus);
          vfwSetReadBuffer(&appStatusBuffer, packetLengthAppStatus);

          const uint16_t nidPacket = vfwGetU16(&appStatusBuffer);
          const uint16_t lPacket = vfwGetU16(&appStatusBuffer);

          // Start unpacking the packet...
          const uint8_t sourceUnit = vfwGetU8(&appStatusBuffer);
          const uint8_t newOpcStatus = vfwGetU8(&appStatusBuffer);
          const uint8_t dmiStatus = vfwGetU8(&appStatusBuffer);
          vfwConsumeBuffer(&appStatusBuffer, 4U); // appStatusTimeStamp

          if (nidPacket != nidAppStatus)
          {
            error = true;
          }
          else if (packetLengthAppStatus != lPacket)
          {
            error = true;
          }
          else if (sourceUnit != appStatusSourceUnitOpc)
          {
            error = true;
          }
          else if (dmiStatus != 0U)
          {
            error = true;
          }
          else
          {
            // Everything OK, check status
            if ((opcAppStatus == appStatusRunning) && (newOpcStatus < appStatusRunning))
            {
              // The status cannot leave running, only to error
              char_t textBuffer[200];
              //lint -e{586} snprintf is needed here
              const int32_t retVal = snprintf(&textBuffer[0], sizeof(textBuffer), "OPC APP status changed from %d to %d!",
                opcAppStatus, newOpcStatus);

              if ((retVal > 0) && (static_cast<size_t>(retVal) < sizeof(textBuffer)))
              {
                ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, &textBuffer[0], "SPL",__FILE__, __LINE__);
              }

              error = true;
            }
            else if (newOpcStatus == appStatusError)
            {
              ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "OPC APP status is error!", "SPL", __FILE__, __LINE__);
              error = true;
            }
            else if (newOpcStatus == appStatusRunning)
            {
              if (opcAppStatus != appStatusRunning) // Only report first time...
              {
                ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "OPC APP status is running!", "SPL", __FILE__, __LINE__);
              }
            }
            else
            {
              // No error, OK!
            }

            opcAppStatus = newOpcStatus;
          }
        }
        else
        {
          error = true;
        }
      }

      if (error)
      {
        opcSplState = OPC_SPL_FAILED;
      }
    }


    /*******************************************************************************
    * readTigrisOffset
    *******************************************************************************/
    void SplHandler::readTigrisOffset()
    {
      while (vfwSyncChannelStat(tigrisOffsetSyncChannel) > 0U)
      {
        VFW_ChannelCheck check = { VFW_ChannelErrorNone, 0U };
        uint8_t tigrisOffsetRawBuffer[packetLengthTigrisOffset];

        // Read the data...
        const uint32_t bytesRead = static_cast<uint32_t>(vfwSyncChannelReadCheck(
          tigrisOffsetSyncChannel, &tigrisOffsetRawBuffer[0], sizeof(tigrisOffsetRawBuffer), &check));

        if (check.error != VFW_ChannelErrorNone)
        {
          ATC::AbstractLogHandler::corePtr()->writeToLog(
            ATC::BriefLog, "OPC channel error:", static_cast<uint32_t>(check.error), "BH", __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "OPC channel error");
        }
        if (check.timeSinceProduced > ATC::maxChannelTransmissionTime)
        {
          ATC::AbstractLogHandler::corePtr()->writeToLog(
            ATC::BriefLog, "OPC message too old:", check.timeSinceProduced, "BH", __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "OPC message too old");
        }

        if (bytesRead == packetLengthTigrisOffset)
        {
          VFW_Buffer tigrisOffsetBuffer;
          vfwInitBuffer(&tigrisOffsetBuffer, &tigrisOffsetRawBuffer[0], packetLengthTigrisOffset);
          vfwSetReadBuffer(&tigrisOffsetBuffer, packetLengthTigrisOffset);

          const uint16_t nidPacket = vfwGetU16(&tigrisOffsetBuffer);
          vfwConsumeBuffer(&tigrisOffsetBuffer, 2U); // Packet length. Already checked anyway when the number of bytes are read

          // Start unpacking the packet...
          vfwConsumeBuffer(&tigrisOffsetBuffer, 1U); // Status | size 1 byte |  Not Used
          const int32_t newTigrisTimeOffset = vfwGetI32(&tigrisOffsetBuffer);
          const uint32_t version = vfwGetU32(&tigrisOffsetBuffer);

          AbstractBTMHandler::corePtr()->getTrace()->write(ATC::briefTrace, "OPC Time Sync Version", static_cast<int32_t>(version));

          if (version != getExpectedTimeSyncServerVersion())
          {
            ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "OPC time sync: Incorrect version number reported!",
              version, "SPL", __FILE__, __LINE__);
            opcSplState = OPC_SPL_FAILED;
          }
          else if (nidPacket != nidTigrisOffset)
          {            
            ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "OPC time sync: NID packet error!",
              "SPL", __FILE__, __LINE__);
            opcSplState = OPC_SPL_FAILED;
          }
          else
          {
            tigisOffsetReceivedCycleCounter = 0U;
            // Packet OK, use offset
            if (newTigrisTimeOffset != currentTigrisTimeOffset)
            {
              currentTigrisTimeOffset = newTigrisTimeOffset;
              ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::VeryDetailedLog, "OPC time sync, new offset: ",
                newTigrisTimeOffset, "SPL", __FILE__, __LINE__);
            }
            AbstractBTMHandler::corePtr()->setOpcReferenceTimeOffset(newTigrisTimeOffset);
          }
        }
        else
        {
          opcSplState = OPC_SPL_FAILED;
        }
      }
    }

    /*******************************************************************************
    * getExpectedTimeSyncServerVersion
    *******************************************************************************/
    uint32_t SplHandler::getExpectedTimeSyncServerVersion() const
    {
      const uint8_t byte1 = AbstractConfig::corePtr()->getExpectedTimeSyncServerVersion1();
      const uint8_t byte2 = AbstractConfig::corePtr()->getExpectedTimeSyncServerVersion2();
      const uint8_t byte3 = AbstractConfig::corePtr()->getExpectedTimeSyncServerVersion3();
      const uint8_t byte4 = AbstractConfig::corePtr()->getExpectedTimeSyncServerVersion4();

      const uint32_t version =
        (static_cast<uint32_t>(byte1) << 24U) |
        (static_cast<uint32_t>(byte2) << 16U) |
        (static_cast<uint32_t>(byte3) << 8U) |
         static_cast<uint32_t>(byte4);

      return version;
    }

    /*******************************************************************************
    * runIn
    *******************************************************************************/
    void SplHandler::runIn()
    {
      OpcSplState prevState = opcSplState;

      readAppStatus();
      readTigrisOffset();

      if (opcAppStatus != appStatusIdle)
      {
        int32_t splResult = SPL_INIT_OK;
        int32_t clockState = SPL_INIT_OK;

        (void)SPL_Bus_Management(&splResult, tokenValue);
        (void)SPL_Clock_Management(&splResult, tokenValue);
        (void)SPL_Get_Clock_Status(&clockState, tokenValue);

        switch (opcSplState)
        {
        case OPC_SPL_CLOCK_SYNCHRONIZING:
          handleOpcSplClockSynchronizing(clockState);
          break;

        case OPC_SPL_WAIT_FOR_LEAVING_IDLE:
          handleOpcSplWaitForLeavingIdle(clockState);
          break;

        case OPC_SPL_SEND_CLOCK_SYNC:
          handleOpcSplSendClockSync(clockState);
          break;

        case OPC_SPL_CONNECTING:
          handleOpcSplConnecting(clockState);
          break;

        case OPC_SPL_CONNECTED:
          handleOpcSplConnected(clockState);
          break;

        case OPC_SPL_FAILED:
          // Do nothing...
          break;

        }
      }

      if ((prevState != opcSplState)  && (opcSplState == OPC_SPL_FAILED))
      {
        ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "SPL connection failed", "SPL", __FILE__, __LINE__);
      }
    }

    /*******************************************************************************
    * runOut
    *******************************************************************************/
    void SplHandler::runOut()
    {
      if (opcSplState == OPC_SPL_CONNECTED)
      {
        packLifeSignPacket();
        sendSplOutput();
      }
    }

    /*******************************************************************************
    * sendOutputToOpc
    *******************************************************************************/
    void SplHandler::sendOutputToOpc(Profibus_Type* const pBuff)
    {
      const uint8_t sapId = pBuff->SAP_no;

      // We only handle these SAP-ID's
      if ((sapId == mvbSapId)  ||
          (sapId == clockSapId)   )
      {
        VFW_Buffer mvbBuffer;
        vfwInitBuffer(&mvbBuffer, &pBuff->Profibus_Data[0], sizeof (pBuff->Profibus_Data));
        vfwSetReadBuffer(&mvbBuffer, sizeof(pBuff->Profibus_Data));

        const uint16_t splDataLen = vfwGetU16(&mvbBuffer) + splHeaderSize;
      
        // Check consistency...
        if (splDataLen <= sizeof(pBuff->Profibus_Data))
        {
          if (sapId == mvbSapId)
          {
            atpToOpcSap19OutputChannel.putBuffer(&pBuff->Profibus_Data[0], splDataLen);
          }
          else
          {
            atpToOpcClockSyncOutputChannel.putBuffer(&pBuff->Profibus_Data[0], splDataLen);
            ++opcSplSendClockSyncCounter;
          }
        }
        else
        {
          // Oops, things have gone wrong!
          opcSplState = OPC_SPL_FAILED;
        }
      }
      else
      {
        // Oops, things have gone wrong, not our SAP-ID!
        ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "SPL error, unknown SAP: ",
          static_cast<uint32_t>(sapId), "SPL", __FILE__, __LINE__);
        opcSplState = OPC_SPL_FAILED;
      }
    }


    /*******************************************************************************
    * getInputFromOpc
    *******************************************************************************/
    uint8_t SplHandler::getInputFromOpc(Profibus_Type* const pBuff)
    {
      // Fill all positions with 0
      static_cast<void>(memset(pBuff, 0, sizeof(Profibus_Type)));
      uint8_t result = 0U;

      uint16_t bytesLeft = static_cast<uint16_t>(vfwGetValidSize(&udpInputBuffer));

      // Do we have data left in the buffer?
      if (bytesLeft == 0U)
      {
        // No buffer data left, see if we have received more data...
        if (vfwSyncChannelStat(opcToAtpSap19SyncChannel) > 0U)
        {
          // Read the data...
          VFW_ChannelCheck check = { VFW_ChannelErrorNone, 0U };
          const int32_t bytesRead = vfwSyncChannelReadCheck(
            opcToAtpSap19SyncChannel, &udpInputRawBuffer[0], sizeof(udpInputRawBuffer), &check);

          if (check.error != VFW_ChannelErrorNone)
          {
            ATC::AbstractLogHandler::corePtr()->writeToLog(
              ATC::BriefLog, "OPC channel error:", static_cast<uint32_t>(check.error), "BH", __FILE__, __LINE__);
            ATC::aosHalt(__FILE__, __LINE__, "OPC channel error");
          }
          if (check.timeSinceProduced > ATC::maxChannelTransmissionTime)
          {
            ATC::AbstractLogHandler::corePtr()->writeToLog(
              ATC::BriefLog, "OPC message too old:", check.timeSinceProduced, "BH", __FILE__, __LINE__);
            ATC::aosHalt(__FILE__, __LINE__, "OPC message too old");
          }

          if (bytesRead > 0)
          {
            bytesLeft = static_cast<uint16_t>(bytesRead);
            Support::AbstractCrossCompare::corePtr()->addCrossCompareInputData(&udpInputRawBuffer[0], bytesLeft);
            vfwSetReadBuffer(&udpInputBuffer, bytesLeft);
          }
        }
      }

      if (bytesLeft > 0U)
      {
        // Get the current buffer position.
        const uint8_t* const p = vfwGetPointer(&udpInputBuffer);

        const uint16_t splDataSize = vfwGetU16(&udpInputBuffer); // Read SPL data size
        const uint16_t splPacketSize = splHeaderSize + splDataSize;
        const uint16_t bytesLeftToConsume = splPacketSize - 2U; // We have already read the 2 bytes for the size

        // Check data sizes are valid
        if (splPacketSize <= bytesLeft)
        {
          vfwConsumeBuffer(&udpInputBuffer, bytesLeftToConsume); // Jump to the next SPL packet...

          // Set the timeStamp 
          pBuff->timeStamp = static_cast<uint32_t>(vfwGetReferenceTime());

          // Copy the SPL packet
          memmove(&pBuff->Profibus_Data[0], p, splPacketSize);
          result = 1U;
        }
        else
        {
          vfwSetReadBuffer(&udpInputBuffer, 0U);
          ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "SPL error, incorrect SPL packet length: ",
            static_cast<uint32_t>(splDataSize), "SPL", __FILE__, __LINE__);
          opcSplState = OPC_SPL_FAILED;
        }
      }

      return result;
    }
  } // namepace IO
} // namespace ATP


/*******************************************************************************
* _IOgetProfibusInputData
* Lint complains here, but it is nothing we can do, it is a part of the SPL Library design.
*   error 1960: (Note -- Violates MISRA C++ 2008 Required Rule 17-0-2, Re-use of C++ identifier pattern: _IOsetProfibusOutputData)
*******************************************************************************/
extern "C" uint8_t _IOgetProfibusInputData(Profibus_Type* const pBuff) //lint !e1960 Needed due to external interface
{
  return ATP::IO::SplHandler::instance().getInputFromOpc(pBuff);
}


/*******************************************************************************
* _IOsetProfibusOutputData
* Lint complains here, but it is nothing we can do, it is a part of the SPL Library design.
*   error 1960: (Note -- Violates MISRA C++ 2008 Required Rule 17-0-2, Re-use of C++ identifier pattern: _IOsetProfibusOutputData)
*******************************************************************************/
extern "C" void _IOsetProfibusOutputData(Profibus_Type* const pBuff) //lint !e1960 Needed due to external interface
{
  ATP::IO::SplHandler::instance().sendOutputToOpc(pBuff);
}

