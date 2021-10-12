/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
* This file defines MessageDispatcher class implementation
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-12-15    spandita    Created
* 2017-02-01    spandita    Implemented the phase 1 functionalities
* 2017-02-14    saprasad    Remove the AOSPC/TCC ip address(host type Not required)
* 2017-02-15    spandita    Added functionality of Class D
* 2017-03-29    marlundg    Refactoring of connectionItems, added TCC3, OPC, LIG(Client)
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "message_dispatcher.hpp"
#include "config.hpp"
#include "basic_ip.hpp"
#include "event_handler.hpp"
#include "atc_util.hpp"
#include "console.hpp"
#include "abstract_log_handler.hpp"
#include "message_dispatcher_event_ids.hpp"
#include "atc_math.hpp"

#ifdef __GNUG__
#include <unistd.h>
#include <cstdio>
#include <vfw_time.h>
#else
extern "C" int64_t vfwGetReferenceTime(void);
#endif

/******************************************************************************
* LINT SUPPRESSIONS
******************************************************************************/
//lint -esym(586,snprintf) snprintf is needed here

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/
namespace
{
  extern "C"
  {
    /******************************************************************************
    * callbackHandleMessageFromATP
    ******************************************************************************/
    static void callbackHandleMessageFromATP(VFW_SyncChannel const channel, void* const data)
    {
      //lint -e{925} Cast is unavoidable here
      Dispatcher::ConnectionItem* const connectionItem = reinterpret_cast<Dispatcher::ConnectionItem*>(data);
      Dispatcher::MessageDispatcher::instance().handleMessageFromATP(channel, connectionItem);
    }
  }
}

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace Dispatcher
{  

  /******************************************************************************
  * Constructor
  ******************************************************************************/
  MessageDispatcher::MessageDispatcher() : IOComponent(msgDispId, "MsgDsp", "MD"),
    portInitDone(false),
    errorWriteChannel(ATC::Event::createLogEvent(msgDispId, ATC::AdaptationContainer, eventIdWriteChannelFailure,
      0U, "Write channel failure")),
    errorConnectionId(ATC::Event::createLogEvent(msgDispId, ATC::AdaptationContainer, eventIdConnecIdFailure,
      0U, "Invalid external Connection ID")),
    errorUdpSequence(ATC::Event::createLogEvent(msgDispId, ATC::AdaptationContainer, eventIdUdpSequenceFailure,
      0U, "UDP sequence number missed"))
  {
    // Set the static pointer to object
    tracePtr = &trace;
    memset(&connectionItems[0][0], 0, sizeof(connectionItems[0][0]) * BasicIP::connectionMax * 2U);
  }

  /******************************************************************************
  * instance
  *
  ******************************************************************************/
  MessageDispatcher& MessageDispatcher::instance(void)
  {
    static MessageDispatcher theOnlyMessageDispInstance;

    return theOnlyMessageDispInstance;
  }

  /******************************************************************************
  * Init
  ******************************************************************************/
  bool MessageDispatcher::init(void)
  {
#ifdef __HIL
    // AOSPC related init of IP connection, only needed in HIL
    //ATP waits for messages from AOSPC (LocoSim) to complete the init of LocoIO. Hence, the need to initialize the connection early
    const uint16_t  portNrSimVIOHSim = 30190U;
    if (!BasicIP::instance().initConnection(BasicIP::connectionIOSim, BasicIP::instance().ConnectionTypeUdpHost,
      static_cast<char_t*>(NULL), portNrSimVIOHSim, maxExternMsgSize, maxInputMessageSize)) //VIOH  
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }

    const uint16_t  portNrSpeedSim = 30191U;
    if (!BasicIP::instance().initConnection(BasicIP::connectionSpeedSim, BasicIP::instance().ConnectionTypeUdpHost,
      static_cast<char_t*>(NULL), portNrSpeedSim, maxExternMsgSize, maxInputMessageSize)) //COD
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }
#endif

    // Send version string to ATP
    sendVersionInfoToAtp();
    return true;
  }

  /**
  * Set up the ports (except tcc-ports)
  */
  void MessageDispatcher::portInitialization()
  {
    portInitDone = true;
    // VFW Sync Channel initialization 

    // TCC initialization
    const uint16_t portNrTcc1Radio = Config::instance().getTCC1Port();
    const uint16_t portNrTcc2Radio = Config::instance().getTCC2Port();
    const uint16_t portNrTcc3Radio = Config::instance().getTCC3Port();

    // TCC1-3
    if (!BasicIP::instance().initConnection(BasicIP::connectionTCC1, BasicIP::instance().ConnectionTypeTcpHost,
      static_cast<char_t*>(NULL), portNrTcc1Radio, maxExternMsgSize, maxInputMessageSize))
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }

    if (!BasicIP::instance().initConnection(BasicIP::connectionTCC2, BasicIP::instance().ConnectionTypeTcpHost,
      static_cast<char_t*>(NULL), portNrTcc2Radio, maxExternMsgSize, maxInputMessageSize))
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }

    if (!BasicIP::instance().initConnection(BasicIP::connectionTCC3, BasicIP::instance().ConnectionTypeTcpHost,
      static_cast<char_t*>(NULL), portNrTcc3Radio, maxExternMsgSize, maxInputMessageSize))
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }

    // LIG initialization
    const char_t * ipAddrLig = Config::instance().getLIGIp();
    const uint16_t portNrLig = Config::instance().getLIGPort();

    // OPC initialization
    const uint16_t localPortNrOpc = Config::instance().getLocalOPCPort();
    const uint16_t remotePortNrOpc = Config::instance().getRemoteOPCPort();
    const uint16_t remotePortNrOpcClockTelegram = 50013U; // TODO: Add config parameter...
    const uint16_t localPortNrOpcAppStatus = 50050U; // TODO: Add config parameter...
    const char_t* const remoteIpAddrOpc = Config::instance().getRemoteOPCIp();

    // DMI1 initialization
    const char_t * ipAddrDmi1 = Config::instance().getDMI1Ip(); // DMI 1
    const char_t * ipAddrDmi2 = Config::instance().getDMI2Ip(); // DMI 1
    const uint16_t portNrDmi = Config::instance().getDMIPort();  // DMI 1/2

    // DMI1-2
    if (!BasicIP::instance().initConnection(BasicIP::connectionDMI1, BasicIP::instance().ConnectionTypeTcpClient,
      ipAddrDmi1, portNrDmi, maxExternMsgSize, maxInputMessageSize))
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }
    
    if (!BasicIP::instance().initConnection(BasicIP::connectionDMI2, BasicIP::instance().ConnectionTypeTcpClient,
      ipAddrDmi2, portNrDmi, maxExternMsgSize, maxInputMessageSize))
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }

    // LIG
    if (!BasicIP::instance().initConnection(BasicIP::connectionLIG, BasicIP::instance().ConnectionTypeTcpClient,
      ipAddrLig, portNrLig, maxExternMsgSize, maxInputMessageSize))
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }

    // OPC UDP-Client
    if (!BasicIP::instance().initConnection(BasicIP::connectionOPCClient, BasicIP::instance().ConnectionTypeUdpClient,
      remoteIpAddrOpc, remotePortNrOpc, maxExternMsgSize, maxInputMessageSize))
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }

    // OPC-Profibus UDP-Client
    if (!BasicIP::instance().initConnection(BasicIP::connectionOPCProfibusClient, BasicIP::instance().ConnectionTypeUdpClient,
      remoteIpAddrOpc, remotePortNrOpcClockTelegram, maxExternMsgSize, maxInputMessageSize))
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }

    // OPC UDP-Host
    if (!BasicIP::instance().initConnection(BasicIP::connectionOPCServer, BasicIP::instance().ConnectionTypeUdpHost,
      static_cast<char_t*>(NULL), localPortNrOpc, maxExternMsgSize, maxInputMessageSize))
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }

#ifdef __VSIM
    // OPC UDP-Host Application status, only used in VSIM
    if (!BasicIP::instance().initConnection(BasicIP::connectionOPCAppStatusServer, BasicIP::instance().ConnectionTypeUdpHost,
      static_cast<char_t*>(NULL), localPortNrOpcAppStatus, maxExternMsgSize, maxInputMessageSize))
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }
#else
    const uint16_t  portNrOPCSim = 30192U;
    if (!BasicIP::instance().initConnection(BasicIP::connectionOPCSim, BasicIP::instance().ConnectionTypeUdpHost,
      static_cast<char_t*>(NULL), portNrOPCSim, maxExternMsgSize, maxInputMessageSize)) //OPC  
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }
#endif

    // OBRD
    const uint16_t portNrOBRD = Config::instance().getOBRDPort();
    if (!BasicIP::instance().initConnection(BasicIP::connectionOBRD, BasicIP::instance().ConnectionTypeTcpHost,
      static_cast<char_t*>(NULL), portNrOBRD, maxExternMsgSize, maxInputMessageSize))
    {
      EventHandler::instance().reportEvent(errorConnectionId, __FILE__, __LINE__);
    }
  }


  /******************************************************************************
  * runIn
  ******************************************************************************/
  void MessageDispatcher::runIn(void)
  {
    // Start of connection ID in dispatcher
    uint8_t connId = ATC::AbstractBasicIP::maxCoreConnections;
    static uint8_t outputExternBuffer[maxExternMsgSize];

    // Check for timeouts when receiving data from ATP-A/B
    int64_t currentTimestamp = vfwGetReferenceTime();

    for (uint8_t queue = 0U; queue <= B_SIDE; ++queue)
    {
      if (!outgoingDispMsgQueue[queue].empty())
      {
        if ((currentTimestamp - outgoingDispMsgQueue[queue].front().timestamp) > maxWaitMsgFromAtp)
        {
          tracePtr->write(ATC::briefTrace, "Too long message delay waiting for 2nd part of ATP message.");
        }
      }
    }

    // Loop through all external Connection ID
    for (; connId < BasicIP::connectionMax; connId++)
    {
      // Check for connection is active(connected)
      if (ATC::AbstractBasicIP::ConnectionStatusConnected == BasicIP::instance().getConnectionStatus(connId))
      {
        // Check is there any data on external ID
        while (BasicIP::instance().isPendingRead(connId))
        {
          if (BasicIP::connectionLIG == connId) //check if it is LIG Message
          {
            uint32_t currentByteIndex = 0U;
            uint32_t noOfBytesLeft = BasicIP::instance().readBuf(connId, &outputExternBuffer[0], sizeof(outputExternBuffer));

            // As long as there are more Class-D messages
            while (noOfBytesLeft > 0U)
            {
              // If the total number of bytes is less than the max size of one class-D message -> Only copy the actual amount needed.
              const uint32_t bytesToCopy = ATC::ATCMath::minimum(noOfBytesLeft, classDObj.getClassDMessageMaxTotalLen());
              tracePtr->write(ATC::veryDetailedTrace, "Class-D number of bytes left: ", noOfBytesLeft);

              // Make sure not to access outside outputExternBuffer
              if (currentByteIndex <= (sizeof(outputExternBuffer) - bytesToCopy))
              {
                // Parse the data
                const ClassD::ErrorType errorType = classDObj.parseMessage(&outputExternBuffer[currentByteIndex], bytesToCopy);

                switch (errorType)
                {
                case ClassD::NoError:
                {
                  tracePtr->write(ATC::veryDetailedTrace, "Incoming message Class D Parsing OK");
                  const uint32_t messageLength = classDObj.getActualMsgLen();
                  const uint8_t* const bodyBuffer = classDObj.getClassDBodyPtr();
                  writeToVfwChannel(connId, bodyBuffer, messageLength); // Write to VFW channel from dispatcher to ATP A & B

                  // Set the index to next Class-D message in outputExternBuffer.
                  currentByteIndex += classDObj.getClassDMessageActualLen();
                  noOfBytesLeft -= classDObj.getClassDMessageActualLen();
                  break;
                }
                case ClassD::MessageIncomplete:
                  // TODO! this case needs proper handling
                  tracePtr->write(ATC::briefTrace, "Incoming message is not completely read");
                  noOfBytesLeft = 0U;
                  break;
                case ClassD::CommIdIncorrect:
                  tracePtr->write(ATC::briefTrace, "Incoming message Incorrect CommID");
                  BasicIP::corePtr()->closeReopen(connId);
                  classDObj.resetCommID();
                  noOfBytesLeft = 0U;
                  break;
                case ClassD::MissingSTX:
                  tracePtr->write(ATC::briefTrace, "Incoming message Incorrect STX");
                  BasicIP::corePtr()->closeReopen(connId);
                  classDObj.resetCommID();
                  noOfBytesLeft = 0U;
                  break;
                case ClassD::MissingETX:
                  tracePtr->write(ATC::briefTrace, "Incoming message Incorrect ETX");
                  BasicIP::corePtr()->closeReopen(connId);
                  classDObj.resetCommID();
                  noOfBytesLeft = 0U;
                  break;
                case ClassD::IncorrectMessageFormat:
                  tracePtr->write(ATC::briefTrace, "Incoming message Incorrect Protocol version");
                  BasicIP::corePtr()->closeReopen(connId);
                  classDObj.resetCommID();
                  noOfBytesLeft = 0U;
                  break;
                case ClassD::NotDataMsg:
                  tracePtr->write(ATC::briefTrace, "Incoming message is not data message");
                  BasicIP::corePtr()->closeReopen(connId);
                  classDObj.resetCommID();
                  noOfBytesLeft = 0U;
                  break;
                default:
                  tracePtr->write(ATC::briefTrace, "Incoming message Unknown fault");
                  BasicIP::corePtr()->closeReopen(connId);
                  classDObj.resetCommID();
                  noOfBytesLeft = 0U;
                  break;
                }
              }
              else
              {
                tracePtr->write(ATC::briefTrace, "Buffer overflow");
                BasicIP::corePtr()->closeReopen(connId);
                classDObj.resetCommID();
                noOfBytesLeft = 0U;
              }
            }
          }
          else
          {
            // Read as many bytes that can be handled by the corresponding VFW-channel

            const uint32_t maxChannelWriteLen = connectionItems[connId][A_SIDE].maxMessageWriteLen;
            uint32_t dataLayerOverheadSize = 0U;

            const DataType dataType = connectionItems[connId][A_SIDE].dataType;

            if (dataType == MiscData)
            {
              dataLayerOverheadSize = udpMiscDataLayerOverheadSize;
            }
            else if (dataType == SplData)
            {
              dataLayerOverheadSize = splDataLayerOverheadSize;
            }
            else
            {
              // No data layer overhead...
            }
            const uint32_t maxBasicIpReadLen = maxChannelWriteLen + dataLayerOverheadSize;
            const uint32_t noOfBytesRead = BasicIP::instance().readBuf(
              connId, &outputExternBuffer[0], maxBasicIpReadLen); //read the bytes

            if (noOfBytesRead > dataLayerOverheadSize)
            {
              const uint32_t channelWriteLen = noOfBytesRead - dataLayerOverheadSize;
              writeToVfwChannel(connId, &outputExternBuffer[0], channelWriteLen); // write to VFW channel from dispatcher to ATP A & B
            }

          }
        }
      }
    }
  }


  /******************************************************************************
  * runOut
  ******************************************************************************/
  void MessageDispatcher::runOut(void)
  {
    //nothing to do
  }

  /******************************************************************************
  * preInit
  ******************************************************************************/
  void MessageDispatcher::preInit(void)
  {
    // Initialize the data for all connections
    initConnectionItems();

    // Read channel initialization
    readChannelInit();

    // Write channel initialization
    writeChannelInit();
  }

  /******************************************************************************
  * initConnectionItems
  ******************************************************************************/
  void MessageDispatcher::initConnectionItems()
  {

    // Loop through all connection Items
    for (uint8_t connId = ATC::AbstractBasicIP::maxCoreConnections; connId < BasicIP::connectionMax; connId++)
    {

      // Set default values for A and B.
      for (uint8_t side = 0U; side <= B_SIDE; side++)
      {
        ConnectionItem& currentItem = connectionItems[connId][side];
        currentItem.side = side;
        currentItem.noOfBytesReceivedFromATP = 0U;
        currentItem.noOfMessagesReceivedFromATP = 0U;
        currentItem.noOfBytesSentToATP = 0U;
        currentItem.noOfMessageSentToATP = 0U;

        currentItem.connId = connId;
        currentItem.dataType = NormalData;
        currentItem.lastUdpSequenceNumber = 0U;
        currentItem.error = false;
      }

      // Populate the different connections with channel-specific values
      switch (connId)
      {
      case BasicIP::connectionTCC1:
        connectionItems[connId][A_SIDE].readChannelName = ATC::radioChannel1ATPAToDisp;
        connectionItems[connId][A_SIDE].writeChannelName = ATC::radioChannel1DispToATPA;
        connectionItems[connId][B_SIDE].readChannelName = ATC::radioChannel1ATPBToDisp;
        connectionItems[connId][B_SIDE].writeChannelName = ATC::radioChannel1DispToATPB;

        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          ConnectionItem& currentItem = connectionItems[connId][side];
          currentItem.readQueueSize = ATC::radioChMsgOutQueueSize;
          currentItem.maxMessageReadLen = ATC::maxRadioMessageSize;
          currentItem.writeQueueSize = ATC::radioChMsgInQueueSize;
          currentItem.maxMessageWriteLen = ATC::maxRadioMessageSize;
        }

        break;
      case BasicIP::connectionTCC2:
        connectionItems[connId][A_SIDE].readChannelName = ATC::radioChannel2ATPAToDisp;
        connectionItems[connId][A_SIDE].writeChannelName = ATC::radioChannel2DispToATPA;
        connectionItems[connId][B_SIDE].readChannelName = ATC::radioChannel2ATPBToDisp;
        connectionItems[connId][B_SIDE].writeChannelName = ATC::radioChannel2DispToATPB;

        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          ConnectionItem& currentItem = connectionItems[connId][side];
          currentItem.readQueueSize = ATC::radioChMsgOutQueueSize;
          currentItem.maxMessageReadLen = ATC::maxRadioMessageSize;
          currentItem.writeQueueSize = ATC::radioChMsgInQueueSize;
          currentItem.maxMessageWriteLen = ATC::maxRadioMessageSize;
        }

        break;

      case BasicIP::connectionTCC3:
        connectionItems[connId][A_SIDE].readChannelName = ATC::radioChannel3ATPAToDisp;
        connectionItems[connId][A_SIDE].writeChannelName = ATC::radioChannel3DispToATPA;
        connectionItems[connId][B_SIDE].readChannelName = ATC::radioChannel3ATPBToDisp;
        connectionItems[connId][B_SIDE].writeChannelName = ATC::radioChannel3DispToATPB;

        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          ConnectionItem& currentItem = connectionItems[connId][side];
          currentItem.readQueueSize = ATC::radioChMsgOutQueueSize;
          currentItem.maxMessageReadLen = ATC::maxRadioMessageSize;
          currentItem.writeQueueSize = ATC::radioChMsgInQueueSize;
          currentItem.maxMessageWriteLen = ATC::maxRadioMessageSize;
        }

        break;

      case BasicIP::connectionDMI1:
        connectionItems[connId][A_SIDE].readChannelName = ATC::dmiChannel1ATPAToDisp;
        connectionItems[connId][A_SIDE].writeChannelName = ATC::dmiChannel1DispToATPA;
        connectionItems[connId][B_SIDE].readChannelName = ATC::dmiChannel1ATPBToDisp;
        connectionItems[connId][B_SIDE].writeChannelName = ATC::dmiChannel1DispToATPB;

        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          ConnectionItem& currentItem = connectionItems[connId][side];
          currentItem.readQueueSize = ATC::dmiMsgOutQueueSize;
          currentItem.maxMessageReadLen = ATC::maxDMIMessageSize;
          currentItem.writeQueueSize = ATC::dmiMsgInQueueSize;
          currentItem.maxMessageWriteLen = ATC::maxDMIMessageSize;
        }

        break;

      case BasicIP::connectionDMI2:
        connectionItems[connId][A_SIDE].readChannelName = ATC::dmiChannel2ATPAToDisp;
        connectionItems[connId][A_SIDE].writeChannelName = ATC::dmiChannel2DispToATPA;
        connectionItems[connId][B_SIDE].readChannelName = ATC::dmiChannel2ATPBToDisp;
        connectionItems[connId][B_SIDE].writeChannelName = ATC::dmiChannel2DispToATPB;

        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          ConnectionItem& currentItem = connectionItems[connId][side];
          currentItem.readQueueSize = ATC::dmiMsgOutQueueSize;
          currentItem.maxMessageReadLen = ATC::maxDMIMessageSize;
          currentItem.writeQueueSize = ATC::dmiMsgInQueueSize;
          currentItem.maxMessageWriteLen = ATC::maxDMIMessageSize;
        }

        break;

      case BasicIP::connectionIOSim:
        connectionItems[connId][A_SIDE].readChannelName = ATC::viohChannelATPAToDisp;
        connectionItems[connId][A_SIDE].writeChannelName = ATC::viohChannelDispToATPA;
        connectionItems[connId][B_SIDE].readChannelName = ATC::viohChannelATPBToDisp;
        connectionItems[connId][B_SIDE].writeChannelName = ATC::viohChannelDispToATPB;

        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          ConnectionItem& currentItem = connectionItems[connId][side];
          currentItem.readQueueSize = ATC::viohMessageOutQueueSize;
          currentItem.maxMessageReadLen = ATC::maxVIOHMessageSize;
          currentItem.writeQueueSize = ATC::viohMessageInQueueSize;
          currentItem.maxMessageWriteLen = ATC::maxVIOHMessageSize;
        }

        break;

      case BasicIP::connectionSpeedSim:
        connectionItems[connId][A_SIDE].readChannelName = ""; // No read channel
        connectionItems[connId][A_SIDE].writeChannelName = ATC::speedChannelSim_A;
        connectionItems[connId][B_SIDE].readChannelName = ""; // No read channel
        connectionItems[connId][B_SIDE].writeChannelName = ATC::speedChannelSim_B;

        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          ConnectionItem& currentItem = connectionItems[connId][side];
          currentItem.writeQueueSize = ATC::speedSimChannelQueueSize;
          currentItem.maxMessageWriteLen = ATC::speedSimChannelTelSize;
        }

        break;

      case BasicIP::connectionOPCSim:
        connectionItems[connId][A_SIDE].readChannelName = ATC::opcChannelATPAToDisp;
        connectionItems[connId][A_SIDE].writeChannelName = ATC::opcChannelDispToATPA;
        connectionItems[connId][B_SIDE].readChannelName = ATC::opcChannelATPBToDisp;
        connectionItems[connId][B_SIDE].writeChannelName = ATC::opcChannelDispToATPB;

        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          ConnectionItem& currentItem = connectionItems[connId][side];
          currentItem.readQueueSize = ATC::opcSimMsgInQueueSize;
          currentItem.maxMessageReadLen = ATC::opcSimMessageSize;
          currentItem.writeQueueSize = ATC::opcSimMsgInQueueSize;
          currentItem.maxMessageWriteLen = ATC::opcSimMessageSize;
        }

        break;

      case BasicIP::connectionLIG:
        connectionItems[connId][A_SIDE].readChannelName = ATC::lcsChannelATPAToDisp;
        connectionItems[connId][A_SIDE].writeChannelName = ATC::lcsChannelDispToATPA;
        connectionItems[connId][B_SIDE].readChannelName = ATC::lcsChannelATPBToDisp;
        connectionItems[connId][B_SIDE].writeChannelName = ATC::lcsChannelDispToATPB;

        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          ConnectionItem& currentItem = connectionItems[connId][side];
          currentItem.dataType = LigData;
          currentItem.readQueueSize = ATC::lcsMessageOutQueueSize;
          currentItem.maxMessageReadLen = ATC::lcsMsgSize;
          currentItem.writeQueueSize = ATC::lcsMessageInQueueSize;
          currentItem.maxMessageWriteLen = ATC::lcsMsgSize;
        }

        break;

      case BasicIP::connectionOPCClient:
#ifdef __VSIM
        connectionItems[connId][A_SIDE].readChannelName = ATC::btmhChannelATPAToDisp;
        connectionItems[connId][B_SIDE].readChannelName = ATC::btmhChannelATPBToDisp;
#else
        connectionItems[connId][A_SIDE].readChannelName = "readIsOnlyUsedInVSimA";
        connectionItems[connId][B_SIDE].readChannelName = "readIsOnlyUsedInVSimB";
#endif
        connectionItems[connId][A_SIDE].writeChannelName = ""; // No write channel (writing made through connectionOPCServer)
        connectionItems[connId][B_SIDE].writeChannelName = ""; // No write channel (writing made through connectionOPCServer)

        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          ConnectionItem& currentItem = connectionItems[connId][side];
          currentItem.maxMessageReadLen = ATC::splMessageSizeAtpToOpc;
          currentItem.readQueueSize = ATC::splQueueSize;
          currentItem.dataType = SplData;
        }

        break;

      case BasicIP::connectionOPCProfibusClient:
#ifdef __VSIM
        connectionItems[connId][A_SIDE].readChannelName = ATC::opcClockSyncChannelATPAToDisp;
        connectionItems[connId][B_SIDE].readChannelName = ATC::opcClockSyncChannelATPBToDisp;
#else
        connectionItems[connId][A_SIDE].readChannelName = "readProfibusIsOnlyUsedInVSimA";
        connectionItems[connId][B_SIDE].readChannelName = "readProfibusIsOnlyUsedInVSimB";
#endif
        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          ConnectionItem& currentItem = connectionItems[connId][side];
          currentItem.writeChannelName = ""; // No write channel (writing made through connectionOPCServer)
          currentItem.maxMessageReadLen = ATC::splMessageSizeAtpToOpc;
          currentItem.readQueueSize = ATC::splQueueSize;
          currentItem.dataType = SplData;
        }

        break;

      case BasicIP::connectionOPCServer:
#ifdef __VSIM
        connectionItems[connId][A_SIDE].writeChannelName = ATC::btmhChannelDispToATPA;
        connectionItems[connId][B_SIDE].writeChannelName = ATC::btmhChannelDispToATPB;
#else
        connectionItems[connId][A_SIDE].writeChannelName = "writeIsOnlyUsedInVSimA";
        connectionItems[connId][B_SIDE].writeChannelName = "writeIsOnlyUsedInVSimB";
#endif

        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          ConnectionItem& currentItem = connectionItems[connId][side];
          currentItem.maxMessageWriteLen = ATC::splMessageSizeOpcToAtp;
          currentItem.writeQueueSize = ATC::splQueueSize;
          currentItem.dataType = SplData;
          currentItem.readChannelName = ""; // No read channel (reading made through connectionOPCClient)
        }

        break;

      case BasicIP::connectionOPCAppStatusServer:
#ifdef __VSIM
        connectionItems[connId][A_SIDE].writeChannelName = ATC::btmhChannelDispAppDataToATPA;
        connectionItems[connId][B_SIDE].writeChannelName = ATC::btmhChannelDispAppDataToATPB;
#else
        connectionItems[connId][A_SIDE].writeChannelName = "appStatusIsOnlyUsedInVSimA";
        connectionItems[connId][B_SIDE].writeChannelName = "appStatusIsOnlyUsedInVSimB";
#endif
        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          ConnectionItem& currentItem = connectionItems[connId][side];
          currentItem.readChannelName = ""; // No read channel
          currentItem.maxMessageWriteLen = ATC::appDataMessageSize;
          currentItem.writeQueueSize = ATC::appDataQueueSize;
          currentItem.dataType = MiscData;
        }

        break;

      case BasicIP::connectionOBRD:
        connectionItems[connId][A_SIDE].readChannelName = ATC::obrdChannelATPAToDisp;
        connectionItems[connId][A_SIDE].writeChannelName = ATC::obrdChannelDispToATPA;
        connectionItems[connId][B_SIDE].readChannelName = ATC::obrdChannelATPBToDisp;
        connectionItems[connId][B_SIDE].writeChannelName = ATC::obrdChannelDispToATPB;

        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          connectionItems[connId][side].readQueueSize = ATC::obrdMessageOutQueueSize;
          connectionItems[connId][side].maxMessageReadLen = ATC::maxOBRDMsgSize;
          connectionItems[connId][side].writeQueueSize = ATC::obrdMessageInQueueSize;
          connectionItems[connId][side].maxMessageWriteLen = ATC::maxOBRDMsgSize;
        }

        break;

      case BasicIP::connectionATP:
        connectionItems[connId][A_SIDE].writeChannelName = ATC::dispatcherToAtpA;
        connectionItems[connId][B_SIDE].writeChannelName = ATC::dispatcherToAtpB;

        // Same data for both A and B
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          connectionItems[connId][side].readChannelName = "";
          connectionItems[connId][side].readQueueSize = 0U;
          connectionItems[connId][side].maxMessageReadLen = ATC::maxDispatcherToAtpMsgSize;
          connectionItems[connId][side].writeQueueSize = ATC::dispatcherToAtpOutQueueSize;
          connectionItems[connId][side].maxMessageWriteLen = ATC::maxDispatcherToAtpMsgSize;
        }
        break;

      default:
        // All channel types should be recognized
        ATC::aosHalt(__FILE__, __LINE__, "Unknown channel type");
        break;
      }
    }
  }

  /******************************************************************************
  * VFW Read Channels Initialization from Disp to ATP A and B
  ******************************************************************************/
  void MessageDispatcher::readChannelInit(void)
  {
    // Used for error-messages
    char_t  buffer[256];

    // Setup the receiving handlers with corresponding data for each connection
    for (uint8_t connId = ATC::AbstractBasicIP::maxCoreConnections; connId < BasicIP::connectionMax; connId++)
    {

      // Initialize all connections from dispatcher towards ATP-A/B.
      for (uint8_t side = 0U; side <= B_SIDE; side++)
      {
        ConnectionItem& currentItem = connectionItems[connId][side];

        // Only connect a read-handler if descriptor is uninitialized and a channel name is present. 
        if ((NULL == currentItem.readChannelDescr) && ('\0' != *(currentItem.readChannelName)))
        {
          currentItem.readChannelDescr = vfwChannelOpenRead(currentItem.readChannelName,
            currentItem.readQueueSize, currentItem.maxMessageReadLen, currentItem.readChannelName);

          if (NULL != currentItem.readChannelDescr)
          {
            // Add to Sync module
            VFW_SyncChannel readsyncChDesc = vfwSyncAddChannel(currentItem.readChannelDescr, 0);
            vfwSyncChannelSetHandler(readsyncChDesc, &callbackHandleMessageFromATP, &currentItem);
          }
          else
          {
            // Message is not available 
            if (currentItem.noOfBytesReceivedFromATP == 0U)
            {
              const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "Failed to open %s channel", currentItem.readChannelName);

              if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
              {
                ATC::aosHalt(__FILE__, __LINE__, &buffer[0]);
              }
              else
              {
                ATC::aosHalt(__FILE__, __LINE__, "Failed to open channel");
              }
            }
          }
        }
      }
    }
  }

  /******************************************************************************
  * VFW Write Channels Initialization from Disp to ATP A and B
  ******************************************************************************/
  void MessageDispatcher::writeChannelInit(void)
  {
    // Used for error-messages
    char_t  buffer[256];

    // Setup the receiving handlers with corresponding data for each connection
    for (uint8_t connId = ATC::AbstractBasicIP::maxCoreConnections; connId < BasicIP::connectionMax; connId++)
    {
      // Both sides
      for (uint8_t side = 0U; side <= B_SIDE; side++)
      {
        ConnectionItem& currentItem = connectionItems[connId][side];
        // Only initialize writeChannelDescr if descriptor is uninitialized and a channel name is present. 
        if ((NULL == currentItem.writeChannelDescr) && ('\0' != *(currentItem.writeChannelName)))
        {
          currentItem.writeChannelDescr = vfwChannelOpenWrite(currentItem.writeChannelName,
            currentItem.writeQueueSize, currentItem.maxMessageWriteLen);

          if (NULL == currentItem.writeChannelDescr)
          {
            const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "Failed to open %s channel", currentItem.writeChannelName);

            if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              ATC::aosHalt(__FILE__, __LINE__, &buffer[0]);
            }
            else
            {
              ATC::aosHalt(__FILE__, __LINE__, "Failed to open channel");
            }
          }
          else
          {
            // Do not set overwrite in VSIM
#ifndef __VSIM
            vfwChannelSetOverwritable(currentItem.writeChannelDescr);
#endif
          }
        }
      }
    }
  }

  /******************************************************************************
  * writeToVfwChannel
  ******************************************************************************/
  void MessageDispatcher::writeToVfwChannel(const uint8_t iD, const uint8_t* const buffer, const uint32_t length)
  {
    if (length > 0U)
    {
      // Sanity check
      if (iD < BasicIP::connectionMax)
      {
        ConnectionItem& currentConnectionA = connectionItems[iD][A_SIDE];
        ConnectionItem& currentConnectionB = connectionItems[iD][B_SIDE];

        // Check if channels are initialized for writing
        if ((NULL != currentConnectionA.writeChannelDescr) &&
          (NULL != currentConnectionB.writeChannelDescr))
        {
          uint32_t bytesToWrite = length;
          const uint8_t* bufferStart = buffer;
          const DataType dataType = currentConnectionA.dataType;

          if (currentConnectionA.error)
          {
            // Ignore input
            bytesToWrite = 0U;
          }
          else if ((dataType == MiscData) || (dataType == SplData))
          {
            const uint8_t newSequenceNumber = *buffer;

            if (dataType == MiscData)
            {
              bufferStart += 3;  // Move forward sequence number + msg_len
            }
            else // SplData
            {
              bufferStart += 1;  // Move forward sequence number
            }

            if (currentConnectionA.noOfBytesSentToATP == 0U)  // If we have never received any data
            {
              // Update the last sequence number to the one we just received if it is the first one...
              currentConnectionA.lastUdpSequenceNumber = newSequenceNumber;
            }
            else
            {
              ++currentConnectionA.lastUdpSequenceNumber;
              if (currentConnectionA.lastUdpSequenceNumber != newSequenceNumber)
              {
                // If a jump forward in the sequence number is discovered, the connection should be ignored
                currentConnectionA.error = true;
                EventHandler::corePtr()->reportEvent(errorUdpSequence, __FILE__, __LINE__);

                char_t strBuf[256];
                const int32_t ret = snprintf(&strBuf[0], sizeof(strBuf),
                  "UDP data received out of sequence, id %d, %d (%d), size = %d!\n",
                  iD, newSequenceNumber, currentConnectionA.lastUdpSequenceNumber, bytesToWrite);

                if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(strBuf)))
                {
                  ATC::debugInfo(&strBuf[0]);
                }

                bytesToWrite = 0U;
              }
            }
          }
          else
          {
            // Other data, no sequence number
          }

          if (bytesToWrite > 0U)
          {
            vfwChannelWrite(currentConnectionA.writeChannelDescr, bufferStart, bytesToWrite);
            currentConnectionA.noOfBytesSentToATP += bytesToWrite;
            currentConnectionA.noOfMessageSentToATP++;
            logChannelWrite(currentConnectionA.writeChannelName);

            vfwChannelWrite(currentConnectionB.writeChannelDescr, bufferStart, bytesToWrite);
            currentConnectionB.noOfBytesSentToATP += bytesToWrite;
            currentConnectionB.noOfMessageSentToATP++;
            logChannelWrite(currentConnectionB.writeChannelName);
          }
        }
      }
      else
      {
        // Event raise for invalid connection ID
        EventHandler::corePtr()->reportEvent(errorWriteChannel, __FILE__, __LINE__);
        tracePtr->write(ATC::briefTrace, "Invalid Connection ID");
      }
    }
  }

  /******************************************************************************
  * traceChannelWrite
  ******************************************************************************/
  void MessageDispatcher::logChannelWrite(const char_t* const channelName) const
  {
    char_t  buffer[512];

    const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "Writing to Channel : %s", channelName);

    if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
    {
      writeToLog(ATC::ChannelLog, &buffer[0]);
    }
  }

  /******************************************************************************
  * traceChannelWrite
  ******************************************************************************/
  void MessageDispatcher::logChannelRead(const char_t* const channelName)
  {
    char_t  buffer[512];

    const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "Reading from Channel : %s", channelName);

    if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
    {
      ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::ChannelLog, &buffer[0], "MD");
    }
  }

  /******************************************************************************
  * sendLigData
  ******************************************************************************/
  uint32_t MessageDispatcher::sendLigData(const uint8_t * const messageData, const uint32_t length, const uint8_t connectionId) const
  {
    uint32_t bytesWritten = 0U;

    // Get the Class D buffer pointer
    const uint8_t* ligBufferPtr = classD.getClassDBufferPtr();

    // Pack the message
    if (classD.buildMessage(messageData, length))
    {
      // Used for trace-messages
      char_t  traceBuffer[256];

      // Write to Connection ID
      bytesWritten = BasicIP::instance().writeBuf(connectionId, ligBufferPtr, classD.getClassDMessageActualLen());

      const int32_t ret = snprintf(&traceBuffer[0], sizeof(traceBuffer), "%d bytes written to %s.",
        bytesWritten,  BasicIP::instance().connectionIdStr(connectionId));

      if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(traceBuffer)))
      {
        tracePtr->write(ATC::veryDetailedTrace, &traceBuffer[0]);
      }
    }

    return bytesWritten;
  }

  /******************************************************************************
  * sendSplMiscData
  ******************************************************************************/
  uint32_t MessageDispatcher::sendSplMiscData(const uint8_t * const messageData, const uint32_t length,
    const uint8_t connectionId, const DataType dataType)
  {
    uint8_t udpBuffer[maxInputMessageSize];
    uint32_t numberOfBytesToWrite = length;

    uint32_t bytesWritten;

    if (dataType == MiscData)
    {
      numberOfBytesToWrite += udpMiscDataLayerOverheadSize;
      VFW_Buffer outputBuffer;
      vfwInitBuffer(&outputBuffer, &udpBuffer[0], numberOfBytesToWrite);

      // SEQ_NUM
      vfwPutU8(&outputBuffer, connectionItems[connectionId][A_SIDE].lastUdpSequenceNumber);

      // MSG_LEN
      vfwPutU16(&outputBuffer, static_cast<uint16_t>(numberOfBytesToWrite - 3U));

      // Copy the incoming buffer to the udpBuffer...
      vfwCpyFromRawBuffer(&outputBuffer, messageData, length);

      // Store NID_PACKET end
      vfwPutU16(&outputBuffer, 0xFFFFU);

      // MSG_LEN end
      vfwPutU16(&outputBuffer, 0U);
    }
    else // SplData
    {
      numberOfBytesToWrite += splDataLayerOverheadSize;
      
      VFW_Buffer outputBuffer;
      vfwInitBuffer(&outputBuffer, &udpBuffer[0], numberOfBytesToWrite);

      // SEQ_NUM
      vfwPutU8(&outputBuffer, connectionItems[connectionId][A_SIDE].lastUdpSequenceNumber);

      // Copy the incoming buffer to the udpBuffer...
      vfwCpyFromRawBuffer(&outputBuffer, messageData, length);
    }

    // Write two times, see ETC, OPC and VAP Interface Specification (1DOC-1002321)
    // The UDP datagrams with the same contents shall be sent twice to achieve high availability.
    bytesWritten = BasicIP::instance().writeBuf(connectionId, &udpBuffer[0], numberOfBytesToWrite);
#ifndef WIN32
    //lint -e{586} usleep is needed here
    if (usleep(1000U * 3U) != 0)
    {
      ATC::aosHalt(__FILE__, __LINE__, "usleep() failed or interrupted");
    }
#endif
    bytesWritten += BasicIP::instance().writeBuf(connectionId, &udpBuffer[0], numberOfBytesToWrite);

    // Update the sequence number...
    ++connectionItems[connectionId][A_SIDE].lastUdpSequenceNumber;

    return bytesWritten;
  }

  /******************************************************************************
  * handleMessageFromATP
  ******************************************************************************/
  void MessageDispatcher::handleMessageFromATP(const VFW_SyncChannel channel, ConnectionItem* const connectionItem)
  {
    // Must have data to work with.
    if (NULL == connectionItem)
    {
      ATC::aosHalt(__FILE__, __LINE__, "Data NULL pointer passed in handleMessageFromATP().");
    }
    else
    {
      OutgoingDispMsg lastReceivedMsgFromATP;
      const uint8_t thisSide = connectionItem->side;
      const uint8_t otherSide = (thisSide + 1U) & 1U;

      if (!portInitDone)
      {
        portInitialization();
      }

      logChannelRead(connectionItem->readChannelName);

      VFW_Buffer inputVfwBuffer;

      // Used for trace-messages
      char_t  traceBuffer[256];

      // Initialize the VFW buffer to raw buffer
      vfwInitBuffer(&inputVfwBuffer, &lastReceivedMsgFromATP.inputBuffer[0], sizeof(lastReceivedMsgFromATP.inputBuffer));

      // Read the message from channel
      VFW_ChannelCheck check = { VFW_ChannelErrorNone, 0U };
      const int32_t noOfSignedBytesReceivedFromATP = vfwSyncChannelReadBufferCheck(channel, &inputVfwBuffer, &check);

      if (noOfSignedBytesReceivedFromATP <= 0)
      {
        writeToLog(ATC::BriefLog, "Channel closed or not correct:", static_cast<int32_t>(connectionItem->connId), __FILE__, __LINE__);
      }
      else if (check.error != VFW_ChannelErrorNone)
      {
        writeToLog(ATC::BriefLog, "Channel read error:", static_cast<int32_t>(check.error), __FILE__, __LINE__);
        ATC::aosHalt(__FILE__, __LINE__, "Channel read error");
      }
      else if (check.timeSinceProduced > ATC::maxChannelTransmissionTime)
      {
        writeToLog(ATC::BriefLog, "Message too old:", check.timeSinceProduced, __FILE__, __LINE__);
        ATC::aosHalt(__FILE__, __LINE__, "Message too old");
      }
      else
      {
        const uint32_t noOfBytesReceivedFromATP = static_cast<uint32_t>(noOfSignedBytesReceivedFromATP);
        // Update statistics
        connectionItem->noOfBytesReceivedFromATP += noOfBytesReceivedFromATP;
        connectionItem->noOfMessagesReceivedFromATP++;

        const int32_t ret = snprintf(&traceBuffer[0], sizeof(traceBuffer),
          "Received Message size from %s is:%d", connectionItem->readChannelName, noOfBytesReceivedFromATP);

        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(traceBuffer)))
        {
          tracePtr->write(ATC::veryDetailedTrace, &traceBuffer[0]);
        }

        // Save Connection ID, received number of bytes and the timestamp.
        lastReceivedMsgFromATP.connId = connectionItem->connId;
        lastReceivedMsgFromATP.size = noOfBytesReceivedFromATP;
        lastReceivedMsgFromATP.timestamp = vfwGetReferenceTime();

        OutgoingDispMsgQueueType& otherQueue = outgoingDispMsgQueue[otherSide];

        OutgoingDispMsgQueueType::iterator it = otherQueue.begin();

        bool messageSent = false;

        // If both ATP-A and B parts has arrived --> Merge and send further from dispatcher.
        // Search the other side for the same message id
        while ((it != otherQueue.end()) && (!messageSent))
        {
          OutgoingDispMsg& otherOutgoingMessage = *it;

          if (otherOutgoingMessage.connId == connectionItem->connId)
          {
            messageSent = true;

            if (thisSide == A_SIDE)
            {
              sendMessageFromAB(lastReceivedMsgFromATP, otherOutgoingMessage);
            }
            else
            {
              sendMessageFromAB(otherOutgoingMessage, lastReceivedMsgFromATP);
            }

            static_cast<void>(otherQueue.erase(it));
          }
          else
          {
            ++it;
          }
        }

        // If not sent, push to proper queue for ATP-A/B
        if (!messageSent)
        {
          if (!outgoingDispMsgQueue[thisSide].full())
          {
            outgoingDispMsgQueue[thisSide].pushBack(lastReceivedMsgFromATP);
          }
          else
          {
            ATC::aosHalt(__FILE__, __LINE__, "The receiving message-queue from ATPA/B is full");
          }
        }
      }
    }
  }


  /******************************************************************************
  * sendMessageFromAB
  ******************************************************************************/
  void MessageDispatcher::sendMessageFromAB(OutgoingDispMsg& msgPartFromATPA, const OutgoingDispMsg& msgPartFromATPB)
  {
    // Used for trace-messages
    char_t  traceBuffer[256];

    const uint8_t connId = msgPartFromATPA.connId;

    const uint32_t msgSizeFromA = msgPartFromATPA.size;
    const uint32_t msgSizeFromB = msgPartFromATPB.size;

    // Concatenate the message-data from ATP-A and B, re-use part A:s buffer as storage to avoid to much copying.
    memmove(&msgPartFromATPA.inputBuffer[msgSizeFromA], &msgPartFromATPB.inputBuffer[0], msgSizeFromB);
    uint8_t *messageData = msgPartFromATPA.inputBuffer;

    // Get actual data-type and total size for message.
    const DataType dataType = connectionItems[connId][A_SIDE].dataType;
    uint32_t totalNoOfBytesReceivedFromATPs = msgSizeFromA + msgSizeFromB;

    uint32_t bytesWritten = 0U;

    // Check for connection
    if (ATC::AbstractBasicIP::ConnectionStatusConnected == BasicIP::instance().getConnectionStatus(connId))
    {
      switch (dataType)
      {
      case NormalData:
        bytesWritten = BasicIP::instance().writeBuf(connId, messageData, totalNoOfBytesReceivedFromATPs);
        break;

      case LigData:
        bytesWritten = sendLigData(messageData, totalNoOfBytesReceivedFromATPs, connId);
        break;

      case MiscData:
      case SplData:
        bytesWritten = sendSplMiscData(messageData, totalNoOfBytesReceivedFromATPs, connId, dataType);
        break;

      default:
        break;
      }

      const int32_t ret = snprintf(&traceBuffer[0], sizeof(traceBuffer), "%d bytes written to %s. (%d + %d)", 
        bytesWritten, BasicIP::instance().connectionIdStr(connId), msgSizeFromA, msgSizeFromB);

      if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(traceBuffer)))
      {
        tracePtr->write(ATC::veryDetailedTrace, &traceBuffer[0]);
      }
    }
    else
    {
      // Reset CommID when LIG is disconnected
      if (LigData == dataType)
      {
        classD.resetCommID();
      }

      const int32_t ret = snprintf(&traceBuffer[0], sizeof(traceBuffer), "Connection to %s is unavailable.",
        BasicIP::instance().connectionIdStr(connId));

      if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(traceBuffer)))
      {
        tracePtr->write(ATC::briefTrace, &traceBuffer[0]);
      }
    }
  }


  /******************************************************************************
  * sendVersionInfoToAtp
  ******************************************************************************/
  void MessageDispatcher::sendVersionInfoToAtp()
  {
    VFW_Buffer dispToAtpBuffer;
    uint8_t buffer[ATC::maxDispatcherToAtpMsgSize];

    memset(&buffer[0U], 0, sizeof(buffer));
    // Initialize the VFW buffer to raw buffer
    vfwInitBuffer(&dispToAtpBuffer, &buffer[0U], sizeof(buffer));

    vfwPutString(&dispToAtpBuffer, ATC::AbstractApplicationBase::corePtr()->getApplicationVersionString());

    const uint8_t* bufferStart = &buffer[0U];

    // Same data for both A and B
    for (uint8_t side = 0U; side <= B_SIDE; side++)
    {
      vfwChannelWrite(connectionItems[BasicIP::connectionATP][side].writeChannelDescr, bufferStart, ATC::maxDispatcherToAtpMsgSize);
      connectionItems[BasicIP::connectionATP][side].noOfBytesSentToATP += ATC::maxDispatcherToAtpMsgSize;
      connectionItems[BasicIP::connectionATP][side].noOfMessageSentToATP++;
      logChannelWrite(connectionItems[BasicIP::connectionATP][side].writeChannelName);
    }
  }


  /*****************************************************************************
  * ConsoleCall
  *******************************************************************************/
  bool MessageDispatcher::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
  {
    bool retVal = false;
    char_t  buffer[1026];
    // Check for argument passed is "help"
    if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
    {
      char_t toWrite[] = "chstat        To get the statics of send/received messages for VFW Channel";
      Console::instance().writeWithNewline(&toWrite[0]);
    }
    else if (ATC::isTextMatch(&argv[0][0], "chstat", sizeof("chstat")) && (argc == 1U))
    {
      Console::instance().writeWithNewline("ChannelName                        ChannelType         NoOfMessages    NoOfBytes");
      Console::instance().writeWithNewline("----------------------------------+-------------------+---------------+---------");

      // Go through the connections
      for (uint8_t connId = ATC::AbstractBasicIP::maxCoreConnections; connId < BasicIP::connectionMax; connId++)
      {
        // Both sides
        for (uint8_t side = 0U; side <= B_SIDE; side++)
        {
          const ConnectionItem& currentItem = connectionItems[connId][side];
          // Only print if a channel is assigned
          if ('\0' != *(currentItem.writeChannelName))
          {
            const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%-35s%-20s%-15d%-5d", currentItem.readChannelName, "Read",
              currentItem.noOfMessagesReceivedFromATP, currentItem.noOfBytesReceivedFromATP);

            if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              Console::instance().writeWithNewline(&buffer[0]);
            }
          }

          // Only print if a channel is assigned
          if ('\0' != *(currentItem.writeChannelName))
          {
            const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%-35s%-20s%-15d%-5d", currentItem.writeChannelName, "Write",
              currentItem.noOfMessageSentToATP, currentItem.noOfBytesSentToATP);

            if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              Console::instance().writeWithNewline(&buffer[0]);
            }
          }
        }
      }

      retVal = true;
    }
    else
    {
      Console::instance().writeWithNewline("Illegal Argument ");
    }

    return retVal;
  }
}

//lint +esym(586,snprintf)
