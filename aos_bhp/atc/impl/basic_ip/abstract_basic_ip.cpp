/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This abstract class implements the core functionality of BasicIP.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-15    bhermans    Created
* 2016-03-16    bhermans    Const Pointer to control-blocks now assigned in constructor init-list
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-04-21    lantback    Implemented corePtr()
* 2016-04-22    lantback    Added component type
* 2016-06-27    adgupta     Implementation of BasicIP functionality
* 2016-07-12    adgupta     Updated init function for host connections
* 2016-08-04    adgupta     Added TCP keep alive values for TCC Sim integration
* 2016-08-19    bhidaji     Changed ulong32_t => u_long in setNonBlockingSocket() to fix compile error in windows
* 2016-09-08    adgupta     Added abstract Console call
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-23    arastogi    Removed ATC::
* 2016-09-23    bhermans    Implemented bip command, removed include of mstcpip.h
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_basic_ip.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_console.hpp"
#include "atc_util.hpp"
#ifndef _DISPATCHER
#include <vfw_checkpoints.h>
#endif

#ifdef _MSC_VER
extern "C" int64_t vfwGetReferenceTime(void);
typedef SOCKADDR_IN sockaddr_in;
typedef int32_t socklen_t;
#else
#include <vfw_time.h>
#include <sys/time.h>
#ifndef WIN32
#include <sys/ioctl.h>
#include <netinet/ip.h>
#include <netinet/tcp.h>
#include <errno.h>
#else
#include <ws2tcpip.h>
#endif
#include <stdio.h>
#include <unistd.h>
#endif

#ifdef WIN32
// WinSock doesn't use the error numbers in errno.h!
#undef EWOULDBLOCK
#undef EINPROGRESS
#undef EALREADY
#undef EISCONN
#undef ENOTCONN
#undef EMSGSIZE
#undef EINTR
#define EWOULDBLOCK WSAEWOULDBLOCK
#define EINPROGRESS WSAEINPROGRESS
#define EALREADY    WSAEALREADY
#define EISCONN     WSAEISCONN
#define ENOTCONN    WSAENOTCONN
#define EMSGSIZE    WSAEMSGSIZE
#define EINTR       WSAEINTR
#define SHUT_RDWR   2
#define close(a) closesocket(a)
#define ioctl(a,b,c) ioctlsocket(a,b,c)
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

  /******************************************************************************
  * Constructor
  ******************************************************************************/
  AbstractBasicIP::AbstractBasicIP(ConnectionControlBlock * const  ptrToCCB, const uint8_t numOfConnections) : 
    connection(ptrToCCB),
    numConnections(numOfConnections),
    initDone(false),
    ProcComponent(atcBasicIpId, "BasicIP", "BIP")
  {
    if (coreBasicIPInstancePtr != static_cast<AbstractBasicIP*>(NULL))
    {
      // Error to event handler
      aosHalt(__FILE__, __LINE__, "BasicIp Constructor already instantiated");
    }

    // Setup single instance pointer for core access
    coreBasicIPInstancePtr = this;
  }

  /******************************************************************************
  * initIPEnvironment
  ******************************************************************************/
  bool AbstractBasicIP::initIPEnvironment(void)
  {
    bool retVal;
#ifdef WIN32

    WSADATA SocketImplementationData;

    if (0 == WSAStartup(0x0202U, &SocketImplementationData))
    {
      retVal = true;
    }
    else
    {
      retVal = false;
    }

#else

    retVal = true;

#endif

    return retVal;
  }

  /******************************************************************************
  * init
  ******************************************************************************/
  bool AbstractBasicIP::init(void)
  {
    //Initialization of IP Environment is put in init and not in initConnection
    //as it should be called only once for an environment.

    if (!initDone)
    {
      initDone = initIPEnvironment();
    }

    return initDone;
  }

  /******************************************************************************
  * run
  ******************************************************************************/
  void AbstractBasicIP::run(void)
  {
#ifndef _DISPATCHER
    static uint32_t cp = 0U; // Must be initialized to 0
    vfwVisitCheckPoint(&cp, "BIP_run");
#endif
    handleConnections();
  }

  /******************************************************************************
  * initConnection
  ******************************************************************************/
  bool AbstractBasicIP::initConnection(const uint8_t connectionId, const IPConnectionType connectionType, char_t const * const ipAddr,
    const uint16_t portNum, const uint32_t sendBufSize, const uint32_t recvBufSize)
  {
    bool success = false;

    if (connectionId < numConnections)
    {
      // Validate arguments and fill the connection Control Block
      // Return true and set connection status = Init if relevant arguments valid

      if ((0U == sendBufSize) || (0U == recvBufSize) ||
        (((AbstractBasicIP::ConnectionTypeTcpClient == connectionType) || (AbstractBasicIP::ConnectionTypeUdpClient == connectionType)) &&
        (static_cast<char_t*>(NULL) == ipAddr)) || // Check for NULL only if connection type is a client
        (0U == portNum))
      {
        writeToLog(ATC::BriefLog, "Initialization of a connection failed!, Connection Id: ", static_cast<int32_t>(connectionId), __FILE__, __LINE__);
      }
      else
      {
        success = true;

        // All good, initialize by populating the connection structure
        connection[connectionId].connectionType = connectionType;
        connection[connectionId].connectionStatus = ConnectionStatusInit;

        // Copy ip address only if connection type is client
        if ((AbstractBasicIP::ConnectionTypeTcpClient == connectionType) || (AbstractBasicIP::ConnectionTypeUdpClient == connectionType) || 
          (AbstractBasicIP::ConnectionTypeUdpHost== connectionType))
        {
          sockaddr_in& address = connection[connectionId].peerAddr.address_in;

          address.sin_family = AF_INET; //lint !e1960 Side effect of external macro
          address.sin_port = htons(portNum);
          //lint -e{586} inet_addr() is considered to be as safe as socket(), bind() etc which are allowed according to 3NSS012264D0048
          address.sin_addr.s_addr = inet_addr(ipAddr);

          if (address.sin_addr.s_addr == 0xFFFFFFFFU)
          {
            success = false;
          }
        }

        connection[connectionId].portNr = portNum;        // Port number for the host to bind to.
        connection[connectionId].idleCount = 0U;
        connection[connectionId].connectWaitCount = 0U;
        connection[connectionId].idleWaitCount = 0U;
        connection[connectionId].connectedCount = 0U;
        connection[connectionId].sendBuffSize = sendBufSize;
        connection[connectionId].recvBuffSize = recvBufSize;

        connection[connectionId].connectionStatistics.startOfConnection = 0;
        connection[connectionId].connectionStatistics.bytesReceived = 0U;
        connection[connectionId].connectionStatistics.packagesReceived = 0U;
        connection[connectionId].connectionStatistics.bytesSent = 0U;
        connection[connectionId].connectionStatistics.packagesSent = 0U;
        connection[connectionId].connectionStatistics.errorCount = 0U;
      }
    }

    return success;
  }

  /******************************************************************************
  * readBuf
  ******************************************************************************/
  uint32_t AbstractBasicIP::readBuf(const uint8_t connectionId, uint8_t *const buf, const uint32_t bufLength)
  {
    uint32_t numBytesRead = 0U;
    bool retVal = false;

    if (connectionId < numConnections)
    {
      // Validate arguments
      if (NULL != buf)
      {
        ConnectionControlBlock& currConnection = connection[connectionId];

        if (ConnectionStatusConnected == currConnection.connectionStatus)
        {
          // Read data from connection
          switch (currConnection.connectionType)
          {
          case ConnectionTypeTcpHost:   // Same for both TCP client and Host connections
          case ConnectionTypeTcpClient:
          case ConnectionTypeUdpClient:
            retVal = readSocket(currConnection.socketId, buf, bufLength, &numBytesRead);
            break;

          case ConnectionTypeUdpHost:
            retVal = readFromSocket(currConnection.socketId, buf, bufLength, &numBytesRead, &currConnection.peerAddr.address);
            break;

          default:
            break;
          }

          if (true == retVal)
          {
            if (numBytesRead > 0U)
            {
              currConnection.connectedCount = 0U;
              currConnection.connectionStatistics.bytesReceived += numBytesRead;
              currConnection.connectionStatistics.packagesReceived++;
            }
          }
          else if ((ConnectionTypeTcpClient == currConnection.connectionType) || (ConnectionTypeTcpHost == currConnection.connectionType))
          {
            // Restart only if connection is a TCP connection.

            // UDP connections read and write may fail even if connections are good! - No need to shut it down.

            shutdownSocket(currConnection.socketId);
            writeToLog(ATC::BriefLog,"Reading from an IP connection failed!", __FILE__, __LINE__);

            currConnection.connectionStatus = ConnectionStatusCloseReopen;
          }
          else
          {
            // Nothing to do in else{} as numBytesRead is set 0 by default. Added for LINT.
          }
        }
        else
        {
          // Nothing to do in else{} as numBytesRead is set 0 by default. Added for LINT.
        }
      }
      else
      {
        // Nothing to do in else{} as numBytesRead is set 0 by default. Added for LINT.
      }
    }

    // Return the number of characters received
    return numBytesRead;
  }

  /******************************************************************************
  * writeBuf
  ******************************************************************************/
  uint32_t AbstractBasicIP::writeBuf(const uint8_t connectionId, const uint8_t* const buf, const uint32_t bufLength)
  {
    uint32_t numBytesWritten = 0U;
    bool retVal = false;

    if (connectionId < numConnections)
    {
      // Validate arguments
      if (NULL != buf)
      {
        ConnectionControlBlock& currConnection = connection[connectionId];

        if (ConnectionStatusConnected == currConnection.connectionStatus)
        {
          // Read data from connection
          switch (currConnection.connectionType)
          {
          case ConnectionTypeTcpHost:   // Same for both TCP client and Host connections
          case ConnectionTypeTcpClient:
            retVal = writeSocket(currConnection.socketId, buf, bufLength, &numBytesWritten);
            break;

          case ConnectionTypeUdpHost:
          case ConnectionTypeUdpClient:
            retVal = writeToSocket(currConnection.socketId, buf, bufLength, &numBytesWritten, &currConnection.peerAddr.address);
            break;

          default:
            break;
          }

          if (true == retVal)
          {
            if (numBytesWritten > 0U)
            {
              currConnection.connectedCount = 0U;
              currConnection.connectionStatistics.bytesSent += numBytesWritten;
              currConnection.connectionStatistics.packagesSent++;
            }
          }
          else if ((ConnectionTypeTcpClient == currConnection.connectionType) || (ConnectionTypeTcpHost == currConnection.connectionType))
          {
            // Restart only if connection is a TCP connection.

            // UDP connections read and write may fail even if connections are good! - No need to shut it down.

            shutdownSocket(currConnection.socketId);

            writeToLog(ATC::DetailedLog,"Writing to an IP connection failed! Port: ", static_cast<int32_t>(currConnection.portNr),
              __FILE__, __LINE__);

            currConnection.connectionStatus = ConnectionStatusCloseReopen;
          }
          else
          {
            // Nothing in else{} as numBytesWritten is set 0 by default. Added for LINT.
          }
        }
        else
        {
          // Nothing in else{} as numBytesWritten is set 0 by default. Added for LINT.
        }
      }
      else
      {
        // Nothing in else{} as numBytesWritten is set 0 by default. Added for LINT.
      }
    } // End of if(connectionId < numConnections)

    // Return the number of characters received
    return numBytesWritten;
  }

  /******************************************************************************
  * getConnectionStatus
  ******************************************************************************/
  AbstractBasicIP::IPConnectionStatus AbstractBasicIP::getConnectionStatus(const uint8_t connectionId) const
  {
    IPConnectionStatus status;

    if (connectionId < numConnections)
    {
      status = connection[connectionId].connectionStatus;
    }
    else
    {
      status = ConnectionStatusNone;
    }

    return status;
  }

  /******************************************************************************
  * isConnectionPendingRead
  ******************************************************************************/
  bool AbstractBasicIP::isPendingRead(const uint8_t connectionId) const
  {
    bool retVal = false;
    SOCKET socketId;
    int32_t bytesRead = socketError; // Invalidating
    int32_t errorCode = 0;
    char_t buff;
    socklen_t udpPeerAddrLen = sizeof(sockaddr_in);


    if (connectionId < numConnections)
    {
      ConnectionControlBlock& currConnection = connection[connectionId];
      if (ConnectionStatusConnected == currConnection.connectionStatus)
      {
        socketId = currConnection.socketId; // get socket from connection Id

        if ((ConnectionTypeTcpHost == currConnection.connectionType) || (ConnectionTypeTcpClient == currConnection.connectionType))
        {
          //lint -e{586} recv() is used according to 3NSS012264D0048
          bytesRead = recv(socketId, &buff, 1U, MSG_PEEK);
          if (bytesRead > 0)
          {
            retVal = true;
          }
        }

        if (ConnectionTypeUdpClient == currConnection.connectionType)
        {
          //lint -e{586} recv() is used according to 3NSS012264D0048
          bytesRead = recv(socketId, &buff, 1U, MSG_PEEK);

#ifdef WIN32
          // Always return true for UDP Client... as recv with MSG_PEEK returns error in windows environment but actual 
          // read by UDP client doesn't...  So force application to read in case of UDP client. This is only for Windows environment.
          retVal = true;
#else
          retVal = false; // Check for the bytes read to get the retVal's value.
#endif
        }

        if (ConnectionTypeUdpHost == currConnection.connectionType)
        {
          //lint -e{586} recvfrom() is used according to 3NSS012264D0048
          bytesRead = recvfrom(socketId, &buff, 1U, MSG_PEEK, &currConnection.peerAddr.address, &udpPeerAddrLen);
          if (bytesRead > 0)
          {
            retVal = true;
          }
        }

        if (false == retVal)   // If not already true
        {
          if (socketError == bytesRead)
          {
            errorCode = getErrorCode();
          }

          if (bytesRead > 0)
          {
            //Bytes available to read
            retVal = true;
          }
          else if (EMSGSIZE == errorCode)
          {
            // Bytes read and it was more than 1 byte(buffer size is only 1 byte). Mostly for UDP connection.
            retVal = true;
          }
          else
          {
            //No bytes to read OR connection not connected OR Socket error.
            retVal = false;
          }
        }
      }
    }// End of if(connectionId < numConnections)

    return retVal;
  }

  /******************************************************************************
  * closeReopenConnection
  ******************************************************************************/
  void AbstractBasicIP::closeReopen(const uint8_t connectionId)
  {
    if (connectionId < numConnections)
    {
      SOCKET socketId;

      if (connection[connectionId].connectionType == ConnectionTypeTcpHost)
      {
        socketId = connection[connectionId].listenSocketId;
      }
      else
      {
        socketId = connection[connectionId].socketId;
      }

      shutdownSocket(socketId);
      writeToLog(ATC::BriefLog,"Shutting down a TCP Host IP connection!", __FILE__, __LINE__);

      connection[connectionId].connectionStatus = ConnectionStatusCloseReopen;
    }
  }


  /******************************************************************************
  * handleConnections
  ******************************************************************************/
  void AbstractBasicIP::handleConnections()
  {
    uint8_t connectionCount;

    // for each connection
    for (connectionCount = 0U; connectionCount < numConnections; connectionCount++)
    {
      handleConnection(connectionCount);
    }
  }

  /******************************************************************************
  * handleConnection
  ******************************************************************************/
  void AbstractBasicIP::handleConnection(const uint8_t connectionId)
  {
    //Handle only if state is initialized or above! Initialization is done in the Init connection function.
    if (ConnectionStatusNone != connection[connectionId].connectionStatus)
    {
      switch (connection[connectionId].connectionType)
      {
      case  ConnectionTypeTcpHost:
        handleTcpHostConnection(connectionId);
        break;
      case ConnectionTypeTcpClient:
        handleTcpClientConnection(connectionId);
        break;
      case ConnectionTypeUdpHost:
        handleUdpHostConnection(connectionId);
        break;
      case ConnectionTypeUdpClient:
        handleUdpClientConnection(connectionId);
        break;
      default:
        // unknown
        break;
      }
    }//End of if(ConnectionStatusNone)
  }

  /******************************************************************************
  * handleTcpHostConnection
  ******************************************************************************/
  void AbstractBasicIP::handleTcpHostConnection(const uint8_t connectionId)
  {
    switch (connection[connectionId].connectionStatus)
    {
    case ConnectionStatusNone:
    {
      // Never reached here...as checked for this before calling handle functions. Added for LINT!
      break;
    }

    case ConnectionStatusInit:
    {
      if (openSocket(connection[connectionId].listenSocketId, static_cast<int32_t>(SOCK_STREAM)))  // Socket opened will be listen socket!
      {
        setNonBlockingSocket(connection[connectionId].listenSocketId);
        connection[connectionId].connectionStatus = ConnectionStatusBind;
      }
      else
      {
        writeToLog(ATC::BriefLog,"Opening of a TCP Host IP connection failed!", __FILE__, __LINE__);
      }
      break;
    }

    case ConnectionStatusBind:
    {
      if (bindSocket(connection[connectionId].listenSocketId, connection[connectionId].portNr))
      {
        connection[connectionId].connectionStatus = ConnectionStatusListen;
      }
      else
      {
        writeToLog(ATC::BriefLog,"Binding of a TCP Host IP connection failed!", __FILE__, __LINE__);
      }
      break;
    }

    case ConnectionStatusListen:
    {
      if (listenSocket(connection[connectionId].listenSocketId))
      {
        connection[connectionId].connectionStatus = ConnectionStatusDisconnected; //Pending accepting connection
      }
      else
      {
        writeToLog(ATC::BriefLog,"Listening of a TCP Host IP connection failed!", __FILE__, __LINE__);
      }
      break;
    }

    case ConnectionStatusDisconnected:
    {
      SOCKET acceptedSocket;

      acceptedSocket = acceptSocket(connection[connectionId].listenSocketId);

      if (static_cast<int32_t>(acceptedSocket) != socketError)
      {
        connection[connectionId].socketId = acceptedSocket;   //For Host connection, socket Id will store the socket Id of the client connection
        setNonBlockingSocket(connection[connectionId].socketId);  // For the client to which connection is accepted, make it non-blocking too
        setTCPNoDelay(connection[connectionId].socketId);     // For the client to which connection is accepted, make it No TCP delay too
        setTCPSendBufSize(connection[connectionId].socketId, connection[connectionId].sendBuffSize);
        setTCPRecvBufSize(connection[connectionId].socketId, connection[connectionId].recvBuffSize);
        connection[connectionId].connectionStatistics.startOfConnection = vfwGetReferenceTime();
        connection[connectionId].connectionStatus = ConnectionStatusConnected;    //connected to acceptedSocket
      }
      else
      {
        //Nothing to do in else...keep listening till anything to accept
      }
      break;
    }

    case ConnectionStatusConnected:
    {

      //Accepting the connection if any new connection attempt is done from client
      //Existing connection will be closed and new connection will be accepted

      SOCKET acceptedSocket;

      acceptedSocket = acceptSocket(connection[connectionId].listenSocketId);

      if (static_cast<int32_t>(acceptedSocket) != socketError)
      {
        // Close old socket
        shutdownSocket(connection[connectionId].socketId);
        closeSocket(connection[connectionId].socketId);
        writeToLog(ATC::BriefLog, "Shutting down a TCP Host IP connection!", __FILE__, __LINE__);

        // New socket
        connection[connectionId].socketId = acceptedSocket;   //For Host connection, socket Id will store the socket Id of the client connection
        setNonBlockingSocket(connection[connectionId].socketId);  // For the client to which connection is accepted, make it non-blocking too
        setTCPNoDelay(connection[connectionId].socketId);     // For the client to which connection is accepted, make it No TCP delay too
        setTCPSendBufSize(connection[connectionId].socketId, connection[connectionId].sendBuffSize);
        setTCPRecvBufSize(connection[connectionId].socketId, connection[connectionId].recvBuffSize);
        connection[connectionId].connectionStatistics.startOfConnection = vfwGetReferenceTime();

        connection[connectionId].connectedCount = 0U;
      }
      else if (connection[connectionId].connectedCount < recheckConnectionCount)
      {
        connection[connectionId].connectedCount++;
      }
      else
      {
        if (isConnectedSocket(connection[connectionId].socketId))
        {
          //Reset after checking connection
          connection[connectionId].connectedCount = 0U;
        }
        else
        {
          shutdownSocket(connection[connectionId].socketId);
          closeSocket(connection[connectionId].socketId);
          writeToLog(ATC::BriefLog, "Shutting down a TCP Host IP connection!", __FILE__, __LINE__);

          connection[connectionId].connectionStatus = ConnectionStatusDisconnected;
        }
      }
      break;
    }

    case ConnectionStatusCloseReopen:
    {
      closeSocket(connection[connectionId].listenSocketId);
      connection[connectionId].connectionStatus = ConnectionStatusInit;

      break;
    }

    case ConnectionStatusConnectInProgress:
    case ConnectionStatusConnectFailed:
    {
      // Not used Enum state. Added for LINT!
      break;
    }


    default:
    {
      break;
    }

    }
  }

  /******************************************************************************
  * handleTcpClientConnection
  ******************************************************************************/
  void AbstractBasicIP::handleTcpClientConnection(const uint8_t connectionId)
  {
    //For connect call
    bool waitingForConnection;
    bool retConnect;

    switch (connection[connectionId].connectionStatus)
    {
    case ConnectionStatusNone:
    {
      // Never reached here...as checked for this before calling handle functions. Added for LINT!
      break;
    }

    case ConnectionStatusInit:
    {
      if (openSocket(connection[connectionId].socketId, static_cast<int32_t>(SOCK_STREAM)))
      {
        setNonBlockingSocket(connection[connectionId].socketId);
        connection[connectionId].connectionStatus = ConnectionStatusDisconnected; // No bind or listen required by the client connection.
      }
      else
      {
        writeToLog(ATC::BriefLog,"Opening of a TCP Client IP connection failed!", __FILE__, __LINE__);
      }
      break;
    }

    case ConnectionStatusDisconnected:
    {
      retConnect = connectSocket(connection[connectionId].socketId, &connection[connectionId].peerAddr.address, &waitingForConnection);

      if (true == retConnect)
      {
        setKeepAliveSocket(connection[connectionId].socketId);
        setTCPNoDelay(connection[connectionId].socketId);
        setTCPSendBufSize(connection[connectionId].socketId, connection[connectionId].sendBuffSize);
        setTCPRecvBufSize(connection[connectionId].socketId, connection[connectionId].recvBuffSize);
        connection[connectionId].connectionStatus = ConnectionStatusConnected;
        connection[connectionId].connectWaitCount = 0U;  // Connected now! Reset wait counter.
      }
      else
      {
        if (waitingForConnection)
        {
          connection[connectionId].connectionStatus = ConnectionStatusConnectInProgress;
          connection[connectionId].connectWaitCount++;
        }
        else
        {
          closeSocket(connection[connectionId].socketId);
          writeToLog(ATC::BriefLog,"Connecting of a TCP Client IP connection failed!", __FILE__, __LINE__);
          connection[connectionId].connectionStatus = ConnectionStatusConnectFailed;
        }
      }

      break;
    }

    case ConnectionStatusConnectInProgress:
    {
      if (connection[connectionId].connectWaitCount < waitCountToRetry)
      {
        //Wait for another cycle.
        connection[connectionId].connectWaitCount++;
      }
      else
      {
        //Try to re-connect after waitCountToRetry times.
        retConnect = connectSocket(connection[connectionId].socketId, &connection[connectionId].peerAddr.address, &waitingForConnection);

        if (true == retConnect)
        {
          setKeepAliveSocket(connection[connectionId].socketId);
          setTCPNoDelay(connection[connectionId].socketId);
          setTCPSendBufSize(connection[connectionId].socketId, connection[connectionId].sendBuffSize);
          setTCPRecvBufSize(connection[connectionId].socketId, connection[connectionId].recvBuffSize);
          connection[connectionId].connectionStatus = ConnectionStatusConnected;
          connection[connectionId].connectionStatistics.startOfConnection = vfwGetReferenceTime(); 
          connection[connectionId].connectWaitCount = 0U;  // Connected now. Reset wait counter.
        }
        else
        {
          if (waitingForConnection)
          {
            //Do nothing! Wait to get connected.
            //Reset wait count to wait for a while before re-connecting!
            connection[connectionId].connectWaitCount = 0U;

            // Don't change the connection status. still in ConnectionStatusConnectInProgress
          }
          else
          {
            closeSocket(connection[connectionId].socketId);
            //Log Event or trace!
            trace.write(briefTrace, "Closing TCPClient connection with ID", static_cast<int32_t>(connectionId));
            connection[connectionId].connectionStatus = ConnectionStatusConnectFailed;
          }
        }
      }//End of else()

      break;
    }

    case ConnectionStatusConnectFailed:
    {
      if (connection[connectionId].idleWaitCount > waitCountToRetry)
      {
        //Wait count enough to try reconnecting.
        connection[connectionId].connectionStatus = ConnectionStatusInit;
        connection[connectionId].idleWaitCount = 0U;
      }
      else
      {
        connection[connectionId].idleWaitCount++;  // Increase the count
      }

      break;
    }

    case ConnectionStatusConnected:
    {
      if (connection[connectionId].connectedCount < recheckConnectionCount)
      {
        connection[connectionId].connectedCount++;
      }
      else
      {
        if (isConnectedSocket(connection[connectionId].socketId))
        {
          //Reset after checking connection
          connection[connectionId].connectedCount = 0U;
        }
        else
        {
          shutdownSocket(connection[connectionId].socketId);
          writeToLog(ATC::BriefLog,"Shutting down a TCP Client IP connection!", __FILE__, __LINE__);
          connection[connectionId].connectionStatus = ConnectionStatusCloseReopen;
        }
      }
      break;
    }

    case ConnectionStatusCloseReopen:
    {
      closeSocket(connection[connectionId].socketId);
      connection[connectionId].connectionStatus = ConnectionStatusInit;

      break;
    }

    case ConnectionStatusBind:
    case ConnectionStatusListen:
    {

      // Added for LINT! These states not used.
      break;
    }

    default:

      break;
    }
  }

  /******************************************************************************
  * handleUdpHostConnection
  ******************************************************************************/
  void AbstractBasicIP::handleUdpHostConnection(const uint8_t connectionId)
  {
    switch (connection[connectionId].connectionStatus)
    {
    case ConnectionStatusNone:
    {
      // Never reach here...as checked for this before calling handle functions. Added for LINT!
      break;
    }

    case ConnectionStatusInit:
    {
      if (openSocket(connection[connectionId].socketId, static_cast<int32_t>(SOCK_DGRAM)))
      {
        setNonBlockingSocket(connection[connectionId].socketId);
        connection[connectionId].connectionStatus = ConnectionStatusBind;
      }
      else
      {
        writeToLog(ATC::BriefLog,"Opening of a UDP Host IP connection failed!", __FILE__, __LINE__);
      }

      break;
    }

    case ConnectionStatusBind:
    {
      if (bindSocket(connection[connectionId].socketId, connection[connectionId].portNr))
      {
        // Assume UDP host to be connected after Bind... Connectionless protocol... No measure to check if connected!
        connection[connectionId].connectionStatus = ConnectionStatusConnected;
      }
      else
      {
        writeToLog(ATC::BriefLog,"Binding of a UDP Host IP connection failed!", __FILE__, __LINE__);
      }

      break;
    }

    case ConnectionStatusCloseReopen:
    case ConnectionStatusListen:
    case ConnectionStatusDisconnected:
    case ConnectionStatusConnectInProgress:
    case ConnectionStatusConnectFailed:
    case ConnectionStatusConnected:
    {
      // Added for LINT! These states not used here.
      break;
    }

    default:

      break;
    }
  }

  /******************************************************************************
  * handleUdpClientConnection
  ******************************************************************************/
  void AbstractBasicIP::handleUdpClientConnection(const uint8_t connectionId)
  {
    switch (connection[connectionId].connectionStatus)
    {
    case ConnectionStatusNone:
    {
      // Never reach here...as checked for this before calling handle functions. Added for LINT!
      break;
    }

    case ConnectionStatusInit:
    {
      if (openSocket(connection[connectionId].socketId, static_cast<int32_t>(SOCK_DGRAM)))
      {
        setNonBlockingSocket(connection[connectionId].socketId);
        // Assume UDP client to be connected after Open... Connectionless protocol... No measure to check if connected!
        connection[connectionId].connectionStatus = ConnectionStatusConnected;
      }
      else
      {
        writeToLog(ATC::BriefLog,"Opening of a UDP Client IP connection failed!", __FILE__, __LINE__);
      }

      break;
    }

    case ConnectionStatusCloseReopen:
    case ConnectionStatusBind:
    case ConnectionStatusListen:
    case ConnectionStatusDisconnected:
    case ConnectionStatusConnectInProgress:
    case ConnectionStatusConnectFailed:
    case ConnectionStatusConnected:
    {
      // Added for LINT! These states not used here.
      break;
    }

    default:

      break;
    }
  }

  /******************************************************************************
  * corePtr
  ******************************************************************************/
  AbstractBasicIP* AbstractBasicIP::corePtr(void)
  {
    return coreBasicIPInstancePtr;
  }

  /******************************************************************************
  * openSocket
  ******************************************************************************/
  bool AbstractBasicIP::openSocket(SOCKET &socketId, const int32_t socketType) const
  {
    bool retVal = true;
    int32_t protocol;

    if (static_cast<int32_t>(SOCK_STREAM) == socketType)
    {
      protocol = IPPROTO_TCP;
    }
    else if (static_cast<int32_t>(SOCK_DGRAM) == socketType)
    {
      protocol = IPPROTO_UDP;
    }
    else
    {
      // Invalidate!
      retVal = false;
      protocol = 0;
    }

    //Supporting only SOCK_STREAM and SOCK_DGRAM
    if (true == retVal)
    {
      //lint -e{586} socket() is used according to 3NSS012264D0048
      socketId = socket(AF_INET, socketType, protocol);

      if (invalidSocket == static_cast<int32_t>(socketId))
      {
        retVal = false;
      }// else already true!
    }

    return retVal;
  }

  /******************************************************************************
  * setNonBlockingSocket
  ******************************************************************************/
  void AbstractBasicIP::setNonBlockingSocket(const SOCKET socketId) const
  {
    u_long mode = 1U;

    //lint -e{586} ioctl() is used according to 3NSS012264D0048
    //lint -e{835} -e{970} -e{1924} -e{1960} Side effect of external macro
    if (ioctl(socketId, FIONBIO, &mode) < 0)
    {
      aosHalt(__FILE__, __LINE__, "ioctl() failed");
    }
  }

  /******************************************************************************
  * setSocketOption
  ******************************************************************************/
  bool AbstractBasicIP::setSocketOption(const SOCKET socketId, const int32_t level, const int32_t option, const uint32_t value) const
  {
    //lint --e{970} Necessary because setsockopt() uses int
    int intValue = static_cast<int>(value);

    //lint -e{586} setsockopt() is used according to 3NSS012264D0048
    int result = setsockopt(
      socketId,
      level,
      option,
#ifdef WIN32
      reinterpret_cast<char*>(&intValue),
      static_cast<int>(sizeof(intValue)));
#else
      &intValue,
      sizeof(intValue));
#endif

    return result == 0;
  }

  /******************************************************************************
  * setKeepAliveSocket
  ******************************************************************************/
  void AbstractBasicIP::setKeepAliveSocket(const SOCKET socketId) const
  {
    if (!setSocketOption(socketId, SOL_SOCKET, SO_KEEPALIVE, 1U))
    {
      aosHalt(__FILE__, __LINE__, "setsockopt() for SO_KEEPALIVE failed");
    }
  }

  /******************************************************************************
  * connectSocket
  ******************************************************************************/
  bool AbstractBasicIP::connectSocket(const SOCKET socketId, const sockaddr* const serverAddr, bool* const waitingForConnection) const
  {
    int32_t ret;  // for connect
    int32_t errorCode;
    bool retVal;

    //lint -e{586} connect() is used according to 3NSS012264D0048
    ret = connect(socketId, serverAddr, sizeof(*serverAddr));

    if (socketError == ret)
    {
      //Error in connecting socket or waiting to connect or already connected?
      errorCode = getErrorCode();
      if ((EWOULDBLOCK == errorCode) || (EINPROGRESS == errorCode) || (EALREADY == errorCode))
      {
        *waitingForConnection = true;
        retVal = false;
      }
      else if (EISCONN == errorCode)
      {
        //Already connected
        *waitingForConnection = false;
        retVal = true;
      }
      else
      {
        // Sorry! Can't connect
        *waitingForConnection = false;
        retVal = false;

        // Report an error? Maybe!
      }
    }
    else
    {
      *waitingForConnection = false;
      retVal = true;
    }

    return retVal;
  }

  /******************************************************************************
  * setTCPNoDelay
  ******************************************************************************/
  void AbstractBasicIP::setTCPNoDelay(const SOCKET socketId) const
  {
    if (!setSocketOption(socketId, IPPROTO_TCP, TCP_NODELAY, 1U))
    {
      aosHalt(__FILE__, __LINE__, "setsockopt() for TCP_NODELAY failed");
    }
  }

  /******************************************************************************
  * setTCPSendBufSize
  ******************************************************************************/
  void AbstractBasicIP::setTCPSendBufSize(const SOCKET socketId, const uint32_t buffSize) const
  {
    if (!setSocketOption(socketId, SOL_SOCKET, SO_SNDBUF, buffSize))
    {
      aosHalt(__FILE__, __LINE__, "setsockopt() for SO_SNDBUF failed");
    }
  }

  /******************************************************************************
  * setTCPRecvBufSize
  ******************************************************************************/
  void AbstractBasicIP::setTCPRecvBufSize(const SOCKET socketId, const uint32_t buffSize) const
  {
    if (!setSocketOption(socketId, SOL_SOCKET, SO_RCVBUF, buffSize))
    {
      aosHalt(__FILE__, __LINE__, "setsockopt() for SO_RCVBUF failed");
    }
  }

  /******************************************************************************
  * isConnectedSocket
  ******************************************************************************/
  bool AbstractBasicIP::isConnectedSocket(const SOCKET socketId) const
  {
    bool retVal;
    int32_t errorCode;
    int32_t error;
    char_t buff;

    //lint -e{586} recv() is used according to 3NSS012264D0048
    error = recv(socketId, &buff, 1U, MSG_PEEK);

    if (socketError == error)
    {
      errorCode = getErrorCode();

      if (EWOULDBLOCK == errorCode)
      {
        retVal = true;
      }
      else
      {
        retVal = false;
      }
    }
    else if (0 == error)
    {
      //Should have returned 1
      retVal = false;
    }
    else
    {
      retVal = true;
    }

    return retVal;
  }

  /******************************************************************************
  * shutdownSocket
  ******************************************************************************/
  void AbstractBasicIP::shutdownSocket(const SOCKET socketId) const
  {
    //lint -e{586} shutdown() is considered to be as safe as socket(), bind() etc which are allowed according to 3NSS012264D0048
    if (shutdown(socketId, SHUT_RDWR) != 0)  //Shut down both send and receive operation
    {
      const int32_t errorCode = getErrorCode();
      if (errorCode != ENOTCONN)
      {
        writeToLog(ATC::BriefLog, "shutdown() failed", __FILE__, __LINE__);
      }
    }
  }

  /******************************************************************************
  * closeSocket
  ******************************************************************************/
  void AbstractBasicIP::closeSocket(const SOCKET socketId) const
  {
    //lint -e{586} close() is used according to 3NSS012264D0048
    if (close(socketId) != 0)
    {
      const int32_t errorCode = getErrorCode();
      if ((errorCode != EINTR)
#ifndef WIN32
        && (errorCode != EIO)
#endif
        )
      {
        writeToLog(ATC::BriefLog, "close() failed", __FILE__, __LINE__);
      }
    }
  }

  /******************************************************************************
  * bindSocket
  ******************************************************************************/
  bool AbstractBasicIP::bindSocket(const SOCKET socketId, const uint16_t portNumber) const
  {
    bool retVal;
    SocketAddress addr;

    addr.address_in.sin_family = AF_INET; //lint !e1960 Side effect of external macro
    addr.address_in.sin_port = htons(portNumber);
    addr.address_in.sin_addr.s_addr = INADDR_ANY; //lint !e1924 Side effect of external macro

    //lint -e{586} bind() is used according to 3NSS012264D0048
    if (socketError == bind(socketId, &addr.address, sizeof(addr.address_in)))
    {
      retVal = false;
    }
    else
    {
      retVal = true;
    }

    return retVal;
  }

  /******************************************************************************
  * listenSocket
  ******************************************************************************/
  bool AbstractBasicIP::listenSocket(const SOCKET socketId) const
  {
    bool retVal;

    //lint -e{586} listen() is considered to be as safe as socket(), bind() etc which are allowed according to 3NSS012264D0048
    if (socketError == listen(socketId, 2)) // Both read and write Mode
    {
      retVal = false;
    }
    else
    {
      retVal = true;
    }

    return retVal;
  }

  /******************************************************************************
  * acceptSocket
  ******************************************************************************/
  SOCKET AbstractBasicIP::acceptSocket(const SOCKET socketId) const
  {
    SOCKET connSock;

    //lint -e{586} accept() is considered to be as safe as socket(), bind() etc which are allowed according to 3NSS012264D0048
    connSock = accept(socketId, static_cast<sockaddr *>(NULL), static_cast<socklen_t *>(NULL));

    return connSock;
  }

  /******************************************************************************
  * readSocket
  ******************************************************************************/
  bool AbstractBasicIP::readSocket(const SOCKET socketId, uint8_t* const pBuff, const uint32_t maxBytes, uint32_t* const pBytesRead) const
  {
    bool retVal;
    int32_t bytesRead;
    int32_t errorCode;

    if ((NULL != pBuff) && (NULL != pBytesRead))// Sanity check!
    {
      //lint -e{926} Cast is unavoidable here
      //lint -e{586} recv() is used according to 3NSS012264D0048
      bytesRead = recv(socketId, reinterpret_cast<char_t*>(pBuff), maxBytes, 0);

      if (bytesRead > 0)
      {
        *pBytesRead = static_cast<uint32_t>(bytesRead);
        retVal = true;
      }
      else if (0 == bytesRead)
      {
        *pBytesRead = static_cast<uint32_t>(bytesRead);
        retVal = false;   // Nothing to read!
      }
      else
      {
        errorCode = getErrorCode();
        if (EWOULDBLOCK == errorCode)  // Receive option would block! Not able to read values. Connection Ok!
        {
          *pBytesRead = 0U;
          retVal = true;    //To avoid reconnection!
        }
        else
        {
          // Failed to receive data.
          *pBytesRead = 0U;
          retVal = false;
        }
      }
    }
    else
    {
      retVal = false;
    }

    return retVal;
  }

  /******************************************************************************
  * readFromSocket
  ******************************************************************************/
  bool AbstractBasicIP::readFromSocket(const SOCKET socketId, uint8_t* const pBuff, const uint32_t maxBytes,
    uint32_t* const pBytesRead, sockaddr* const pUdpPeerAddr) const
  {
    bool retVal;
    int32_t bytesRead;
    int32_t errorCode;
    socklen_t udpPeerAddrLen;

    if ((NULL != pBuff) && (NULL != pBytesRead) && (NULL != pUdpPeerAddr))  // Sanity check!
    {
      udpPeerAddrLen = sizeof(sockaddr_in);

      //lint -e{926} Cast is unavoidable here
      //lint -e{586} recvfrom() is used according to 3NSS012264D0048
      bytesRead = recvfrom(socketId, reinterpret_cast<char_t*>(pBuff), maxBytes, 0, pUdpPeerAddr, &udpPeerAddrLen);

      if (bytesRead >= 0)
      {
        *pBytesRead = static_cast<uint32_t>(bytesRead);
        retVal = true;
      }
      else
      {
        errorCode = getErrorCode();
        if (EWOULDBLOCK == errorCode)
        {
          *pBytesRead = 0U;
          retVal = true;
        }
        else
        {
          *pBytesRead = 0U;
          retVal = false;
        }
      }
    }
    else
    {
      retVal = false;
    }

    return retVal;
  }

  /******************************************************************************
  * writeSocket
  ******************************************************************************/
  bool AbstractBasicIP::writeSocket(const SOCKET socketId, const uint8_t* const pBuff, const uint32_t maxBytes,
    uint32_t* const pBytesWritten) const
  {
    bool retVal;
    int32_t bytesWritten;
    int32_t errorCode;

    if ((NULL != pBuff) && (NULL != pBytesWritten))  // Sanity check!
    {
      //lint -e{926} Cast is unavoidable here
      //lint -e{586} send() is used according to 3NSS012264D0048
      bytesWritten = send(socketId, reinterpret_cast<const char_t*>(pBuff), maxBytes, 0);

      if (bytesWritten >= 0)
      {
        *pBytesWritten = static_cast<uint32_t>(bytesWritten);
        retVal = true;
      }
      else
      {
        errorCode = getErrorCode();
        if (EWOULDBLOCK == errorCode)
        {
          *pBytesWritten = 0U;
          retVal = true;
        }
        else
        {
          *pBytesWritten = 0U;
          retVal = false;
        }
      }
    }
    else
    {
      retVal = false;
    }

    return retVal;
  }

  /******************************************************************************
  * writeToSocket
  ******************************************************************************/
  bool AbstractBasicIP::writeToSocket(const SOCKET socketId, const uint8_t* const pBuff, const uint32_t maxBytes,
    uint32_t* const pBytesWritten, const sockaddr* const pUdpPeerAddr) const
  {
    bool retVal;
    int32_t bytesWritten;
    int32_t errorCode;

    //lint -e{926} Cast is unavoidable here
    //lint -e{586} sendto() is used according to 3NSS012264D0048
    bytesWritten = sendto(socketId, reinterpret_cast<const char_t*>(pBuff), maxBytes, 0, pUdpPeerAddr, sizeof(sockaddr_in));

    if (bytesWritten >= 0)
    {
      *pBytesWritten = static_cast<uint32_t>(bytesWritten);
      retVal = true;
    }
    else
    {
      errorCode = getErrorCode();
      if (EWOULDBLOCK == errorCode)
      {
        *pBytesWritten = 0U;
        retVal = true;
      }
      else
      {
        *pBytesWritten = 0U;
        retVal = false;
      }
    }

    return retVal;
  }

  /******************************************************************************
  * consoleCall
  ******************************************************************************/
  bool AbstractBasicIP::consoleCall(const uint32_t argc, const ConsoleArguments argv)
  {
    bool retVal = false;

    // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
    if (isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
    {
      const char_t* const toWrite = "bip           To get information of all the basic ip connections.";

      AbstractConsole::corePtr()->writeWithNewline(toWrite);
    }
    else if (isTextMatch(&argv[0][0], "bip", sizeof("bip")))
    {
      int64_t thisTime = vfwGetReferenceTime();

      switch (argc)
      {
      case 1:

        AbstractConsole::corePtr()->writeWithNewline("BasicIP Statistics ----------------------------------------------------------------");
        AbstractConsole::corePtr()->writeWithNewline("No Id         Port  Type State Recv(bytes)( reads) Sent(bytes)(writes) Err Duration");

        for (uint8_t ci = 0U; ci < getMaxConnections(); ci++)
        {
          if (connection[ci].connectionStatus >= ConnectionStatusInit)
          {
            char_t displayBuf[120];
            collectConnectionStatistics(ci, &displayBuf[0], sizeof(displayBuf), thisTime);
            AbstractConsole::corePtr()->writeWithNewline(&displayBuf[0]);
          }
        }

        retVal = true;
        break;

      default:
        AbstractConsole::corePtr()->writeWithNewline("Illegal Argument: bip takes 0 argument");

      }//End of switch(argc)

    }
    else
    {
      // Do nothing
    }

    return retVal;

  }
  /******************************************************************************
  * connectionTypeStr
  *
  ******************************************************************************/
  const char_t *AbstractBasicIP::connectionTypeStr(const IPConnectionType connType) const
  {
    const char_t *connTypeStr;
    switch (connType)
    {
    case ConnectionTypeTcpHost:
      connTypeStr = "Host";
      break;
    case ConnectionTypeTcpClient:
      connTypeStr = "Clnt";
      break;
    case ConnectionTypeUdpHost:
      connTypeStr = "UDPH";
      break;
    case ConnectionTypeUdpClient:
      connTypeStr = "UDPC";
      break;
    default:
      connTypeStr = "?!";
      break;
    }
    return connTypeStr;
  }
  /******************************************************************************
  * connectionStatusStr
  *
  ******************************************************************************/
  const char_t *AbstractBasicIP::connectionStatusStr(const IPConnectionStatus status) const
  {
    const char_t *statusStr;
    switch (status)
    {
    case ConnectionStatusNone:
      statusStr = "None";
      break;
    case ConnectionStatusInit:
      statusStr = "Init";
      break;
    case ConnectionStatusBind:
      statusStr = "Bind";
      break;
    case ConnectionStatusListen:
      statusStr = "Listn";
      break;
    case ConnectionStatusDisconnected:
      statusStr = "Disc";
      break;
    case ConnectionStatusConnectInProgress:
      statusStr = "Con..";
      break;
    case ConnectionStatusConnectFailed:
      statusStr = "Con!!";
      break;
    case ConnectionStatusConnected:
      statusStr = "Conn";
      break;
    case ConnectionStatusCloseReopen:
      statusStr = "Reop";
      break;
    default:
      statusStr = "Undef";
      break;

    }
    return statusStr;

  }
  /******************************************************************************
  * collectConnectionStatistics
  *
  ******************************************************************************/
  void AbstractBasicIP::collectConnectionStatistics(const uint8_t connectionId, char_t *const displayBuf,
    const size_t bufSize, const int64_t thisTime)
  {
    char_t durationStr[20];

    int32_t res;

    if (connection[connectionId].connectionStatus == ConnectionStatusConnected)
    {
      /* PlatformTime in milliseconds */
      const int32_t totalDurationSecs = static_cast<int32_t>((thisTime - connection[connectionId].connectionStatistics.startOfConnection) / 1000);
      const int32_t durationHours = totalDurationSecs / 3600;
      const int32_t durationMins = (totalDurationSecs % 3600) / 60;
      const int32_t durationSecs = (totalDurationSecs % 3600) % 60;

      //lint -e{586} snprintf is needed here
      res = snprintf(&durationStr[0], sizeof(durationStr), "%03d:%02d:%02d", durationHours, durationMins, durationSecs);

      if ((res < 0) || (static_cast<size_t>(res) >= sizeof(durationStr)))
      {
        durationStr[0] = '\0';
      }
    }
    else
    {
      durationStr[0] = '\0';
    }

    //lint -e{586} snprintf is needed here
    res = snprintf(displayBuf, bufSize, "%02u %-10s %5u %-4s %-5s %11u %7u %11u %7u %3u %s",
      connectionId,
      connectionIdStr(connectionId),
      connection[connectionId].portNr,
      connectionTypeStr(connection[connectionId].connectionType),
      connectionStatusStr(connection[connectionId].connectionStatus),
      connection[connectionId].connectionStatistics.bytesReceived, connection[connectionId].connectionStatistics.packagesReceived,
      connection[connectionId].connectionStatistics.bytesSent, connection[connectionId].connectionStatistics.packagesSent,
      connection[connectionId].connectionStatistics.errorCount, durationStr);

    if ((res < 0) || (static_cast<size_t>(res) >= bufSize))
    {
      displayBuf[0] = '\0';
    }

  }

  int32_t AbstractBasicIP::getErrorCode() const
  {
    int32_t retVal;
#ifdef WIN32
    retVal = WSAGetLastError();
#else
    retVal = errno;
#endif
    return retVal;
  }
}
