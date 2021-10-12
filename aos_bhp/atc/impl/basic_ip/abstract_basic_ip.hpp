#ifndef AbstractBasicIP_hpp
#define AbstractBasicIP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This abstract class implements the core functionality of BasicIP
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-09    bhermans    Created
* 2016-03-15    bhermans    Inherits from ATC::ProcComponent
* 2016-03-16    bhermans    Const Pointer to control-blocks now assigned in constructor init-list
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-04-21    lantback    Make abstract constructor protected
* 2016-04-21    lantback    Implemented corePtr()
* 2016-06-27    adgupta     Implementation of BasicIP functionality
* 2016-09-08    adgupta     Implementation Console call for basic IP.
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-23    arastogi    Removed ATC::
* 2016-09-23    bhermans    bip console-call improved
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "event.hpp"
#include "atc_base.hpp"

#ifdef WIN32
#include <winsock2.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#endif


/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATC
{
#ifndef WIN32
  typedef int32_t SOCKET;
#endif

  /** Wait for this number of cycles to re-connect. */
  static const uint8_t waitCountToReconnect = 2U;
  /** Wait for this number of cycles to retry connection. */
  static const uint8_t waitCountToRetry = 5U;
  /** Wait for this number of cycles after last communication to check if connection is still alive */
  static const uint8_t recheckConnectionCount = 10U;

  /** Error value for Socket connection */
  static const int32_t socketError = -1;
  /** Invalid Socket value */
  static const int32_t invalidSocket = -1;

  class AbstractBasicIP;
  /**
  * Static variable to store the single instance of AbstractBasicIP
  *
  * Variable shall be setup during construction of the single instance used within ATP.
  * The variable is returned by corePtr() and used by the core ATP logic to access
  * adaptation objects through the core class.
  *
  * Note: During construction the variable shall be checked to guarantee that only
  *       one instance is created. Should the variable be set to non-zero the execution shall
  *       be immediately interrupted and a safe state issued.
  */
  static AbstractBasicIP* coreBasicIPInstancePtr = static_cast<AbstractBasicIP*>(NULL);

  /**
  * The class AbstractBasicIP implements the interface defined by the ProcComponent class.
  *
  */
  class AbstractBasicIP : public ProcComponent
  {
  public:

    /**
    * The connection types supported by BasicIP
    *
    */
    enum IPConnectionType
    {
      ConnectionTypeTcpHost,    //!< TCP Host connection
      ConnectionTypeTcpClient,  //!< TCP Client connection
      ConnectionTypeUdpHost,    //!< UDP Host connection
      ConnectionTypeUdpClient   //!< UDP Client connection
    };

    /**
    * The connection status of a connection as defined by BasicIP
    *
    */
    enum IPConnectionStatus
    {
      ConnectionStatusNone,               //!< Not yet initialized
      ConnectionStatusInit,               //!< Connection Initialized
      ConnectionStatusBind,               //!< Connection Bind to socket. For Host connections
      ConnectionStatusListen,             //!< Listening socket for packets. For Host connections
      ConnectionStatusDisconnected,       //!< Disconnected/yet to be started
      ConnectionStatusConnectInProgress,  //!< Connection in progress
      ConnectionStatusConnectFailed,      //!< Connection Failed
      ConnectionStatusConnected,          //!< Connected. Connection successful.
      ConnectionStatusCloseReopen         //!< Close and Re-open the connection
    };

    /**
    * To store statistics of a particular connection
    *
    */
    struct ConnectionStatistics
    {
      int64_t  startOfConnection; //!< Time when connection started
      uint32_t bytesReceived;     //!< Number of bytes received by the connection
      uint32_t packagesReceived;  //!< Number of packages received by the connection
      uint32_t bytesSent;         //!< Number of bytes sent by the connection
      uint32_t packagesSent;      //!< Number of packages sent by the connection
      uint32_t errorCount;        //!< Number of error count
    };

    /**
    * Connection Id(s) for the IP connections
    */
    static const uint8_t connectionConsole = 0U;        //!< Connection for ATP Console
    static const uint8_t connectionNJRU    = 1U;        //!< Connection to NJRU-logger 
    static const uint8_t connectionRU      = 2U;        //!< Connection to RU-logger

    static const uint8_t maxCoreConnections =3U;        //!< maximum number of Core connections

    /**
    * The socket address, both in generic and in IP format
    */
    //lint -e{1960} Using a union for this eliminates a lot of pointer casts
    typedef union
    {
      sockaddr address;  //!< Generic Struct for any kind of socket operation
      sockaddr_in address_in; //!<  Struct specific to IP-based communication
    } SocketAddress;

    /**
    * The control block associated with each connection in BasicIP
    *
    */
    struct ConnectionControlBlock
    {
      IPConnectionType     connectionType;       //!< Type of connection (TCP or UDP?, Host or Client?)
      IPConnectionStatus   connectionStatus;     //!< Status of this connection (None, Init,...,Connected. etc..) 
      SocketAddress        peerAddr;             //!< IP address of the peer (only valid for TCP/UDP client and UDP host)
      uint16_t             portNr;               //!< Port nr to bind to (if connectionType is Host)
      SOCKET               socketId;             //!< Socket connection Id. Connection to connect to
      SOCKET               listenSocketId;       //!< Socket Id for host to listen for connections.
      uint16_t             idleCount;            //!< Number of counts being idle before trying re-initialization
      uint16_t             connectWaitCount;     //!< Number of counts waiting for connection for connect() for TCP client
      uint16_t             idleWaitCount;        //!< Number of counts waiting for connection for retrying connection for TCP client
      uint16_t             connectedCount;       //!< Number of counts since last communication from the socket
      uint32_t             sendBuffSize;         //!< Sending Buffer Size
      uint32_t             recvBuffSize;         //!< Receiving Buffer size
      ConnectionStatistics connectionStatistics; //!< statistics of the connections(Time, error count,..., bytes sent, etc...)
    };

    /**
    * Initialize the Basic IP control blocks and IP environment.
    *
    * @return true if successful
    */
    virtual bool init(void);

    /**
    * The run function must be called each cycle by the scheduler.
    *
    */
    virtual void run(void);

    /**
    * Get core instance pointer
    *
    * @return Pointer to single instance core object.
    */
    static AbstractBasicIP* corePtr();

    /**
    * Must be called for each connection to be handled by BasicIP.
    *
    *  @param[in] connectionId   An id identifying the connection to be established and it must be less than numConnections
    *  @param[in] connectionType The type of connection to be established
    *  @param[in] ipAddr         The ipAddr needed for the connection (usage depends on connectionType)
    *  @param[in] portNum         The portNr needed for the connection (usage depends on connectionType)
    *  @param[in] sendBufSize    Size of Tcp send buffer
    *  @param[in] recvBufSize    Size of Tcp receive buffer
    *
    *  @return true if the arguments are accepted
    */
    bool initConnection(const uint8_t connectionId, const IPConnectionType connectionType, char_t const * const ipAddr,
      const uint16_t portNum, const uint32_t sendBufSize, const uint32_t recvBufSize);

    /**
    * Reads a data-buffer from the connection handled by BasicIP
    *
    *  @param[in] connectionId   An id identifying the connection
    *  @param[in] buf            A pointer to the data-buffer
    *  @param[in] bufLength      The length of the data-buffer
    *
    *  @return the no of characters received
    */
    uint32_t readBuf(const uint8_t connectionId, uint8_t *const buf, const uint32_t bufLength);

    /**
    * Sends a data-buffer on the connection handled by BasicIP
    *
    *  @param[in] connectionId   An id identifying the connection
    *  @param[in] buf            A pointer to the data-buffer
    *  @param[in] bufLength      The length of the data-buffer
    *
    *  @return the no of characters sent
    */
    uint32_t writeBuf(const uint8_t connectionId, const uint8_t* const buf, const uint32_t bufLength);

    /**
    * Returns the status of a connection handled by BasicIP
    *
    *  @param[in] connectionId   An id identifying the connection
    *
    *  @return the connectionStatus of the connection
    */
    IPConnectionStatus getConnectionStatus(const uint8_t connectionId) const;

    /**
    * Returns if the connection has anything to be read
    * The using component will call the corresponding function in the adapted inherited class where connectionId is defined as an enum.
    *
    *  @param[in] connectionId   An id identifying the connection
    *
    *  @return true is something is to be read of the connection
    */
    bool isPendingRead(const uint8_t connectionId) const;

    /**
    * Force a close/reopen of a connection handled by BasicIP
    * The using component will call the corresponding function in the adapted inherited class where connectionId is defined as an enum.
    *
    * @param[in] connectionId   An id identifying the connection
    */
    void closeReopen(uint8_t connectionId);

    /**
    * This functions parses the arguments searches for the "help", or any other Console
    * component specific command calls and handles it. Returns true if completely handled
    * else returns false. returning false will let other components handle the call. help always returns false.
    *
    * @param[in] argc - Number of arguments in the argument array argv
    *
    * @param[in] argv - Arguments array
    *
    * @return - returns true if handled successfully, except "help"(it always returns false)
    */
    virtual bool consoleCall(const uint32_t argc, const ConsoleArguments argv);

    /**
    * Translate connection id to printable string
    *
    * This is a pure virtual method. Needs to be implemented in the adaptation.
    *
    * @param[in] connectionId An id identifying the connection
    *
    * @returns the connection id as an abbreviated character string
    */
    virtual const char_t *connectionIdStr(const uint8_t connectionId) = 0;

  protected:
    /**
    * Constructor for the common abstract BasicIP
    *
    *  @param[in] ptrToCCB  A pointer to the ConnectionControlBlocks. Will be assigned to the const connection-pointer in the initializer-list of the constructor.
    *  @param[in] numOfConnections Number of connections to be handled by Basic IP 
    */
    AbstractBasicIP( ConnectionControlBlock * const ptrToCCB, const uint8_t numOfConnections);

    /**
    * Initialization of the IP environment. Common for most adaptations but may be overridden if necessary.
    *
    */
    virtual bool initIPEnvironment();

    /**
    * Get max connections available
    *
    * This is a pure virtual method. Needs to be implemented in the adaptation.
    *
    * @returns the max connections available
    */
    virtual uint8_t getMaxConnections() = 0;

    /**
    * Get Error Code
    *
    */
    int32_t getErrorCode() const;

  private:

    /**
    * Default Constructor for the Abstract BasicIP
    *
    */
    AbstractBasicIP();
    
    /**
    * One control block is associated with each connection
    */
    ConnectionControlBlock * const connection;

    /**
    * Number of connections to be handled by Basic IP
    *
    */
    const uint8_t numConnections;

    /**
    * Flag to prevent multiple initialization.
    */
    bool initDone;

    /**
    * Handle all the connections.
    *
    */
    void handleConnections();


    /**
    * Handle the connection identified by the connectionId.
    *
    *  @param[in] connectionId   An id identifying the connection
    *
    */
    void handleConnection(const uint8_t connectionId);


    /**
    * Handle the Tcp Host connection identified by the connectionId.
    *
    *  @param[in] connectionId   An id identifying the connection
    *
    */
    void handleTcpHostConnection(const uint8_t connectionId);


    /**
    * Handle the Tcp Client connection identified by the connectionId.
    *
    *  @param[in] connectionId   An id identifying the connection
    *
    */
    void handleTcpClientConnection(const uint8_t connectionId);


    /**
    * Handle the Udp Host connection identified by the connectionId.
    *
    *  @param[in] connectionId   An id identifying the connection
    *
    */
    void handleUdpHostConnection(const uint8_t connectionId);


    /**
    * Handle the Udp Client connection identified by the connectionId.
    *
    *  @param[in] connectionId   An id identifying the connection
    *
    */
    void handleUdpClientConnection(const uint8_t connectionId);


    /**
    * Open the socket connection
    *
    * @param[in] socketId Socket Id of the connection
    *
    * @param[in] socketType Type of socket connection
    *
    * @return true if socket opened correctly
    */
    bool openSocket(SOCKET &socketId, const int32_t socketType) const;


    /**
    * Set the socket to Non-Blocking
    *
    * @param[in] socketId socket Id of the connection
    *
    * @return true if socket set correctly
    */
    void setNonBlockingSocket(const SOCKET socketId) const;

    /**
    * Sets an option for the given socket.
    *
    * @param[in] socketId  socket Id of the connection
    * @param[in] level     the level of the option (SOL_SOCKET, IPPROTO_TCP, etc)
    * @param[in] option    the option to set (SO_KEEPALIVE, TCP_NODELAY, etc)
    * @param[in] value     the value to set the option to
    *
    * @return true if the option was set successfully
    */
    bool setSocketOption(const SOCKET socketId, const int32_t level, const int32_t option, const uint32_t value) const;


    /**
    * Set the socket to keep alive
    *
    * @param[in] socketId socket Id of the connection
    */
    void setKeepAliveSocket(const SOCKET socketId) const;


    /**
    * Connect the socket to server
    *
    * @param[in] socketId socket Id to connect
    *
    * @param[in] serverAddr address of server to connect to
    *
    * @param[out] waitingForConnection connection is waiting
    *
    * @returns true when connect succeded
    */
    bool connectSocket(const SOCKET socketId, const sockaddr* const serverAddr, bool* const waitingForConnection) const;


    /**
    * Set No delay to TCP connections
    *
    * @param[in] socketId Socket Id of the connection to set the No Delay.
    *            It should be a TCP connection Socket.
    */
    void setTCPNoDelay(const SOCKET socketId) const;


    /**
    * Set the TCP buffer size for sending
    *
    * @param[in] socketId Socket Id of the connection to set the buffer size.
    * @param[in] buffSize The buffer size to set.
    */
    void setTCPSendBufSize(const SOCKET socketId, const uint32_t buffSize) const;


    /**
    * Set the TCP buffer size for receiving
    *
    * @param[in] socketId Socket Id of the connection to set the buffer size.
    * @param[in] buffSize The buffer size to set.
    */
    void setTCPRecvBufSize(const SOCKET socketId, const uint32_t buffSize) const;


    /**
    * Check if the socket is still connected
    *
    * @param[in] socketId Socket Id to check the connection of
    *
    * @return true if socket is still connected
    */
    bool isConnectedSocket(const SOCKET socketId) const;


    /**
    * Shut down the socket
    *
    * @param[in] socketId Socket Id of the socket to be shut down
    */
    void shutdownSocket(const SOCKET socketId) const;


    /**
    * close the socket
    *
    * @param[in] socketId Socket Id of the socket to be closed
    */
    void closeSocket(const SOCKET socketId) const;


    /**
    * bind the socket
    *
    * @param[in] socketId Socket Id of the socket to be binded
    *
    * @param[in] portNumber port number for the socket to bind to
    *
    * @return true if bind succeeds
    */
    bool bindSocket(const SOCKET socketId, const uint16_t portNumber) const;


    /**
    * listen to a socket Id
    *
    * @param[in] socketId Id of the socket to listen
    *
    * @return true if listen succeeds
    */
    bool listenSocket(const SOCKET socketId) const;


    /**
    * accept connection by a client connection
    *
    * @param[in] socketId Id of the host socket to be able to connect to
    *
    * @return socket Id of the connection connected
    */
    SOCKET acceptSocket(const SOCKET socketId) const;


    /**
    * Read from a connection based communication socket
    *
    * @param[in] socketId Id of the socket to read from
    *
    * @param[in] pBuff Point to the buffer to write the read values form socket
    *
    * @param[in] maxBytes maximum number of bytes to read.
    *
    * @param[out] pBytesRead Actual number of bytes read from socket
    *
    * @returns true when reading from the socket is successful
    */
    bool readSocket(const SOCKET socketId, uint8_t* pBuff, const uint32_t maxBytes, uint32_t* pBytesRead) const;


    /**
    * Read from a connection less communication socket
    *
    * @param[in] socketId Id of the socket to read from
    *
    * @param[in] pBuff Point to the buffer to write the read values form socket
    *
    * @param[in] maxBytes maximum number of bytes to read.
    *
    * @param[out] pBytesRead Actual number of bytes read from socket
    *
    * @param[out] pUdpPeerAddr Address of peer in a UDP Host connection.
    *
    * @returns true when reading from the socket is successful
    */
    bool readFromSocket(const SOCKET socketId, uint8_t* pBuff, uint32_t maxBytes, uint32_t* pBytesRead, sockaddr* pUdpPeerAddr) const;


    /**
    * Write to a connection based communication socket
    *
    * @param[in] socketId Id of the socket to write to
    *
    * @param[in] pBuff Point to the buffer to read the values to be written to the socket
    *
    * @param[in] maxBytes maximum number of bytes to write
    *
    * @param[out] pBytesWritten Actual number of bytes written to socket
    *
    * @returns true when writing to the socket is successful
    */
    bool writeSocket(const SOCKET socketId, const uint8_t* const pBuff, const uint32_t maxBytes, uint32_t* const pBytesWritten) const;


    /**
    * Write to a connection less communication socket
    *
    * @param[in] socketId Id of the socket to write to
    *
    * @param[in] pBuff Point to the buffer to read the values to be written to the socket
    *
    * @param[in] maxBytes maximum number of bytes to write.
    *
    * @param[out] pBytesWritten Actual number of bytes written to socket
    *
    * @param[out] pUdpPeerAddr Address of peer in a UDP Host connection.
    *
    * @returns true when reading from the socket is successful
    */
    bool writeToSocket(const SOCKET socketId, const uint8_t* const pBuff, const uint32_t maxBytes, uint32_t* const pBytesWritten,
      const sockaddr* const pUdpPeerAddr) const;

    /**
    * Translate connection type to printable string
    *
    * @param[in] connType Type of connection (TCP or UDP?, Host or Client?)
    *
    * @returns the connection type as a character string
    */
    const char_t *connectionTypeStr(const IPConnectionType connType) const;

    /**
    * Translate connection status to printable string
    *
    * @param[in] status Status of this connection (None, Init,...,Connected. etc..)
    *
    * @returns the connection status as a character string
    */
    const char_t *connectionStatusStr(const IPConnectionStatus status) const;

    /**
    * Collect connection statistics for a connection
    *
    * @param[in]  connectionId An id identifying the connection
    * @param[out] displayBuf   A buffer for the printable statistics
    * @param[in]  bufSize      The size of the displayBuf
    * @param[in]  thisTime     This time (ms)
    */
    void collectConnectionStatistics(const uint8_t connectionId, char_t *const displayBuf, const size_t bufSize, const int64_t thisTime);
  };



}

#endif
