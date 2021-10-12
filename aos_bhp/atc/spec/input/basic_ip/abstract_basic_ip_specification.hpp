namespace ATC
{

  /**
  \if AsMainPage
  \mainpage Basic IP Component Specification
  @anchor bip
  \endif

  \ifnot AsMainPage
  \class AbstractBasicIP
  \endif

  \section Purpose Purpose
  This document specifies the software design for the Basic IP core component.

  \section Overview Overview

  \subsection GeneralFunctionality General Functionality

  The AbstractBasicIP class represents the core part of the Basic IP component. It encapsulates the handling of connections using the TCP/UDP protocol.
  It supports four types of connections such as TCP host, TCP client, UDP host and UDP client and any application can create a connection of any of the connection types.
  Basic IP gives the application a handler to simplify and standardize the usage of IP-connections in AOS and all connections supported are only Peer to Peer.
  It is used by ATP components such as Console and Log handler.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  This component has dependencies to the following components:
  - EventHandler: For reporting an event if any error occurs for the IP connections.

  Other components have dependencies to this component because they use its public types
  and methods, see \ref ATC::AbstractBasicIP Class Reference.

  A component using the AbstractBasicIP needs to
  - call AbstractBasicIP::initConnection() to initialize the connection at startup. BasicIP will after that take care of the connection handling.
  - call AbstractBasicIP::writeBuf()/ AbstractBasicIP::readBuf() to send/receive data on the channel
  - call AbstractBasicIP::getConnectionStatus() to get info about the connection status.

  \section FunctionalDesign Functional Design
  \subsection Initialization Initialization
  The AbstractBasicIP::init() function called by the ATP application initializes the IP communication environment if required based on the platform.
  The components using the BasicIP must call the BasicIP::initConnection once for each connection.
  This will describe the connection in an internal Connection Control Block used when handling the connection.

  \subsection ModeDependentOperation Mode dependent operation
  Basic IP component is executed in all ATP or ATO modes.

  \subsection Scheduling Scheduling
  The AbstractBasicIP has one function (inherited from the base-class ProcComponent ) that must be called each execution-cycle:

  \subsection initConnection InitConnection()
  The component using AbstractBasicIP must call AbstractBasicIP::initConnection() with arguments
  - Connection ID
  - Connection Type
  - IP Address
  - Port Number
  - Send Buffer Size
  - Receive Buffer Size

  CONNECTION TYPE  |Connection ID|IP Addr|Port Num |Send BufSize|Recv BufSize
  -----------------|------------ |-------|-------- |----------- |-----------|
  TCP Host         |  x          |       |         |    x       |   x       |
  TCP Client       |  x          |     x |      x  |    x       |   x       |
  UDP Host         |  x          |       |         |    x       |   x       |
  UDP Client       |  x          |     x |      x  |    x       |   x       |

  x - Indicates the following parameters are mandatory for the Connection Type.

  The arguments of the initConnection() is validated and when valid:
  - The Connection Control Block is filled with the information
  - The connectionStatus of the connection is changed to Init

  \subsubsection run run()
  AbstractBasicIP::run() calls AbstractBasicIP::handleConnections() in order to iterate through and handle all the connections.
  
  \paragraph HandleConnections Handling Connections

  Basic IP goes through all the connections created by other components. Then, for each of the connections depending upon connection type
  corresponding handler functions are invoked by corresponding connection ID.

  @image html basic_ip_handle_connections.png "Handle Connections"
  @image latex basic_ip_handle_connections.png "Handle Connections"

  \paragraph HandleConnection Connection Handling
  Depending on the connectionType a connection requires different handling. The connectionType specifies the protocol (TCP or UDP)
  and also indicates a client or a host (i.e. server).

  \paragraph TCPHostConnection  Handle TCPHost Connection
  A TCP host waits for the connection and accepts when a TCP client tries to connect to its listening socket.

  @image html basic_ip_handle_tcp_host_connection.png "Handle TCP Host Connection"
  @image latex basic_ip_handle_tcp_host_connection.png "Handle TCP Host Connection"

  \paragraph TCPClientConnection Handle TCPClient Connection
  A TCP client tries to connect to the listening socket of a TCP host. The IP-address and port number of the TCP host to connect to is specified in the call to initConnection for this connection.
  AbstractBasicIP will also
    + checks for disconnected if connected and attempts to reconnect if disconnected by TCP client. 
    + reconnect after a timeout if the connect failed.
    + periodically checks if a connected socket is still alive and will close a connection that appears to be disconnected.

  @image html basic_ip_handle_tcp_client_connection.png "Handle TCP Client Connection"
  @image latex basic_ip_handle_tcp_client_connection.png "Handle TCP Client Connection"

  \latexonly \newpage \endlatexonly
  \paragraph HandleUDPHostConnection Handle UDPHost connection
  The UDP host waits for any datagram to arrive and stores the IP-address and port of its origin as its peer.
  UDP is connection-less and is not aware of any connections. Connection status is changed to connected when the socket is created and bound.

  @image html basic_ip_handle_udp_host_connection.png "Handle UDP Host Connection"
  @image latex basic_ip_handle_udp_host_connection.png "Handle UDP Host Connection"

  \latexonly \newpage \endlatexonly
  \paragraph HandleUDPClientConnection Handles UDPClient connection
   A UDP client sends datagrams to a UDP host. The IP address and port number of the UDP host to send to shall be specified in the
   call to initConnection for this AbstractBasicIP client connection.

   @image html basic_ip_handle_udp_client_connection.png "Handle UDP Client Connection"
   @image latex basic_ip_handle_udp_client_connection.png "Handle UDP Client Connection"

  \subsection readBuf ReadBuf()
  The component using AbstractBasicIP calls AbstractBasicIP::readBuf() to receive any data available from a connection.<br>

  @image html basic_ip_readbuf.png "BasicIP Readbuf"
  @image latex basic_ip_readbuf.png "BasicIP Readbuf"

  \subsection writeBuf WriteBuf()
  The component using AbstractBasicIP calls AbstractBasicIP::writeBuf() to send data on a connection.

  @image html basic_ip_writebuf.png "BasicIP Writebuf"
  @image latex basic_ip_writebuf.png "BasicIP Writebuf"

  \subsection getConnectionStatus getConnectionStatus()
  AbstractBasicIP::getConnectionStatus() is used by the component using AbstractBasicIP in order to check the status of a connection.

  \latexonly \newpage \endlatexonly
  \section ClassDiagram Class Diagram
  @image html abstract_basic_ip_class_diagram.png "Abstract BasicIP Class Diagram"
  @image latex abstract_basic_ip_class_diagram.png "Abstract BasicIP Class Diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console-commands
  The following component-specific console-commands shall be implemented.
  + bip : Displays statistics for all the connections. The statistics is stored in the connectionStatistics of each Connection Control Block.

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation
  The adaptation of the Basic IP component
  + creates as many connection control blocks as there are connections to support.
  + defines the valid connectionId(s) for the adaptation. See enum ATC::AbstractBasicIP::IPConnectionType

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATC.

  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATC.

  */
}
