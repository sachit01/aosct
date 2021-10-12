#pragma once 
/* tcpip.h
 *
 * Handles IP port functions in the Windows environment.
 *
 */


/****************************************************************************
*
*    REVISION HISTORY :
*
*      Rev   Date        Name      Measures
*      --------------------------------------------------------------------
*           2011-02-18   BoH	   Created.
****************************************************************************/

typedef short          int16;
typedef int            int32;

#define MMI_SOCKETVERSION 0x0202	// Version 2.2 is supported on Windows Server 2008, Windows Vista, Windows Server 2003, 
						// Windows XP, Windows 2000, Windows NT 4.0 with Service Pack 4 (SP4) and later, 
						// Windows Me, Windows 98, and Windows 95 OSR2. 

#define TB_OK 0
#define TB_ERROR (-1)

/**********************************************************
* Function:     initSocketEnvironment
* Description:
**********************************************************/
int16 initSocketEnvironment(void);
/**********************************************************
* Function:     exitSocketEnvironment
* Description:
**********************************************************/
int16 exitSocketEnvironment(void);

/**********************************************************
* Function:     openSocket
* Description:
* socketType : SOCK_STREAM for TCP connection-based operation
* socketType : SOCK_DGRAM for UDP connection-less operation
**********************************************************/

SOCKET openSocket(int socketType);
/**********************************************************
* Function:     closeSocket
* Description:  Assumes a connection-based communication
**********************************************************/
int16 closeSocket(SOCKET socketId);

/**********************************************************
* Function:     readSocket
* Description:  Assumes a connection-based communication
**********************************************************/
int16 readSocket(SOCKET socketId,char* pBuffer, int32 maxBytes, int32* pnoBytesRead);

/**********************************************************
* Function:     writeSocket
* Description:  Assumes a connection-based communication
**********************************************************/
int16 writeSocket(SOCKET socketId, char* pBuffer, int32 nBytes, int32* pnoBytesWritten);
/**********************************************************
* Function:     bindSocket
* Description:  Assumes a connection-based communication
**********************************************************/
int16 bindSocket(SOCKET socketId,int portNumber);
/**********************************************************
* Function:     listenSocket
* Description:  Listen for a connection from client
**********************************************************/
int16 listenSocket(SOCKET socketId);
/**********************************************************
* Function:     AsyncSelectSocket
* Description:  Set the socket to non-blocking asynchronous mode
*               hWnd : handle of the window to receive the events
*				wMsg : custom notification message id
*			    lEvent : flags to indicate what to be handled by this window message handler 
*				e.g. (FD_ACCEPT | FD_CONNECT | FD_READ | FD_CLOSE)
**********************************************************/
int16 AsyncSelectSocket(SOCKET socketId, HWND hWnd, u_int wMsg, long lEvent);
/**********************************************************
* Function:     acceptSocket
* Description:  Accept a connection from client
*               Returns the accepted socket or INVALID_SOCKET 
*               if no connection accepted
**********************************************************/
SOCKET acceptSocket(SOCKET socketId, struct sockaddr *addr);

/**********************************************************
* Function:     isConnectedSocket
* Description:  Checks that Connected socket still is Ok
*				Note! Will hang if socket is blocking
**********************************************************/
int16 isConnectedSocket(SOCKET socketId);
/**********************************************************
* Function:     SetNonBlockingSocket
* Description:  Set non-blocking socket
**********************************************************/
int16 SetNonBlockingSocket(SOCKET socketId);
/**********************************************************
* Function:     connectSocket
* Description:  Connect to a server
**********************************************************/
int16 connectSocket(SOCKET socketId,struct sockaddr *server_addr);

int16 SetSocketKeepAlive(SOCKET socketId);

int16 IPAddressToString(struct sockaddr *addr, char *IPAddr, LPDWORD IPAddrBufLen );

