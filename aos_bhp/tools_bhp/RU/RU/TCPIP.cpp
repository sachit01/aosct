/* tcpip.c
 *
 * Handles IP port functions in the Windows environment.
 *
 */
#include "stdafx.h"
#include "tcpip.h"


/****************************************************************************
*
*    REVISION HISTORY :
*
*      Rev   Date        Name      Measures
*      --------------------------------------------------------------------
*           2011-02-18   BoH	   Created.
****************************************************************************/


/**********************************************************
* Function:     initSocketEnvironment
* Description:
**********************************************************/
int16 initSocketEnvironment(void)
{
 WSADATA SocketImplementationData;
 if (WSAStartup(MMI_SOCKETVERSION,&SocketImplementationData) == 0)
	 return TB_OK;
 else 
     return TB_ERROR;
}
/**********************************************************
* Function:     exitSocketEnvironment
* Description:
**********************************************************/
int16 exitSocketEnvironment(void)
{
	// Clean up the socket environment
 if (WSACleanup() == 0)
	 return TB_OK;
 else 
     return TB_ERROR;
}


/**********************************************************
* Function:     openSocket
* Description:
* socketType : SOCK_STREAM for TCP connection-based operation
* socketType : SOCK_DGRAM for UDP connection-less operation
**********************************************************/
SOCKET openSocket(int socketType)
{

    return socket(AF_INET, socketType, IPPROTO_TCP); 
}
 
/**********************************************************
* Function:     closeSocket
* Description:  Assumes a connection-based communication
**********************************************************/
int16 closeSocket(SOCKET socketId) 
{
    closesocket(socketId);

    return TB_OK;
}

/**********************************************************
* Function:     readSocket
* Description:  Assumes a connection-based communication
**********************************************************/
int16 readSocket(SOCKET socketId,char* pBuffer, int32 maxBytes, int32* pnoBytesRead)
{
 int retval;

 if (socketId < 0)
   return TB_ERROR;

 retval = recv(socketId,pBuffer, maxBytes, 0);

 if (retval >= 0)
 {
   *pnoBytesRead = retval;
   return TB_OK;
 } 
 else 
 {
     *pnoBytesRead = 0;   
     if (retval == WSAEWOULDBLOCK)
     {
		 
		 return TB_OK;
     }
	 else
         return TB_ERROR;

 } 
}

/**********************************************************
* Function:     writeSocket
* Description:  Assumes a connection-based communication
**********************************************************/
int16 writeSocket(SOCKET socketId, char* pBuffer, int32 nBytes, int32* pnoBytesWritten)
{
 int32 noBytesWritten;

 if (socketId < 0)
   return TB_ERROR;
   
 noBytesWritten = send(socketId, pBuffer, nBytes, 0);

 if (noBytesWritten == -1)
	 return TB_ERROR;   


 if (pnoBytesWritten) 
					// Pointer NOT NULL
	*pnoBytesWritten = noBytesWritten;

 else if (noBytesWritten != nBytes)
    return TB_ERROR;	// No of bytes written different from bytes to write 		

 return TB_OK;
}


/**********************************************************
* Function:     bindSocket
* Description:  Assumes a connection-based communication
**********************************************************/
int16 bindSocket(SOCKET socketId,int portNumber)
{
 SOCKADDR_IN addr; // The address structure for a TCP socket

 addr.sin_family = AF_INET;      // Address family
 addr.sin_port = htons (portNumber);   // Assign port to this socket

    //Accept a connection from any IP using INADDR_ANY
    //You could pass inet_addr("0.0.0.0") instead to accomplish the 
    //same thing. If you want only to watch for a connection from a 
    //specific IP, specify that //instead.
 addr.sin_addr.s_addr = htonl (INADDR_ANY);  

 if (bind(socketId, (LPSOCKADDR)&addr, sizeof(addr)) == SOCKET_ERROR)
  {
       //We couldn't bind (this will happen if you try to bind to the same  
       //socket more than once)
   return TB_ERROR;
  }
 
 return TB_OK;
}

/**********************************************************
* Function:     listenSocket
* Description:  Listen for a connection from client
**********************************************************/
int16 listenSocket(SOCKET socketId)
{


			// Max ten connections pending
 if (listen(socketId, 10) == -1)	
   return TB_ERROR;
 else
   return TB_OK;
}
/**********************************************************
* Function:     AsyncSelectSocket
* Description:  Set the socket to non-blocking asynchronous mode
*               hWnd : handle of the window to receive the events
*				wMsg : custom notification message id
*			    lEvent : flags to indicate what to be handled by this window message handler 
*				e.g. (FD_ACCEPT | FD_CONNECT | FD_READ | FD_CLOSE)
**********************************************************/
int16 AsyncSelectSocket(SOCKET socketId, HWND hWnd, u_int wMsg, long lEvent)
{
 if (WSAAsyncSelect(socketId, hWnd, wMsg, lEvent) == 0)
   return TB_OK;
 else
   return TB_ERROR;	  
}
/**********************************************************
* Function:     acceptSocket
* Description:  Accept a connection from client
*               Returns the accepted socket or INVALID_SOCKET 
*               if no connection accepted
**********************************************************/
SOCKET acceptSocket(SOCKET socketId, struct sockaddr *addr)
{
 int AddrLen=sizeof(*addr);
 SOCKET AcceptedSocket=accept(socketId,addr,&AddrLen);
 if (AcceptedSocket >= 0)
  return AcceptedSocket;
 else
  return INVALID_SOCKET;
}

/**********************************************************
* Function:     isConnectedSocket
* Description:  Checks that Connected socket still is Ok
*				Note! Will hang if socket is blocking
**********************************************************/
int16 isConnectedSocket(SOCKET socketId)
{
 char buf;
 int err;
 if (socketId >= 0) 
 {
				// Try to peek one char
	 err = recv(socketId, &buf, 1, MSG_PEEK);      
	 if (err == SOCKET_ERROR) 
	 {           
				// Failed to peek, but it is OK if the input queue is empty
				// If no char is available recv will return WSAEWOULDBLOCK if non-blocking socket
				// Otherwise we have some problem with the socket
		 if (WSAGetLastError() == WSAEWOULDBLOCK)           
			 return TB_OK;      
		 else
			 return TB_ERROR;      
	 }      
	 else if (err == 0)
	        // Socket closed
        return TB_ERROR;
	 else
        return TB_OK;
 } 
 else
	 return TB_ERROR;
}
/**********************************************************
* Function:     SetNonBlockingSocket
* Description:  Set non-blocking socket
**********************************************************/
int16 SetNonBlockingSocket(SOCKET socketId)
{
 u_long iMode;
 if (socketId >= 0) {
	 iMode = 1; // Non-zero will set the socket to non-blocking
				//
				// Note ! For connection-based operation:
				// WSAAsyncSelect and WSAEventSelect functions automatically set a socket to nonblocking mode

     ioctlsocket(socketId,FIONBIO,&iMode); 
	 return TB_OK;

 } else
     return TB_ERROR;
}
/**********************************************************
* Function:     SetSocketKeepAlive
* Description:  Set socket option SO_KEEPALIVE
*               This means that TCP/IP will send keep-alive 
*               messages periodically in order to detect 
*               non-responding remote peers
**********************************************************/
int16 SetSocketKeepAlive(SOCKET socketId)
{
 BOOL OptVal=TRUE;
 if (socketId >= 0) 
 {
     if (setsockopt(socketId,SOL_SOCKET,SO_KEEPALIVE,(const char*)&OptVal,sizeof(OptVal)) == 0)
	    return TB_OK;
     else
	    return TB_ERROR;

 } 
 else
     return TB_ERROR;
}
/**********************************************************
* Function:     connectSocket
* Description:  Connect to a server
**********************************************************/
int16 connectSocket(SOCKET socketId,struct sockaddr *server_addr)
{
 if (connect(socketId,server_addr, sizeof(*server_addr)) == SOCKET_ERROR) 
	return TB_ERROR;
 else
	return TB_OK;
}
/**********************************************************
* Function:     IPAddressToString
* Description:  Convert to human readable IP Address
**********************************************************/
int16 IPAddressToString(struct sockaddr *addr, char *IPAddr, LPDWORD IPAddrBufLen )
{
 if (WSAAddressToStringA(addr, sizeof(*addr), NULL, IPAddr, IPAddrBufLen ) == 0)
 {
    return TB_OK;
 }
 else
 {
    return TB_ERROR;
 }
}

