/* mmi_ip.c
 *
 * Handles IP port read/write functions for the MMI in the Windows environment.
 *
 */
#include <windows.h>
#include <stdio.h>
#include "mmi_ip.h"


/****************************************************************************
*
*    REVISION HISTORY :
*
*      Rev   Date        Name      Measures
*      --------------------------------------------------------------------
*           2011-02-15   BoH	   Created.
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
int16 openSocket(SOCKET* socketId, int socketType)
{
		// Init global variables
 *socketId = socket(AF_INET, socketType, IPPROTO_TCP); 
 if (*socketId < 0)
     return TB_ERROR;
 else 
     return TB_OK;
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

    if (retval > 0)
    {
        *pnoBytesRead = retval;
        return TB_OK;
    } 
    else if (retval == 0)
    {       /* Socket closed */
        return TB_ERROR;
    }
    else
    {
				// Failed to peek, but it is OK if the input queue is empty
				// If no char is available recv will return WSAEWOULDBLOCK if non-blocking socket
				// Otherwise we have some problem with the socket
		 if (WSAGetLastError() != WSAEWOULDBLOCK)           
			 return TB_ERROR;      
    
    
        if (retval == WSAEWOULDBLOCK)
        {
		    *pnoBytesRead = 0;
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
			// Max two connections pending
    if (listen(socketId, 2) == -1)	
        return TB_ERROR;
    else
        return TB_OK;
}
/**********************************************************
* Function:     acceptSocket
* Description:  Accept a connection from client
**********************************************************/
int16 acceptSocket(SOCKET socketId, SOCKET *pAcceptedSocket)
{
    *pAcceptedSocket=accept(socketId,NULL,NULL);
    if (*pAcceptedSocket >= 0)
        return TB_OK;
    else
        return TB_ERROR;
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
                // Socket was closed
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
    if (socketId >= 0) 
	{
	    iMode = 1; // Non-zero will set the socket to non-blocking
				//
				// Note ! For connection-based operation:
				// WSAAsyncSelect and WSAEventSelect functions automatically set a socket to nonblocking mode

        ioctlsocket(socketId,FIONBIO,&iMode); 
	    return TB_OK;
	} 
    else
        return TB_ERROR;
}
