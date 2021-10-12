/****************************************************************************
*           (C) COPYRIGHT Bombardier , SWEDEN 2011
*           ======================================
*
*    The copyright to the computer program herein is the
*    property of Bombardier, Sweden. All rights reserved.
*    The program may be used and/or copied only with the
*    written permission from Bombardier, or in accordance
*    with the terms and conditions stipulated in the
*    agreement/contract under which the program has been
*    supplied.
*
*
*    MODULE NAME:  mmi_ip.h
*
*    REVISION:     1.0
*
*    PREPARED:     Bo Hermansson
*
*    DESCRIPTION:  Header file for IP socket communication used by the MMI in a Windows environment.
*
*
****************************************************************************/

/****************************************************************************
*
*    REVISION HISTORY :
*
*    Rev   Date        Name      Measures
*    -----------------------------------------------------------------------
*    1.0  2011-02-15   Bo H	     Start
*    1.1  2017-08-22   Bo H      Tailored for DMI / BHP
****************************************************************************/
#ifndef MMI_IP_H
#define MMI_IP_H

#include <winsock.h>
#include "mmi_types.h"

#ifndef TB_OK
#define TB_OK 0
#endif

#ifndef TB_ERROR
#define TB_ERROR (-1)
#endif

#define MMI_SOCKETVERSION 0x0202	// Version 2.2 is supported on Windows Server 2008, Windows Vista, Windows Server 2003, 
						// Windows XP, Windows 2000, Windows NT 4.0 with Service Pack 4 (SP4) and later, 
						// Windows Me, Windows 98, and Windows 95 OSR2. 

int16 initSocketEnvironment(void);
int16 exitSocketEnvironment(void);

int16 openSocket(SOCKET* socketId, int socketType); 
int16 bindSocket(SOCKET socketId,int portNumber); 
int16 listenSocket(SOCKET socketId);
int16 acceptSocket(SOCKET socketId, SOCKET *pAcceptedSocket); 
int16 closeSocket(SOCKET socketId);
int16 readSocket(SOCKET socketId, char* pBuffer, int32 maxBytes, int32* pnoBytesRead);
int16 writeSocket(SOCKET socketId, char* pBuffer, int32 nBytes, int32* pnoBytesWritten);
int16 shutdownSocket(SOCKET socketId);
int16 isConnectedSocket(SOCKET socketId);
int16 SetNonBlockingSocket(SOCKET socketId);
#endif
