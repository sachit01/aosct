/****************************************************************************
*           (C) COPYRIGHT Bombardier Signal AB, SWEDEN 2011.
*           ================================================
*
*    The copyright to the computer program herein is the
*    property of Adtranz AB, Sweden. All rights reserved.
*    The program may be used and/or copied only with the
*    written permission from Adtranz AB, or in accordance
*    with the terms and conditions stipulated in the
*    agreement/contract under which the program has been
*    supplied.
*
*
*    MODULE NAME:  mmiclmain.c
*
*    REVISION:     PA1
*
*    PREPARED:     Bo Hermansson
*
*    DESCRIPTION:  Main module in mmicl.
*
*
*    Macros used to control the package:
*      DEBUG_CONSOLE
*        A console is allocated and debug messages is printed to it.
*        Works for MMI_VERSION at least.
*
*
*
****************************************************************************/

/****************************************************************************
*
*    REVISION HISTORY :
*
*      Rev   Date        Name      Measures
*      --------------------------------------------------------------------
*      PA1  1997-12-03   KONSBG    Created.
*           2011-02-18	 Bo H	   Adjusted for TCP/IP
*           2011-08-19   Bo H      For MMI, IP and Windows only 
*			2011-09-28	 Bo H	   New protocol for LK
*           2012-03-20   Bo H      DebugLevel 
*           2012-03-28   Bo H      Retransmit if ack not received within timeout 
****************************************************************************/

/****************************************************************************
* INCLUDE FILES                                                             *
****************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <windows.h>


#include "mmi_ip.h"
#include "crc32.h"
#include "mmiclmain.h"
#include "MMI_Queue.h"

#ifdef DEBUG_CONSOLE
#include "mmi_debug.h"
#endif


/****************************************************************************
* LOCAL MACROS                                                              *
****************************************************************************/
#define QUEUE_LEN 25
#define OUTQUEUE_LEN 25

/****************************************************************************
* GLOBAL DECLARATIONS                                                       *
****************************************************************************/

#ifdef DEBUG_CONSOLE
  static int MaxoutqCount=0;
  static int MaxAckMsgqCount=0;
  static int MaxUnackMsgqCount=0;
  static int MaxAckqCount=0;
#endif

  static int DebugLevel;            /* DebugLevel = 0, No debug window 
                                       DebugLevel = 1, Debug window with basic messages
                                       DebugLevel = 2, Debug window with details */
  
  static SOCKET ListenSocket;       /* Socket that MMICL - DLL is using to accept connections from the ATP */
  static SOCKET ConnectedSocket;    /* Socket that is connected to the ATP */
  
  static int LastAckMsgMessageNo;    /* Last received vital message; used to skip duplicated messages */
  static byte AckRequestMessageNo;
  static byte PendingAckMessageNo;
  
  static int MessageNo;

  static int ErrCount;              /* Counts CRC - errors on received messages */

  static byte inbuf[2*MMI_MAX_PACKAGE_SIZE];        
  static word inbufLength;
  
  static int AckMsgSendCount;
  static bool AckMsgOkToSend;
  static dword AckMsgSendTime;
  static int AckMsgErrCount;

  static queueT outq;
  static queueT AckMsgq;
  static queueT UnackMsgq;
  static queueT Ackq;

  static byte buffer[2*MMI_MAX_PACKAGE_SIZE];
  static int noOfBytesInBuffer = 0;

  
/****************************************************************************
* LOCAL DECLARATIONS                                                        *
****************************************************************************/
/* In these structures, part of the message info (nr, len, crc) is stored both as
 * separated struct-members and as part of the msg-vector. We cannot just map a
 * vector with the leading items as a message as we don't have full control of how
 * the structure is laid out (including pad bytes).
 */
typedef struct 
{
	uint8 messageNumber;
	uint16 Length;
	uint8 msg[MMI_MAX_PACKAGE_SIZE]; 
} AckMsgQueueItem;

typedef struct 
{
	uint16 Length;
	uint8 msg[MMI_MAX_PACKAGE_SIZE];
} UnackMsgQueueItem;

typedef struct 
{
	uint8 messageNumber; 
	uint8 msg[MMI_HEADER_SIZE + MMI_TRAILER_SIZE];		/* No data in acknowledge */
} AckQueueItem;

typedef struct 
{
  uint8 msg[MMI_MAX_PACKAGE_SIZE - MMI_TRAILER_SIZE];	/* No need to copy trailer to application */
} OutputQueueItem;

/****************************************************************************
* FUNCTIONS                                                                 *
****************************************************************************/

static void ReportFailure(char *Msg, char *FileName, int Line)
{
    
#ifdef DEBUG_CONSOLE
	prf(prfError,"%s File:%s, Line:%d\n",Msg, FileName, Line);
#endif

}
/**********************************************************
* Function:    getSystemTime
* Description: Returns the time in ms. Time base is undefined, to be used
*              for delta times only.
**********************************************************/
static void getSystemTime(dword *w_time)
{
    *w_time = GetTickCount();
}

/**********************************************************
* Function:    initIP
* Description:
**********************************************************/
static int initIP(int port)
{
    int retval;

    retval = initSocketEnvironment();
    if (retval != TB_OK) 
	{
        ReportFailure("initSocketEnvironment failed!", __FILE__, __LINE__);
        return retval;
	}
   
    retval = openSocket(&ListenSocket,SOCK_STREAM );
    if (retval != TB_OK) 
	{
        ReportFailure("openSocket failed!", __FILE__, __LINE__);
        return retval;
    }

    retval = bindSocket(ListenSocket,port);
    if (retval != TB_OK) 
	{
        ReportFailure("bindSocket failed!", __FILE__, __LINE__);
        return retval;
    }

    retval = listenSocket(ListenSocket);
    if (retval != TB_OK) 
	{
        ReportFailure("listenSocket failed!", __FILE__, __LINE__);
        return retval;
	}

				// Set non-blocking in order to avoid acceptSocket to wait forever
    SetNonBlockingSocket(ListenSocket);

    return 0;
}

/**********************************************************
* Function:    exitIP
* Description:
**********************************************************/
static void exitIP(void)
{
    int retval = closeSocket(ListenSocket);
    if (retval != TB_OK) 
    {
        ReportFailure("closeSocket failed!", __FILE__, __LINE__);
    }
    exitSocketEnvironment();
}

/**********************************************************
* Function:    MMICL_getErrors
* Description: 
**********************************************************/
void MMICL_getErrors(int *CurrentAckMsgErrCount, int *recErrCount)
{
    *CurrentAckMsgErrCount = AckMsgErrCount;
    AckMsgErrCount = 0;

    *recErrCount = ErrCount;
    ErrCount = 0;
}

/**********************************************************
* Function:    getDataFromQueues 
* Description: Moves messages from queues to a bundle
*              In *vC, the number of vital messages is returned.
**********************************************************/
static void getDataFromQueues(uint16 *length, void *bundleP, uint16 *vCount)
{
    uint16 bundleLength = 0;
    uint16 messageLength;
    AckMsgQueueItem *AckMsg;
    UnackMsgQueueItem *UnackMsg;
    AckQueueItem *Ack;
    dword currentTime;
	char MsgBuf[80];
    uint8 *bundle = bundleP;

    getSystemTime(&currentTime);
    *vCount = 0;

        /* ACK MESSAGES, only one at a time */
    Ack = queueGetW(&Ackq);
    if (Ack != NULL) 
    {
        memcpy(bundle, Ack->msg, MMI_HEADER_SIZE + MMI_TRAILER_SIZE);
        bundleLength += MMI_HEADER_SIZE + MMI_TRAILER_SIZE;
        (void)queueDrop(&Ackq);
        (*vCount)++;
    }
 

        /* Acknowledged messages, only one at a time */
    if (AckMsgOkToSend) 
    {
        AckMsg = queueGetW(&AckMsgq);
        if (AckMsg != NULL) 
        {

			messageLength = AckMsg->Length;
            if (messageLength > MMI_MAX_PACKAGE_SIZE)
			{
				sprintf_s(MsgBuf,sizeof(MsgBuf),"Acknowledged message too long! Length:%d\n", AckMsg->Length);
				ReportFailure(MsgBuf, __FILE__, __LINE__);
			}

            if (messageLength + bundleLength <= MMI_MAX_PACKAGE_SIZE) 
            {
                memcpy(&bundle[bundleLength], AckMsg->msg, messageLength);
                bundleLength   += messageLength;
                AckMsgOkToSend = false;
                AckMsgSendTime = currentTime;
                AckMsgSendCount++;
                (*vCount)++;
            } 
            else 
            {
                *length = bundleLength;
                return;
            }
        }
    }
    else
    {       /* Ack not yet received , timeout expired ? */
        dword currentTime;            
        dword timeExpired;
		getSystemTime(&currentTime);
        timeExpired = currentTime-AckMsgSendTime;

        if (timeExpired > RESEND_TIME)
        {
		    prf(prfInfo,"Ack not yet received after %ld ms\n",timeExpired);
            if (AckMsgSendCount < MAX_NO_OF_TRANSMISSIONS)
            {
                AckMsg = queueGetW(&AckMsgq);
                if (AckMsg != NULL) 
                {
			        messageLength = AckMsg->Length;
                    if (messageLength + bundleLength <= MMI_MAX_PACKAGE_SIZE) 
                    {
                        memcpy(&bundle[bundleLength], AckMsg->msg, messageLength);
                        bundleLength   += messageLength;
                        AckMsgSendTime = currentTime;

                        AckMsgSendCount++;
		                prf(prfInfo,"Resending(%d) acknowledged message\n",AckMsgSendCount);
                    }
                    else 
                    {
                        *length = bundleLength;
                        return;
                    }
                }
            }
            else
            {   /* too many retransmissions */
                queueDrop(&AckMsgq);
                AckMsgSendCount = 0;
                AckMsgOkToSend = true;
                prf(prfInfo,"Too many retransmissions. Discard message\n");

            }
        }
             
    }
   
        /*** Unacknowledged MESSAGES ***/
    for (;;) 
    {
        UnackMsg = queueGetW(&UnackMsgq);
        if (UnackMsg != NULL) 
        {
			messageLength = UnackMsg->Length;
            if (messageLength + bundleLength <= MMI_MAX_PACKAGE_SIZE) 
            {
                memcpy(&bundle[bundleLength], UnackMsg->msg, (uint)messageLength);
                bundleLength += messageLength;
                (void)queueDrop(&UnackMsgq);
            } 
            else
                break;
        } 
        else
            break;
    }
	*length = bundleLength;
}



/**********************************************************
* Function:     HandleBundle()
* Description:  Check header and trailer of bundle and act 
*               depending on type of message in bundle
**********************************************************/
static void HandleBundle (uint16 length, uint8 *bundle)
{
    uint16 DataLength;
    uint8 MessageNumber;
	uint8 HeaderType;
    uint32 bundleCrc, calculatedCrc;
    int i = 0;
    OutputQueueItem *q;
    int skipMsg;


                // Scan through bundle
    while (i < length) 
    {           
        skipMsg = 0;

				// first byte is MS
		HeaderType=bundle[i+1]; 
		MessageNumber=bundle[i+2]; 
		DataLength=VWORD(bundle, i+3);


        bundleCrc = VDWORD(bundle, i + MMI_HEADER_SIZE + DataLength);
            /* Get CRC from bundle (swap from Motorola to INTEL byte order) */
            /* Calculate the checksum of the message */
        calculatedCrc = crc32_calc(&bundle[i], MMI_HEADER_SIZE + DataLength);
        if (bundleCrc == calculatedCrc) 
        {
            if (HeaderType == 0x10) 
            {         /* Acknowledged message */
                if (MessageNumber == LastAckMsgMessageNo)
                {
                    skipMsg = 1;
                }
            
                    /* Ack even if the message is skipped */
                LastAckMsgMessageNo = MessageNumber;
                AckRequestMessageNo = MessageNumber;        
            }
        
            if (HeaderType != 0x80) 
            {       /* Acknowledged or unacknowledged message, put to output queue */
                if (!skipMsg) 
                {
                    q = queuePutW(&outq);
                    
                    if (q == NULL) 
                    {
						ReportFailure("queue full!",__FILE__, __LINE__);	
                    } 
                    else 
                    {
                        memcpy(&q->msg, &bundle[i], (uint)(MMI_HEADER_SIZE + DataLength));
                        queueCommit(&outq);
#ifdef DEBUG_CONSOLE
						{
							int ThisCount = queueCount(&outq);
							if (ThisCount > MaxoutqCount)
							{
								prf(prfRcv,"OutqCount:%d\n",ThisCount);
								MaxoutqCount=ThisCount;	
							}
						}
#endif
                    }
                }
            } 
            else
            {       /* Ack */
                PendingAckMessageNo = MessageNumber;
            }
        } 
        else 
        {           /* CRC not ok */
            ErrCount++;

        }
        i += (MMI_HEADER_SIZE + DataLength + MMI_TRAILER_SIZE);
        
    } /* while */
}

/**********************************************************
* Function:     getDataFromATP
* Description:  Read any data available from ATP and append
*               to the inbuf
*               Move message to outbuf
**********************************************************/

static void getDataFromATP(uint16 *outbufLength, uint8 *outbuf, uint16 outbufSize)
{
    uint16 DataLength,MessageLength;
    int32 BytesReadFromSocket;
	uint16 readBytes;


	if (inbufLength >= sizeof(inbuf))
	{
		/* We have filled up the inbuffer without being able to extract a complete message*/
		/* Clear ! */
		inbufLength = 0;
        ReportFailure("Inbuf full!", __FILE__, __LINE__);
	}

			// Read from connected socket
    if (readSocket(ConnectedSocket, &inbuf[inbufLength], sizeof(inbuf) - inbufLength, &BytesReadFromSocket) == TB_ERROR)
		{
         BytesReadFromSocket = 0;
		}

    readBytes = (uint16)BytesReadFromSocket;

#ifdef DEBUG_CONSOLE
	if (readBytes && (DebugLevel >= 2))	
		prtRBundle(readBytes, &inbuf[inbufLength]);
#endif

   
    inbufLength += readBytes;

    *outbufLength = 0;
    while (inbufLength) 
    {
        /* Make sure the message starts with a MESSAGE_START_BYTE.
         * Resynchronositation could be handled better. If the check sum is wrong, we should start
         * to look for a MESSAGE_START_BYTE at the first byte of the failed message (MT)
         * But as the checksum check is done by the caller, that will need more changes.
         * So for the moment, we just scan for the first MESSAGE_START_BYTE */
        if (inbuf[0] != MESSAGE_START_BYTE) 
        {       // First char is NOT the MESSAGE_START_BYTE
                // Scan for the first MESSAGE_START_BYTE 
            uint16 i = 0;
            while ((i < inbufLength) && (inbuf[i] != MESSAGE_START_BYTE)) 
            {
                i++;
#ifdef DEBUG_CONSOLE
                prf(0, ",");
#endif

            }
            if (i) 
            {   // Either end of inbuf or MESSAGE_START_BYTE found
                // Cut the leading part before the MESSAGE_START_BYTE
                inbufLength -= i;
                memmove(inbuf, inbuf+i, inbufLength);
            }
        }
        
        if (inbufLength < (MMI_HEADER_SIZE + MMI_TRAILER_SIZE)) 
        {       // The remaining length of inbuf is too small for any kind of message
                // Break out of loop and wait until(hopefully) the rest of the message arrives

#ifdef DEBUG_CONSOLE
            prf(0, "Chars in inbuf less than the shortest message!\n");
#endif

            break;
        }

			// Extract length
		DataLength = VWORD(inbuf, 3);
		MessageLength = MMI_HEADER_SIZE + DataLength + MMI_TRAILER_SIZE;
        
			// Not enough bytes in inbuffer for this message
        if (inbufLength < MessageLength) 
        {
#ifdef DEBUG_CONSOLE
            prf(0, "Chars in inbuf less than complete message!\n");
#endif
            break;
        }

			// Not enough bytes left in output buffer
		if ((*outbufLength + MessageLength) > outbufSize)
		{
#ifdef DEBUG_CONSOLE
            prf(0, "Not enough space in output buffer\n");
#endif
			break;	
		}

        
            /* There is enough for a complete msg */
        memcpy(outbuf+*outbufLength, inbuf, MessageLength);
        inbufLength -= MessageLength;
        memmove(inbuf, inbuf + MessageLength, inbufLength);
        *outbufLength += MessageLength;
    }
}

/**********************************************************
* Function:     mmicl_receive
* Description:  Calls the receive function in mmicl
* Return Value: None
**********************************************************/
static void MMICL_receive(void)
{
    word length = 1;
    byte rcvMsg[2*MMI_MAX_PACKAGE_SIZE]; 

    while (length > 0) 
    {
        getDataFromATP(&length, rcvMsg, sizeof(rcvMsg)); /* May get a bundle */
        if (length > 0)
            HandleBundle (length, rcvMsg);
    }
}

/**********************************************************
* Function:    CreateHeader
* Description: Creates the header part of the message
**********************************************************/
static void CreateHeader(uint8 *Msg, uint8 HeaderType, uint8 MessageNr, uint16 DataLength)
{
	Msg[0] = MESSAGE_START_BYTE;				
    Msg[1] = HeaderType;								
	Msg[2] = MessageNr;					
	VPUTWORD(Msg,3,DataLength);
}
/**********************************************************
* Function:    CreateTrailer
* Description: Creates the trailer part of the message
*			   Msg is a pointer to start of the header
*			   Length is the length of the header and the data
**********************************************************/
static void CreateTrailer(uint8 *Msg, uint16 Length)
{
	uint32 crc = crc32_calc(Msg, Length);	/* CRC32 */
    VPUTDWORD(Msg, Length, crc);
}

/**********************************************************
* Function:    AckMessage
* Description: Generates an Ack message and queues it
**********************************************************/
static void AckMessage(void)
{
    uint8 messageNumber = 1;

    while (messageNumber != 0) 
    {
        messageNumber = AckRequestMessageNo;
        AckRequestMessageNo = 0;

        if (messageNumber != 0) 
        {
            AckQueueItem *Ack = queuePutW(&Ackq);
            if (Ack == NULL) 
            {
                ReportFailure("ACK queue full!", __FILE__, __LINE__);
            }
            else 
            {
                Ack->messageNumber = (byte)messageNumber;
				CreateHeader(Ack->msg, 0x80, messageNumber, 0);		/* Data length is 0 */
				CreateTrailer(Ack->msg, MMI_HEADER_SIZE);
                queueCommit(&Ackq);
               
#ifdef DEBUG_CONSOLE
				prf(prfInfo, "<Ack Message nr:%d>\n", (int)Ack->messageNumber);
				{
					int ThisCount = queueCount(&Ackq);
					if (ThisCount > MaxAckqCount)
					{
						prf(prfRcv,"AckqCount:%d\n",ThisCount);
						MaxAckqCount=ThisCount;	
					}
				}
#endif
            }
        }
    }
}

/**********************************************************
* Function:    getPendingAck
* Description: Gets Ack messages and removes corresponding
*              vital message from queue
**********************************************************/
static void getPendingAck(void)
{
    int result = 1;
    byte messageNumber = 1;

    while (messageNumber != 0) 
    {
        messageNumber = PendingAckMessageNo;
        PendingAckMessageNo = 0;

            /* If any acknowledge is received. */
        if (messageNumber != 0) 
        { 
            AckMsgQueueItem *AckMsg = queueGetW(&AckMsgq);
            if (AckMsg == NULL) 
				break;

			if (AckMsg->messageNumber == messageNumber) 
            {
                result = queueDrop(&AckMsgq);
                AckMsgSendCount = 0;
                AckMsgOkToSend = true;

#ifdef DEBUG_CONSOLE
				{
					dword currentTime;              
					getSystemTime(&currentTime);
					prf(prfInfo,"Ack received after %ldms\n",currentTime-AckMsgSendTime);
				}
#endif
            }            
        }
    }
    if (result == 0) 
    {
        ReportFailure("Error at queue drop!", __FILE__, __LINE__); /* ("error at queue drop", ...); */
    }
}

/**********************************************************
* Function:    sendData
* Description: Sends data on TCP/IP port. Buffers unwritten data
**********************************************************/
static void sendData(void)
{
    word length;
    int32 bytesWritten;
    word vCount;

    if (noOfBytesInBuffer == 0)
        getDataFromQueues(&length, &buffer[noOfBytesInBuffer], &vCount);
    else
        length = 0;
    
    noOfBytesInBuffer += length;
    
    if (noOfBytesInBuffer <= 0)
        return;

			    // Write to connected socket
    if (writeSocket(ConnectedSocket,(char*)buffer, noOfBytesInBuffer, &bytesWritten) == TB_ERROR) 
    {
        ReportFailure("writeSocket failed", __FILE__, __LINE__);
    } 
    else 
    {
#ifdef DEBUG_CONSOLE
        if (DebugLevel >= 2)
            prtBundle(bytesWritten, buffer);
#endif

        if (bytesWritten < noOfBytesInBuffer) 
        {       // Not all bytes written, save the rest for next write
            memcpy(buffer, &buffer[bytesWritten], noOfBytesInBuffer - bytesWritten);
            noOfBytesInBuffer -= bytesWritten;
        } 
        else
                // All bytes written, nothing left to write
            noOfBytesInBuffer = 0;
    }

}

/**********************************************************
* Function:     mmicl_send
* Description:  Calls the send function in mmicl
* Return Value: None
**********************************************************/
void MMICL_send(void)
{
  AckMessage();
  getPendingAck();
  sendData();
}

/**********************************************************
* Function:     MMICL_close_connection()
* Description:  Close the connection
**********************************************************/
static void MMICL_close_connection(void)
{
    closeSocket(ConnectedSocket);
}
/**********************************************************
* Function:     MMICL_connected()
* Description:  Check that client is still connected
*               Returns TB_OK if socket is still working OK
**********************************************************/
static int32 MMICL_connected(void)
{
    return isConnectedSocket(ConnectedSocket);
}
/**********************************************************
* Function:     MMICL_accept_connection()
* Description:  Wait for connection attempt from client and then accept
*				Change the connected socket to non-blocking
*               Returns TB_OK when connection accepted OK
**********************************************************/
static int32 MMICL_accept_connection(void)
{
	if (acceptSocket(ListenSocket, &ConnectedSocket)==TB_OK)
	{
		SetNonBlockingSocket(ConnectedSocket);
		return TB_OK;
	} 
	else 
		return TB_ERROR;
}

/**********************************************************
* Function:     mmiclMainThreadProc() / MMICL_main()
* Description:  Main thread in MMICL for MMI
**********************************************************/
static bool mmiclShutDown;
HANDLE mmiclMainThread;
DWORD mmiclMainThreadID;

long WINAPI mmiclMainThreadProc(LPARAM lparam) 
{
    while (!mmiclShutDown)
	{
	    if (MMICL_accept_connection()==TB_OK)
		{
		    while ((MMICL_connected()==TB_OK) && !mmiclShutDown) 
			{
			    MMICL_receive();
			    MMICL_send();
			    Sleep(MMICL_PERIOD);
		    }
		    
		    MMICL_close_connection();
		    Sleep(MMICL_PERIOD);
		    if (mmiclShutDown)
			    break;	// Do not check for new connection if we are terminating
        }
	}   
  return 1;
}

/**********************************************************
* Function:     mmicl_init
* Description:  Calls the init functions of all other modules
*               in mmicl
* Return Value: TB_OK (0) if success, TB_ERROR (-1) if failure
**********************************************************/
int MMICL_init(int port, int debug)
{
    int retval;

    DebugLevel = debug;

    mmiclShutDown = false;
    mmiclMainThread = CreateThread (NULL,
                                  0, // dwStackSize
                                  (LPTHREAD_START_ROUTINE)mmiclMainThreadProc,
                                  0, // lpParameter
                                  CREATE_SUSPENDED,
                                  &mmiclMainThreadID);
                                  
    if (mmiclMainThread == INVALID_HANDLE_VALUE)
        return TB_ERROR;
    
    retval = initIP(port);
  
    if (retval != TB_OK) 
    {
        CloseHandle(mmiclMainThread);
        return retval;
    }

#ifdef DEBUG_CONSOLE
    if (debug) 
    {
        char buf[32];
        sprintf_s(buf, sizeof(buf), "mmicl.dll com%d", port);
        prfInit(buf, 1);
        prf(prfInfo, "--Starting--\n");
    }
#endif


    crc32_init(); 
  
    LastAckMsgMessageNo     = 0;
    AckRequestMessageNo    = 0;
    PendingAckMessageNo    = 0;
    ErrCount               = 0;
    queueZap(&outq);

    if (!queueInit(&outq, sizeof(OutputQueueItem), OUTQUEUE_LEN))
        ReportFailure("queueInit error!",__FILE__, __LINE__);

  
    AckMsgSendTime          = 0;
    AckMsgSendCount         = 0;
    AckMsgErrCount          = 0;
    MessageNo              = 1;
    AckMsgOkToSend       = true;

    /* Call queueZap on each queue so that it is safe to call exitmmicl01() even
     * if we fail in a queueInit() below. */
    queueZap(&AckMsgq);
    queueZap(&UnackMsgq);
    queueZap(&Ackq);

    if (!queueInit(&AckMsgq, sizeof(AckMsgQueueItem), QUEUE_LEN) || /* Last param. = no. of items in queue */
        !queueInit(&UnackMsgq, sizeof(UnackMsgQueueItem), QUEUE_LEN) ||
        !queueInit(&Ackq, sizeof(AckQueueItem), QUEUE_LEN))
    ReportFailure("queueInit error!",__FILE__, __LINE__);
    
    inbufLength = 0;
  
    ResumeThread(mmiclMainThread);
    
    return TB_OK;
}

/**********************************************************
* Function:     mmicl_exit
* Description:  Calls the exit functions in mmicl
* Return Value: None
**********************************************************/
void MMICL_exit(void)
{
        // Set variable to terminate the thread
    mmiclShutDown = true;
    WaitForSingleObject(mmiclMainThread, INFINITE);

    queueExit(&Ackq);
    queueExit(&UnackMsgq);
    queueExit(&AckMsgq);

    queueExit(&outq);

    CloseHandle(mmiclMainThread);
    exitIP();
}
/**********************************************************
* Function:     DllEntryPoint
* Description:  
* Return Value: 
**********************************************************/
BOOL WINAPI DllEntryPoint(
    HINSTANCE  hinstDLL,  // handle of DLL module
    DWORD  fdwReason,     // reason for calling function
    LPVOID  lpvReserved)  // reserved
  {
  (void)hinstDLL; (void)lpvReserved;
  switch(fdwReason) {
    case DLL_PROCESS_ATTACH:
      return TRUE;
    case DLL_THREAD_ATTACH:
      break;
    case DLL_THREAD_DETACH:
      break;
    case DLL_PROCESS_DETACH:
      break;
    default:
      break;
  }
  return FALSE;
}

/* At startup, the function _GetExceptDLLinfo() is looked for
 * (via GetProcAddress()) and fails according to CodeGuard.
 */




/**********************************************************
* Function:    addMessageToQueue
* Description: Queue handling function. Adds an item to a queue.
*              Returns TB_OK if success, TB_ERROR if failure. 
*
*              Returns TB_OK if succesful, else TB_ERROR 
**********************************************************/
static int addMessageToQueue(uint16 dataLength,uint8 *data)
{
    AckMsgQueueItem *AckMsg;
    UnackMsgQueueItem *UnackMsg;
	char MsgBuf[80];

	uint8 MessageType;				/* First byte of data*/
									/* Most significant bit is set when the message needs to be acknowledged */	
  
    if (dataLength > 0) 
    {
		MessageType = data[0];

        /**********************************************************/
        /* Check that the message length is okay, if a message    */
        /* is too long it will not be added to the queue. If we   */
        /* were to add a message that is too long it would never  */
        /* be delivered.										  */
		/* It would freeze all subsequent messages and the		  */
		/* communication would									  */
        /* die.                                                   */
        /*														  */	
		/* The messages arriving here starts with a two-byte	  */	
		/* length followed  by the data.						  */
		/*														  */
		/* The first byte of the data is the message-type (MT)    */
        /**********************************************************/

        if (dataLength > MMI_DATA_MAX_SIZE)
        {
            sprintf_s(MsgBuf,sizeof(MsgBuf),"Message too long for sending, skipped! MsgType: %d\n", MessageType);
            ReportFailure(MsgBuf,__FILE__, __LINE__);
        }
        else
        { 

            if (MessageType & 0x80) 
            {		/* Acknowledged message */
                AckMsg = queuePutW(&AckMsgq);
                if (AckMsg == NULL) 
                {
                    ReportFailure("Acknowledged messages queue full!",__FILE__, __LINE__);
                    return TB_ERROR;
                }
                else 
                {          
                    MessageNo++;
                    if (MessageNo > MAX_MESSAGE_NO) 
                    {
                        MessageNo = 1;
                    }
          
					AckMsg->messageNumber = (uint8)MessageNo;
					AckMsg->Length = MMI_HEADER_SIZE + dataLength + MMI_TRAILER_SIZE;
					CreateHeader(AckMsg->msg, 0x10, AckMsg->messageNumber, dataLength);	
                    memcpy(&AckMsg->msg[MMI_HEADER_SIZE], data, dataLength); /* data */
					CreateTrailer(AckMsg->msg, MMI_HEADER_SIZE + dataLength);

                    queueCommit(&AckMsgq);
#ifdef DEBUG_CONSOLE
					{
						int ThisCount = queueCount(&AckMsgq);
						if (ThisCount > MaxAckMsgqCount)
						{
							prf(prfRcv,"AckMsgqCount:%d\n",ThisCount);
							MaxAckMsgqCount=ThisCount;	
						}
					}
#endif

                }
            }
            else 
            {            /* Unacknowledged */
                UnackMsg = queuePutW(&UnackMsgq);
                if (UnackMsg == NULL) 
                {
                    ReportFailure("Unacknowledged messages queue full!",__FILE__, __LINE__);
                    return TB_ERROR;
                }
                else 
                {
					UnackMsg->Length = MMI_HEADER_SIZE + dataLength + MMI_TRAILER_SIZE;	
					CreateHeader(UnackMsg->msg, 0, 0, dataLength);	
                    memcpy(&UnackMsg->msg[MMI_HEADER_SIZE], data, dataLength); /* data */
					CreateTrailer(UnackMsg->msg, MMI_HEADER_SIZE + dataLength);
                    queueCommit(&UnackMsgq);

#ifdef DEBUG_CONSOLE
					{
						int ThisCount = queueCount(&UnackMsgq);
						if (ThisCount > MaxUnackMsgqCount)
						{
							prf(prfRcv,"UnackMsgqCount:%d\n",ThisCount);
							MaxUnackMsgqCount=ThisCount;	
						}
					}
#endif
                }
            }
        }
    }
    return TB_OK;
}



/**********************************************************
* Function:    MMICL_putMessageDataToATP
*
* Description: The new protocol allows a 16-bit datalength and
* expects the MessageType in the first byte of the data
*
* Called by the DELPHI MMI program when something is to be
* transmitted to the ATP.
*
* Returns TB_OK if successful else TB_ERROR  
**********************************************************/
int MMICL_putMessageDataToATP(uint16 dataLength, void *data) 
{
    return addMessageToQueue(dataLength, data);
}

/**********************************************************
* Function:    getNextMsg   
* Description: Get next message from output queue (i.e. from ATP to MMI)
**********************************************************/
static void getNextMsg (uint16 *dataLength, void *data)
{
 	OutputQueueItem *q;

    q = queueGetW(&outq);
    if (NULL == q) 
    {
        *dataLength = 0;
        (void)data;
    }
    else
    {
		*dataLength = VWORD(q->msg, 3);
        memcpy(data,&q->msg[MMI_HEADER_SIZE], *dataLength);
        (void)queueDrop(&outq);
    } 
}


/**********************************************************
* Function:    MMICL_getNextMsgDataFromATP
* Description: Get next message from output queue 
*               (i.e. output from ATP to MMI)
*
**********************************************************/
void MMICL_getNextMsgDataFromATP(uint16 *dataLength, void *data)
{
    getNextMsg(dataLength, data);
}
