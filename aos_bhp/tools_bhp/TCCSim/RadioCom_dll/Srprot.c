/* srprot.c
 *
 * The radio interface of a simulated stationary system.
 *
 * Mats Åkerblom  Kvaser AB  1998-04-06
 *
 *
 *  Macros used to control the package:
 *    DEBUG_CONSOLE
 *       A console is allocated and debug messages is printed to it.
 *
 */
/****************************************************************************
*
*    REVISION HISTORY :
*
*    Date       Name        Measures
*    -----------------------------------------------------------------------
*    2009-03-30 Antbäck     Rewritten BCC to support latest version in radio 
*                           protocol. Also turned off debug console.
****************************************************************************/
//#define DEBUG_CONSOLE

#include <stdio.h>
#include <windows.h>
#include <stdarg.h>
#include "atpdefs.h"
#include "crc32.h"
#include "srprot.h"

#define STX 0x02

#define T_CHAR_TIMEOUT 200
#define T_MESS_TIMEOUT 5000

#define TREF_STARTUP 0x5665

#define RX_MAX_LEN 1024

#define false 0;
#define true 1;


// CRC table for new BCC
static const unsigned char crcTable[256] =
{ 0x00, 0x85, 0x8F, 0x0A, 0x9B, 0x1E, 0x14, 0x91, 0xB3, 0x36,
  0x3C, 0xB9, 0x28, 0xAD, 0xA7, 0x22, 0xE3, 0x66, 0x6C, 0xE9,
  0x78, 0xFD, 0xF7, 0x72, 0x50, 0xD5, 0xDF, 0x5A, 0xCB, 0x4E,
  0x44, 0xC1, 0x43, 0xC6, 0xCC, 0x49, 0xD8, 0x5D, 0x57, 0xD2,
  0xF0, 0x75, 0x7F, 0xFA, 0x6B, 0xEE, 0xE4, 0x61, 0xA0, 0x25,
  0x2F, 0xAA, 0x3B, 0xBE, 0xB4, 0x31, 0x13, 0x96, 0x9C, 0x19,
  0x88, 0x0D, 0x07, 0x82, 0x86, 0x03, 0x09, 0x8C, 0x1D, 0x98,
  0x92, 0x17, 0x35, 0xB0, 0xBA, 0x3F, 0xAE, 0x2B, 0x21, 0xA4,
  0x65, 0xE0, 0xEA, 0x6F, 0xFE, 0x7B, 0x71, 0xF4, 0xD6, 0x53,
  0x59, 0xDC, 0x4D, 0xC8, 0xC2, 0x47, 0xC5, 0x40, 0x4A, 0xCF,
  0x5E, 0xDB, 0xD1, 0x54, 0x76, 0xF3, 0xF9, 0x7C, 0xED, 0x68,
  0x62, 0xE7, 0x26, 0xA3, 0xA9, 0x2C, 0xBD, 0x38, 0x32, 0xB7,
  0x95, 0x10, 0x1A, 0x9F, 0x0E, 0x8B, 0x81, 0x04, 0x89, 0x0C,
  0x06, 0x83, 0x12, 0x97, 0x9D, 0x18, 0x3A, 0xBF, 0xB5, 0x30,
  0xA1, 0x24, 0x2E, 0xAB, 0x6A, 0xEF, 0xE5, 0x60, 0xF1, 0x74,
  0x7E, 0xFB, 0xD9, 0x5C, 0x56, 0xD3, 0x42, 0xC7, 0xCD, 0x48,
  0xCA, 0x4F, 0x45, 0xC0, 0x51, 0xD4, 0xDE, 0x5B, 0x79, 0xFC,
  0xF6, 0x73, 0xE2, 0x67, 0x6D, 0xE8, 0x29, 0xAC, 0xA6, 0x23,
  0xB2, 0x37, 0x3D, 0xB8, 0x9A, 0x1F, 0x15, 0x90, 0x01, 0x84,
  0x8E, 0x0B, 0x0F, 0x8A, 0x80, 0x05, 0x94, 0x11, 0x1B, 0x9E,
  0xBC, 0x39, 0x33, 0xB6, 0x27, 0xA2, 0xA8, 0x2D, 0xEC, 0x69,
  0x63, 0xE6, 0x77, 0xF2, 0xF8, 0x7D, 0x5F, 0xDA, 0xD0, 0x55,
  0xC4, 0x41, 0x4B, 0xCE, 0x4C, 0xC9, 0xC3, 0x46, 0xD7, 0x52,
  0x58, 0xDD, 0xFF, 0x7A, 0x70, 0xF5, 0x64, 0xE1, 0xEB, 0x6E,
  0xAF, 0x2A, 0x20, 0xA5, 0x34, 0xB1, 0xBB, 0x3E, 0x1C, 0x99,
  0x93, 0x16, 0x87, 0x02, 0x08, 0x8D };



//void WriteStrToLog(char *Str);

byte trainId;
HANDLE hComm = INVALID_HANDLE_VALUE;
HANDLE readThread = INVALID_HANDLE_VALUE;
HANDLE readSignal; /* Waited on by rpReadThread when rpState==rpsIdle. */
DWORD readThreadID;

boolA rpShutDown;


int lastRcvTimeStamp;
int t_sender;

int rxMsgLen;
byte rxMsg[RX_MAX_LEN+5+8]; // Data, CRC and BCC.
int msgLost;
int rxError;
int rxFormError;

/* rpState is set to rpsIdle by readThread when a message has been received
 * (or failed),
 * set to rpsReceiving by txMessage when a message is transmitted and a response
 * is expected (and readSignal is signalled).
 */
enum RPState {
  rpsIdle = 0,
  rpsReceiving
} rpState;


static long WINAPI readThreadProc(LPARAM lparam);
static const char *commSettings = "baud=19200 parity=E data=8 stop=1";

/* Initiate the radio protocol. After this call, reference times etc are
 * after start-up. The comport in comPort (1,2,...) is used.
 * Returns 0 if success, non zero id error.
 *
 * Return codes
 *  rpOK
 *     OK.
 *  rpComError
 *     Error accessing the com port.
 *  rpError
 *     Some other error occurred.
 */
__declspec(dllexport)int _rpInit(int comPort, int trainIdI, int debug)
//int _rpInit(int comPort, int trainIdI, int debug) 
{
	
/*  int debug = 1;*/
  DCB dcb;
  COMMTIMEOUTS timeouts;
  BOOL errF;
  char comDeviceStr[10];


  trainId = (byte)trainIdI;
  sprintf(comDeviceStr, "com%u", comPort);

  crc32_init();

#ifdef DEBUG_CONSOLE
  if (debug) {
    char buf[32];
    sprintf(buf, "srprot.dll com%d", comPort);
    prfInit(buf, 1);
    prf(prfInfo, "--Starting--\n");
  }
#endif

  hComm = CreateFile(comDeviceStr,
                     GENERIC_READ | GENERIC_WRITE,
                     0, // dwShareMode, no sharing is allowed
                     NULL, // pSecurityAttributes,
                     OPEN_EXISTING,
                     FILE_FLAG_WRITE_THROUGH,
                     0);   // hTemplateFile
  if (hComm == INVALID_HANDLE_VALUE)
    return rpComError;

  ZeroMemory(&dcb, sizeof(dcb));
  dcb.DCBlength = sizeof(dcb);
  if (!BuildCommDCB(commSettings, &dcb)) {
    CloseHandle(hComm);
    hComm = INVALID_HANDLE_VALUE;
    return rpComError;
  }
  dcb.fDtrControl = DTR_CONTROL_DISABLE;
  dcb.fRtsControl = RTS_CONTROL_DISABLE;

  // Read and write operations should never timeout
  timeouts.ReadIntervalTimeout = T_CHAR_TIMEOUT; /* and wait no more than 200 ms for a single character */
  timeouts.ReadTotalTimeoutMultiplier = 0;
  timeouts.ReadTotalTimeoutConstant = T_MESS_TIMEOUT;
  timeouts.WriteTotalTimeoutMultiplier = 0;
  timeouts.WriteTotalTimeoutConstant = 0;

  errF = !SetCommState(hComm, &dcb);
  if (!errF)
    errF = !SetCommTimeouts(hComm, &timeouts);

  if (!errF) {
    readThread = CreateThread (NULL,
                               0, // dwStackSize
                               (LPTHREAD_START_ROUTINE)readThreadProc,
                               0, // lpParameter
                               CREATE_SUSPENDED,
                               &readThreadID);
    readSignal = CreateEvent(NULL,  // lpEventAttributes
                             FALSE, // bManualReset
                             FALSE, // bInitialState
                             NULL); // lpName
  }

  if (errF ||
      readThread == INVALID_HANDLE_VALUE ||
      readSignal == INVALID_HANDLE_VALUE) {
    CloseHandle(hComm);
    CloseHandle(readThread);
    CloseHandle(readSignal);
    hComm = INVALID_HANDLE_VALUE;
    readThread = INVALID_HANDLE_VALUE;
    readSignal = INVALID_HANDLE_VALUE;
    if (errF)
      return rpComError;
    else
      return rpError;
  }
  rpShutDown = falseA;
  rpState = rpsIdle;
  rxMsgLen = 0;
  msgLost = 0;
  rxError = 0;
  rxFormError = 0;

  lastRcvTimeStamp = TREF_STARTUP;
  t_sender = 0;

  ResumeThread(readThread);

  return rpOK;
}

/* Shuts down the system including closing of the com port.
 */
__declspec(dllexport)void _rpExit(void)
//void _rpExit(void) 
{
  /* There should be no harm in callinf CloseHandle(INVALID_HANDLE_VALUE) */
  rpShutDown = trueA;
  SetEvent(readSignal); /* Wake up the thread if it is sleeping */
  WaitForSingleObject(readThread, INFINITE);
  CloseHandle(hComm);
  CloseHandle(readThread);
  CloseHandle(readSignal);
  hComm = INVALID_HANDLE_VALUE;
  readThread = INVALID_HANDLE_VALUE;
  readSignal = INVALID_HANDLE_VALUE;
}


/* Transmit a message; after it, if waitForResponse, we expect an answer from
 * the train.
 * In case of error, a non-zero error code is returned.
 * The control is returned to the caller as soon as the message is placed in the
 * transmit queue; there is no wait for an answer.
 * The message should contain only the data part; extra protocol information
 * such as reference times and CRC is added by the function.
 * If the previous transmision failed, one should probably let retransmission
 * be true making the reference tim ethe same (causing the train to skip the
 * data if the previous message was received).
 *
 * Return codes
 *  txOK
 *     Message is queued.
 *  txBusyReceiving
 *     The receiver is busy; nothing is transmitted (nor queued).
 *     Try again later.
 *  txError
 *     Something went wrong.
 *
 *  Lead in of 5 0x7e and one terminating 0x7e added 980520 SIGJKG
 */
#define LEADIN_SIZE 5
#define LEADIN_CHAR 0x7e
__declspec(dllexport)int _rpTxMessage(byte *msg, int dataLen, int retransmission, int waitForResponse, int crcError)
//int _rpTxMessage(byte *msg, int dataLen, int retransmission, int waitForResponse,int crcError) 
{
  byte leadIn[LEADIN_SIZE];  // 5 bytes of LEADIN_CHAR
  byte msgHead[8]; // {STX, ID, MSB(LEN),LSB(LEN), MSB(T_SENDER),LSB(T_SENDER), MSB(T_REF),LSB(T_REF)}
  byte msgCrc[4];
  DWORD csum;
  DWORD dwWritten;
  byte bcc = 0;
  int j;
  int i;

  if (rpState != rpsIdle)
    return txBusyReceiving;

  if (!retransmission)
    t_sender = (unsigned short)(GetTickCount()/16);

#ifdef DEBUG_CONSOLE
  prf(prfTx, "[%d:%04x,%04x ", trainId, t_sender, lastRcvTimeStamp);
  if (retransmission || waitForResponse) {
    if (retransmission)
      prf(prfTx, "R");
    if (waitForResponse)
      prf(prfTx, "W");
    prf(prfTx, " ");
  }
  for (i = 0; i < dataLen; i++) {
    if (i)
      prf(prfTx, " ");
    prf(prfTx, "%02x", msg[i]);
  }
  prf(prfTx, "]\n");
#endif

  for (i=0;i<LEADIN_SIZE;i++) {
    leadIn[i] = LEADIN_CHAR;
  }/*
  msgHead[0] = STX;
  msgHead[1] = trainId;
  VPUTWORD(msgHead, 2, dataLen);
  VPUTWORD(msgHead, 4, t_sender);
  VPUTWORD(msgHead, 6, lastRcvTimeStamp);
  csum = crc32_update(0, msgHead+1, sizeof(msgHead)-1);
  csum = crc32_update(csum, msg, dataLen);

  if (crcError)
    csum++;   /* Inject crc error */

//  VPUTDWORD(msgCrc, 0, csum);

  /* Calculate longitudinal BCC. (konedlu 2003-10-06) */
/*  bcc = 0;

  for (j = 0; j < sizeof(msgHead); j++)
    bcc = crcTable[bcc ^ msgHead[j]]; 
  for (j = 0; j < dataLen; j++)
    bcc = crcTable[bcc ^ msg[j]]; 
  for (j = 0; j < sizeof(msgCrc); j++)
    bcc = crcTable[bcc ^ msgCrc[j]]; */

  /* Set RTS high to enable the radio. */

  /* Write the message to the port. */
  if (!WriteFile(hComm, leadIn, LEADIN_SIZE, &dwWritten, NULL) ||
  		dwWritten != LEADIN_SIZE ||

  		/*!WriteFile(hComm, msgHead, sizeof(msgHead), &dwWritten, NULL) ||
      dwWritten != sizeof(msgHead) ||*/

      !WriteFile(hComm, msg, dataLen, &dwWritten, NULL) ||
      dwWritten != dataLen ||

     /* !WriteFile(hComm, msgCrc, sizeof(msgCrc), &dwWritten, NULL) ||
      dwWritten != sizeof(msgCrc) ||

      !WriteFile(hComm, &bcc, 1, &dwWritten, NULL) ||
      dwWritten != 1 ||*/

      !WriteFile(hComm, leadIn, 1, &dwWritten, NULL) ||
      dwWritten != 1) 
  {
 #ifdef DEBUG_CONSOLE
   prf(prfError, "Tx Error!\n");
 #endif
    return txError;
  }

  if (waitForResponse) {
    /* Activate the receiver */
    rpState = rpsReceiving;
    SetEvent(readSignal);
  }

  return txOK;
}

/* Returns the latest message from the train.
 * Return codes:
 *  rxsNothing
 *     Nothing is received.
 *  rxsOK
 *     A message is received and placed in msg[] length in *len.
 *  rxsOK_overrun
 *     Overrun; a message was received that overwrote the previous message;
 *     the latest message is returned in msg[]/*len.
 * rxsOK_formError
 *     A message was received but it was preceeded by illegal data bytes.
 * rxsMessTimeout
 *     Message timeout, no response was received. A retransmission should
 *     be done (a new call of rpTxPollMessage() must be done).
 * rxsCharTimeout
 *     Char timeout error. Time between two characters was too long and
 *     the message is discarded. Retransmit.
 * rxsTimestampError
 *     Time stamp error. The message should be discarded.
 * rxsFormError
 *     Format error. Resend the previous message.
 * rxsCrcError
 *     CRC-Error. An illegal response has been received. The caller should
 *     probably resend the previous message.
 * rxsError
 *     Some other error
 */
__declspec(dllexport)int _rpGetResponse(int maxLen, byte *msg, int *len)
//int _rpGetResponse(int maxLen, byte *msg, int *len) 
{
  if (rxMsgLen) 
  {
    *len = rxMsgLen;
    memcpy(msg, rxMsg, min(maxLen, rxMsgLen));
#ifdef DEBUG_CONSOLE
    prf(prfInfo, "(read %d bytes)", *len);
#endif
    rxMsgLen = 0;
    if (msgLost) 
	{
      msgLost = 0;
      return rxsOK_overrun;
    } 
	else if (rxFormError) 
	{
      rxFormError = 0;
      return rxsFormError;
    } 
	else
      return rxsOK;
  } 
  else 
  {
    if (rpState == rpsIdle && rxError) 
	{
      int ret = rxError;
      rxError = 0;
      return ret;
    } 
	else
      return rxsNothing;
  }
}


/* The receiving thread.
 *
 * We wait for a signal that we should start to receive; the signaller should
 * have ensured we are in the idle state and set rpState to rpsReceiving before
 * signalling. We start to read, skipping illegal charcaters in the beginning
 * (we wait for a STX). After this we read the head, the message and the crc.
 * In case of timeout, rxError is assigned and we return to the
 * idle state.
 *
 * As we read in three steps (STX, head, data+CRC) and use the built-in
 * timeouts, a long timeout after STX and after the head may be accepted
 * even as we really should be using the inter-character timeout there.
 */
long WINAPI readThreadProc(LPARAM lparam) {
  int skipping;
  DWORD res;
  byte msgHead[8]; // {STX, ID, MSB(LEN),LSB(LEN), MSB(T_SENDER),LSB(T_SENDER), MSB(T_REF),LSB(T_REF)}
  byte tempdata[RX_MAX_LEN+5];
  DWORD msgCrc, crc;
  DWORD dwRead;
  DWORD err;
  int dataLen;
  byte calcBCC;
  byte recBCC;
  int j, i;
#ifdef DEBUG_CONSOLE
  int i;
#endif

  (void)lparam;
  while (!rpShutDown) {
#ifdef DEBUG_CONSOLE
    if (rxError)
      prf(prfError, "(rxErr %d)", rxError);
#endif
    rpState = rpsIdle;
    WaitForSingleObject(readSignal, INFINITE);
    rxError = 0;
    /* Start to receive a message. */
    skipping = (rxMsgLen != 0);
    if (skipping)
      msgLost++;
    /* There may be garbage in the beginning. Read until we
     * we get an STX or we time out, in the latter case, we should abort
     * the read operation. */
    while (!rpShutDown) {
      /* Read one character until STX */
		
      res = ReadFile(hComm, msgHead, 1, &dwRead, NULL);
	  
      if (!res || dwRead != 1) {
        if (res || (err = GetLastError()) == STATUS_TIMEOUT)
          rxError = rxsMessTimeout;
        else
          rxError = rxsError;
        break;
      }
      if (msgHead[0] == STX)
        break;
      if (msgHead[0] != LEADIN_CHAR)
        rxFormError++;
    }
    if (rpShutDown)
      break;

    res = ReadFile(hComm, msgHead+1, sizeof(msgHead)-1, &dwRead, NULL);
    if (!res || dwRead != sizeof(msgHead)-1) {
      if (res || (err = GetLastError()) == STATUS_TIMEOUT)
        rxError = rxsCharTimeout;
      else
        rxError = rxsError;
      continue;
    }

    dataLen = VWORD(msgHead, 2);
    if (dataLen > RX_MAX_LEN) {
      rxError = rxsFormError;
      continue;
    }

    /* There is room for five extra bytes in the receive buffer */
    res = ReadFile(hComm, tempdata, dataLen+5, &dwRead, NULL);
    if (!res || dwRead != dataLen+5) {
      if (res || (err = GetLastError()) == STATUS_TIMEOUT)
        rxError = rxsCharTimeout;
      else
        rxError = rxsError;
      continue;
    }

    /* Check the BCC. (konedlu 2003-10-06) */
    calcBCC = 0;
    for (j = 0; j < sizeof(msgHead); j++)
      calcBCC = crcTable[calcBCC ^ msgHead[j]]; 
    for (j = 0; j < dataLen + 4; j++)
      calcBCC = crcTable[calcBCC ^ tempdata[j]]; 
    recBCC = tempdata[dataLen + 4];
    if (recBCC != calcBCC)
    {
      rxError = rxsError;
      continue;
    }

    crc = crc32_update(0, msgHead+1, sizeof(msgHead)-1);
    crc = crc32_update(crc, tempdata, dataLen);
    msgCrc = VDWORD(tempdata, dataLen);
    if (crc != msgCrc) {
      rxError = rxsCrcError;
      continue;
    }
    lastRcvTimeStamp = VWORD(msgHead, 4);

#ifdef DEBUG_CONSOLE
    prf(prfRcv, "(%d:%04x,%04x ", msgHead[1], lastRcvTimeStamp, VWORD(msgHead, 6));
    for (i = 0; i < dataLen; i++) {
      if (i)
        prf(prfRcv, " ");
      prf(prfRcv, "%02x", rxMsg[i]);
    }
    prf(prfRcv, ")\n");
#endif
    rxMsgLen = dataLen + 8 + 5;
	memcpy(rxMsg, msgHead, 8);
	for(i = 0; i < sizeof(tempdata)/sizeof(*tempdata); i++)
	{
		rxMsg[i+8] = tempdata[i];
	}
  }
  return 1;
}

/* ========================================================================= */
/* Console handling functions. */

#ifdef DEBUG_CONSOLE
static HANDLE hConsole = INVALID_HANDLE_VALUE;
static const COORD consoleSize = {80,70}; // Columns, rows
static const WORD consoleBackground = BACKGROUND_RED + BACKGROUND_GREEN + BACKGROUND_BLUE + BACKGROUND_INTENSITY;
static int prfVerbose = 0;

void prfInit(const char *title, int verboseI) {
  SMALL_RECT consoleRect; // = {0,0,consoleSize.X-1,consoleSize.Y-1};
  COORD zCord = {0,0};
  DWORD z;

  prfVerbose = verboseI;

/*if (hConsole == INVALID_HANDLE_VALUE) {
    if (!AllocConsole()) {
      z = GetLastError();
      return;
    }
    hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hConsole == INVALID_HANDLE_VALUE)
      return;
  }*/

  /* Behaviour in a DLL called from a Delphi programme:
   * Calling GetStdHandle() before AllocConsole works (does not return
   * INVALID_HANDLE_VALUE), but printing to it doesn't show anything.
   * Calling AllocCOnsole() returns True (OK) and displays a window after
   * which it works.
   */

  if (!AllocConsole()) {
    z = GetLastError();
    /* We continue anyway as there may already be a console. */
  }

  hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
  if (hConsole == INVALID_HANDLE_VALUE)
    return;

  if (title)
    SetConsoleTitle(title);

  consoleRect.Left = 0; consoleRect.Top = 0;
  consoleRect.Right = consoleSize.X-1; consoleRect.Bottom = consoleSize.Y-1;

  SetConsoleScreenBufferSize((HANDLE)hConsole, consoleSize);
  SetConsoleWindowInfo((HANDLE)hConsole, 1, &consoleRect);
  SetConsoleTextAttribute((HANDLE)hConsole, consoleBackground);
  /* Assign the whole screen the background color. */
  FillConsoleOutputAttribute((HANDLE)hConsole, consoleBackground, consoleSize.X*consoleSize.Y, zCord, &z);

/*{ // This doesn't work.
  HWND hW = GetTopWindow(NULL);
  prf (prfInfo, "%d %d\n", GetActiveWindow(), GetTopWindow(NULL));
  if (!SetWindowPos(hW, // HWND  hwnd,	handle of window
                    HWND_TOP, //MOST, // HWND  hwndInsertAfter, placement-order handle
                    0, // int  x,	// horizontal position
                    0, // int  y,	// vertical position
                    110, // int  cx,	// width
                    110, // int  cy,	// height
                    0)) // SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE)) // UINT  fuFlags, window-positioning flags
    prf(prfError, "SetWindowPos: Error %d\n", GetLastError());
  }*/
}

/* Prints as printf but different colors depending on mode.
 * It can be ensured that a single printout never is interrupted by another
 * thread's (assuming all threads have the same priority).
 * For output with several calls, start by call prf with prfBegin; the printing
 * must be ended by prfEnd (fmt is ignored in both cases).
 * After printing, the prioroty is always reset to THREAD_PRIORITY_NORMAL.
 *
 * verbose >= 0: Print only prtInfo and prtError.
 *         >= 1: Print everything.
 *         >= 2: Prevent task switching while printing.
 *
 * If hConsole == INVALID_HANDLE_VALUE, nothing is displayed (this is the
 * case if prfInit() hasn't been called).
 */
void prf(const prfT p, const char *fmt, ...) {
  va_list argptr;
/*CONSOLE_SCREEN_BUFFER_INFO screenBufInfo;*/
  int oldPriority;

  if (hConsole == INVALID_HANDLE_VALUE) {
    return;
  }

  if (prfVerbose >= 2) {
    oldPriority = GetThreadPriority(GetCurrentThread());
    if (p == prfBegin) {
      SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
      return;
    } else if (p == prfEnd) {
      SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_NORMAL);
      return;
    }

    /* By rising the priority of the calling thread, we makes sure the printout is not interrupted. */
    SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
  } else if (p == prfBegin || p == prfEnd)
    return;

  /* Adding WORD's seems to result in DWORD or something which results in warnings below, thus the casts. */
  switch(p) {
    case prfInfo:
      SetConsoleTextAttribute((HANDLE)hConsole, consoleBackground); /* Black text  */
      break;
    case prfRcv:
      if (prfVerbose < 1)
        return;
      SetConsoleTextAttribute((HANDLE)hConsole, (WORD)(consoleBackground + FOREGROUND_RED + FOREGROUND_INTENSITY));
      break;
    case prfTx:
      if (prfVerbose < 1)
        return;
      SetConsoleTextAttribute((HANDLE)hConsole, (WORD)(consoleBackground + FOREGROUND_BLUE + FOREGROUND_INTENSITY));
      break;
    case prfError:
    default:
      SetConsoleTextAttribute((HANDLE)hConsole, (WORD)(consoleBackground + FOREGROUND_GREEN + FOREGROUND_INTENSITY));
      break;
  }
  va_start(argptr, fmt);
//vprintf(fmt, argptr); /* This seems to buffer the output ... */
  {
    char buf[1024];    /* ... so we use WriteConsole instead. */
    DWORD z;
    vsprintf(buf, fmt, argptr);
    WriteConsole(hConsole, buf, strlen(buf), &z, NULL);
  }
  va_end(argptr);

/*if ((prfBreakY || prfBreakX) &&
      GetConsoleScreenBufferInfo((HANDLE)hConsole, &screenBufInfo)) {
    if (screenBufInfo.dwCursorPosition.Y >= prfBreakY &&
      screenBufInfo.dwCursorPosition.X >= prfBreakX) {
      prfBreakX = prfBreakY = 0; // Avoid infinite recursion
      prf(prfInfo, "<BREAK>");
      prfBreakX = prfBreakY = 0; // The place for a break point
    }
  }*/

  if (prfVerbose >= 2)
    SetThreadPriority(GetCurrentThread(), oldPriority);
}

#endif // DEBUG_CONSOLE

/*************************************************************
*	WriteStrToLog
*
*	Write a string to the log file
*
**************************************************************/
/*
void WriteStrToLog(char *Str)
{
    HANDLE hFile; 
    DWORD dwBytesWritten;

	// Open file to write log to
	hFile = CreateFile("D:\\DLLDebug.txt", //BTLogFileName,    // name of the write
                       GENERIC_WRITE,          // open for writing
                       FILE_SHARE_READ,                      // do not share
                       NULL,                   // default security
                       OPEN_ALWAYS,          // overwrite existing
                       FILE_ATTRIBUTE_NORMAL,  // normal file
                       NULL);                  // no attr. template

	// Write to log file
    if (hFile != INVALID_HANDLE_VALUE) 
    { 
		// Position write pointer at end of file
		SetFilePointer(hFile,
					   0,
					   0,
					   FILE_END);

		dwBytesWritten = 0;

		WriteFile(hFile,           // open file handle
				  Str,			   // start of data to write
				  strlen(Str),  // number of bytes to write
				  &dwBytesWritten, // number of bytes that were written
				  NULL);           // no overlapped structure

		WriteFile(hFile,           // open file handle
				  "\r\n",			   // start of data to write
				  2,  // number of bytes to write
				  &dwBytesWritten, // number of bytes that were written
				  NULL);           // no overlapped structure

		CloseHandle(hFile);
    }
}
*/