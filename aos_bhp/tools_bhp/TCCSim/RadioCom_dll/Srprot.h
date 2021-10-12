/* srprot.h
 *
 * The radio interface of a simulated stationary system.
 *
 * Mats Åkerblom  Kvaser AB  1998-04-06
 */

#ifndef __SRPROT_H
#define __SRPROT_H

#ifdef DEBUG_CONSOLE
typedef enum {
  prfInfo = 0,
  prfRcv,
  prfTx,
  prfError,
  prfBegin,
  prfEnd
} prfT;

void prfInit(const char *title, int verboseI);
void prf(const prfT p, const char *fmt, ...);
#endif // DEBUG_CONSOLE


//int __declspec(dllexport) _rpInit(int comPort, int trainIdI, int debug);
__declspec(dllexport)int _rpInit(int comPort, int trainIdI, int debug);

#define rpOK 0
#define rpComError -1
#define rpError -2

//void __declspec(dllexport) _rpExit(void);
__declspec(dllexport)void _rpExit(void);

//int __declspec(dllexport) _rpTxMessage(byte *msg, int dataLen, int retransmission, int waitForResponse,int crcError);
__declspec(dllexport)int _rpTxMessage(byte *msg, int dataLen, int retransmission, int waitForResponse,int crcError);
#define txOK 0
#define txBusyReceiving -1
#define txError -2

//int __declspec(dllexport) _rpGetResponse(int maxLen, byte *msg, int *len);
__declspec(dllexport)int _rpGetResponse(int maxLen, byte *msg, int *len);
#define rxsNothing         0
#define rxsOK              1 /* For positive return codes, a message is stored */
#define rxsOK_overrun      2
#define rxsOK_formError    3
#define rxsMessTimeout    -4 /* For negative values, nothing is stored. */
#define rxsCharTimeout    -5
#define rxsTimestampError -6
#define rxsFormError      -7
#define rxsCrcError       -8
#define rxsError          -9

#endif /* __SRPROT_H */

