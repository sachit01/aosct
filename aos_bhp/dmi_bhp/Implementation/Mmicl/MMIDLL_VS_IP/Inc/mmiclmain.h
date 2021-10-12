#ifndef MICLMAIN_H
#define MICLMAIN_H

/****************************************************************************
*           (C) COPYRIGHT Bombardier Transport AB, SWEDEN 2011.
*           ===================================================
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
*    MODULE NAME:  miclmain.h
*
*    REVISION:     1.0
*
*    PREPARED:     Bo Hermansson
*
*    DESCRIPTION:  Header file for accessfunctions provided by mmicl for
*                  other blocks.
*
*
****************************************************************************/

/****************************************************************************
*
*    REVISION HISTORY :
*
*    Rev   Date        Name      Measures
*    -----------------------------------------------------------------------
*    1.0  2011-08-22   Bo H      Created
*		  2011-09-28   Bo H		 New protocol for LK		
*     2012-03-28   Bo H      RESEND_TIME, MAX_NO_OF_TRANSMISSIONS adjusted  
*     2017-08-22   Bo H      Max package size increased 512->8500
*
****************************************************************************/

/****************************************************************************
* MACROS                                                                    *
****************************************************************************/

#define MMI_MAX_PACKAGE_SIZE 8787
#define MMI_HEADER_SIZE 5					/* MS,HT,MN,DL(MSB),DL(LSB) */
#define MMI_TRAILER_SIZE 4					/* 4-byte CRC */
#define MMI_DATA_MAX_SIZE (MMI_MAX_PACKAGE_SIZE - MMI_HEADER_SIZE - MMI_TRAILER_SIZE) 
#define MMI_MSG_MAX (MMI_DATA_MAX_SIZE - 1) /* First byte is MessageType */


#define RESEND_TIME               2000      /* Resend message if ack not received */
#define MAX_NO_OF_TRANSMISSIONS   3         /* Max no of transmissions */

#define MAX_MESSAGE_NO            255



#define MESSAGE_START_BYTE 0x7a /* The value of the MS byte */

 
#define MMICL_PERIOD 100
 
typedef unsigned short word;
typedef unsigned long  dword;
typedef signed char    sbyte;
typedef short          sword;
typedef long           sdword;

/* Macros to access words/dwords etc from byte-arrays, where the values are stored
 * with Motorola byte order. */
 
/* These definitions are adapted for an x86. The first macros are only good for reading. */
#define VBYTE(buf, p) (*(byte*)((byte*)(buf)+(p))+0) /* Add 0 so they can't be used as lvalues */
#define VSBYTE(buf, p) ((sbyte)VBYTE((buf), p))
#define VWORD(buf, p) (word)((((word)(((byte*)(buf))[p]) << 8) + ((byte*)buf)[(p)+1])+0)
#define VSWORD(buf, p) ((sword)VWORD(buf, p))
#define VWORD3(buf, p) (((((word)(((byte*)(buf))[p]) << 16) + ((word)(((byte*)(buf))[(p)+1]) << 8) + ((byte*)buf)[p+2])))
#define VSWORD3(buf, p) ((sdword)(((VBYTE(buf, p) & 0x80) ? 0xff000000 : 0) | (((((word)(((byte*)(buf))[p]) << 16) + ((word)(((byte*)(buf))[(p)+1]) << 8) + ((byte*)buf)[p+2])))))
#define VDWORD(buf, p) (dword)(((VWORD(buf,p) << 16) + VWORD(buf, p+2))+0)
#define VSDWORD(buf, p) ((sdword)VDWORD(buf, p))

#define VPUTBYTE(buf, p, v) (((byte*)(buf))[p] = (v))
#define VPUTSBYTE(buf, p, v) VPUTBYTE(buf, p, (sbyte)(v))
#define VPUTWORD(buf, p, v) do { VPUTBYTE(buf, p, (byte)((v) >> 8)); VPUTBYTE(buf, p+1, (byte)((v) & 0xff)); } while (false)
#define VPUTSWORD(buf, p, v) VPUTWORD(buf, p, (word)v)
#define VPUTDWORD(buf, p, v) do { VPUTWORD(buf, p, (word)((v) >> 16)); VPUTWORD(buf, p+2, (word)((v) & 0xffff)); } while (false)
#define VPUTWORD3(buf, p, v) do { VPUTWORD(buf, p, (word)((v) >> 8)); VPUTBYTE(buf, p+2, (byte)((v) & 0xff)); } while (false)
#define VPUTSWORD3(buf, p, v) VPUTWORD3(buf, p, (dword)(v))
#define VPUTSDWORD(buf, p, v) VPUTDWORD(buf, p, (dword)(v))


/**********************************************************
* Function:    MMMICL_getNextMsgFromATP
* Description:
**********************************************************/

void __declspec(dllexport) MMICL_getNextMsgDataFromATP(uint16 *dataLength, void *data);

/**********************************************************
* Function:     MMICL_getErrors
* Description:
**********************************************************/
void __declspec(dllexport) MMICL_getErrors(int *CurrentAckMsgErrCount, int *recErrCount);

/**********************************************************
* Function:    MMICL_putMessageDataToATP
*
* Description: The new protocol allows a 16-bit datalength and
* expects the MessageType in the first byte of the data
*
**********************************************************/
int __declspec(dllexport)MMICL_putMessageDataToATP(uint16 dataLength, void *data);

/**********************************************************
* Function:     mmicl_init
* Description:  Calls the init functions of all other modules
*               in mmicl
* Return Value: None
**********************************************************/
int __declspec(dllexport) MMICL_init(int port, int debug);


/**********************************************************
* Function:     mmicl_exit
* Description:  Calls the exit functions in mmicl
* Return Value: None
**********************************************************/
void __declspec(dllexport) MMICL_exit(void);


#endif
