/**********************************************************************
*
* Filename:    crc.h
* 
* Description: A header file describing the various CRC standards.
*
* Notes:       
*
* 
* Copyright (c) 2000 by Michael Barr.  This software is placed into
* the public domain and may be used for any purpose.  However, this
* notice must not be changed or removed and no warranty is either
* expressed or implied by its publication or distribution.
**********************************************************************/
/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
* 1.0     130222  wahmad   Reused
* 1.1     270222  wahmad   Few descriptions added
* 1.2	  130604  wahmad   Lint corrections have been made
* 1.3     140626  wahmad   Compilable on B-board
*******************************************************************************/
#ifndef _crc_h
#define _crc_h

#include "pre_crc.h"

#define FALSE 0
#define TRUE  !FALSE

/*
* Select the CRC standard from the list that follows.
*/
#define CRC32


#if defined(CRC_CCITT)

typedef US_SHORT  crc;

#define CRC_NAME      "CRC-CCITT"
#define POLYNOMIAL      0x1021
#define INITIAL_REMAINDER 0xFFFF
#define FINAL_XOR_VALUE   0x0000
#define REFLECT_DATA    FALSE
#define REFLECT_REMAINDER FALSE
#define CHECK_VALUE     0x29B1

#elif defined(CRC16)

typedef US_SHORT  crc;

#define CRC_NAME      "CRC-16"
#define POLYNOMIAL      0x8005
#define INITIAL_REMAINDER 0x0000
#define FINAL_XOR_VALUE   0x0000
#define REFLECT_DATA    TRUE
#define REFLECT_REMAINDER TRUE
#define CHECK_VALUE     0xBB3D

#elif defined(CRC32)

typedef US_LONG  crc;

#define CRC_NAME      "CRC-32"
#define POLYNOMIAL      0x4A503DF1 /*0x04C11DB7*/
#define INITIAL_REMAINDER 0
#define FINAL_XOR_VALUE   0
#define REFLECT_DATA    FALSE
#define REFLECT_REMAINDER FALSE
#define CHECK_VALUE     0xE0AEB3E7

#else

#error "One of CRC_CCITT, CRC16, or CRC32 must be #define'd."

#endif


void  crcInit(void);
/*crc   crcSlow(unsigned char const message[], int nBytes);*/
crc   crcFast(US_CHAR const message[], S_INT nBytes);


#endif /* _crc_h */
