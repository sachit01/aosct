/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2002
*
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without express authority is strictly forbidden.
*
* Module name: 
*
* %name: preCRC.h %
* %version: 1 %
* %created_by: nsyed %
* %date_created: 2017-01-31 16:11 %
* %Creation date of original object: Fri Nov 17 13:56:51 2000 %
*
* Description:
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
* 1.0     130222  wahmad   Initially created
* 1.1     250222  wahmad   initParameters() is added since Struct is used now 
*						   instead of global arrays for NVSHFT parameters
* 1.2     270222  wahmad   Few Descriptions added
* 1.3	  130604  wahmad   Lint corrections have been made
* 1.4     140626  wahmad   Compilable on B-board
* 1.5     170127  nsyed    Adapted to BHP Project (Ver 1.0)
*******************************************************************************/

#ifndef __PRE_CRC__
#define __PRE_CRC__


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
typedef __int8            int8_t;
typedef __int16           int16_t;
typedef __int32           int32_t;
typedef __int64           int64_t;
typedef unsigned __int8   uint8_t;
typedef unsigned __int16  uint16_t;
typedef unsigned __int32  uint32_t;
typedef unsigned __int64  uint64_t;
#else
#include <inttypes.h>
#endif

typedef unsigned char  US_CHAR;
typedef char           S_CHAR;
typedef unsigned short US_SHORT;
typedef short		   S_SHORT;
typedef unsigned int   US_INT;
typedef int			   S_INT;
typedef unsigned long  US_LONG;
typedef long		   S_LONG;


#define COMMON 1
#define MAINTENANCE   2
#define RUNTIME       3
#define DISPATCHER    4
#define INSTANCE 5
#define TYPE 6

#define COMMON_NAME "cfg"
#define MAINTENANCE_NAME "mnt"
#define RUNTIME_NAME "rt"
#define DISPATCHER_NAME "dispatcher"
#define INSTANCE_NAME "instance"
#define TYPE_NAME "type"

US_CHAR* removeComments(const US_CHAR* str, US_INT len);

US_CHAR* removeSpacesCarriages(const US_CHAR* str, US_INT len);

US_CHAR* removeSpacesCarriages1(const US_CHAR* str, US_INT len);

/* char* preCRC(const char* str, S_INT len); */
US_CHAR* preCRC(const US_CHAR* str, S_INT len, US_INT noOfParams, US_CHAR *fileVersion, US_INT *crcInFile);

S_INT checkCRCVersionASCII(const US_CHAR* str, S_INT len, S_INT option);

/* S_INT checkCRCVersionNVSH(const US_CHAR* str, S_INT len, S_INT option); */
S_INT checkCRCVersionNVSH(US_CHAR* str, S_INT len, S_INT option);


uint64_t calcCrc64(US_CHAR *buffer, S_INT len);

void initParameters();

US_INT nvshft_strlen(US_CHAR *p);
#endif
