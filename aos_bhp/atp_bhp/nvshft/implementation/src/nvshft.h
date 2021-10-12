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
* %name: nvshFT.h %
* %created_by: nsyed %
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
*
* 4.1.1    160307  gberglun Version for P8B updated to conf. spec 1.5
*                           5 COD parameters added in MBT and COD parameters added to cfg.
* 4        151125  gberglun Version for P8B configuration
* 2        141124  dthorell Version for P8A configuration
*
* 1.0     130222  wahmad   Initially created
* 1.1     130225  wahmad   Constants converted to MACROS
* 1.2     130227  wahmad   Few Descriptions added
* 1.3     130314  wahmad   NVSHFT version1.1 according to parameters updated 1.2
*                          of CFPARAM and 1.4 of IFSATPCU document
* 1.4     130603  wahmad   Changes due to new requirements added for NVSHFT such as
*                          interface_version and read_only_data.
* 1.5     130604  wahmad   Lint corrections have been made
* 1.6     140305  anlindel WP020, arn_043#989, arn_043#990: updated to conform with
*                          CFPARAM 1.5 and IFSATPCU 1.9.
* 1.8     140626  wahmad   Compilable on B-board
* 1.10    141002  wahmad   P7B (WP_045+WP_042+45Days Stuff): NOP_CFG is now
*                          incremented 14 (Total 366)
* 1.11    170127  nsyed    Adapted to BHP Project (Ver 1.0)
*******************************************************************************/


#ifndef __NVSHFT__
#define __NVSHFT__

#include "pre_crc.h"

#define NVSHFT_MAJOR 1
#define NVSHFT_MINOR 0
#define CFPARAM_MAJOR 1
#define CFPARAM_MINOR 0

#define INTERFACE_VERSION 1

#define NOP_COMMON 136
#define NOP_MNT 6
#define NOP_RT  6
#define NOP_INSTANCE 2
#define NOP_CFG_DISP 51
#define NOP_TYPE 244


extern S_INT parameterSize_TYPE[NOP_TYPE];
extern S_INT parameterSize_INSTANCE[NOP_INSTANCE];
extern S_INT parameterSize_MNT[NOP_MNT];
extern S_INT parameterSize_RT[NOP_RT];
extern S_INT parameterSize_COMMON[NOP_COMMON];
extern S_INT parameterSize_CFG_DISP[NOP_CFG_DISP];

extern US_CHAR* parameterName_TYPE[NOP_TYPE];
extern US_CHAR* parameterName_INSTANCE[NOP_INSTANCE];
extern US_CHAR* parameterName_COMMON[NOP_COMMON];
extern US_CHAR* parameterName_MNT[NOP_MNT];
extern US_CHAR* parameterName_RT[NOP_RT];
extern US_CHAR* parameterName_CFG_DISP[NOP_CFG_DISP];

typedef struct COMMON_PARAMS {
    S_INT parameterSize_COMMON;
    US_CHAR parameterName_COMMON[50];
} COMMON_PARAMS;

extern struct COMMON_PARAMS common[NOP_COMMON];

typedef struct INSTANCE_PARAMS
{
    S_INT parameterSize_INSTANCE;
    US_CHAR parameterName_INSTANCE[50];
} INSTANCE_PARAMS;

extern struct INSTANCE_PARAMS instance[NOP_INSTANCE];

typedef struct TYPE_PARAMS
{
    S_INT parameterSize_TYPE;
    US_CHAR parameterName_TYPE[50];
} TYPE_PARAMS;

extern struct TYPE_PARAMS typespec[NOP_TYPE];

typedef struct MNT_PARAMS {
    S_INT parameterSize_MNT;
    US_CHAR parameterName_MNT[30];
} MNT_PARAMS;

extern struct MNT_PARAMS mnt[NOP_MNT];

typedef struct RT_PARAMS {
    S_INT parameterSize_RT;
    US_CHAR parameterName_RT[30];
} RT_PARAMS;

extern struct RT_PARAMS rt[NOP_RT];

typedef struct CFG_DISP_PARAMS {
    S_INT parameterSize_CFG_DISP;
    US_CHAR parameterName_CFG_DISP[50];
} CFG_DISP_PARAMS;

extern struct CFG_DISP_PARAMS cfg_disp[NOP_CFG_DISP];

void showVersion();
S_INT showHelp();

S_INT writeNVSHFile(US_CHAR * dataFileBuffer, US_INT fileLength, S_INT option);
S_INT writeASCIIFile(US_CHAR * dataFileBuffer, US_INT fileLength, S_INT option);

US_CHAR* readASCIIFile(S_INT option, US_INT *length);
US_CHAR* readNVSHFile(S_INT option, US_INT *length);

US_INT calcSizeOfPayload(S_INT option);
#ifndef WIN32
void itoa(S_INT value, S_CHAR* str, S_INT base);
#endif
#endif
