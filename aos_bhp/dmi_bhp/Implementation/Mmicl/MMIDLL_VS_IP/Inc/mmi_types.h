#ifndef MMI_TYPES_H
#define MMI_TYPES_H
/**************************************************************************

 PRODUCT:       DMI / MMI / BHP
 DESCRIPTION:   MMI data types

 Modification history:

 Date         Revision Name         Comment
 --------------------------------------------------
 2017-08-22   1.0      bhermans     Created.
                               
***************************************************************************
 We reserve all rights in this file and in the information contained
 therein. Reproduction, use or disclosure to third parties without
 express authority is strictly forbidden.
 Copyright Bombardier Transportation AB, 2017
**************************************************************************/

typedef unsigned int   uint;
typedef unsigned long  ulong;

typedef signed char    int8;
typedef unsigned char  uint8;
#define byte uint8

typedef short int      int16;
typedef unsigned short int uint16;

typedef long int            int32;
typedef unsigned long int   uint32;

  /* bool is not defined in C , only in C++ */
typedef unsigned char bool;
enum { false = 0, true = 1 };


#endif


