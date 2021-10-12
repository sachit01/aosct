#ifndef BS_TYPES_H
#define BS_TYPES_H
/**************************************************************************
 %name:         bs_types.h %
 %created_by:   akushwah %
 %version:      1 %
 %date_created: 2016-06-28 16:09 %
 PRODUCT:       Basic System, 3NSS 000250 S9013 
 DESCRIPTION:   Basic System types.

 Modification history:

 Date     Revision Name       Comment
 ------------------------------------
 980225   1.10     konjgu     Added condition for defining bool. Not defined when 
                              using Visual C++ 5.0 or higher.

 970722   1.9      konerl     Updated product number.

 970714   1.8      konerl     Changed definition of boolA/boolB (SIGBL21).
                               Changed definition of byte from a typedef to a 
                              #define to keep compiler satisfied when compiling
                              SDL code.

 970422   1.7      konerl     Removed Borland-specific symbols, BOOLA_DEFINED
                              and BOOLB_DEFINED.

 970401   1.6      konerl     Added disabling of warning 4710 for Visual C++.

 970319   1.5      sigrem     FileType added.
 
 970227   1.4.0    konerl     Updated max length of failure msg.
                              Updated handling of type bool to facilitate
                              PC-Lint type checking.

 970221   1.3.0    konerl     Updated definitions of debug level macros.

 970219   1.2.0    konerl     Added debug level macros. For Visual C++, 
                              added disabling of warning 4121.

 970212   1.1.0    konerl     Added ABSTRACT macro. For Visual C++, added
                              disabling of warning 4097. Added the type
                              FailureCode.

 970122   1.0.0    sigrem     Created.
                               
***************************************************************************
 We reserve all rights in this file and in the information contained
 therein. Reproduction, use or disclosure to third parties without
 express authority is strictly forbidden.
 Copyright ABB Daimler-Benz Transportation Signal AB, 1996
**************************************************************************/

/**************************************************************************
*
* Macros
*
**************************************************************************/

/* GCC (v2.6-95q2) can not handle pure virtual function declarations */
/* Fail if the body is executed */
#ifdef __GNUG__
#include <assert.h>
#define ABSTRACT { /*lint -save -e792 -e506 */assert(0);/*lint -restore */}
#else
#define ABSTRACT = 0;
#endif

/* Debug level macros */
/* Usage:
 * 
 * #if NORMAL_DEBUG
 *  // Debug code here, included for NORMAL_DEBUG and VERBOSE_DEBUG 
 * #endif
 *
 * #if TERSE_DEBUG
 * // Other debug code here, included for all debug levels
 * #endif
 *
 * #if (TERSE_DEBUG && !NORMAL_DEBUG)
 * // Debug code here, included only with TERSE_DEBUG
 * #endif
 *
 * Set DEBUG to 0 for terse debugging, to 1 for normal debugging and to
 * 2 (or above) for verbose debugging.
 */
#ifdef DEBUG
#define TERSE_DEBUG   (DEBUG >= 0)
#define NORMAL_DEBUG  (DEBUG >= 1)
#define VERBOSE_DEBUG (DEBUG >= 2)
#else
#define TERSE_DEBUG   0
#define NORMAL_DEBUG  0
#define VERBOSE_DEBUG 0
#endif

/**************************************************************************
*
* Constants
*
**************************************************************************/


/* Failure message constant */
enum 
{ 
  maxFailureMsgLength = 53, /* 52 characters + 0-byte */
  maxFailureCodeLength = 9  /*  8 characters + 0-byte */
};

/**************************************************************************
*
* Type and class definitions
*
**************************************************************************/

/* Check if compiler supports bool type */

/* bool defined in G++ and Visual C++ 5.0 or higher */
#if !defined(__GNUG__) && !(_MSC_VER>=1100)

#if defined(_MSC_VER) || defined(_lint)
/* Disable Microsoft Visual C++ warnings about bool types. */
#pragma warning( disable : 4237 )
/* Disable the following Microsoft Visual C++ warnings (level 4):      *
 *  C4514: unreferenced inline/local function has been removed         *
 *  C4511: 'class' : copy constructor could not be generated           *
 *  C4512: 'class' : assignment operator could not be generated        *
 *  C4201: nonstandard extension used : nameless struct/union          *
 *  C4097: typedef-name 'name' used as synonym for class-name 'class'  *
 *  C4121: 'struct': alignment of a member was sensitive to packing    *
 *  C4710: 'function': function not expanded                           *
 */
#pragma warning( disable : 4514 4511 4512 4201 4097 4121 4710)
#endif

typedef int bool;
/* Specific definition of false and true for PC-Lint typechecking */
#ifdef _lint
#define false ((bool)0)
#define true ((bool)1)
#else
enum { false = 0, true = 1 };
#endif
#endif


/* Common type definitions... */

/* ...for CPU32 systems with GCC, or WIN32 systems */
#if (defined(__GNUC__) && (CPU==CPU32)) || defined(WIN32)

typedef unsigned int   uint;
typedef unsigned long  ulong;

typedef signed char    int8;
typedef unsigned char  uint8;

typedef short          int16;
typedef unsigned short uint16;

/* int is always 32 bits with the defined systems above */
typedef int            int32;
typedef unsigned int   uint32;
#endif


/* Use a define for the type byte to keep SDL code happy 
 * (or at least not unhappy...) 
 */
#define byte uint8

/* Time type (milliseconds) used in the platform. */
typedef uint32         PlatformTime;

/* Failure message types */
typedef char FailureMsgType[maxFailureMsgLength];
typedef char FailureCode[maxFailureCodeLength];


/* boolA and boolB moved here from VSS */
typedef enum boolA
{
  falseA = 0, trueA = 1
} boolA;


typedef enum boolB 
{ 
  falseB = ~0, trueB = ~1 
} boolB;


typedef struct SinkPortDescr
{
  void* agent;  
} SinkPortDescr;

typedef struct SourcePortDescr
{
  void* agent;  
} SourcePortDescr;


/* FileType defiend as OS_IO_FILE in CSS */
#define FileType uint32

#endif


