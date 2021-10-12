#ifndef ATPDEFS_H
#define ATPDEFS_H

/**********************/
/* Formerly StdAtp. h */
/**********************/

/****************************************************************************
*           (C) COPYRIGHT Adtranz AB, SWEDEN 1996.
*           ======================================
*
*    The copyright to the computer program herein is the
*    property of Adtranz AB, Sweden. All rights reserved.
*    The program may be used and/or copied only with the
*    written permission from Adtranz AB, or in accordance
*    with the terms and conditions stipulated in the
*    agreement/contract under which the program has been
*    supplied.
*
*    MODULE NAME :   stdatp.h
*
*    CREATION INFORMATION :
*           Programmer : Stefan Sjögren
*           Department : RPA
*                 Date : <1996-09-02>

*    DESCRIPTION: Definition of ATP standard types and some macros that
*                 expand differently depending on target platform.
*
****************************************************************************/

/****************************************************************************
*
*    REVISION HISTORY :
*
*    Rev   Date        Name      Measures
*    -----------------------------------------------------------------------
*    1.0   1996-09-02   STS       Created
*    1.1   1997-08-08   SIGKUR    Updated according to PUSATP158
*    1.2   1997-08-08   SIGKUR    ifndef GLUEATP changed to BS_TYPES_H
*    1.3   1997-08-08   SIGKUR    ifndef BS_TYPES_H changed to byte
*    1.4   1997-08-08   SIGKUR    if byte defined, undefine byte and typedef byte
*    1.5   1997-08-11   SIGKUR    byte removed, defined in bs_types.h
*          1997-11-12   KONAKE    Added the CompilerAssert()-macro
*    1.6   2003-04-28   konedlu   Renamed this file from stdatp.h to atpdefs.h, 
*                                 since this version of stdatp.h is only used 
*                                 by the radio communication dll used with 
*                                 the stationary simulator.
*
****************************************************************************/

/****************************************************************************
* INCLUDE FILES                                                             *
****************************************************************************/
#ifdef WIN32
  #undef ERROR /* ERROR is defined in Windows as 0 but as -1 in Basic System */
#endif
#ifndef OK
  #define OK      0
#endif
#ifndef ERROR
  #define ERROR   (-1)
#endif

/* min- and max macros are defined in different header files depending on
 * compiler.
 * We must support
 *  Tornado:      vxWorks.h
 *  Visual C++:   minmax.h
 *  Borland:      stdlib.h
 */
#if defined(__vxworks)
 #include <vxWorks.h>
#elif defined(__BORLANDC__)
 #include <stdlib.h>
#else
 #include <minmax.h>
#endif

#include <stdio.h>
/* For defining integer type limits */
#include <limits.h>
/* bs_types.h must be included after vxWorks.h! */
#include "bs_types.h"
/*
#include <typea.h>
#include <typeb.h>
*/

/* === The serial ports to use ============================================== */
#if defined(VCU_VERSION)
  #define SER_T44IH   BS_IO_COM4  /* /tyCo/3 was BS_IO_COM1 /tyCo/0 */
  #define SER_RCOND   BS_IO_COM3  /* /tyCo/2 was BS_IO_COM4 /tyCo/3 */
  #define SER_MONITOR BS_IO_COM2  /* /tyCo/1 */
#elif defined(COMC_VERSION)
  #define SER_T44IH   BS_IO_COM4
  #define SER_RCOND   BS_IO_COM6
  #define SER_MONITOR BS_IO_COM5
#else /*defined (PC_VERSION)*/
  /* These are only default ports, they can be changed from the command line  */
  #define SER_T44IH  BS_IO_COM1
  #define SER_RCOND  BS_IO_COM2
  #define SER_MONITOR 
#endif

#ifdef SIMULATE_BS_IO
  #define bs_readPort(sinkPd, aData, bData, token) bs_readPortSIM(sinkPd, aData, bData, token)
  #define bs_writePort(sourcePd, aData, bData) bs_writePortSIM(sourcePd, aData, bData)
#endif

#ifndef WIN32
  int printfSIM(const char *fmt, ...);
  int fprintfSIM(FILE *f, const char *fmt, ...);
  #define printf printfSIM
  #define fprintf fprintfSIM
#endif

/****************************************************************************
* MACROS                                                                    *
****************************************************************************/
/* Packed structures. For GNUC, let each structure member that may be mis-aligned
 * be followed by PACKED before the ';'.
 * The '#pragma' below will make all structures packed in MSVC. To avoid problems,
 * let <stdatp> be the first include file to include (and include it first in
 * all other ATP-headers) and include no standard headers (include Basic System)
 * after it.
 * According to the DJGPP FAQ, in GCC v2.7.2 '__attribute__ ((packed))' has no effect
 * but '#pragma pack(1)' will! (Also, in GCC 2.7, '__attribute__ ((packed))' can
 * be placed at the end of a struct declaration to make the whole structure packed,
 * but that is not the case for v2.6 which is used in Tornada 1.00.)
 * In MSVC, structures can have an odd length, but in GCC v2.6, the size will always
 * be even.
 *
 * Reorder all structures if possible so that shorts or bigger members will be evenly
 * aligned as much as possible, for others, use PACKED. If the length is odd, add
 * 'char __filler;' at the end. This should also be done for structures that is
 * left unacked to make it clear what size they will get (and as the '#pragma pack(1)'
 * for MSVC and possibly GCC v2.7 otherwise would make them packed anyway).
 * Also, calculate what size the structure should get and put a CompilerAssert()
 * after the declaration to verify it.
 *
 * Note that the size of enums (including boolA/B) is 4!
 */
/*
 * We do not pack any structures for the time being. The default for gcc 2.6 seems
 * to align values on even addresses, but this knowledge should not be used in the
 * code as it may change with different compiler versions.
 *
#ifdef __GNUC__
  #define PACKED __attribute__ ((packed))
#else
  #define PACKED
#endif
#pragma pack(1)  // For MSVC.
*/

/* Macros to access words/dwords etc from byte-arrays, where the values are stored
 * with Motorla byte order. */
/*#if CPU_FAMILY==MC680X0*/
#ifdef __vxworks
  /* Motorola version. Can, except VWORD3, also act as lvalues...
  */
  #define VBYTE(buf, p) *(byte*)((byte*)(buf)+(p))
  #define VSBYTE(buf, p) ((sbyte)VBYTE((buf), p))
  #define VWORD(buf, p) *(word*)((byte*)(buf)+(p))
  #define VSWORD(buf, p) *(sword*)((byte*)(buf)+(p))
  #define VWORD3(buf, p) (VDWORD(buf, p) >> 8)
  #define VSWORD3(buf, p) (sdword)((VDWORD(buf, p) >> 8) | (VBYTE(buf, p) & 0x80 ? 0xff000000 : 0))
  #define VDWORD(buf, p) *(dword*)((byte*)(buf)+(p))
  #define VSDWORD(buf, p) *(sdword*)((byte*)(buf)+(p))

  /* ... but for compatibility reasons, we should use these macros */
  #define VPUTBYTE(buf, p, v) (VBYTE(buf, p) = (v))
  #define VPUTSBYTE(buf, p, v) (VSBYTE(buf, p) = (v))
  #define VPUTWORD(buf, p, v) (VWORD(buf, p) = (v))
  #define VPUTSWORD(buf, p, v) (VSWORD(buf, p) = (v))
  #define VPUTWORD3(buf, p, v) do { VPUTWORD(buf, p, (word)((v) >> 8)); VPUTBYTE(buf, p+2, (byte)((v) & 0xff)); } while (false)
  #define VPUTSWORD3(buf, p, v) VPUTWORD3(buf, p, (dword)(v))
  #define VPUTDWORD(buf, p, v) (VDWORD(buf, p) = (v))
  #define VPUTSDWORD(buf, p, v) (VSDWORD(buf, p) = (v))
#else
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
  #define VPUTWORD(buf, p, v) do { VPUTBYTE(buf, p, (byte)((v) >> 8)); VPUTBYTE(buf, p+1, (byte)((v) & 0xff)); } while (0)
  #define VPUTSWORD(buf, p, v) VPUTWORD(buf, p, (word)v)
  #define VPUTDWORD(buf, p, v) do { VPUTWORD(buf, p, (word)((v) >> 16)); VPUTWORD(buf, p+2, (word)((v) & 0xffff)); } while (0)
  #define VPUTWORD3(buf, p, v) do { VPUTWORD(buf, p, (word)((v) >> 8)); VPUTBYTE(buf, p+2, (byte)((v) & 0xff)); } while (false)
  #define VPUTSWORD3(buf, p, v) VPUTWORD3(buf, p, (dword)(v))
  #define VPUTSDWORD(buf, p, v) VPUTDWORD(buf, p, (dword)(v))
#endif




/* Limit values for integer types */
#define BYTE_MAX  UCHAR_MAX
#define BYTE_MIN  0
#define WORD_MAX  USHRT_MAX
#define WORD_MIN  0
#define DWORD_MAX ULONG_MAX
#define DWORD_MIN 0

#define SBYTE_MAX  SCHAR_MAX
#define SBYTE_MIN  SCHAR_MIN
#define SWORD_MAX  SHRT_MAX
#define SWORD_MIN  SHRT_MIN
#define SDWORD_MAX LONG_MAX

/* LONG_MIN is defined as (-2147483648) in Tornado; this upsets lint for some reason
 * (but there are no problems with SHORT_MIN or CHAR_MIN).
 * Warning 648: Overflow in computing constant for operation: 'negation'
 * Warning 501: Expected signed type
 * Warning 569: Loss of information (assignment) (32 bits to 31 bits) */    
#define SDWORD_MIN /*lint -save -e648 -e501 -e569 */ (long)LONG_MIN /*lint -restore */

/* Standard macro defintions follows here */
#ifndef NULL
 #define NULL 0
#endif

/* Given the following global var. decl.
 * EXTERN CONST  word  XXTBL_blaBlaA INIT(42);
 * Expands to const __far word XXTBL_blaBlaA = (10); if ALLOCMEMORY is defined
 * Expands to extern const __far word XXTBL_blaBlaA; if ALLOCMEMORY is !defined
 */

/* We use a Lint option to turn off the warning 723 about 'Suspicious use of =' */

#if defined(ALLOCMEMORY) || defined(ALLOCMEMORY_B) || defined(ALLOCMEMORY_A)
 #define INIT(x) /*lint -e723 */ = /*lint -restore */ (x)
 #define EXTERN
#else
 #define INIT(x)
 #define EXTERN   extern
#endif

#define CONST const


/* A compile time assert. Use it e.g. as
*    CompilerAssert(VCOM_PERIOD <= MISC_PERIOD);
* if some code relies on the fact that VCOM runs at least once for each time MISC runs.
* Should the period lengths change, a warning will be printed during compilation
* (although it will complain on that the length of the vector is 0, the real
* warning will not be shown).
* Any constant C-expression can be used; an '#if...#endif' is much more restricted.
*
* Warning 506: Constant value Boolean
* Info 762: Redundantly declared ... previously declared at line ...
* Info 752: local declarator '_Dummy' (line ..., file ...) not referenced
* Info 757: global declarator '_Dummy' (line ..., file ...) not referenced
*/
#define CompilerAssert(e)     \
  /*lint -save -e506 -e762 -esym(752,_Dummy) -esym(757,_Dummy) */ \
  extern char _Dummy[(e)?1:0]  /*lint -restore */


/****************************************************************************
* TYPES                                                                     *
****************************************************************************/
/* Unsigned integer types */


/*byte removed , defined in bs_types.h instead, SIGKUR 970811*/
/*
#ifndef byte
typedef unsigned char       byte;
#else
#undef byte
typedef unsigned char       byte;
#endif
*/
typedef unsigned short      word;
typedef unsigned long       dword;

/* Signed integer types */
typedef signed char         sbyte;
typedef short               sword;
typedef long                sdword;

/* Warning: Non-standard definition of true - does not conform with
 * ANSI/ISO C definition for boolean expressions. Inherited from old
 * definitions in VR.
 */

typedef enum
{
  zerobit = 0,
  onebit = 1
} bitvalue;

/****************************************************************************
* FUNCTION PROTOTYPES                                                       *
****************************************************************************/

#endif
