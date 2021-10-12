/** 
 * @file 
 * <b>Provide type definitions (typedefs) for the most common used types.</b>
 * @warning 
 * Do not define NULL, but use 0U directly everywhere!
*/
 /*********************************************************************
 *                    Bombardier Transportation
 *
 *  Revision 1.17  2005/06/27 09:10:40  jkusmira
 *  Fixed Doxygen Comments 1 Stage
 *
 *  Revision 1.16  2005/05/20 14:22:44  mkilianj
 *  converting to doxygen style comments
 *  converting to doxygen style comments
 *
 *  Revision 1.15  2005/02/22 17:16:44  AGeck
 *      Replace define NULL by 0U in source file
 *      because of conflicts with NULL define in LZB and
 *      disapproved NULL define for C++
 *
 *  Revision 1.14  2004/12/22 14:16:49  mjoost
 *  Due to LZB requirements I linted the file according to MISRA rules
 *
 *  Revision 1.13  2004/12/21 13:38:27  mjoost
 *  linted according to misra
 *
 *  Revision 1.12  2004/12/21 10:29:27  mjoost
 *  changed typedef of TIME to spUINT32
 *
 *  Revision 1.11  2004/12/15 14:56:34  mczaprag
 *  temporary back to: char spCHAR
 *
 *  Revision 1.10  2004/12/13 17:07:05  mczaprag
 *  LZB adaptation: spCHAR -> unsigned char
 *
 *  Revision 1.9  2004/08/27 10:22:49  jdiezper
 *  Minor changes when moving from Borland to GNU comipler
 *
 *  Revision 1.8  2004/08/05 09:44:44  jdiezper
 *  Add a global prefix (sp) to all basic types (BYTE, INT...)
 *  Modify all variables and function prototypes accordingly.
 *
 *  Revision 1.7  2004/06/29 15:24:17  mjoost
 *  made compatible to Tornado environment
 *
 *  Revision 1.6  2004/06/23 14:11:10  jdiezper
 *  Transform cplusplus comments in c comments for Tornado compatibility.
 *
 *  Revision 1.5  2004/06/23 13:05:17  jdiezper
 *  Add FFFIS_Clock
 *
 *  Revision 1.4  2004/06/08 14:46:33  mjoost
 *  Fixes for IFAK
 *
 *  Revision 1.3  2004/03/04 08:50:58  jdiezper
 *  options.lnt
 *
 *  Revision 1.2  2004/02/26 14:04:29  jdiezper
 *  Update top header. Use valid CVS keywords for CVS meta-data expansion.
 *  Update top header. Use valid CVS keywords for CVS meta-data expansion.
 *
 *
 **********************************************************************/
#ifndef FFFIS_TYPES_H_
#define FFFIS_TYPES_H_

/*************************************************************************************
**************************************************************************************
DO NOT ADD ANY INCLUDES OR IFDEFS TO THAT FILE TO KEEP APPLICATION INTERFACE SIMPLE!!!
*************************************************************************************
**************************************************************************************/

 /*@{*/
/**
 * Boolean variables implemented as enumeration.
 * spFALSE must be 0, spTRUE != 0.*/ 
typedef enum { spFALSE, spTRUE } spBOOL;
/**
 * Not computional, unsigned char.*/
typedef unsigned char         spBYTE;
/**
 * Natural size integer. 
 first cover the case we're compiling for the LZB Platform  but with the MinGw 
 Compiler. This configuration is used for testing purposes */
#if defined(SPL_PLATFORM_LZB) && defined(__GNUC__)

/**
 * Unsigned integer. Native size of the host.*/
typedef   unsigned  short int  spUINT;

#else

/**
 * Unsigned integer. Native size of the host.*/
typedef unsigned  int         spUINT;
#endif
/**
 * Computational, signed integer.*/
typedef   signed  char        spINT8;
/**
 * Computational, unsigned integer.*/
typedef unsigned  char       spUINT8;
/**
 * Computational, signed integer.*/
typedef   signed  short int  spINT16;
/**
 * Computational, unsigned integer.*/
typedef unsigned  short int spUINT16;
/**
 * Computational, signed integer.*/
typedef   signed  long  int  spINT32;
/**
 * Computational, unsigned integer.*/
typedef unsigned  long  int spUINT32;

/**
 * Not computational, unsigned long.*/
typedef           spUINT32   spDWORD;
/**
 * Not computational, unsigned short.*/
typedef           spUINT16    spWORD;
/**
 * Computational, used to represent times.*/
typedef           spUINT32      TIME;
typedef           spUINT32    spTIME; /* consistant naming */
/**
 * Computational, signed char.*/
typedef   unsigned  char      spCHAR;
/**
 * Natural size integer. */
#if defined(SPL_PLATFORM_LZB) && defined(__GNUC__)

/**
 * Signed integer, native size of the host.*/
typedef   signed  short        spINT;
#else
/**
 * Signed integer, native size of the host.*/
typedef   signed  int          spINT;
#endif

#ifndef SPL_PLATFORM_LZB

/**
 * Computational, floating point.*/
typedef           float      spFLOAT;
/**
 * Computational, floating point.*/
typedef           double    spDOUBLE;

#endif

/*@}*/


#endif /* FFFIS_TYPES_H_*/
