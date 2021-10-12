/** @file 
 * Header file for Basic string and memory manipulation functions 
 * for various platforms
 * @note 
 * <b>18.01.2005 10:35</b>
 * Remark: Please take care by writing comments, code or just adding a space in a macro.
 * The strange Tasking compiler is counting the overall numbers of characters in the
 * macro body. If a maximum is exeeded, you will get an error message. Very nice!
 */
 /*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by: jkrupp %
 *                      %version: 27 %
 *                      %date_created: 2013-05-27 09:24 %
 **********************************************************************
 *
 *  Revision 27     2013/05/27      jkrupp
 *  add casts for function parameters of Current_Assert_Handler
 *
 *  Undocumented changes after switch from CVS to Synergy
 *
 *  $Log: fffis_string.h,v $
 *  Revision 1.19  2005/06/27 07:45:36  jkusmira
 *  Fixed Doxygen Comments 1 Stage
 *
 *  Revision 1.18  2005/05/20 14:22:46  mkilianj
 *  converting to doxygen style comments
 *  converting to doxygen style comments
 *
 *  Revision 1.17  2005/01/18 09:43:07  mjoost
 *  fixed the tasking related bug: no more room for macro body
 *
 *  Revision 1.16  2004/12/22 14:16:50  mjoost
 *  Due to LZB requirements I linted the file according to MISRA rules
 *
 *  Revision 1.15  2004/12/20 13:51:19  mjoost
 *  add lint exception 960, rule 54
 *
 *  Revision 1.14  2004/12/20 12:45:25  mjoost
 *  add the braces for else branch
 *
 *  Revision 1.13  2004/11/26 10:53:13  jdiezper
 *  Global Project Traversal for separating Simulator from SPL Library.
 *
 *  Revision 1.12  2004/10/21 11:38:22  jdiezper
 *  Export the my_allocated_memory function
 *
 *  Revision 1.11  2004/09/17 13:44:04  mjoost
 *  added some ifdef's VXWORKS in order to run the code on VCU-Lite
 *
 *  Revision 1.10  2004/09/07 13:28:30  jdiezper
 *  Add my_safe_copy and my_memcmp
 *
 *  Revision 1.9  2004/08/05 09:44:44  jdiezper
 *  Add a global prefix (sp) to all basic types (BYTE, INT...)
 *  Modify all variables and function prototypes accordingly.
 *
 *  Revision 1.8  2004/06/23 14:11:10  jdiezper
 *  Transform cplusplus comments in c comments for Tornado compatibility.
 *
 *  Revision 1.7  2004/04/14 10:19:01  jdiezper
 *  Add my_strcmp
 *
 *  Revision 1.6  2004/04/14 09:21:57  wroewe
 *  change the types from spBYTE to spUINT
 *
 *  Revision 1.5  2004/03/30 12:43:50  wroewe
 *  change spUINT my_strlen(const void *src) in spBYTE my_strlen(const void *src)
 *
 *  Revision 1.4  2004/03/30 08:53:25  jdiezper
 *  Add my_strlen and my_strcat functions.
 *
 *  Revision 1.3  2004/03/26 12:49:08  wroewe
 *  change my_memcopy(void *dest, const void *src, const spUINT Size)
 *  in my_memcopy(void *dest, const void *src, const spBYTE Size)
 *
 *  Revision 1.2  2004/03/17 14:27:27  wroewe
 *  - new code
 *
 *  Revision 1.1  2004/03/04 09:01:34  jdiezper
 *  Traces and standard library replacement
 *
 *
 **********************************************************************/
#ifndef FFFIS_STRING_H_
#define FFFIS_STRING_H_

#ifdef SPL_PLATFORM_LZB
#include "../include/typmisra.h"
#include "../ablauf/crc002.h"

/**
 * @todo Define  description.*/ 
#define MY_SAFE_COPY(the_dest, the_dest_size, the_src, the_src_size)  \
        func_my_safe_copy(((void*)(the_dest)),(spUINT16)(the_dest_size),((void*)(the_src)),(spUINT16)(the_src_size))
/**
 * @todo Define  description.*/ 
#define my_memset(dest,c,Size)  InitBytes(((void*)dest),(c),((spUINT16)(Size)))

#else /* SPL_PLATFORM_LZB */

/**
 * Bounded string length function.
 *        
 *       @param  src               String
 *
 *       @param  Size              Length of buffer to be search
 * 
 *       @return Result            Length of string. If no terminating '\0' is 
 *                                 found, Size is returned
 * 
 ******************************************************************************/
spUINT my_strnlen (const void *src, const spUINT Size);


/**
 * String n copy.
 *        
 *       @param  src               Source string
 * 
 *       @param  dest              Destination buffer
 * 
 *       @param  Size              Size of destination buffer
 *
 ******************************************************************************/
void   my_strncpy (void *dest, const void *src, const spUINT Size);


/**
 * String n copy that limits size of source buffer.  Buffer is truncated 
 * if the source size is greater or equal to the destination.  
 * \0 added to end of buffer.
 *        
 *       @param  src               Source string
 * 
 *       @param  dest              Destination buffer
 * 
 *       @param  srcSize           Size of destination buffer
 *
 *       @param  dstSize           Size of source buffer
 *
 ******************************************************************************/
spUINT my_safe_strncpy (void *dest, const void *src, const spUINT destSize, spUINT srcSize );

#ifdef SPL_OPTION_ENABLE_TRACES

/**
 * String concatenation
 *        
 *       @param  src               Source string
 * 
 *       @param  dest              Destination buffer
 *
 * @note This function is not permitted in vital applications.
 * 
 ******************************************************************************/
void   my_strcat (const void *dest, const void *src);

#endif /* SPL_OPTION_ENABLE_TRACES */

/**
 * String n compare.
 *        
 *       @param  a                 String a.
 * 
 *       @param  b                 String b.
 * 
 *       @param  Size              Maximum number of characters to compare
 *
 *       @return Result            =spTRUE when the same until length or Size <br>
 *                                 =spFALSE
 * 
 ******************************************************************************/
spBOOL my_strncmp (const spCHAR * a, const spCHAR * b, const spUINT Size);


/**
 * Memory set.
 *        
 *       @param  dest              Destination area.
 *       
 *       @param  c                 Constant to set.
 * 
 *       @param  Size              Size of area to be set.
 * 
 ******************************************************************************/
void   my_memset (void *dest, const spBYTE c, const spUINT Size);

/**
 * Memory compare.
 *        
 *       @param  dest              Buffer 1.
 * 
 *       @param  src               Buffer 2.
 * 
 *       @param  Size              Size of to be compared.
 * 
 *       @return Result            0 if dest and src are equal.
 * 
 ******************************************************************************/
spINT  my_memcmp (const void * dest, const void *src, const spUINT Size);
/**
 * Memory copy.
 *        
 *       @param  dest              Destination area.
 * 
 *       @param  src               Source area.
 * 
 *       @param  Size              Size of bytes to be copied.
 * 
 ******************************************************************************/
void   my_memcpy (      void * dest, const void *src, const spUINT Size);

/*lint -esym(960, 98) 98 Violates MISRA Rule 98, Multiple use of '#/##' operators in macro definition
 *                       we don't use TRACEs in LZB context  */

/**
 * MY_SAFE_COPY macro.
 * Memory copy only if size of destionation area is equal or greater then size of source area.
 ******************************************************************************/
#define MY_SAFE_COPY(the_dest, the_dest_size,the_src, the_src_size)       \
/*lint -e717 do...while (0) MISRA 2004 rule 19.4 */       \
do {   \
    /*lint --e{774} */     \
    /*lint --e{506} */     \
    /*lint -esym(960, 54) Null statement not in line by itself*/          \
    if ( (the_src_size) > (the_dest_size) )                                                     \
    {                                                                                           \
        TRACE ((CH_ERROR, "my_safe_copy overflow (%s:%d)\n",  __FILE__,  __LINE__));            \
        TRACE ((CH_ERROR, "   desination      = (%s)\n",     #the_dest));                       \
        TRACE ((CH_ERROR, "   desination size = %4d (%s)\n",  the_dest_size,  #the_dest_size)); \
        TRACE ((CH_ERROR, "   source          = (%s)\n",     #the_src));                        \
        TRACE ((CH_ERROR, "   source     size = %4d (%s)\n",  the_src_size,   #the_src_size));  \
        Current_Assert_Handler ((const spCHAR * const)__FILE__, __LINE__, (const spCHAR * const)"MY_SAFE_COPY Assert");                                                                     \
    }                                                                                           \
    else                                                                                        \
    {                                                                                           \
        my_memcpy (the_dest, the_src, the_src_size);                                            \
    }                                                                                           \
} while (0)     \
/*lint +e717 */
/*lint +esym(960, 98, 54) */

#endif /* !SPL_PLATFORM_LZB */

#endif /* FFFIS_String */
