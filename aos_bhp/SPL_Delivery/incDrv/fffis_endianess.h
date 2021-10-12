/** @file 
 *     Provide macros for converting LITTLE endian to BIG ENDIAN.
 *
 * Consider the 32 bits integer value 0x04030201.
 * The Most significant byte of this multi-byte variable is 0x04
 *
 * BIG ENDIAN processors the most significant byte is stored
 *    at the lowest address. On these architectures
 *    multi-bytes values are stored in memory in the same
 *    order as we (human beeings) write them.
 *
 * LITTLE ENDIAN processors the most significant byte is stored
 *    at the highest address.
 *<table>
 *   <tr>
 *       <td><br></td>
 *       <td>BIG ENDIAN<br></td>
 *       <td>LITTLE ENDIAN<br></td>
 *   </tr>
 *   <tr>
 *       <td>Lowest address<br></td>
 *       <td>0x04<br></td>
 *       <td>0x01<br></td>
 *   </tr>
 *   <tr>
 *       <td><br></td>
 *       <td><br>0x03<br>0x02<br></td>
 *       <td><br>0x02<br>0x03<br></td>
 *   </tr>
 *   <tr>
 *       <td>Highest address<br></td>
 *       <td>0x01<br></td>
 *       <td>0x04<br></td>
 *   </tr>
 *   <tr>
 *       <td>Examples of<br>processors<br></td>
 *       <td>Motorola 68000<br>Motorola 88000<br>IBM 360/370<br>Sun SPARC<br>ARM, PEP6xx<br></td>
 *       <td>Intel x86 family<br>AMD x86 family<br>DEC architectures<br>(PDP-11, VAX, Alpha)<br></td>
 *   </tr>
 * </table>
 * 
*/
 /*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by: mczaprag %
 *                      %version: 11 %
 *                      %date_created: Fri Dec 09 12:58:21 2005 %
 **********************cvs out of date********************************* 
 *                      $Author: jkusmira $
 *                      $Revision: 1.26 $
 *                      $Date: 2005/06/27 05:44:35 $
 *                      $Source: P://mpg/sl/com/fffis_endianess.h,v $
 *
 *  $Log: fffis_endianess.h,v $
 *  Revision 1.26  2005/06/27 05:44:35  jkusmira
 *  Fixed Doxygen Comments 1 Stage
 *
 *  Revision 1.25  2005/05/20 14:22:45  mkilianj
 *  converting to doxygen style comments
 *  converting to doxygen style comments
 *
 *  Revision 1.24  2005/04/11 07:23:35  mczaprag
 *  fixed bug with functions for BIG ENDIAN
 *
 *  Revision 1.23  2005/04/08 19:59:20  alamb
 *  Additional functions for little endianness on Hilscher - not pretty
 *  These should be removed and the existing functions used
 *
 *  Revision 1.22  2005/04/06 13:33:20  mczaprag
 *  MISRA RULES: fixed bugs with unsigned casts, but there is a still small bug
 *
 *  Revision 1.21  2005/02/23 13:52:27  alamb
 *  Additions to support Hilscher on Windows
 *
 *  Revision 1.20  2005/02/22 16:14:59  mjoost
 *  bug fixes
 *
 *  Revision 1.19  2005/01/27 17:16:30  alamb
 *  Updated functions for Hilscher
 *
 *  Revision 1.18  2005/01/26 08:41:54  rsmilgin
 *  syntax error repaired
 *
 *  Revision 1.17  2005/01/20 23:56:42  alamb
 *  Modifications for link
 *
 *  Revision 1.16  2005/01/20 18:33:35  rsmilgin
 *  *** empty log message ***
 *
 *  Revision 1.14  2005/01/18 18:25:14  alamb
 *  Addition of endian switching macros and functions for Hilscher driver
 *
 *  Revision 1.13  2005/01/18 11:09:57  alamb
 *  Addition of #ifdef so that additional functionas are only
 *  for Hilscher baords i.e. not LZB.
 *
 *  Revision 1.12  2005/01/18 09:39:31  alamb
 *  Addition of functions for switching between Endians
 *  These still should be renamed
 *
 *  Revision 1.11  2004/12/22 14:16:49  mjoost
 *  Due to LZB requirements I linted the file according to MISRA rules
 *
 *  Revision 1.10  2004/12/15 09:30:14  mczaprag
 *  LZB adaptation: fixed mini bug (for LINT MISRA)
 *
 *  Revision 1.9  2004/12/10 12:50:33  mczaprag
 *  LZB adaptation: avoid shift count out of range
 *
 *  Revision 1.8  2004/12/02 12:17:48  jdiezper
 *  Implement new endianness conversion macros and functions
 *
 *  Revision 1.7  2004/11/26 10:53:10  jdiezper
 *  Global Project Traversal for separating Simulator from SPL Library.
 *
 *  Revision 1.6  2004/08/27 10:22:50  jdiezper
 *  Minor changes when moving from Borland to GNU comipler
 *
 *  Revision 1.5  2004/08/05 09:44:44  jdiezper
 *  Add a global prefix (sp) to all basic types (BYTE, INT...)
 *  Modify all variables and function prototypes accordingly.
 *
 *  Revision 1.4  2004/06/23 14:11:10  jdiezper
 *  Transform cplusplus comments in c comments for Tornado compatibility.
 *
 *  Revision 1.3  2004/04/07 07:06:05  wroewe
 *  insert cast to spUINT32 or spDWORD
 *
 *  Revision 1.2  2004/02/26 14:04:29  jdiezper
 *  Update top header. Use valid CVS keywords for CVS meta-data expansion.
 *  Update top header. Use valid CVS keywords for CVS meta-data expansion.
 *
 *
 **********************************************************************/
#ifndef FFFIS_ENDIANESS_H_
#define FFFIS_ENDIANESS_H_

#ifdef SPL_PLATFORM_LZB
#include "../include/typmisra.h"
#include "../ablauf/crc002.h"
#endif

/**
 * @name
 * <b>LITTLE ENDIAN (aka Intel) system macros.</b>
 **************************************************************/
 /*@{*/
#ifdef SPL_OPTION_PROCESSOR_IS_LITTLE_ENDIAN

     #ifdef SPL_PLATFORM_LZB

 /*     #include "../include/typmisra.h"*/
      #define CONVERT_NETWORK_TO_HOST_32(src_byte_ptr) load32fromoddbytes(src_byte_ptr)
      /* 31 Byte I96 - 14 Byte CALL via stack + 17 Byte Function register byte access */

     #else /* SPL_PLATFORM_LZB */

      /**
       * Convert 4 bytes of memory pointed by src_byte_ptr
       * organized in Network orderto a 32 Bits integer
       * in Host order. */
      #define CONVERT_NETWORK_TO_HOST_32(src_byte_ptr)       \
                   ((((((( (spUINT32)(src_byte_ptr)[3]) << 8) \
                          |(spUINT32)(src_byte_ptr)[2]) << 8) \
                          |(spUINT32)(src_byte_ptr)[1]) << 8) \
                          |(spUINT32)(src_byte_ptr)[0])       

     #endif /* SPL_PLATFORM_LZB */

      /**
       * Convert 2 bytes of memory pointed by src_byte_ptr
       * organized in Network orderto a 16 Bits integer
       * in Host order. */
      #define CONVERT_NETWORK_TO_HOST_16(src_byte_ptr) \
          (spUINT16) (   ((spUINT16)(src_byte_ptr)[1] <<  8)     \
                       | ((spUINT16)(src_byte_ptr)[0]      ) )

     #ifdef SPL_PLATFORM_LZB

       #define CONVERT_HOST_TO_NETWORK_32(dst_byte_ptr, src_int_ptr)  copy4bytes(dst_byte_ptr, (void*)src_int_ptr)
            
     #else /* SPL_PLATFORM_LZB */

      /**
       * Convert the 32 bits (Host format) number
       * pointed by src_int_ptr into 4 bytes
       * pointed by dst_byte_ptr. */
      #define CONVERT_HOST_TO_NETWORK_32(dst_byte_ptr, src_int_ptr)   \
      /*lint -e717 do...while (0) MISRA 2004 rule 19.4 */       \
         do {   \
             (dst_byte_ptr)[0] = ((spBYTE *) (src_int_ptr))[0]; \
             (dst_byte_ptr)[1] = ((spBYTE *) (src_int_ptr))[1]; \
             (dst_byte_ptr)[2] = ((spBYTE *) (src_int_ptr))[2]; \
             (dst_byte_ptr)[3] = ((spBYTE *) (src_int_ptr))[3]; \
         } while (0)   \
      /*lint +e717 */

     #endif /* SPL_PLATFORM_LZB */

      /**
       *  Convert the 16 bits (Host format) number
       * pointed by src_int_ptr into 2 bytes
       * pointed by dst_byte_ptr. */
      #define CONVERT_HOST_TO_NETWORK_16(dst_byte_ptr, src_int_ptr)   \
      /*lint -e717 do...while (0) MISRA 2004 rule 19.4 */       \
         do {   \
             (dst_byte_ptr)[0] = ((spBYTE *) (src_int_ptr))[0]; \
             (dst_byte_ptr)[1] = ((spBYTE *) (src_int_ptr))[1]; \
         } while (0)   \
      /*lint +e717 */


#endif /* SPL_OPTION_PROCESSOR_IS_BIG_ENDIAN */
/*@}*/


/**
 * @name
 * <b>BIG ENDIAN (aka Motorola) system macros.</b>
 **************************************************************/
 /*@{*/
#ifdef SPL_OPTION_PROCESSOR_IS_BIG_ENDIAN

      /**
       * Convert 4 bytes of memory pointed by src_byte_ptr
       * organized in Network orderto a 32 Bits integer
       * in Host order. */
      #define CONVERT_NETWORK_TO_HOST_32(src_byte_ptr)       \
                   ((((((( (spUINT32)(src_byte_ptr)[3]) << 8) \
                          |(spUINT32)(src_byte_ptr)[2]) << 8) \
                          |(spUINT32)(src_byte_ptr)[1]) << 8) \
                          |(spUINT32)(src_byte_ptr)[0])       
      /**
       * Convert 2 bytes of memory pointed by src_byte_ptr
       * organized in Network orderto a 16 Bits integer
       * in Host order. */
      #define CONVERT_NETWORK_TO_HOST_16(src_byte_ptr) \
          (spUINT16) (   ((spUINT16)(src_byte_ptr)[1] <<  8)     \
                       |  (spUINT16)(src_byte_ptr)[0]      ) 
      /**
       * Convert the 32 bits (Host format) number
       * pointed by src_int_ptr into 4 bytes
       * pointed by dst_byte_ptr. */
      #define CONVERT_HOST_TO_NETWORK_32(dst_byte_ptr, src_int_ptr)   \
      /*lint -e717 do...while (0) MISRA 2004 rule 19.4 */       \
         do {   \
             (dst_byte_ptr)[0] = ((spBYTE *) (src_int_ptr))[3]; \
             (dst_byte_ptr)[1] = ((spBYTE *) (src_int_ptr))[2]; \
             (dst_byte_ptr)[2] = ((spBYTE *) (src_int_ptr))[1]; \
             (dst_byte_ptr)[3] = ((spBYTE *) (src_int_ptr))[0]; \
         } while (0)   \
      /*lint +e717 */
      
       /**
       *  Convert the 16 bits (Host format) number
       * pointed by src_int_ptr into 2 bytes
       * pointed by dst_byte_ptr. */
      #define CONVERT_HOST_TO_NETWORK_16(dst_byte_ptr, src_int_ptr)   \
      /*lint -e717 do...while (0) MISRA 2004 rule 19.4 */       \
         do {   \
             (dst_byte_ptr)[0] = ((spBYTE *) (src_int_ptr))[1]; \
             (dst_byte_ptr)[1] = ((spBYTE *) (src_int_ptr))[0]; \
         } while (0)   \
      /*lint +e717 */


#endif /* SPL_OPTION_PROCESSOR_IS_LITTLE_ENDIAN */
/*@}*/


#ifdef SPL_INCLUDE_DRIVER_HILSCHER


/** 
 * @name
 * <b>Conversion rountines for switching between endians.</b> 
 * They are currently used to switch endians when communicating between
 * the PPC603 (Big endian) and Kontron CP-353 PB board (little endian).
 *****************************************************************************/
 /*@{*/
#ifdef SPL_OPTION_PROCESSOR_IS_BIG_ENDIAN

     /**
      * @todo Definition description */
	 #define SWAP_ENDIAN_32(_x_)   \
	   (spUINT32)( (((_x_) & 0xFF000000UL) >> 24 ) |       \
	             (((_x_) & 0x00FF0000UL) >>  8 ) |       \
	             (((_x_) & 0x0000FF00UL) <<  8 ) |       \
	             (((_x_) & 0x000000FFUL) << 24 ) )
      /**
      * @todo Definition description */
	  #define SWAP_ENDIAN_16(_x_) \
	   (spUINT16) ( (((_x_) & 0xFF00U) >>  8 ) |         \
	              (((_x_) & 0x00FFU) <<  8 ) )

#endif /* SPL_OPTION_PROCESSOR_IS_BIG_ENDIAN */


#ifdef SPL_OPTION_PROCESSOR_IS_LITTLE_ENDIAN

     /**
      * @todo Definition description */
	 #define SWAP_ENDIAN_32(_x_)   \
	   (spUINT32)( _x_ )
      /**
      * @todo Definition description */
	  #define SWAP_ENDIAN_16(_x_) \
	   (spUINT16) ( _x_ )

#endif /* SPL_OPTION_PROCESSOR_IS_LITTLE_ENDIAN */

#endif /* SPL_INCLUDE_DRIVER_HILSCHER */
/*@}*/

#endif /* FFFIS_ENDIANESS_H_ */
