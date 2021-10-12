#pragma ident "@(#) Bombardier Transportation %full_filespec:  Crc32.c-1:csrc:arn_006#9 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          Crc32.c %
*
*  %version:       1 %
*
*  %created_by:    bhermans %
*
*  %date_created:  2017-08-22 11:04 %
*
*  DESCRIPTION: 
*              
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2012-05-17    Hidaji      Lint
* 2017-08-22    bhermans    Tailored for BHP
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "mmi_types.h"
#include <crc32.h>

#define POLYNOMIAL 0x1edc6f41   /* CRC-32 polynomial from Eurosig Consortium */

/* Test of the implementation: Using the polynomial above (after gen_crc_table())
* should give the results
*   update_crc(0, "\xa0\x06\xf7\xff\x00\xb4", 6) == 0xb2ccd37d
*   update_crc(0, "abcdef", 6) == 0x737988ae 
* When transmitting the 32-bit value on the serial link, transmit the MSB first.
*/
static unsigned long crc_table[256];

void crc32_init(void)
/* generate the table of CRC remainders for all possible bytes */
{

    int i, j;
    unsigned long crc_accum;

    for ( i = 0;  i < 256;  i++ ) {
        crc_accum = ( (unsigned long) i << 24 );
        for ( j = 0;  j < 8;  j++ ) 
        {
            if ( crc_accum & 0x80000000UL )
            {
                crc_accum = ( crc_accum << 1 ) ^ POLYNOMIAL;
            }
            else
            {
                crc_accum = ( crc_accum << 1 );
            }
        }
        crc_table[i] = crc_accum;
    }
    return;
}

unsigned long crc32_update(unsigned long crc_accum, const byte *data_blk_ptr, unsigned int data_blk_size)
/* update the CRC on the data block one byte at a time */
{
    int i;
    unsigned int j;
    for ( j = 0;  j < data_blk_size;  j++ ) {
        i = ( (int) ( crc_accum >> 24) ^ *data_blk_ptr++ ) & 0xff;
        crc_accum = ( crc_accum << 8 ) ^ crc_table[i];
    }
    return crc_accum;
}

/* calculate the CRC on the data block using a initial value of 0xffffffff
*/
unsigned long crc32_calc(const byte *data_blk_ptr, unsigned int data_blk_size) 
{
    return crc32_update(CRC32_INITIAL_VALUE, data_blk_ptr, data_blk_size);
}
