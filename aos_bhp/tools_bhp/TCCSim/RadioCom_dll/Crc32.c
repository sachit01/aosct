/* crc32.cpp
 *
 * Mats èkerblom  Kvaser AB
 *
 */
#include "atpdefs.h"
#include "crc32.h"

#define POLYNOMIAL 0x1edc6f41   /* CRC-32 polynomial from Eurosig Consortium */

/* Test of the implementation: Using the polynomial above (after gen_crc_table())
 * should give the results
 *   update_crc(0, "\xa0\x06\xf7\xff\x00\xb4", 6) == 0xb2ccd37d
 *   update_crc(0, "abcdef", 6) == 0x737988ae 
 * When transmitting the 32-bit value on the serial link, transmit the MSB first.
 */
static unsigned long crc_table[256];

__declspec(dllexport)void crc32_init(void)
 /* generate the table of CRC remainders for all possible bytes */
{
  register int i, j;
  register unsigned long crc_accum;
  for ( i = 0;  i < 256;  i++ ) {
    crc_accum = ( (unsigned long) i << 24 );
    for ( j = 0;  j < 8;  j++ ) {
      if ( crc_accum & 0x80000000L )
        crc_accum = ( crc_accum << 1 ) ^ POLYNOMIAL;
      else
        crc_accum = ( crc_accum << 1 );
    }
    crc_table[i] = crc_accum;
  }
  return;
}

__declspec(dllexport)unsigned long crc32_update(unsigned long crc_accum, byte *data_blk_ptr, unsigned int data_blk_size)
 /* update the CRC on the data block one byte at a time */
{
  register int i;
  register unsigned int j;
  for ( j = 0;  j < data_blk_size;  j++ ) {
    i = ( (int) ( crc_accum >> 24) ^ *data_blk_ptr++ ) & 0xff;
    crc_accum = ( crc_accum << 8 ) ^ crc_table[i];
  }
  return crc_accum;
}

/* calculate the CRC on the data block using a initial value of 0xffffffff
 */
unsigned long crc32_calc(byte *data_blk_ptr, unsigned int data_blk_size) 
{
	return crc32_update(CRC32_INITIAL_VALUE, data_blk_ptr, data_blk_size);
}
