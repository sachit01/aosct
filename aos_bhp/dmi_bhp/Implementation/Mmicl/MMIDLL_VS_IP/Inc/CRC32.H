/* crc32.h  ---
 *
 *
 */

#ifndef __CRC32_H
#define __CRC32_H

#define CRC32_INITIAL_VALUE 0xffffffffUL
void crc32_init(void);
unsigned long crc32_update(unsigned long crc_accum, const byte *data_blk_ptr, unsigned int data_blk_size);
unsigned long crc32_calc(const byte *data_blk_ptr, unsigned int data_blk_size);

#endif /* __CRC32_H  */
