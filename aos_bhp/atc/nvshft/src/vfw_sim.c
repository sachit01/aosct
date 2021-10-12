#include <vfw_buffer.h>
#include <vfw_checkpoints.h>
#include <vfw_crosscompare.h>
#include <vfw_init.h>
#include <vfw_string.h>
#include <vfw_sync.h>
#include <vfw_identity.h>
#include <time.h>
#include <vfw_nvsh.h>
#include <vfw_crc.h>
#include <vfw_string.h>
#include <string.h>

#ifdef WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

#ifndef __GNUG__
#define __attribute__(A) /* If not GNU, do nothing. This must be defined before including vfw_halt.h */
#endif
#include <vfw_halt.h>
#include <vfw_version.h>

#define bool char
#define false 0
#define true 1

typedef __uint64_t uint64_t;
typedef char char_t;

#define BUFFER_READ_MODE (0)
#define BUFFER_WRITE_MODE (1)

void vfwValidateBuffer(const VFW_Buffer* const buffer);

const char_t* module_C = __FILE__;

bool vfwInitIsDone = false;
bool vfwStartIsDone = false;

const uint64_t crc64Init = 0U;

static const uint64_t crc64Table[256] =
{
  0x0000000000000000, 0x71d0d03b74604ee7, 0xe3a1a076e8c09dce, 0x9271704d9ca0d329,
  0xb69390d6a5e1757b, 0xc74340edd1813b9c, 0x553230a04d21e8b5, 0x24e2e09b3941a652,
  0x1cf7f1963fa2a411, 0x6d2721ad4bc2eaf6, 0xff5651e0d76239df, 0x8e8681dba3027738,
  0xaa6461409a43d16a, 0xdbb4b17bee239f8d, 0x49c5c13672834ca4, 0x3815110d06e30243,
  0x39efe32c7f454822, 0x483f33170b2506c5, 0xda4e435a9785d5ec, 0xab9e9361e3e59b0b,
  0x8f7c73fadaa43d59, 0xfeaca3c1aec473be, 0x6cddd38c3264a097, 0x1d0d03b74604ee70,
  0x251812ba40e7ec33, 0x54c8c2813487a2d4, 0xc6b9b2cca82771fd, 0xb76962f7dc473f1a,
  0x938b826ce5069948, 0xe25b52579166d7af, 0x702a221a0dc60486, 0x01faf22179a64a61,
  0x73dfc658fe8a9044, 0x020f16638aeadea3, 0x907e662e164a0d8a, 0xe1aeb615622a436d,
  0xc54c568e5b6be53f, 0xb49c86b52f0babd8, 0x26edf6f8b3ab78f1, 0x573d26c3c7cb3616,
  0x6f2837cec1283455, 0x1ef8e7f5b5487ab2, 0x8c8997b829e8a99b, 0xfd5947835d88e77c,
  0xd9bba71864c9412e, 0xa86b772310a90fc9, 0x3a1a076e8c09dce0, 0x4bcad755f8699207,
  0x4a30257481cfd866, 0x3be0f54ff5af9681, 0xa9918502690f45a8, 0xd84155391d6f0b4f,
  0xfca3b5a2242ead1d, 0x8d736599504ee3fa, 0x1f0215d4ccee30d3, 0x6ed2c5efb88e7e34,
  0x56c7d4e2be6d7c77, 0x271704d9ca0d3290, 0xb566749456ade1b9, 0xc4b6a4af22cdaf5e,
  0xe05444341b8c090c, 0x9184940f6fec47eb, 0x03f5e442f34c94c2, 0x72253479872cda25,
  0xe7bf8cb1fd152088, 0x966f5c8a89756e6f, 0x041e2cc715d5bd46, 0x75cefcfc61b5f3a1,
  0x512c1c6758f455f3, 0x20fccc5c2c941b14, 0xb28dbc11b034c83d, 0xc35d6c2ac45486da,
  0xfb487d27c2b78499, 0x8a98ad1cb6d7ca7e, 0x18e9dd512a771957, 0x69390d6a5e1757b0,
  0x4ddbedf16756f1e2, 0x3c0b3dca1336bf05, 0xae7a4d878f966c2c, 0xdfaa9dbcfbf622cb,
  0xde506f9d825068aa, 0xaf80bfa6f630264d, 0x3df1cfeb6a90f564, 0x4c211fd01ef0bb83,
  0x68c3ff4b27b11dd1, 0x19132f7053d15336, 0x8b625f3dcf71801f, 0xfab28f06bb11cef8,
  0xc2a79e0bbdf2ccbb, 0xb3774e30c992825c, 0x21063e7d55325175, 0x50d6ee4621521f92,
  0x74340edd1813b9c0, 0x05e4dee66c73f727, 0x9795aeabf0d3240e, 0xe6457e9084b36ae9,
  0x94604ae9039fb0cc, 0xe5b09ad277fffe2b, 0x77c1ea9feb5f2d02, 0x06113aa49f3f63e5,
  0x22f3da3fa67ec5b7, 0x53230a04d21e8b50, 0xc1527a494ebe5879, 0xb082aa723ade169e,
  0x8897bb7f3c3d14dd, 0xf9476b44485d5a3a, 0x6b361b09d4fd8913, 0x1ae6cb32a09dc7f4,
  0x3e042ba999dc61a6, 0x4fd4fb92edbc2f41, 0xdda58bdf711cfc68, 0xac755be4057cb28f,
  0xad8fa9c57cdaf8ee, 0xdc5f79fe08bab609, 0x4e2e09b3941a6520, 0x3ffed988e07a2bc7,
  0x1b1c3913d93b8d95, 0x6acce928ad5bc372, 0xf8bd996531fb105b, 0x896d495e459b5ebc,
  0xb178585343785cff, 0xc0a8886837181218, 0x52d9f825abb8c131, 0x2309281edfd88fd6,
  0x07ebc885e6992984, 0x763b18be92f96763, 0xe44a68f30e59b44a, 0x959ab8c87a39faad,
  0xbeafc9588e4a0ff7, 0xcf7f1963fa2a4110, 0x5d0e692e668a9239, 0x2cdeb91512eadcde,
  0x083c598e2bab7a8c, 0x79ec89b55fcb346b, 0xeb9df9f8c36be742, 0x9a4d29c3b70ba9a5,
  0xa25838ceb1e8abe6, 0xd388e8f5c588e501, 0x41f998b859283628, 0x302948832d4878cf,
  0x14cba8181409de9d, 0x651b78236069907a, 0xf76a086efcc94353, 0x86bad85588a90db4,
  0x87402a74f10f47d5, 0xf690fa4f856f0932, 0x64e18a0219cfda1b, 0x15315a396daf94fc,
  0x31d3baa254ee32ae, 0x40036a99208e7c49, 0xd2721ad4bc2eaf60, 0xa3a2caefc84ee187,
  0x9bb7dbe2ceade3c4, 0xea670bd9bacdad23, 0x78167b94266d7e0a, 0x09c6abaf520d30ed,
  0x2d244b346b4c96bf, 0x5cf49b0f1f2cd858, 0xce85eb42838c0b71, 0xbf553b79f7ec4596,
  0xcd700f0070c09fb3, 0xbca0df3b04a0d154, 0x2ed1af769800027d, 0x5f017f4dec604c9a,
  0x7be39fd6d521eac8, 0x0a334feda141a42f, 0x98423fa03de17706, 0xe992ef9b498139e1,
  0xd187fe964f623ba2, 0xa0572ead3b027545, 0x32265ee0a7a2a66c, 0x43f68edbd3c2e88b,
  0x67146e40ea834ed9, 0x16c4be7b9ee3003e, 0x84b5ce360243d317, 0xf5651e0d76239df0,
  0xf49fec2c0f85d791, 0x854f3c177be59976, 0x173e4c5ae7454a5f, 0x66ee9c61932504b8,
  0x420c7cfaaa64a2ea, 0x33dcacc1de04ec0d, 0xa1addc8c42a43f24, 0xd07d0cb736c471c3,
  0xe8681dba30277380, 0x99b8cd8144473d67, 0x0bc9bdccd8e7ee4e, 0x7a196df7ac87a0a9,
  0x5efb8d6c95c606fb, 0x2f2b5d57e1a6481c, 0xbd5a2d1a7d069b35, 0xcc8afd210966d5d2,
  0x591045e9735f2f7f, 0x28c095d2073f6198, 0xbab1e59f9b9fb2b1, 0xcb6135a4effffc56,
  0xef83d53fd6be5a04, 0x9e530504a2de14e3, 0x0c2275493e7ec7ca, 0x7df2a5724a1e892d,
  0x45e7b47f4cfd8b6e, 0x34376444389dc589, 0xa6461409a43d16a0, 0xd796c432d05d5847,
  0xf37424a9e91cfe15, 0x82a4f4929d7cb0f2, 0x10d584df01dc63db, 0x610554e475bc2d3c,
  0x60ffa6c50c1a675d, 0x112f76fe787a29ba, 0x835e06b3e4dafa93, 0xf28ed68890bab474,
  0xd66c3613a9fb1226, 0xa7bce628dd9b5cc1, 0x35cd9665413b8fe8, 0x441d465e355bc10f,
  0x7c08575333b8c34c, 0x0dd8876847d88dab, 0x9fa9f725db785e82, 0xee79271eaf181065,
  0xca9bc7859659b637, 0xbb4b17bee239f8d0, 0x293a67f37e992bf9, 0x58eab7c80af9651e,
  0x2acf83b18dd5bf3b, 0x5b1f538af9b5f1dc, 0xc96e23c7651522f5, 0xb8bef3fc11756c12,
  0x9c5c13672834ca40, 0xed8cc35c5c5484a7, 0x7ffdb311c0f4578e, 0x0e2d632ab4941969,
  0x36387227b2771b2a, 0x47e8a21cc61755cd, 0xd599d2515ab786e4, 0xa449026a2ed7c803,
  0x80abe2f117966e51, 0xf17b32ca63f620b6, 0x630a4287ff56f39f, 0x12da92bc8b36bd78,
  0x1320609df290f719, 0x62f0b0a686f0b9fe, 0xf081c0eb1a506ad7, 0x815110d06e302430,
  0xa5b3f04b57718262, 0xd46320702311cc85, 0x4612503dbfb11fac, 0x37c28006cbd1514b,
  0x0fd7910bcd325308, 0x7e074130b9521def, 0xec76317d25f2cec6, 0x9da6e14651928021,
  0xb94401dd68d32673, 0xc894d1e61cb36894, 0x5ae5a1ab8013bbbd, 0x2b357190f473f55a
};

/******************************************************************************
* vfwCalcCrc64
******************************************************************************/
uint64_t vfwCalcCrc64(const void *pData, uint32_t nBytes)
{
  uint64_t crc = crc64Init;
  uint32_t bytesLeft = nBytes;
  uint64_t xorval;
  uint8_t* start = (uint8_t*) pData;

  if (NULL != start)
  {
    while (bytesLeft > 0U)
    {
      crc = crc64Table[*start ^ (uint8_t)(crc >> 56)] ^ (crc << 8);
      start++;
      bytesLeft--;
    }
  }
  else
  {
    VFW_HALT(("NULL pointer for start passed to calculateCRC()"));
  }
 
  return crc;
}

/* Local helper functions
*/

/******************************************************************************
* unsigned minimum
******************************************************************************/
uint32_t minimum(const uint32_t a, const uint32_t b)
{
  return (a < b) ? a : b;
}

VFW_Buffer  *
vfwBufferHead(VFW_Buffer * buffer)
{
  VFW_Buffer         *head = buffer;

  if (buffer == NULL) {
   VFW_HALT(("Illegal parameter"));
  }
  else
  {
    while (head->p != NULL) {
      head = (VFW_Buffer *)head->p;
    }
  }

  return head;
}

void
vfwUpdateBuffer(VFW_Buffer * buffer, int32_t relative_offset)
{
  if (buffer == NULL)
  {
    VFW_HALT(("Illegal parameter"));
  }
  else
  {
    VFW_Buffer         *current;

    for (current = buffer; current != NULL; current = current->p)
    {
      current->o += relative_offset;

      if (current->b_s < current->o)
      {
        VFW_HALT(("buffer overflow sanity check"));
      }
    }
  }
}

void vfwValidateBuffer(const VFW_Buffer* const buffer)
{
  if (buffer == NULL)
  {
    VFW_HALT(("Illegal parameter"));
  }
  else
  {
    if (buffer->b == NULL)
    {
      VFW_HALT(("Illegal parameter"));
    }

    if (buffer->b_s < buffer->o)
    {
      VFW_HALT(("Illegal parameter"));
    }

    switch (buffer->m)
    {
    case BUFFER_READ_MODE:
      if ((buffer->v < buffer->o))
      {
        VFW_HALT(("Illegal parameter, offset greater than valid"));
      }
      break;

    case BUFFER_WRITE_MODE:
      break;

    default:
      VFW_HALT(("Illegal mode"));
      break;
    }
  }
}

/***************************************************************************
* vfw buffer mode check functions
*-----------------------------------------------------------------------------
*
* \brief  Used for validating the mode of buffer (Internal file used functions)
* \return  Mode of buffer(True - Write /False -Read)
******************************************************************************/

void VfwRecursivelyValidateBuffer(const VFW_Buffer * buffer)
{
  VFW_Buffer         *current = (VFW_Buffer *)buffer;

  do
  {
    vfwValidateBuffer(current);
    current = current->p;
   } while (current != NULL);
}

/***************************************************************************
* vfw Buffer handling functions
*-----------------------------------------------------------------------------
*
* Simulate vfw behaviour
*
******************************************************************************/

/******************************************************************************
* \brief       Initialize a raw buffer for VFW_Buffer usage.
*
*              The VFW buffer framework will take care of buffer overflow protection,
*              current offset and alignment.
*
*******************************************************************************/
void vfwInitBuffer(VFW_Buffer* buffer, uint8_t* raw_buffer, uint32_t buffer_size)
{
  if (buffer != NULL)
  {
    buffer->b = raw_buffer;
    buffer->o = 0;
    buffer->b_s = buffer_size;
    buffer->p = NULL;
    buffer->m = BUFFER_WRITE_MODE;
    buffer->v = 0;
  }
  else
  {
  }
}

/**
* \brief       Reserves part of an existing VFW_Buffer for indepenent usage from the
*              existing source buffer.
*
*              The buffer inherits its mode from the parent buffer.
*/
void vfwInitSubBuffer(VFW_Buffer * buffer, VFW_Buffer * source_buffer, uint32_t buffer_size)
{
  if (buffer == NULL)
  {
    VFW_HALT(("Illegal parameter"));
  }
  else
  {
    VfwRecursivelyValidateBuffer(source_buffer);
    if (buffer_size > (source_buffer->b_s - source_buffer->o))
    {
      VFW_HALT(("Trying to allocate outside buffer size"));
    }

    buffer->b = &source_buffer->b[source_buffer->o];
    buffer->b_s = buffer_size;
    buffer->o = 0;
    buffer->m = source_buffer->m;
    buffer->v = (buffer->m == BUFFER_READ_MODE ? buffer->b_s : 0);
    buffer->p = NULL;
    vfwUpdateBuffer(source_buffer, buffer_size);
  }
}

/*******************************************************************************
* \brief           Sets the used memory area in buffer to zero and resets the
*                  internal buffer offset to zero.
* \param [in,out]  buffer buffer to be cleared.
********************************************************************************/

void vfwClearBuffer(VFW_Buffer* buffer)
{
  vfwValidateBuffer(buffer);

  buffer->v = 0;
  buffer->m = BUFFER_WRITE_MODE;
  memset(buffer->b, 0, (size_t)(buffer->o));
  buffer->o = 0;
}

/*******************************************************************************
* \brief       Update the valid length of a write buffer and set it to read mode.
*
*              To be used when the buffer has been initialised with a raw_buffer
*              that already contains data to be used by buffer functions.
*******************************************************************************/

void vfwSetReadBuffer(VFW_Buffer* buffer, uint32_t buffer_size)
{
  vfwValidateBuffer(buffer);
  buffer->v = buffer_size;
  buffer->o = 0;            
  buffer->m = BUFFER_READ_MODE;   
}

/*******************************************************************************
* \brief           Stop writing data to a buffer, reset offset and set read mode.
*                  If the buffer is already in read mode it is unchanged.
*
* \param [in,out]  buffer Buffer to change offset and mode in.
*******************************************************************************/

void vfwSetFullBuffer(VFW_Buffer* buffer)
{
  vfwValidateBuffer(buffer);

  if (buffer->m != BUFFER_READ_MODE)
  {
    buffer->v = buffer->o;
    buffer->m = BUFFER_READ_MODE;
    buffer->o = 0;
  }
}

/*******************************************************************************
* \brief           Appends an uint64_t value to the buffer.
* \param [in,out]  buffer Buffer to be appended.
* \param [in]      value Value to add.
*******************************************************************************/

void vfwPutU64(VFW_Buffer* buffer, uint64_t value)
{
  uint32_t hiValue = htonl((uint32_t) (value >> 32U));
  uint32_t loValue = htonl((uint32_t) value);

  vfwCpyFromRawBuffer(buffer, (uint8_t*) &hiValue, (int32_t) sizeof(hiValue));
  vfwCpyFromRawBuffer(buffer, (uint8_t*) &loValue, (int32_t) sizeof(loValue));
}

/***************************************************************************
* \brief           Reads an uint64_t value from buffer.
*
* \param           buffer Buffer to read from.
* \return          The value pointed to at the current offset is returned in host byte order.
*
****************************************************************************/

uint64_t vfwGetU64(VFW_Buffer* buffer)
{
  uint32_t hiValue;
  uint32_t loValue;
  uint64_t host_value;

  vfwCpyToRawBuffer((uint8_t*) &hiValue, buffer, (int32_t) sizeof(hiValue));
  vfwCpyToRawBuffer((uint8_t*) &loValue, buffer, (int32_t) sizeof(loValue));

  host_value = (((uint64_t) ntohl(hiValue)) << 32U) + ((uint64_t) ntohl(loValue));

  return host_value;
}

/****************************************************************************
* \brief           Appends an int64_t value to the buffer.
*
* \param [in,out]  buffer Buffer to append.
* \param [in]      value Value to add.
*****************************************************************************/

void vfwPutI64(VFW_Buffer* buffer, const int64_t value)
{
  vfwPutU64(buffer, (uint64_t) value);
}

/****************************************************************************
* \brief           Reads an int64_t value from the buffer.
*
* \param [in,out]  buffer Buffer to read from.
*****************************************************************************/

int64_t vfwGetI64(VFW_Buffer* buffer)
{
  int64_t host_value = (int64_t) vfwGetU64(buffer);

  return host_value;
}
/*****************************************************************************
* \brief           Appends an uint32_t value to the buffer.
* \param [in,out]  buffer Buffer to be appended.
* \param [in]      value Value to add.
****************************************************************************/

void vfwPutU32(VFW_Buffer* buffer, uint32_t value)
{
  uint32_t            network_value = htonl(value);
  vfwCpyFromRawBuffer(buffer, (uint8_t *)& network_value, (int32_t) sizeof(network_value));
}
/*****************************************************************************
* \brief           Reads an uint32_t value from the buffer.
*
* \param [in,out]  buffer Buffer to read from.
* \return          The value pointed to at the current offset is returned in host byte order.
****************************************************************************/

uint32_t vfwGetU32(VFW_Buffer* buffer)
{
  uint32_t            value, host_value;
  vfwCpyToRawBuffer((uint8_t *)& value, buffer, (int32_t) sizeof(value));
  host_value = ntohl(value);

  return host_value;
}

/*****************************************************************************
* \brief           Appends an int32_t value to the buffer.
* \param [in,out]  buffer Buffer to append.
* \param [in]      value Value to add.
****************************************************************************/
void vfwPutI32(VFW_Buffer* buffer, int32_t value)
{
  vfwPutU32(buffer, (uint32_t) (value));
}
/****************************************************************************
* \brief           Reads an int32_t value from the buffer.
*
* \param [in,out]  buffer Buffer to read from.
* \return          The value pointed to at the current offset is returned in host byte order.
****************************************************************************/
int32_t vfwGetI32(VFW_Buffer* buffer)
{
  return (int32_t) (vfwGetU32(buffer));
}
/***************************************************************************
* \brief           Appends an uint16_t value to the buffer.
* \param [in,out]  buffer Buffer to append.
* \param [in]      value Value to add.
****************************************************************************/
void vfwPutU16(VFW_Buffer* buffer, uint16_t value)
{
  uint16_t            network_value = htons(value);
  vfwCpyFromRawBuffer(buffer, (uint8_t *)& network_value, (int32_t) sizeof(network_value));
}

/***************************************************************************
* \brief           Reads an uint16_t value from the buffer.
*
* \param [in,out]  buffer Buffer to read from.
* \return          The value pointed to at the current offset is returned in host byte order.
 ****************************************************************************/
uint16_t vfwGetU16(VFW_Buffer* buffer)
{
  uint16_t            value, host_value;
  vfwCpyToRawBuffer((uint8_t *)& value, buffer, (int32_t) sizeof(value));
  host_value = ntohs(value);

  return host_value;
}
/***************************************************************************
* \brief           Appends an int16_t value to the buffer.
* \param [in,out]  buffer Buffer to append.
* \param [in]      value Value to add.
****************************************************************************/
void vfwPutI16(VFW_Buffer* buffer, int16_t value)
{
  vfwPutU16(buffer, (uint16_t)(value));
}
/***************************************************************************
* \brief           Reads an int16_t value from the buffer.
* \param [in,out]  buffer Buffer to read from.
* \return          The value pointed to at the current offset is returned in host byte order.
****************************************************************************/
int16_t vfwGetI16(VFW_Buffer* buffer)
{
  return (int16_t)(vfwGetU16(buffer));
}

/*****************************************************************************
* \brief           Appends an uint8_t value to the buffer.
* \param [in,out]  buffer Buffer to append.
* \param [in]      value Value to add.
***************************************************************************/
void vfwPutU8(VFW_Buffer* buffer, uint8_t value)
{
  vfwCpyFromRawBuffer(buffer, (uint8_t *)& value, (int32_t) sizeof(value));
}

/***************************************************************************
* \brief           Reads an uint8_t value from the buffer.
*
* \param [in,out]  buffer Buffer to read from.
* \return          The value pointed to at the current offset is returned.
 ***************************************************************************/
uint8_t vfwGetU8(VFW_Buffer* buffer)
{
  uint8_t             value;
  vfwCpyToRawBuffer(&value, buffer, (int32_t) sizeof(value));

  return value;
}
/***************************************************************************
* \brief           Appends an int8_t value to the buffer.
* \param [in,out]  buffer Buffer to append.
* \param [in]      value Value to add
****************************************************************************/
void vfwPutI8(VFW_Buffer* buffer, int8_t value)
{
  vfwPutU8(buffer, (uint8_t)(value));
}
/***************************************************************************
* \brief           Reads an int8_t value from the buffer.
*
* \param [in,out]  buffer Buffer to read from.
* \return          The value pointed to at the current offset is returned.

****************************************************************************/
int8_t vfwGetI8(VFW_Buffer* buffer)
{
  return (int8_t) (vfwGetU8(buffer));
}
/*****************************************************************************
* \brief           Appends a bool_t value to the buffer.
* \param [in,out]  buffer Buffer to append.
* \param [in]      value Value to add.
****************************************************************************/
void vfwPutBOOL(VFW_Buffer* buffer, bool_t value)
{
  uint8_t  bool_value = value ? 1 : 0;

  vfwCpyFromRawBuffer(buffer, &bool_value, (int32_t) sizeof(bool_value));
}
/****************************************************************************
* \brief           Reads a bool_t value from the buffer.
*
* \param [in,out]  buffer Buffer to read from.
* \return          The value pointed to at the current offset is returned in host byte order.
****************************************************************************/
bool_t vfwGetBOOL(VFW_Buffer* buffer)
{
  bool_t              value;
  uint8_t             bool_value;

  vfwCpyToRawBuffer(&bool_value, buffer, (int32_t) sizeof(bool_value));
  value = (bool_value == 1);

  return value;
}
/*****************************************************************************
* \brief           Appends a character string to the buffer and increments the offset in the buffer.
*
*                  The length of the string is placed before the string as an int32_t value.
*
* \param [in,out]  buffer      Buffer to append.
* \param [in]      text_string String to add.
****************************************************************************/
void vfwPutString(VFW_Buffer* buffer, const char * text_string)
{
  if (text_string == NULL)
  {
    VFW_HALT(("Illegal parameter"));
  }
  else
  {
    const int32_t  text_length = (int32_t)strlen(text_string);
    vfwValidateBuffer(buffer);

    if (buffer->b_s < (buffer->o + text_length + 4))
    {
      VFW_HALT(("Writing outside buffer"));
    }

    vfwPutI32(buffer, text_length);
    vfwCpyFromRawBuffer(buffer, (uint8_t *)text_string, text_length);
  }
}

/****************************************************************************
* \brief           Reads a character string from the buffer.
*
*                  The string is always nul-terminated.
*
* \param [in,out]  buffer      Buffer to read from.
*        [out]      text_string  String to be used for returning the read string.
*        [in]      size        The maximum length of the string to read, including the nul character.
* \return          The actual length of the read string, excluding the nul character.
****************************************************************************/
uint32_t vfwGetString(VFW_Buffer* buffer, char * text_string, uint32_t size)
{
  uint32_t retValue = 0U;

  vfwValidateBuffer(buffer);
  {
    const int32_t strLen = vfwGetI32(buffer);

    if (strLen < 0)
    {

    }
    else if ((uint32_t)(strLen) >= size)
    {

    }
    else
    {
      retValue = (uint32_t)(strLen);
      vfwCpyToRawBuffer(text_string, buffer, retValue);
      text_string[retValue] = '\0';
    }
  }

  return retValue;
}

/***************************************************************************
* \brief           Copy from a VFW_Buffer buffer to a raw buffer.
*
*                  The data will be copied from the current offset in buffer.
* \param [in, out] raw_buffer Raw buffer to copy to.
* \param [in,out]  buffer Buffer to copy from.
* \param [in]      size Number of bytes to copy.
****************************************************************************/
void vfwCpyToRawBuffer(void *  raw_buffer, VFW_Buffer *  buffer, uint32_t  size)
{
  if (raw_buffer == NULL)
  {
    VFW_HALT(("Illegal parameter"));
  }
  else
  {
    vfwValidateBuffer(buffer);
    if (buffer->m != BUFFER_READ_MODE)
    {
      VFW_HALT(("Illegal mode"));
    }
    else
    {
      if (buffer->v < (size + buffer->o))
      {
       VFW_HALT(("Reading outside buffer"));
      }

      memmove(raw_buffer, &buffer->b[buffer->o], (size_t)size);
      vfwUpdateBuffer(buffer, size);
    }
  }
}

/***************************************************************************
* \brief           Copy from a raw buffer to a buffer.
*
*                  The data will be appended at the current offset in the buffer.
* \param [in,out]  buffer Buffer to copy to.
* \param [in]      raw_buffer Buffer to copy from. May be NULL if buffer is fake buffer.
* \param [in]      size Number of bytes to copy.
****************************************************************************/
void vfwCpyFromRawBuffer(VFW_Buffer *  buffer, const void * raw_buffer, uint32_t  size)
{
  if (raw_buffer == NULL)
  {
    VFW_HALT(("Illegal parameter"));
  }
  else
  {
    vfwValidateBuffer(buffer);
    if (buffer->m != BUFFER_WRITE_MODE)
    {
      VFW_HALT(("Illegal mode"));
    }
    else 
    {
      if (size > (buffer->b_s - buffer->o))
      {
        VFW_HALT(("Reading outside buffer"));
      }

      memmove(&buffer->b[buffer->o], raw_buffer, (size_t)size);
      vfwUpdateBuffer(buffer, size);
    }
  }
}


/**
* \brief           Copy from source_buffer to dest_buffer.
*
*                  The data will be
*                  retrieved from the current offset in source_buffer and appended
*                  at the current offset in the dest_buffer.
*                  If dest_buffer is a fake buffer, source_buffer will not be copied,
*                  however, dest_buffer's offset and size will be updated.
*
* \param [in,out]  dest_buffer Buffer to copy to.
* \param [in,out]  source_buffer Buffer to copy from. May be NULL if dest_buffer is a fake buffer.
* \param [in]      size Number of bytes to copy.
* \pre             source_buffer and dest_buffer must have been initialized.
* \pre             source_buffer and dest_buffer may not be part of the same parent/child tree.
* \post            The data has been copied from source_buffer to dest_buffer and the
*                  internal offset of each buffer has been incremented.
* \exception       dest_buffer is invalid.
* \exception       source_buffer is invalid.
* \exception       source_buffer is fake and dest_buffer is not fake.
* \exception       dest_buffer mode is read.
* \exception       source_buffer mode is write.
* \exception       dest_buffer trying to write outside dest_buffer.
* \exception       source_buffer trying to read outside source_buffer.
* \exception       source_buffer and dest_buffer origin from the same parent buffer.
*/

void
vfwCpyBuffer(VFW_Buffer * dest_buffer, VFW_Buffer * source_buffer, uint32_t size)
{
  vfwValidateBuffer(dest_buffer);
  vfwValidateBuffer(source_buffer);

  if (dest_buffer->m != BUFFER_WRITE_MODE)
  {
    VFW_HALT(("Buffer mode error in destination buffer"));
  }

  if (source_buffer->m != BUFFER_READ_MODE)
  {
    VFW_HALT(("Buffer mode error in source buffer"));
  }

  if (vfwBufferHead(dest_buffer) == vfwBufferHead(source_buffer))
  {
    VFW_HALT(("Not allowed to copy within buffer this way"));
  }

  if (vfwGetFreeSpace(dest_buffer) < size)
  {
    VFW_HALT(("Buffer overflow in destination buffer"));
  }

  if (vfwGetValidSize(source_buffer) < size)
  {
    VFW_HALT(("Buffer overflow in source buffer"));
  }

  uint8_t* const destPointer = vfwGetPointer(dest_buffer);
  const uint8_t* const sourcePointer = vfwGetPointer(source_buffer);

  memmove(destPointer, sourcePointer, size);

  vfwUpdateBuffer(dest_buffer, size);
  vfwUpdateBuffer(source_buffer, size);
}



/***************************************************************************
* \brief           In read mode, returns the number of bytes left to read.
*                  In write mode, returns offset.
* \param [in]      buffer Buffer to get valid size of.
* \return          number of bytes left to read.
****************************************************************************/
uint32_t vfwGetValidSize(const VFW_Buffer * buffer)
{
  if (buffer->m == BUFFER_READ_MODE)
  {
    return (buffer->v - buffer->o);
  }
  else
  {
    return buffer->o;
  }
}

/***************************************************************************
* vfw Get current pointer functions
*-----------------------------------------------------------------------------
*
* \brief  Used to get the pointer to current offset
* \return  pointer to current offset
******************************************************************************/
uint8_t* vfwGetPointer(const VFW_Buffer* buffer)
{
  return (buffer->b + buffer->o);
}

/**
* \brief           Returns the number of bytes left in the buffer, based on the
*                  current offset.
* \param [in]      buffer Buffer to get free space of.
* \return          Remaining bytes in buffer.
* \note            For a fake buffer UINT32_MAX - the current offset will be returned.
* \pre             buffer must have been initialized.
* \exception       buffer is invalid.
*/
uint32_t vfwGetFreeSpace(const VFW_Buffer * buffer)
{
  vfwValidateBuffer(buffer);

  return (buffer->b_s - buffer->o);
}

/**
* \brief           Returns the size of the buffer.
* \param [in]      buffer Buffer to get size of.
* \return          Buffer size.
* \note            For a fake buffer UINT32_MAX - the current offset will be returned.
* \pre             buffer must have been initialized.
* \exception       buffer is invalid.
*/
uint32_t vfwGetMaxSize(const VFW_Buffer* buffer)
{
  vfwValidateBuffer(buffer);

  return buffer->b_s;
}

/**
* \brief           Return an uint8_t pointer to the start of the byte array in buffer.
* \param [in]      buffer Buffer to get pointer from.
* \return          uint8_t pointer to start of buffer.
* \pre             buffer must have been initialized.
* \exception       buffer is invalid.
* \exception       buffer is fake.
*/
uint8_t            *
 vfwGetStart(const VFW_Buffer * buffer)
{
  vfwValidateBuffer(buffer);

  return buffer->b;
}

/**
* \brief           Consume data in buffer.
*
* \param [in,out]  buffer Buffer to consume.
* \param [in]      size Number of bytes to consume.
*
* \pre             buffer must have been initialized.
* \pre             buffer must be in read mode.
* \post            The internal offset in the buffer has been incremented by size.
* \exception       buffer is invalid.
* \exception       buffer mode is write.
* \exception       buffer trying to move offset outside buffer.
* \see             VFW_BufferStatus vfwConsumeBufferCheck()
*/
void
 vfwConsumeBuffer(VFW_Buffer * buffer, uint32_t size)
{
  vfwValidateBuffer(buffer);

  if (buffer->m != BUFFER_READ_MODE)
  {
    VFW_HALT(("Illegal mode"));
  }
  else
  {
    if (buffer->v < (size + buffer->o))
    {
      VFW_HALT(("Consuming outside buffer"));
    }

    vfwUpdateBuffer(buffer, size);
  }
}

/**
* \brief Concatenate strings.
*
*  Guarantees to NUL-terminate the destination string for all
*  strings where the given size is non-zero.
* \param [out]  dst The destination.
* \param [in]   src The source.
* \param [in]   siz The size of dst.
* \return       The total length of the created string.
* \exception    dst is too small.
*/
size_t vfw_strlcat(char *dst, const char *src, size_t siz)
{
  const size_t lenSrc = strnlen(src, siz);
  const size_t lenDst = strnlen(dst, siz);  
  size_t cpyLen;

  if (lenDst < (siz - 1))
  {
    cpyLen = siz - lenDst - 1;
    if (lenSrc < cpyLen)
    {
      cpyLen = lenSrc;
    }

    strncat(dst, src, cpyLen);
  }
  else
  {
    cpyLen = 0;
    dst[siz] = '\0'; 
  }

  cpyLen = strnlen(dst, siz);   

  return cpyLen;
}

/**
* \brief Copy a string.
*
*  Guarantees to NUL-terminate the destination string for all
*  strings where the given size is non-zero.
* \param [out]  dst The destination.
* \param [in]   src The source.
* \param [in]   siz The size of dst.
* \return       The total length of the created string.
* \exception    dst is too small.
*/
size_t vfw_strlcpy(char *dst, const char *src, size_t siz)
{
  const size_t lenSrc = strnlen(src, siz);

  VFW_ASSERT((lenSrc < siz), ("vfw_strlcpy ERROR: lenSrc >= siz"));

  strncpy(dst, src, lenSrc);
  dst[lenSrc] = 0;
 
  return (lenSrc);
}

/**
* \brief  Prints information and halts the system.
*
* This function
* \li formats and prints information including time, file information,
*     line number, function name, and a custom message, using syslog
* \li calls the registred callback (if vfwAtHalt() has been called)
* \li halts the calling process and sends
*     a SIGTERM signal to the process group.
*
* Time is printed in the format YYYY-MM-DD hh:mm:ss.sss (ssssss.sss)
* i.e. local time and master time as returned from vfwGetUnsynchronizedTime().
*
* When compiled with -DVFW_TEST_SYSTEM the function prints to stderr.
* If not compiled with -DVFW_TEST_SYSTEM the function prints to stderr and syslog.
*
* \param [in] sourceFileInfo Information about the caller, such as filename.
* \param [in] lineNumber The line number from where this function was called.
* \param [in] functionName The name of the calling function.
* \param [in] format A printf style format specifier.
*
* \note  Use macro VFW_HALT() or VFW_ASSERT()
*        rather than calling this function directly.
*/
/*lint -esym(960,16.1) MISRA 2004 Required Rule 16.1, function has variable number of arguments
* The benefit of having a flexible and simple way to report diagnostics when terminating outweighs
* the problems associated with variable number of argument semantics. */
void vfwHalt(const char* sourceFileInfo, const integer_t lineNumber, const char* functionName, const char* format, ...)
 /*lint +esym(960,16.1) */
{
  char_t text[250];

  va_list args;
  va_start(args, format);
  int ret = vsnprintf(text, sizeof(text), format, args);
  va_end(args);

  if (ret <= 0)
  {
    text[0] = '\0';
  }

  fprintf(stderr, "HALT in aosHalt: %s\n", text);

  exit(1);
}

/**
* \brief   Prints information and halts the system.
*
* \li formats and prints information including file information,
*     line number, function name, failed condition and a custom message,
*     using syslog
* \li calls the registred callback (if vfwAtHalt() has been called)
* \li halts the calling process and sends
*     a SIGTERM signal to the process group.
*
* Time is printed in the format YYYY-MM-DD hh:mm:ss.sss (ssssss.sss)
* i.e. local time and master time as returned from vfwGetUnsynchronizedTime().
*
* When compiled with -DVFW_TEST_SYSTEM the function prints to stderr.
* If not compiled with -DVFW_TEST_SYSTEM the function prints to stderr and syslog.
*
* \param [in] condition The condition to assert.
* \param [in] sourceFileInfo Information about the caller, such as filename.
* \param [in] lineNumber The line number from where this function was called.
* \param [in] functionName The name of the calling function.
* \param [in] format A printf style format specifier.
*
* \note  Use macro VFW_ASSERT()
*        rather than calling this function directly.
*/
/*lint -esym(960,16.1) MISRA 2004 Required Rule 16.1, function has variable number of arguments
* The benefit of having a flexible and simple way to report diagnostics when terminating outweighs
* the problems associated with variable number of argument semantics. */
void vfwAssert(const char* condition, const char* sourceFileInfo, const integer_t lineNumber, 
 const char* functionName, const char* format, ...)
 /*lint +esym(960,16.1) */
{
  printf("Assertion '%s' failed, file %s, line %d\n", condition, sourceFileInfo, lineNumber);
  exit(1);
}

/**
* \brief Format an error string
*
* The function is intended for internal use.
*
* \param [in] format A printf style format specifier.
* \return The formatted error string.
*/
/*lint -esym(960,16.1) MISRA 2004 Required Rule 16.1, function has variable number of arguments */
char *vfwBuildErrStr(const char* format, ...)
 /*lint +esym(960,16.1) */
{
  static char         errorString[1000];
  va_list             argp;

  va_start(argp, format);     /*lint !e530 concequence from variable arguments */

  (void)vsnprintf(errorString, sizeof errorString, format, argp);

  va_end(argp);               /*lint !e950 concequence from variable arguments */

  return errorString;
}

int32_t             vfwApiVersionMajor(void)
{
  return VFW_API_VERSION_MAJOR;
}

int32_t             vfwApiVersionMinor(void)
{
  return VFW_API_VERSION_MINOR;
}

const char* vfwVersion(void)
{
  return "1.3";
}

void vfwCalcBlockedCrc64(const VFW_Buffer* inbuf, VFW_Buffer* crcbuf, uint32_t blockSize)
{
  uint32_t bytesLeftToCrc = vfwGetValidSize(inbuf);
  const bool isOK = (inbuf  != (VFW_Buffer*)(NULL)) && (bytesLeftToCrc > 0U)  && 
                    (crcbuf != (VFW_Buffer*)(NULL)) && (blockSize != 0U);

  VFW_ASSERT((isOK), (("vfwCalcBlockedCrc64: Illegal parameters")));

  const uint8_t* p = vfwGetPointer(inbuf);

  while (bytesLeftToCrc > 0U)
  {
    const uint32_t dataSizeForCrc = minimum(blockSize, bytesLeftToCrc);

    vfwPutU64(crcbuf, vfwCalcCrc64(p, dataSizeForCrc));

    p += dataSizeForCrc;

    bytesLeftToCrc -= dataSizeForCrc;
  }
}
