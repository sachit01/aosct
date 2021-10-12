/******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2020
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Implementation of AOS NVSHFT
*
******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "crc32.h"

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/
static uint32_t crcTable[256];

/******************************************************************************
* EXPORTED FUNCTIONS
******************************************************************************/

void initCrc32(void)
{
  uint32_t remainder;
  uint32_t byte;
  uint32_t bit;

  for (byte = 0U; byte < 256U; ++byte)
  {
    remainder = byte << 24;

    for (bit = 0U; bit < 8U; ++bit)
    {
      if (remainder > 0x7FFFFFFFU)
      {
        remainder = (remainder << 1U) ^ 0x4A503DF1U;
      }
      else
      {
        remainder = (remainder << 1U);
      }
    }

    crcTable[byte] = remainder;
  }
}

void addToCrc32(const char* const line, uint32_t* const crc)
{
  const uint8_t* ptr;

  for (ptr = (const uint8_t*) line; *ptr; ++ptr)
  {
    if (*ptr == '#')
    {
      break;
    }

    if ((*ptr != ' ') && (*ptr != '\t'))
    {
      *crc = crcTable[*ptr ^ (uint8_t) (*crc >> 24)] ^ (*crc << 8);
    }
  }
}
