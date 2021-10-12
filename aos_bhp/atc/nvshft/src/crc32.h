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
#include <stdint.h>

/******************************************************************************
* EXPORTED FUNCTIONS
******************************************************************************/

/**
* Initializes the CRC table. Must be called before @ref addToCrc32.
*/
extern void initCrc32(void);

/**
* Calculates CRC for the given line of text. May only be called after @ref initCrc32.
*
* @param[in]    The text line for which to calculate CRC
* @param[inout] The CRC value which will be updated with this calculation
*/
extern void addToCrc32(const char* const line, uint32_t* const crc);
