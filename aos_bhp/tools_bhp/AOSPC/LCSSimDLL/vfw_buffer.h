#pragma once

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          vfw_buffer.h %
*
*  %version:       3 %
*
*  %created_by:    nsyed %
*
*  %date_created:  2017-05-03 17:03 %
*
*  DESCRIPTION:
*  Wrapper functions for VFW calls in EMP/Class-D Layers.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-02-15    Marlundg	File created
*
*******************************************************************************/

#include "atc_types.hpp"

/******************************************************************************
* Description:  Wrapper functions/types for VFW calls in EMP/Class-D Layers.
******************************************************************************/

const uint32_t BUFFER_READ_MODE = 0U;
const uint32_t BUFFER_WRITE_MODE = 1U;

typedef struct VFW_BufferNode *VFW_Bufferp;

typedef struct VFW_BufferNode {
  VFW_Bufferp p; /**< Pointer to parent buffer, shall not be accessed directly by VFW_Buffer users */
  uint8_t* b; /**< Pointer to data, shall not be accessed directly by VFW_Buffer users */
  uint32_t b_s; /**< Data size, shall not be accessed directly by VFW_Buffer users */
  uint32_t o; /**< Offset, shall not be accessed directly by VFW_Buffer users */
  uint32_t v; /**< Size of valid data, shall not be accessed directly by VFW_Buffer users */
  uint32_t m; /**< Mode, shall not be accessed directly by VFW_Buffer users */
} VFW_Buffer;

typedef enum {
  /** Output parameters are valid */
  VFW_BUFFER_OK,

  /** Reading outside buffer */
  VFW_BUFFER_READING_OUTSIDE,

  /** Raw buffer too small*/
  VFW_BUFFER_RAW_BUFFER_TOO_SMALL
} VFW_BufferStatus;

extern "C"
{
  void      vfwInitBuffer(VFW_Buffer* buffer, uint8_t* raw_buffer, uint32_t buffer_size);
  bool      vfwIsValidBuffer(VFW_Buffer *&buffer);
  void      vfwPutU32(VFW_Buffer* buffer, uint32_t value);
  uint32_t  vfwGetU32(VFW_Buffer* buffer);
  void      vfwPutU16(VFW_Buffer* buffer, uint16_t value);
  uint16_t  vfwGetU16(VFW_Buffer* buffer);
  void      vfwPutU8(VFW_Buffer* buffer, uint8_t value);
  uint8_t   vfwGetU8(VFW_Buffer* buffer);
  void      vfwCpyFromRawBuffer(VFW_Buffer* buffer, const void* raw_buffer, uint32_t size);
  void      vfwCpyToRawBuffer(void* raw_buffer, VFW_Buffer* buffer, uint32_t size);
  void      vfwSetReadBuffer(VFW_Buffer* buffer, uint32_t buffer_size);
}