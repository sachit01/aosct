/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          vfw_buffer.cpp %
*
*  %version:       2 %
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

#include "stdafx.h"

#include "vfw_buffer.h"

extern "C"
{
  void vfwInitBuffer(VFW_Buffer* buffer, uint8_t* raw_buffer, uint32_t buffer_size)
  {
    // Simulate vfw behaviour
    if (buffer != NULL) {           // Can't test for "valid buffer" here, since it shouldn't be valid before inited... Just check its not null.
      buffer->b = raw_buffer;     //pointing to raw buffer
      buffer->o = 0;              //offset set to 0 
      buffer->b_s = buffer_size;  //set the buffer size
      buffer->p = NULL;           //Pointer to parent buffer
      buffer->m = BUFFER_WRITE_MODE;    //to check for mode 
      buffer->v = 0;              //No valid data to start with
    }
    else {
    }
  }

  void vfwValidateBuffer(const VFW_Buffer* const buffer)
  {
    //do nothing
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
    buffer->v = buffer_size;  //Set the Valid Size in Buffer
    buffer->o = 0;            //Reset offset
    buffer->m = BUFFER_READ_MODE;   //Change to read mode
  }

  /******************************************************************************
  * Function:     VfwCheckWrite
  * Description:  Wrapper functions for VFW calls in EMP/Class-D Layers.
  ******************************************************************************/
  bool VfwCheckWrite(VFW_Buffer *&buffer)
  {

    switch (buffer->m) {
    case BUFFER_READ_MODE:
      return false;
      break;
    case BUFFER_WRITE_MODE:
      if ((buffer->b_s <= buffer->o)) {
        return false;
      }
      else {
        return true;
      }
      break;
    default:
#ifdef DEBUG
      std::cout << "In default mode" << std::endl;
#endif
      return false;
      break;
    }
  }

  /******************************************************************************
  * Function:     vfwIsValidBuffer
  * Description:  Wrapper functions for VFW calls in EMP/Class-D Layers.
  ******************************************************************************/
  bool vfwIsValidBuffer(VFW_Buffer *&buffer)
  {
    bool statusBuff = false;
    if (buffer == NULL) {
      statusBuff = false;
    }
    else if (buffer->b == NULL) {
      statusBuff = false;
    }
    else if ((buffer->o < 0) || (buffer->b_s < buffer->o)) {
      statusBuff = false;
    }
    else {
      statusBuff = true;
    }
    return statusBuff;
  }

  /******************************************************************************
  * Function:     vfwPutU32
  * Description:  Wrapper functions for VFW calls in EMP/Class-D Layers.
  ******************************************************************************/
  void vfwPutU32(VFW_Buffer* buffer, uint32_t value)
  {

    if (vfwIsValidBuffer(buffer)) {
      uint32_t network_value = htonl(value);
      memmove(&buffer->b[buffer->o], reinterpret_cast<uint8_t *>(&network_value), static_cast<size_t>(sizeof(network_value)));
      buffer->o += (uint32_t) sizeof(network_value);
      if (buffer->v < buffer->o)
      {
        buffer->v = buffer->o;
      }
    }
    else
    {

    }
  }
 
  /******************************************************************************
  * Function:     vfwGetU32
  * Description:  Wrapper functions for VFW calls in EMP/Class-D Layers.
  ******************************************************************************/
  uint32_t vfwGetU32(VFW_Buffer* buffer)
  {
    uint32_t value, host_value;
    memmove(reinterpret_cast<uint8_t *>(&value), &buffer->b[buffer->o], static_cast<size_t>(sizeof(value)));
    host_value = ntohl(value);
    buffer->o += static_cast<uint32_t>(sizeof(value));
    return host_value;
  }

  /******************************************************************************
  * Function:     vfwPutU16
  * Description:  Wrapper functions for VFW calls in EMP/Class-D Layers.
  ******************************************************************************/
  void vfwPutU16(VFW_Buffer* buffer, uint16_t value)
  {
    if (vfwIsValidBuffer(buffer)) {
      uint16_t network_value = htons(value);
      memmove(&buffer->b[buffer->o], reinterpret_cast<uint8_t *>(&network_value), static_cast<size_t>(sizeof(network_value)));
      buffer->o += sizeof(network_value);
      if (buffer->v < buffer->o)
      {
        buffer->v = buffer->o;
      }
    }
    else
    {

    }
  }

  /******************************************************************************
  * Function:     vfwGetU16
  * Description:  Wrapper functions for VFW calls in EMP/Class-D Layers.
  ******************************************************************************/
  uint16_t vfwGetU16(VFW_Buffer* buffer)
  {
    uint16_t value, host_value;
    if (vfwIsValidBuffer(buffer))
    {
      memmove(reinterpret_cast<uint8_t *>(&value), &buffer->b[buffer->o], static_cast<size_t>(sizeof(value)));
      host_value = ntohs(value);
      buffer->o += sizeof(value);
    }
    else
    {
      host_value = 0U;
    }
    return host_value;
  }

  /******************************************************************************
  * Function:     vfwPutU8
  * Description:  Wrapper functions for VFW calls in EMP/Class-D Layers.
  ******************************************************************************/
  void vfwPutU8(VFW_Buffer* buffer, uint8_t value)
  {
    if (vfwIsValidBuffer(buffer) && VfwCheckWrite(buffer)) {
      uint8_t network_value = value;
      memmove(&buffer->b[buffer->o], static_cast<uint8_t *>(&network_value), static_cast<size_t>(sizeof(network_value)));
      buffer->o += sizeof(network_value);
      if (buffer->v < buffer->o)
      {
        buffer->v = buffer->o;
      }
    }
    else
    {

    }
  }

  /******************************************************************************
  * Function:     vfwGetU8
  * Description:  Wrapper functions for VFW calls in EMP/Class-D Layers.
  ******************************************************************************/
  uint8_t vfwGetU8(VFW_Buffer* buffer)
  {
    uint8_t value, host_value;
    memmove(static_cast<uint8_t *>(&value), &buffer->b[buffer->o], static_cast<size_t>(sizeof(value)));
    host_value = value;
    buffer->o += sizeof(value);
    return host_value;
  }

  /******************************************************************************
  * Function:     vfwCpyFromRawBuffer
  * Description:  Wrapper functions for VFW calls in EMP/Class-D Layers.
  ******************************************************************************/
  void vfwCpyFromRawBuffer(VFW_Buffer *  buffer, const void * raw_buffer, uint32_t  size)
  {
    memmove(&buffer->b[buffer->o], raw_buffer, static_cast<size_t>(size));

    buffer->o += size;
  }

  /******************************************************************************
  * Function:     vfwCpyToRawBuffer
  * Description:  Wrapper functions for VFW calls in EMP/Class-D Layers.
  ******************************************************************************/
  void vfwCpyToRawBuffer(void *  raw_buffer, VFW_Buffer *  buffer, uint32_t  size)
  {
    memmove(raw_buffer, &buffer->b[buffer->o], static_cast<size_t>(size));
    buffer->o += size;
  }

}