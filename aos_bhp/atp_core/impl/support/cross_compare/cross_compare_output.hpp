#ifndef CrossCompareOutput_hpp
#define CrossCompareOutput_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
* This file defines CrossCompare class which contains the core braking logic 
* used by the AOS. 
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-01-27    rquensel    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include <vfw_buffer.h>

/******************************************************************************
* DECLARATIONS    
******************************************************************************/


namespace ATP
{
  namespace Support
  {
    /**
    * Implements cross comparison of output data.
    */
    class CrossCompareOutput
    {
    public:

      /**
      * This is the function that commits the data if the cross compare succeeded.
      *
      */
      virtual void commit() = 0;

      /**
      * Return a description for the cross compare.
      *
      */
      virtual const char_t* getDescription() const = 0;

      /**
      * Move to the next message
      *
      */
      void useNextMessage();

      /**
      * Returns the next Cross Compare Object in the linked list
      *
      */
      CrossCompareOutput* getNext();

      /**
      * Writes a bool variable to the output data
      *
      * @param[in] val   Value to store
      */
      void putBoolean(const bool val);

      /**
      * Writes a variable to the output data
      *
      * @param[in] val   Value to store
      */
      void putUint64(const uint64_t val);

      /**
      * Writes a variable to the output data
      *
      * @param[in] val   Value to store
      */
      void putUint32(const uint32_t val);

      /**
      * Writes a variable to the output data
      *
      * @param[in] val   Value to store
      */
      void putUint16(const uint16_t val);

      /**
      * Writes a variable to the output data
      *
      * @param[in] val   Value to store
      */
      void putUint8(const uint8_t val);

      /**
      * Writes a variable to the output data
      *
      * @param[in] val   Value to store
      */
      void putInt32(const int32_t val);

      /**
      * Writes a variable to the output data
      *
      * @param[in] val   Value to store
      */
      void putInt16(const int16_t val);

      /**
      * Writes a variable to the output data
      *
      * @param[in] val   Value to store
      */
      void putInt8(const int8_t val);

      /**
      * Writes a buffer to the output data
      *
      * @param[in] buffer   Address to the buffer to write
      * @param[in] length   Number of bytes to copy
      */
      void putBuffer(const uint8_t* const buffer, const uint32_t length);


      /**
      * CrossCompareOutput destructor
      */
      virtual ~CrossCompareOutput();

      /**
      * setNext appends a Cross Compare object to the end of the linked list
      *
      * @param[in] endObject   Cross Compare object to append to the end of the linked list
      */
      void setNext(CrossCompareOutput* const endObject);

      /**
      * This is the function that writes data the provided buffer
      *
      * @param[in] ownBuffer   Buffer to write all cross compare data to
      */
      void writeDataBinary(VFW_Buffer* const ownBuffer) const;

      /**
      * This is the function clears all buffers
      *
      */
      void clearBuffers();

    protected:

      /**
      * Constructor
      */
      CrossCompareOutput();

      /**
      * init_        Initialize and create the buffers.
      *
      * @param [in] nMessages      Max number of messages.
      * @param [in] sizeMessages   Max size of a message.
      */
      void init_(const uint32_t nMessages, const uint32_t sizeMessages);

      /**
      * getBuffer_   Returns the buffer of a given index, NULL if no buffer is available.
      *      *
      * @param [in]  bufferIndex    Index of the buffer to get
      */
      VFW_Buffer* getBuffer_(const uint8_t bufferIndex) const;


      /**
      * getBuffer_   Calculates and returns the index to the current buffer to write to.
      *
      */
      uint8_t calculateCurrentMessageIndex_();

    private:
      
      /**
      * The absolute maximum number of messages written each cycle.
      *
      */
      static const uint8_t maxNumberOfMessages = 50U;
      
      /**
      * Internal pointer to the linked list of Cross Compare Objects
      */
      CrossCompareOutput* next_;

      /**
      * The maximum size of a buffer
      */
      uint32_t  bufferMaxSize_;

      /**
      * The maximum number of messages written each cycle. must be less or equal to maxNumberOfMessages_.
      */
      uint32_t   nMessages_;

      /**
      * Array to hold the buffers.
      */
      VFW_Buffer* vfwBuffers_[maxNumberOfMessages];

      /**
      * Array to hold the raw byte buffer data.
      */
      uint8_t* bufferData_[maxNumberOfMessages];

      /**
      * Number of messages we have written to.
      */
      uint8_t   currentMessageCount_;

      /**
      * Index to the current message
      */
      uint8_t   currentMessageIndex_;
    };
  }
}
#endif
