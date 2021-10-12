#ifndef CrossCompareObject_hpp
#define CrossCompareObject_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
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
* 2016-12-06    rquensel    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "atc_util.hpp"
#include <vfw_buffer.h>

/******************************************************************************
* DECLARATIONS    
******************************************************************************/


namespace ATP
{
  namespace Support
  {
    /**
    * Abstract base class for implementing cross comparison.
    */
    class CrossCompareObject
    {
    public:

      /**
      * This is the function that writes data the provided Cross Compare buffer
      *
      * @param[in] ownBuffer   Buffer to write all cross compare data to
      */
      virtual void writeDataBinary(VFW_Buffer* const ownBuffer) const = 0;

      /**
      * Returns the maximum number of bytes that could be written by this object
      *
      */
      virtual uint32_t getMaxDataSize() const = 0;

      /**
      * Returns the next Cross Compare Object in the linked list
      *
      */
      const CrossCompareObject* getNext() const;

      /**
      * CrossCompareObject destructor
      */
      virtual ~CrossCompareObject();

      /**
      * setNext appends a Cross Compare object to the end of the linked list
      *
      * @param[in] endObject   Cross Compare object to append to the end of the linked list
      */
      void
      setNext(CrossCompareObject* const endObject);

    protected:
      /**
      * putBoolean constructor
      */
      CrossCompareObject();

      /**
      * writeBuffer writes a uint64_t to the provided VFW_Buffer
      *
      * @param[in] ownBuffer Buffer to write data to
      * @param[in] data      Value to write to the cross compare buffer
      */
      void writeBuffer(VFW_Buffer* const ownBuffer, const uint64_t data) const;

      /**
      * writeBuffer writes a int64_t to the provided VFW_Buffer
      *
      * @param[in] ownBuffer Buffer to write data to
      * @param[in] data      Value to write to the cross compare buffer
      */
      void writeBuffer(VFW_Buffer* const ownBuffer, const int64_t data) const;

      /**
      * writeBuffer writes a uint32_t to the provided VFW_Buffer
      *
      * @param[in] ownBuffer Buffer to write data to
      * @param[in] data      Value to write to the cross compare buffer
      */
      void writeBuffer(VFW_Buffer* const ownBuffer, const uint32_t data) const;

      /**
      * writeBuffer writes a int32_t to the provided VFW_Buffer
      *
      * @param[in] ownBuffer Buffer to write data to
      * @param[in] data      Value to write to the cross compare buffer
      */
      void writeBuffer(VFW_Buffer* const ownBuffer, const int32_t data) const;

      /**
      * writeBuffer writes a uint16_t to the provided VFW_Buffer
      *
      * @param[in] ownBuffer Buffer to write data to
      * @param[in] data      Value to write to the cross compare buffer
      */
      void writeBuffer(VFW_Buffer* const ownBuffer, const uint16_t data) const;

      /**
      * writeBuffer writes a int16_t to the provided VFW_Buffer
      *
      * @param[in] ownBuffer Buffer to write data to
      * @param[in] data      Value to write to the cross compare buffer
      */
      void writeBuffer(VFW_Buffer* const ownBuffer, const int16_t data) const;

      /**
      * writeBuffer writes a uint8_t to the provided VFW_Buffer
      *
      * @param[in] ownBuffer Buffer to write data to
      * @param[in] data      Value to write to the cross compare buffer
      */
      void writeBuffer(VFW_Buffer* const ownBuffer, const uint8_t data) const;

      /**
      * writeBuffer writes a int8_t to the provided VFW_Buffer
      *
      * @param[in] ownBuffer Buffer to write data to
      * @param[in] data      Value to write to the cross compare buffer
      */
      void writeBuffer(VFW_Buffer* const ownBuffer, const int8_t data) const;

      /**
      * writeBuffer writes a char_t to the provided VFW_Buffer
      *
      * @param[in] ownBuffer Buffer to write data to
      * @param[in] data      Value to write to the cross compare buffer
      */
      void writeBuffer(VFW_Buffer* const ownBuffer, const char_t data) const;

      /**
      * writeBuffer writes a bool variable to the provided VFW_Buffer
      *
      * @param[in] ownBuffer Buffer to write data to
      * @param[in] data      Value to write to the cross compare buffer
      */
      void writeBuffer(VFW_Buffer* ownBuffer, const bool data) const;

    private:
      /**
      * Internal pointer to the linked list of Cross Compare Objects
      */
      CrossCompareObject* next_;

    };

    /**
    * CrossCompare template class for simple types
    */
    template<class T>
    class CrossCompareSimpleType : public CrossCompareObject
    {
    public:

      /**
      * Constructor
      *
      * @param[in] pointer   Pointer to the attribute to Cross Compare
      */
      CrossCompareSimpleType(const T* pointer = static_cast<const T* const>(NULL));

      /**
      * Puts the value into the compare buffer.
      * @param ownBuffer points to the cross compare buffer
      */
      virtual void writeDataBinary(VFW_Buffer* const ownBuffer) const;

      /**
      * Returns the maximum number of bytes that could be written by this object.
      *
      */
      virtual uint32_t getMaxDataSize() const;

    private:

      /**
      * Internal pointer to store the address of the value to be cross compared
      *
      */
      const T* const pointer_;

      /**
      *  Avoid unintentional copy-construction
      */
      CrossCompareSimpleType(const CrossCompareSimpleType&);

      /**
      *  Avoid unintentional assignment
      */
      CrossCompareSimpleType& operator=(const CrossCompareSimpleType&);

      /**
      *  Destructor
      */
      virtual ~CrossCompareSimpleType();
    };

    /**
    * CrossCompareUint64 class
    */
    typedef CrossCompareSimpleType<uint64_t> CrossCompareUint64;

    /**
    * CrossCompareInt64 class
    */
    typedef CrossCompareSimpleType<int64_t>  CrossCompareInt64;

    /**
    * CrossCompareUint32 class
    */
    typedef CrossCompareSimpleType<uint32_t> CrossCompareUint32;

    /**
    * CrossCompareInt32 class
    */
    typedef CrossCompareSimpleType<int32_t>  CrossCompareInt32;

    /**
    * CrossCompareUint16 class
    */
    typedef CrossCompareSimpleType<uint16_t> CrossCompareUint16;

    /**
    * CrossCompareInt16 class
    */
    typedef CrossCompareSimpleType<int16_t>  CrossCompareInt16;

    /**
    * CrossCompareUint8 class
    */
    typedef CrossCompareSimpleType<uint8_t> CrossCompareUint8;

    /**
    * CrossCompareInt8 class
    */
    typedef CrossCompareSimpleType<int8_t>  CrossCompareInt8;

    /**
    * CrossCompareBool class
    */
    typedef CrossCompareSimpleType<bool>  CrossCompareBool;


    /**
    * CrossCompare data type enum.
    */
    template<class T>
    class CrossCompareEnum : public CrossCompareObject
    {
    public:

      /**
      * Constructor
      *
      * @param[in] pointer   Pointer to the attribute to Cross Compare
      */
      CrossCompareEnum(const T* pointer = static_cast<const T* const>(NULL));

      /**
      * Puts the value into the compare buffer.
      * @param ownBuffer points to the cross compare buffer
      */
      virtual void writeDataBinary(VFW_Buffer* const ownBuffer) const;

      /**
      * Returns the maximum number of bytes that could be written by this object, 4 in this case.
      *
      */
      virtual uint32_t getMaxDataSize() const;

    private:

      /**
      * Internal pointer to store the address of the value to be cross compared
      *
      */
      const T* const pointer_;

      /**
      *  Avoid unintentional copy-construction
      */
      CrossCompareEnum(const CrossCompareEnum&);

      /**
      *  Avoid unintentional assignment
      */
      CrossCompareEnum& operator=(const CrossCompareEnum&);

      /**
      *  Destructor
      */
      virtual ~CrossCompareEnum();
    };


    /******************************************************************************
    * CrossCompareEnum constructor
    ******************************************************************************/
    template<class T>
    CrossCompareEnum<T>::CrossCompareEnum(const T* const pointer) :
      CrossCompareObject(),
      pointer_(pointer)
    {
      if (pointer_ == NULL)
      {
        ATC::aosHalt(__FILE__, __LINE__, "NULL Pointer used for pointer in enum");
      }
    };

    /******************************************************************************
    * CrossCompareEnum destructor
    ******************************************************************************/
    template<class T>
    CrossCompareEnum<T>::~CrossCompareEnum()
    {
    };


    /******************************************************************************
    * writeDataBinary
    ******************************************************************************/
    //Template classes needs to be defined in header file
    template<class T>
    void
    CrossCompareEnum<T>::writeDataBinary(VFW_Buffer* const ownBuffer) const
    {
      vfwPutU32(ownBuffer, static_cast<uint32_t>(*pointer_));
    }

    /******************************************************************************
    * getMaxDataSize
    ******************************************************************************/
    template<class T>
    uint32_t
    CrossCompareEnum<T>::getMaxDataSize() const
    {
      return 4;
    }
  }

}
#endif
