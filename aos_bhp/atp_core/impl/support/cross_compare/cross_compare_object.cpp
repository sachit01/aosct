/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  This file implements the abstract (core) brake component class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "cross_compare_object.hpp"


/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/
/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace Support
  {
    /******************************************************************************
    * CrossCompare constructor
    ******************************************************************************/
    CrossCompareObject::CrossCompareObject()
    : next_(static_cast<CrossCompareObject*>(NULL))
    {
    }

    /******************************************************************************
    * writeBuffer
    ******************************************************************************/
    void CrossCompareObject::writeBuffer(VFW_Buffer* const ownBuffer, const uint64_t data) const
    {
      vfwPutU64(ownBuffer, data);
    }

    /******************************************************************************
    * writeBuffer
    ******************************************************************************/
    void CrossCompareObject::writeBuffer(VFW_Buffer* const ownBuffer, const int64_t data) const
    {
      vfwPutI64(ownBuffer, data);
    }

    /******************************************************************************
    * writeBuffer
    ******************************************************************************/
    void CrossCompareObject::writeBuffer(VFW_Buffer* const ownBuffer, const uint32_t data) const
    {
      vfwPutU32(ownBuffer, data);
    }

    /******************************************************************************
    * writeBuffer
    ******************************************************************************/
    void CrossCompareObject::writeBuffer(VFW_Buffer* const ownBuffer, const int32_t data) const
    {
      vfwPutI32(ownBuffer, data);
    }

    /******************************************************************************
    * writeBuffer
    ******************************************************************************/
    void CrossCompareObject::writeBuffer(VFW_Buffer* const ownBuffer, const uint16_t data) const
    {
      vfwPutU16(ownBuffer, data);
    }

    /******************************************************************************
    * writeBuffer
    ******************************************************************************/
    void CrossCompareObject::writeBuffer(VFW_Buffer* const ownBuffer, const int16_t data) const
    {
      vfwPutI16(ownBuffer, data);
    }

    /******************************************************************************
    * writeBuffer
    ******************************************************************************/
    void CrossCompareObject::writeBuffer(VFW_Buffer* const ownBuffer, const uint8_t data) const
    {
      vfwPutU8(ownBuffer, data);
    }

    /******************************************************************************
    * writeBuffer
    ******************************************************************************/
    void CrossCompareObject::writeBuffer(VFW_Buffer* const ownBuffer, const int8_t data) const
    {
      vfwPutI8(ownBuffer, data);
    }

    /******************************************************************************
    * writeBuffer
    ******************************************************************************/
    void CrossCompareObject::writeBuffer(VFW_Buffer* const ownBuffer, const char_t data) const
    {
      vfwPutU8(ownBuffer, static_cast<uint8_t>(data));
    }

    /******************************************************************************
    * writeBuffer
    ******************************************************************************/
    void CrossCompareObject::writeBuffer(VFW_Buffer* const ownBuffer, const bool data) const
    {
      vfwPutU8(ownBuffer, data ? 0xFFU : 0x00U);
    }

    /******************************************************************************
    * getNext
    ******************************************************************************/
    const CrossCompareObject*
    CrossCompareObject::getNext() const
    {
      return next_; 
    }

    /******************************************************************************
    * CrossCompare destructor
    ******************************************************************************/
    CrossCompareObject::~CrossCompareObject()
    {
      next_ = static_cast<CrossCompareObject*>(NULL);
    }

    /******************************************************************************
    * setNext
    ******************************************************************************/
    void CrossCompareObject::setNext(CrossCompareObject* const endObject)
    {
      if (next_ != NULL)
      {
        ATC::aosHalt(__FILE__, __LINE__, "Object already added to Cross Compare!");
      }
      else
      {
        next_ = endObject;
      }
    }

    /******************************************************************************
    * CrossCompareSimpleType constructor
    ******************************************************************************/
    template<class T>
    CrossCompareSimpleType<T>::CrossCompareSimpleType(const T* pointer) :
      CrossCompareObject(),
      pointer_(pointer)
    {
      if (pointer_ == NULL)
      {
        ATC::aosHalt(__FILE__, __LINE__, "NULL Pointer used for pointer in simple type!");
      }
    }

    /******************************************************************************
    * writeDataBinary
    ******************************************************************************/
    template<class T>
    void
    CrossCompareSimpleType<T>::writeDataBinary(VFW_Buffer* const ownBuffer) const
    {
      const T* p = pointer_;
      writeBuffer(ownBuffer, *p);
    }

    /******************************************************************************
    * getMaxDataSize
    ******************************************************************************/
    template<class T>
    uint32_t
    CrossCompareSimpleType<T>::getMaxDataSize() const
    {
      return sizeof(T);
    }

    /******************************************************************************
    * CrossCompareSimpleType destructor
    ******************************************************************************/
    template<class T>
    CrossCompareSimpleType<T>::~CrossCompareSimpleType()
    {
      // To avoid compiler warning "non-virtual-dtor", destructor is empty, that should be OK since this class 
      // did not create the objects its pointers are pointing to. Deleting them should be someone elses responsibility.
    };

    // Instantiate the template classes used
    // By doing like this we avoid putting the implementation of the template in the header file.
    //lint -esym(751,ATP::Support::CrossCompareSimpleType*) Lint is wrong, these templates are used
    template class CrossCompareSimpleType<uint64_t>;
    template class CrossCompareSimpleType<int64_t>;
    template class CrossCompareSimpleType<uint32_t>;
    template class CrossCompareSimpleType<int32_t>;
    template class CrossCompareSimpleType<uint16_t>;
    template class CrossCompareSimpleType<int16_t>;
    template class CrossCompareSimpleType<uint8_t>;
    template class CrossCompareSimpleType<int8_t>;
    template class CrossCompareSimpleType<bool>;

  }  // namespace Support
} // namespace ATP
