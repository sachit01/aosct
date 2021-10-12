#ifndef CrossCompareArray_hpp
#define CrossCompareArray_hpp
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
* 2018-01-15    rquensel    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "cross_compare_object.hpp"

/******************************************************************************
* DECLARATIONS    
******************************************************************************/

namespace ATP
{
  namespace Support
  {
    /**
    * Template class for implementing cross comparison of an array of objects of class T.
    */
    template<class T>
    class CrossCompareArray : public CrossCompareObject
    {
    public:

      /**
      * Constructor
      *
      * @param[in] arrayPointer Pointer to the array to be cross compared
      * @param[in] arraySize    The number of objects in the array
      */
      CrossCompareArray(const T* const arrayPointer, const uint16_t arraySize);

      /**
      * Writes the data to the provided Cross Compare buffer
      *
      * @param[in] ownBuffer   Buffer to write all cross compare data to
      */
      virtual void writeDataBinary(VFW_Buffer* const ownBuffer) const;

      /**
      * Returns the maximum number of bytes that could be written by this object, 1 in this case.
      *
      */
      virtual uint32_t getMaxDataSize() const;

    private:
      /**
      *  Avoid unintentional default-construction
      */
      CrossCompareArray();

      /**
      *  Avoid unintentional copy-construction
      */
      CrossCompareArray(const CrossCompareArray&);

      /**
      *  Avoid unintentional assignment
      */
      CrossCompareArray& operator=(const CrossCompareArray&);

      /**
      *  Destructor
      */
      virtual ~CrossCompareArray();

      /**
      * Internal pointer to store the address of the boolean to be cross compared
      *
      */
      const T* const arrayPointer_;

      /**
      * Size of the array
      *
      */
      const uint16_t arraySize_;
    };

    /******************************************************************************
    * CrossCompareArray constructor
    ******************************************************************************/
    template<class T>
    CrossCompareArray<T>::CrossCompareArray(const T* const arrayPointer, const uint16_t arraySize) :
      CrossCompareObject(),
      arrayPointer_(arrayPointer),
      arraySize_(arraySize)
    {
      if (arrayPointer_ == static_cast<const T* const>(NULL))
      {
        ATC::aosHalt(__FILE__, __LINE__, "NULL Pointer used for pointer in array template!");
      }
    }

    /******************************************************************************
    * writeDataBinary
    ******************************************************************************/
    template<class T>
    void
    CrossCompareArray<T>::writeDataBinary(VFW_Buffer* const ownBuffer) const
    {
      const T* p = arrayPointer_;

      // Loop through the array and write all data...
      for (uint16_t i = 0U; i < arraySize_; ++i)
      {
        writeBuffer(ownBuffer, *p);
        ++p;
      }
    }

    /******************************************************************************
    * getMaxDataSize
    ******************************************************************************/
    template<class T>
    uint32_t
    CrossCompareArray<T>::getMaxDataSize() const
    {
      return arraySize_ * sizeof(T);
    }

    /******************************************************************************
    * CrossCompareArray destructor
    ******************************************************************************/
    template<class T>
    CrossCompareArray<T>::~CrossCompareArray()
    {
      // To avoid compiler warning "non-virtual-dtor", destructor is empty, that should be OK since this class 
      // did not create the objects its pointers are pointing to. Deleting them should be someone elses responsibility.
    };
  }
}
#endif
