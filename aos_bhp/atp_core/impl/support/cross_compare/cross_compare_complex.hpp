#ifndef CrossCompareComplex_hpp
#define CrossCompareComplex_hpp
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
    * Template class for implementing cross comparison of objects of class T.
    */
    template<class T>
    class CrossCompareComplex : public CrossCompareObject
    {
    public:

      /**
      * Constructor
      *
      * @param[in] complexPointer   Pointer to the complex object to Cross Compare
      */
      CrossCompareComplex(const T* const complexPointer);

      /**
      * Writes the data to the provided Cross Compare buffer
      *
      * @param[in] ownBuffer   Buffer to write all cross compare data to
      */
      virtual void writeDataBinary(VFW_Buffer* const ownBuffer) const;

      /**
      * Returns the maximum number of bytes that could be written by this object.
      *
      */
      virtual uint32_t getMaxDataSize() const;

    private:
      /**
      *  Avoid unintentional default-construction
      */
      CrossCompareComplex();

      /**
      *  Avoid unintentional copy-construction
      */
      CrossCompareComplex(const CrossCompareComplex&);

      /**
      *  Avoid unintentional assignment
      */
      CrossCompareComplex& operator=(const CrossCompareComplex&);

      /**
      *  Destructor
      */
      virtual ~CrossCompareComplex();

      /**
      * Internal pointer to store the address of the complex object to be cross compared
      *
      */
      const T* const complexPointer_;
    };

    /******************************************************************************
    * CrossCompareComplex constructor
    ******************************************************************************/
    template<class T>
    CrossCompareComplex<T>::CrossCompareComplex(const T* const complexPointer) :
      CrossCompareObject(),
      complexPointer_(complexPointer)
    {
      if (complexPointer_ == static_cast<const T* const>(NULL))
      {
        ATC::aosHalt(__FILE__, __LINE__, "NULL Pointer used for pointer in Complex template!");
      }
    }

    /******************************************************************************
    * writeDataBinary
    ******************************************************************************/
    template<class T>
    void
    CrossCompareComplex<T>::writeDataBinary(VFW_Buffer* const ownBuffer) const
    {
      complexPointer_->writeCrossCompare(ownBuffer);
    }

    /******************************************************************************
    * getMaxDataSize
    ******************************************************************************/
    template<class T>
    uint32_t
    CrossCompareComplex<T>::getMaxDataSize() const
    {
      return complexPointer_->getWriteCrossCompareMaxSize();
    }

    /******************************************************************************
    * CrossCompareComplex destructor
    ******************************************************************************/
    template<class T>
    CrossCompareComplex<T>::~CrossCompareComplex()
    {
      // To avoid compiler warning "non-virtual-dtor", destructor is empty, that should be OK since this class 
      // did not create the objects its pointers are pointing to. Deleting them should be someone elses responsibility.
    };
  }
}
#endif
