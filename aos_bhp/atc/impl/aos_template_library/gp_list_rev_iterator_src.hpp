#ifndef GPListRevIteratorSrc_hpp
#define GPListRevIteratorSrc_hpp

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of the GPListRevIterator class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-09-11    spandita    created

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

namespace ATC
{

  /******************************************************************************
  * LOCAL FUNCTION PROTOTYPES
  ******************************************************************************/

  /******************************************************************************
  * Constructor
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  GPListRevIterator<T, maxSize, TRef, LPtr, F>::GPListRevIterator(LPtr list, uint16_t startPos) : current_(startPos), list_(list)
  {
  }

  /******************************************************************************
  * operator *
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  inline TRef GPListRevIterator<T, maxSize, TRef, LPtr, F>::operator *()
  {
    if (current_ == maxSize)
    {
      F::report("List index out of range");
    }

    return list_->list_[current_];
  }

  /******************************************************************************
  * operator ==
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  inline bool GPListRevIterator<T, maxSize, TRef, LPtr, F>::operator ==(const GPListRevIterator<T, maxSize, TRef, LPtr, F>& arg) const
  {
    return  (current_ == arg.current_ ? true : false);
  }

  /******************************************************************************
  * operator !=
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  inline bool GPListRevIterator<T, maxSize, TRef, LPtr, F>::operator !=(const GPListRevIterator<T, maxSize, TRef, LPtr, F>& arg) const
  {
    return  ((current_ != arg.current_) ? true : false);
  }

  /******************************************************************************
  * operator ++
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  inline GPListRevIterator<T, maxSize, TRef, LPtr, F>& GPListRevIterator<T, maxSize, TRef, LPtr, F>::operator ++()
  {
    if (current_ != maxSize)
    {
      current_ = list_->listIndexPrev_[current_];
    }
    else
    {
      F::report("List index out of range");
    }

    return *this;
  }

  /******************************************************************************
  * operator --
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  inline GPListRevIterator<T, maxSize, TRef, LPtr, F> &GPListRevIterator<T, maxSize, TRef, LPtr, F>::operator --()
  {
    if (list_->listIndexNext_[current_] != maxSize)
    {

      current_ = list_->listIndexNext_[current_];
    }
    else
    {
      F::report("List index out of range");
    }

    return *this;
  }

  /******************************************************************************
  * operator =
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  inline GPListRevIterator<T, maxSize, TRef, LPtr, F> &GPListRevIterator<T, maxSize, TRef, LPtr, F>::operator =(
    const GPListRevIterator<T, maxSize, TRef, LPtr, F>& arg)
  {
    current_ = arg.current_;
    list_ = arg.list_;

    return (*this);
  }

  /******************************************************************************
  * getCurrentIndex
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  uint16_t GPListRevIterator<T, maxSize, TRef, LPtr, F>::getCurrentIndex() const
  {
    return current_;
  }

  /******************************************************************************
  * getItrList
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  const LPtr&  GPListRevIterator<T, maxSize, TRef, LPtr, F>::getItrList() const
  {
    return list_;
  }

  /******************************************************************************
  * operator =
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  inline GPListRevIterator<T, maxSize, TRef, LPtr, F> &GPListRevIterator<T, maxSize, TRef, LPtr, F>::operator =(
    const GPListIterator<T, maxSize, TRef, LPtr, F>& arg)
  {
    current_ = arg.getCurrentIndex();
    list_ = arg.getItrList();
    return (*this);
  }
}

#endif

/*************************** end of file **************************************/
