#ifndef GPListIteratorSrc_hpp
#define GPListIteratorSrc_hpp

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of the GPListIterator class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-07    marlundg    Created ((Adapted from gp_a_list_iterator_src.h)
* 2017-01-25    spandita    Updated the -- operator
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
  GPListIterator<T, maxSize, TRef, LPtr, F>::GPListIterator(LPtr list, uint16_t startPos) : current_(startPos), list_(list)
  {
  }

  /******************************************************************************
  * operator *
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  inline TRef GPListIterator<T, maxSize, TRef, LPtr, F>::operator *()
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
  inline bool GPListIterator<T, maxSize, TRef, LPtr, F>::operator ==(const GPListIterator<T, maxSize, TRef, LPtr, F>& arg) const
  {
    return  (current_ == arg.current_ ? true : false);
  }

  /******************************************************************************
  * operator !=
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  inline bool GPListIterator<T, maxSize, TRef, LPtr, F>::operator !=(const GPListIterator<T, maxSize, TRef, LPtr, F>& arg) const
  {
    return  ((current_ != arg.current_) ? true : false);
  }

  /******************************************************************************
  * operator ++
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  inline GPListIterator<T, maxSize, TRef, LPtr, F>& GPListIterator<T, maxSize, TRef, LPtr, F>::operator ++()
  {
    if (current_ != maxSize)
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
  * operator --
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  inline GPListIterator<T, maxSize, TRef, LPtr, F> &GPListIterator<T, maxSize, TRef, LPtr, F>::operator --()
  {
    if (list_->listIndexPrev_[current_] != maxSize)
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
  * operator =
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  inline GPListIterator<T, maxSize, TRef, LPtr, F> &GPListIterator<T, maxSize, TRef, LPtr, F>::operator =(
    const GPListIterator<T, maxSize, TRef, LPtr, F>& arg)
  {
    current_ = arg.current_;
    list_ = arg.list_;

    return (*this);
  }

  /******************************************************************************
  * operator =
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  inline GPListIterator<T, maxSize, TRef, LPtr, F> &GPListIterator<T, maxSize, TRef, LPtr, F>::operator =(
    const GPListRevIterator<T, maxSize, TRef, LPtr, F>& arg)
  {
    current_ = arg.getCurrentIndex();
    list_ = arg.getItrList();

    return (*this);
  }

  /******************************************************************************
  * getCurrentIndex
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  uint16_t GPListIterator<T, maxSize, TRef, LPtr, F>::getCurrentIndex() const
  {
    return current_;
  }

  /******************************************************************************
  * getItrList
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  const LPtr&  GPListIterator<T, maxSize, TRef, LPtr, F>::getItrList() const
  {
    return list_;
  }

}

#endif

/*************************** end of file **************************************/
