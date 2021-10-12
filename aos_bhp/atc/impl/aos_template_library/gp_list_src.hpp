#ifndef GPListSrc_hpp
#define GPListSrc_hpp

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of the GPList class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-07    marlundg    Created ((Adapted from gp_a_list_src.h)
* 2017-01-21    spandita    Created the reverse functionality
* 2017-01-25    spandita     Updated with rbegin and rend functionalities
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
  template<class T, const uint16_t maxSize, class F>
  GPList<T, maxSize, F>::GPList() : frontMarkerFree_(0), size_(0)
  {

    for (uint16_t i = 0U; i < maxSize; ++i)
    {
      listIndexNext_[i] = i + 1U;
    }
    listIndexNext_[maxSize] = maxSize;
    listIndexPrev_[maxSize] = maxSize;

  }

  /******************************************************************************
  * Copy Constructor
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  GPList<T, maxSize, F>::GPList(const GPList& srcList) : frontMarkerFree_(0), size_(0)
  {
    uint16_t i = 0U;
    for (; i < maxSize; ++i)
    {
      listIndexNext_[i] = i + 1U;
    }

    listIndexNext_[maxSize] = maxSize;
    listIndexPrev_[maxSize] = maxSize;

    if (srcList.size_ != 0U)
    {
      uint16_t oldLast = maxSize;

      uint16_t iSrcIndex = srcList.listIndexNext_[maxSize];

      for (i = 0U; i < srcList.size_; ++i)
      {
        uint16_t last = frontMarkerFree_;
        frontMarkerFree_ = listIndexNext_[frontMarkerFree_];

        list_[last] = srcList.list_[iSrcIndex];

        iSrcIndex = srcList.listIndexNext_[iSrcIndex];

        listIndexPrev_[last] = oldLast;
        listIndexNext_[oldLast] = last;

        oldLast = last;
      }

      listIndexNext_[oldLast] = maxSize;
      listIndexPrev_[maxSize] = oldLast;

      size_ = srcList.size_;
    }
  }

  /******************************************************************************
  * empty()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline bool GPList<T, maxSize, F>::empty() const
  {
    return size_ == 0U ? true : false;
  }

  /******************************************************************************
  * full()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline bool GPList<T, maxSize, F>::full() const
  {
    return  size_ == maxSize ? true : false;
  }

  /******************************************************************************
  * size()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline uint16_t GPList<T, maxSize, F>::size() const
  {
    return size_;
  }

  /******************************************************************************
  * size() const
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline uint16_t GPList<T, maxSize, F>::maximumSize() const
  {
    return maxSize;
  }

  /******************************************************************************
  * begin()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline GPListIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F> GPList<T, maxSize, F>::begin()
  {
    return iterator(this, listIndexNext_[maxSize]);
  }

  /******************************************************************************
  * begin() const
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline GPListIterator<T, maxSize, const T&, const GPList<T, maxSize, F>*, F> GPList<T, maxSize, F>::begin() const
  {
    return const_iterator(this, listIndexNext_[maxSize]);
  }

  /******************************************************************************
  * end()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline GPListIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F> GPList<T, maxSize, F>::end()
  {
    return iterator(this, maxSize);
  }

  /******************************************************************************
  * end() const
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline GPListIterator<T, maxSize, const T&, const GPList<T, maxSize, F>*, F> GPList<T, maxSize, F>::end() const
  {
    return const_iterator(this, maxSize);
  }

  /******************************************************************************
  * front()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline T& GPList<T, maxSize, F>::front()
  {
    if (size_ == 0U)
    {
      F::report("Empty list when using front()");
    }

    return list_[listIndexNext_[maxSize]];
  }

  /******************************************************************************
  * front() const
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline const T& GPList<T, maxSize, F>::front() const
  {
    if (size_ == 0U)
    {
      F::report("Empty list when using front() const");
    }

    return list_[listIndexNext_[maxSize]];
  }

  /******************************************************************************
  * back()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline T& GPList<T, maxSize, F>::back()
  {
    if (size_ == 0U)
    {
      F::report("Empty list when using back()");
    }

    return list_[listIndexPrev_[maxSize]];
  }

  /******************************************************************************
  * back() const
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline const T& GPList<T, maxSize, F>::back() const
  {
    if (size_ == 0U)
    {
      F::report("Empty list when using back() const");
    }

    return list_[listIndexPrev_[maxSize]];
  }

  /******************************************************************************
  * clear()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline void GPList<T, maxSize, F>::clear()
  {
    if (size_ != 0U)
    {
      listIndexNext_[listIndexPrev_[maxSize]] = frontMarkerFree_;
      frontMarkerFree_ = listIndexNext_[maxSize];
      listIndexNext_[maxSize] = maxSize;
      listIndexPrev_[maxSize] = maxSize;

      size_ = 0U;
    }
  }

  /******************************************************************************
  * pushFront()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline void GPList<T, maxSize, F>::pushFront(const T& element)
  {
    if (size_ != maxSize)  // Check if full
    {
      uint16_t newIndex = frontMarkerFree_;
      frontMarkerFree_ = listIndexNext_[frontMarkerFree_];

      uint16_t oldFirst = listIndexNext_[maxSize];
      listIndexPrev_[oldFirst] = newIndex;
      listIndexNext_[newIndex] = oldFirst;
      listIndexPrev_[newIndex] = maxSize;
      listIndexNext_[maxSize] = newIndex;

      list_[newIndex] = element;

      ++size_;
    }
    else
    {
      F::report("Full list when using pushFront()");
    }
  }

  /******************************************************************************
  * popFront()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline void GPList<T, maxSize, F>::popFront()
  {
    if (size_ != 0U)  // Check if empty
    {
      uint16_t remIndex = listIndexNext_[maxSize];

      uint16_t newFirst = listIndexNext_[remIndex];

      listIndexPrev_[newFirst] = maxSize;
      listIndexNext_[maxSize] = newFirst;

      listIndexNext_[remIndex] = frontMarkerFree_;
      frontMarkerFree_ = remIndex;

      --size_;
    }
    else
    {
      F::report("Empty list when using popFront()");
    }
  }

  /******************************************************************************
  * pushBack()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline void GPList<T, maxSize, F>::pushBack(const T& element)
  {
    if (size_ != maxSize)  // Check if full
    {
      uint16_t newIndex = frontMarkerFree_;
      frontMarkerFree_ = listIndexNext_[frontMarkerFree_];

      uint16_t oldLast = listIndexPrev_[maxSize];
      listIndexNext_[oldLast] = newIndex;
      listIndexPrev_[newIndex] = oldLast;
      listIndexNext_[newIndex] = maxSize;
      listIndexPrev_[maxSize] = newIndex;

      list_[newIndex] = element;

      ++size_;
    }
    else
    {
      F::report("Full list when using pushBack()");
    }
  }

  /******************************************************************************
  * popBack()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline void GPList<T, maxSize, F>::popBack()
  {
    if (size_ != 0U)  // Check if empty
    {
      uint16_t remIndex = listIndexPrev_[maxSize];

      uint16_t newLast = listIndexPrev_[remIndex];

      listIndexNext_[newLast] = maxSize;
      listIndexPrev_[maxSize] = newLast;

      listIndexNext_[remIndex] = frontMarkerFree_;
      frontMarkerFree_ = remIndex;

      --size_;
    }
    else
    {
      F::report("Empty list when using popBack()");
    }
  }

  /******************************************************************************
  * insert()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline GPListIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F> GPList<T, maxSize, F>::insert(
    const GPListIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F>& pos, const T& element)
  {
    uint16_t newIndex = pos.current_;

    if (size_ != maxSize)  // Check if full
    {
      newIndex = frontMarkerFree_;
      frontMarkerFree_ = listIndexNext_[frontMarkerFree_];

      uint16_t oldPrev = listIndexPrev_[pos.current_];

      listIndexPrev_[pos.current_] = newIndex;
      listIndexNext_[newIndex] = pos.current_;

      listIndexNext_[oldPrev] = newIndex;
      listIndexPrev_[newIndex] = oldPrev;

      list_[newIndex] = element;

      ++size_;
    }
    else
    {
      F::report("Full list when using insert()");
    }
    return (iterator(this, newIndex));
  }

  /******************************************************************************
  * erase()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline GPListIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F> GPList<T, maxSize, F>::erase(
    const GPListIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F>& pos)
  {
    if (size_ != 0U)  // Check if empty
    {
      if (pos.current_ != maxSize)
      {
        uint16_t remIndex = pos.current_;

        uint16_t next = listIndexNext_[remIndex];
        uint16_t prev = listIndexPrev_[remIndex];

        listIndexNext_[prev] = next;
        listIndexPrev_[next] = prev;

        listIndexNext_[remIndex] = frontMarkerFree_;
        frontMarkerFree_ = remIndex;

        --size_;

        return iterator(this, next);
      }

      F::report("Element not in list using erase()");
    }

    F::report("Empty list when using erase()");

    return iterator(this, maxSize);
  }

  /******************************************************************************
  * erase()
  *
  * Erase a a range of elements.
  *
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  GPListIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F> GPList<T, maxSize, F>::erase(
    const GPListIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F>& first,
    const GPListIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F>& last)
  {
    if (size_ != 0U)
    {
      if (first.current_ != last.current_)  // Must remove more than one object
                                            // Last must not be outside list
      {
        uint16_t pos = first.current_;

        uint16_t delCount = 0U;

        while (pos != last.current_  &&
          pos != maxSize)
        {
          pos = listIndexNext_[pos];
          ++delCount;
        }

        if (pos == last.current_)  // Last iterator was found
        {
          // Link in removed elements into free list
          listIndexNext_[listIndexPrev_[pos]] = frontMarkerFree_;
          frontMarkerFree_ = first.current_;

          // Fix prev and next linkage
          uint16_t prev = listIndexPrev_[first.current_];
          listIndexNext_[prev] = pos;
          listIndexPrev_[pos] = prev;

          size_ -= delCount;

        }
        else
        {
          F::report("Inverted iterators when using erase()");
        }
      }
      else
      {
        F::report("Element not in list when using erase()");
      }
    }
    else
    {
      F::report("Empty list when using erase()");
    }

    return last;
  }

  /******************************************************************************
  * operator =
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  GPList<T, maxSize, F>& GPList<T, maxSize, F>::operator =(const GPList<T, maxSize, F>& arg)
  {
    clear();

    if (arg.size_ != 0)
    {
      uint16_t oldLast = maxSize;

      uint16_t iSrcIndex = arg.listIndexNext_[maxSize];

      for (uint16_t i = 0; i < arg.size_; ++i)
      {
        uint16_t last = frontMarkerFree_;
        frontMarkerFree_ = listIndexNext_[frontMarkerFree_];

        list_[last] = arg.list_[iSrcIndex];

        iSrcIndex = arg.listIndexNext_[iSrcIndex];

        listIndexPrev_[last] = oldLast;
        listIndexNext_[oldLast] = last;

        oldLast = last;
      }

      listIndexNext_[oldLast] = maxSize;
      listIndexPrev_[maxSize] = oldLast;

      size_ = arg.size_;
    }

    return *this;
  }

  /******************************************************************************
  * reverse
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  void GPList<T, maxSize, F>::reverse()
  {
    if (size_ != 0U)  // Check if empty
    {
      //Get the first element index
      uint16_t val = listIndexNext_[maxSize];
      //iterate till number of current elements
      for (uint16_t i = 0U; i < size_; i++)
      {
        uint16_t temp = listIndexNext_[val];
        //Swap the previous element with next element and vice versa
        listIndexNext_[val] = listIndexNext_[val] + listIndexPrev_[val];
        listIndexPrev_[val] = listIndexNext_[val] - listIndexPrev_[val];
        listIndexNext_[val] = listIndexNext_[val] - listIndexPrev_[val];
        //Get the next element
        val = temp;
      }
      uint16_t temp2 = listIndexNext_[maxSize];
      listIndexNext_[maxSize] = listIndexPrev_[maxSize];
      listIndexPrev_[maxSize] = temp2;
    }
    else
    {
      F::report("Empty list when using reverse()");
    }
  }

  /******************************************************************************
  * rbegin()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline GPListRevIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F> GPList<T, maxSize, F>::rbegin()
  {
    return reverse_iterator(this, listIndexPrev_[maxSize]);
  }

  /******************************************************************************
  * rbegin()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline GPListRevIterator<T, maxSize, const T&, const GPList<T, maxSize, F>*, F> GPList<T, maxSize, F>::rbegin() const
  {
    return const_reverse_iterator(this, listIndexPrev_[maxSize]);
  }



  /******************************************************************************
  * rend()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline GPListRevIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F> GPList<T, maxSize, F>::rend()
  {
    return reverse_iterator(this, maxSize);
  }

  /******************************************************************************
  * rend()
  ******************************************************************************/
  template<class T, const uint16_t maxSize, class F>
  inline GPListRevIterator<T, maxSize, const T&, const GPList<T, maxSize, F>*, F> GPList<T, maxSize, F>::rend() const
  {
    return const_reverse_iterator(this, maxSize);
  }


}

#endif

/*************************** end of file **************************************/
