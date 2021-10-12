/*******************************************************************************
*
* (C) COPYRIGHT ABB Daimler-Benz Transportation Signal AB, 1998
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without express authority is strictly forbidden.
*
* Module name: General Purpose SW Components
*
* %name: gp_a_list_src.h %
* %name: gp_a_list_src.h %
* %version: 1 %
* %date_created: 2014-09-23 13:17 %
* %Creation date of original object: Wed Jul 15 08:38:58 1998 %
*
* Description: The file contain the implementation of class 
*              GP_ListA.
*
*******************************************************************************/

/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
*
*  1.1.1   131219  wahmad   constant get methods added in order to get the reference of list private
*							members to be used in cross-comparison.
*--------------------------------------------------------------------------
*  2       070504  rquensel WP60 Task 18196: GP has unnecessary instance method for error handler.
*
*--------------------------------------------------------------------------
*
*  9       011019  konraqu Changed insert from void to return iterator.
*  8       010910  konraqu Added copy constructor.
*  7       010816  konraqu Removed the check if iterators are used, ATP uses
*                          modified iterators.
*
*  6       010810  konraqu Rewrote code, now it also checks if iterators are 
*                          used on a modified list (only if _DEBUG).
*                          If GP_DEBUG is defined internal checks are performed.
*
*  5       010730  konraqu Optimized clear and operator =.
*
*-----------------------------------------------------------------------------
*
* 17       000828  konjph  Updated acc. to changed in gp_error_info (KONSCN88).
*
* 16       000615  konjko  Changed return values if error in erase().
*
* 15       000615  konjko  Changed erase(), now it returns iterator to the 
*                          last remaining item, not last->next.
*                          
* 14       000606  konjko  Changed in end().
*
* 13       000515  konjko  New internal implementation.
*
* 12       000509  konraqu Optimized using inline.
*
* 11       000508  konraqu Optimized clear, operator= and some small changes
*                          in rest of code.
*
* 10       000327  konjko  Changed in erase() according to NCR SIGPEBO11.
*
* 9        990510  konblg  Removed __FILE__ macro.
*
* 8        981208  sigmsv  Removed error offset from copy constructor
*
* 7        981111  konjko  Changed error_ to a pointer.
*                          Return something else than illegalDummy_.
*                          Inlines on most used functions.
*
* 6        981013  konjko  Remived all inline.
*
* 5        980915  konjko  Implemented error codes using errorOffset_
*
* 4        980811  konjko  Use GP_ListIteratorA<...> instead of the typedef
*                          iterator to satisfy GNU.
*
* 3        980715  konjko  Change of initialisation of members endElement and
*                          beginElement. Added copy constructor and operator =.
*                          Change of dummy returns when error occured.
*                          Added maximumSize() function.
*                          The array index is no longer stored in the link,
*                          it is calculated with pointer offset, changes made
*                          in insert, erase, allocate_ and deAllocate_.
*
* 2        980624  konjko  all class members have ending underscore, error
*                          messages moved to separate file
*
* 1        980605  konjko  created
*
*******************************************************************************/




/*******************************************************************************
* Includes
*******************************************************************************/
//#include "vss_a_scalar_types.h"



/*******************************************************************************
* Local macro definitions
*******************************************************************************/

#ifdef _DEBUG
#define inline_if_no_debug
#else
#define inline_if_no_debug inline
#endif



/*******************************************************************************
* Class variables/Constants
*******************************************************************************/

//  GP_ListA::GP_ListA(int errorOffset)
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
GP_ListA<T, maxSize, F>::GP_ListA(
  int uniqueErrorOffset)
  : 
#ifdef GP_DEBUG_COUNTER
  modCount_(0),
#endif
  frontMarkerFree_(0),
  size_(0),
  errorOffset_(uniqueErrorOffset)
{
  for (uint16_t i = 0; i < maxSize; ++i)
  {
    listIndexNext_[i] = i + 1;
  }

  listIndexNext_[maxSize] = maxSize;
  listIndexPrev_[maxSize] = maxSize;

#ifdef GP_DEBUG_COUNTER
  ++modCount_;
#endif
#ifdef GP_DEBUG
  checkConsistency();
#endif

}

  
  
  
//  GP_ListA::GP_ListA(int errorOffset)
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
GP_ListA<T, maxSize, F>::GP_ListA(
  const GP_ListA& srcList)
  : 
#ifdef GP_DEBUG_COUNTER
  modCount_(0),
#endif
  frontMarkerFree_(0),
  size_(0),
  errorOffset_(srcList.errorOffset_)
{
  uint16_t i = 0;

  for (; i < maxSize; ++i)
  {
    listIndexNext_[i] = i + 1;
  }

  listIndexNext_[maxSize] = maxSize;
  listIndexPrev_[maxSize] = maxSize;

  if (srcList.size_ != 0)
  {
    uint16_t oldLast = maxSize;

    uint16_t iSrcIndex = srcList.listIndexNext_[maxSize];

    for (i = 0; i < srcList.size_; ++i)
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

#ifdef GP_DEBUG_COUNTER
  ++modCount_;
#endif
#ifdef GP_DEBUG
  checkConsistency();
#endif

}

  
  
  
//  GP_ListA::empty()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F> 
inline_if_no_debug
bool 
GP_ListA<T, maxSize, F>::empty() 
  const
{
  return size_ == 0 ? trueA : falseA;
}




//  GP_ListA::full()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
inline_if_no_debug
bool 
GP_ListA<T, maxSize, F>::full() 
  const
{
  return  size_ == maxSize ? trueA : falseA;
}




//  GP_ListA::size()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
inline_if_no_debug
uint16_t 
GP_ListA<T, maxSize, F>::size() 
  const
{
  return size_;
}




//  GP_ListA::maximumSize()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
inline_if_no_debug
uint16_t 
GP_ListA<T, maxSize, F>::maximumSize() 
  const
{
  return maxSize;
}




//  GP_ListA::begin()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
inline_if_no_debug
GP_ListIteratorA<T, maxSize, T&, GP_ListA<T, maxSize, F>*, F> 
GP_ListA<T, maxSize, F>::begin()
{
  return iterator(this, listIndexNext_[maxSize]);
}




//  GP_ListA::begin()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
inline_if_no_debug
GP_ListIteratorA<T, maxSize, const T&, const GP_ListA<T, maxSize, F>*, F>
GP_ListA<T, maxSize, F>::begin()
  const
{
  return const_iterator(this, listIndexNext_[maxSize]);
}




//  GP_ListA::end()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F> 
inline_if_no_debug
GP_ListIteratorA<T, maxSize, T&, GP_ListA<T, maxSize, F>*, F>
GP_ListA<T, maxSize, F>::end()
{
  return iterator(this, maxSize);
}




//  GP_ListA::end()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F> 
inline_if_no_debug
GP_ListIteratorA<T, maxSize, const T&, const GP_ListA<T, maxSize, F>*, F>
GP_ListA<T, maxSize, F>::end()
  const
{
  return const_iterator(this, maxSize);
}




//  GP_ListA::front()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
inline_if_no_debug
T& 
GP_ListA<T, maxSize, F>::front()
{
  if(size_ == 0)
  {
    F::report( GP_L_EMPTY_ON_GET_FRONT_A,
                    "",
                    __LINE__);
  }

  return list_[listIndexNext_[maxSize]];
}




//  GP_ListA::front()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
inline_if_no_debug
const T& 
GP_ListA<T, maxSize, F>::front()
  const
{
  if(size_ == 0)
  {
    F::report( GP_L_EMPTY_ON_GET_FRONT_A,
                    "",
                    __LINE__);
  }

  return list_[listIndexNext_[maxSize]];
}




//  GP_ListA::back()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
inline_if_no_debug
T& 
GP_ListA<T, maxSize, F>::back()
{
  if(size_ == 0)
  {
    F::report( GP_L_EMPTY_ON_GET_BACK_A,
                    "",
                    __LINE__);

  }

  return list_[listIndexPrev_[maxSize]];
}




//  GP_ListA::back()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
inline_if_no_debug
const T& 
GP_ListA<T, maxSize, F>::back()
  const
{
  if(size_ == 0)
  {
    F::report( GP_L_EMPTY_ON_GET_BACK_A,
                    "",
                    __LINE__);

  }

  return list_[listIndexPrev_[maxSize]];
}




//  GP_ListA::errorOffset()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
inline_if_no_debug
int 
GP_ListA<T, maxSize, F>::errorOffset()
  const
{
  return (errorOffset_);
}




//  GP_ListA::clear()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
inline_if_no_debug
void 
GP_ListA<T, maxSize, F>::clear()
{
  if (size_ != 0)
  {
    listIndexNext_[listIndexPrev_[maxSize]] = frontMarkerFree_;
    frontMarkerFree_ = listIndexNext_[maxSize];
    listIndexNext_[maxSize] = maxSize;
    listIndexPrev_[maxSize] = maxSize;

    size_ = 0;
  }

#ifdef GP_DEBUG_COUNTER
  ++modCount_;
#endif
#ifdef GP_DEBUG
  checkConsistency();
#endif
}



//  GP_ListA::pushFront(const T& element)
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F> 
inline_if_no_debug
void 
GP_ListA<T, maxSize, F>::pushFront(
  const T& element)
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
  else // List is full!!!
  {
    F::report( "Full on insert!");
  }
#ifdef GP_DEBUG_COUNTER
  ++modCount_;
#endif
#ifdef GP_DEBUG
  checkConsistency();
#endif
}




//  GP_ListA::popFront()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F> 
inline_if_no_debug
void 
GP_ListA<T, maxSize, F>::popFront()
{
  if (size_ != 0)  // Check if empty
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
    F::report( GP_L_EMPTY_ON_POP_A,
                    "",
                    __LINE__);
  }
#ifdef GP_DEBUG_COUNTER
  ++modCount_;
#endif
#ifdef GP_DEBUG
  checkConsistency();
#endif
}




//  GP_ListA::pushBack(const T& element)
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F> 
inline_if_no_debug
void 
GP_ListA<T, maxSize, F>::pushBack(
  const T& element)
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
  else // List is full!!!
  {
    F::report("List is full" );
  }
#ifdef GP_DEBUG_COUNTER
  ++modCount_;
#endif
#ifdef GP_DEBUG
  checkConsistency();
#endif
}




//  GP_ListA::popBack()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F> 
inline_if_no_debug
void 
GP_ListA<T, maxSize, F>::popBack()
{
  if (size_ != 0)  // Check if empty
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
    F::report( "Empty on pop!");
  }

#ifdef GP_DEBUG_COUNTER
  ++modCount_;
#endif

#ifdef GP_DEBUG
  checkConsistency();
#endif
}




//  GP_ListA::insert(iterator pos, const T& element)
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
inline_if_no_debug 
GP_ListIteratorA<T, maxSize, T&, GP_ListA<T, maxSize, F>*, F>
GP_ListA<T, maxSize, F>::insert(
  const GP_ListIteratorA<T, maxSize, T&, GP_ListA<T, maxSize, F>*, F>& pos,
  const T& element)
{
#ifdef GP_DEBUG_COUNTER
  if (pos.modCount_ != modCount_)
  {
    F::report( GP_L_NOT_IN_LIST_A,
                    "",
                    __LINE__);
  }
#endif

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
  else // List is full!!!
  {
    F::report( GP_L_FULL_ON_INSERT_A,
                    "",
                    __LINE__);
  }
#ifdef GP_DEBUG
  checkConsistency();
#endif
#ifdef GP_DEBUG_COUNTER
  ++modCount_;
#endif

  return (iterator(this, newIndex));
}




//  GP_ListA::erase(iterator pos)
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
inline_if_no_debug
GP_ListIteratorA<T, maxSize, T&, GP_ListA<T, maxSize, F>*, F>
GP_ListA<T, maxSize, F>::erase(
  const GP_ListIteratorA<T, maxSize, T&, GP_ListA<T, maxSize, F>*, F>& pos)
{
#ifdef GP_DEBUG_COUNTER
  if (pos.modCount_ != modCount_)
  {
    F::report( GP_L_NOT_IN_LIST_A,
                    "",
                    __LINE__);
  }
  ++modCount_;
#endif
  if (size_ != 0)  // Check if empty
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

#ifdef GP_DEBUG
  checkConsistency();
#endif

      return iterator(this, next);
    }

    //Error!!!
    F::report( GP_L_NOT_IN_LIST_A,
                    "",
                    __LINE__);

    return pos;
  }

  //Error!!!
  F::report( GP_L_EMPTY_ON_ERASE_A,
                    "",
                    __LINE__);

  return iterator(this, maxSize);
}




//  GP_ListA::erase(iterator first, iterator last)
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F>
GP_ListIteratorA<T, maxSize, T&, GP_ListA<T, maxSize, F>*, F>
GP_ListA<T, maxSize, F>::erase(
  const GP_ListIteratorA<T, maxSize, T&, GP_ListA<T, maxSize, F>*, F>& first, 
  const GP_ListIteratorA<T, maxSize, T&, GP_ListA<T, maxSize, F>*, F>& last)
{ 
//  iterator pos(first);
#ifdef GP_DEBUG_COUNTER
  if (first.modCount_ != modCount_  ||
      last.modCount_  != modCount_)
  {
    F::report( GP_L_NOT_IN_LIST_A,
                    "",
                    __LINE__);
  }
#endif

  if(size_ != 0)
  {
    if (first.current_ != last.current_ )  // Must remove more than one object
                                           // Last must not be outside list
    {
      uint16_t pos = first.current_;

      uint16_t delCount = 0;

      while (pos != last.current_  &&
             pos != maxSize)
      {
        pos = listIndexNext_[pos];
        ++delCount;
      }

      if (pos == last.current_)  // Last iteratot was found!
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
        F::report( GP_L_REVERSE_SET_A,
                                "",
                                __LINE__);
      }

    }
    else
    {
      //Error!!!
      F::report( GP_L_NOT_IN_LIST_A,
                      "",
                      __LINE__);

    }
  }
  else
  {
    F::report( GP_L_EMPTY_ON_ERASE_A,
                    "",
                    __LINE__);
  }

#ifdef GP_DEBUG
  checkConsistency();
#endif
#ifdef GP_DEBUG_COUNTER
  ++modCount_;
#endif
      
  return last;
}



//  GP_ListA::operator =(GP_ListA& arg)
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class F> 
GP_ListA<T, maxSize, F>&
GP_ListA<T, maxSize, F>::operator =(
  const GP_ListA<T, maxSize, F>& arg)
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

#ifdef GP_DEBUG
  checkConsistency();
#endif
#ifdef GP_DEBUG_COUNTER
  ++modCount_;
#endif
  return *this;
}



template<class T, const uint16_t maxSize, class F> 
const T& 
GP_ListA<T, maxSize, F>::getItem(const uint32_t index) const 
{
  if( index < maxSize + 1)
  {
    return list_[index];
  }
  else
  {
    return list_[0];
  }
}


template<class T, const uint16_t maxSize, class F> 
const uint16_t&
GP_ListA<T, maxSize, F>::get_frontMarkerFree()
const
{
	return frontMarkerFree_;
}

template<class T, const uint16_t maxSize, class F> 
const uint16_t&
GP_ListA<T, maxSize, F>::get_size()
const
{
	return size_;
}

template<class T, const uint16_t maxSize, class F> 
const uint16_t&
GP_ListA<T, maxSize, F>::get_listIndexPrev()
const
{
	return *listIndexPrev_;
}

template<class T, const uint16_t maxSize, class F> 
const uint16_t&
GP_ListA<T, maxSize, F>::get_listIndexNext()
const
{
	return *listIndexNext_;
}

template<class T, const uint16_t maxSize, class F> 
const int&
GP_ListA<T, maxSize, F>::get_errorOffset()
const
{
	return errorOffset_;
}

#ifdef GP_DEBUG

template<class T, const uint16_t maxSize, class F> 
void
GP_ListA<T, maxSize, F>::checkConsistency()
{
  // Check list forward ----------------------

  uint16_t i = listIndexNext_[maxSize];
  uint16_t size = 0;

  while (i != maxSize)
  {
    i = listIndexNext_[i];
    ++size;

    if (size > size_)
    {
      F::report( GP_L_NOT_IN_LIST_A,
                      "",
                      __LINE__);

      return;
    }
  }

  if (size != size_)
  {
    F::report( GP_L_NOT_IN_LIST_A,
                    "",
                    __LINE__);

  }

  // Check list backward ----------------------

  i = listIndexPrev_[maxSize];
  size = 0;

  while (i != maxSize)
  {
    i = listIndexPrev_[i];
    ++size;

    if (size > size_)
    {
      F::report( GP_L_NOT_IN_LIST_A,
                      "",
                      __LINE__);
      return;
    }
  }

  if (size != size_)
  {
    F::report( GP_L_NOT_IN_LIST_A,
                    "",
                    __LINE__);

  }


  // Check free list ----------------------

  i = frontMarkerFree_;

  uint16_t freeSize = maxSize - size_;

  size = 0;

  while (i != maxSize)
  {
    i = listIndexNext_[i];
    ++size;

    if (size > freeSize)
    {
      F::report( GP_L_NOT_IN_LIST_A,
                      "",
                      __LINE__);

      return;
    }
  }

  if (size != freeSize)
  {
    F::report( GP_L_NOT_IN_LIST_A,
                    "",
                    __LINE__);

  }

}

#endif // GP_DEBUG

/*************************** end of file **************************************/
