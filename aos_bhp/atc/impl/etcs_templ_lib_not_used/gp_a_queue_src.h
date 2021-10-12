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
* %name: gp_a_queue_src.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:17 %
* %date_created: 2014-09-23 13:17 %
* %Creation date of original object: Wed Jul 15 08:40:13 1998 %
*
* Description: The file contain the implementation of class 
*              GP_QueueA.
*
*******************************************************************************/

/*******************************************************************************
* Revision History
*
* Version  Date    Sign     Change description
*
*          151222  bhermans Removed some ATPCU-dependencies
*
* 5        131220  wahmad   merger of version 3.1.1 and 4
* 3.1.1    131220  wahmad   constant get methods added in order to get the reference of private
*	              						members to be used in cross-comparison.
* 4        131119  konuddj  WP008:Crosscompare in SPLH [6157]
* 3        130422  konuddj  Removed warning [task 4165]. 
* 2        130402  konuddj  Remove Sap parameter in profibus subscription [task 3894]. 
*-----------------------------------------------------------------
*  4       070504  rquensel WP60 Task 18196: GP has unnecessary instance method for error handler.
*
*---------------------------------------------------------------
*
*   4      011114  konraqu Changed code according to review.
*   3      011004  konraqu Added maxSize to iterator template parameter
*                           (in order to make a friend declaration of queue).
*
----------------------------------------------------------------------------
*
* 10       000828  konjph  Updated acc. to changed in error info (KONSCN88).
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
* 4        980811  konjko  Use GP_QueueIteratorA<...> instead of the typedef
*                          iterator to satisfy GNU.
*
* 3        980715  konjko  Added copy constructor and operator =.
*                          Change of dummy returns when error occured.
*                          Added maximumSize() function.
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
//#include "ATP_FailureClassA.hpp"
//#include "ATP_ErrorLevelA.hpp"



/*******************************************************************************
* Local macro definitions
*******************************************************************************/




/*******************************************************************************
* Class variables/Constants
*******************************************************************************/


//  GP_QueueA::GP_QueueA(int errorOffset)
//-----------------------------------------------------------------------------
//

template <class T, const int16_t maxSize, class F>
GP_QueueA<T, maxSize, F>::GP_QueueA(
  int uiqueErrorOffset) 
  :
  nElements_(0),
  head_(0), 
  tail_(0),
  errorOffset_(uiqueErrorOffset)
{ 
}

template<class T, const int16_t maxSize, class F> 
void 
GP_QueueA<T, maxSize, F>::init() 
{
  for( int32 i= 0; i< (maxSize+1); ++i )
  {
    bs_memzeroA( &(queue_[i]), sizeof(T) );
  }
}


template<class T, const int16_t maxSize, class F> 
const T& 
GP_QueueA<T, maxSize, F>::getItem(const uint32_t index) const 
{
  if( index < maxSize+1)
  {
    return queue_[index];
  }
  else
  {
    return queue_[0];
  }
}


//  GP_QueueA::GP_QueueA(const GP_QueueA& arg, int errorOffset)
//-----------------------------------------------------------------------------
//

template <class T, const int16_t maxSize, class F>
GP_QueueA<T, maxSize, F>::GP_QueueA( 
  const GP_QueueA& arg)
  : 
  nElements_(0),
  head_(0), 
  tail_(0),
  errorOffset_(arg.errorOffset_)
{ 
  const_iterator iter = arg.begin();

  while (iter != arg.end())
  {
    push(*iter);
    ++iter;
  }
}




//  GP_QueueA::empty()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
bool 
GP_QueueA<T, maxSize, F>::empty() 
  const
{
  return (nElements_ == 0) ? trueA : falseA;
}




//  GP_QueueA::full()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
bool 
GP_QueueA<T, maxSize, F>::full() 
  const
{
  return (nElements_ == maxSize) ? trueA : falseA;
}




//  GP_QueueA::size()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
int16_t
GP_QueueA<T, maxSize, F>::size() 
  const
{
  return nElements_;
}




//  GP_QueueA::maximumSize()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
int16_t
GP_QueueA<T, maxSize, F>::maximumSize() 
  const
{
  return maxSize;
}




//  GP_QueueA::begin()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
GP_QueueIteratorA<T, T&, maxSize, GP_QueueA<T, maxSize, F>*, F>
GP_QueueA<T, maxSize, F>::begin()
{
  
  return (iterator(this, 0));
}




//  GP_QueueA::begin()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
GP_QueueIteratorA<T, const T&, maxSize, const GP_QueueA<T, maxSize, F>*, F>
GP_QueueA<T, maxSize, F>::begin()
  const
{
  return (const_iterator(this, 0)); 
}




//  GP_QueueA::end()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
GP_QueueIteratorA<T, T&, maxSize, GP_QueueA<T, maxSize, F>*, F>
GP_QueueA<T, maxSize, F>::end()
{
  return (iterator(this, nElements_)); 
}




//  GP_QueueA::end()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
GP_QueueIteratorA<T, const T&, maxSize, const GP_QueueA<T, maxSize, F>*, F>
GP_QueueA<T, maxSize, F>::end()
  const
{
  return (const_iterator(this, nElements_)); 
}




//  GP_QueueA::front()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
T&
GP_QueueA<T, maxSize, F>::front()
{
  int16_t frontPos = head_;

  if (nElements_ == 0)
  {
    F::report( "Empty on get front!");

    frontPos = 0;
  }

  return(queue_[frontPos]);
}




//  GP_QueueA::front()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
const T& 
GP_QueueA<T, maxSize, F>::front()
  const
{
  int16_t frontPos = head_;

  if (nElements_ == 0)
  {
    F::report( "Empty on get front!");

    frontPos = 0;
  }

  return(queue_[frontPos]);
}




//  GP_QueueA::back()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
T&
GP_QueueA<T, maxSize, F>::back()
{
  int16_t backPos = (int16_t)(tail_ == 0  ? maxSize : (tail_-1));

  if (nElements_ == 0)
  {
    F::report( "Empty on get back!");

    backPos = 0;
  }

  return queue_[backPos];
}




//  GP_QueueA::back()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
const T&
GP_QueueA<T, maxSize, F>::back()
  const
{
  int16_t backPos = (int16_t)(tail_ == 0  ? maxSize : (tail_-1));

  if (nElements_ == 0)
  {
    F::report( "Empty on get back!");

    backPos = 0;
  }

  return queue_[backPos];
}




//  GP_QueueA::errorOffset()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
int
GP_QueueA<T, maxSize, F>::errorOffset()
  const
{
  return errorOffset_;
}



//  GP_QueueA::clear()
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F> 
inline
void 
GP_QueueA<T, maxSize, F>::clear()
{
  nElements_ = 0;
  head_ = 0;
  tail_ = 0;
}




//  GP_QueueA::push(T& element)
//-----------------------------------------------------------------------------
//

template <class T, const int16_t maxSize, class F>
inline
void 
GP_QueueA<T, maxSize, F>::push(
  const T& element)
{
  if(nElements_ != maxSize)
  {
    queue_[tail_] = element;

    ++tail_;
    if (tail_ == maxSize + 1)
    {
      tail_ = 0;
    }

    ++nElements_;
  }
  else
  {
    F::report( "Queue full!");

  }

}




//  GP_QueueA::pop()
//-----------------------------------------------------------------------------
//

template <class T, const int16_t maxSize, class F>
inline
void 
GP_QueueA<T, maxSize, F>::pop()
{
  if (nElements_ != 0)
  {
    ++head_;
    if (head_ == maxSize + 1)
    {
      head_ = 0;
    }

    --nElements_;
  }
  else
  {
    F::report( "Empty on pop" );

  }
}




//  GP_QueueA::operator =(GP_QueueA& arg)
//-----------------------------------------------------------------------------
//

template<class T, const int16_t maxSize, class F>  
GP_QueueA<T, maxSize, F>&
GP_QueueA<T, maxSize, F>::operator =(
  const GP_QueueA<T, maxSize, F>& arg)
{
  const_iterator iter = arg.begin();

  clear();

  while (iter != arg.end())
  {
    push(*iter);
    ++iter;
  }

  return *this;
}


template <class T, const int16_t maxSize, class F> 
const int16_t&
GP_QueueA<T, maxSize, F>::get_nElements()
const
{
  return nElements_;
}

template <class T, const int16_t maxSize, class F>
const int16_t&
GP_QueueA<T, maxSize, F>::get_head()
const
{
  return head_;
}

template <class T, const int16_t maxSize, class F>
const int16_t&
GP_QueueA<T, maxSize, F>::get_tail()
const
{
  return tail_;
}

template <class T, const int16_t maxSize, class F>
const int&
GP_QueueA<T, maxSize, F>::get_errorOffset()
const
{
  return errorOffset_;
}


/*************************** end of file **************************************/
