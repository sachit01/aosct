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
* %name: gp_a_list_iterator_src.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:17 %
* %Creation date of original object: Wed Jul 15 08:39:17 1998 %
*
* Description: The file contain the implementation of class 
*              GP_ListIteratorA.
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
*
*  4       070504  rquensel WP60 Task 18196: GP has unnecessary instance method for error handler.
*  3       060915  sigbjal Changed error code in operator *(). CR 3035, task 14498.
*
*---------------------------------------------------------------
*
* 10       020221  konraqu Removed default constructor, NCR SBB BAS369.
*  9       010925  konraqu Added isEnd method.
*  8       010817  konraqu Removed operator --(int), ++(int), changed op--.
*
*  7       010816  konraqu Changed compile flags.
*
*  6       010810  konraqu Rewrote code, now it also checks if iterators are 
*                          used on a modified list (only if _DEBUG).
*
* -----------------------------------------------------------------------------
*
* 11       010104  konfrsk Updated the TRef-constructor. 
*
* 10       001213  konfrsk Added new constructor (to take the TRef argument)
*                          according to SDS 1.18. Implemented KONHBD25 too. 
*
* 11       000828  konjph  Updated acc. to changed in error info (KONSCN88).
*
* 10       000515  konjko  More inlines.
*
* 9        000509  konraqu Optimized using inline.
*
* 8        990510  konblg  Removed __FILE__ macro.
*
* 7        981111  konjko  Changed error_ to a pointer.
*                          Return something else than illegalDummy_.
*                          operator *() const excluded to satisfy gcc v2.6.
*                          Inlines on most used functions.
*
* 6        981013  konjko  Remived all inline.
*
* 5        980915  konjko  Implemented error codes using errorOffset_
*
* 4        980811  konjko  Use GP_ListIteratorA<...> instead of the typedef
*                          iterator to satisfy GNU.
*
* 3        980715  konjko  The templates takes a list pointer argument. 
*                          maxSize removed from template arguments.
*                          operator = returns a reference to this.
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

//#include "ba_types.h"

//#include "ATP_FailureClassA.hpp"
//#include "ATP_ErrorLevelA.hpp"

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


//  GP_ListIteratorA::GP_ListIteratorA(LPtr list, GP_LinkA<T>* startPos)
//-----------------------------------------------------------------------------
//

/*template<class T, class TRef, class LPtr, class F>
GP_ListIteratorA<T, TRef, LPtr, F>::GP_ListIteratorA(
  LPtr list, 
  GP_LinkA<T>* startPos)
  :
  list_(list),
  current_(startPos),
  error_ (F::instance ()),
  errorOffset_(0)
{
  if (list != NULL)
  {
    errorOffset_ = list->errorOffset();
  }
}
*/

//  GP_ListIteratorA::GP_ListIteratorA(LPtr list, int16A startPos)
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
GP_ListIteratorA<T, maxSize, TRef, LPtr, F>::GP_ListIteratorA(
  LPtr list, 
  uint16_t startPos)
  :
  current_(startPos),
#ifdef GP_DEBUG_COUNTER
  modCount_(list->modCount_),
#endif
  list_(list),
  errorOffset_(list->errorOffset_)
{
}




// Member functions
//-----------------------------------------------------------------------------
//

//  GP_ListIteratorA::operator *()
//-----------------------------------------------------------------------------
//
  
template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
inline_if_no_debug
TRef
GP_ListIteratorA<T, maxSize, TRef, LPtr, F>::operator *() 
{
#ifdef GP_DEBUG_COUNTER
  if (modCount_ != list_->modCount_)
  {
    F::report( GP_LI_NOT_INITIALISED_A,
                        "",
                        __LINE__);
  }
#endif

  if (current_ == maxSize)  // *this == list_.end()
  {
    F::report( "List index out of range!");
  }

  return list_->list_[current_];
}




//  GP_ListIteratorA::operator *()
//-----------------------------------------------------------------------------
//
  
/*     gcc2.6 remove 
template<class T, class TRef, class LPtr, class F>
inline_if_no_debug
TRef
GP_ListIteratorA<T, TRef, LPtr, F>::operator *() 
  const
{
  if (list_ == NULL)
  {
    F::report( GP_LI_NOT_INITIALISED_A,
                    "",
                    __LINE__);


    return (current_->element());    // Return illegal data
  }
  if (current_ == &(list_->endMarker_))  // *this == list_.end()
  {
    F::report( GP_LI_OUTSIDE_A,
                    "",
                    __LINE__);


    return (current_->element());    // Return illegal data
  }

  return(current_->element());
}
*/





//  GP_ListIteratorA::operator ==(GP_ListIteratorA arg)
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
inline_if_no_debug
bool
GP_ListIteratorA<T, maxSize, TRef, LPtr, F>::operator ==(
  const GP_ListIteratorA<T, maxSize, TRef, LPtr, F>& arg) 
  const
{
#ifdef GP_DEBUG_COUNTER
  if (current_  == maxSize)  // End should not be checked
  {
    if (arg.current_ != maxSize  &&        // Check if arg is not end
        arg.modCount_ != list_->modCount_) // Arg not end, check if modified
    {
      F::report( GP_LI_NOT_INITIALISED_A,
                          "",
                          __LINE__);
    }
  }
  else if (arg.current_  == maxSize)      // Is the arg end?
  {
    if (modCount_ != list_->modCount_)  // Check if modified
    {
      F::report( GP_LI_NOT_INITIALISED_A,
                          "",
                          __LINE__);
    }
  }
  else if (modCount_ != arg.modCount_    ||
           modCount_ != list_->modCount_  )  // Check if modified
  {
    F::report( GP_LI_NOT_INITIALISED_A,
                        "",
                        __LINE__);
  }
#endif

  return  (current_ == arg.current_  ?  trueA : falseA);
}




//  GP_ListIteratorA::operator !=(GP_ListIteratorA arg)
//-----------------------------------------------------------------------------
//
  
template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
inline_if_no_debug 
bool
GP_ListIteratorA<T, maxSize, TRef, LPtr, F>::operator !=(
  const GP_ListIteratorA<T, maxSize, TRef, LPtr, F>& arg) 
  const
{
#ifdef GP_DEBUG_COUNTER
  if (current_  == maxSize)  // End should not be checked
  {
    if (arg.current_ != maxSize  &&        // Check if arg is not end
        arg.modCount_ != list_->modCount_) // Arg not end, check if modified
    {
      F::report( GP_LI_NOT_INITIALISED_A,
                          "",
                          __LINE__);
    }
  }
  else if (arg.current_  == maxSize)      // Is the arg end?
  {
    if (modCount_ != list_->modCount_)  // Check if modified
    {
      F::report( GP_LI_NOT_INITIALISED_A,
                          "",
                          __LINE__);
    }
  }
  else if (modCount_ != arg.modCount_    ||
           modCount_ != list_->modCount_  )  // Check if modified
  {
    F::report( GP_LI_NOT_INITIALISED_A,
                        "",
                        __LINE__);
  }
#endif
  return  ((current_ != arg.current_) ?  trueA : falseA);
}





//  GP_ListIteratorA::errorOffset()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
inline_if_no_debug 
int 
GP_ListIteratorA<T, maxSize, TRef, LPtr, F>::errorOffset()
  const
{
  return errorOffset_;
}




//  GP_ListIteratorA::operator ++()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
inline_if_no_debug
GP_ListIteratorA<T, maxSize, TRef, LPtr, F>&
GP_ListIteratorA<T, maxSize, TRef, LPtr, F>::operator ++()
{
#ifdef GP_DEBUG_COUNTER
  if (list_  &&  modCount_ != list_->modCount_)
  {
    F::report( GP_LI_NOT_INITIALISED_A,
                        "",
                        __LINE__);
  }
#endif

  if (current_ != maxSize)
  {
    current_ = list_->listIndexNext_[current_];
  }
  else
  {
    F::report( "List index out of range");

//    return (*this);    // Return illegal data
  }

  return *this;
}




//  GP_ListIteratorA::operator --()
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
inline_if_no_debug 
GP_ListIteratorA<T, maxSize, TRef, LPtr, F>&
GP_ListIteratorA<T, maxSize, TRef, LPtr, F>::operator --()
{
#ifdef GP_DEBUG_COUNTER
  if (list_  &&  modCount_ != list_->modCount_)
  {
    F::report( GP_LI_NOT_INITIALISED_A,
                        "",
                        __LINE__);
  }
#endif

  if (list_->listIndexPrev_[current_] != maxSize)
  {
    current_ = list_->listIndexPrev_[current_];
  }
  else
  {
    F::report( GP_LI_OUTSIDE_A,
                    "",
                    __LINE__);
  }

  return *this;
}




//  GP_ListIteratorA::operator =(GP_ListIteratorA& arg)
//-----------------------------------------------------------------------------
//

template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
inline_if_no_debug
GP_ListIteratorA<T, maxSize, TRef, LPtr, F>&
GP_ListIteratorA<T, maxSize, TRef, LPtr, F>::operator =(
  const GP_ListIteratorA<T, maxSize, TRef, LPtr, F>& arg)
{
#ifdef GP_DEBUG_COUNTER
  modCount_ = arg.modCount_;
#endif

  current_ = arg.current_;
  list_ = arg.list_;
  errorOffset_ = arg.errorOffset_;

  return (*this);
}




template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
inline_if_no_debug
bool
GP_ListIteratorA<T, maxSize, TRef, LPtr, F>::isEnd() const
{
  return (current_ == maxSize) ? true : false;
}

/*************************** end of file **************************************/
