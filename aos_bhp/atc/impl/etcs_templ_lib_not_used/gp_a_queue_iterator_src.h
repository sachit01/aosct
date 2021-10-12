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
* %name: gp_a_queue_iterator_src.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:17 %
* %Creation date of original object: Wed Jul 15 08:40:59 1998 %
*
* Description: The file contain the implementation of class 
*              GP_QueueIteratorA.
*
*******************************************************************************/

/*******************************************************************************
* Revision History
*
* Version  Date    Sign       Change description
*
*          151222  bhermans   Removed some ATPCU-dependencies
*
*  eca_3   070504  rquensel WP60 Task 18196: GP has unnecessary instance method for error handler.
*
*---------------------------------------------------------------
*
*   4      011003  konraqu Removed postfix ++/--, removed defualt constructor,
*                          added maxSize to iterator template parameter
*                          (in order to make a friend declaration of queue),
*                          changed list calls to directly use list attributes.
*
------------------------------------------------------------------------------
*
* 12       000828  konjph  Updated acc. to changed in error info (KONSCN88).
*
* 11       000608  konjko  Changed conditions in ++, -- and * operators and 
*                          constructor.
*
* 10       000607  konjko  Added precondition in operator*(). Added error in
*                          constructor.
*
* 9        000111  konraqu Added #include "ba_types.h" (for NULL). 
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
*                          Change in ++ and -- to make out of bound check 
*                          correct.
*
* 4        980811  konjko  Use GP_QueueIteratorA<...> instead of the typedef
*                          iterator to satisfy GNU.
*
* 3        980715  konjko  The templates takes a queue pointer argument. 
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
//#include "ATP_FailureClassA.hpp"
//#include "ATP_ErrorLevelA.hpp"

/*******************************************************************************
* Local macro definitions
*******************************************************************************/




/*******************************************************************************
* Class variables/Constants
*******************************************************************************/


//  GP_QueueIteratorA::GP_QueueIteratorA(QPtr queue, int16_t startPos)
//-----------------------------------------------------------------------------
//

template<class T, class TRef, const int16_t maxSize, class QPtr, class F>
GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>::GP_QueueIteratorA(
  QPtr queue, 
  int16_t startPos)
  :
  queue_(queue),
  current_(startPos),
  errorOffset_(0)
{
  errorOffset_ = queue->errorOffset_;

  if (current_ < 0  ||  current_ > queue_->nElements_) // nElements_ == size()
  {
    F::report( GP_QI_OUTSIDE_QUEUE_A,
                    "",
                    __LINE__);
  }
}




//  GP_QueueIteratorA::operator *()
//-----------------------------------------------------------------------------
//

template<class T, class TRef, const int16_t maxSize, class QPtr, class F>
inline
TRef
GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>::operator *() 
{
  if(current_ == queue_->nElements_) // *this == queue_.end()
  {
    F::report( GP_QI_EMPTY_QUEUE_A,
                    "",
                    __LINE__);
    
    return(queue_->queue_[0]);    // Return illegal data
  }

  return(queue_->queue_[(current_ + queue_->head_)  % 
                        (maxSize + 1)]);
}




//  GP_QueueIteratorA::operator *()
//-----------------------------------------------------------------------------
//

/*     gcc2.6 remove 
template<class T, class TRef, const int16_t maxSize, class QPtr, class F>
TRef
GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>::operator *() 
  const
{
  if (queue_ == NULL)
  {
    F::report( GP_QI_NOT_INITIALISED_A,
                    "",
                    __LINE__);


    return (queue_->queue_[0]);    // Return illegal data
  }
  if(current_ == queue_->nElements_) // *this == queue_.end()
  {
    F::report( GP_QI_EMPTY_QUEUE_A,
                    "",
                    __LINE__);
    
    return(queue_->queue_[0]);    // Return illegal data
  }

  return(queue_->queue_[(queue_->head_ + current_)  %
                        (maxSize + 1)]);
}
*/



//  GP_QueueIteratorA::operator ==(GP_QueueIteratorA arg)
//-----------------------------------------------------------------------------
//
  
template<class T, class TRef, const int16_t maxSize, class QPtr, class F> 
inline
bool
GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>::operator ==(
  const GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>& arg) 
  const
{
  return  (current_ == arg.current_  ?  trueA : falseA);
}




//  GP_QueueIteratorA::operator !=(GP_QueueIteratorA arg)
//-----------------------------------------------------------------------------
//

template<class T, class TRef, const int16_t maxSize, class QPtr, class F>
inline
bool
GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>::operator !=(
  const GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>& arg) 
  const
{
  return  ((current_ != arg.current_) ?  trueA : falseA);
}




//  GP_QueueIteratorA::isEnd()
//-----------------------------------------------------------------------------
//
template<class T, class TRef, const int16_t maxSize, class QPtr, class F>
inline
bool
GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>::isEnd() const
{
  return current_ == queue_->nElements_ ? trueA : falseA;
}




//  GP_QueueIteratorA::operator ++()
//-----------------------------------------------------------------------------
//

template<class T, class TRef, const int16_t maxSize, class QPtr, class F>
inline
GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>&
GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>::operator ++()
{
  if (current_ >= queue_->nElements_) // nElements_ == size()
  {
    F::report( GP_QI_OUTSIDE_QUEUE_A,
                    "",
                    __LINE__);


    return (*this);    // Return illegal data
  }

  ++current_;

  return *this;
}




//  GP_QueueIteratorA::operator --()
//-----------------------------------------------------------------------------
//

template<class T, class TRef, const int16_t maxSize, class QPtr, class F>
inline
GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>&
GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>::operator --()
{
  if (current_ <= 0)
  {
    F::report( GP_QI_OUTSIDE_QUEUE_A,
                    "",
                    __LINE__);


    return (*this);    // Return illegal data
  }
  
  --current_;

  return *this;
}




//  GP_QueueIteratorA::operator =(GP_QueueIteratorA& arg)
//-----------------------------------------------------------------------------
//

template<class T, class TRef, const int16_t maxSize, class QPtr, class F>
inline
GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>&
GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>::operator =(
  const GP_QueueIteratorA<T, TRef, maxSize, QPtr, F>& arg)
{
  current_ = arg.current_;
  queue_ = arg.queue_;

  errorOffset_ = queue_->errorOffset_;

  return (*this);
}


/*************************** end of file **************************************/
