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
* %name: gp_a_stack_iterator_src.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:18 %
* %Creation date of original object: Wed Jul 15 08:41:31 1998 %
*
* Description: The file contain the implementation of class 
*              GP_StackIteratorA.
*
*******************************************************************************/

/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
*
*  2       070504  rquensel WP60 Task 18196: GP has unnecessary instance method for error handler.
*
*---------------------------------------------------------------
*
*   4      011003  konraqu Removed postfix ++/--, removed defualt constructor,
*                          added maxSize to iterator template parameter
*                          (in order to make a friend declaration of stack),
*                          changed list calls to directly use list attributes.
*
------------------------------------------------------------------------------
*
* 13       000829  konraqu Fixed copy-paste bug...
*
* 12       000828  konjph  Updated acc. to changed in error info (KONSCN88).
*
* 11       000608  konjko  Changed conditions in ++ and -- operators.
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
*
* 6        981013  konjko  Removed all inline.
*
* 5        980915  konjko  Implemented error codes using errorOffset_
*
* 4        980811  konjko  Use GP_StackIteratorA<...> instead of the typedef
*                          iterator to satisfy GNU.
*
* 3        980715  konjko  The templates takes a stack pointer argument. 
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


/*******************************************************************************
* Local macro definitions
*******************************************************************************/




/*******************************************************************************
* Class variables/Constants
*******************************************************************************/


//  GP_StackIteratorA::GP_StackIteratorA(SPtr stack, int16A startPos)
//-----------------------------------------------------------------------------
//

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
GP_StackIteratorA<T, TRef, maxSize, SPtr, F>::GP_StackIteratorA( 
  SPtr stack, 
  int16A startPos)
  :
  current_(startPos),
  stack_(stack),
  errorOffset_(0)
{
  errorOffset_ = stack->errorOffset_;

  if (current_ < 0  ||  current_ > stack_->head_)
  {
    F::report( GP_SI_OUTSIDE_STACK_A,
                    "",
                    __LINE__);
  }
}

  
  
  
//  GP_StackIteratorA::operator *()
//-----------------------------------------------------------------------------
//
  
template<class T, class TRef, const int16A maxSize, class SPtr, class F>
inline
TRef
GP_StackIteratorA<T, TRef, maxSize, SPtr, F>::operator *()
{
  if(current_ == 0) // *this == stack_.end()
  {
    F::report( GP_SI_EMPTY_STACK_A,
                    "",
                    __LINE__);
    
    return(stack_->stack_[0]);    // Return illegal data
  }

  return(stack_->stack_[current_]);
}




//  GP_StackIteratorA::operator *()
//-----------------------------------------------------------------------------
//

/*     gcc2.6 remove 
template<class T, class TRef, const int16A maxSize, class SPtr, class F>
TRef
GP_StackIteratorA<T, TRef, maxSize, SPtr, F>::operator *() 
  const
{
  if (stack_ == NULL)
  {
    F::report( v,
                    "",
                    __LINE__);


    return (stack_->stack_[0]);    // Return illegal data
  }
  if(current_ == 0) // *this == stack_.end()
  {
    F::report( GP_SI_EMPTY_STACK_A,
                    "",
                    __LINE__);
    
    return(stack_->stack_[0]);    // Return illegal data
  }

  return(stack_->stack_[current_]);
}
*/



//  GP_StackIteratorA::operator ==(GP_StackIteratorA arg)
//-----------------------------------------------------------------------------
//
  
template<class T, class TRef, const int16A maxSize, class SPtr, class F> 
boolA
GP_StackIteratorA<T, TRef, maxSize, SPtr, F>::operator ==(
  const GP_StackIteratorA<T, TRef, maxSize, SPtr, F>& arg) 
  const
{
  return  ((current_ == arg.current_ ) ?  trueA : falseA);
}




//  GP_StackIteratorA::operator !=(GP_StackIteratorA arg)
//-----------------------------------------------------------------------------
//
  
template<class T, class TRef, const int16A maxSize, class SPtr, class F>
inline
boolA
GP_StackIteratorA<T, TRef, maxSize, SPtr, F>::operator !=(
  const GP_StackIteratorA<T, TRef, maxSize, SPtr, F>& arg) 
  const
{
  return  ((current_ != arg.current_) ?  trueA : falseA);
}




//  GP_StackIteratorA::isEnd()
//-----------------------------------------------------------------------------
//
  
template<class T, class TRef, const int16A maxSize, class SPtr, class F>
inline
boolA
GP_StackIteratorA<T, TRef, maxSize, SPtr, F>::isEnd() const
{
  return current_ == 0 ? trueA : falseA;
}




//  GP_StackIteratorA::operator ++()
//-----------------------------------------------------------------------------
//

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
inline
GP_StackIteratorA<T, TRef, maxSize, SPtr, F>&
GP_StackIteratorA<T, TRef, maxSize, SPtr, F>::operator ++()
{
  if (current_ <= 0)
  {
    F::report( GP_SI_OUTSIDE_STACK_A,
                    "",
                    __LINE__);

    return (*this);    // Return illegal data
  }
  
  --current_;

  return (*this);
}






//  GP_StackIteratorA::operator --()
//-----------------------------------------------------------------------------
//

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
inline
GP_StackIteratorA<T, TRef, maxSize, SPtr, F>&
GP_StackIteratorA<T, TRef, maxSize, SPtr, F>::operator --()
{
  if (current_ >= stack_->head_)
  {
    F::report( GP_SI_OUTSIDE_STACK_A,
                    "",
                    __LINE__);


    return (*this);    // Return illegal data
  }
  
  ++current_;

  return *this;
}




//  GP_StackIteratorA::operator =(GP_StackIteratorA& arg)
//-----------------------------------------------------------------------------
//

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
inline
GP_StackIteratorA<T, TRef, maxSize, SPtr, F>& 
GP_StackIteratorA<T, TRef, maxSize, SPtr, F>::operator =(
  const GP_StackIteratorA<T, TRef, maxSize, SPtr, F>& arg)
{
  current_ = arg.current_;
  stack_ = arg.stack_;

  errorOffset_ = stack_->errorOffset_;

  return (*this);
}


/*************************** end of file **************************************/
