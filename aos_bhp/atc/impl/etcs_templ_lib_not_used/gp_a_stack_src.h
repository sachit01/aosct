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
* %name: gp_a_stack_src.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:18 %
* %Creation date of original object: Wed Jul 15 08:41:15 1998 %
*
* Description: The file contain the implementation of class 
*              GP_StackA.
*
*******************************************************************************/

/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
*
*  eca_2   070504  rquensel WP60 Task 18196: GP has unnecessary instance method for error handler.
*
*---------------------------------------------------------------
*
*   4      011004  konraqu Added maxSize to iterator template parameter
*                           (in order to make a friend declaration of queue).
*
----------------------------------------------------------------------------
* 11       000828  konjph  Updated acc. to changed in error info (KONSCN88).
*
* 10       000515  konjko  Added inlines.
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
* 4        980811  konjko  Use GP_StackIteratorA<...> instead of the typedef
*                          iterator to satisfy GNU.
*
* 3        980715  konjko  Added copy constructor and operator =.
*                          Change of dummy returns when error occured.
*                          head_ is used instead of nElements (removed)
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




/*******************************************************************************
* Local macro definitions
*******************************************************************************/




/*******************************************************************************
* Class variables/Constants
*******************************************************************************/


//  GP_StackA::GP_StackA(int errorOffset)
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
GP_StackA<T, maxSize, F>::GP_StackA(
  int uiqueErrorOffset)
  :
  head_(0),
  errorOffset_(uiqueErrorOffset)
{
}




//  GP_StackA::GP_StackA(const GP_StackA& arg, int errorOffset)
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
GP_StackA<T, maxSize, F>::GP_StackA(
  const GP_StackA& arg)
  :
  head_(0),
  errorOffset_(arg.errorOffset_)
{
  const_iterator iter = arg.end();

  while (iter != arg.begin())
  {
    --iter;
    push(*iter);
  }
}




//  GP_StackA::empty()
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
inline
boolA 
GP_StackA<T, maxSize, F>::empty() 
  const
{
  return ((head_ == 0) ? trueA : falseA);
}




//  GP_StackA::full()
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
inline
boolA 
GP_StackA<T, maxSize, F>::full() 
  const
{
  return ((head_ == maxSize) ? trueA : falseA);
}




//  GP_StackA::size()
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
inline
int16A
GP_StackA<T, maxSize, F>::size() 
  const
{
  return (head_);
}




//  GP_StackA::maximumSize()
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
inline
int16A
GP_StackA<T, maxSize, F>::maximumSize() 
  const
{
  return maxSize;
}




//  GP_StackA::begin()
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
inline
GP_StackIteratorA<T, T&, maxSize, GP_StackA<T, maxSize,F>*, F>
GP_StackA<T, maxSize,F>::begin()
{
  return (iterator(this, head_)); 
}




//  GP_StackA::begin()
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
inline
GP_StackIteratorA<T, const T&, maxSize, const GP_StackA<T, maxSize,F>*, F>
GP_StackA<T, maxSize,F>::begin()
  const
{
  return (const_iterator(this, head_)); 
}




//  GP_StackA::end()
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
inline
GP_StackIteratorA<T, T&, maxSize, GP_StackA<T, maxSize,F>*, F>
GP_StackA<T, maxSize,F>::end()
{
  return (iterator(this, 0)); 
}




//  GP_StackA::end()
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
inline
GP_StackIteratorA<T, const T&, maxSize, const GP_StackA<T, maxSize,F>*, F>
GP_StackA<T, maxSize,F>::end()
  const
{
  return (const_iterator(this, 0)); 
}




//  GP_StackA::top()
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
inline
T& 
GP_StackA<T, maxSize,F>::top()
{ 
  if(head_ == 0) //empty() == trueA)
  {
    F::report( GP_S_EMPTY_ON_TOP_A,
                    "",
                    __LINE__);

    return(stack_[0]);    // Return illegal data
  }

  return(stack_[head_]);
}




//  GP_StackA::top()
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
inline
const T& 
GP_StackA<T, maxSize,F>::top()
  const
{ 
  if(head_ == 0) //empty() == trueA)
  {
	F::report( GP_S_EMPTY_ON_TOP_A,
                    "",
                    __LINE__);

    return(stack_[0]);    // Return illegal data
  }

  return(stack_[head_]);
}




//  GP_StackA::errorOffset()
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
inline 
int 
GP_StackA<T, maxSize,F>::errorOffset()
  const
{
  return errorOffset_;
}




//  GP_StackA::clear()
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F> 
inline
void 
GP_StackA<T, maxSize,F>::clear()
{
  head_ = 0; 
}




//  GP_StackA::push(T& element)
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
inline
void 
GP_StackA<T, maxSize,F>::push(
  const T& element)
{
  if(head_ == maxSize) //full() == trueA)
  {
    F::report( GP_S_FULL_A,
                    "",
                    __LINE__);

    return;
  }

  ++head_;
  stack_[head_] = element;
}




//  GP_StackA::pop()
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
inline
void 
GP_StackA<T, maxSize,F>::pop()
{
  if(head_ == 0) //empty() == trueA)
  {
    F::report( GP_S_EMPTY_ON_POP_A,
                    "",
                    __LINE__);

    return;
  }
  
  --head_;
}




//  GP_StackA::operator =(const GPStackA& arg)
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
GP_StackA<T, maxSize,F>& 
GP_StackA<T, maxSize,F>::operator =(
  const GP_StackA<T, maxSize, F>& arg)
{
  const_iterator iter = arg.end();

  clear();
  
  while (iter != arg.begin())
  {
    --iter;
    push(*iter);
  }

  return *this;
}



/*************************** end of file **************************************/
