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
* %name: gp_a_stack_iterator.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:18 %
* %Creation date of original object: Wed Jul 15 08:41:39 1998 %
*
* Description: This file contain the definition of class GP_StackIteratorA
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
*   3      011003  konraqu Removed postfix ++/--, removed defualt constructor,
*                          added maxSize to iterator template parameter
*                          (in order to make a friend declaration of stack),
*                          changed list calls to directly use list attributes.
*
------------------------------------------------------------------------------
*
* 7        000525  konjko  Removed include of gp_default_error_handler.h.
*
* 6        000111  konraqu Removed default error handler in the template.
*
* 5        981111  konjko  Changed error_ to a pointer.
*                          illegalDummy_ removed.
*                          operator *() const excluded to satisfy gcc v2.6.
*
* 4        980915  konjko  Implemented error codes using errorOffset_
*
* 3        980715  konjko  The templates takes a stack pointer argument that 
*                          is used to declare stack_. maxSize removed from
*                          template arguments.
*                          operator = returns a reference to this.
*
* 2        980624  konjko  a const *() function is added, improved comments
*
* 1        980605  konjko  created
*
*******************************************************************************/

#ifndef GP_A_STACK_ITERATOR_H
#define GP_A_STACK_ITERATOR_H

#ifdef __cplusplus




/*******************************************************************************
* Includes
*******************************************************************************/

#include "vss_a_scalar_types.h"
#include "gp_a_errorinfo.hpp"




/*******************************************************************************
* Macros
*******************************************************************************/




/*******************************************************************************
* Global Type definitions, Class definitions and Function prototypes
*******************************************************************************/

#ifdef _MSC_VER
#pragma warning(disable: 4786) // Don't warn about truncated debug info names
#endif


//  GP_StackA
//-----------------------------------------------------------------------------
//
//  Forward declaration of the container class.
//

template<class T, const int16A maxSize, class F>
class GP_StackA;




//  GP_StackIteratorA
//-----------------------------------------------------------------------------
//
//  GP_StackIteratorA is an iterator used to traverse and inspect GP_StackA.
//  The  iterator refers to one of the elements in the stacks internal array. 
//  The iterator holds the refered elements array index.
//  Forward iteration is done from the top most element and downwards.
//

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
class GP_StackIteratorA
{
public:


  // Inspectors
  TRef                      operator *();
//  TRef                      operator *() const;                  gcc2.6 remove
  boolA                     operator ==(const GP_StackIteratorA& arg) const;
  boolA                     operator !=(const GP_StackIteratorA& arg) const;

  boolA                     isEnd() const;

  // Mutators
  GP_StackIteratorA&        operator ++();
//  GP_StackIteratorA         operator ++(int);
  GP_StackIteratorA&        operator --();
//  GP_StackIteratorA         operator --(int);
  GP_StackIteratorA&        operator =(const GP_StackIteratorA& arg);

private:

  friend class GP_StackA<T, maxSize, F>;


  // Constructor
  GP_StackIteratorA(SPtr stack, int16A startPos);

  int16A                    current_;
  SPtr                      stack_;
  int                       errorOffset_;
};




#if 0




//  GP_StackIteratorA::GP_StackIteratorA(SPtr stack, int16A startPos)
//-----------------------------------------------------------------------------
//
//  The iterator is initiate to refer to the element with array index startPos.
//

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
GP_StackIteratorA<T, TRef, SPtr, F>::GP_StackIteratorA(
  SPtr stack, 
  int16A startPos);




//  GP_StackIteratorA::GP_StackIteratorA(int errorOffset)
//-----------------------------------------------------------------------------
//
//  The iterator is initiate to null reference.
//

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
GP_StackIteratorA<T, TRef, SPtr, F>::GP_StackIteratorA(
  int errorOffset);




//  GP_StackIteratorA::operator *()
//-----------------------------------------------------------------------------
//
//  The operation returns a reference to the element that the iterator is 
//  currently refering to.
//

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
TRef
GP_StackIteratorA<T, TRef, SPtr, F>::operator *();




//  GP_StackIteratorA::operator *() const
//-----------------------------------------------------------------------------
//
//  The operation returns a reference to the element that the 
//  iterator is currently refering to.
//

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
TRef
GP_StackIteratorA<T, TRef, SPtr, F>::operator *() 
  const;




//  GP_StackIteratorA::operator ==(GP_StackIteratorA arg)
//-----------------------------------------------------------------------------
//
//  The operation returns trueA if this iterator refers to the same element as
//  the iterator arg is, otherwise it returns falseA.
//
//  Should be a global function but is not due to VC++4.2 compiler bug.
//
 
template<class T, class TRef, const int16A maxSize, class SPtr, class F>
inline 
boolA
GP_StackIteratorA<T, TRef, SPtr, F>::operator ==(
  const GP_StackIteratorA<T, TRef, SPtr, F>& arg) 
  const;




//  GP_StackIteratorA::operator !=(GP_StackIteratorA arg)
//-----------------------------------------------------------------------------
//
//  The operation returns falseA if this iterator refers to the same element as
//  the iterator arg is, otherwise it returns trueA.
//
//  Should be a global function but is not due to VC++4.2 compiler bug.
//

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
inline 
boolA
GP_StackIteratorA<T, TRef, SPtr, F>::operator !=(
  const GP_StackIteratorA<T, TRef, SPtr, F>& arg) 
  const;




//  GP_StackIteratorA::operator ++()
//-----------------------------------------------------------------------------
//
//  Operator ++ makes the iterator refer to the next element in the stack.
//  The operation returns a reference to this iterator.
//  The iterator is not allowed to refer to the end element before the 
//  operation.
//

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
inline 
GP_StackIteratorA<T, TRef, SPtr, F>&
GP_StackIteratorA<T, TRef, SPtr, F>::operator ++();




//  GP_StackIteratorA::operator ++(int)
//-----------------------------------------------------------------------------
//
//  Operator ++ makes the iterator refer to the next element in the stack.
//  The operation returns an iterator refering to the old element.
//  The iterator is not allowed to refer to the end element before the 
//  operation.
//  

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
inline 
GP_StackIteratorA<T, TRef, SPtr, F>
GP_StackIteratorA<T, TRef, SPtr, F>::operator ++(int);




//  GP_StackIteratorA::operator --()
//-----------------------------------------------------------------------------
//
//  Operator -- makes the iterator refer to the previous element in the stack.
//  The operation returns a reference to this iterator.
//  The iterator is not allowed to refer to the first element before the 
//  operation.
//  

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
inline 
GP_StackIteratorA<T, TRef, SPtr, F>&
GP_StackIteratorA<T, TRef, SPtr, F>::operator --();




//  GP_StackIteratorA::operator --()
//-----------------------------------------------------------------------------
//
//  Operator -- makes the iterator refer to the previous element in the stack.
//  The operation returns an iterator refering to the old element.
//  The iterator is not allowed to refer to the first element before the 
//  operation.
//  

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
inline 
GP_StackIteratorA<T, TRef, SPtr, F>
GP_StackIteratorA<T, TRef, SPtr, F>::operator --(int);




//  GP_StackIteratorA::operator =(GP_StackIteratorA& arg)
//-----------------------------------------------------------------------------
//
//  Operator = makes this iterator to refer to the same element as the
//  iterator arg. The operation returns a reference to this iterator.
//
//  POST:    
//        operator *() = *arg
//

template<class T, class TRef, const int16A maxSize, class SPtr, class F>
inline 
GP_StackIteratorA<T, TRef, SPtr, F>&
GP_StackIteratorA<T, TRef, SPtr, F>::operator =(
  const GP_StackIteratorA<T, TRef, SPtr, F>& arg);



#endif
#include "gp_a_stack_iterator_src.h"


#else
#error C++ compilation required!!!
#endif

#endif // GP_A_STACK_ITERATOR_H


/*************************** end of file **************************************/
