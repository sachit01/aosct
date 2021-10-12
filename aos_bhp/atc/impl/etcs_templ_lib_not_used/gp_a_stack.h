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
* %name: gp_a_stack.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:18 %
* %Creation date of original object: Wed Jul 15 08:41:23 1998 %
*
* Description: This file contain the definition of class GP_StackA
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
*   4      011004  konraqu Added maxSize to iterator template parameter
*                           (in order to make a friend declaration of queue).
*   3      010912  konraqu Moved copy constructor to be private (should not be
*                          used).
*
----------------------------------------------------------------------------
* 10       000525  konjko  Removed include of gp_default_error_handler.h.
*
* 9        000111  konraqu Removed default error handler in the template.
*
* 8        991110  sigmsv  Disabled silly MSC debug info warning.
*
* 7        981208  sigmsv  Replaced gp_text_msg.h with gp_a_errorinfo.hpp
*                          Removed error offset from copy constructor
*
* 6        981111  konjko  Changed error_ to a pointer.
*                          illegalDummy_ removed.
*
* 5        980915  konjko  Implemented error codes using errorOffset_
*
* 4        980825  konjko  Change from only "GP_StackA" to 
*                          "GP_StackA<T, maxSize, F>".
*
* 3        980715  konjko  Added copy constructor and operator =.
*                          Change of dummy returns when error occured.
*                          The spec part is hidden from compiler with #if 0.
*                          head_ is used instead of nElements (removed)
*                          Added maximumSize() function.
*
* 2        980624  konjko  added some const inspectors, improved comments
*
* 1        980605  konjko  created
*
*******************************************************************************/

#ifndef GP_A_STACK_H
#define GP_A_STACK_H

#ifdef __cplusplus




/*******************************************************************************
* Includes
*******************************************************************************/

#include "vss_a_scalar_types.h"
#include "gp_a_stack_iterator.h"
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
//  GP_StackA is a LIFO class used to hold elements of type T. 
//  The elements are stored in an array. The array is used as a stack 
//  buffer. The indexes of the head element is remembered. 
//  It is possible to traverse the stack with iterator and const_iterator.
//  GP_StackIteratorA is a friend because the iterator needs access to the 
//  array elements.
//  
//  The stacks maximum size is maxSize and errors are reported to F.report().
//
//  INV:    
//        size() >= 0
//
//        size() <= maxSize
//        
  
template<class T,const int16A maxSize, class F>
class GP_StackA
{
  friend class GP_StackIteratorA<T, T&, maxSize, GP_StackA<T,maxSize, F>*, F>;
  friend class GP_StackIteratorA<T, const T&, maxSize, const GP_StackA<T,maxSize, F>*, F>;

public:
  typedef GP_StackIteratorA<T, T&, maxSize, GP_StackA<T,maxSize, F>*, F>         iterator;
  typedef GP_StackIteratorA<T, const T&, maxSize, const GP_StackA<T,maxSize, F>*, F>  const_iterator;
  
  // Constructor
  explicit GP_StackA(int uiqueErrorOffset);

  
  // Inspectors
  boolA                           empty() const;
  boolA                           full() const;
  int16A                          size() const;
  int16A                          maximumSize() const;
  iterator                        begin();
  const_iterator                  begin() const;
  iterator                        end();
  const_iterator                  end() const;
  T&                              top();
  const T&                        top() const;
  int                             errorOffset () const;


  // Mutators
  void                            clear();
  void                            push(const T& element);
  void                            pop();
  GP_StackA&                      operator =(const GP_StackA& arg);

private:
  GP_StackA(const GP_StackA& arg);
  T                               stack_[maxSize+1];
  int16A                          head_;
  int                             errorOffset_;
};




#if 0




//  GP_StackA::GP_StackA(int errorOffset)
//-----------------------------------------------------------------------------
//
//  Creates the component and allocates memory for the stack. 
//
//  POST:    
//        size() == 0
//        begin() == end()
//

template<class T,const int16A maxSize, class F>
GP_StackA<T, maxSize, F>::GP_StackA(
  int errorOffset);




//  GP_StackA::GP_StackA(const GP_StackA& arg, int errorOffset)
//-----------------------------------------------------------------------------
//
//  Creates the component and allocates memory for the stack. Initiates all
//  members to the same values as arg's mambers.
//

template<class T,const int16A maxSize, class F>
GP_StackA<T, maxSize, F>::GP_StackA(
  const GP_StackA& arg,
  int errorOffset);




//  GP_StackA::empty() const
//-----------------------------------------------------------------------------
//
//  empty() returns trueA if there is no elements in the stack, else it returns 
//  falseA.
//

template<class T,const int16A maxSize, class F>
inline 
boolA 
GP_StackA<T, maxSize, F>::empty() 
  const;




//  GP_StackA::full() const
//-----------------------------------------------------------------------------
//
//  full() returns trueA if the number of elements in stack equals the given 
//  max size of the stack (template argument maxSize).
//

template<class T,const int16A maxSize, class F>
inline
boolA 
GP_StackA<T, maxSize, F>::full() 
  const;




//  GP_StackA::size() const
//-----------------------------------------------------------------------------
//
//  size() returns the number of elements in the stack.
//

template<class T,const int16A maxSize, class F>
inline 
int16A
GP_StackA<T, maxSize, F>::size() 
  const;




//  GP_StackA::maximumSize() const
//-----------------------------------------------------------------------------
//
//  maximumSize() returns the maximal number of elements that can be stored in
//  the stack.
//

template<class T,const int16A maxSize, class F>
inline 
int16A
GP_StackA<T, maxSize, F>::maximumSize() 
  const;




//  GP_StackA::begin()
//-----------------------------------------------------------------------------
//
//  begin() returns an iterator pointing at the first element in the stack.
//

template<class T,const int16A maxSize, class F>
inline 
GP_StackA<T, maxSize,F>::iterator
GP_StackA<T, maxSize,F>::begin();




//  GP_StackA::begin() const
//-----------------------------------------------------------------------------
//
//  begin() returns a constant iterator pointing at the first element in the 
//  stack.
//

template<class T,const int16A maxSize, class F>
inline 
GP_StackA<T, maxSize,F>::const_iterator
GP_StackA<T, maxSize,F>::begin()
  const;




//  GP_StackA::end()
//-----------------------------------------------------------------------------
//
//  end() returns an iterator pointing one past the last element in the 
//  stack.
//

template<class T,const int16A maxSize, class F>
inline 
GP_StackA<T, maxSize,F>::iterator
GP_StackA<T, maxSize,F>::end();




//  GP_StackA::end() const
//-----------------------------------------------------------------------------
//
//  end() returns a constant iterator pointing one past the last element 
//  in the stack.
//

template<class T,const int16A maxSize, class F>
inline 
GP_StackA<T, maxSize,F>::const_iterator
GP_StackA<T, maxSize,F>::end()
  const;




//  GP_StackA::top()
//-----------------------------------------------------------------------------
//
//  top() returns a reference to the top most element in the stack. The
//  reference is valid until the element is poped.
//
//  PRE:
//        empty() == falseA
//

template<class T,const int16A maxSize, class F>
T& 
GP_StackA<T, maxSize,F>::top();




//  GP_StackA::top() const
//-----------------------------------------------------------------------------
//
//  top() returns a constant reference to the top most element in the stack. 
//  The reference is valid until the element is poped.
//
//  PRE:
//        empty() == falseA
//

template<class T,const int16A maxSize, class F>
const T& 
GP_StackA<T, maxSize,F>::top()
  const;




//  GP_StackA::errorOffset() const
//-----------------------------------------------------------------------------
//

template<class T,const int16A maxSize, class F>
int
GP_StackA<T, maxSize,F>::errorOffset()
  const;




//  GP_StackA::clear()
//-----------------------------------------------------------------------------
//
//  POST:
//        size() == 0
//

template<class T,const int16A maxSize, class F>
inline 
void 
GP_StackA<T, maxSize,F>::clear();




//  GP_StackA::push(T& element)
//-----------------------------------------------------------------------------
//
//  PRE:
//        full() == falseA
//
//  POST:
//        size() == size'() + 1
//
//        top() == element
//

template<class T,const int16A maxSize, class F>
void 
GP_StackA<T, maxSize,F>::push(
  const T& element);




//  GP_StackA::pop()
//-----------------------------------------------------------------------------
//
//  PRE:
//        empty() == falseA
//
//  POST:
//        *begin() == *(begin'() + 1)
//
//        size() == size'() - 1
//

template<class T,const int16A maxSize, class F>
void 
GP_StackA<T, maxSize,F>::pop();




//  GP_StackA::operator =(const GP_StackA& arg)
//-----------------------------------------------------------------------------
//
//  Changes all members to the same values as arg's mambers.
//

template<class T,const int16A maxSize, class F>
GP_StackA<T, maxSize,F>& 
GP_StackA<T, maxSize,F>::operator =(
  const GP_StackA& arg);


#endif
#include "gp_a_stack_src.h"


#else
#error C++ compilation required!!!
#endif

#endif //GP_A_STACK_H



/*************************** end of file **************************************/
