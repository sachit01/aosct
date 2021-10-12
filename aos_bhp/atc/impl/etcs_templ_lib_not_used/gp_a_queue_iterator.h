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
* %name: gp_a_queue_iterator.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:17 %
* %Creation date of original object: Wed Jul 15 08:41:07 1998 %
*
* Description: This file contain the definition of class GP_QueueIteratorA
*
*******************************************************************************/

/*******************************************************************************
* Revision History
*
* Version  Date    Sign     Change description
*
*          151222  bhermans Removed some ATPCU-dependencies 
*
* 3        130520  konuddj  Lint corrections [4346]. 
*  2       070504  rquensel WP60 Task 18196: GP has unnecessary instance method for error handler.
*
*---------------------------------------------------------------
*   3      011003  konraqu Removed postfix ++/--, removed defualt constructor,
*                          added maxSize to iterator template parameter
*                          (in order to make a friend declaration of queue).
*
------------------------------------------------------------------------------
*
* 10       000615  konjko  Fixed version history.
*
* 9        000525  konjko  Removed include of gp_default_error_handler.h.
*
* 8        000111  konraqu Removed default error handler in the template.
*
* 7        ??????  sigmsv  Removed warning.
*
* 6        981208  sigmsv  Replaced gp_text_msg.h with gp_a_errorinfo.hpp
*
* 5        981111  konjko  Changed error_ to a pointer.
*                          illegalDummy_ removed.
*                          operator *() const excluded to satisfy gcc v2.6.
*
* 4        980915  konjko  Implemented error codes using errorOffset_
*
* 3        980715  konjko  The templates takes a queue pointer argument that 
*                          is used to declare queue_. maxSize removed from
*                          template arguments.
*                          operator = returns a reference to this.
*
* 2        980624  konjko  added const *(), improved comments
*
* 1        980605  konjko  created
*
*******************************************************************************/

#ifndef GP_A_QUEUE_ITERATOR_H
#define GP_A_QUEUE_ITERATOR_H

#ifdef __cplusplus




/*******************************************************************************
* Includes
*******************************************************************************/

//#include "vss_a_scalar_types.h"
//#include "gp_a_errorinfo.hpp"




/*******************************************************************************
* Macros
*******************************************************************************/




/*******************************************************************************
* Global Type definitions, Class definitions and Function prototypes
*******************************************************************************/

#ifdef _MSC_VER
#pragma warning(disable: 4786) // Don't warn about truncated debug info names
#endif

//  GP_QueueA
//-----------------------------------------------------------------------------
//
//  Forward declaration of the container class.
//

template<class T, const int16_t maxSize, class F>
class GP_QueueA;




//  GP_QueueIteratorA
//-----------------------------------------------------------------------------
//
//  GP_QueueIteratorA is an iterator used to traverse and inspect GP_QueueA.
//  The  iterator refers to one of the elements in the queues internal array. 
//  The iterator holds the refered elements array index.
//
//  current_ is counted from the queue front.
//
  
template<class T, class TRef, const int16_t maxSize, class QPtr, class F>
class GP_QueueIteratorA
{
public:
//  GP_QueueIteratorA(int errorOffset = 0);

  
  // Inspectors
  TRef                      operator *();
//  TRef                      operator *() const;                  gcc2.6 remove
  bool                     operator ==(const GP_QueueIteratorA& arg) const;
  bool                     operator !=(const GP_QueueIteratorA& arg) const;

  bool                     isEnd() const;

  // Mutators
  GP_QueueIteratorA&        operator ++();
//  GP_QueueIteratorA         operator ++(int);
  GP_QueueIteratorA&        operator --();
//  GP_QueueIteratorA         operator --(int);
  GP_QueueIteratorA&        operator =(const GP_QueueIteratorA& arg);

private:
    
  friend class GP_QueueA<T, maxSize, F>;

  // Constructor
  GP_QueueIteratorA(QPtr queue, int16_t startPos);

  QPtr                      queue_;
  int16_t                   current_;  
  int                       errorOffset_;
};




#if 0




//  GP_QueueIteratorA::GP_QueueIteratorA(QPtr queue, int16A startPos)
//-----------------------------------------------------------------------------
//
//  The iterator is initiate to refer to the element with array index startPos.
//

template<class T, class TRef, class QPtr, class F>
GP_QueueIteratorA<T, TRef, QPtr, F>::GP_QueueIteratorA(
  QPtr queue, 
  int16A startPos);





//  GP_QueueIteratorA::GP_QueueIteratorA(int errorOffset)
//-----------------------------------------------------------------------------
//
//  The iterator is initiate to null reference.
//

template<class T, class TRef, class QPtr, class F>
GP_QueueIteratorA<T, TRef, QPtr, F>::GP_QueueIteratorA(
  int errorOffset);




//  GP_QueueIteratorA::operator *()
//-----------------------------------------------------------------------------
//
//  The operation returns a reference to the element that the iterator is 
//  currently refering to.
//

template<class T, class TRef, class QPtr, class F>
TRef
GP_QueueIteratorA<T, TRef, QPtr, F>::operator *(); 




//  GP_QueueIteratorA::operator *() const
//-----------------------------------------------------------------------------
//
//  The operation returns a constant reference to the element that the 
//  iterator is currently refering to.
//

template<class T, class TRef, class QPtr, class F>
TRef
GP_QueueIteratorA<T, TRef, QPtr, F>::operator *()
  const; 




//  GP_QueueIteratorA::operator ==(GP_QueueIteratorA arg)
//-----------------------------------------------------------------------------
//
//  The operation returns trueA if this iterator refers to the same element as
//  the iterator arg is, otherwise it returns falseA.
//
//  Should be a global function but is not due to VC++4.2 compiler bug.
//

template<class T, class TRef, class QPtr, class F>
inline 
boolA
GP_QueueIteratorA<T, TRef, QPtr, F>::operator ==(
  const GP_QueueIteratorA<T, TRef, QPtr, F>& arg) 
  const;




//  GP_QueueIteratorA::operator !=(GP_QueueIteratorA arg)
//-----------------------------------------------------------------------------
//
//  The operation returns falseA if this iterator refers to the same element as
//  the iterator arg is, otherwise it returns trueA.
//
//  Should be a global function but is not due to VC++4.2 compiler bug.
//

template<class T, class TRef, class QPtr, class F>
inline 
boolA
GP_QueueIteratorA<T, TRef, QPtr, F>::operator !=(
  const GP_QueueIteratorA<T, TRef, QPtr, F>& arg)
  const;




//  GP_QueueIteratorA::operator ++()
//-----------------------------------------------------------------------------
//
//  Operator ++ makes the iterator refer to the next element in the queue.
//  The operation returns a reference to this iterator.
//  The iterator is not allowed to refer to the end element before the 
//  operation.
//

template<class T, class TRef, class QPtr, class F>
inline 
GP_QueueIteratorA<T, TRef, QPtr, F>&
GP_QueueIteratorA<T, TRef, QPtr, F>::operator ++();




//  GP_QueueIteratorA::operator ++(int)
//-----------------------------------------------------------------------------
//
//  Operator ++ makes the iterator refer to the next element in the queue.
//  The operation returns an iterator refering to the old element.
//  The iterator is not allowed to refer to the end element before the 
//  operation.
//  

template<class T, class TRef, class QPtr, class F>
inline 
GP_QueueIteratorA<T, TRef, QPtr, F>
GP_QueueIteratorA<T, TRef, QPtr, F>::operator ++(int);




//  GP_QueueIteratorA::operator --()
//-----------------------------------------------------------------------------
//
//  Operator -- makes the iterator refer to the previous element in the queue.
//  The operation returns a reference to this iterator.
//  The iterator is not allowed to refer to the first element before the 
//  operation.
//  

template<class T, class TRef, class QPtr, class F>
inline
GP_QueueIteratorA<T, TRef, QPtr, F>&
GP_QueueIteratorA<T, TRef, QPtr, F>::operator --();




//  GP_QueueIteratorA::operator --()
//-----------------------------------------------------------------------------
//
//  Operator --  makes the iterator refer to the previous element in the queue.
//  The operation returns an iterator refering to the old element.
//  The iterator is not allowed to refer to the first element before the 
//  operation.
//  

template<class T, class TRef, class QPtr, class F>
inline 
GP_QueueIteratorA<T, TRef, QPtr, F>
GP_QueueIteratorA<T, TRef, QPtr, F>::operator --(int);




//  GP_QueueIteratorA::operator =(GP_QueueIteratorA& arg)
//-----------------------------------------------------------------------------
//
//  Operator =  makes this iterator to refer to the same element as the
//  iterator arg. The operation returns a reference to this iterator.
//
//  POST:    
//        operator *() = *arg
//

template<class T, class TRef, class QPtr, class F>
inline 
GP_QueueIteratorA<T, TRef, QPtr, F>&
GP_QueueIteratorA<T, TRef, QPtr, F>::operator =(
  const GP_QueueIteratorA<T, TRef, QPtr, F>& arg);



#endif
#include "gp_a_queue_iterator_src.h"


#else
#error C++ compilation required!!!
#endif

#endif // GP_A_QUEUE_ITERATOR_H


/*************************** end of file **************************************/
