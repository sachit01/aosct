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
* %name: gp_a_list_iterator.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:17 %
* %Creation date of original object: Wed Jul 15 08:39:47 1998 %
*
* Description: This file contain the definition of class GP_ListIteratorA
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
*                  Bo H    Experiment   
*
*******************************************************************************/

#ifndef GP_A_LIST_ITERATOR_H
#define GP_A_LIST_ITERATOR_H

#ifdef __cplusplus




/*******************************************************************************
* Includes
*******************************************************************************/

//#include "vss_a_scalar_types.h"
//#include "gp_a_link.h"
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

//  GP_ListA
//-----------------------------------------------------------------------------
//
//  Forward declaration of the container class.
//

template<class T, const uint16_t maxSize, class F>
class GP_ListA;




//  GP_ListIteratorA
//-----------------------------------------------------------------------------
//
//  GP_ListIteratorA is an iterator used to traverse and inspect GP_ListA.
//  The  iterator points to one of the links in the list. The link hold an
//  element of type T.
//

  
template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
class GP_ListIteratorA
{
public:
  
  // Inspectors
  TRef                      operator *();
//  TRef                      operator *() const;                  gcc2.6 remove
  bool                     operator ==(const GP_ListIteratorA& arg) const;
  bool                     operator !=(const GP_ListIteratorA& arg) const;
  int                       errorOffset () const;

  bool                     isEnd() const;
  
  // Mutators
  GP_ListIteratorA&         operator ++();
  GP_ListIteratorA&         operator --();
  GP_ListIteratorA&         operator =(const GP_ListIteratorA& arg);



private:

  friend class GP_ListA<T, maxSize, F>;

  GP_ListIteratorA(LPtr list, uint16_t startPos);
  uint16_t                    current_;  

#ifdef GP_DEBUG_COUNTER
  int                       modCount_;
#endif

  LPtr                      list_;
  int                       errorOffset_;
};




#if 0




//  GP_ListIteratorA::GP_ListIteratorA(LPtr list, GP_LinkA<T>* startPos)
//-----------------------------------------------------------------------------
//
//  The iterator is initialized to refer to the startPos link.
//

template<class T, class TRef, class LPtr, class F>
GP_ListIteratorA<T, TRef, LPtr, F>::GP_ListIteratorA(
  LPtr list,
  TRef startPos);




//  GP_ListIteratorA::constructor(int errorOffset)
//-----------------------------------------------------------------------------
//
//  The iterator is initiate to null reference.
//

template<class T, class TRef, class LPtr, class F>
GP_ListIteratorA<T, TRef, LPtr, F>::GP_ListIteratorA(
  int errorOffset);




//  GP_ListIteratorA::operator *()
//-----------------------------------------------------------------------------
//
//  The operation returns a reference to the element that the iterator is 
//  currently refering to.
//
  
template<class T, class TRef, class LPtr, class F>
inline 
TRef
GP_ListIteratorA<T, TRef, LPtr, F>::operator *();




//  GP_ListIteratorA::operator *() const
//-----------------------------------------------------------------------------
//
//  The operation returns a constant reference to the element that the 
//  iterator is currently refering to.
//
  
template<class T, class TRef, class LPtr, class F>
inline 
TRef
GP_ListIteratorA<T, TRef, LPtr, F>::operator *() 
  const;




//  GP_ListIteratorA::current()
//-----------------------------------------------------------------------------
//
//  The operation returns a pointer to the link that the iterator is 
//  currently refering to.
//
  
template<class T, class TRef, class LPtr, class F>
inline 
GP_LinkA<T>*
GP_ListIteratorA<T, TRef, LPtr, F>::current();




//  GP_ListIteratorA::current() const
//-----------------------------------------------------------------------------
//
//  The operation returns a constant pointer to the link that the iterator is 
//  currently refering to.
//
  
template<class T, class TRef, class LPtr, class F>
inline 
const GP_LinkA<T>*
GP_ListIteratorA<T, TRef, LPtr, F>::current() 
  const;




//  GP_ListIteratorA::errorOffset() const
//-----------------------------------------------------------------------------
//

template<class T, class TRef, class LPtr, class F>
int
GP_ListIteratorA<T, TRef, LPtr, F>::errorOffset()
  const;




//  GP_ListIteratorA::operator ==(GP_ListIteratorA arg)
//-----------------------------------------------------------------------------
//
//  The operation returns trueA if this iterator refers to the same element as
//  the iterator arg is, otherwise it returns falseA.
//
//  Should be a global function but is not due to VC++4.2 compiler bug.
//

template<class T, class TRef, class LPtr, class F>
inline 
boolA
GP_ListIteratorA<T, TRef, LPtr, F>::operator ==(
  const GP_ListIteratorA<T, TRef, LPtr, F>& arg) 
  const;




//  GP_ListIteratorA::operator !=(GP_ListIteratorA arg)
//-----------------------------------------------------------------------------
//
//  The operation returns trueA if this iterator does not refer to the same 
//  element as the iterator arg is, otherwise it returns falseA.
//
//  Should be a global function but is not due to VC++4.2 compiler bug.
//

template<class T, class TRef, class LPtr, class F>
inline 
boolA
GP_ListIteratorA<T, TRef, LPtr, F>::operator !=(
  const GP_ListIteratorA<T, TRef, LPtr, F>& arg) 
  const;




//  GP_ListIteratorA::operator ++()
//-----------------------------------------------------------------------------
//
//  Operator ++ makes the iterator refer to the next element in the list.
//  The operation returns a reference to this iterator.
//  The iterator is not allowed to refer to the end element before the 
//  operation.
//

template<class T, class TRef, class LPtr, class F>
inline 
GP_ListIteratorA<T, TRef, LPtr, F>&
GP_ListIteratorA<T, TRef, LPtr, F>::operator ++();




//  GP_ListIteratorA::operator ++(int)
//-----------------------------------------------------------------------------
//
//  Operator ++ makes the iterator refer to the next element in the list.
//  The operation returns an iterator refering to the new element.
//  The iterator is not allowed to refer to the end element before the 
//  operation.
//  

template<class T, class TRef, class LPtr, class F>
inline 
GP_ListIteratorA<T, TRef, LPtr, F>
GP_ListIteratorA<T, TRef, LPtr, F>::operator ++(int);




//  GP_ListIteratorA::operator --()
//-----------------------------------------------------------------------------
//
//  Operator -- makes the iterator refer to the previous element in the list.
//  The operation returns a reference to this iterator.
//  The iterator is not allowed to refer to the first element before the 
//  operation.
//  

template<class T, class TRef, class LPtr, class F>
inline 
GP_ListIteratorA<T, TRef, LPtr, F>&
GP_ListIteratorA<T, TRef, LPtr, F>::operator --();




//  GP_ListIteratorA::operator --(int)
//-----------------------------------------------------------------------------
//
//  Operator -- makes the iterator refer to the previous element in the list.
//  The operation returns an iterator refering to the old element.
//  The iterator is not allowed to refer to the first element before the 
//  operation.
//  

template<class T, class TRef, class LPtr, class F>
inline 
GP_ListIteratorA<T, TRef, LPtr, F>
GP_ListIteratorA<T, TRef, LPtr, F>::operator --(int);




//  GP_ListIteratorA::operator =(GP_ListIteratorA& arg)
//-----------------------------------------------------------------------------
//
//  Operator = makes this iterator to refer to the same element as the
//  iterator arg. The operation returns a reference to this iterator.
//
//  POST:    
//        operator *() = *arg
//


template<class T, class TRef, class LPtr, class F>
inline 
GP_ListIteratorA<T, TRef, LPtr, F>&
GP_ListIteratorA<T, TRef, LPtr, F>::operator =(
  const GP_ListIteratorA<T, TRef, LPtr, F>& arg);


#endif
#include "gp_a_list_iterator_src.h"


#else
#error C++ compilation required!!!
#endif

#endif // GP_A_LIST_ITERATOR_H


/*************************** end of file **************************************/
