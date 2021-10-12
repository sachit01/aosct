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
* %name: gp_a_queue.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:17 %
* %date_created: 2014-09-23 13:17 %
* %Creation date of original object: Wed Jul 15 08:40:50 1998 %
*
* Description: This file contain the definition of class GP_QueueA
*
*******************************************************************************/

/*******************************************************************************
* Revision History
*
* Version  Date    Sign     Change description
*
*          151222  bhermans Removed some ATPCU-dependencies
*
*  5       131220  wahmad merge for version 3.1.1 and 4
*  3.1.1   131220  wahmad   constant get methods added in order to get the reference of private
*             							members to be used in cross-comparison.
*  4       131119  konuddj  WP008:Crosscompare in SPLH [6157]
*  3       130520  konuddj  Lint corrections [4346]. 
*  2       070504  rquensel WP60 Task 18196: GP has unnecessary instance method for error handler.
*
*---------------------------------------------------------------
*   4      011004  konraqu Added maxSize to iterator template parameter
*                           (in order to make a friend declaration of queue).
*   3      010912  konraqu Moved copy constructor to be private (should not be
*                          used).
*
----------------------------------------------------------------------------
*
* 10       000525  konjko  Removed include of gp_default_error_handler.h.
*
* 9        000111  konraqu Removed default error handler in the template.
*
* 8        991110  sigmsv  Disabled silly MSC debug info warning.
*
* 7        981208  sigmsv  Replaced gp_text_msg.h with gp_a_errorinfo.hpp
*                          Remove error offset from copy constructor
*
* 6        981111  konjko  Changed error_ to a pointer.
*                          illegalDummy_ removed.
*
* 5        980915  konjko  Implemented error codes using errorOffset_
*
* 4        980825  konjko  Change from only "GP_QueueA" to 
*                          "GP_QueueA<T, maxSize, F>".
*
* 3        980715  konjko  Added copy constructor and operator =.
*                          Change of dummy returns when error occured.
*                          The spec part is hidden from compiler with #if 0.
*                          Added maximumSize() function.
*
* 2        980624  konjko  added const inspectors, improved comments
*
* 1        980605  konjko  created
*
*******************************************************************************/

#ifndef GP_A_QUEUE_H
#define GP_A_QUEUE_H

#ifdef __cplusplus




/*******************************************************************************
* Includes
*******************************************************************************/

//#include "vss_a_scalar_types.h"
#include "gp_a_queue_iterator.h"
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
//  GP_QueueA is a FIFO class used to hold elements of type T. 
//  The elements are stored in an array. The array is used as a circular 
//  buffer. The indexes of the head and the tail elements are remembered.
//  It is possible to traverse the queue with iterator and const_iterator.
//  GP_QueueIteratorA is a friend because the iterator needs access to the 
//  array elements.
//  
//  The queues maximum size is maxSize and errors are reported to report()
//  in the F class.
//
//  INV:    
//        size() >= 0
//
//        size() <= maxSize
//        
  
template <class T, const int16_t maxSize, class F>
class GP_QueueA
{
  friend class GP_QueueIteratorA<
    T,
    T&,
    maxSize,
    GP_QueueA<
      T,
      maxSize,
      F>*,
    F>;

  friend class GP_QueueIteratorA<
    T, 
    const T&,
    maxSize,  
    const GP_QueueA<
      T,
      maxSize,
      F>*, 
    F>;

public:
  typedef GP_QueueIteratorA<
    T,
    T&,
    maxSize,
    GP_QueueA<
      T,
      maxSize,
      F>*,
    F> iterator;

  typedef GP_QueueIteratorA<
    T, 
    const T&,
    maxSize, 
    const GP_QueueA<
      T,
      maxSize,
      F>*, 
    F> const_iterator;

  // Constructor
  explicit GP_QueueA(int uiqueErrorOffset);


  // Inspectors
  bool                           empty() const;
  bool                           full() const;
  int16_t                          size() const;
  int16_t                          maximumSize() const;
  iterator                        begin();
  const_iterator                  begin() const;
  iterator                        end();
  const_iterator                  end() const;
  T&                              front();
  const T&                        front() const;
  T&                              back();
  const T&                        back() const;
  int                             errorOffset() const;


  // Mutators
  void                            clear();
  void                            push(const T& element);
  void                            pop();
  GP_QueueA&                      operator =(const GP_QueueA& arg);

  void                            init();
  const T&                        getItem(const uint32_t index) const;

  const int16_t& get_nElements() const;
  const int16_t& get_head() const;
  const int16_t& get_tail() const;
  const int& get_errorOffset() const;
  
private:
  GP_QueueA(const GP_QueueA& arg);

  T                               queue_[maxSize+1];
  int16_t                          nElements_;
  int16_t                          head_;
  int16_t                          tail_;
  int                             errorOffset_;
};




#if 0




//  GP_QueueA::GP_QueueA(int errorOffset)
//-----------------------------------------------------------------------------
//
//  Creates the component and allocates memory for the queue. 
//
//  POST:    
//        size() == 0
//        begin() == end()
//

template <class T, const int16A maxSize, class F>
inline 
GP_QueueA<T, maxSize, F>::GP_QueueA(
  int errorOffset);




//  GP_QueueA::GP_QueueA(const GP_QueueA& arg )
//-----------------------------------------------------------------------------
//
//  Creates the component and allocates memory for the list. The components
//  internal state is equal to arg.
//

template<class T, const int16A maxSize, class F>
GP_QueueA<T, maxSize, F>::GP_QueueA(
  const GP_QueueA& arg,
  errorOffset);




//  GP_QueueA::empty() const
//-----------------------------------------------------------------------------
//
//  empty() returns trueA if there is no elements in the queue, else it returns 
//  falseA.
//

template<class T, const int16A maxSize, class F> 
inline 
boolA 
GP_QueueA<T, maxSize, F>::empty() 
  const;




//  GP_QueueA::full() const
//-----------------------------------------------------------------------------
//
//  full() returns trueA if the number of elements in queue equals the given 
//  max size of the queue(template argument maxSize).
//

template<class T, const int16A maxSize, class F> 
inline 
boolA 
GP_QueueA<T, maxSize, F>::full() 
const;




//  GP_QueueA::size() const
//-----------------------------------------------------------------------------
//
//  size() returns the number of elements in the queue.
//

template<class T, const int16A maxSize, class F> 
inline 
int16A
GP_QueueA<T, maxSize, F>::size() 
const;




//  GP_QueueA::maximumSize() const
//-----------------------------------------------------------------------------
//
//  maximumSize() returns the maximal number of elements that can be stored in
//  the queue.
//

template<class T, const int16A maxSize, class F> 
inline 
int16A
GP_QueueA<T, maxSize, F>::maximumSize() 
const;




//  GP_QueueA::begin()
//-----------------------------------------------------------------------------
//
//  begin() returns an iterator pointing at the first element in the queue.
//

template<class T, const int16A maxSize, class F> 
inline 
GP_QueueA<T, maxSize, F>::iterator
GP_QueueA<T, maxSize, F>::begin();




//  GP_QueueA::begin() const
//-----------------------------------------------------------------------------
//
//  begin() returns an iterator pointing at the first element in the queue.
//

template<class T, const int16A maxSize, class F> 
inline 
GP_QueueA<T, maxSize, F>::const_iterator
GP_QueueA<T, maxSize, F>::begin()
  const;




//  GP_QueueA::end()
//-----------------------------------------------------------------------------
//
//  end() returns an iterator pointing at the one past last element in the 
//  queue.
//

template<class T, const int16A maxSize, class F> 
inline 
GP_QueueA<T, maxSize, F>::iterator
GP_QueueA<T, maxSize, F>::end();




//  GP_QueueA::end() const
//-----------------------------------------------------------------------------
//
//  end() returns a constant iterator pointing at the one past last element 
//  in the queue.
//

template<class T, const int16A maxSize, class F> 
inline 
GP_QueueA<T, maxSize, F>::const_iterator
GP_QueueA<T, maxSize, F>::end()
  const;




//  GP_QueueA::front()
//-----------------------------------------------------------------------------
//
//  front() returns a reference to the first element in the queue.
//
//  PRE:
//        empty() == falseA
//

template<class T, const int16A maxSize, class F> 
inline 
T& 
GP_QueueA<T, maxSize, F>::front();




//  GP_QueueA::front() const
//-----------------------------------------------------------------------------
//
//  front() returns a constant reference to the first element in the queue.
//
//  PRE:
//        empty() == falseA
//

template<class T, const int16A maxSize, class F> 
inline 
const T& 
GP_QueueA<T, maxSize, F>::front()
  const;




//  GP_QueueA::back()
//-----------------------------------------------------------------------------
//
//  front() returns a reference to the last element in the queue.
//
//  PRE:
//        empty() == falseA
//

template<class T, const int16A maxSize, class F> 
inline 
T&
GP_QueueA<T, maxSize, F>::back();




//  GP_QueueA::back() const
//-----------------------------------------------------------------------------
//
//  front() returns a constant reference to the last element in the queue.
//
//  PRE:
//        empty() == falseA
//

template<class T, const int16A maxSize, class F> 
inline 
const T&
GP_QueueA<T, maxSize, F>::back()
  const;




//  GP_QueueA::errorOffset() const
//-----------------------------------------------------------------------------
//

template<class T, const int16A maxSize, class F> 
int
GP_QueueA<T, maxSize, F>::errorOffset()
  const;




//  GP_QueueA::clear()
//-----------------------------------------------------------------------------
//
//  POST:
//        size() == 0
//

template<class T, const int16A maxSize, class F> 
inline 
void 
GP_QueueA<T, maxSize, F>::clear();




//  GP_QueueA::push(const T& element)
//-----------------------------------------------------------------------------
//
//  PRE:
//        full() == falseA
//
//  POST:
//        size() == size'() + 1
//
//        back() == element
//

template <class T, const int16A maxSize, class F>
inline 
void 
GP_QueueA<T, maxSize, F>::push(
  const T& element);




//  GP_QueueA::pop()
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


template <class T, const int16A maxSize, class F>
inline 
void 
GP_QueueA<T, maxSize, F>::pop();




//  GP_QueueA::operator =(GP_QueueA& arg)
//-----------------------------------------------------------------------------
//
//  The operation makes this list contents the same as arg's contents.
//


template<class T, const int16A maxSize, class F> 
GP_QueueA<T, maxSize, F>&
GP_QueueA<T, maxSize, F>::operator =(
  const GP_QueueA<T, maxSize, F>& arg);



#endif
#include "gp_a_queue_src.h"


#else
#error C++ compilation required!!!
#endif

#endif /* GP_A_QUEUE_H */



/*************************** end of file **************************************/
