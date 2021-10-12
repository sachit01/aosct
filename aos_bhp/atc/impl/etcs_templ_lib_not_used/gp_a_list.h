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
* %name: gp_a_list.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:17 %
* %Creation date of original object: Wed Jul 15 08:39:08 1998 %
*
* Description: This file contain the definition of class GP_ListA
*
*******************************************************************************/

/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
*
*  5	   140210  szafar   BM: merged v4 and v3.2.1
*  4	   131219  wahmad   constant get Apis added in order to get the reference of list private
*							members to be used in cross-comparison.
*  3       130520  konuddj  Lint corrections [4346]. 
*  2       070504  rquensel WP60 Task 18196: GP has unnecessary instance method for error handler.
*
*--------------------------------------------------------------------------
*  8       011019  konraqu Changed insert from void to return iterator.
*  7       010912  konraqu Made copy constructor private (should not be used).
*  6       010910  konraqu Added copy constructor.
*  5       010816  konraqu Changed version history.
*
*  4       010816  konraqu Changed compile flags.
*
*  3       010810  konraqu Rewrote code, now it also checks if iterators are 
*                          used on a modified list (only if _DEBUG).
*
*---------------------------------------------------------------
*
* 11       000525  konjko  Removed include of gp_default_error_handler.h.
*
* 10       000515  konjko  New internal implementation.
*
* 9        000111  konraqu Removed default error handler in the template.
*
* 8        991110  sigmsv  Disabled silly MSC debug info warning.
*
* 7        981208  sigmsv  Replaced gp_text_msg.h with gp_a_error_info.hpp.
*                          Removed error offset from copy constructor.
*
* 6        981111  konjko  Changed error_ to a pointer.
*                          illegalDummy_ removed.
*
* 5        980915  konjko  Implemented error codes using errorOffset_
*
* 4        980825  konjko  Change from only "GP_ListA" to 
*                          "GP_ListA<T, maxSize, F>".
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

#ifndef GP_A_LIST_H
#define GP_A_LIST_H

#ifdef __cplusplus

/*#ifdef _DEBUG
#define GP_DEBUG
#endif
*/

/*******************************************************************************
* Includes
*******************************************************************************/

//#include "vss_a_scalar_types.h"
//#include "gp_a_stack.h"
//#include "gp_a_link.h"
#include "gp_a_list_iterator.h"
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
//  GP_ListA is a double linked list used to hold elements of type T. 
//  The list is built of links, each link holds one T element and pointers to 
//  the previous and the next link. The links are stored in an array. A stack 
//  is used to keep track of free array index's. 
//  It is possible to traverse the list with iterator and const_iterator.
//  The lists maximum size is maxSize and errors are reported to F.report(). 
//
//  INV:    
//        size() >= 0
//
//        size() <= maxSize
//        
  
template<class T, const uint16_t maxSize, class F>
class GP_ListA
{
public:

  // Type defs ----------------------------------------------------------------

  typedef GP_ListIteratorA<
    T,
    maxSize,
    T&,
    GP_ListA<
      T,
      maxSize,
      F>*,
    F> iterator;

  typedef GP_ListIteratorA<
    T,
    maxSize,
    const T&,
    const GP_ListA<
      T,
      maxSize,
      F>*,
    F> const_iterator;


  // Constructors -------------------------------------------------------------

  explicit GP_ListA(
    int uniqueErrorOffset = 0); // Error offset


  // Inspectors ---------------------------------------------------------------

  bool
  empty() const;

  bool
  full() const;

  uint16_t
  size() const;

  iterator
  begin();
  
  const_iterator
  begin() const;
  
  iterator
  end();

  const_iterator
  end() const;
  
  T&
  front();

  const T&
  front() const;

  T&
  back();

  const T&
  back() const;

  int
  errorOffset () const;

  uint16_t
  maximumSize() const;

#ifdef GP_DEBUG
  void
  checkConsistency();
#endif

  // Mutators ----------------------------------------------------------------

  void
  clear();

  void
  pushFront(
    const T& element); // Element to add to the front of the list

  void
  popFront();

  void
  pushBack(
    const T& element); // Element to add to the end of the list

  void
  popBack();

  iterator
  insert(
    const iterator& pos,  // Element is inserted before pos.
    const T& element);    // Element to insert

  iterator
  erase(
    const iterator& pos); // Position to erase

  iterator
  erase(
    const iterator& first, // Position to erase from
    const iterator& last); // Position to erase to

  GP_ListA&
  operator =(
    const GP_ListA& arg); // List to copy from

  const T&                        getItem(const uint32_t index) const;

	const uint16_t& get_frontMarkerFree() const;
	const uint16_t& get_size() const;
	const uint16_t& get_listIndexPrev() const;
	const uint16_t& get_listIndexNext() const;
	const int& get_errorOffset() const;

  // Private atributes --------------------------------------------------------

private: 
  GP_ListA(               // Copy constructor
    const GP_ListA& srcList);

  friend class GP_ListIteratorA<T, maxSize, T&, GP_ListA<T,maxSize,F>*,F>;
  friend class GP_ListIteratorA<T, maxSize, const T&, const GP_ListA<T,maxSize,F>*, F>;

#ifdef GP_DEBUG_COUNTER
  int                             modCount_;
#endif

  uint16_t                          frontMarkerFree_;
  uint16_t                          size_;

  T                               list_[maxSize + 1];
  uint16_t                          listIndexPrev_[maxSize + 1];
  uint16_t                          listIndexNext_[maxSize + 1];
  
  int                             errorOffset_;

};



#include "gp_a_list_src.h"


#else
#error C++ compilation required!!!
#endif

#endif //GP_A_LIST_H



/*************************** end of file **************************************/
