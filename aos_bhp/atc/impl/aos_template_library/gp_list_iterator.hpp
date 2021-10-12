#ifndef GPListIterator_hpp
#define GPListIterator_hpp

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration of the GPListIterator class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-06    marlundg    Created (Adapted from gp_a_list_iterator.h)
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "gp_list_rev_iterator.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATC
{
  /**
  * Forward declaration of the list class.
  */
  template<class T, const uint16_t maxSize, class F> class GPList;
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F> class GPListRevIterator;

  /**
  *  GPListIterator is an iterator used to traverse and inspect a GPList.
  *
  *  The  iterator points to one of the links in the list. The link holds an
  *  element of type T.
  */
  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  class GPListIterator
  {
  public:

    /**
    * Dereferencing operator
    *
    * @return Reference to current element
    */
    TRef operator *();

    /**
    * Equal to Operator
    *
    * @return true if elements are equal
    */
    bool operator ==(const GPListIterator& arg) const;

    /**
    * Not equal to operator
    *
    * @return true if elements are NOT equal
    */
    bool operator !=(const GPListIterator& arg) const;

    /**
    * Increment operator
    *
    * @return Iterator to next element in list
    */
    GPListIterator& operator ++();

    /**
    * Decrement operator
    *
    * @return Iterator to previous element in list
    */
    GPListIterator& operator --();

    /**
    * Assignment operator
    *
    * @return *this
    */
    GPListIterator& operator =(const GPListIterator& arg);

    /**
    * Assignment operator
    *
    * @return *this
    */
    GPListIterator& operator =(const GPListRevIterator<T, maxSize, TRef, LPtr, F>& arg);

    /**
    * Get the Current Index of list
    *
    * @return index  Value of index
    */
    uint16_t getCurrentIndex() const;

    /**
    * Get the list
    *
    * @return list reference
    */
    const LPtr& getItrList() const;

  private:

    /**
    *
    * The GPList class needs to access the GPListIterator class
    *
    */
    friend class GPList<T, maxSize, F>;

    /**
    * Constructor
    *
    * @param[in]  list      List to be iterated
    * @param[in]  startPos  Start position in list
    */
    GPListIterator(LPtr list, uint16_t startPos);

    uint16_t  current_;   //!< Current index 
    LPtr  list_;          //!< The list that the iterator works with
  };
}

/**
* Include the implementation of the class-template.
*/
#include "gp_list_iterator_src.hpp"

#endif

/*************************** end of file **************************************/
