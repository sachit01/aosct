#ifndef GPListRevIterator_hpp
#define GPListRevIterator_hpp

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration of the GPListRevIterator class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-09-11    spandita    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "gp_list_iterator.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATC
{
  /**
  * Forward declaration of the list class.
  */
  template<class T, const uint16_t maxSize, class F> class GPList;
  template<class T, const uint16_t  maxSize, class TRef, class LPtr, class F> class GPListIterator;
  /**
  *  GPListRevIterator is a reverse iterator used to traverse and inspect a GPList.
  *
  *  The  iterator points to one of the links in the list. The link holds an
  *  element of type T.
  */

  template<class T, const uint16_t maxSize, class TRef, class LPtr, class F>
  class GPListRevIterator
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
    bool operator ==(const GPListRevIterator& arg) const;

    /**
    * Not equal to operator
    *
    * @return true if elements are NOT equal
    */
    bool operator !=(const GPListRevIterator& arg) const;

    /**
    * Increment operator
    *
    * @return Iterator to next element in list
    */
    GPListRevIterator& operator ++();

    /**
    * Decrement operator
    *
    * @return Iterator to previous element in list
    */
    GPListRevIterator& operator --();

    /**
    * Assignment operator
    *
    * @return *this
    */
    GPListRevIterator& operator =(const GPListRevIterator& arg);

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

    /**
    * Overloaded assignment operator
    *
    * @return *this
    */

    GPListRevIterator& operator =(const GPListIterator<T, maxSize, TRef, LPtr, F>& arg);


  private:

    /**
    *
    * The GPList class needs to access the GPListRevIterator class
    *
    */
    friend class GPList<T, maxSize, F>;

    /**
    * Constructor
    *
    * @param[in]  list      List to be iterated
    * @param[in]  startPos  Start position in list
    */
    GPListRevIterator(LPtr list, uint16_t startPos);

    uint16_t  current_;   //!< Current index 
    LPtr  list_;          //!< The list that the iterator works with
  };
}

/**
* Include the implementation of the class-template.
*/
#include "gp_list_rev_iterator_src.hpp"

#endif

/*************************** end of file **************************************/
