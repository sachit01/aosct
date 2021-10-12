#ifndef GPList_hpp
#define GPList_hpp

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration of the GPlist class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-06    marlundg    Created (Adapted from gp_a_list.h)
* 2017-01-10    spandita    Added reverse function
* 2017-01-25    spandita    Added reverse iterator
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <stdint.h>

#include "gp_list_iterator.hpp"
#include "gp_list_rev_iterator.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATC
{

  /**
  * GPList is a double linked list used to hold elements of type T.
  *
  * The list is built of links, each link holds one T element and pointers to
  * the previous and the next link. The links are stored in an array. A stack
  * is used to keep track of free array index's.
  * It is possible to traverse the list with iterator and const_iterator.
  * The lists maximum size is maxSize and errors are reported to F.report().
  *
  * @param[in]  T         Type to be stored in list
  * @param[in]  maxSize   Static size of list
  * @param[in]  F         Type to be used for errorhandling. NOTE: This mechanism will be changed further on.
  *                       (For now this type only needs a report(char*) method as can be seen in gp_list_sample.cpp)
  */
  template<class T, const uint16_t maxSize, class F>
  class GPList
  {
  public:

    typedef GPListIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F> iterator;
    typedef GPListIterator<T, maxSize, const T&, const GPList<T, maxSize, F>*, F> const_iterator;
    
    typedef GPListRevIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F> reverse_iterator;
    typedef GPListRevIterator<T, maxSize, const T&, const GPList<T, maxSize, F>*, F> const_reverse_iterator;
    /**
     * Constructor
     */
    GPList();

    /**
    * Copy-constructor
    */
    GPList(const GPList& srcList);

    /**
    * Returns whether the list is empty.
    *
    * @return True if the list is empty, false otherwise
    */
    bool empty() const;

    /**
    * Returns whether the list is full.
    *
    * @return True if the list is full, false otherwise
    */
    bool full() const;

    /**
    * Returns the number of elements in the list.
    *
    * @return The number of elements in the list
    */
    uint16_t size() const;

    /**
    * Returns an iterator pointing to the first element in the list container.
    *
    * @return An iterator (if list-object is not const-qualified) to the beginning of the sequence.
    *
    */
    iterator begin();

    /**
    * Returns an iterator pointing to the first element in the list container.
    *
    * @return A const-iterator (if list-object is const-qualified) to the beginning of the sequence.
    *
    */
    const_iterator begin() const;

    /**
    * Returns an iterator referring to the past-the-end element in the list.
    *
    * The past-the-end element is the theoretical element that would follow the last element in the list container.
    * It does not point to any element, and thus shall not be dereferenced.
    * If the container is empty, this function returns the same as GPList::begin.
    *
    * @return An iterator (if list-object is not const-qualified) to the element past-the-end of the list.
    *
    */
    iterator end();

    /**
    * Returns an const-iterator referring to the past-the-end element in the list.
    *
    * The past-the-end element is the theoretical element that would follow the last element in the list.
    * It does not point to any element, and thus shall not be dereferenced.
    * If the container is empty, this function returns the same as GPList::begin.
    *
    * @return A const_iterator (if list-object is const-qualified) to the element past-the-end of the list.
    *
    */
    const_iterator end() const;

    /**
    * Returns a reference to the first element in the list.
    *
    * Unlike member GPList::begin, which returns an iterator to this same element,
    * this function returns a direct reference.
    *
    * @return A reference (if list-object is not const-qualified) to the first element in the list.
    *
    */
    T& front();

    /**
    * Returns a const-reference to the first element in the list.
    *
    * Unlike member GPList::begin, which returns an iterator to this same element,
    * this function returns a direct reference.
    *
    * @return A const-reference if list-object is const-qualified) to the first element in the list.
    *
    */
    const T& front() const;

    /**
    * Returns a reference to the last element in the list.
    *
    * Unlike member GPList::end, which returns an iterator just pass this element,
    * this function returns a direct reference.
    *
    * @return A reference (if list-object is not const-qualified) to the last element in the list.
    *
    */
    T& back();

    /**
    * Returns a const-reference to the last element in the list.
    *
    * Unlike member GPList::end, which returns an iterator just pass this element,
    * this function returns a direct reference.
    *
    * @return A const-reference (if list-object is const-qualified) to the last element in the list.
    *
    */
    const T& back() const;

    /**
    * Returns the maximum number of elements the list can hold.
    *
    * @return The maximum number of elements the list can hold
    */
    uint16_t maximumSize() const;

    /**
    * Removes all elements from the list, and leaving the list with a size of 0.
    *
    * @return none
    */
    void clear();

    /**
    * Inserts a new element at the beginning of the list, right before its current first element.
    *
    * @param[in]  element    Value to be assigned to the new element.
    * @return     none
    */
    void pushFront(const T& element);

    /**
    * Removes the first element in the list
    *
    * @return   none
    */
    void popFront();

    /**
    * Adds a new element at the end of the list, after its current last element.
    *
    * @param[in]  element    Value to be assigned to the new element.
    * @return     none
    */
    void pushBack(const T& element);

    /**
    * Removes the last element in the list.
    *
    * @return none
    */
    void popBack();

    /**
    * The list is extended by inserting a new element before the element at the specified position.
    *
    * @param[in]  pos      Position in the list where the new element is inserted.
    * @param[in]  element  Value to be assigned to the new element.
    * @return     An iterator that points to the newly inserted element.
    */
    iterator insert(const iterator& pos, const T& element);

    /**
    * Removes a single element from the list.
    *
    * @param[in]  pos      Position in the list where the element is removed.
    * @return     An iterator pointing to the element that followed the erased by the function call.
    */
    iterator erase(const iterator& pos);

    /**
    * Removes a range of elements from the list.
    *
    * The range includes all the elements between first and last,
    * including the element pointed by first but not the one pointed by last.
    *
    * @param[in]  first     First position in the range of elements to be removed.
    * @param[in]  last      Last position in the range of elements to be removed.
    * @return     An iterator pointing to the element that followed the last element erased by the function call.
    *             This is the list end if the operation erased the last element in the sequence.
    */
    iterator erase(const iterator& first, const iterator& last);

    /**
    * Assignment operator
    *
    * @param[in]  arg     A list object of the same type (i.e., with the same template parameters).
    * @return     *this
    */
    GPList& operator =(const GPList& arg);

    /**
    * Reverse the GP list
    *
    * @return    none
    */
    void reverse();

    /**
    * Returns an iterator pointing to the last element in the list container.
    *
    * @return An reverse iterator (if list-object is not const-qualified) to the end of the sequence.
    *
    */
    reverse_iterator rbegin();

    /**
    * Returns an const iterator pointing to the last element in the list container.
    *
    * @return An const reverse iterator (if list-object is const-qualified) to the end of the sequence.
    *
    */
    const_reverse_iterator rbegin() const;
    

    /**
    * Returns an iterator pointing to the theoretical element preceding the first element in the list container.
    *
    * @return An reverse iterator (if list-object is not const-qualified) theoretical element preceding the first element.
    *
    */
    reverse_iterator rend();

    /**
    * Returns an const iterator pointing to the theoretical element preceding the first element in the list container.
    *
    * @return An const reverse iterator (if list-object is const-qualified) theoretical element preceding the first element.
    *
    */
    const_reverse_iterator rend() const;

  private:

    /**
    * The Iterator class needs to access the GPList class (non-const qualified objects)
    */
    friend class GPListIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F>;

    friend class GPListRevIterator<T, maxSize, T&, GPList<T, maxSize, F>*, F>;

    friend class GPListRevIterator<T, maxSize, const T&, const GPList<T, maxSize, F>*, F>;

    /**
    * The Iterator class needs to access the GPList class (const qualified objects)
    */
    friend class GPListIterator<T, maxSize, const T&, const GPList<T, maxSize, F>*, F>;

    uint16_t  frontMarkerFree_;             //!< The next free element in list.
    uint16_t  size_;                        //!< The current number of elements in the list.
    T         list_[maxSize + 1];           //!< The actual array for the stored elements of type T.
    uint16_t  listIndexPrev_[maxSize + 1];  //!< Keeping track of the previous link (index) in the list
    uint16_t  listIndexNext_[maxSize + 1];  //!< Keeping track of the next link (index) in the list.
  };
}

/**
* Include the implementation of the class-template.
*/
#include "gp_list_src.hpp"

#endif

/*************************** end of file **************************************/
