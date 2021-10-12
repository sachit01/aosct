/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  Sample program to demonstrate the features of the GPList and GpListIterator.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-07    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "stdafx.h"

#include <stdint.h>
#include <iostream>

#include "gp_list.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

using namespace std;

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/**
* Temporary error handler to be used with GPList.
*
* Note: This error-handling mechanism will be updated in GPList
*/
class ErrorHandler
{
public:
	static void report(char *str)
  {
		std::cout << str;
	}
};


// Type to be stored in list (in this example, int)
typedef int myType;

// Defining the template parameters for the list. (in this example, max 10 instances of myType)
typedef ATC::GPList<myType, 10, ErrorHandler> myListType;

// Defining the template parameters for the iterator.
typedef ATC::GPListIterator<myType, 10, myType&, myListType*, ErrorHandler> myListIteratorType;

// Print the provided list
void printList(myListType& list);


int _tmain(int argc, _TCHAR* argv[])
{
  myType i = 0;
	myListType myList;

  myListIteratorType myIt = myList.begin();

  // clear()
  cout << "clear()" << endl;
	myList.clear();

  printList(myList);

  // empty()
  cout << "empty(): " << myList.empty() ? "true" : "false";
  cout << endl;

  // maximumSize()
  cout << "maximumSize(): " << myList.maximumSize() << endl;
  myList.clear();

  // pushBack()
  cout << "5 * pushback()" << endl;
	myList.pushBack(i++);
	myList.pushBack(i++);
	myList.pushBack(i++);
	myList.pushBack(i++);
	myList.pushBack(i++);

  printList(myList);

  // size()
  cout << "size(): " << myList.size() << endl;

  // pushBack()
  cout << "5 * pushback()" << endl;
	myList.pushBack(i++);
	myList.pushBack(i++);
	myList.pushBack(i++);
	myList.pushBack(i++);
	myList.pushBack(i++);

  // size()
  cout << "size(): " << myList.size() << endl;

  printList(myList);

  // front()
  cout << "front(): " <<  myList.front() << endl;

  // back()
  cout << "back(): " << myList.back() << endl;

  // full()
  cout << "full(): " << myList.full() ? "true" : "false";
  cout << endl;
  
  // erase()
  myIt = myList.end();
  --myIt;
  --myIt;
  myList.erase(myIt);
  cout << "erase(8) " << endl;

  printList(myList);

  // insert()
  myIt = myList.end();
  --myIt;
  myList.insert(myIt, 8);
  cout << "insert(8) " << endl;

  printList(myList);


  myIt = myList.begin();
  myListIteratorType myIt2 = myList.begin();

  // Iterator ==operator
  if(myIt == myIt2)
  {
    cout << "Iterator compared correct" << endl;
  }
  else
  {
    cout << "Iterator compared NOT correct" << endl;
  }

  ++myIt2;
  ++myIt2;
  ++myIt2;

  // erase(range)
  cout << "erase(0-2) " << endl;
  myList.erase(myIt, myIt2);
  
  printList(myList);

  // popBack()
  cout << "popBack() " << endl;
  myList.popBack();

  printList(myList);

  // popFront()
  cout << "popFront() " << endl;
  myList.popFront();

  printList(myList);

  // pushFront()
  cout << "pushFront(3) " << endl;
  myList.pushFront(3);

  printList(myList);
  
  // Copy Constructor
  cout << "Copy Constructor " << endl;
  myListType myList2(myList);

  printList(myList2);

  // Error cases popFront()/popBack() when empty
  myList.clear();
  myList.popFront();
  cout << endl;
  myList.popBack();
  cout << endl;

	cin.get();

	return 0;
}

void printList(myListType& list)
{
  cout << "Current list: ";

  for(myListIteratorType it = list.begin(); it != list.end(); ++it)
  {
    cout << *it;
  }

  cout << endl;
}
