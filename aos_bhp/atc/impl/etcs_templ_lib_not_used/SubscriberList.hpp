/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation, 2001
*
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without express authority is strictly forbidden.
*
* Module name: Template Library
*
* %name: SubscriberList.hpp %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:18 %
* %Creation date of original object: 20-okt-2004 14:53:35 %
*
* Description: Declaration of the class SubscriberList
*
*******************************************************************************/

/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
*
* 6        071015  plofvenb WP70 CR atpcu_se#2396 task atpcu_se#20226. Added
*                           initial value for firstSubscriber_.
*
* 5        071004  plofvenb WP70 CR atpcu_se#2396 task atpcu_se#20226. Changed
*                           the subscriber list template, so that the list will
*                           now only point to the first element in the list.
*                           All list subscribers now have a pointer to the next
*                           Subscriber, and the SubscriberList use the next
*                           pointer to traverse the list. This way the list
*                           sizes don't need to specified.
* 4        070413  rquensel WP60 atpcu_se#17910: Subscriber should not subscribe more than once
* 3        070110  mhagdahl Removed TL_ASSERT_A, changed to report an Unconditional Stopping Failure and  
*                           called it if count >= SIZE in addSubscriber (CR atpcu_se#3806, task 16276).
* 2        061212  mhagdahl Added TL_ASSERT_A and called it if not count < SIZE
*                           in addSubscriber (CR atpcu_se#3806, task 16066).         
*
*
*******************************************************************************/

#if !defined(EA_882B1876_1E67_4543_9254_24AD6B4F47E1__INCLUDED_)
#define EA_882B1876_1E67_4543_9254_24AD6B4F47E1__INCLUDED_

/**
 * This class has been code-reviewed and approved by sigkrso
 */



#include "IErrorHandlerA.hpp"

#include "gp_a_errorinfo.hpp"
#include "Subscriber.hpp"


/**
 * Base class for containers of observer subscribers. Sub-class this class with an
 * instantiation using your Subscriber-type as TYPE, the argument type of the
 * receive-operation as DATA, max no of subscribers as SIZE and implement
 * informSubscriber to call the receive-operation.
 */
template<typename TYPE, typename DATA>
class SubscriberList
{

public:
  SubscriberList();
  void addSubscriber(TYPE* subscriber, int slErrorCode = 0xBC00);
  void informSubscribers(DATA data);
  virtual void informSubscriber(TYPE* subscriber, DATA data) =0;
  
private:
  TYPE* firstSubscriber_;
  int slErrorCode_;
  
};

template<typename TYPE, typename DATA>
void 
SubscriberList<TYPE,DATA>::addSubscriber(TYPE* newSub, int slErrorCode)
{

  slErrorCode_ = slErrorCode; // If the caller provided a special error code,
                              // store it and use it later if needed. 
  TYPE* curSub = firstSubscriber_; // Traverses the subscriber list
  boolA alreadyInserted(falseA);

  while (curSub != NULL) // When curSub is NULL, we have traversed the list
  {
    if (curSub == newSub) // Check if the Subscriber already was in the list
    {
      // It was already in the list. Report Unconditional stopping failure
      reportErrorA(TL_SL_ALREADY_IN_LIST_A,ATP::referenceTimeA());
      alreadyInserted = trueA;
    }
    else
    {
      // Step to the next Subscriber in the list
      curSub = curSub->getNextSubscriber();
    }
  }

  if (alreadyInserted == falseA)
  {
    // Let the new subscriber point to the current beginning of the list. 
    newSub->setNextSubscriber(firstSubscriber_);
    // Let the new Subscriber be the beginning of the list
    firstSubscriber_ = newSub;
  }
  else
  {
    // Do nothing. Error is already reported
  }
}


template<typename TYPE, typename DATA>
void 
SubscriberList<TYPE,DATA>::informSubscribers(DATA data)
{
  TYPE* curSub = firstSubscriber_; // Traverses the subscriber list

  while (curSub != NULL) // When curSub is NULL, we have traversed the list
  {
    informSubscriber(curSub, data);
    // Step to the next Subscriber in the list
    curSub = curSub->getNextSubscriber();
  }
}

template<typename TYPE, typename DATA>
SubscriberList<TYPE,DATA>::SubscriberList()
  :
  firstSubscriber_(NULL),
  slErrorCode_(0xBC00)
{
}



#endif // !defined(EA_882B1876_1E67_4543_9254_24AD6B4F47E1__INCLUDED_)
