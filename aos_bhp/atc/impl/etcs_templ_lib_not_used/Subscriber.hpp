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
* %name: Subscriber.hpp %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:18 %
* %Creation date of original object: %
*
* Description: This file contains the declaration of the class Subscriber
* All data subscribers shall inherit from the class Subscriber. The Subscriber
* class shall be used together with a SubscriberList. Operations like adding
* a subscriber to the list, or sending new data to the subscribers are done
* via the subscriber list. A SubscriberList only has a pointer to the first
* Subscriber, and all Subscribers have a pointer to the next Subscriber.
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
*
* 1        071004  plofvenb Created first version.
*                           WP70 CR atpcu_se#2396 task atpcu_se#20226. Changed
*                           the subscriber list template, so that the list will
*                           now only point to the first element in the list.
*                           All list subscribers will now have a pointer to the
*                           next subscriber.
*
*******************************************************************************/

#ifndef SUBSCRIBER_HPP
#define SUBSCRIBER_HPP

#include "ba_types.h"

template <typename TYPE>
class Subscriber
{

public:
  Subscriber();
  // These methods shall only be accessed from a SubscriberList. It's not
  // relevant to access them in the context of a real Subscriber instance, i.e.
  // a Subscriber subclass instance shall not access the next Subscriber by
  // using the method getNextSubscriber()
  void setNextSubscriber(TYPE* nextSub);
  TYPE* getNextSubscriber() const;

private:

  TYPE* nextSubscriber_;
  
};


template <typename TYPE>
Subscriber<TYPE>::Subscriber() :
nextSubscriber_(NULL)
{
}


template <typename TYPE>
void
Subscriber<TYPE>::setNextSubscriber(TYPE* nextSub)
{
  nextSubscriber_ = nextSub;
}


template <typename TYPE>
TYPE* 
Subscriber<TYPE>::getNextSubscriber() const
{
  return nextSubscriber_;
}



#endif // SUBSCRIBER_HPP
