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
* %name: gp_a_protected_data_area.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:17 %
* %Creation date of original object: Wed Jul 15 08:40:04 1998 %
*
* Description: This file contain the definition of class GP_ProtectedDataAreaA
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
*
* 2        130402  konuddj  Remove Sap parameter in profibus subscription [task 3894]. 
*------------------------------------------------------------------------------------
*  2       070504  rquensel WP60 Task 18196: GP has unnecessary instance method for error handler.
*
*---------------------------------------------------------------
* 10-11    010216  konelb  Changes according to TÜV.
*
* 9        000525  konjko  Removed include of gp_default_error_handler.h.
*
* 8        000111  konraqu Removed default error handler in the template.
*
* 7        981208  sigmsv  Replaced gp_text_msg.h with gp_a_errorinfo.hpp
*
* 6        981111  konjko  Changed error_ to a pointer.
*
* 5        980918  konjko  Default constructor using errorOffset = 0
*
* 4        980915  konjko  Implemented error codes using errorOffset_
*
* 3        980715  konjko  no change
*
* 2        980624  konjko  the T* is used to identify a data area insted of
*                          a sequence number
*
* 1        980605  konjko  created
*
*******************************************************************************/

#ifndef GP_A_PROTECTED_DATA_AREA_H
#define GP_A_PROTECTED_DATA_AREA_H

#ifdef __cplusplus




/*******************************************************************************
* Includes
*******************************************************************************/

#include "vss_a_scalar_types.h"
#include "gp_a_queue.h"
#include "gp_a_errorinfo.hpp"




/*******************************************************************************
* Macros
*******************************************************************************/




/*******************************************************************************
* Global Type definitions, Class definitions and Function prototypes
*******************************************************************************/


//  GP_ProtectedDataAreaA
//-----------------------------------------------------------------------------
//
//  GP_ProtectedDataAreaA is a memory area used to store data of type T. Data
//  is stored using a queue, data is stored in FIFO order. When data is 
//  released a check is made to make sure data is released in FIFO order. The 
//  data area reports errors to F.report().
//
//  INV:    
//        size() >= 0
//
//        size() <= maxSize
//


template <class T, const int16A maxSize, class F>
class GP_ProtectedDataAreaA
{
public:
  // Constructor
  explicit GP_ProtectedDataAreaA(int errorOffset);


  // Inspectors
  boolA                     available() const;
  int16A                    size() const;


  // Mutators
  T*                        getAndOccupy();
  void                      release(T* memory);

private:
  GP_QueueA<T, maxSize, F>  dataArea_;
  int                       errorOffset_;

  GP_ProtectedDataAreaA(const GP_ProtectedDataAreaA& );
  GP_ProtectedDataAreaA& operator= (const GP_ProtectedDataAreaA& );
};




#if 0




//  GP_ProtectedDataAreaA::GP_ProtectedDataAreaA(int errorOffset)
//-----------------------------------------------------------------------------
//
//  Constructs a GP_ProtectedDataAreaA instance.
//
//  POST:
//        size() == 0
//

template <class T, const int16A maxSize, class F>
inline 
GP_ProtectedDataAreaA<T, maxSize, F>::GP_ProtectedDataAreaA(
  int errorOffset);




//  GP_ProtectedDataAreaA::available() const
//-----------------------------------------------------------------------------
//
//  available() returns trueA if there is data area available else it returns 
//  falseA.
//

template <class T, const int16A maxSize, class F> 
inline 
boolA
GP_ProtectedDataAreaA<T, maxSize, F>::available()
  const; 




//  GP_ProtectedDataAreaA::size() const
//-----------------------------------------------------------------------------
//
//  size() returns the number of used data areas.
//

template <class T, const int16A maxSize, class F> 
inline 
int16A
GP_ProtectedDataAreaA<T, maxSize, F>::size()
  const; 




//  GP_ProtectedDataAreaA::getAndOccupy()
//-----------------------------------------------------------------------------
//
//  getAndOccupy() returns a pointer to an available data area and the data 
//  area is occupied until it is released by the user.
//
//  PRE:
//        available() == trueA
//
//  POST:
//        size() == size'() + 1
//

template <class T, const int16A maxSize, class F> 
inline 
T*
GP_ProtectedDataAreaA<T, maxSize, F>::getAndOccupy();




//  GP_ProtectedDataAreaA::release(T* memory)
//-----------------------------------------------------------------------------
//
//  The operation releases data areas in a FIFO order based on the order the 
//  data areas were occupied. The given memory pointer shall match the pointer
//  for the data area to be released.
//
//  PRE:
//        size() != 0
//  
//        memory == pointer to the next element to be released
//
//  POST:
//        size() == size'() - 1
//

template <class T, const int16A maxSize, class F> 
inline 
void
GP_ProtectedDataAreaA<T, maxSize, F>::release(
  T* memory); 


#endif
#include "gp_a_protected_data_area_src.h"


#else
#error C++ compilation required!!!
#endif

#endif /* GP_A_PROTECTED_DATA_AREA_H */



/*************************** end of file **************************************/
