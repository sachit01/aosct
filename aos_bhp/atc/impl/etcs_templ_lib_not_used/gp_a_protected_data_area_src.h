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
* %name: gp_a_protected_data_area_src.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:17 %
* %Creation date of original object: Wed Jul 15 08:39:55 1998 %
*
* Description: The file contain the implementation of class 
*              GP_ProtectedDataAreaA.
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
*
*  eca_2   070504  rquensel WP60 Task 18196: GP has unnecessary instance method for error handler.
*
*---------------------------------------------------------------
*    2     011004  konraqu Added include of "ba_types.h" (NULL).
*
----------------------------------------------------------------------------
*
* 7        990510  konblg  Removed __FILE__ macro.
*
* 6        981111  konjko  Changed error_ to a pointer.
*
* 5        981013  konjko  Remived all inline.
*
* 4        980915  konjko  Implemented error codes using errorOffset_
*
* 3        980715  konjko  In getAndOccupy the pushed element is createted
*                          while it is pushed.
*
* 2        980624  konjko  all class members have ending underscore, error
*                          messages moved to separate file
*
* 1        980605  konjko  created
*
*******************************************************************************/




/*******************************************************************************
* Includes
*******************************************************************************/

#include "ba_types.h"


/*******************************************************************************
* Local macro definitions
*******************************************************************************/




/*******************************************************************************
* Class variables/Constants
*******************************************************************************/


//  GP_ProtectedDataAreaA::GP_ProtectedDataAreaA(int errorOffset)
//-----------------------------------------------------------------------------
//

template <class T, const int16A maxSize, class F>
GP_ProtectedDataAreaA<T, maxSize, F>::GP_ProtectedDataAreaA(
  int errorOffset)
  :
  dataArea_(GP_DA_QUEUE_A),
  errorOffset_(errorOffset)
{
}




//  GP_ProtectedDataAreaA::available()
//-----------------------------------------------------------------------------
//

template <class T, const int16A maxSize, class F> 
boolA
GP_ProtectedDataAreaA<T, maxSize, F>::available() 
  const
{
  return ((dataArea_.full() == trueA) ? falseA : trueA); 
}




//  GP_ProtectedDataAreaA::size()
//-----------------------------------------------------------------------------
//

template <class T, const int16A maxSize, class F> 
int16A
GP_ProtectedDataAreaA<T, maxSize, F>::size() 
  const
{
  return (dataArea_.size()); 
}




//  GP_ProtectedDataAreaA::getAndOccupy()
//-----------------------------------------------------------------------------
//

template <class T, const int16A maxSize, class F> 
T*
GP_ProtectedDataAreaA<T, maxSize, F>::getAndOccupy() 
{
  if(available() == falseA)
  {
    F::report( GP_DA_FULL_A,
                    "",
                    __LINE__);

    return (NULL);    // Return illegal data
  } 
  
  dataArea_.push(T());
  
  return(&dataArea_.back());
}




//  GP_ProtectedDataAreaA::release(T* memory)
//-----------------------------------------------------------------------------
//

template <class T, const int16A maxSize, class F> 
void
GP_ProtectedDataAreaA<T, maxSize, F>::release(
  T* memory) 
{
   
  if(dataArea_.empty() == trueA)
  {
    F::report( GP_DA_EMPTY_A,
                    "",
                    __LINE__);

    return;
  } 
  
  if (memory != &dataArea_.front())
  {
    F::report( GP_DA_WRONG_SEQUENCE_A,
                    "",
                    __LINE__);

    return;
  } 

  dataArea_.pop();
  memory = NULL;
}



/*************************** end of file **************************************/
