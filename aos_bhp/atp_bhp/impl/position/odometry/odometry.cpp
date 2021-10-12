/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file implements the Odometry class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-01    arastogi    Created
* 2016-09-19    akushwah    Corrected Init function
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "odometry.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace Pos
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    Odometry::Odometry(void):AbstractOdometry()
    {
    }

    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    * (This info is not included in doxygen documentation but may be usefull)
    *
    ******************************************************************************/
    Odometry& Odometry::instance(void)
    {
      static Odometry theOnlyOdometryInstance;

      return theOnlyOdometryInstance;
    }

  }
}
