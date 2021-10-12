#ifndef LocationModeBHP_hpp
#define LocationModeBHP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the location mode adaptation part for BHP.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-05-15    spandita    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "location_mode.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    /**
    * The class LocationModeBHP defines the Adaptation Location mode.
    *
    */
    class LocationModeBHP : public LocationMode
    {

    public:

      /**
      * Constructor.
      *
      */
      LocationModeBHP();

    protected:

      /**
      * To manage the Free Rolling train state.
      * @param[in] commonData   reference to train states
      */
      virtual void manageFreeRolling(CommonDataForModes &commonData);

      /**
      * To manage finish sub state of location mode
       * @param[in] commonData   reference to train states
      */
      virtual void runLocFinishOK(CommonDataForModes &commonData);

      /**
      * To manage Handling done flag
      * @param[in] commonData   reference to train states
      */
      virtual void manageHandlingDone(CommonDataForModes &commonData);
    };
  }
}

#endif

