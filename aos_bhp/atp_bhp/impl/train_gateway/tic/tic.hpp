#ifndef TIC_hpp
#define TIC_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines the TIC class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-06-12    nsyed       Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_tic.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace TG
  {
    /**
    * The class TIC instantiates the abstract class and implements
    * the interfaces needed for both inherited classes and component.
    *
    */
    class TIC : public AbstractTIC
    {
    public:

      /**
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static TIC& instance(void);

      /**
      * Returns the availability of locomotive orientation.
      *
      * @return true if locomotive orientation is provided by TIC
      */
      virtual bool getLocoOrientationAvailable() const;

    protected:

      /**
      * Implements the virtual evaluateTICAvailable function.
      *
      * @return true if TIC is available.
      */
      virtual bool evaluateTICAvailable();

      /**
      * Implements the virtual evaluateConfigReqStatus function.
      *
      * @return Automatic Train configuration request status.
      */
      virtual TICConfigReqStatus evaluateConfigReqStatus();

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:
 
      /**
      * Constructor
      */
      TIC();

      /** The time when TIC Initialization started (reference time, ms)
      */
      int64_t ticInitStartTime;
    };
  }
}

#endif
