#ifndef AbstractATPApplication_hpp
#define AbstractATPApplication_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Main executable ans scheduling component of ATP.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-22    lantback    Created
* 2016-07-06    adgupta     Updated functions to return iterators of component list
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vector>
#include "atc_base.hpp"
#include "abstract_application_base.hpp"
#include <vfw_sync.h>
#include "vio_types.h"
#include "vioh_client.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
      /**
      * The class AbstractATPApplication implements the interface defined by the AbstractApplicationBase class.
      *
      */
      class AbstractATPApplication : public ATC::AbstractApplicationBase
      {
      public:

        /**
        * ATP Application Cycle time in ms
        * (Will also been used as VIOH client task cycle)
        */
        static const uint32_t atpAppCycleTime = 100U;

        /**
        * Implements the virtual init function.
        * (If needed)
        *
        * @return Returns true when initialization completed
        */
        virtual bool init(void);

        /**
        * Implements the virtual run function.
        *
        * A call to the run() method will schedule a full ATP execution cycle. Scheduling is
        * based on the core (abstract) classes.
        */
        virtual void run(void);

        /**
        * Get core instance pointer
        *
        * @return Pointer to single instance core object.
        */
        static AbstractATPApplication* corePtr();

        /**
        * Get the vital I/O Handle
        */
        virtual VIOHnames::VIOHClient* getVIOHClientHandle() = 0;

        /**
        * Implements the specific adaptation function of the version validation of the dispatcher versions
        * @return true if the version is validated
        */
        virtual bool validateDispatcherVersion() const = 0;

      protected:

        /**
        * Constructor
        */
        AbstractATPApplication();

      };
    }
  }


#endif
