#ifndef BTMHandler_hpp
#define BTMHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The BTM Handler component deals with the interface between the OPC Agent and the AOS SW.
*  This is the declaration of the adaptation for the component
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-01    arastogi    Created
* 2016-09-23    adgupta     Implementation for BTM Handler
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_btm_handler.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace IO
  {
    /**
    * The class BTMHandler instantiates the abstract class and implements
    * the interfaces needed for both inherited classes and component.
    *
    */
    class BTMHandler : public AbstractBTMHandler
    {
    public:

      /** 
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static BTMHandler& instance(void);

    protected:

    private:

      /** 
      * Constructor. Declared as private in order to prevent illegal use.
      */
      BTMHandler();

      /** 
      * Copy constructor. Declared as private in order to prevent illegal use.
      */
      BTMHandler(const BTMHandler&);

    };
  }
}
#endif
