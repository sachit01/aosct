/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The BTM Handler component deals with the interface between the OPC Agent and
*  the AOS SW. This is the implementation of the adaptation for the component
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-01    arastogi    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "btm_handler.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/


/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace IO
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    BTMHandler::BTMHandler(void) : AbstractBTMHandler()
    {
    }

    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    *
    ******************************************************************************/
    BTMHandler& BTMHandler::instance(void)
    {
      static BTMHandler theOnlyBTMHandlerInstance;

      return theOnlyBTMHandlerInstance;
    }

  }
}
