/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  Replace this text with a short description of the classes etc implemented.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2015-11-13    bhermans    Created
* 2016-03-08    lantback    Updated template
* 2016-03-18    lantback    Minor corrections
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "my_aos_component.hpp"

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
  namespace AOSNamespaceGroup
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    MyAOSComponent::MyAOSComponent(void)
    {
    }

    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    * (This info is not included in doxygen documentation but may be usefull)
    *
    ******************************************************************************/
    MyAOSComponent& MyAOSComponent::instance(void)
    {
      static MyAOSComponent theOnlyMyComponentInstance;

      return theOnlyMyComponentInstance;
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool MyAOSComponent::init(void)
    {
      return true;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void MyAOSComponent::run(void)
    {
    }
  }
}
