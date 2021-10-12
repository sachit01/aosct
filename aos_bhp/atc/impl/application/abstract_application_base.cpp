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
* 2016-11-07    nsyed       Created
* 2016-12-05    rquensel    Added basic checks for NULL
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_application_base.hpp"
#include "atc_util.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/
/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATC
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  AbstractApplicationBase::AbstractApplicationBase(const ComponentBlock blockNumber) : blockNr(blockNumber)
  {
    if (coreAbstractApplicationBasePtr != NULL)
    {
      // Error handler
      aosHalt(__FILE__, __LINE__, "Application base constructor already instantiated");
    }

    // Setup single instance pointer for core access
    coreAbstractApplicationBasePtr = this;
  }

  /******************************************************************************
  * Destructor. Placeholder for derived implementations.
  ******************************************************************************/
  AbstractApplicationBase::~AbstractApplicationBase()
  {
  }

  /******************************************************************************
  * addComponent
  ******************************************************************************/
  void AbstractApplicationBase::addComponent(const BaseComponentPtr comp)
  {
    if (comp == NULL)
    {
      aosHalt(__FILE__, __LINE__, "Base Component is NULL");
    }
    else
    {
      compList.push_back(comp);
    }
  }

  /******************************************************************************
  * getCompIterator
  ******************************************************************************/
  CompPtrIter AbstractApplicationBase::getCompIterator()
  {
    return compList.begin();
  }

  /******************************************************************************
  * getCompIteratorEnd
  ******************************************************************************/
  CompPtrIter AbstractApplicationBase::getCompIteratorEnd()
  {
    return compList.end();
  }

  /******************************************************************************
  * corePtr
  ******************************************************************************/
  AbstractApplicationBase* AbstractApplicationBase::corePtr(void)
  {
    return coreAbstractApplicationBasePtr;
  }

  /******************************************************************************
  * getBlockNr
  ******************************************************************************/

  const ComponentBlock AbstractApplicationBase::getBlockNr() const
  {
    return blockNr;
  }
}
