#ifndef AbstractApplicationBase_hpp
#define AbstractApplicationBase_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Base class for the ATP application
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-07    lantback    Created
* 2016-12-05    rquensel    Removed Lint-warning
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vector>
#include "atc_base.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATC
{
  class AbstractApplicationBase;

  /** Pointer to BaseComponent type declaration
  */
  typedef BaseComponent* BaseComponentPtr;

  /** The type of container used for the component objects
  */
  typedef std::vector<BaseComponentPtr> CompPtrVector;

  /** The type used for the component objects vector iterator
  */
  typedef std::vector<BaseComponentPtr>::iterator CompPtrIter;

  /**
  * Static variable to store the single instance of AbstractApplicationBase
  *
  * Variable shall be setup during construction of the single instance used within ATP.
  * The variable is returned by corePtr() and used by the core ATP logic to access
  * adaptation objects through the core class.
  *
  * Note: During construction the variable shall be checked to guarante that only
  *       one instance is created. Should the variable be set to non-zero the execution shall
  *       be immediately interrupted and a safe state issued.
  */
  static AbstractApplicationBase* coreAbstractApplicationBasePtr = static_cast<AbstractApplicationBase*>(NULL);

  /**
  * The class AbstractApplicationBase
  *
  */
  class AbstractApplicationBase
  {
  public:

    /**
    * Destructor. Placeholder for derived implementations.
    */
    virtual ~AbstractApplicationBase();

    /**
    * Add component to stored component list
    *
    * Store pointer in maintained component list.
    */
    void addComponent(const BaseComponentPtr comp);

    /**
    * Get iterator of components.
    *
    */
    CompPtrIter getCompIterator();

    /**
    * Get iterator of components.
    *
    */
    CompPtrIter getCompIteratorEnd();

    /**
    * Get core instance pointer
    *
    * @return Pointer to single instance core object.
    */
    static AbstractApplicationBase* corePtr();

    /**
    * Get the Software Block enclosing the Component
    *
    * @return Block Number
    */
    const ComponentBlock getBlockNr() const;

    /**
    * Get the Application name
    *
    * @return application name
    */
    virtual const char_t* getApplicationName() const = 0;

    /**
    * Get the Application Version String
    * Version format: x.y.z. 
    * Where Major Version number = x, Minor Version = y, SubVersion = z
    *
    * @return application version string
    */
    virtual const char_t* getApplicationVersionString() const = 0;

  protected:
    /**
    * Constructor
    */
    AbstractApplicationBase(const ComponentBlock blockNumber);

  private:
    /**
    * Vector of all components as BaseComponent pointers
    */
    CompPtrVector compList;

    /**
    * The Software Block enclosing the Component
    */
    const ComponentBlock blockNr;

    /**
    * Constructor
    */
    AbstractApplicationBase();
  };
}

#endif
