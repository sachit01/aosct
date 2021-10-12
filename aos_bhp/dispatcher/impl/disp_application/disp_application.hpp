#ifndef DispApplication_hpp
#define DispApplication_hpp
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
* 2016-11-03    nsyed    Created
* 2016-12-29    spandita  Added preinit declaration
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_application_base.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace Dispatcher
{
  class DispApplication : public ATC::AbstractApplicationBase
  {
  public:

    /**
    * Implements the virtual pre-init function.
    *
    * This function should call preInit functions for all components.
    * Should be called after add components.
    */
    virtual void preInit();

    /**
    * The init function.
    *
    * @return true when initialization completed
    */
    bool init(void);

    /**
    * The run function.
    */
    void run(void) const;

    /**
    * Singleton instance.
    * Only one instance of this class is allowed.
    * @return the one and only instance.
    *
    * NOTE: Singleton handling shall only be used in Adaptation, not Core!
    */
    static DispApplication& instance(void);

    /**
    * Create objects for all components and add it to component list
    */
    void addAllComponents(void);

    /**
    * Get the Application name
    *
    * @return application name
    */
    virtual const char_t* getApplicationName() const;

    /**
    * Get the Application Version String
    *
    * @return application version string
    */
    virtual const char_t* getApplicationVersionString() const;

  private:

    /**
    * Singleton instance.
    * Declare constructor as private in order to prevent illegal use.
    */
    DispApplication();

    /**
    * The current initialization state of DispApplication.
    */
    uint8_t initState;

    /**
    * The initialization state to initialize Config.
    */
    static const uint8_t initConfig = 1U;

    /**
    * The initialization state to initialize the logging and debug components.
    */
    static const uint8_t initStage1 = 2U;

    /**
    * The initialization state to initialize the components
    */
    static const uint8_t initStage2 = 3U;

    /**
    * The final state of initialization.
    */
    static const uint8_t portInitDone = 4U;

  };

}
#endif
