#ifndef Console_hpp
#define Console_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class implements the adaptation functionality of Console component.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-04    adgupta     Created
* 2015-11-04    saprasad    Added consolecall prototype for ATP versions command
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_console.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{

  /**
  * The class Console implements the interface defined by the ComponentBase class.
  *
  */

  class Console : public ATC::AbstractConsole
  {
  public:

    /** 
    * Singleton instance.
    * Only one instance of this class is allowed.
    * @return the one and only instance.
    *
    * NOTE: Singleton handling shall only be used in Adaptation, not Core!
    */
    static Console& instance(void);

    /**
    * This functions parses the arguments searches for the "help", "trace" or any other Console
    * component specific command calls and handles it. Returns true if completeley handled
    * else returns false. returning false will let other componets handle the call. help always returns false.
    *
    * @param[in] argc - Number of arguments in the argument array argv
    *
    * @param[in] argv - Arguments array
    *
    * @return - returns true if handled successfully, except "help"(it always returns false)
    */
    virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

  protected:
    
    /**
    * get the iterator of the component to loop while calling consoleCall
    *
    * @return the iterator to the component list acquired from Base component
    */
    virtual ATC::CompPtrIter getComponentIter();

    /**
    * get the last iterator of the component to loop while calling consoleCall
    *
    * @return the last iterator to the component list acquired from Base component
    */
    virtual ATC::CompPtrIter getComponentIterEnd();

    /**
    * write Version information.
    */
    virtual void writeVersion(void);

  private:
    /** 
    * Singleton instance.
    * Declare constructor as private in order to prevent illegal use.
    */
    Console();

    /** 
    * Declare copy-constructor as private in order to prevent illegal use.
    */
    Console(const Console&);

    /** 
    * Declare assignment-operator as private in order to prevent illegal use.
    */
    Console& operator = (const Console&);

    /**
    * Buffer size for console print-outs.
    *
    */
    static const uint16_t consoleBufferSize = 512U;

  };
}

#endif
