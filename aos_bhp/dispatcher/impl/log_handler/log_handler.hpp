#ifndef LogHandler_hpp
#define LogHandler_hpp
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
* Date          Name           Changes
* ---------------------------------------------------------------------------
* 2016-11-03    nsyed          Created
* 2016-11-17    akushwah      updated Prel design
*******************************************************************************/
#include "abstract_log_handler.hpp"
/******************************************************************************
* INCLUDE FILES
******************************************************************************/
namespace Dispatcher
{
  /**
  * The class LogHandler instantiates the abstract class and implements
  * the interfaces needed for both inherited classes and component.
  *
  */
  class LogHandler : public ATC::AbstractLogHandler
  {
  public:

    /**
    * Singleton instance.
    * Only one instance of this class is allowed.
    * @return the one and only instance.
    *
    * NOTE: Singleton handling shall only be used in Adaptation, not Core!
    */
    static LogHandler& instance(void);

  protected:

    /**
    * Adds the application ID, as a string, to the given buffer.
    *
    * @param[out] buffer  The destination buffer
    */
    virtual void addApplicationIdToBuffer(LineBuffer& buffer) const;

  private:

    /**
    * Singleton instance.
    * Declare constructor as private in order to prevent illegal use.
    */
    LogHandler();

    /**
    * Declare copy-constructor as private in order to prevent illegal use.
    */
    LogHandler(const LogHandler&);

    /**
    * Declare assignment-operator as private in order to prevent illegal use.
    */
    LogHandler& operator = (const LogHandler&);
  };
}
#endif
