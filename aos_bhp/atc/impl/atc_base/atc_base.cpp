/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*
* DESCRIPTION: 
*  Defines base-classes for the all components within ATC/ATP/ATO.
*
*****************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-21    adgupta     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "abstract_log_handler.hpp"
#include <vfw_string.h>

/******************************************************************************
* DECLARATIONS    
******************************************************************************/

namespace ATC
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  BaseComponent::BaseComponent(const ComponentIDType id, const char_t* const compName, const char_t* const shortCompName) :
    trace(shortCompName, 0U, "Default component trace object\n")
  {
    compId = id;
    static_cast<void>(vfw_strlcpy(&name[0], compName, maxComponentNameLength));
    static_cast<void>(vfw_strlcpy(&shortName[0], shortCompName, maxComponentShortNameLength));
  };
  
  /******************************************************************************
  * writeToLog
  ******************************************************************************/
  void BaseComponent::writeToLog(LogLevel const level, const char_t* const text,
    const char_t* const filepath, const int32_t line) const
  {
    AbstractLogHandler::corePtr()->writeToLog(level, text, getShortName(),filepath, line);
  }

  /******************************************************************************
  * writeToLog
  ******************************************************************************/
  void BaseComponent::writeToLog(LogLevel const level, const char_t* const text, const uint32_t val,
    const char_t* const filepath, const int32_t line) const
  {
    AbstractLogHandler::corePtr()->writeToLog(level, text, val, getShortName(),filepath, line);
  }

  /******************************************************************************
  * writeToLog 
  ******************************************************************************/
  void BaseComponent::writeToLog(LogLevel const level, const char_t* const text, const int32_t val,
    const char_t* const filepath, const int32_t line) const
  {
    AbstractLogHandler::corePtr()->writeToLog(level, text, val, getShortName(), filepath, line);
  }

  /******************************************************************************
  * writeToLog
  ******************************************************************************/
  void BaseComponent::writeToLog(LogLevel const level, const char_t* const text, const bool val,
    const char_t* const filepath, const int32_t line) const
  {
    AbstractLogHandler::corePtr()->writeToLog(level, text, val, getShortName(), filepath, line);
  }

  /******************************************************************************
  * getId
  ******************************************************************************/
  ComponentIDType BaseComponent::getId() const
  {
    return compId;
  }

  /******************************************************************************
  * getName
  ******************************************************************************/
  const char_t* BaseComponent::getName() const
  {
    return name;
  }

  /******************************************************************************
  * getShortName
  ******************************************************************************/
  const char_t* BaseComponent::getShortName() const
  {
    return shortName;
  }

  /******************************************************************************
  * getTrace
  ******************************************************************************/
  TraceInterface* BaseComponent::getTrace()
  {
    return &trace; //lint !e1536 Low access member is exposed by design
  }

  /******************************************************************************
  * consoleCall
  ******************************************************************************/
  bool BaseComponent::consoleCall(const uint32_t /*argc*/, const ConsoleArguments /*argv*/)
  {
    return false;
  }

  /******************************************************************************
  * preInit
  ******************************************************************************/
  void BaseComponent::preInit()
  {
    //Do Nothing..
  }

  /******************************************************************************
  * Constructor - PocComponent
  ******************************************************************************/
  ProcComponent::ProcComponent(const ComponentIDType id, const char_t* const compName, const char_t* const shortCompName) :
                 BaseComponent(id, compName, shortCompName)
  {
  }

  /******************************************************************************
  * Constructor - IOComponent
  ******************************************************************************/
  IOComponent::IOComponent(const ComponentIDType id, const char_t* const compName, const char_t* const shortCompName) :
    BaseComponent(id, compName, shortCompName)
  {
  }
}
