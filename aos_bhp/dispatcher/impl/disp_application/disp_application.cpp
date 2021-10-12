/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The class will call init and run functions for all the dispatcher components.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-03    nsyed    Created
* 2016-11-23    saprasad Added console and config related calls(init & run)
* 2016-11-25    saprasad Added log,event related calls(init & run)
* 2016-11-26    saprasad Added DispatcherBlock const to remove compilation error in Linux
* 2016-12-29    spandita Added preinit and message dispatcher
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "basic_ip.hpp"
#include "config.hpp"
#include "console.hpp"
#include "log_handler.hpp"
#include "event_handler.hpp"
#include "disp_application.hpp"
#include "message_dispatcher.hpp"
#include "atc_util.hpp"
#include <cstdio>
#include "dispatcher_version.hpp"


/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/
namespace
{
  const char_t dispatcherFullVersionString[] = DISPATCHER_VERSION_STRING;
  const char_t dispatcherApplicationName[] = DISPATCHER_APPLICATION_NAME;
  const char_t binaryIdentificationString[] = DISPATCHER_APPLICATION_NAME " VERSION=" DISPATCHER_VERSION_STRING;
}


/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace Dispatcher
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  DispApplication::DispApplication(void)
    :initState(initConfig), AbstractApplicationBase(ATC::DispatcherBlock)
  {

  }

  /******************************************************************************
  * addAllComponents
  ******************************************************************************/
  void DispApplication::addAllComponents(void)
  {
    // Create all Dispatcher components at this point
    addComponent(&Console::instance());
    addComponent(&BasicIP::instance());
    addComponent(&Config::instance());
    addComponent(&EventHandler::instance());
    addComponent(&LogHandler::instance());
    addComponent(&MessageDispatcher::instance());
  }

  /******************************************************************************
  * getApplicationName
  ******************************************************************************/
  const char_t* DispApplication::getApplicationName() const
  {
    return dispatcherApplicationName;
  }

  /******************************************************************************
  * getApplicationVersionString
  ******************************************************************************/
  const char_t* DispApplication::getApplicationVersionString() const
  {
    return dispatcherFullVersionString;
  }

  /******************************************************************************
  * preInit
  ******************************************************************************/
  void DispApplication::preInit()
  {
    ATC::CompPtrIter compListItr = getCompIterator();
    ATC::CompPtrIter compListEnd = getCompIteratorEnd();
    if (compListItr != compListEnd)
    {
      while (compListItr != compListEnd)
      {
        (*compListItr)->preInit();
        ++compListItr;
      }
    }
    else
    {
      ATC::aosHalt(__FILE__, __LINE__, "Add components not called before preInit");
    }
  }

  /******************************************************************************
  * init
  ******************************************************************************/
  bool DispApplication::init(void)
  {
    bool result = false;

    switch (initState)
    {
    case initConfig:

      // Initialize Config before all others, so they can get their configuration values.
      // Might need to run several times while reading data from NVSH.
      if (Config::instance().init())
      {
        initState = initStage1;
      }
      break;

    case initStage1:

      // Init the components
      if (Console::instance().init())
      {
        if (BasicIP::instance().init())
        {
          if (EventHandler::instance().init())
          {
            if (LogHandler::instance().init())
            {
              initState = initStage2;
            }
          }
        }
      }
      break;

    case initStage2:

      // Run the logging and debug components
      Console::instance().run();
      BasicIP::instance().run();
      Config::instance().run();
      EventHandler::instance().run();
      LogHandler::instance().run();

      // Call initialization of message dispatcher
      if (MessageDispatcher::instance().init())
      {
        initState = portInitDone;
        char_t buffer[200];
        //lint -esym(586,snprintf) snprintf is needed here
        const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%s: Init done!", binaryIdentificationString);

        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, &buffer[0], "DP", __FILE__, __LINE__);
        }

      }
      break;

    case portInitDone:
      result = true;
      break;

    default:
      break;

    }

    return result;
  }

  /******************************************************************************
  * run
  ******************************************************************************/
  void DispApplication::run(void) const
  {
    // Run or initialize
    if (initState == portInitDone)
    {
      //Process main loop
      Console::instance().run();
      BasicIP::instance().run();
      Config::instance().run();
      EventHandler::instance().run();
      LogHandler::instance().run();
      MessageDispatcher::instance().runIn();
      MessageDispatcher::instance().runOut();
    }
    else
    {
      ATC::aosHalt(__FILE__, __LINE__, "Init not done");
    }
  }

  /******************************************************************************
  * instance
  *
  * Add additional functional description here if needed.
  * (This info is not included in doxygen documentation but may be useful)
  *
  ******************************************************************************/
  DispApplication& DispApplication::instance(void)
  {
    static DispApplication theOnlyDispApplicationInstance;

    return theOnlyDispApplicationInstance;
  }
}
