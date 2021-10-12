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
* 2016-04-22    lantback    Created
* 2016-04-27    lantback    Corrected namespace of Decode
* 2016-05-06    bhermans    Added simulators to ATP
* 2016-06-10    adgupta     Added get and set functions to acquire VIOH Client Handler
* 2016-07-26    adgupta     Added console related calls
* 2016-08-10    akushwah    Added Log Handler related calls
* 2016-09-05    arastogi    Added the init function and removed adding components
                            in instance function.
* 2016-09-06    arastogi    Added btm handler related calls
* 2016-09-09    adgupta     Added dmi related calls
* 2016-09-13    jeneman     Added config related calls
* 2016-08-10    akushwah    corrected the Console related calls
* 2016-09-21    arastogi    added function to add all components and moved the functionality
*                           out of init()
* 2016-09-30    bhidaji     Added target_calculation
* 2016-10-05    bhidaji     Added supervise
* 2016-12-05    rquensel    Added call to Cross Compare Handler init
* 2016-12-15    saprasad    Added Analyzer IF Handler init and run calls 

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "atc_math.hpp"
#include "atp_application.hpp"
#include "basic_ip.hpp"
#include "config.hpp"
#include "console.hpp"
#include "brake.hpp"
#include "decode.hpp"
#include "event_handler.hpp"
#include "loco_io.hpp"
#include "message_handler.hpp"
#include "odometry.hpp"
#include "position.hpp"
#include "radio_handler.hpp"
#include "dmi_handler.hpp"
#include "targets.hpp"
#include "tracks.hpp"
#include "tsetup.hpp"
#include "mode_control.hpp"
#include "log_handler.hpp"
#include "btm_handler.hpp"
#include "target_calculation.hpp"
#include "supervise.hpp"
#include "cross_compare.hpp"
#include "cross_compare_array.hpp"
#include "analyzer_if.hpp"
#include "vehicle_com.hpp"
#include "tims.hpp"
#include "tic.hpp"
#include "abstract_log_handler.hpp"
#include "atp_version.hpp"

#include <cstdio>

#ifdef WIN32
extern "C" int64_t vfwGetHighResolutionClock(void);
#else
#include <vfw_time.h>
#endif

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

namespace
{
  const char_t atpFullVersionString[] = ATP_VERSION_STRING;
  const char_t atpApplicationName[] = ATP_APPLICATION_NAME;
  const char_t binaryIdentificationString[] = ATP_APPLICATION_NAME " VERSION=" ATP_VERSION_STRING;
}


/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    ATPApplication::ATPApplication(void): AbstractATPApplication::AbstractATPApplication(),
      initState(initConfig),
      viohClientHandle(static_cast<VIOHnames::VIOHClient*>(NULL)),
      lastCycleExecutionTime(0),
      minExecutionTime(ATC::int32Max),
      maxExecutionTime(0)
    {
      memset(&dispatcherVersionString[0U], 0, sizeof(dispatcherVersionString));

#ifdef _SIL
      // Maybe this should be simulated in SIL?
      strncpy(&dispatcherVersionString[0U], &expectedDispatcherVersion[0], sizeof(dispatcherVersionString));
#endif

    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    void ATPApplication::initCrossCompare() const
    {
      // Add all vital attributes to CC
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareArray<char_t>(&dispatcherVersionString[0],
        dispatcherVersionLength));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&initState));
      // Note, exec times shall not be sent to CC, they are not vital (could diff) and could be reset by console command (min/max).
    }

    /******************************************************************************
    * addAllComponents
    ******************************************************************************/
    void ATPApplication::addAllComponents(void)
    {
      // Create all adaptation components at this point
      addComponent(&Console::instance());
      addComponent(&Support::CrossCompare::instance());
      addComponent(&BasicIP::instance());
      addComponent(&Config::instance());
      addComponent(&AnalyzerIF::instance());
      addComponent(&DS::Tracks::instance());
      addComponent(&DS::Targets::instance());
      addComponent(&DS::TSetup::instance());
      addComponent(&ATC::EventHandler::instance());
      addComponent(&LogHandler::instance());
      addComponent(&IO::LocoIO::instance());
      addComponent(&Kernel::MessageHandler::instance());
      addComponent(&Kernel::ModeControl::instance());
      addComponent(&Pos::Decode::instance());
      addComponent(&Pos::Odometry::instance());
      addComponent(&Pos::Position::instance());
      addComponent(&RadioCom::RadioHandler::instance());
      addComponent(&DMICom::DMIHandler::instance());
      addComponent(&Supv::TargetCalculation::instance());
      addComponent(&Supv::Supervise::instance());
      addComponent(&Supv::Brake::instance());
      addComponent(&IO::BTMHandler::instance());
      addComponent(&TG::VehicleCom::instance());
      addComponent(&TG::TIMS::instance());
      addComponent(&TG::TIC::instance());

      //Initialize non component classes
      static_cast<void>(ATC::ATCMath::instance());
      static_cast<void>(Supv::BrakeCalculations::instance());
    }

    /******************************************************************************
    * preInit
    ******************************************************************************/
    void ATPApplication::preInit()
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
    bool ATPApplication::init(void)
    {
      static_cast<void>(AbstractATPApplication::init());
      bool notReady = false;
      bool ret = false;

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
        // Init the logging and debug components
        notReady = (!ATP::AnalyzerIF::instance().init());
        notReady = (!Console::instance().init()) || notReady;
        notReady = (!Support::CrossCompare::instance().init()) || notReady;
        notReady = (!BasicIP::instance().init()) || notReady;
        notReady = (!ATP::LogHandler::instance().init()) || notReady;
        notReady = (!ATC::EventHandler::instance().init()) || notReady;

        if (!notReady)
        {
          //need to run Basic IP here as basic IP needs to run a few times
          //before channels are connected.
          ATC::AbstractBasicIP::corePtr()->run();
          initState = initStage2;
        }
        break;

      case initStage2:
        // Run the logging and debug components
        ATC::AbstractConsole::corePtr()->run();
        ATC::AbstractBasicIP::corePtr()->run();
        ATC::AbstractEventHandler::corePtr()->run();
        ATC::AbstractLogHandler::corePtr()->run();
        ATC::AbstractAnalyzerIF::corePtr()->run();

        // Cross Compare Input
        Support::AbstractCrossCompare::corePtr()->runIn();

        // Call initialization of all components until all done

        notReady = (!DS::Tracks::instance().init());
        notReady = (!DS::Targets::instance().init()) || notReady;
        notReady = (!DS::TSetup::instance().init()) || notReady;
        notReady = (!IO::LocoIO::instance().init()) || notReady;
        notReady = (!Kernel::MessageHandler::instance().init()) || notReady;
        notReady = (!Kernel::ModeControl::instance().init()) || notReady;
        notReady = (!Pos::Decode::instance().init()) || notReady;
        notReady = (!Pos::Position::instance().init()) || notReady;
        notReady = (!Pos::Odometry::instance().init()) || notReady;
        notReady = (!RadioCom::RadioHandler::instance().init()) || notReady;
        notReady = (!DMICom::DMIHandler::instance().init()) || notReady;
        notReady = (!Supv::TargetCalculation::instance().init()) || notReady;
        notReady = (!Supv::Supervise::instance().init()) || notReady;
        notReady = (!Supv::Brake::instance().init()) || notReady;
        notReady = (!IO::BTMHandler::instance().init()) || notReady;
        notReady = (!TG::VehicleCom::instance().init()) || notReady;
        notReady = (!TG::TIMS::instance().init()) || notReady;
        notReady = (!TG::TIC::instance().init()) || notReady;

        // Cross Compare
        Support::AbstractCrossCompare::corePtr()->runOut();

        if (!notReady)
        {
          initCrossCompare();
          Support::CrossCompare::instance().initDone();

          // Consider initialization finished
          initState = initDone;

          char_t buffer[200];
          //lint -esym(586,snprintf) snprintf is needed here
          const int32_t res = snprintf(&buffer[0], sizeof(buffer), "%s: Init done!", binaryIdentificationString);

          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(buffer)))
          {
            ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, &buffer[0], "AP", __FILE__, __LINE__);
          }

        }
        break;

      case initDone:
#ifdef GENERATE_TCC_EVENTLIST_XML_PATH

        // All events are initialized (and put in a csv-file). They can now be converted to XML and XLSX format.
        convertEventListToXmlAndXlsx();
#endif

        ret = true;
        break;

      default:
        break;
      }

      return ret;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void ATPApplication::run(void)
    {

      // Run or initialize
      if (initState == initDone)
      {
        int64_t timeBeforeRun = vfwGetHighResolutionClock();

        // Process all components
        AbstractATPApplication::run();

        int64_t timeAfterRun = vfwGetHighResolutionClock();

        // Store the execution time as milliseconds, round correctly
        lastCycleExecutionTime = ATC::ATCMath::instance().signDiv(timeAfterRun - timeBeforeRun, 1000000, __FILE__, __LINE__);

        // Update min execution time
        if (lastCycleExecutionTime < minExecutionTime)
        {
          minExecutionTime = lastCycleExecutionTime;
        }

        // Update max execution time
        if (lastCycleExecutionTime > maxExecutionTime)
        {
          maxExecutionTime = lastCycleExecutionTime;
        }
      }
      else
      {
        //Error.. should never come here.
      }
  
    }

    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    * (This info is not included in doxygen documentation but may be useful)
    *
    ******************************************************************************/
    ATPApplication& ATPApplication::instance(void)
    {
      static ATPApplication theOnlyATMainInstance;

      return theOnlyATMainInstance;
    }

    /*******************************************************************************
    *setVIOHClientHandle
    *
    *Set the values of pointer in ATP Application.
    *
    ********************************************************************************/
    void ATPApplication::setVIOHClientHandle(VIOHnames::VIOHClient* const handle)
    {
      viohClientHandle = handle;
    }

    /*******************************************************************************
    *getVIOHClientHandle
    *
    *Get the values of pointer from ATP Application.
    *
    ********************************************************************************/
    VIOHnames::VIOHClient* ATPApplication::getVIOHClientHandle()
    {
      return viohClientHandle;
    }


    /******************************************************************************
    * getLastExecTime
    ******************************************************************************/
    int32_t ATPApplication::getLastExecTime() const
    {
      return lastCycleExecutionTime;
    }

    /******************************************************************************
    * getMinExecTime
    ******************************************************************************/
    int32_t ATPApplication::getMinExecTime() const
    {
      return minExecutionTime;
    }

    /******************************************************************************
    * getMaxExecTime
    ******************************************************************************/
    int32_t ATPApplication::getMaxExecTime() const
    {
      return maxExecutionTime;
    }

    /******************************************************************************
    * resetExecTimes
    ******************************************************************************/
    void ATPApplication::clearExecTimes(void)
    {
      minExecutionTime = 255;
      maxExecutionTime = 0;
    }

    /******************************************************************************
    * getApplicationName
    ******************************************************************************/
    const char_t* ATPApplication::getApplicationName() const
    {
      return atpApplicationName;
    }

    /******************************************************************************
    * getApplicationVersionString
    ******************************************************************************/
    const char_t* ATPApplication::getApplicationVersionString() const
    {
      return atpFullVersionString;
    }

    /******************************************************************************
    * setDispatcherVersionString
    ******************************************************************************/
    const void ATPApplication::setDispatcherVersionString(const char_t * const versionString)
    {
      strncpy(&dispatcherVersionString[0U], versionString, dispatcherVersionLength);
    }

    /******************************************************************************
    * getDispatcherVersionString
    ******************************************************************************/
    const char_t* ATPApplication::getDispatcherVersionString() const
    {
      return dispatcherVersionString;
    }

    /******************************************************************************
    * validateDispatcherVersion
    ******************************************************************************/
    bool ATPApplication::validateDispatcherVersion() const
    {
      const bool dispatcherVersionValidated = strncmp(&dispatcherVersionString[0U], &expectedDispatcherVersion[0], dispatcherVersionLength) == 0;

      if (!dispatcherVersionValidated)
      {
        char_t buffer[100];

        const int32_t res = snprintf(&buffer[0], sizeof(buffer), "Dispatcher version mismatch: %s, expecting: %s", &dispatcherVersionString[0U],
          &expectedDispatcherVersion[0U]);

        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(buffer)))
        {
          ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, &buffer[0], "AP", __FILE__, __LINE__);
        }
      }

      return dispatcherVersionValidated;
    }

#ifdef GENERATE_TCC_EVENTLIST_XML_PATH
    static char_t commandLine[1024];

    static void executeCommand(const int32_t res)
    {
      if ((res > 0) && (static_cast<size_t>(res) < sizeof(commandLine)))
      {
        const int32_t res2 = system(commandLine);
        if (res2 != 0)
        {
          exit(res2);
        }
      }
      else
      {
        ATC::debugInfo("executeCommand: snprintf() failed\n");
        exit(-1);
      }
    }

    /******************************************************************************
    * convertEventListToXmlAndXlsx
    ******************************************************************************/
    void ATPApplication::convertEventListToXmlAndXlsx() const
    {
      ATC::debugInfo("Generating events files...\n");

      // Convert CSV-file to xlsx-file
      int32_t res = snprintf(commandLine, sizeof(commandLine), "%s %s %s %s",
        ATC::registerEventPythonCommand,
        ATC::registerEventPythonToXlsxScriptFile,
        ATC::registerEventCsvFile,
        ATC::registerEventXlsFile);
      executeCommand(res);

      // Convert CSV-file to XML-file
      res = snprintf(commandLine, sizeof(commandLine), "%s %s %s %s",
        ATC::registerEventPythonCommand,
        ATC::registerEventPythonToXmlScriptFile,
        ATC::registerEventCsvFile,
        ATC::registerEventXmlFile);
      executeCommand(res);

      // Validate XML-schema with xmllint on the generated XML-file.
      res = snprintf(commandLine, sizeof(commandLine), "%s --noout --schema %s %s >> %s.ErrorLog 2>&1",
        ATC::registerEventXmlLintFile,
        ATC::registerEventPythonToXmlSchemaFile,
        ATC::registerEventXmlFile,
        ATC::registerEventXmlFile);
      executeCommand(res);

      // Delete CSV-file
      res = snprintf(commandLine, sizeof(commandLine), "%s %s",
        ATC::registerEventDeleteCommand,
        ATC::registerEventCsvFile);
      executeCommand(res);

      ATC::debugInfo("Done.\n");

      exit(0);
    }
#endif

  }
}
