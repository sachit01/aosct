/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file defines AtpMain implementation
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
*2017-01-16   spandita       Implemented the AtpMain Functionalities
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#ifndef __GNUG__
#define __attribute__(A) /* If not GNU, do nothing. This must be defined before including vfw_halt.h */
#endif
#include "atp_main.hpp"
#include "abstract_cross_compare.hpp"
#include "abstract_log_handler.hpp"
#include "atp_application.hpp"
#include "channel_config.hpp"
#include <signal.h>
#include <vfw_checkpoints.h>
#include <vfw_identity.h>
#include <vfw_version.h>
#include <vfw_halt.h>
#include "atc_util.hpp"
#include <vfw_init.h>

#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime(void);
extern "C" void vfwSetMaxTimeDiff(uint32_t maxTimeDiff);
#else
#include <vfw_time.h>
#endif


#if defined(__HIL) || defined(_SIL)
#include "vioh_sim.hpp"
#include "cod_sim.hpp"
#include "opc_sim.hpp"
#endif



#ifdef __EMD
#include "opc_sim.hpp"
#endif

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  
  extern "C"
  {
    /**
    * \brief Module_C identity string, used by VFW-macros.
    */
    const char_t* const module_C = __FILE__;

    /*******************************************************************************
    * \brief           Halt handler callback, called if we receive a halt signal.
    *******************************************************************************/
    static void haltHandler(void);
#ifndef _SIL
    static void sigHandler(const integer_t signo);
#endif
    /*******************************************************************************
    * \brief           Main loop, callback for VFW in order to run our 100 ms task.
    *******************************************************************************/
    static void mainLoopFunction(VFW_SyncTimer const timer, void* data); // Event for ATP application
  };


  /*******************************************************************************
  * \brief           Handle start up message
  *******************************************************************************/
  void AtpMain::handleStartupArgs(const int32_t argc, const char_t* const argv[]) const
  {
    if (argc == 2)
    {
      //A Side ATP
      if (strncmp(argv[1], "-a", sizeof("-a")) == 0)
      {
        vfwSetUniqId("AOS");
        vfwSetSide(VFW_A_SIDE);
        vfwSetApplicationName("Atp", 1);
      }
      else if (strncmp(argv[1], "-b", sizeof("-b")) == 0)
      {
        vfwSetUniqId("AOS");
        vfwSetSide(VFW_B_SIDE);
        vfwSetApplicationName("Atp", 1);
      }
      else
      {
        ATC::aosHalt(__FILE__, __LINE__, "unknown side!");
      }
      //append here the code for more arguments 
    }
    else
    {
      ATC::aosHalt(__FILE__, __LINE__, "Incorrect command line arguments");
    }
  }


  /*******************************************************************************
  * \brief           Init function
  *******************************************************************************/
  void AtpMain::initApplication()
  {
    //Initialize VFW
    vfwInit();

    /* Set the maximum Number of CP */
    vfwInitCheckPoints(maxNumberOfCheckpoints, maxNumberOfCheckpointsLog);

    //Initialize the VIOH Client 
    static VIOHnames::VIOHClient viohClientHandle(clientID, VIOHnames::enTTVITALSWIT, ATP::Kernel::ATPApplication::atpAppCycleTime);
    viohClientHandle.VIOHClientInit(clientID); //Should be after VFWInit() and before vfwStart()
    //Create and get the instance of ATP Application
    ATP::Kernel::ATPApplication& application = ATP::Kernel::ATPApplication::instance();
    application.setVIOHClientHandle(&viohClientHandle);
    //Add the Simulator
#if defined(__HIL) || defined(_SIL)
    application.addComponent(&ATC::Sim::VIOHSim::instance());
    application.addComponent(&ATC::Sim::CODSim::instance());
    application.addComponent(&ATC::Sim::OPCSim::instance());
#endif
#ifdef __EMD
    application.addComponent(&ATC::Sim::OPCSim::instance());
#endif
    //Add the others components of ATP
    application.addAllComponents();
    //call preInit
    application.preInit();
    //Create a timer.
    const VFW_Timer timer = vfwTimerAllocate();
    //add the timer to sync module with sync parameter to false
    evtTimer = vfwSyncAddTimer(timer, 0);
    //set the event for timer
    vfwSyncTimerSetHandler(evtTimer, &mainLoopFunction, static_cast<void *>(NULL));
    //set the time out 
    vfwSyncTimerSetTimeout(evtTimer, static_cast<int64_t>(ATP::Kernel::ATPApplication::atpAppCycleTime));
    vfwSyncTimerSetTimeoutTolerance(evtTimer, static_cast<int32_t>(watchdogTimeout));
    //set the halt handler
    vfwAtHalt(&haltHandler);
    //Signal handler in case vfwWatchdog is not kicked
#ifndef _SIL
    //Set the signal handler
    //lint -e{923} -e{1924} Side effect of external macro
    //lint -e{586} signal is used according to 3NSS012264D0048
    if (signal(SIGALRM, &sigHandler) == SIG_ERR)
    {
      ATC::aosHalt(__FILE__, __LINE__, "Failed to set handler for SIGALRM.");
    }
#endif

    //set the  Max allowed time difference between CPU A and B
    vfwSetMaxTimeDiff(maxAllowedTimeDiff);

    initCrossCompare();

 #ifndef _SIL
    VFW_ChannelDesc channelReadDesc = static_cast<VFW_ChannelDesc>(NULL);

    if (vfwGetSide() == VFW_A_SIDE)
    {
      channelReadDesc = vfwChannelOpenRead(ATC::dispatcherToAtpA, ATC::dispatcherToAtpOutQueueSize,
        ATC::maxDispatcherToAtpMsgSize, ATC::dispatcherToAtpA);
    }
    else
    {
      channelReadDesc = vfwChannelOpenRead(ATC::dispatcherToAtpB, ATC::dispatcherToAtpOutQueueSize,
        ATC::maxDispatcherToAtpMsgSize, ATC::dispatcherToAtpB);
    }


    if ((NULL != channelReadDesc))
    {
      // Synchronize with diversified channel (A/B)
      syncChannelReadDesc = vfwSyncAddChannel(channelReadDesc, ATC::trueVfw);
      // Not event-driven, cyclic polled
      vfwSyncChannelDeactivate(syncChannelReadDesc);
    }
    else
    {
      ATC::aosHalt(__FILE__, __LINE__, "Failed to open channels");
    }
#endif
  }

  /*******************************************************************************
  * \brief           mainLoop function
  *******************************************************************************/
  void AtpMain::mainLoop(const VFW_SyncTimer timer)
  {
    // To avoid jitter, call the watchdog first!
    vfwWatchdogKick(watchdogTimeout);

    // Call update to VIOH Client
    const VIOHnames::VIOH_clientResultType updateResult = ATP::Kernel::ATPApplication::instance().getVIOHClientHandle()->Update();

    switch (state)
    {
    case ATPMainDispInit:
      handleInitDispatcher();
      break;

    case ATPMainSimInit:
      handleInitSimulators();
      break;

    case ATPMainAppInit:
      handleInit();
      break;

    case ATPMainAppRun:
      handleRun(updateResult);
      break;

    default:
      ATC::aosHalt(__FILE__, __LINE__, "Invalid state");
      break;
    }

    vfwSyncTimerReactivateCyclic(timer);
    //add here code to measure the execution time and memory usage
  }

  /*******************************************************************************
    * \brief           Simulators initialization
  *******************************************************************************/
  void AtpMain::handleInitSimulators()
  {
#if defined(__HIL) || defined(_SIL)
    bool notReady = (!ATC::Sim::VIOHSim::instance().init());
    notReady = (!ATC::Sim::CODSim::instance().init()) || notReady;
    notReady = (!ATC::Sim::OPCSim::instance().init()) || notReady;
#endif
#ifdef __EMD
    const bool notReady = (!ATC::Sim::OPCSim::instance().init());
#endif

#if defined(__HIL) || defined(__EMD)  || defined (_SIL)
    if (!notReady)
#endif
    {
      state = ATPMainAppInit;
    }
  }

  /*******************************************************************************
  * \brief    handleInitDispatcher
  *******************************************************************************/
  void AtpMain::handleInitDispatcher()
  {
#ifndef _SIL
    if (vfwSyncChannelStat(syncChannelReadDesc) > 0U)
    {
      VFW_Buffer dispToAtpBuffer;
      uint8_t buffer[ATC::maxDispatcherToAtpMsgSize];

      memset(&buffer[0U], 0, sizeof(buffer));
      // Initialize the VFW buffer to raw buffer
      vfwInitBuffer(&dispToAtpBuffer, &buffer[0U], sizeof(buffer));

      const int32_t noOfBytesRead = vfwSyncChannelReadBuffer(syncChannelReadDesc, &dispToAtpBuffer);

      if (noOfBytesRead > 0)
      {
        char_t dispatcherVersionString[20];

        const uint32_t dispatcherVersionStringLen = vfwGetString(&dispToAtpBuffer, &dispatcherVersionString[0], sizeof(dispatcherVersionString));

        if (dispatcherVersionStringLen != 0U)
        {
          ATP::Kernel::ATPApplication::instance().setDispatcherVersionString(&dispatcherVersionString[0]);
        }
        else
        {
          ATC::aosHalt("Dispatcher not sending version!", __LINE__, __FILE__);
        }
        state = ATPMainSimInit;
      }
    }
#else
    state = ATPMainSimInit;
#endif
  }

  /*******************************************************************************
  * \brief           initialization
  *******************************************************************************/
  void AtpMain::handleInit()
  {
    // Run simulators
#if defined(__HIL) || defined(_SIL)
    ATC::Sim::VIOHSim::corePtr()->runIn();
    ATC::Sim::CODSim::corePtr()->runIn();
#endif

    // Check the client status
    VIOHnames::VIOH_clientStatusType myClientStatus;
    const VIOHnames::VIOH_clientResultType clientStatusRes = ATP::Kernel::ATPApplication::instance().getVIOHClientHandle()->Status(&myClientStatus);

    if (ATP::Kernel::ATPApplication::instance().init() &&
      (clientStatusRes == VIOHnames::enCRT_OK) &&
      (myClientStatus == VIOHnames::enCST_RUNOK))
    {
      state = ATPMainAppRun;
      // Calculation of watchdog timeout  - over scheduling
      watchdogTimeout = (ATP::Kernel::ATPApplication::atpAppCycleTime * (100U + overschedulingMarginDuringRunInPercentage)) / 100U;
      vfwSyncTimerSetTimeoutTolerance(evtTimer, static_cast<int32_t>(watchdogTimeout));
    }

    // Run simulators
#if defined(__HIL) || defined(_SIL)
    ATC::Sim::VIOHSim::corePtr()->runOut();
    ATC::Sim::CODSim::corePtr()->runOut();
#endif
  }

  /*******************************************************************************
  * \brief           initCrossCompare
  *******************************************************************************/
  void AtpMain::initCrossCompare() const
  {
    // Add all attributes to CC...
    // There is no need to add synchronized timers (evtTimer), they are every cross-compare point by vfw.
    Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<ATPMainStatus>(&state));
    Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&watchdogTimeout));
  }

  /*******************************************************************************
  * \brief           run
  *******************************************************************************/
  void AtpMain::handleRun(const VIOHnames::VIOH_clientResultType updateResult) const
  {
    if ((updateResult == VIOHnames::enCRT_OK) ||
      (updateResult == VIOHnames::enCRT_OKNU))
    {
      // OK
    }
    else
    {
      ATC::aosHalt(__FILE__, __LINE__, "Failed to Update VIOH Client Handle");
    }

    // Run simulators
#if defined(__HIL) || defined(_SIL)
    ATC::Sim::VIOHSim::corePtr()->runIn();
    ATC::Sim::CODSim::corePtr()->runIn();
    ATC::Sim::OPCSim::corePtr()->runIn();
#endif
#ifdef __EMD
    ATC::Sim::OPCSim::corePtr()->runIn();
#endif

    // Run ATP
    ATP::Kernel::ATPApplication::instance().run();

    // Run simulators
#if defined(__HIL) || defined(_SIL)
    ATC::Sim::VIOHSim::corePtr()->runOut();
    ATC::Sim::CODSim::corePtr()->runOut();
    ATC::Sim::OPCSim::corePtr()->runOut();
#endif
#ifdef __EMD
    ATC::Sim::OPCSim::corePtr()->runOut();
#endif
  }

  /*******************************************************************************
  * \brief           main Function
  *******************************************************************************/
  void AtpMain::mainFunction(const int32_t argc, const char_t* const argv[])
  {
    handleStartupArgs(argc, argv);
    // Initialization of ATP application
    initApplication();
    // Start the VFW 
    vfwStart();
    // Do the dummy compare
    vfwCrossCompare(static_cast<VFW_CrossCompare> (NULL));
    // Should be after vfwStart() and before cyclic operation
    ATP::Kernel::ATPApplication::instance().getVIOHClientHandle()->VIOHClientInit2SyncDeact();
    // Start the sync timer
    vfwSyncTimerStart(evtTimer);
    // Endless loop for sync
    while (true) //lint !e716 Endless loop is needed here
    {
      vfwSync();
    }
  }

  /*******************************************************************************
  * \brief           instance
  *******************************************************************************/
  AtpMain& AtpMain::instance()
  {
    static AtpMain mainObject;

    return mainObject;
  }

  /*******************************************************************************
  * \brief           constructor
  *******************************************************************************/
  AtpMain::AtpMain()
    :
    state(ATPMainDispInit),
    evtTimer(static_cast<VFW_SyncTimer>(NULL)),
    watchdogTimeout(watchdogTimeoutDuringStartup)
  {
#ifndef _SIL
    syncChannelReadDesc = static_cast<VFW_SyncChannel>(NULL);
#endif
  }

  extern "C"
  {
#ifndef _SIL
    /*******************************************************************************
    * \brief           Main entry function
    *******************************************************************************/
    //lint -e{970} main() uses int and char
    int main(const int argc, const char* const* const argv)
    {
      //check the VFW Version 
      //lint -e{717} -e{909} Side effect of external macro
      VFW_ASSERT_VERSION();
      // Handle startup arguments if any
      AtpMain::instance().mainFunction(argc, argv);

      return 0;
    }
#endif

    /*******************************************************************************
    * \brief           Main Loop function for ATP application
    *******************************************************************************/
    static void mainLoopFunction(VFW_SyncTimer const timer, void* /*data*/)
    {
      AtpMain::instance().mainLoop(timer);
    }

    /*******************************************************************************
    * \brief           Halt Handler
    *******************************************************************************/
    static void haltHandler(void)
    {
      //code for handler of Halt
      //will get implement later
    }
#ifndef _SIL
    /*******************************************************************************
    * \brief           Signal Handler
    *******************************************************************************/
    static void sigHandler(const integer_t signo)
    {
      //code here for signal handler
      switch (signo)
      {
      case SIGALRM:
        ATC::aosHalt(__FILE__, __LINE__, "Entered in Signal handler -ATP");
        break;

      default:
        ATC::aosHalt(__FILE__, __LINE__, "Entered in Signal handler -ATP");
        break;
      }
    }
#endif
  }
} // namespace ATP
