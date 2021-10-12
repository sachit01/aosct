/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file defines DispMain implementation
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-03    nsyed    Created
* 2016-12-15    spandita  Implemented the DispMain Functionalities
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "disp_application.hpp"
#ifndef WIN32
#include <signal.h>
#endif
#include "vfw_sync.h"
#include "vfw_identity.h"
#include "message_dispatcher.hpp"
#include "vfw_time.h"
#include "channel_config.hpp"
#ifndef __GNUG__
#define __attribute__(A) /* If not GNU, do nothing. This must be defined before including vfw_halt.h */
#endif
#include "vfw_halt.h"
#include "atc_util.hpp"
#include "vfw_init.h"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace
{
  const int32_t disAppCycleTime = 100;            // Dispatcher application cyclic time
  void mainLoop(VFW_SyncTimer const timer, void* data); // Event for dispatcher application
  VFW_SyncTimer evtTimer;                         // Timer for disp application
  uint32_t watchdogTimeout = 3000U;               // Watchdog timeout set to 3 seconds before running

  /*******************************************************************************
  * \brief           Handle start up message
  *******************************************************************************/
  void handleStartupArgs(const int32_t argc, const char_t* const* const argv)
  {
    if (argc == 2)
    {
      if (strncmp(&argv[1][0], "-c", sizeof("-c")) == 0)
      {
        vfwSetUniqId("AOS");
        vfwSetSide(VFW_C_SIDE);
        vfwSetApplicationName("Disp", 1);
      }
      else  if (strncmp(&argv[1][0], "-a", sizeof("-a")) == 0)
      {
        vfwSetUniqId("AOS");
        vfwSetSide(VFW_A_SIDE);
        vfwSetApplicationName("Disp", 1);
      }
      else
      {
        ATC::aosHalt(__FILE__, __LINE__, "Unknown side and arguments");
      }
      //ATO related arguments
      //append here the code for more arguments 
    }
  }

  /*******************************************************************************
  * \brief           Halt Handler
  *******************************************************************************/
  void haltHandler(void)
  {
    //code for handler of Halt
    //will get implement later
  }

  /*******************************************************************************
  * \brief           Signal Handler
  *******************************************************************************/
#ifndef WIN32
  void sigHandler(integer_t /*signo*/)
  {
    ATC::aosHalt(__FILE__, __LINE__, "Entered in Signal handler -Dispatcher");
  }
#endif

  /*******************************************************************************
  * \brief           Init function
  *******************************************************************************/
  void initApplication(void)
  {
    //timer
    VFW_Timer timer;
    //Initialize VFW
    vfwInit();
    //Create the instance of Disp Application
    static_cast<void>(Dispatcher::DispApplication::instance());
    //Add the components of dispatcher
    Dispatcher::DispApplication::instance().addAllComponents();
    //call preInit
    Dispatcher::DispApplication::instance().preInit();
    //Create a timer.
    timer = vfwTimerAllocate();
    //add the timer to sync module with sync parameter to false
    evtTimer = vfwSyncAddTimer(timer, 0);
    //set the event for timer
    vfwSyncTimerSetHandler(evtTimer, &mainLoop, static_cast<void *>(NULL));
    //set the time out 
    vfwSyncTimerSetTimeout(evtTimer, static_cast<int64_t>(disAppCycleTime));
    //set the halt handler
    vfwAtHalt(&haltHandler);
#ifndef WIN32
    //Set the signal handler
    //lint -e{923} -e{1924} Side effect of external macro
    //lint -e{586} signal is used according to 3NSS012264D0048
    if (signal(SIGALRM, &sigHandler) == SIG_ERR)
    {
      ATC::aosHalt(__FILE__, __LINE__, "Failed to set handler for SIGALRM");
    }
#endif
    //set the max time difference between A and B 
    vfwSetMaxTimeDiff(15000U); //need to discuss how much to set 

    vfwStart();
    printf("vfwStarted\n"); // In order for our start script to work (sp-command)
  }

  /*******************************************************************************
  * \brief           Main Loop function for dispatcher application
  *******************************************************************************/
  void mainLoop(VFW_SyncTimer const timer, void* /*data*/)
  {
    static uint8_t state = 1U;

    vfwWatchdogKick(watchdogTimeout);

    switch (state)
    {
    case 1:
      if (Dispatcher::DispApplication::instance().init())
      {
        // Calculation of watchdog timeout  - 20% over scheduling
        watchdogTimeout = (static_cast<uint32_t>(disAppCycleTime) * 12U) / 10U;
        state = 2U;
      }
      vfwSyncTimerReactivateCyclic(timer);
      break;

    case 2:
      // Run Dispatcher
      Dispatcher::DispApplication::instance().run();
      vfwSyncTimerReactivateCyclic(timer);
      break;

    default:
      ATC::aosHalt(__FILE__, __LINE__, "Invalid state");
      break;
    }
  }
  //add here code to measure the execution time and memory usage
}
/*******************************************************************************
* \brief           Main entry function
*******************************************************************************/
extern "C"
{
  //lint -e{970} main() uses int and char
  int main(int const argc, const char* const* const argv)
  {
    bool running = true;
    //handle startup arguments if any
    handleStartupArgs(argc, argv);
    //Initialization
    initApplication();
    //start the sync timer 
    vfwSyncTimerStart(evtTimer);
    //Endless loop for sync
    while (running)
    {
      vfwSync();
    }
  }
}
