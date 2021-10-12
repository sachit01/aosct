#ifndef ATP_Main_hpp
#define ATP_Main_hpp

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file defines AtpMain declaration
*
******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include <vfw_sync.h>
#include <vio_types.h>

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  
  /**
  * The class AtpMain implements the main function.
  *
  */
  class AtpMain
  {
  public:

    /**
    * Get the instance
    *
    * @return reference to the instance.
    */
    static AtpMain& instance();

    /**
    * Main loop, used by the C-callback registered in VFW.
    *
    * @param [in] timer The timer.
    */
    void mainLoop(const VFW_SyncTimer timer);

    /**
    * Main loop, used by the C-callback registered in VFW.
    *
    * @param[in] argc  Number of arguments in the argument array argv
    * @param[in] argv  Arguments array
    */
    void mainFunction(const int32_t argc, const char_t* const argv[]);

  private:

    /**
    * Increased cycle watchdog timeout during startup
    */
    static const uint32_t watchdogTimeoutDuringStartup = 3000U;

    /**
    * Percentage of allowed overschedule margin
    */
    static const uint32_t overschedulingMarginDuringRunInPercentage = 20U;

    /**
    * Maximum number of checkpoints
    */
    static const uint32_t maxNumberOfCheckpoints = 250U;         

    /**
    * Maximum number for checkpoints logs
    */
    static const uint32_t maxNumberOfCheckpointsLog = 250U;      // Initialize the

    /**
    * Client ID - The ID shall be an index number that is less than or equal to the number set
    * In the common configuration parameter for number of vital and non-vital tasks using the VIOH client.
    */
    static const uint32_t  clientID = 0U;

    /**
    *Max allowed time difference between CPU A and B
    */
    static const uint32_t maxAllowedTimeDiff = 15000U;

    /**
    * State machine for status of ATP Application
    */
    enum ATPMainStatus
    {
      ATPMainDispInit = 0,
      ATPMainSimInit = 1,
      ATPMainAppInit = 2,
      ATPMainAppRun = 3
    };

    /**
    * Init application, run once at startup, Initializes VFW etc.
    */
    void initApplication();

    /**
    * Function for initialization of simulators
    */
    void handleInitSimulators();

    /**
    * Function waiting for the dispatcher to be alive
    */
    void handleInitDispatcher();

    /**
    * Handle the init state
    */
    void handleInit();

    /**
    * Init cross compare, add all attributes to CC
    */
    void initCrossCompare() const;

    /**
    * Handle the run state
    * @param[in] updateResult  VIOH update result
    */
    void handleRun(const VIOHnames::VIOH_clientResultType updateResult) const;

    /**
    * Handle startup argumets
    *
    * @param[in] argc  Number of arguments in the argument array argv
    * @param[in] argv  Arguments array
    */
    void handleStartupArgs(const int32_t argc, const char_t* const argv[]) const;

    /**
    * Constructor
    */
    AtpMain();

    /**
    * State
    */
    ATPMainStatus state;

    /**
    * Timer for ATP application
    */
    VFW_SyncTimer evtTimer;
    
    /**
    * Watchdog timeout time in ms
    */
    uint32_t watchdogTimeout;
#ifndef _SIL
    /** Channel handle returned by vfwSyncAddChannel
    */
    VFW_SyncChannel syncChannelReadDesc;
#endif
  };
} // namespace ATP

#endif
