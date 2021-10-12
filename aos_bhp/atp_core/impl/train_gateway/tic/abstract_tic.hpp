#ifndef AbstractTIC_hpp
#define AbstractTIC_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
* This file defines AbstractTIC class which contains the core functionality 
* of the TIC Component.
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-06-11    nsyed    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "event.hpp"
#include <vfw_timer.h>

/******************************************************************************
* DECLARATIONS    
******************************************************************************/
namespace ATP
{
  namespace TG
  { 
    /** TIC Train Configuration Status (Automatic Train Configuration Status)
    */
    enum TICConfigStatus
    {
      /** TIC Train Config Status: Idle */
      TICConfigStatusIdle = 0,
      /** TIC Train Config Status: Pending */
      TICConfigStatusPending = 1,
      /** TIC Train Config Status: In Progress */
      TICConfigStatusInProgress = 2,
      /** TIC Train Config Status: Completed */
      TICConfigStatusCompleted = 3,
      /** TIC Train Config Status: Error */
      TICConfigStatusError = 4
    };

    /** Status of the Automatic Train Configuration Request
    */
    enum TICConfigReqStatus
    {
      /** Automatic Train Configuration not sent by TIC yet */
      TICConfigReqNotSent = 0,
      /** Automatic Train Configuration request sent by TIC and waiting for the response */
      TICWaitingForConfig = 1,
      /** Automatic Train Configuration received by TIC and stored in ATP */
      TICConfigReceived = 2
    };

    class AbstractTIC;
    /** 
    * Static variable to store the single instance of AbstractTIC
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only 
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractTIC* coreTICInstancePtr = static_cast<AbstractTIC*>(NULL);

    /**
    * The class AbstractTIC implements the interface defined by the ComponentBase class.
    *
    */
    class AbstractTIC : public ATC::ProcComponent
    {
    public:

      /**
      * Implements the virtual init function.
      */
      virtual bool init();

      /**
      * Implements the virtual run function.
      */
      virtual void run();

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractTIC* corePtr();

      /**
      * Access function to get the TIC Availability.
      *
      * @return true if TIC functionality is available.
      */
      bool getTICAvailable() const;

      /**
      * Access function to get TIC Train Configuration Status (Automatic Train Configuration)
      *
      * @return current TIC Train Configuration state.
      */
      TICConfigStatus getStatus() const;

      /**
      * Function to abort any retrieval of Train Configuration in progress
      */      
      void abortConfig();

      /**
      * Function to request retrieval of Train Configuration.
      */
      void requestConfig();

      /**
      * Function to reset the TIC component in order to receive new configuration.
      */
      void reset();

      /**
      * Returns the availability of locomotive orientation.
      *
      * @return true if locomotive orientation is provided by TIC
      */
      virtual bool getLocoOrientationAvailable() const;

      /**
      * Interface to call different levels of Console Command
      *
      * @param[in] argc  Number of arguments in the argument array argv
      * @param[in] argv  Arguments array
      *
      * @return true if the Call is successful.
      */
      virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

    protected:

      /**
      * Constructor
      */
      AbstractTIC();

      /**
      * Evaluate the TIC Availability function.
      * This function shall be implemented in adaptation.
      *
      * @return true if TIC is available.
      */
      virtual bool evaluateTICAvailable();

      /**
      * Evaluate the status of the Automatic Train configuration request.
      * This function shall be implemented in adaptation.
      *
      * @return Automatic Train configuration request status.
      */
      virtual TICConfigReqStatus evaluateConfigReqStatus();

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * To get string for mode.
      *
      * @param[in] configStatus enum to get string for.
      * @param[in] buffer          the TICConfigStatus string is copied to the buffer
      */
      void getTICConfigStatusStr(const TICConfigStatus configStatus, char_t* const buffer) const;

      /**
      * Handle TIC Status transitions when TIC is available
      */
      void handleTicAvailable();

      /**
      * Maximum size of the TICConfigStatus string (including the NULL character)
      */
      static const uint8_t maxConfigStatusNameLength = 20U;

      /** 
      * TIC availability value 
      */
      bool ticAvailable;

      /**
      * Automatic Train Configuration Requested
      */
      bool configRequested;
      
      /**
      * Timeout for delivering Train Composition
      */
      uint32_t timeoutDeliverConfigInMilliSec;

      /**
      * Exceeded the max. time to deliver Train Configuration
      */
      const ATC::Event deliverTrainConfigTimeout;

      /**
      * TIC transitions into being unavailable
      */
      const ATC::Event ticIsNowUnavailable;

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * Counter to calculate TIC count
      */
      uint32_t ticTimeoutCounter;

      /**
      * Flag to check if TIC timeout has occured
      */
      bool isTicTimeoutExpired;

      /**
      * TIC Train Configuration Status value
      */
      TICConfigStatus ticConfigStatus;
    };
  }
}

#endif
