#ifndef TrainConfigModeBHP_hpp
#define TrainConfigModeBHP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the TrainConfig mode adaptation part for BHP.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-05-10    skothiyal    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "train_config_mode.hpp"
#include "atp_application.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    static const uint32_t confirmTrainLoadWaitCyclesMax = 5000U / Kernel::ATPApplication::atpAppCycleTime; // (ms), scaled to execution-cycles

    /**
    * The class TrainConfigModeStateBHP defines the Train Configuration mode.
    *
    */
    class TrainConfigModeBHP : public TrainConfigMode
    {

    public:

      /**
      * TrainConfig mode to start checking for valid brake pressure
      */
      static const TrainConfigModeState trainConfigPreDepartureBrakeTest = 100U;

      /**
      * TrainConfig mode used while checking for valid brake pressure
      */
      static const TrainConfigModeState trainConfigCheckValidBrkPrFeedBack = 101U;

      /**
      * Constructor.
      *
      */
      TrainConfigModeBHP();

      /**
      * Get Confirm needed when Train Loaded state changed by driver
      *
      * @return true if Confirm Train Loaded state needed, false otherwise.
      */
      bool getConfirmTrainLoadedStateNeeded() const;

      /**
      * Get Confirm that driver is manually aborted the last car brake pressure in progress
      *
      * @return true if Confirm Train Loaded state needed, false otherwise.
      */
      bool getLastCarBrakePressureTestAborted() const;

      /**
      * Get confirm needed when last car brake pressure is aborted
      *
      * @return true if Confirm Train Loaded state needed, false otherwise.
      */
      bool isManualAbortConfirmationNeeded() const;

     /**
      * Function to reset mode state.
      *
      */
      virtual void resetMode();

      /**
      To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[in] buffer the mode state string is copied to the buffer
      */
      virtual void getModeStateString(const TrainConfigModeState state, char_t* const buffer) const;

    protected:

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

      /**
      * Calls the appropriate run function depending in the current mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runModeFunction(CommonDataForModes &commonData);
      /**
      * Function to run the runTrainConfigWaitNewConfigDMI state of TrainConfig mode (Manual Configuration).
      * @param[in] commonData   reference to common data shared between different modes and mode control class
      */
      virtual void runTrainConfigWaitNewConfigDMI(CommonDataForModes &commonData);

      /**
      * Function to run the adaptation of trainConfigWaitForAcceptAutomatic state of TrainConfig mode (Automatic Configuration).
      */
      virtual void runTrainConfigWaitForAcceptAutomatic();

      /**
      * Function to run the trainConfigConfirmNewConfigFrmTCC state of TrainConfig mode.
      * 
      */
      virtual void runTrainConfigConfirmNewConfigFrmTCC();

      /**
      * Function to run the trainConfigConfirmNewConfigFrmTCC state of TrainConfig mode.
      */
      virtual void runTrainConfigTSetupAccepted();

      /**
      * Function to start the pre-departure brake test.
      */
      virtual void runTrainConfigPreDepartureBrakeTest();

      /**
      * Function to run the pre-departure brake test.
      *
      * @param[in] commonData   reference to common data shared between different modes and mode control class
      */
      virtual void runTrainConfigCheckValidBrkPrFeedBack(CommonDataForModes &commonData);

      /**
      * Function to run the runTrainConfigWaitForDepartureTest state of TrainConfig mode.
      */
      virtual void runTrainConfigWaitForDepartureTest();

     private:

      /**
      * Manually Aborted Last car Brake Pressure in Progress by Driver
      */
      bool abortLastCarBrakeTestButtonPressed;

      /**
      * If not zero, the time when a manual confirmation to abort the brake test is started. (Reference time [ms])
      */
      int64_t startTimeToConfirmAbortOfLastCarBrakeTest;

      /**
      * Handle confirmation of TrainLoaded status requested by driver
      *
      * @return true if confirmation in progress, false when no confirmation in progress (core behavior)
      */
      bool handleTrainLoadedConfirmation();

      /**
      * Handle DMI Actions when Last Car brake test is in progress
      */
      void handleAbortBrakeTestActions();

      /**
      * Function to check if the loco pressure BP1 and/or BP2 are below the brake test pressure
      * 
      * @return true if loco brake pressure is below config value, false otherwise
      */
      bool checkBrakePressureLocoBelowPsb() const;

      /**
      * Train Loaded state requested by driver
      */
      TrainLoaded trainLoadedStatusRequested;

      /**
      * Confirm Train Loaded state needed 
      */
      bool confirmTrainLoadedStateNeeded;

      /**
      * Check if last Car Brake Pressure Test is pending
      */
      bool lastCarBrakeTestPending;
      /**
      * Number of execution-cycles waiting fro confirmation of Trainb Loaded state
      */
      uint16_t confirmTrainLoadedWaitCycles;

      /**
      * The cycles passed since waiting for valid feedback from OBRD
      */
      uint16_t waitForBrakePressureTimer;

      /**
      * Event for start of brake test
      */
      const ATC::Event waitingFeedbackFromOBRD;

      /**
      * Event for brake test succeeded
      */
      const ATC::Event brakePressureDidDrop;

      /**
      * Event for brake test failed
      */
      const ATC::Event brakePressureDidNotDrop;

      /** Log Event confirmed that last car brake pressure is aborted
      */
      const ATC::Event abortLastCarBrakeTestConfirmed;

      /** Log Event last Car Brake Pressure Test is pending
      */
      const ATC::Event lastCarBrakeTestIsPending;

      /**
      * Returns the timeout (expressed in seconds) for dropping the Last Car Brake Pressure.
      *
      * @return Timeout in seconds for dropping the Last Car Brake pressure
      */
      uint32_t getBrakePressureDropTimeout() const;
    };
  }
}

#endif
