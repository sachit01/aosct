#ifndef PowerUpMode_hpp
#define PowerUpMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the PowerUP mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-19    arastogi    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_mode.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    typedef uint8_t PowerUpModeState;

    /**
    * The class PowerUpMode defines the Power up mode.
    *
    */
    class PowerUpMode : public AbstractMode
    {
    public:

      /**
      * The start state of PowerUp mode
      */
      static const PowerUpModeState powerUpStart = 1U;

      /**
      * The state of PowerUp mode to run startup tests
      */
      static const PowerUpModeState powerUpTest = 2U;

      /**
      * The state of PowerUp mode to wait for activation signals
      */
      static const PowerUpModeState powerUpActivation = 3U;

      /**
      * The state of PowerUp mode to wait for DMI Confirmation and possible transition to Sleep mode
      */
      static const PowerUpModeState powerUpWaitConfigOrSleep = 4U;

      /**
      * The state of PowerUp mode to wait for DMI Sleep mode confirmation
      */
      static const PowerUpModeState powerUpWaitSleepConfirmDMI = 5U;

      /**
      * An end state of PowerUp mode to enter Sleep mode
      */
      static const PowerUpModeState powerUpFinishSleep = 6U;

      /**
      * An end state of PowerUp mode to enter Configuration mode
      */
      static const PowerUpModeState powerUpFinishConfig = 7U;

      /**
      * Main run function of the mode.
      * @param[in] commonData   reference to common data
      */
      virtual void handleMode(CommonDataForModes &commonData);

      /**
      * Function to reset mode state.
      *
      */
      virtual void resetMode();

      /**
      * Constructor.
      *
      */
      PowerUpMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModePowerUp enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Getter for the state of the power up mode
      *
      * @return PowerUpModeState value of modeState variable.
      */
      virtual PowerUpModeState getModeState() const;

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const PowerUpModeState state, char_t* const buffer) const;

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the mode state is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

    protected:

      /**
      * Function to run the PowerUpStart state of PowerUp mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runPowerUpStart(CommonDataForModes &commonData);

      /**
      * Function to run the PowerUpTest state of PowerUp mode.
      * @param[in] commonData   reference to common data
      *
      */
      virtual void runPowerUpTest(CommonDataForModes &commonData);

      /**
      * Function to run the PowerUpActivation state of PowerUp mode.
      * @param[in] commonData   reference to common data
      *
      */
      virtual void runPowerUpActivation(CommonDataForModes &commonData);

      /**
      * Function to run the PowerUpWaitConfigConfirmDM state of PowerUp mode.
      */
      virtual void runPowerUpWaitConfigOrSleep();

      /**
      * Function to run the PowerUpWaitSleepConfirmDMI state of PowerUp mode.
      */
      virtual void runPowerUpWaitSleepConfirmDMI();

      /**
      * Function to run the PowerUpFinishSleep state of PowerUp mode.
      */
      virtual void runPowerUpFinishSleep();

      /**
      * Function to run the PowerUpFinishConfig state of PowerUp mode.
      *
      */
      virtual void runPowerUpFinishConfig();

      /**
      * To manage the transition of train to idling.
      * @param[in] commonData   reference to train states
      */
      virtual void manageTrainIdling(CommonDataForModes &commonData);

      /**
      * To manage the MA timeout train state.
      * @param[in] commonData   reference to train states
      */
      virtual void manageMATimeout(CommonDataForModes &commonData);

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * Perform Start Up and health supervision
      * @return true if successful
      */
      bool startUpTestAndHealthSup() const;

      /**
      * No of cycles that is at least needed for EB test
      */
      static const uint8_t ebWaitCycle = 5U;

      /**
      * No of cycles that is at least needed for health supervision and integrity test.(~ 5min)
      *
      */
      static const uint16_t supTestWaitCycle = 3000U;

      /**
      * No of cycles that is allowed for health supervision for VehicleCom (~30 sec)
      *
      */
      static const uint16_t supTestVehicleComWaitCycle = 300U;

      /**
      * The current state of PowerUp mode
      */
      PowerUpModeState modeState;

      /**
      * Event to apply SB and not allow movement in Power Up
      */
      const ATC::Event noMovementInPOU;

      /**
      * Event for fatal failure in Power Up
      */
      const ATC::Event safetyHaltInPOU;

      /**
      * Event to log Power Up sequence
      */
      const ATC::Event powerUpSequenceStarted;

      /**
      * Event to if dispatcher version mismatches
      */
      const ATC::Event dispatcherVersionMismatch;

      /**
      * Current Wait cycle
      */
      uint16_t waitCycle;

      /**
      * Current toggle Wait cycle
      */
      uint16_t toggleWaitCycle;
    };
  }
}

#endif
