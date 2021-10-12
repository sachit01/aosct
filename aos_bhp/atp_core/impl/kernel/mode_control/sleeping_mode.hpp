#ifndef SleepingMode_hpp
#define SleepingMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the sleeping mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-08    spandita    Created
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
    /******************************************************************************
    * GLOBAL DECLARATIONS
    ******************************************************************************/
    typedef uint8_t SleepingModeState;

    /**
    * The class SleepingMode defines the sleeping ATP mode.
    *
    */
    class SleepingMode : public AbstractMode
    {
    public:

      /**
      * The start state (remove targets, tracks) of sleeping mode 
      */
      static const SleepingModeState sleepingStart            = 1U;

      /**
      * The state to wait for deactivation of sleeping signal in sleeping mode.
      */
      static const SleepingModeState sleepingWaitSleepingSigDeactiv  = 2U;

      /**
      * The state to wait for train to standstill
      */
      static const SleepingModeState  sleepingNotSleepingWaitStandstill = 3U;

      /**
      * The state to wait for driver to confirm for configuration change
      */
      static const SleepingModeState sleepingWaitNotSleeping = 4U;

      /**
      * The state to finish sleeping mode and enter configuration mode.
      */
      static const SleepingModeState sleepingFinishConfig     = 5U;

      /**
      * Main run function of the mode.
      * @param[in] commonData   reference to train states
      */
      virtual void handleMode(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      SleepingMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeSleeping enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Getter for the state of the sleeping mode
      *
      * @return SleepingModeState value of modeState variable.
      */
      virtual SleepingModeState getModeState() const;

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const SleepingModeState state, char_t* const buffer) const;

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the mode state is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

      /**
      * Function to reset mode state.
      *
      */
      virtual void resetMode();

    protected:

      /**
      * To apply standstill event in case sleeping signal is inactive. and mode is sleeping
      */
      const ATC::Event sleepingSignalInactiveStandstill;

      /**
      * Function to run the sleepingStart state of Sleeping mode
      *
      * @param[in] commonData   reference to train states
      */
      virtual void runSleepingStart(CommonDataForModes &commonData);

      /**
      * Function to run the runSleepingWaitSleepingSigDeactive state of Sleeping mode
      */
      virtual void runSleepingWaitSleepingSigDeactive();
      
      /**
      * Function to run the runSleepingNotSleepingWaitStandstill state of Sleeping mode
      */
      virtual void  runSleepingNotSleepingWaitStandstill();
      /**
      * Function to run the sleepingWaitDMIConfirm state of Sleeping mode
      */
      virtual void runSleepingWaitNotSleeping();

      /**
      * Function to run the sleepingFinishConfig state of Sleeping mode
      */
      virtual void runSleepingFinishConfig();
      
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
      * The current state of Sleeping mode
      */
      SleepingModeState modeState;

    };
  }
}

#endif
