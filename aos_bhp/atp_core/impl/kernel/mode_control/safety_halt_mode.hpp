#ifndef SafetyHaltMode_hpp
#define SafetyHaltMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Safety Halt mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-12-02    nsyed    Created
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
    typedef uint8_t SafetyHaltModeState;
    /**
    * The class SafetyHaltMode defines the Safety Halt ATP mode.
    *
    */
    class SafetyHaltMode : public AbstractMode
    {
    public:

      /**
      * The start state of SafetyHalt mode
      */
      static const SafetyHaltModeState safetyHaltStart = 1U;

      /**
      * The state of SafetyHalt mode to wait for power down or restart
      */
      static const SafetyHaltModeState safetyHaltWaitForPowerDownOrRestart = 2U;

      /**
      * Main run function of the mode.
      * @param[in] commonData   reference to train states
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
      SafetyHaltMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeSafetyHalt enum value.
      */
      virtual ATPMode getModeId();

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const SafetyHaltModeState state, char_t* const buffer) const;

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the mode state is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

    protected:

      /**
      * Function to run the safetyHaltStart state of SafetyHalt mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runSafetyHaltStart(CommonDataForModes &commonData);

      /**
      * Function to run the safetyHaltWaitForPowerDownOrRestart state of SafetyHalt mode.
      * @param[in] commonData   reference to common data
      *
      */
      virtual void runSafetyHaltWaitForPowerDownOrRestart(CommonDataForModes &commonData);

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
      * Frequency (including duty cycle) to toggle the lamp status (in cycle count)
      */
      static const uint8_t toggleFrequency = 10U;

      /**
      * Max time (in cycle count) to toggle the lamp status
      */
      static const uint16_t maxTimeToggleAOSStatus = 3000U;

      /**
      * Event to apply safety halt in SafetyHalt Mode
      */
      const ATC::Event safetyHaltModeEvent;

      /**
      * Current toggle Wait cycle
      */
      uint16_t toggleWaitCycle;

      /**
      * The current state of SafetyHalt mode
      */
      SafetyHaltModeState modeState;

    };
  }
}

#endif
