#ifndef ShuntingMode_hpp
#define ShuntingMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Shunting mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-04-10    spandita    Created
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
    typedef uint8_t ShuntingModeState;
    
    /**
    * The class ShuntingMode defines the Shunting mode.
    */
    class ShuntingMode : public AbstractMode
    {
    public:

      /**
      * The state of Shunting mode to initialize and reset data
      */
      static const ShuntingModeState shuntingStart = 1U;

      /**
      * The state of Shunting mode to wait for Shunting Ack or possible transition to Sleep mode
      */
      static const ShuntingModeState shuntingWaitSleep = 2U;

      /**
      * The state of Shunting mode to wait for DMI Sleep mode confirmation
      */
      static const ShuntingModeState shuntingWaitSleepConfirmDMI = 3U;

      /**
      * An end state of Shunting mode to enter Sleep mode
      */
      static const ShuntingModeState shuntingFinishSleep = 4U;

      /**
      * Main run function of the Shunting mode.
      * @param[in] commonData   reference to train states
      */
      virtual void handleMode(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      ShuntingMode();

      /**
      * Function to reset mode state.
      */
      virtual void resetMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeShunting enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Getter for the state of the shunting mode
      *
      * @return ShuntingModeState value of modeState variable.
      */
      virtual ShuntingModeState getModeState() const;

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const ShuntingModeState state, char_t* const buffer) const;

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the mode state is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

    protected:

      /**
      * Function to run the runShuntingStart state of Shunting mode.
      * @param[in] commonData   reference to train states
      *
      */
      virtual void runShuntingStart(CommonDataForModes &commonData);

      /**
      * Function to run the runShuntingWaitSleep state of Shunting mode.
      *
      */
      virtual void runShuntingWaitSleep();

      /**
      * Function to run the runShuntingWaitSleepConfirmDMI state of Shunting mode.
      */
      virtual void runShuntingWaitSleepConfirmDMI();

      /**
      * Function to run the runShuntingFinishSleep state of Shunting mode.
      */
      virtual void runShuntingFinishSleep();

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
      * To manage the current driving direction in shunting mode
      * @param[in] commonData   reference to train states
      */
      virtual void handleCurrentDrivDirection(CommonDataForModes &commonData);

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * The current state of Shunting mode
      */
      ShuntingModeState modeState;


    };
  }
}
#endif
