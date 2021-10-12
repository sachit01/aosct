#ifndef PossessionMode_hpp
#define PossessionMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Possession mode.
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
    typedef uint8_t PossessionModeState;

    /**
    * The class PossessionMode defines the Possession mode.
    *
    */
    class PossessionMode : public AbstractMode
    {
    public:

      /**
      * The start state of possession mode
      */
      static const PossessionModeState possessionModeStart = 1U;

      /**
      * The finish state of possession mode
      */
      static const PossessionModeState possessionModeFinish = 3U;

      /**
      * Main run function of the possession mode.
      * @param[in] commonData   reference to train states
      */
      virtual void handleMode(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      PossessionMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModePossession enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Function to reset mode state.
      *
      */
      virtual void resetMode();

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const PossessionModeState state, char_t* const buffer) const;

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the modestate is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

      /**
      * Getter for the state of the possession mode
      *
      * @return PossessionModeState value of modeState variable.
      */
      virtual PossessionModeState getModeState() const;

    protected:

      /**
      * To manage the current driving direction in possesion mode
      * @param[in] commonData   reference to train states
      */
      virtual void handleCurrentDrivDirection(CommonDataForModes &commonData);

      /**
      * To manage the transition of train to idling.
      * @param[in] commonData   reference to commonData
      */
      virtual void manageTrainIdling(CommonDataForModes &commonData);

      /**
      * To manage the MA timeout train state.
      * @param[in] commonData   reference to commonData
      */
      virtual void manageMATimeout(CommonDataForModes &commonData);

      /**
      * Function to run the possessionModeStart state of Possession mode.
      * @param[in] commonData   reference to train states
      */
      virtual void runPossessionModeStart(CommonDataForModes &commonData);

      /**
      * Function to run the possessionModeFinish state of Possession mode.
      */
      virtual void runPossessionModeFinish();

      /**
       * Initializes the cross compare module. Called by the init function.
       */
      virtual void initCrossCompare() const;

    private:

      /**
      * The current state of Possession mode
      */
      PossessionModeState modeState;

    };
  }
}
#endif
