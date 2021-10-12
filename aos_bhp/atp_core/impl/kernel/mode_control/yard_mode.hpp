#ifndef YardMode_hpp
#define YardMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Yard mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-25    akushwah    Created
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
    typedef uint8_t YardModeState;

    /**
    * The class YardMode defines the Yard mode.
    *
    */
    class YardMode : public AbstractMode
    {
    public:

      /**
      * The state of Yard mode to initialize and reset data
      */
      static const YardModeState yardStart = 1U;

      /**
      * The state of Yard mode to wait for Yard Ack and possible transition to Sleep- or Configuration mode
      */
      static const YardModeState yardWaitConfigOrSleep = 2U;

      /**
      * The state of Yard mode to wait for DMI Sleep mode confirmation
      */
      static const YardModeState yardWaitSleepConfirmDMI = 3U;

      /**
      * An end state of Yard mode to enter Configuration mode
      */
      static const YardModeState yardFinishConfig = 4U;

      /**
      * An end state of Yard mode to enter Sleep mode
      */
      static const YardModeState yardFinishSleep = 5U;

      /**
      * Main run function of the Yard mode.
      * @param[in] commonData   reference to common mode data
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
      YardMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeYard enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Getter for the state of the yard mode
      *
      * @return YardModeState value of modeState variable.
      */
      virtual YardModeState getModeState() const;

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const YardModeState state, char_t* const buffer) const;

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the mode state is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

    protected:

      /**
      * Function to run the runYardStart state of Yard mode.
      * @param[in] commonData   reference to train states
      *
      */
      virtual void runYardStart(CommonDataForModes &commonData);

      /**
      * Function to run the runYardWaitConfigOrSleep state of Yard mode.
      *
      */
      virtual void runYardWaitConfigOrSleep();

      /**
      * Function to run the runYardWaitSleepConfirmDMI state of Yard mode.
      */
      virtual void runYardWaitSleepConfirmDMI();

      /**
      * Function to run the runYardFinishConfig state of Yard mode.
      */
      virtual void runYardFinishConfig();

      /**
      * Function to run the runYardFinishSleep state of Yard mode.
      */
      virtual void runYardFinishSleep();
      
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
      * To manage the current driving direction in yard mode.
      * @param[in] commonData   reference to train states
      */
      virtual void handleCurrentDrivDirection(CommonDataForModes &commonData);

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * The current state of Yard mode
      */
      YardModeState modeState;

      /**
      * Previous Travel direction
      */
      TravelDir prevTravelDirection;
    };
  }
}

#endif
