#ifndef UnregisteredMode_hpp
#define UnregisteredMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Unregistered mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-03    arastogi    Created
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
    typedef uint8_t UnregisteredModeState;


    /**
    * The class UnregisteredMode defines the unregistered atp mode.
    *
    */
    class UnregisteredMode : public AbstractMode
    {
    public:

      /**
      * The start state of Unregistered mode
      */
      static const UnregisteredModeState unregisteredStart = 1U;

      /**
      * Main run function of the mode.
      * @param[in] commonData   reference to train states
      */
      virtual void handleMode(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      UnregisteredMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeUnregistered enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the mode state is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const UnregisteredModeState state, char_t* const buffer) const;

      /**
      * Function to reset mode state
      */
      virtual void resetMode();

    protected:

      /**
      * To manage the transition of train to idling.
      */
      virtual void manageTrainIdling(CommonDataForModes &commonData);

      /**
      * To manage the MA timeout train state.
      */
      virtual void manageMATimeout(CommonDataForModes &commonData);

      /**
      * Function to run the unregisteredStart state of unregistered mode
      */
      virtual void runUnregisteredStart();

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * Event to apply Standstill event after in PowerDown mode
      */
      const ATC::Event standstillInUnregisteredMode;

      /**
      * The current state of Unregistered mode
      */
      UnregisteredModeState modeState;

    };
  }
}

#endif
