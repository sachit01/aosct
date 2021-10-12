#ifndef SafeBrakeToStopMode_hpp
#define SafeBrakeToStopMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Safe Brake to Stop mode.
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
    typedef uint8_t SafeBrakeToStopModeState;

    /**
    * The class SafeBrakeToStopMode defines the Safe Brake to Stop ATP mode.
    *
    */
    class SafeBrakeToStopMode : public AbstractMode
    {
    public:

      /** 
      * The start state for Safe Brake To Stop mode
      */
      static const SafeBrakeToStopModeState safeBrakeToStopStart = 1U;

      /**
      * The  wait MAstate for Safe Brake To Stop mode
      */
      static const SafeBrakeToStopModeState safeBrakeToStopWaitMA = 2U;

      /**
      * The finish state for Safe Brake To Stop mode
      */
      static const SafeBrakeToStopModeState safeBrakeToStopFinishOk = 3U;

      /**
      * Main run function of the mode.
      * @param[in] commonData   reference to train states
      */
      virtual void handleMode(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      SafeBrakeToStopMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeNormal enum value.
      */
      virtual ATPMode getModeId();

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const SafeBrakeToStopModeState state, char_t* const buffer) const;

      /**
      * Retrieves the mode state as a string.
      *
      * @param[out] str The string where the mode state is copied to.
      * @return true if the mode has a mode state, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

      /**
      * Virtual Function for Q route type check in Safe Brake To Stop mode
      *
      *@param[in] routeType The Q route type
      *@return true if valid Q_ROUTE_TYPE received
      */
      virtual bool isValidQRouteType(const RouteType routeType) const;

      /**
      * Getter for the state of the Safe Brake To Stop mode 
      *
      * @return SafeBrakeToStopModeState value of modeState variable.
      */
      virtual SafeBrakeToStopModeState getModeState() const;

      /**
      * Function to reset mode state
      */
      virtual void resetMode();

    protected:

      /**
      * The current state of Safe Brake To Stop mode
      */
      SafeBrakeToStopModeState modeState;

      /**
      * Function to run the runSafeBrakeToStopStart state of Safe Brake To Stop mode
      *
      */
      virtual void runSafeBrakeToStopStart();

      /**
      * Function to run the runsafeBrakeToStopWaitMA state of Safe Brake To Stop mode
      *
      */
      virtual void runSafeBrakeToStopWaitMA();

      /**
      * Function to run the runsafeBrakeToStopFinishOk state of Safe Brake To Stop mode
      *
      */
      virtual void runSafeBrakeToStopFinishOk();

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
      * Event to apply SB after in SafeBrake mode
      */
      const ATC::Event sbInSafeBrakeToStopMode;

      /**
      * Event to raise Safety Halt if valid train setup not exist
      */
      const ATC::Event safetyHaltInvalidTSetup;

    };
  }
}

#endif
