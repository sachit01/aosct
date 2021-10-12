#ifndef ShuntingRouteMode_hpp
#define ShuntingRouteMode_hpp

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Shunting Route mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-04-27    skothiya    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_mode.hpp"
#include "event.hpp"

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
    typedef uint8_t TrainShuntingRouteModeState;

    /**
    * The class ShuntingRouteMode defines the Shunting Route mode.
    */
    class ShuntingRouteMode : public AbstractMode
    {

    public:

      /**
      * The start state of shuntingRouteStart mode
      */
      static const TrainShuntingRouteModeState shuntingRouteStart = 1U;

      /**
      * shuntingRouteWaitConfirmFromDMI mode to wait for Driver Confirmation
      */
      static const TrainShuntingRouteModeState shuntingRouteWaitConfirmFromDMI = 2U;

      /**
      * shuntingRouteAcceptedFromDMI mode if shunting route request accepted by driver
      */
      static const TrainShuntingRouteModeState shuntingRouteAcceptedFromDMI = 3U;

      /**
      * shuntingRouteWaitMAScratch mode to accept the MA from scratch
      */
      static const TrainShuntingRouteModeState shuntingRouteWaitMAScratch = 4U;

      /**
      * shuntingRouteConfirmMAScratch mode to confirm MA from scratch by driver on DMI
      */
      static const TrainShuntingRouteModeState shuntingRouteConfirmMAScratch = 5U;

      /**
      * Constructor.
      *
      */
      ShuntingRouteMode();

      /**
      * Handle function of the Shunting mode.
      * @param[in] commonData   reference to train states
      */
      virtual void handleMode(CommonDataForModes &commonData);

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeShuntingRoute enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Getter for the sub state of the shunting route mode
      *
      * @return TrainShuntingRouteModeState value of sub mode state variable.
      */
     virtual TrainShuntingRouteModeState getModeState() const;

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const TrainShuntingRouteModeState state, char_t* const buffer) const;

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the modestate is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

      /**
      * Virtual Function for Q route type check in Shunting Route mode
      */
      virtual bool isValidQRouteType(const RouteType routeType) const;

      /**
      * Function to reset mode state
      */
      virtual void resetMode();

      /**
      * To get the status of validity of unconditional shortening message in shunting route mode
      *
      * @return true if the unconditional shortening message is valid in the shunting route mode
      */
      virtual bool isValidUncondShorteningMsg() const;

    protected:

      /**
      * Function to run the shuntingRouteStart state of Shunting Route mode.
      *
      */
      virtual void runShuntingRouteStart();

      /**
      * Function to run the shuntingRouteWaitingConfirmationFromDMI state of Shunting Route mode.
      *
      */
      virtual void runShuntingRouteWaitConfirmFromDMI();

      /**
      * Function to run the shuntingRouteAcceptedFromDMI state of Shunting Route mode.
      *
      * @param[in] commonData   reference to train states
      */
      virtual void runShuntingRouteAcceptedFromDMI(CommonDataForModes &commonData);

      /**
      * Function to run the runShuntingRouteWaitMAScratch state of Shunting Route mode.
      *
      */
      virtual void runShuntingRouteWaitMAScratch();

      /**
      * Function to run the runShuntingRouteConfirmMAScratch state of Shunting Route mode.
      *
      */
      virtual void runShuntingRouteConfirmMAScratch();

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * The current state of TrainShuntingRoute submode
      */
      TrainShuntingRouteModeState subModeState;

      /**
      * Event to apply Standstill event while waiting for confirmation for Shunting Route mode on DMI
      */
      const ATC::Event standStillShuntingRouteConfirm;

      /**
      * Event for fatal failure in Shunting Route mode
      */
      const ATC::Event unKnownStateInShuntRoute;
    };
  }

}

#endif // !ShuntingRouteMode_hpp

