#ifndef LocationMode_hpp
#define LocationMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Location mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-04-26    spandita    Created
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
    typedef uint8_t LocationModeState;

    /**
    * The class LocationMode defines the location mode.
    *
    */
    class LocationMode : public AbstractMode
    {
    public:

      /**
      * The start state of Location mode
      */
      static const LocationModeState locationStart = 1U;

      /**
      * The state of location mode to check for location type
      */
      static const LocationModeState locationWaitToConfirmType = 2U;

      /**
      * The state of location mode to run initial start of yard handling
      */
      static const LocationModeState locationYardModeHandlingStart = 4U;

      /**
      * The state of location mode to run yard mode handling done
      */
      static const LocationModeState locationYardModeHandlingDone = 5U;

      /**
      * The state of location mode for wait of yard Ack
      */
      static const LocationModeState locationYardModWaitAck = 6U;

      /**
      * The state of location mode to run unload
      */
      static const LocationModeState locationUnloadLocation = 7U;

      /**
      * The state of location mode to run finish
      */
      static const LocationModeState locationFinishOk = 8U;

      /**
      * Main run function of the location mode
      * @param[in] commonData   reference to train states
      *
      */
      virtual void handleMode(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      LocationMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeLocation enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the modestate is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const LocationModeState state, char_t* const buffer) const;

      /**
      * Virtual Function for Q route type check in Location mode
      *
      * @param[in] routeType Type of MA
      */
      virtual bool isValidQRouteType(const RouteType routeType) const;

      /**
      * Getter for the state of the location mode
      *
      * @return LocationModeState value of modeState variable.
      */
      virtual LocationModeState getModeState() const;

      /**
      * Setter Function to set the maximum allowed speed.
      *
      * @param[in] speed   max allowed speed in location as defined in LOCATION_BORDERS
      */
      virtual void setMaxAllowedSpeedInLocation(const uint16_t speed);

      /**
      * Function to reset mode state.
      *
      */
      virtual void resetMode();

    protected:

      /**
      * Function to run the runLocationStart state of Location mode
      * @param[in] commonData   reference to train states
      */
      virtual void runLocationStart(CommonDataForModes &commonData);

      /**
      * Function to run the locationWaitToConfirmType state of Location mode
      *
      */
      virtual void runLocationWaitToConfirmType();

      /**
      * Function to run the locationYardModeHandlingDone state of Location mode
      *
      */
      virtual void runLocYardModeHandlingDone();

      /**
      * Function to run the locationYardModeHandlingStart state of Location mode
      *
      */
      virtual void runLocYardModeHandlingStart();

      /**
      * Function to run the locationYardModWaitAck state of Location mode
      *
      */
      virtual void runLocYardModWaitAck();

      /**
      * Function to run the locationUnloadLocation state of Location mode
      * @param[in] commonData   reference to train states
      */
      virtual void runLocUnloadLocation(CommonDataForModes &commonData);

      /**
      * Function to run the locationFinishOk state of Location mode
      * @param[in] commonData   reference to train states
      */
      virtual void runLocFinishOK(CommonDataForModes &commonData);


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

      /**
      * To manage the current driving direction in location mode.
      * @param[in] commonData   reference to train states
      */
      virtual void handleCurrentDrivDirection(CommonDataForModes &commonData);

      /**
      * To manage Handling done flag
      * @param[in] commonData   reference to train states
      */
      virtual void manageHandlingDone(CommonDataForModes &commonData);

      /**
      * When handling done has been set
      * @param[in] commonData   reference to train states
      */
      void setHandlingDone(CommonDataForModes& commonData);

    private:

      /**
      * To handle the standstill condition in location mode
      */
      virtual void handleStandStillCondition() const;

      /**
      * To handle the change mode condition in location mode
      */
      void handleModeChangeCondition();

      /**
      * To handle the gradient values in location mode
      */
      virtual void handleGradInLocation(CommonDataForModes &commonData) const;

      /**
      * The current state of Location mode
      */
      LocationModeState modeState;

      /**
      * Maximum allowed Speed In location
      */
      uint16_t maxAllowedSpeeInLoc;

    };
  }
}
#endif
