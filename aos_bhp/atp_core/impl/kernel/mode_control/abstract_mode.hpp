#ifndef AbstractMode_hpp
#define AbstractMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the abstract mode class which all modes must inherit.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-19    arastogi    Created
* 2017-06-14    skothiya    Updated for train states implementation
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "event.hpp"

#include "radio_message_types.hpp"


/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    typedef uint8_t ModeState;

    /**
    * Constant for the ma margin for primary target when searching for first balise(new registration).
    */
    static const uint32_t maMarginBaliseSearch = 600U;

    /**
    * FreeRollingLocationButton structure contains the data which is used to display the buttons in location mode
    */
    struct FreeRollingLocationButton
    {
      /**
      * To indicate free rolling status display data to DMI.
      */
      bool freeRollingDisplayToDMI;

      /**
      * To indicate free rolling Confirmed data to DMI.
      */
      bool freeRollingConfirmToDMI;
    };

    /**
    * The CommonDataForModes structure contain the data which is shared between different modes and required in Abstract_Mode_Control class
    * Note that AbstractModeControl must clear this struct in its constructor.
    */
    struct CommonDataForModes
    {
      /**
      * Clear function
      */
      void clear();

      /**
      * To indicate if the stop train message is received from TCC.
      */
      bool stopTrainActive;

      /**
      * To indicate if the train is idling.
      */
      bool idling;

      /**
      * To indicate if MA has timed out.
      */
      bool maTimeOut;

      /**
      * To indicate Train Status Handling Done.
      */
      bool handlingDone;

      /**
      * To indicate Odometer Invalid state
      */
      bool odometerInvalid;

      /**
      * To indicate Free Rolling state
      */
      bool freeRolling;

      /**
      * To indicate the ATP OK status signal
      */
      bool atpOkStatus;

      /**
      * To indicate the ATP Lamp status signal
      */
      bool atpLampStatus;

      /**
      * To indicate if the ATP has been reset.
      */
      bool isATPReset;

      /**
      * To indicate new configuration and re configuration
      */
      bool isNewConfig;

      /**
      * To indicate mode requested by DMI
      */
      ATPMode modeReqByDMI;

      /**
      * To indicate buzzer raised by mode
      */
      BuzzerType buzzer;

      /**
      * To indicate Abort Setup Button Status
      */
      bool isAbortSetupActive;

      /**
      * Odo-direction (orientation in track from DMI).
      */
      OdoDir odoDirectionNewReg;

      /**
      * To indicate allowed to enter location Mode
      */
      bool isAllowedToEnterLocationMode;

      /**
      * To indicate current driving direction
      */
      TravelDir currentDrivDirection;

      /**
      * To indicate free rolling in location
      */
      FreeRollingLocationButton freeRollingButton;

      /**
      * To indicate if setup aborted
      */
      bool trainSetupAborted;

      /**
      * Abort Setup Reason
      */
      AbortReason abortSetupReason;
    };

    /**
    * The class AbstractMode defines the mode.
    *
    */
    class AbstractMode
    {
    public:

      /**
      * Implements the virtual init function.
      */
      virtual bool init(void);

      /**
      * Main run function of the mode.
      * @param[in] commonData   reference to train states
      */
      virtual void handleMode(CommonDataForModes &commonData) = 0;

      /**
      * Getter for the nextMode variable.
      *
      * @return ATPMode enum value of the nextMode variable.
      */
      ATPMode getNextModeId() const;

      /**
      * Virtual function to reset mode state. This function is called when a new mode is entered
      *
      */
      virtual void resetMode();

      /**
      * Pure virtual function for name of the mode.
      *
      * @return ATPMode Enum of the mode.
      */
      virtual ATPMode getModeId() = 0;

      /**
      * Retrieves the mode state as a string.
      *
      * @param[out] str The string where the mode state is copied to.
      * @return true if the mode has a mode state, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

      /**
      * Getter for the state of the power up mode
      *
      * @return value of mode state of a mode.
      */
      virtual ModeState getModeState() const;

      /**
      * Virtual Function for Q route type check
      */
      virtual bool isValidQRouteType(const RouteType routeType) const;

      /**
      * The start state of PowerUp mode
      */
      static const ModeState undefinedModeState = 0U;

      /**
      * Maximum size of the modeState string (including the NULL character)
      */
      static const uint8_t maxModeStateNameLength = 50U;

      /**
      * Virtual Function for setting maximum allowed speed for location mode
      */
      virtual void setMaxAllowedSpeedInLocation(const uint16_t speed);

      /**
      * To get the status of validity of unconditional shortening message in respective mode
      * 
      * @return true if the unconditional shortening message is valid in the respective mode
      */
      virtual bool isValidUncondShorteningMsg() const;

    protected:

      /**
      * Constructor.
      *
      */
      AbstractMode();

      /**
      * Destructor.
      * Virtual destructor for pure abstract class.
      *
      */
      virtual ~AbstractMode(void);

      /**
      * Setter for the nextMode variable.
      *
      *  @param[in] mode   Value for next mode.
      */
      void setNextMode(const ATPMode mode);

      /**
      * manage stop train state
      * @param[in] commonData   reference to train states
      */
      virtual void manageStopTrain(CommonDataForModes &commonData);

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
      * To manage the TCC timeout functionality
      */
      virtual void manageTCCTimeOut();

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

      /**
      * To manage the Free Rolling train state.
      * @param[in] commonData   reference to train states
      */
      virtual void manageFreeRolling(CommonDataForModes &commonData);

      /**
      * To manage the Odometer Invalid train state.
      * @param[in] commonData   reference to train states
      */
      virtual void manageOdometerInvalid(CommonDataForModes &commonData);

      /**
      * To manage the entering to location mode
      * @param[in] commonData   reference to train states
      */
      virtual void manageLocationModeValidity(CommonDataForModes &commonData);

      /**
      * To manage Handling done flag
      * @param[in] commonData   reference to train states
      */
      virtual void manageHandlingDone(CommonDataForModes &commonData);

      /**
      * To Check if Received Q_ROUTE_TYPE parameter of MA message has changed the mode and get the changed mode.
      */
      virtual bool isModeChangedByMA(ATPMode& mode) const;

      /**
      * Remove the tracks, targets and balises which the train has passed.
      * @param[in] commonData   reference to train states
      */
      void removePassedObjects(const CommonDataForModes &commonData) const;

      /**
      * Function for handling unregistration message received from TCC.
      */
      virtual bool handleUnRegMessage(void);

      /**
      * Function for handling Abort Setup by DMI.
      * @param[in] commonData   reference to train states
      */
      virtual void handleAbortSetup(CommonDataForModes &commonData);

      /**
      * Trace interface to be used
      */
      ATC::TraceInterface *trace;

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * qSetupFromTSetup to be used to hold the value in Q_SETUP
      * parameter will be used to communicated between different mode classes
      */
      static TrainSetupReason qSetupFromTSetup;

      /**
      * isValidQSetupReceived , true if valid Q_SETUP received in Tsetup message
      *
      */
      static bool isValidQSetupReceived;

      /**
      * maxSearchDistanceInReReg for max search distance in re-reg for first balise
      *
      */
      static uint32_t maxSearchDistanceInReReg;

      /**
      * To manage current driving direction
      * @param[in] commonData   reference to train states
      */
      virtual void handleCurrentDrivDirection(CommonDataForModes &commonData);

    private:

      /**
      * The enum of the nexMode to transition to.
      * This will be ATPModeUndefined if no transition is allowed.
      *
      */
      ATPMode nextMode;

      /**
      * To apply standstill event in Train Idle Status
      */
      const ATC::Event idleTrainStateStandstill;

      /**
      * To apply service break in case Odometer Invalid Status
      */
      const ATC::Event odometerInvalidSB;

      /**
      * Event for TCC communication timeout error
      */
      const ATC::Event tccConnectionLost;

      /**
      * Event for TCC communication timeout error in standstill
      */
      const ATC::Event tccConnectionLostInStandstill;

      /**
      * Event for MA Timeout
      */
      const ATC::Event maTimeout;

      /**
      * Event for Stop Train requested
      */
      const ATC::Event stopTrainRequested;

    };

  }
}

#endif
