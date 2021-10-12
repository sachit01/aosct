#ifndef TrainRegistrationMode_hpp
#define TrainRegistrationMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Train registration mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-25    arastogi    Created
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
    typedef uint8_t TrainRegistrationModeState;

    /**
    * The class TrainRegistrationMode defines the train registration mode.
    *
    */
    class TrainRegistrationMode : public AbstractMode
    {
    public:

      /**
      * The start state of TrainRegistration mode
      */
      static const TrainRegistrationModeState trainRegistrationStart = 1U;

      /**
      * The state of TrainRegistration mode to wait for Re-registration MA
      */
      static const TrainRegistrationModeState trainRegistrationWaitReRegMA = 2U;

      /**
      * The state of TrainRegistration mode to send train vs track direction info to DMI
      */
      static const TrainRegistrationModeState trainRegistrationSendTrackDirToDMI = 3U;

      /**
      * The state of TrainRegistration mode to wait for train vs track direction
      */
      static const TrainRegistrationModeState trainRegistrationWaitTrackDir = 4U;

      /**
      * The state of TrainRegistration mode to setup a balise search target
      */
      static const TrainRegistrationModeState trainRegistrationSetupBSTarget = 5U;

      /**
      * The finish state of TrainRegistration mode
      */
      static const TrainRegistrationModeState trainRegistrationFinish = 6U;

      /**
      * The state of TrainRegistration mode for reposition
      */
      static const TrainRegistrationModeState trainRegistrationRePosition = 7U;

      /**
      * TrainRegistration mode sub state to waiting for Approximate Message in case of repositioning
      */
      static const TrainRegistrationModeState trainRegistrationWaitForApprxMesg = 8U;

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
      TrainRegistrationMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeRegistration enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Getter for the state of the TrainRegistration mode
      *
      * @return TrainRegistrationModeState value of modeState variable.
      */
      virtual TrainRegistrationModeState getModeState() const;

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const TrainRegistrationModeState state, char_t* const buffer) const;

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the modestate is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

      /**
      * Virtual Function for Q route type check in Registration mode
      */
      virtual bool isValidQRouteType(const RouteType routeType) const;

    protected:

      /**
      * Function to run the trainRegistrationStart state of TrainRegistration mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runTrainRegistrationStart(CommonDataForModes &commonData);

      /**
      * Function to run the trainRegistrationWaitReRegMA state of TrainRegistration mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runTrainRegistrationWaitReRegMA(CommonDataForModes &commonData);

      /**
      * Function to run the trainRegistrationSendTrackDirToDMI state of TrainRegistration mode.
      *
      */
      virtual void runTrainRegistrationSendTrackDirToDMI();

      /**
      * Function to run the trainRegistrationWaitTrackDir state of TrainRegistration mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runTrainRegistrationWaitTrackDir(CommonDataForModes &commonData);

      /**
      * Function to run the trainRegistrationSetupBSTarget state of TrainRegistration mode.
      */

      virtual void runTrainRegistrationSetupBSTarget();

      /**
      * Function to run the trainRegistrationFinish state of TrainRegistration mode.
      */
      virtual void runTrainRegistrationFinish();

      /**
      * Function to run the runTrainRegistrationRePos state of TrainRegistration mode.
      */
      virtual void runTrainRegistrationRePos();

      /**
      * Function to run the runTrainRegistrationWaitForApproxMesg state of TrainRegistration mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runTrainRegistrationWaitForApproxMesg(CommonDataForModes &commonData);

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
      * The current state of TrainRegistration mode
      */
      TrainRegistrationModeState modeState;

      /**
      * The train travel direction entered from DMI
      */
      TravelDir dmiTravelDir;

      /**
      * Event to apply standstill event and not allow movement in Train Registration
      */
      const ATC::Event noMovementinREG;

      /**
      * Event for an InValid TSetup in Train Registration
      */
      const ATC::Event inValidTsetupInREG;

      /**
      * Event for Incorrect Q_SETUP and Position combination Error
      */
      const ATC::Event incorrectQsetupAndPosCombination;

      /**
      * Event for an InValid MA in Train Re-Registration
      */
      const ATC::Event inValidMAReceivedInReRegistration;

      /**
      * Event for Emergency Alert set in Registration Mode
      */
      const ATC::Event emergencyAlertInRegistrationMode;
    };
  }
}

#endif
