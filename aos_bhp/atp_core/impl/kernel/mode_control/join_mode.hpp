#ifndef JoinMode_hpp
#define JoinMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Join mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-06-08    skothiya    Created
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
    typedef uint8_t JoinModeState;

    /**
    * The class JoinMode defines the Join ATP-mode.
    *
    */
    class JoinMode : public AbstractMode
    {
    public:

      /**
      * The state to wait for DMI Confirmation to enter Join mode
      */
      static const JoinModeState joinWaitJoinConfirmDMI = 1U;

      /**
      * The state used when driving the approaching train to stationary train
      */
      static const JoinModeState joinInProgress = 2U;

      /**
      * The state to wait for DMI Confirmation to enter Sleep mode when joined
      */
      static const JoinModeState joinWaitSleepConfirmDMI = 3U;

      /**
      * The start for transition to Sleep mode (approaching train)
      */
      static const JoinModeState joinFinishSleep = 4U;

      /**
      * The start for transition to Configuration mode (stationary train)
      */
      static const JoinModeState joinFinishConfig = 5U;

      /**
      * Transition state to accept MA in Join mode
      */
      static const JoinModeState joinWaitMAScratch = 6U;

      /**
      * Transition state to confirm MA in Join mode by driver on DMI
      */
      static const JoinModeState joinConfirmMAScratch = 7U;


      /**
      * Main run function of the mode.
      * @param[in] commonData   reference to train states
      *
      */
      virtual void handleMode(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      JoinMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeJoin enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Getter for the state of the join mode
      *
      * @return JoinModeState value of modeState variable.
      */
      virtual JoinModeState getModeState() const;

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const JoinModeState state, char_t* const buffer) const;

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the mode state is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

      /**
      * Virtual Function for Q route type check in Join mode
      *
      * @param[in] routeType Q route type of MA
      *
      * @return true if valid Q route type
      */
      virtual bool isValidQRouteType(const RouteType routeType) const;

      /**
      * Function to reset mode state
      */
      virtual void resetMode();

      /**
      * To get the status of validity of unconditional shortening message in join mode
      *
      * @return true if the unconditional shortening message is valid in the join mode
      */
      virtual bool isValidUncondShorteningMsg() const;

    protected:

      /**
      * The current state of Join mode
      */
      JoinModeState modeState;

      /**
      * Function to run the JoinWaitJoinConfirmDMI state of Join mode.
      *
      */
      virtual void runJoinWaitJoinConfirmDMI();

      /**
      * Function to run the JoinInProgress state of Join mode.
      *
      */
      virtual void runJoinInProgress();

      /**
      * Function to run the JoinWaitSleepConfirmDMI state of Join mode.
      *
      */
      virtual void runJoinWaitSleepConfirmDMI();

      /**
      * Function to run the JoinFinishSleep state of Join mode.
      *
      */
      virtual void runJoinFinishSleep();

      /**
      * Function to run the JoinFinishConfig state of Join mode.
      *
      */
      virtual void runJoinFinishConfig();

      /**
      * Function to run the JoinWaitMAScratch state of Join mode.
      *
      */
      virtual void runJoinWaitMAScratch();

      /**
      * Function to run the JoinConfirmMAScratch state of Join mode.
      *
      */
      virtual void runJoinConfirmMAScratch();

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * Event to apply Standstill event while waiting for confirmation for Join mode on DMI
      */
      const ATC::Event standstillWaitJoinConfirm;

      /**
      * Flag if a Join Command has been received
      */
      bool joinCommandReceived;
    };
  }
}

#endif

