#ifndef PowerDownMode_hpp
#define PowerDownMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the PowerDown mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-03    arastogi    Created
* 2017-11-06    keisele     implement powering down sequence
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
    /**
    * The class PowerDownMode defines the power down atp mode.
    *
    */
    class PowerDownMode : public AbstractMode
    {
    public:

      /**
      * * Poweringdown submodes 
      */
      enum PowerDownModeState
      {
        powerDownStateSendLog = 1U,
        powerDownStateWaitPositionSent = 2U,
        powerDownStateTurnOffLight = 3U
      };

      /**
      * Main run function of the mode.
      * @param[in] commonData   reference to train states
      */
      virtual void handleMode(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      PowerDownMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModePoweringDown enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Test weather the conditions for entering Powering Down mode are met
      *
      * @return true if Powering Down should be entered
      */
      bool powerDownRequested();

      /**
      * Return the value of the PowerOff digital output used by LocoIO
      *
      * @return value of powerOff signal
      */
      bool powerOffSignalValue() const;

    protected:

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
      * Implement the blinking while powering down
      */
      void blink(CommonDataForModes &commonData);

      /**
      * Event to apply Standstill in PowerDown mode
      */
      const ATC::Event standstillInPowerDownMode;

      /**
      * Event to log PoweringDown sequence 
      */
      const ATC::Event powerDownSequenceStarted;

      /**
      * The current state of PowerDownMode mode
      */
      PowerDownModeState modeState;

      /**
      * Start value of AOS Off press detection timer
      */
      int64_t offInPressTimerStart;

      /**
      * Value indicating weather AOS Off press detection timer is active
      */
      bool offInPressTimerActive;

      /**
      * Value used in AOS Off press rising edge detection
      */
      bool lastOffIn;

      /**
      * Value indicating weather lastOffIn is valid
      */
      bool lastOffInValid;

      /**
      * Start value of two-position-report-sent timeout
      */
      int64_t lastNumPositionMessagesTime;

      /**
      * Number of previously sent position reports
      */
      uint32_t lastNumPositionMessages;

      /**
      * This flag is reset in constructor and set when the 
      * power-down conditions are met. Once it is set it is never cleared.
      */
      bool fPowerDownRequested;

      /**
      * Start value of the blink timeout 
      */
      int64_t blinkLastToggle;
      
      /**
      * flag to prevent multiple status messages to be sent 
      */
      bool statusSent;
    };
  }
}

#endif
