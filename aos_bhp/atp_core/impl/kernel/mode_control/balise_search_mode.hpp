#ifndef BaliseSearchMode_hpp
#define BaliseSearchMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Balise Search mode.
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
#include "event.hpp"
#include "abstract_decode.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    typedef uint8_t BaliseSearchModeState;

    /**
    * The class BaliseSearchMode defines the Balise Search mode.
    *
    */
    class BaliseSearchMode : public AbstractMode
    {
    public:

      /**
      * The start state of Balise Search mode
      */
      static const BaliseSearchModeState baliseSearchStart = 1U;

      /**
      * The state of Balise Search mode to wait for balise for new registration
      */
      static const BaliseSearchModeState baliseSearchWaitForBaliseReg = 2U;

      /**
      * The state of Balise Search mode to wait for balise for re-registration
      */
      static const BaliseSearchModeState baliseSearchWaitForBaliseReReg = 3U;

      /**
      * The state of Balise Search mode to wait for MA
      */
      static const BaliseSearchModeState baliseSearchWaitMA = 4U;

      /**
      * The state of Balise Search mode to wait for second balise
      */
      static const BaliseSearchModeState baliseSearchWaitBalise2 = 5U;

      /**
      * The finish state of Balise Search mode if successful
      */
      static const BaliseSearchModeState baliseSearchFinishOK = 6U;

      /**
      * The finish state of Balise Search mode if failed
      */
      static const BaliseSearchModeState baliseSearchFinishNOK = 7U;

      /**
      * Balise Search sub state when abort button is pressed
      */
      static const BaliseSearchModeState baliseSearchConfigAborted = 8U;

      /**
      * Balise Search sub state when Unregistration message from TCC received before receiving first MA
      */
      static const BaliseSearchModeState baliseSearchHandleUnregWithoutMA = 9U;

      /**
      * Main run function of the mode.
      *
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
      BaliseSearchMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeBaliseSearch enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Getter for the state of the BaliseSearch mode
      *
      * @return BaliseSearchModeState value of modeState variable.
      */
      virtual BaliseSearchModeState getModeState() const;

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const BaliseSearchModeState state, char_t* const buffer) const;

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the modestate is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

      /**
      * Virtual Function for Q route type check in Balise Search mode
      */
      virtual bool isValidQRouteType(const RouteType routeType) const;

      /**
      * To get the status of validity of unconditional shortening message in balise search mode
      *
      * @return true if the unconditional shortening message is valid in the balise search mode
      */
      virtual bool isValidUncondShorteningMsg() const;

    protected:

      /**
      * Function to run the baliseSearchStart state of BaliseSearch mode.
      *
      */
      virtual void runBaliseSearchStart(CommonDataForModes &commonData);

      /**
      * Function to run the baliseSearchWaitForBaliseReg state of BaliseSearch mode.
      *
      */
      virtual void runBaliseSearchWaitForBaliseReg();

      /**
      * Function to run the baliseSearchWaitForBaliseReReg state of BaliseSearch mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runBaliseSearchWaitForBaliseReReg(CommonDataForModes &commonData);

      /**
      * Function to run the baliseSearchWaitMA state of BaliseSearch mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runBaliseSearchWaitMA(CommonDataForModes &commonData);

      /**
      * Function to run the baliseSearchWaitBalise2 state of BaliseSearch mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runBaliseSearchWaitBalise2(CommonDataForModes &commonData);

      /**
      * Function to run the baliseSearchFinishOK state of BaliseSearch mode.
      *
      */
      virtual void runBaliseSearchFinishOK();

      /**
      * Function to run the baliseSearchHandleUnregWithoutMA state of BaliseSearch mode.
      *
      */
      virtual void runbaliseSearchHandleUnregWithoutMA();

      /**
      * Function to run the baliseSearchFinishNOK state of BaliseSearch mode.
      *
      */
      virtual void runBaliseSearchFinishNOK();
      /**
      * Function to run the baliseSearchConfigAborted state of BaliseSearch mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runBaliseSearchConfigAborted(CommonDataForModes &commonData);

      /**
      * Virtual Function for checking Unregistration message in Balise Search mode
      */
      virtual bool handleUnRegMessage(void);

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
      * The current state of BaliseSearch mode
      */
      BaliseSearchModeState modeState;

      /**
      * The odometer reading for the balise search distance in re registration
      */
      OdoPosition baliseSearchOdometerReReg;

      /**
      * Log event for detecting the first balise
      */
      const ATC::Event logFirstBalise;

      /**
      * Log event for detecting the second balise
      */
      const ATC::Event logSecondBalise;

      /**
      * Event for fatal failure in Balise Search mode
      */
      const ATC::Event unknownState;

      /**
      * Event for balise not found error in balise search mode
      */
      const ATC::Event baliseNotFoundInBS;

      /**
      * Event for max search distance of wait for MA in balise search mode
      */
      const ATC::Event exceededBalSearchDistForSecBal;

      /**
      * Event for Invalid Q route type in Balise Search Mode
      */
      const ATC::Event inCorrectRouteTypeInBS;



      /**
      * Event for 2nd balise found before MA in balise search
      */
      const ATC::Event secondBaliseFoundBeforeMA;

      /**
      * Event for Emergency Alert set in Balise Search Mode
      */
      const ATC::Event emergencyAlertInBS;

      /**
      * Event for unconditional shortening message in Balise Search Mode
      */
      const ATC::Event unConditionalShortInBS;

      /**
      * SB Event if Unregistration message received without before receiving MA
      */
      const ATC::Event unRegMsgSBEvent;

      /**
      * Storage for second balise
      */
      Pos::AbstractDecode::BaliseInfo  secondBaliseInfo;

    };
  }
}

#endif
