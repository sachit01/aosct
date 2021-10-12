#ifndef SplitMode_hpp
#define SplitMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Split mode.
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
    typedef uint8_t SplitModeState;

    /**
    * The class SplitMode defines the Split ATP-mode.
    *
    */
    class SplitMode : public AbstractMode
    {
    public:

      /**
      * The start state of Split mode
      */
      static const SplitModeState splitStart = 0U;

      /**
      * The start state of SplitMode, waiting for confirmation to enter Split mode from DMI.
      */
      static const SplitModeState splitWaitConfirmDMI = 1U;

      /**
      * The state to wait for confirmation to enter Configuration mode from DMI.
      */
      static const SplitModeState splitProcessing = 2U;

      /**
      * The end state of Split mode, entering Configuration mode.
      */
      static const SplitModeState splitFinishConfig = 3U;

      /**
      * Main run function of the mode.
      * @param[in] commonData   reference to train states
      */
      virtual void handleMode(CommonDataForModes &commonData);

      /**
      * Function to reset mode state
      */
      virtual void resetMode();

      /**
      * Constructor.
      *
      */
      SplitMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeSplit enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Getter for the state of the split mode
      *
      * @return SplitModeState value of modeState variable.
      */
      virtual SplitModeState getModeState() const;

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const SplitModeState state, char_t* const buffer) const;

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the mode state is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

      /**
      * Virtual Function for Q route type check in Split mode
      *
      * @param[in] routeType Q route type of MA
      *
      * @return true if valid Q route type
      */
      virtual bool isValidQRouteType(const RouteType routeType) const;

      /**
      * To get the status of validity of unconditional shortening message in split mode
      *
      * @return true if the unconditional shortening message is valid in the split mode
      */
      virtual bool isValidUncondShorteningMsg() const;

    protected:
       
       /**
       * Function to run the SplitStart state of Split mode
       */
       virtual void runSplitStart();
       
      /**
      * Function to run the splitWaitConfirmDMI state of Split mode
      */
      virtual void runSplitWaitConfirmDMI();

      /**
      * Function to run the splitProcessing state of Split mode
      */
      virtual void runSplitProcessing();

      /**
      * Function to run the splitFinishConfig state of Split mode
      */
      virtual void runSplitFinishConfig();

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * The current state of Sleep mode
      */
      SplitModeState modeState;

      /**
      * Event to apply Standstill event when waiting for confirmation to enter Split mode in DMI.
      */
      const ATC::Event standstillWaitForConfirm;

    };
  }
}

#endif
