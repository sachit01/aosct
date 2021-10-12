#ifndef TrainConfigMode_hpp
#define TrainConfigMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the TrainConfig mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-21    arastogi    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_mode.hpp"
#include "radio_message_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    typedef uint8_t TrainConfigModeState;


    /**
    * The class TrainConfigModeState defines the Train Configuration mode.
    *
    */
    class TrainConfigMode : public AbstractMode
    {
    public:

      /**
      * The start state of TrainConfig mode
      */
      static const TrainConfigModeState trainConfigStart = 1U;

      /**
      * TrainConfig mode to wait for setup from ATP
      */
      static const TrainConfigModeState trainConfigWaitSetupFrmATP = 2U;

      /**
      * TrainConfig mode to wait for setup from TCC
      */
      static const TrainConfigModeState trainConfigWaitSetupFrmTCC = 3U;

      /**
      * TrainConfig mode to wait for config from DMI
      */
      static const TrainConfigModeState trainConfigWaitNewConfigDMI = 4U;

      /**
      * TrainConfig mode to wait for response from TCC
      */
      static const TrainConfigModeState trainConfigConfirmNewConfigFrmTCC = 5U;

      /**
      * TrainConfig mode to send the re-reg data to DMI
      */

      static const TrainConfigModeState trainConfigSendReRegDataToDMI = 6U;

      /**
      * TrainConfig mode to wait for confirmation of re-reg from DMI
      */

      static const TrainConfigModeState trainConfigConfirmReRegFrmDMI = 7U;

      /**
       * TrainConfig mode to wait for config from TIC
       */
      static const TrainConfigModeState trainConfigWaitTIC = 8U;

      /**
      * TrainConfig mode to compare config from TIC
      */
      static const TrainConfigModeState trainConfigTICvsTS = 9U;

      /**
      * TrainConfig mode to wait for driver to accept Automatic Configuration
      */
      static const TrainConfigModeState trainConfigWaitForAcceptAutomatic = 10U;

      /**
      * TrainConfig mode to send startup message for new config to TCC
      */
      static const TrainConfigModeState trainConfigSendStartUpForNewConfig = 11U;

      /**
      * TrainConfig mode to send startup message in re-reg to TCC
      */
      static const TrainConfigModeState trainConfigSendStartUpForReReg = 12U;

      /**
      * TrainConfig mode if TSetup is rejected by TCC
      */
      static const TrainConfigModeState trainConfigTSetupRejected = 13U;

      /**
      * TrainConfig mode if TSetup is accepted by TCC
      */
      static const TrainConfigModeState trainConfigTSetupAccepted = 14U;

      /**
      * TrainConfig mode if configuration finished OK
      */
      static const TrainConfigModeState trainConfigFinishOK = 15U;

      /**
      * TrainConfig mode if configuration finished not OK
      */
      static const TrainConfigModeState trainConfigFinishNOK = 16U;

      /**
      * TrainConfig mode if Q_Setup from Train Setup is Reconfiguration
      */
      static const TrainConfigModeState trainConfigReconfiguration = 17U;

      /**
      * TrainConfig mode for trainConfigRejectedOrAborted from AOS
      */
      static const TrainConfigModeState trainConfigRejectedOrAborted = 18U;

      /**
      * TrainConfig mode to ask for departure test
      */
      static const TrainConfigModeState trainConfigWaitForDepartureTest = 19U;

      /**
      * Main run function of the mode.
      * @param[in] commonData   reference to common data
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
      TrainConfigMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeConfiguration enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Getter for the state of the power up mode
      *
      * @return TrainConfigModeState value of modeState variable.
      */
      virtual TrainConfigModeState getModeState() const;

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      virtual void getModeStateString(const TrainConfigModeState state, char_t* const buffer) const;

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the modestate is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

    protected:

      /**
      * Calls the appropriate run function depending in the current mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runModeFunction(CommonDataForModes &commonData);

      /**
      * Function to run the trainConfigStart state of TrainConfig mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runTrainConfigStart(CommonDataForModes &commonData);

      /**
      * Function to run the trainConfigWaitSetUpFrmATP state of TrainConfig mode.
      */
      virtual void runTrainConfigWaitSetUpFrmATP();

      /**
      * Function to run the trainConfigWaitReRegTCC state of TrainConfig mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runTrainConfigWaitSetupFrmTCC(CommonDataForModes &commonData);

      /**
      * Function to run the trainConfigConfirmReRegFrmDMI state of TrainConfig mode.
      */
      virtual void runTrainConfigConfirmReRegFrmDMI();

      /**
      * Function to run the trainConfigConfirmNewConfigFrmTCC state of TrainConfig mode.
      */
      virtual void runTrainConfigConfirmNewConfigFrmTCC();

      /**
      * Function to run the trainConfigSendReRegDataToDMI state of TrainConfig mode.
      */

      virtual void runTrainConfigSendReRegDataToDMI();

      /**
      * Function to run the trainConfigWaitTIC state of TrainConfig mode.
      * @param[in] commonData   reference to common data shared between different modes and mode control class
      */
      virtual void runTrainConfigWaitTIC(CommonDataForModes &commonData);

      /**
      * Function to run the trainConfigTICvsTS state of TrainConfig mode.
      */
      virtual void runTrainConfigTICvsTS();

      /**
      * Function to run the trainConfigSendStartUpForNewConfig state of TrainConfig mode.
      */
      virtual void runTrainConfigSendStartUpForNewConfig();

      /**
      * Function to run the trainConfigSendStartUpForReReg state of TrainConfig mode.
      */
      virtual void runTrainConfigSendStartUpForReReg();

      /**
      * Function to run the trainConfigTSetupRejected state of TrainConfig mode.
      */
      virtual void runTrainConfigTSetupRejected();

      /**
      * Function to run the trainConfigTSetupAccepted state of TrainConfig mode.
      */
      virtual void runTrainConfigTSetupAccepted();

      /**
      * Function to run the trainConfigFinishOK state of TrainConfig mode.
      */
      virtual void runTrainConfigFinishOK();

      /**
      * Function to run the trainConfigFinishNOK state of TrainConfig mode.
      */
      virtual void runTrainConfigFinishNOK();
      
      /**
      * Function to run the trainConfigRejectedOrAborted state of TrainConfig mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runTrainConfigRejectedOrAborted(CommonDataForModes &commonData);

      /**
      * Function to run the runTrainConfigWaitForDepartureTest state of TrainConfig mode.
      */
      virtual void runTrainConfigWaitForDepartureTest();

      /**
      * Function to run the trainConfigWaitForAcceptAutomatic state of TrainConfig mode.
      */
      virtual void runTrainConfigWaitForAcceptAutomatic();

      /**
      * Function to run the trainConfigWaitNewConfigDMI state of TrainConfig mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runTrainConfigWaitNewConfigDMI(CommonDataForModes &commonData);


      /**
      * The current state of TrainConfig mode
      */
      TrainConfigModeState modeState;

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
      * Function to handle if train setup is rejected
      */
      virtual void handleConfigAndTSetupRejected();

      /**
      * Function for handling Abort Setup by DMI.
      * @param[in] commonData   reference to common data shared between different modes and mode control class
      */
      virtual void handleAbortSetup(CommonDataForModes &commonData);

    private:

      /**
      * To indicate new configuration and re configuration
      */
      bool isNewConfig;

      /**
      * To indicate whether TSetup accepted during Re-registration or not
      * True = Accepted, False = Rejected
      */
      bool isTsetupAcceptedByDriverInReReg;

      /**
      * Event to apply Standstill and not allow movement in Train Configuration
      */
      const ATC::Event noMovementInConf;

      /**
      * Event for fatal failure in Train Configuration
      */
      const ATC::Event unKnownStateInConf;

    };
  }
}

#endif
