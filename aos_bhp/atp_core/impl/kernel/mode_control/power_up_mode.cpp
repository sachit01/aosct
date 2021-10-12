/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the PowerUpMode class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-19    arastogi    Created
* 2016-10-17    arastogi    Set ATP reset to true when in powerup
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_atp_application.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_btm_handler.hpp"
#include "abstract_config.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_odometry.hpp"
#include "abstract_radio_handler.hpp"
#include "atc_util.hpp"
#include "dmi_event_codes.hpp"
#include "loco_io.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "vehicle_com.hpp"
#include "abstract_mode_control_event_ids.hpp"
#include <vfw_string.h>

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    PowerUpMode::PowerUpMode() : AbstractMode(),
      modeState(powerUpStart),
      noMovementInPOU(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdSBinPowerUp,
        ATC::NoSB, 0x0U, "No movement allowed in PowerUp Mode")),
      safetyHaltInPOU(ATC::Event::createSafetyHaltEvent(atpModeControlId, ATC::CoreContainer, eventIdFFinPowerUp,
        ATC::NoEB, DMICom::modPwrUpModFailure, "Health supervision and Core integrity test Failed")),
      powerUpSequenceStarted(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer, eventIdPowerUpSeqStarted,
        0U, "Power Up sequence started")),
      dispatcherVersionMismatch(ATC::Event::createSafetyHaltEvent(atpModeControlId, ATC::CoreContainer, eventIdDispatcherVersionMismatch,
        ATC::NoEB, DMICom::modPwrUpModFailure, "Dispatcher version mismatch")),
      waitCycle(0U),
      toggleWaitCycle(0U)
    {
    }

    /******************************************************************************
    * resetMode
    ******************************************************************************/
    void PowerUpMode::resetMode()
    {
      char_t buffer[maxModeStateNameLength];
      getModeStateString(modeState, &buffer[0]);

      AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
      AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
    }

    /******************************************************************************
    * handleMode
    ******************************************************************************/
    void PowerUpMode::handleMode(CommonDataForModes &commonData)
    {
      PowerUpModeState oldModeState = modeState;
      handleCurrentDrivDirection(commonData);
      //Check for startup procedure
      if (modeState >= powerUpActivation)
      {
        //raise standstill event to restrict movement once startup is done 
        ATC::AbstractEventHandler::corePtr()->reportEvent(noMovementInPOU,
          __FILE__, __LINE__);
      }

      // Run the function corresponding to the modeState.
      switch (modeState)
      {
      case powerUpStart:
        runPowerUpStart(commonData);
        break;

      case powerUpTest:
        runPowerUpTest(commonData);
        break;

      case powerUpActivation:
        runPowerUpActivation(commonData);
        break;

      case powerUpWaitConfigOrSleep:
        runPowerUpWaitConfigOrSleep();
        break;

      case powerUpWaitSleepConfirmDMI:
        runPowerUpWaitSleepConfirmDMI();
        break;

      case powerUpFinishSleep:
        runPowerUpFinishSleep();
        break;

      case powerUpFinishConfig:
        runPowerUpFinishConfig();
        break;

      default:
        break;
      }

      //If Mode state has changed
      if (oldModeState != modeState)
      {
        char_t buffer[maxModeStateNameLength];
        getModeStateString(modeState, &buffer[0]);

        AbstractModeControl::corePtr()->getTrace()->write(2U, "Current Mode State :");
        AbstractModeControl::corePtr()->getTrace()->write(2U, &buffer[0]);
      }
    }

    /******************************************************************************
    * getModeId
    ******************************************************************************/
    ATPMode PowerUpMode::getModeId()
    {
      return ATPModePowerUp;
    }

    /******************************************************************************
    * runPowerUpStart
    ******************************************************************************/
    void PowerUpMode::runPowerUpStart(CommonDataForModes &commonData)
    {
       //raise log event on Power up
       ATC::AbstractEventHandler::corePtr()->reportEvent(powerUpSequenceStarted, __FILE__, __LINE__);
      //Clearing MA Timeout and Train Idling
      manageMATimeout(commonData);
      manageTrainIdling(commonData);
      //set the ATP reset bit to true;
      commonData.isATPReset = true;

      if (!ATP::Kernel::AbstractATPApplication::corePtr()->validateDispatcherVersion())
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(dispatcherVersionMismatch, __FILE__, __LINE__);
      }

      modeState = powerUpTest;
    }

    /******************************************************************************
    * runPowerUpTest
    ******************************************************************************/
    void PowerUpMode::runPowerUpTest(CommonDataForModes &commonData)
    {
      //Toggle only after 2s
      if (0U == (toggleWaitCycle % (AbstractModeControl::corePtr()->getToggleFrequencyAOSStatus())))
      {
        //Toggle the AOS Status 
        commonData.atpLampStatus = !commonData.atpLampStatus;
      }
      //increment the toggle cycle
      ++toggleWaitCycle;
      //Start the health supervision and start up test of core Blocks
      //Should not exceed more than supTestWaitCycle
      if ((supTestWaitCycle) >= waitCycle)
      {
        if (startUpTestAndHealthSup())
        {
          ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "Health supervision and Core integrity test Passed!", "MC",
            __FILE__, __LINE__);
          modeState = powerUpActivation;
          waitCycle = 0U;
          //Set the Buzzer(short beep) and AOS Status (on)
          commonData.atpLampStatus = true;
          commonData.buzzer = (commonData.buzzer > BuzzerTypeOneBeep) ? commonData.buzzer : BuzzerTypeOneBeep;
        }
        else
        {
          ++waitCycle;
        }
      }
      else
      {
        //AOS status set to state off after 5 minutes
        commonData.atpLampStatus = false;
        // raise a safety halt event in power up mode
        ATC::AbstractEventHandler::corePtr()->reportEvent(safetyHaltInPOU,__FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * runPowerUpActivation
    ******************************************************************************/
    void PowerUpMode::runPowerUpActivation(CommonDataForModes &commonData)
    {
      bool lcsReady = false;
      //Vehicle ready check
      static_cast<void>(IO::AbstractLocoIO::corePtr()->getCoreDigitalInputValue(IO::AbstractLocoIO::LCSRdy, &lcsReady));

      if (lcsReady)
      {
        //Set the AOS status output
        commonData.atpLampStatus = true;

        // Start the health supervision of vehicle interface
        const bool vehicleComStartUpFlag = TG::VehicleCom::corePtr()->startupAndHealthSupTest();

        // Continue if the VehicleCom Health test is OK (or if time exceeds the supTestVehicleComWaitCycle)
        if (vehicleComStartUpFlag || (waitCycle > supTestVehicleComWaitCycle))
        {
          // If any cab is active and lcs is ready
          if ((AbstractModeControl::corePtr()->getActiveCab() != NoCabActive))
          {
            modeState = powerUpWaitConfigOrSleep;
          }

          // Vehicle Communication not operational
          if (false == vehicleComStartUpFlag)
          { 
            ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "Vehicle Communication not operational", "MC", __FILE__, __LINE__);
          }

          waitCycle = 0U;
        }
        else
        {
          ++waitCycle;
        }
      }
    }

    /******************************************************************************
    * runPowerUpWaitConfigOrSleep
    ******************************************************************************/
    void PowerUpMode::runPowerUpWaitConfigOrSleep()
    {
      const bool isConfigButtonPressed = (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonTrainConfig);
      const bool isDriverLoggedIn = (DriverLoginSeq::driverLoggedIn == AbstractModeControl::corePtr()->getDriverLoginSeqState());

      InitiateConfigReason initiateConfig;

      const bool isInitiateConfigReceived = Kernel::AbstractMessageHandler::corePtr()->getInitiateConfig(initiateConfig);
      if (AbstractModeControl::corePtr()->isAllowedToEnterSleepMode())
      {
        modeState = powerUpWaitSleepConfirmDMI;

      } else if (isConfigButtonPressed ||
        (isInitiateConfigReceived &&
        ((initiateConfig == ConfigKnownByTCC) && isDriverLoggedIn)))
      {
        modeState = powerUpFinishConfig;
      }
      else
      {
        // Do Nothing
      }
    }

    /******************************************************************************
    * runPowerUpWaitSleepConfirmDMI
    ******************************************************************************/
    void PowerUpMode::runPowerUpWaitSleepConfirmDMI()
    {
      if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonConfirmSleep)
      {
        // Sleep mode is confirmed from DMI -> Proceed to next state
        modeState = powerUpFinishSleep;
      }
      else if (!IO::AbstractLocoIO::corePtr()->getSleepingSignal())
      {
        // Continue in Power-Up mode if sleeping signal is deactivated before driver confirmed mode change to Sleeping
        modeState = powerUpWaitConfigOrSleep;
      }
      else
      {
        // Do Nothing
      }
    }

    /******************************************************************************
    * runPowerUpFinishSleep
    ******************************************************************************/
    void PowerUpMode::runPowerUpFinishSleep()
    {
      setNextMode(ATPModeSleeping);
    }

    /******************************************************************************
    * runPowerUpFinishConfig
    ******************************************************************************/
    void PowerUpMode::runPowerUpFinishConfig()
    {
      setNextMode(ATPModeConfiguration);
    }

    /******************************************************************************
    * getModeState
    ******************************************************************************/
    PowerUpModeState PowerUpMode::getModeState() const
    {
      return modeState;
    }

    /******************************************************************************
    * getCurrentModeStateString
    ******************************************************************************/
    bool PowerUpMode::getCurrentModeStateString(char_t* const str)
    {
      getModeStateString(modeState, str);
      return true;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void PowerUpMode::initCrossCompare() const
    {
      AbstractMode::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&modeState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&noMovementInPOU));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&safetyHaltInPOU));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&powerUpSequenceStarted));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&waitCycle));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&toggleWaitCycle));
    }

    /******************************************************************************
    * getModeStateString
    ******************************************************************************/
    void PowerUpMode::getModeStateString(const PowerUpModeState state, char_t* const buffer) const
    {
      switch (state)
      {
      case powerUpStart:
        static_cast<void>(vfw_strlcpy(buffer, "powerUpStart", maxModeStateNameLength));
        break;

      case powerUpTest:
        static_cast<void>(vfw_strlcpy(buffer, "powerUpTest", maxModeStateNameLength));
        break;

      case powerUpActivation:
        static_cast<void>(vfw_strlcpy(buffer, "powerUpActivation", maxModeStateNameLength));
        break;

      case powerUpWaitConfigOrSleep:
        static_cast<void>(vfw_strlcpy(buffer, "powerUpWaitConfigOrSleep", maxModeStateNameLength));
        break;

      case powerUpWaitSleepConfirmDMI:
        static_cast<void>(vfw_strlcpy(buffer, "powerUpWaitSleepConfirmDMI", maxModeStateNameLength));
        break;

      case powerUpFinishSleep:
        static_cast<void>(vfw_strlcpy(buffer, "powerUpFinishSleep", maxModeStateNameLength));
        break;

      case powerUpFinishConfig:
        static_cast<void>(vfw_strlcpy(buffer, "powerUpFinishConfig", maxModeStateNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "invalidModeState", maxModeStateNameLength));
        break;
      }
    }

    /******************************************************************************
    * startUpTestAndHealthSup
    *verify the configuration and accessibility of the required core blocks
    * If successful start the health supervision of these core blocks.
    * ATP -  No need to check as this will function will get call only after INIT of ATP.
    * COD -  Check for INIT flag
    * BTM  - Check for BTM Service OK, if it is OK the OPC (gateway) is also OK.
    * DMI  - Check for connection status
    ******************************************************************************/
    bool PowerUpMode::startUpTestAndHealthSup() const
    {
      // Verify the configuration and accessibility of the required core blocks
      // May get changed upon the confirmation from requirement team
      // COD
      const bool odoHealthStatus = Pos::AbstractOdometry::corePtr()->startupAndHealthSupTest();

      // DMI
      const bool dmiHealthStatus = DMICom::AbstractDMIHandler::corePtr()->startupAndHealthSupTest();

      // OPC & BTM
      const bool btmHealthStatus = ATP::IO::AbstractBTMHandler::corePtr()->isBTMServiceAvailable();

      // Combine the required blocks to the result flag
      const bool retFlag = odoHealthStatus && dmiHealthStatus && btmHealthStatus;

      if (retFlag)
      {
        DMICom::AbstractDMIHandler::corePtr()->addStartupMsg("ATP test OK!\r\n");
        DMICom::AbstractDMIHandler::corePtr()->addStartupMsg("COD test OK!\r\n");
        DMICom::AbstractDMIHandler::corePtr()->addStartupMsg("DMI test OK!\r\n");
        DMICom::AbstractDMIHandler::corePtr()->addStartupMsg("BTM service OK!\r\n");
        DMICom::AbstractDMIHandler::corePtr()->addStartupMsg("OPC service OK!\r\n");
      }
      return retFlag;
    }


    /******************************************************************************
    * manageIdling
    ******************************************************************************/
    void PowerUpMode::manageTrainIdling(CommonDataForModes &commonData)
    {
      commonData.idling = false;
    }

    /******************************************************************************
    * manageMATimeoutState
    ******************************************************************************/
    void PowerUpMode::manageMATimeout(CommonDataForModes &commonData)
    {
      commonData.maTimeOut = false;
    }
  }
}
