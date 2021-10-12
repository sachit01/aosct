/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the DriverLoginSeq class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-01    arastogi    Created
* 2017-04-11    skothiya    updated for cabin handling and authorization requirement implementation
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "driver_login_seq.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_radio_handler.hpp"
#include "sleeping_mode.hpp"
#include "abstract_odometry.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_mode_control_event_ids.hpp"

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
    DriverLoginSeq::DriverLoginSeq() :
      driverNotAuthStandstill(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdDriverNotAuth,
        ATC::DriverSB, 0x0U, "Applying standstill as driver not authorized")),
      driverLogoutEvent(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer, eventIdDriverLogout,
        0U, "Driver Logout from DMI")),
      driverLoginNotAuthorizedStandstill(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdDriverLoginNotAuthorized,
        ATC::NoSB, 0x0U, "Standstill on Driver Login not verified"))
    {
      seqState = driverLoggedOut;
      loginFailedCycleCount = 0U;
      isLogInFromYardWithoutTCC = false;
      AbstractModeControl::corePtr()->getTrace()->write
      (2U, "Current Driver Login State :", static_cast<uint32_t>(seqState));

    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void DriverLoginSeq::run()
    {
      DriverLoginState oldState = seqState;

      //run the function corresponding to the sequence state.
      switch (seqState)
      {
      case driverLoggedOut:
        runDriverLoggedOut();
        break;

      case driverLoginVerification:
        runDriverLoginVerification();
        break;

      case driverLoggedIn:
        runDriverLoggedIn();
        break;

      case driverLoginFailed:
        runDriverLoginFailed();
        break;

      case atoAuthorized:
        runAtoAuthorized();
        break;

      default:
        break;
      }

      //If sequence state has changed
      if (oldState != seqState)
      {
        AbstractModeControl::corePtr()->getTrace()->write
        (2U, "Current Driver Login State :", static_cast<uint32_t>(seqState));
      }

      //Apply brake if driver is not Authorized and current mode is not sleeping
      const ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
      if ((seqState != driverLoggedIn) && (currentMode != ATPModeSleeping))
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(driverNotAuthStandstill, __FILE__, __LINE__);
      }
      else
      {
        //do nothing
      }
    }

    /******************************************************************************
    * getState
    ******************************************************************************/
    DriverLoginState DriverLoginSeq::getState() const
    {
      return seqState;
    }

    /******************************************************************************
    * runDriverLoggedOut
    ******************************************************************************/
    void DriverLoginSeq::runDriverLoggedOut()
    {
      const ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
      const bool isTCCTimedOut = RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus();

      DMICom::DriverIdAndPassword driverIdAndPassword;

      if (isAllowedToLogin())
      {
        // Send login information to TCC
        if (DMICom::AbstractDMIHandler::corePtr()->getDriverIdAndPassword(driverIdAndPassword))
        {
          seqState = driverLoginVerification;
        }
      }
      else if (((currentMode == ATPModeYard) || (currentMode == ATPModeSleeping) || (currentMode == ATPModePowerUp)) && (isTCCTimedOut))
      {
        //set driver status to authorized (logged-in) in yard mode as TCC not connected
        if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonConfirmYard)
        {
          isLogInFromYardWithoutTCC = true;
          seqState = driverLoggedIn;
        }
      }
      else
      {
        // Do nothing
      }
    }

    /******************************************************************************
    * runDriverLoggedIn
    ******************************************************************************/
    void DriverLoginSeq::runDriverLoggedIn()
    {
      // If cabin got deactivated then making driver login state to logout 
      const ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
      const ATPMode previousMode = AbstractModeControl::corePtr()->getPreviousMode();
      const ATOMode previousATOMode = AbstractModeControl::corePtr()->getPreviousATOMode();
      const ATOMode currentATOMode = AbstractModeControl::corePtr()->getATOMode();

      const Kernel::SleepingModeState sleepingModeState = Kernel::AbstractModeControl::corePtr()->getSleepingModeState();

      if (AbstractModeControl::corePtr()->getActiveCab() != AbstractModeControl::corePtr()->getPrevActiveCab())
      {
        seqState = driverLoggedOut;
      }
      else if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonLogoutDriver)
      {
        //If Logout button pressed on DMI need to logout driver from AOS
        //Reporting log related to driver logout
        ATC::AbstractEventHandler::corePtr()->reportEvent(driverLogoutEvent, __FILE__, __LINE__);
        //Setting Driver status to logout.
        seqState = driverLoggedOut;
      }
      else if (((currentMode == ATPModeConfiguration) || (currentMode == ATPModePossession) || (currentMode == ATPModeShunting))
          && (previousMode == ATPModeYard) && (isLogInFromYardWithoutTCC))
      {
        //set driver status to unauthorized (logout) if mode changed to configuration from yard mode
        seqState = driverLoggedOut;
        isLogInFromYardWithoutTCC = false;
      }

      // Driver is logged out when entering sleep-mode
      else if ((ATPModeSleeping == currentMode) && (SleepingMode::sleepingStart == sleepingModeState))
      {
        seqState = driverLoggedOut;
      }

      /*******
      Requirements for the following piece of code is postponed until next released requirements drop.
      ********/
      // Switch to ATO authorized for the transition from ATO Manual/supervised to ATO Automatic 
      else if ((((previousATOMode == ATOModeManual) || (previousATOMode == ATOModeSupervisedAutomatic))) && (currentATOMode == ATOModeAutomatic))
      {
        seqState = atoAuthorized;
      }
      else
      {
        // Do nothing
      }
    }

    /******************************************************************************
    * runDriverLoginVerification
    ******************************************************************************/
    void DriverLoginSeq::runDriverLoginVerification()
    {
      LogonStatus lStatus;
      //Get Driver login status message from message handler
      if (AbstractMessageHandler::corePtr()->getDriverLogonStatus(lStatus))
      {
        //Login verified by TCC
        if (lStatus == DriverLogonSuccesful)
        {
          seqState = driverLoggedIn;
          //set the flag to false that login through Yard after loggedIn
          isLogInFromYardWithoutTCC = false;
        }
        else
        {
          seqState = driverLoginFailed;
          loginFailedCycleCount = 0U;
        }
      }
      else
      {
        // Region 1 TCC Connected
        const bool isRegion1TCCConnected = RadioCom::AbstractRadioHandler::corePtr()->getConnected(ATP::RadioCom::radioChannelId2);
        // Region 2 TCC Connected
        const bool isRegion2TCCConnected = RadioCom::AbstractRadioHandler::corePtr()->getConnected(ATP::RadioCom::radioChannelId3);
        // Any region TCC connected?
        const bool isTCCConnected = (isRegion1TCCConnected) || (isRegion2TCCConnected);
        if (!isTCCConnected)
        {
          // Cancel login if TCC communication is lost
          seqState = driverLoggedOut;
        }
      }
    }

    /******************************************************************************
    * runDriverLoginFailed
    ******************************************************************************/
    void DriverLoginSeq::runDriverLoginFailed()
    {
      //Wait to display login failed status before going to logged out.
      if (loginFailedCycleCount >= maxLoginFailedCycles)
      {
        seqState = driverLoggedOut;
      }
      else
      {
        loginFailedCycleCount++;
      }
    }

    /******************************************************************************
    * runAtoAuthorized
    ******************************************************************************/
    void DriverLoginSeq::runAtoAuthorized()
    {
      /*******
      Requirements for the following piece of code is postponed until next released requirements drop.
      ********/
      const ATOMode previousATOMode = AbstractModeControl::corePtr()->getPreviousATOMode();
      const ATOMode currentATOMode = AbstractModeControl::corePtr()->getATOMode();
      {
        // Switch to Driver logged out for the transition from ATO authorized to ATO Manual/Supervised
        if ((previousATOMode == ATOModeAutomatic) && ((currentATOMode == ATOModeManual) || (currentATOMode == ATOModeSupervisedAutomatic)))
        {
          seqState = driverLoggedOut;
        }
      }
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void DriverLoginSeq::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&seqState));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&loginFailedCycleCount));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&isLogInFromYardWithoutTCC));

      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&driverNotAuthStandstill));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&driverLogoutEvent));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&driverLoginNotAuthorizedStandstill));
    }

    /******************************************************************************
    * isAllowedToLogin
    ******************************************************************************/
    bool DriverLoginSeq::isAllowedToLogin() const
    {
      bool retFlag = false;

      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      const bool isCabActive = (AbstractModeControl::corePtr()->getActiveCab() != NoCabActive);
      const bool isConfigDataRecv = AbstractMessageHandler::corePtr()->isConfigDataReceived();
      const ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
      const bool isSeqStateOK = ((seqState == driverLoggedOut) || (seqState == driverLoginVerification));

      // Region 1 TCC Connected
      const bool isRegion1TCCConnected = RadioCom::AbstractRadioHandler::corePtr()->getConnected(ATP::RadioCom::radioChannelId2);

      // Region 2 TCC Connected
      const bool isRegion2TCCConnected = RadioCom::AbstractRadioHandler::corePtr()->getConnected(ATP::RadioCom::radioChannelId3);

      // Any region TCC connected
      const bool isTCCConnected = (isRegion1TCCConnected) || (isRegion2TCCConnected);

      if (isTCCConnected && (isSeqStateOK) && (isStandStill) && (isCabActive) && isConfigDataRecv &&
        ((currentMode != ATPModePoweringDown) && (currentMode != ATPModeSafetyHalt)))
      {
        retFlag = true;
      }

      return retFlag;
    }

  }
}
