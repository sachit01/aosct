/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the ModeRequestSeq class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-12-20   spandita    Updated for mode requested button seq implementation
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "mode_request_seq.hpp"
#include "abstract_odometry.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_radio_handler.hpp"
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
    ModeRequestSeq::ModeRequestSeq() :
      modeAckRejectNotify(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer, eventIdNotifyRejectedAck,
        DMICom::modNegativeAck, "Mode Change Request Rejected By TCC")),
      waitForModAckStandStill(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdWaitForTCCResp,
        ATC::DriverSB, 0x0U, "Waiting for TCC response of a mode transition")),
      waitForModAckDriver(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdWaitForDriverAck,
        ATC::DriverSB, 0x0U, "Waiting for driver to ack mode transition")),
      modAckReceived(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer, eventIdModAckRcv,
        0x0U, "Mode Transition accepted by TCC")),
      modTCCNotAvail(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer, eventIdModTCCTimeout,
        0x0U, "TCC Timeout!")),
      modAckTimeout(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer, eventIdModAckTimeout,
        0x0U, "Timeout waiting on Ack for Mode Change Request"))
    {
      trace = AbstractModeControl::corePtr()->getTrace();
      seqState = 0U;
      lastNumPositionMessages = 0U;
    }

    /******************************************************************************
    * getModeReqSeqState
    ******************************************************************************/
    ModeRequestSeqState ModeRequestSeq::getModeReqSeqState() const
    {
      return seqState;
    }

    /******************************************************************************
    * isDMIButtonNeeded
    ******************************************************************************/
    bool ModeRequestSeq::isDMIButtonNeeded()
    {
      //standstill condition
      bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      //Driver Login State
      DriverLoginState atpDriverLoginState = AbstractModeControl::corePtr()->getDriverLoginSeqState();
      //Safe brake to stop event
      bool isSafeBrkToStopEvntActive = ATC::AbstractEventHandler::corePtr()->isModeChangeToSafeBrakeToStop();
      //validate modes
      bool isModeValid = validateModes();
      bool isDriverLoggedIn = false;
      bool isSleepingSignalInactive = true;

      // Get ATO-Mode
      const bool atoModeManual = (ATOModeManual == AbstractModeControl::corePtr()->getATOMode());

      //Sleeping signal is inactive in ATP mode Sleeping, Join, Yard, Shunting or Power Up
      ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();

      if ((currentMode == ATPModeSleeping) || (currentMode == ATPModeJoin) || (currentMode == ATPModeYard)
         || (currentMode == ATPModeShunting) || (currentMode == ATPModePowerUp))
      {
         if (IO::AbstractLocoIO::corePtr()->getSleepingSignal())
         {
            isSleepingSignalInactive = false;
         }
      }
      
      // Driver logged in?
      if (atpDriverLoginState == DriverLoginSeq::DriverLoginSeq::driverLoggedIn)
      {
        isDriverLoggedIn = true;
      }
      bool isTCCTimeout = RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus();

      bool isValidData = (isModeValid && (!isSafeBrkToStopEvntActive) && (!isTCCTimeout) && isDriverLoggedIn &&
        isStandStill && atoModeManual && isSleepingSignalInactive);

      return isValidData;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void ModeRequestSeq::initCrossCompare()
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&seqState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&modeAckRejectNotify));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&waitForModAckStandStill));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&modAckReceived));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&modTCCNotAvail));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&modAckTimeout));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&lastNumPositionMessages));
    }

    /******************************************************************************
    * validateModes
    ******************************************************************************/
    bool ModeRequestSeq::validateModes()
    {
      return false;
    }

    /******************************************************************************
    * isAckTimeout
    ******************************************************************************/
    bool ModeRequestSeq::isAckTimeout()
    {
      bool timeoutOccured = false;

      // Check if it has taken to many polls to receive the Ack message for mode change.
      uint32_t curPosMessages = RadioCom::AbstractRadioHandler::corePtr()->getNumPositionMessages();

      if ((curPosMessages - lastNumPositionMessages) > maxNrPollsWaitForAck)
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(modAckTimeout, __FILE__, __LINE__);
        timeoutOccured = true;
      }

      return timeoutOccured;
    }

    /******************************************************************************
    * Destructor
    ******************************************************************************/
    ModeRequestSeq::~ModeRequestSeq()
    {
      trace = static_cast<ATC::TraceInterface*>(NULL);
    }

  }
}
