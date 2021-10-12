/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the EmergencyAlertSeq class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-01    arastogi    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "emergency_alert_seq.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tracks.hpp"
#include "abstract_targets.hpp"
#include "abstract_odometry.hpp"
#include "abstract_supervise.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_tims.hpp"
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
    EmergencyAlertSeq::EmergencyAlertSeq() :
      emergencyAlertActiveEvent(ATC::Event::createSBReqEvent(atpModeControlId, ATC::CoreContainer, eventIdEaFromTCC, ATC::DispatcherSB,
        DMICom::eaActiveFromTCC, "Emergency alert Active"))
    {
      seqState = emergencyAlertInactive;
      trace = AbstractModeControl::corePtr()->getTrace();
      trace->write(2U, "Emergency Alert State :", static_cast<uint32_t>(seqState));
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void EmergencyAlertSeq::run(CommonDataForModes &commonData)
    {
      EmergencyAlertState oldState = seqState;

      //run the function corresponding to the sequence state.
      switch (seqState)
      {
      case emergencyAlertInactive:
        runEmergencyAlertInactive();
        break;

      case emergencyAlertActive:
        runEmergencyAlertSeq(commonData);
        break;

      default:
        break;
      }

      //If sequence state has changed
      if (oldState != seqState)
      {
        trace->write(2U, "Emergency Alert State :", static_cast<uint32_t>(seqState));
      }

    }

    /******************************************************************************
    * getState
    ******************************************************************************/
    EmergencyAlertState EmergencyAlertSeq::getState() const
    {
      return seqState;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void EmergencyAlertSeq::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&seqState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&emergencyAlertActiveEvent));
    }

    /******************************************************************************
    * runEmergencyAlertInactive
    ******************************************************************************/
    void EmergencyAlertSeq::runEmergencyAlertInactive()
    {
      EmAlertReasonInfo eaReason;
      const bool isEmAlertByUncondShortMsg = AbstractMessageHandler::corePtr()->isEmAlertStatusSetByUncondMsg();
      const bool isEmAlertByEmergencyMsg = AbstractMessageHandler::corePtr()->getEmAlertReason(eaReason);
      const bool isEmAlertBySupervise = Supv::AbstractSupervise::corePtr()->isEmAlertRequested();

      if (isEmAlertByEmergencyMsg || isEmAlertByUncondShortMsg || isEmAlertBySupervise)
      {
        seqState = emergencyAlertActive;

        ATC::AbstractEventHandler::corePtr()->reportEvent(emergencyAlertActiveEvent, __FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * runEmergencyAlertSeq
    ******************************************************************************/
    void EmergencyAlertSeq::runEmergencyAlertSeq(CommonDataForModes &commonData)
    {
      commonData.handlingDone = false;

      ATC::AbstractEventHandler::corePtr()->reportEvent(emergencyAlertActiveEvent, __FILE__, __LINE__);

      if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
      {
        //To avoid clearing the value of current ceiling speed, gradient, and direction in modes where 
        //targets are not available (e.g - Yard)
        if (!ATP::DS::AbstractTargets::corePtr()->isMATargetListEmpty())
        {
          // Take actions and clear any passed targets
          DS::AbstractTargets::corePtr()->removePassedTargets();
          // Clear any other existing target data
          DS::AbstractTargets::corePtr()->removeAll();

          //remove tracks not under the train
          if (TG::AbstractTIMS::corePtr()->isRearPositionValid())
          {
            if (DS::AbstractTracks::corePtr()->removeNotCovered(
              TG::AbstractTIMS::corePtr()->getSafePositionToDeleteTrack(),
              Pos::AbstractPosition::corePtr()->getSafeLeadingPosOdo()))
            {
              // No tracks or balises to remove.
            }
          }
        }
      }

      //Clear the Emergency Alert status in Power-up, Sleeping, Safety halt, Powering down mode
      const ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
      //Clear the Emergency Alert status when revoke message is received.
      const bool isRevokeEA = AbstractMessageHandler::corePtr()->getRevokeEmAlert();

      // Revoke EA in following modes OR when Revoke messages from TCC is received.
      if (((currentMode == ATPModePowerUp) || (currentMode == ATPModeSleeping)
        || (currentMode == ATPModeSafetyHalt) || (currentMode == ATPModePoweringDown)
        || (currentMode == ATPModeYard) || (currentMode == ATPModePossession)
        || (currentMode == ATPModeShunting) || (currentMode == ATPModeUnregistered))
        || (isRevokeEA))
      {
        seqState = emergencyAlertInactive;
      }

    }

    /******************************************************************************
    * Destructor
    ******************************************************************************/
    EmergencyAlertSeq:: ~EmergencyAlertSeq(void)
    {
      trace = static_cast<ATC::TraceInterface*>(NULL);
    }
  }
}
