/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->TCC) has an associated creator class inherited from AbstractRadioMessageOut.
* This file implements the creator for the PositionReport message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-26    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "base_target.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_odometry.hpp"
#include "abstract_targets.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_supervise.hpp"
#include "abstract_loco_io.hpp"
#include "abstract_brake.hpp"
#include "radio_message_out_position_report.hpp"
#include "abstract_tims.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_radio_handler.hpp"
#include "abstract_tracks.hpp"
#include "abstract_dmi_handler.hpp"
#include "vehicle_com.hpp"

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
    * RadioMessageOutPositionReport constructor
    ******************************************************************************/
    RadioMessageOutPositionReport::RadioMessageOutPositionReport(const uint16_t chId) : AbstractRadioMessageOut(MTypePositionReportRegion1),
      // Start with a simulated ack received from radio-handler.
      isSlipClearStatusReportedToTCC(true), ackDefaultPositionReportReceived(true), dedicatedChannelId(chId), msgAckToSend(false)
    {
      positionReport.trailingTrackAndPosition.track = 0U;
      positionReport.trailingTrackAndPosition.position = 0U;
      positionReport.leadingTrackAndPosition.track = 0U;
      positionReport.leadingTrackAndPosition.position = 0U;
      positionReport.positionClassification = Pos::PosUnknown;
      positionReport.orientationAndDirection = 0U;
      positionReport.trainSpeed = 0U;
      positionReport.trainCoreStatus = 0U;
      positionReport.frontConfidenceInterval = 0U;
      positionReport.rearConfidenceInterval = 0U;
      positionReport.maTargetTrackAndPosition.track = 0U;
      positionReport.maTargetTrackAndPosition.position = 0U;
      positionReport.atpMode = ATPModeUndefined;
      positionReport.atoMode = 0U;
      positionReport.brakeSystem = BrakeSystemTypeUndefined;
      positionReport.msgAck.messageAccepted = false;
      positionReport.msgAck.msgId = 0U;

      implemented = true;

      // Setup fixed size
      positionReport.stopDistDataVec.reserve(stopDistDataSize);
      positionReport.possesionRequestReceived = false;
      positionReport.shuntingRequestReceived = false;
      positionReport.yardRequestReceived = false;
      positionReport.lastBaliseReceived.idLastReadBalise = 0U;
      positionReport.trainNameReceived = false;
      positionReport.etaConfirmationReceived = false;
      positionReport.cancelAreaReceived = false;
      positionReport.eventDataVec.reserve(eventDataSize);
      positionReport.eventDataTextVec.reserve(eventDataTextSize);
      positionReport.externalDataReceived = false;
    }

    /******************************************************************************
    * RadioMessageOutPositionReport::ackDefaultPositionReport
    ******************************************************************************/
    void RadioMessageOutPositionReport::ackDefaultPositionReport()
    {
      ackDefaultPositionReportReceived = true;
    }

    /******************************************************************************
    * RadioMessageOutPositionReport::getAckDefaultPositionReport
    ******************************************************************************/
    bool RadioMessageOutPositionReport::getAckDefaultPositionReport() const
    {
      return ackDefaultPositionReportReceived;
    }

    /******************************************************************************
    * RadioMessageOutPositionReport::collectPositionData
    ******************************************************************************/
    void RadioMessageOutPositionReport::collectPositionData()
    {
      positionReport.leadingTrackAndPosition = Pos::AbstractPosition::corePtr()->getLeadingPos();
      positionReport.trailingTrackAndPosition = TG::AbstractTIMS::corePtr()->getRearPosition();
      
      const ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
        
      if ((positionReport.trailingTrackAndPosition.track != 0U)
        && (positionReport.leadingTrackAndPosition.track != 0U)
        && (currentMode != ATPModeBaliseSearch))
      {
        positionReport.positionClassification = Pos::AbstractPosition::corePtr()->getAccuracyState();
      }
      else
      {
        positionReport.positionClassification = Pos::PosUnknown;
      }

      if (positionReport.positionClassification == Pos::PosUnknown)
      {
        positionReport.trailingTrackAndPosition.track = 0U;
        positionReport.trailingTrackAndPosition.position = 0U;
        positionReport.leadingTrackAndPosition.track = 0U;
        positionReport.leadingTrackAndPosition.position = 0U;
        positionReport.frontConfidenceInterval = 0U;
        positionReport.rearConfidenceInterval = 0U;
      }
      else
      {
        // Get confidence interval only when not unknown
        positionReport.frontConfidenceInterval = static_cast<uint16_t>(TG::AbstractTIMS::corePtr()->getFrontBaliseWindow());
        positionReport.rearConfidenceInterval = static_cast<uint16_t>(TG::AbstractTIMS::corePtr()->getRearBaliseWindow());
      }
    }

    /******************************************************************************
    * RadioMessageOutPositionReport::collectData
    ******************************************************************************/
    void RadioMessageOutPositionReport::collectData()
    {
      if ((!isSlipClearStatusReportedToTCC) && ackDefaultPositionReportReceived)
      {
        isSlipClearStatusReportedToTCC = true;
      }

      collectPositionData();

      // Get direction and M_BRAKE_SYSTEM from train setup
      const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

      if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
      {
        uint8_t orientationInTrack = 0U;
        const TravelDir dir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
        const uint8_t drivingDirValue = (DirReverse == dir) ? 1U : 0U;

        //if the position is not known, send the odo direction from the new registration procedure or the default value.
        if (positionReport.positionClassification == Pos::PosUnknown)
        {
          OdoDir odoDir = AbstractModeControl::corePtr()->getOdoDirInNewRegistration();

          if (OdoUndefined == odoDir)
          {
            // Unknown orientation, use default value.
            trace->write(ATC::briefTrace, "Unknown orientation in track");
          }
          else
          {
            // Orientation in track is '1' if Odometer direction is set to negative.
            orientationInTrack = (OdoNegative == odoDir) ? 1U : 0U;
          }
        }
        else
        {
          const DS::Track* const pFoundTrack = DS::AbstractTracks::corePtr()->getTrack(positionReport.trailingTrackAndPosition.track);

          if (static_cast<const DS::Track*>(NULL) != pFoundTrack)
          {
            if (OdoUndefined != pFoundTrack->getOdoDirection())
            {
              orientationInTrack = (OdoPositive == pFoundTrack->getOdoDirection()) ? 0U : 1U;
            }
          }
          else
          {
            // Unknown orientation, use default value.
            trace->write(ATC::briefTrace, "Unknown orientation in track, not found track and passed baliseSearchWaitMA.");
          }
        }
        // Bit2 = LocoOrientation, Bit1 = Orientation in Track, Bit0 = DrivingDirection 
        positionReport.orientationAndDirection = (pTrainSetup->orientation | static_cast<uint8_t>(orientationInTrack << 1U) | drivingDirValue);
      }
      else
      {
        trace->write(ATC::veryDetailedTrace, "Unable to fetch train Setup");
      }

      if (AbstractModeControl::corePtr()->getIdleState())
      {
        // Setting train speed to 0 in idle state
        positionReport.trainSpeed = 0U;
      }
      else
      {
        // Get current train speed from Odometry 
        positionReport.trainSpeed = Pos::AbstractOdometry::corePtr()->getSpeed();
      }

      // Get the active brake system type.
      // If the train setup is not valid (eg before receiving TSetup), get the type directly from VehicleCom.
      if (DS::AbstractTSetup::corePtr()->isTrainSetupValid())
      {
        positionReport.brakeSystem = DS::AbstractTSetup::corePtr()->getBrakeSystemInUse();
      }
      else
      {
        positionReport.brakeSystem = TG::VehicleCom::instance().getBrakeSystem();
      }

      // Get train-status: B_TRAIN_CORE_STATUS
      collectTrainStatusInfo(positionReport.trainCoreStatus);

      if (!isSlipClearStatusReportedToTCC)
      {   // Clear bit
        positionReport.trainCoreStatus &= ~trainStatusSlipDetected;
      }

      // Get target track and distance on ctrack, 0 if no target found
      positionReport.maTargetTrackAndPosition.track = 0U;
      positionReport.maTargetTrackAndPosition.position = 0U;

      DS::BaseTarget* pTarget =
          DS::AbstractTargets::corePtr()->getPrimaryTarget(DS::AbstractTargets::corePtr()->getSupposedTravelDir());
      // Find the Primary target which holds the requested parameters
      if (pTarget != static_cast<DS::BaseTarget*>(NULL))
      {
        TrackAndPos maTargetPos = pTarget->getPosition();

        positionReport.maTargetTrackAndPosition.track = maTargetPos.track;
        positionReport.maTargetTrackAndPosition.position = maTargetPos.position;
      }

      // Get ATP Mode
      positionReport.atpMode = AbstractModeControl::corePtr()->getCurrentMode();

      // TODO: ATP-Limited, Support only for ATO-Manual
      positionReport.atoMode = 1U;

      // Dynamic data - Updated even if the default position report is not acknowledged (i e other message is sent to TCC than PR). 
      // Otherwise we might miss data that is only valid for one cycle.

      // Send Possession Request
      bool isAckRecvd = false;
      const LocationModeState locationModeState = AbstractModeControl::corePtr()->getLocationModeState();
      if (AbstractModeControl::corePtr()->timeToSendPossesionReqest())
      {
        if (!positionReport.possesionRequestReceived)
        {
          positionReport.possesionRequestReceived = true;
        }
      }
      // Send Shunting Request
      else if (AbstractModeControl::corePtr()->timeToSendShuntRequest())
      {
        if (!positionReport.shuntingRequestReceived)
        {
          positionReport.shuntingRequestReceived = true;
        }
      }
      // Send Yard Request
      else if (AbstractModeControl::corePtr()->timeToSendYardRequest()
        || (LocationMode::locationYardModWaitAck == locationModeState))
      {
        YardAcknowledge yardAck;
        isAckRecvd = AbstractMessageHandler::corePtr()->getYardAcknowledge(yardAck);
        if ((!positionReport.yardRequestReceived) && (!isAckRecvd))
        {
          positionReport.yardRequestReceived = true;
        }
      }
      else
      {
        //do nothing
      }

      // Get Balise ID if available
      Pos::AbstractDecode::BaliseInfo baliseInfo;
      if (Pos::AbstractPosition::corePtr()->getBaliseInfo(baliseInfo))
      {
        positionReport.lastBaliseReceived.idLastReadBalise = baliseInfo.nidBG;
      }

      // Get Train Name
      TrainName trainNameData;
      memset(&trainNameData.trainName[0], 0, sizeof(trainNameData.trainName));

      if (DMICom::AbstractDMIHandler::corePtr()->getChangedTrainName(&trainNameData.trainName[0]))
      {
        if ((positionReport.atpMode == ATPModeNormal) || (positionReport.atpMode == ATPModeStaffResponsible)
          || (positionReport.atpMode == ATPModeLocation))
        {
          positionReport.trainNameReceived = true;
          positionReport.trainName = trainNameData;
        }

      }
      else
      {
        trace->write(ATC::veryDetailedTrace, "Unable to fetch Changed Train Name");
      }

      // TODO: ETA Confirmation, not implemented yet

      // Collect the CANCEL AREA Data here
      if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonCancelRegistrationArea)
      {
        positionReport.cancelAreaReceived = true;
        trace->write(ATC::briefTrace, "Cancel Registration Area by Driver");
        writeToLog(ATC::BriefLog, "Cancel Registration Area by Driver", __FILE__, __LINE__);

        // The Cancel Area Action is noticed and the current selected area can be reset 
        // (to enable another one to be chosen at next AreaRequest from TCC)
        Kernel::AbstractMessageHandler::corePtr()->clearAreaSelectedByDriver();
      }

      // Collect the Message Ack Data
      collectMsgAckData();

      // Collect the Stopping Distance Data
      collectStopDistData();

      // Get Event Data
      const ATC::Event *event = static_cast<ATC::Event*>(NULL);

      if (dedicatedChannelId == RadioCom::radioChannelId2)
      {
        event = ATC::AbstractEventHandler::corePtr()->getNextTcc2Event();
      }
      else
      {
        event = ATC::AbstractEventHandler::corePtr()->getNextTcc3Event();
      }

      bool bEnter = true;

      while ( (event != NULL) && bEnter)
      {
        //get Dynamic text to decide whether it is to be send as event id or event text
        const char_t *dynamicText = event->getDynamicText();
        if (dynamicText == NULL)
        {
          //Event Data
          EventData eventData;
          eventData.idEventData = event->getId();

          if (positionReport.eventDataVec.size() < eventDataSize)
          {
            positionReport.eventDataVec.push_back(eventData);

            if (dedicatedChannelId == RadioCom::radioChannelId2)
            {
              ATC::AbstractEventHandler::corePtr()->deleteTcc2Event();
            }
            else
            {
              ATC::AbstractEventHandler::corePtr()->deleteTcc3Event();
            }
          }
          else
          {
            trace->write(ATC::veryDetailedTrace, "Too Many Events Reported!!");
            bEnter = false;
          }
        }
        else
        {
          //Event Data Text
          EventDataText eventDataText;
          eventDataText.idEventDataText = event->getId();

          //Reset the value of event data text to zero
          memset(&eventDataText.text[0], 0, sizeof(eventDataText.text));
          strncpy(&eventDataText.text[0], dynamicText, sizeof(eventDataText.text) - 1U);

          if (positionReport.eventDataTextVec.size() < eventDataSize)
          {
            positionReport.eventDataTextVec.push_back(eventDataText);
            if (dedicatedChannelId == RadioCom::radioChannelId2)
            {
              ATC::AbstractEventHandler::corePtr()->deleteTcc2Event();
            }
            else
            {
              ATC::AbstractEventHandler::corePtr()->deleteTcc3Event();
            }
          }
          else
          {
            trace->write(ATC::veryDetailedTrace, "Too Many Events Reported!!");
            bEnter = false;
          }
        }
        if (dedicatedChannelId == RadioCom::radioChannelId2)
        {
          event = ATC::AbstractEventHandler::corePtr()->getNextTcc2Event();
        }
        else
        {
          event = ATC::AbstractEventHandler::corePtr()->getNextTcc3Event();
        }

      }

      // TODO: External Data, not implemented yet

      // Data shall always be available for PositionReport
      dataProcessState = DataAvailable;
    }

    /******************************************************************************
    * RadioMessageOutPositionReport::validate
    ******************************************************************************/
    bool RadioMessageOutPositionReport::validate()
    {
      // assemble, validate and publish data
      if (DataAvailable == dataProcessState)
      {
        if (assembleMessageData())
        {
          dataProcessState = DataValidated;
          // Reset acknowledgment-flag (to be set again by radio handler when variable data is sent to TCC)
          ackDefaultPositionReportReceived = false;

          if ((Pos::AbstractOdometry::corePtr()->isSlipStatusClear()) && (isSlipClearStatusReportedToTCC))
          {
            isSlipClearStatusReportedToTCC = false;
          }

        }
      }

      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageOutPositionReport::collectMsgAckData
    ******************************************************************************/
    void RadioMessageOutPositionReport::collectMsgAckData()
    {
      uint8_t msgIdToAck = 0U;
      uint16_t channel;

      // MA Received?
      if (AbstractMessageHandler::corePtr()->getMAReceived(msgIdToAck, channel))
      {
        // Shall MA be acknowledged in this instance of position-report?
        if (dedicatedChannelId == channel)
        {
          MAHead maHead;

          // Save status and ID. Either a complete and published MA or a validated partly MA
          bool isMaPublished = AbstractMessageHandler::corePtr()->getMAHead(maHead);
          bool isMaPartlyReceivedAndValidated = AbstractMessageHandler::corePtr()->getPartlyMaReceived();

          positionReport.msgAck.messageAccepted = isMaPublished || isMaPartlyReceivedAndValidated;
          positionReport.msgAck.msgId = msgIdToAck;
          msgAckToSend = true;
        }
      }
      // Train Setup Received?
      else if (AbstractMessageHandler::corePtr()->getTrainSetupReceived(msgIdToAck, channel))
      {
        // Shall TSetup be acknowledged in this instance of position-report?
        if (dedicatedChannelId == channel)
        {
          TrainSetupReason reason;
          // Save status and ID
          positionReport.msgAck.messageAccepted = AbstractMessageHandler::corePtr()->getQSetup(reason);
          positionReport.msgAck.msgId = msgIdToAck;
          msgAckToSend = true;
        }
      }
      // Approximate Position Received?
      else if (AbstractMessageHandler::corePtr()->getApproxPosReceived(msgIdToAck, channel))
      {
        // Shall Approx Pos be acknowledged in this instance of position-report?
        if (dedicatedChannelId == channel)
        {
          // Save status and ID
          positionReport.msgAck.messageAccepted = AbstractMessageHandler::corePtr()->getApproximatePosition();
          positionReport.msgAck.msgId = msgIdToAck;
          msgAckToSend = true;
        }
      }
      // Configuration Data Received?
      else if (AbstractMessageHandler::corePtr()->getConfigDataReceived(msgIdToAck, channel))
      {
        // Shall ConfigData be acknowledged in this instance of position-report?
        if (dedicatedChannelId == channel)
        {
          positionReport.msgAck.messageAccepted = AbstractMessageHandler::corePtr()->getConfigDataReceived();
          positionReport.msgAck.msgId = msgIdToAck;
          msgAckToSend = true;
        }
      }
      else
      {
        // Do nothing
      }
    }

    /******************************************************************************
    * RadioMessageOutPositionReport::collectStopDistData
    ******************************************************************************/
    void RadioMessageOutPositionReport::collectStopDistData()
    {
      uint8_t msgIdToAck = 0U;
      uint16_t channel;
      if (AbstractMessageHandler::corePtr()->getTrainSetupReceived(msgIdToAck, channel))
      {
        // Shall TSetup be acknowledged in this instance of position-report?
        if (dedicatedChannelId == channel)
        {
          int8_t maxGradient = 0;
          if (AbstractMessageHandler::corePtr()->getTrainSetupMaxGradient(maxGradient))
          {
            int8_t startGradient = static_cast<int8_t>(maxGradient*-1);
            int8_t endGradient = maxGradient;
            if (maxGradient <= 0)
            {
              endGradient = 0;
              startGradient = maxGradient;
            }

            for (int8_t i = startGradient; i <= endGradient; ++i)
            {
              StopDistData stopDistData;
              stopDistData.gradient = i;
              //Set state to loaded so the
              if (DS::AbstractTSetup::corePtr()->getTrainLoadStatus() == TrainIsLoaded)
              {
                stopDistData.loadedDist = Supv::BrakeCalculations::instance().calcAccDeaccDistance(AbstractConfig::corePtr()->getReleaseSpeed(), 0U, Supv::BrakeCalculations::instance().getSBCurveDelay(), i);
                DS::AbstractTSetup::corePtr()->setTrainLoadStatus(TrainIsEmpty);
                stopDistData.unloadedDist = Supv::BrakeCalculations::instance().calcAccDeaccDistance(AbstractConfig::corePtr()->getReleaseSpeed(), 0U, Supv::BrakeCalculations::instance().getSBCurveDelay(), i);
                DS::AbstractTSetup::corePtr()->setTrainLoadStatus(TrainIsLoaded);
              }
              else
              {
                stopDistData.unloadedDist = Supv::BrakeCalculations::instance().calcAccDeaccDistance(AbstractConfig::corePtr()->getReleaseSpeed(), 0U, Supv::BrakeCalculations::instance().getSBCurveDelay(), i);
                DS::AbstractTSetup::corePtr()->setTrainLoadStatus(TrainIsLoaded);
                stopDistData.loadedDist = Supv::BrakeCalculations::instance().calcAccDeaccDistance(AbstractConfig::corePtr()->getReleaseSpeed(), 0U, Supv::BrakeCalculations::instance().getSBCurveDelay(), i);
                DS::AbstractTSetup::corePtr()->setTrainLoadStatus(TrainIsEmpty);
              }
              positionReport.stopDistDataVec.push_back(stopDistData);
            }
          }
        }
      }
    }

    /******************************************************************************
    * RadioMessageOutPositionReport::assembleMessageData
    ******************************************************************************/
    bool RadioMessageOutPositionReport::assembleMessageData()
    {
      bool assembleDataValid = true;

      // Sanity check of certain (that can be checked) parameter values
      if (!validateQ_POSITION(static_cast<uint8_t>(positionReport.positionClassification)))
      {
        trace->write(ATC::detailedTrace, "Q_POSITION invalid");
        assembleDataValid = false;
      }

      if (!validateB_DIRECTION(positionReport.orientationAndDirection))
      {
        trace->write(ATC::detailedTrace, "B_DIRECTION invalid");
        assembleDataValid = false;
      }

      if (!validateB_TRAIN_STATUS(positionReport.trainCoreStatus))
      {
        trace->write(ATC::detailedTrace, "B_TRAIN_STATUS invalid");
        assembleDataValid = false;
      }

      if (!validateQ_ATP_MODE(static_cast<uint8_t>(positionReport.atpMode)))
      {
        trace->write(ATC::detailedTrace, "ATP_MODE invalid");
        assembleDataValid = false;
      }

      if (!validateQ_ATO_MODE(positionReport.atoMode))
      {
        trace->write(ATC::detailedTrace, "ATO_MODE invalid");
        assembleDataValid = false;
      }

      if (!validateM_BRAKE_SYSTEM(static_cast<uint8_t>(positionReport.brakeSystem)))
      {
        trace->write(ATC::detailedTrace, "M_BRAKE_SYSTEM invalid");
        assembleDataValid = false;
      }

      if (assembleDataValid)
      {
        VFW_Buffer buffer;

        // Initialize buffer to first byte of Application level message
        vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));

        // Assemble all data into net-message format
        vfwPutU8(&buffer, static_cast<uint8_t>(messageType));

        vfwPutU16(&buffer, positionReport.trailingTrackAndPosition.track);
        vfwPutU32(&buffer, positionReport.trailingTrackAndPosition.position);
        vfwPutU16(&buffer, positionReport.leadingTrackAndPosition.track);
        vfwPutU32(&buffer, positionReport.leadingTrackAndPosition.position);

        vfwPutU8(&buffer, static_cast<uint8_t>(positionReport.positionClassification));
        vfwPutU8(&buffer, positionReport.orientationAndDirection);

        vfwPutU16(&buffer, positionReport.trainSpeed);
        vfwPutU32(&buffer, positionReport.trainCoreStatus);

        vfwPutU16(&buffer, positionReport.frontConfidenceInterval);
        vfwPutU16(&buffer, positionReport.rearConfidenceInterval);

        vfwPutU16(&buffer, positionReport.maTargetTrackAndPosition.track);
        vfwPutU32(&buffer, positionReport.maTargetTrackAndPosition.position);

        vfwPutU8(&buffer, static_cast<uint8_t>(positionReport.atpMode));
        vfwPutU8(&buffer, positionReport.atoMode);

        vfwPutU8(&buffer, static_cast<uint8_t>(positionReport.brakeSystem));

        //Put POSSESSION_REQUEST Block
        if (positionReport.possesionRequestReceived)
        {
          vfwPutU8(&buffer, BTypePossesionRequest);
        }

        //Put SHUNTING_REQUEST Block
        if (positionReport.shuntingRequestReceived)
        {
          vfwPutU8(&buffer, BTypeShuntingRequest);
        }

        //Put YARD_REQUEST Block
        if (positionReport.yardRequestReceived)
        {
          vfwPutU8(&buffer, BTypeYardRequest);
        }

        //Put LAST_BALISE Block
        if (positionReport.lastBaliseReceived.idLastReadBalise != 0U)
        {
          vfwPutU8(&buffer, BTypeLastBalise);
          vfwPutU16(&buffer, positionReport.lastBaliseReceived.idLastReadBalise);
        }

        //Put TRAIN_NAME Block
        if (positionReport.trainNameReceived)
        {
          vfwPutU8(&buffer, BTypeTrainName);
          for (uint8_t i = 0U; i < trainNameMaxLength; i++)
          {
            vfwPutI8(&buffer, static_cast<int8_t>(positionReport.trainName.trainName[i]));
          }
        }

        //Put ETA_CONFIRMATION Block
        if (positionReport.etaConfirmationReceived)
        {
          vfwPutU8(&buffer, BTypeETAConfirmation);
          vfwPutU8(&buffer, static_cast<uint8_t>(positionReport.etaConfirmation.arrivalTimeAcknowledge));
          vfwPutU64(&buffer, positionReport.etaConfirmation.newETATime);
        }

        //Put CANCEL_AREA
        if (positionReport.cancelAreaReceived)
        {
          vfwPutU8(&buffer, BTypeCancelArea);
        }

        if (msgAckToSend)
        {
          //Message Acknowledge
          vfwPutU8(&buffer, BTypeMessageAcknowledge);
          vfwPutU8(&buffer, positionReport.msgAck.msgId);
          positionReport.msgAck.messageAccepted ? vfwPutU8(&buffer, 1U) : vfwPutU8(&buffer, 0U);
        }

        // STOP_DIST Block
        if (positionReport.stopDistDataVec.size() > 0U)
        {
          for (std::vector<StopDistData>::iterator it = positionReport.stopDistDataVec.begin();
            it != positionReport.stopDistDataVec.end(); ++it)
          {
            vfwPutU8(&buffer, BTypeStopDistData);
            vfwPutI8(&buffer, it->gradient);
            vfwPutU8(&buffer, static_cast<uint8_t>((it->unloadedDist & 0xFF0000U) >> 16U));
            vfwPutU8(&buffer, static_cast<uint8_t>((it->unloadedDist & 0xFF00U) >> 8U));
            vfwPutU8(&buffer, static_cast<uint8_t>(it->unloadedDist & 0xFFU));
            vfwPutU8(&buffer, static_cast<uint8_t>((it->loadedDist & 0xFF0000U) >> 16U));
            vfwPutU8(&buffer, static_cast<uint8_t>((it->loadedDist & 0xFF00U) >> 8U));
            vfwPutU8(&buffer, static_cast<uint8_t>(it->loadedDist & 0xFFU));
          }
        }

        //Put EVENT_DATA Block
        for (std::vector<EventData>::iterator it = positionReport.eventDataVec.begin();
          it != positionReport.eventDataVec.end(); ++it)
        {
          vfwPutU8(&buffer, BTypeEventData);
          vfwPutU32(&buffer, it->idEventData);
        }

        //Put EVENT_DATA_TEXT Block
        for (std::vector<EventDataText>::iterator it = positionReport.eventDataTextVec.begin();
          it != positionReport.eventDataTextVec.end(); ++it)
        {
          const EventDataText& eventDataText = *it;
          vfwPutU8(&buffer, BTypeEventDataText);
          vfwPutU32(&buffer, eventDataText.idEventDataText);

          for (uint8_t i = 0U; i < (sizeof(eventDataText.text) - 1U); i++)
          {
            vfwPutI8(&buffer, static_cast<int8_t>(eventDataText.text[i]));
          }
        }

        //Put EXTERNAL_DATA Block
        if (positionReport.externalDataReceived)
        {
          vfwPutU8(&buffer, BTypeExternalData);
          vfwPutU8(&buffer, positionReport.externalData.nidSystem);
          vfwPutU16(&buffer, positionReport.externalData.noOfAppData);
        }

        assembleAdditionalBlocks(buffer);

        vfwPutU8(&buffer, M_END_OF_MESSAGE);

        // Total length of message
        messageData.message.dataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));
      }

      traceAssembleData(assembleDataValid);

      return assembleDataValid;
    }

    /******************************************************************************
    * RadioMessageOutPositionReport::getChannelId
    ******************************************************************************/
    uint16_t RadioMessageOutPositionReport::getChannelId() const
    {
      return dedicatedChannelId;
    }

    /******************************************************************************
    * RadioMessageOutPositionReport::invalidate
    ******************************************************************************/
    void RadioMessageOutPositionReport::invalidate()
    {
      // Only clear variable data part if an ack was received from radio-handler
      if (ackDefaultPositionReportReceived)
      {
        //clear the vector
        positionReport.shuntingRequestReceived = false;
        positionReport.yardRequestReceived = false;
        positionReport.possesionRequestReceived = false;

        // Clear all optional data
        positionReport.trainNameReceived = false;
        positionReport.etaConfirmationReceived = false;
        positionReport.eventDataVec.clear();
        positionReport.eventDataTextVec.clear();
        positionReport.externalDataReceived = false;

        // CANCEL AREA Data is collected and it has been sent to connected Region TCC.
        if (positionReport.cancelAreaReceived)
        {
          // To disable the Cancel Reg Area in Manu overlay.
          Kernel::AbstractMessageHandler::corePtr()->setRegistrationAreaMessageSentToCentralTCC(false);
        }

        positionReport.cancelAreaReceived = false;
        positionReport.stopDistDataVec.clear();
        positionReport.msgAck.messageAccepted = false;
        positionReport.msgAck.msgId = 0U;
        msgAckToSend = false;
      }

      dataProcessState = NoDataAvailable;
    }

    /******************************************************************************
    * RadioMessageOutPositionReport::getPositionReport
    ******************************************************************************/
    const PositionReport& RadioMessageOutPositionReport::getPositionReport() const
    {
      return positionReport;
    }

    /******************************************************************************
    * RadioMessageOutPositionReport::assembleAdditionalBlocks
    ******************************************************************************/
    void RadioMessageOutPositionReport::assembleAdditionalBlocks(VFW_Buffer &buffer)
    {
      // Writing the below trace for removing warning
      trace->write(ATC::detailedTrace, "Size of Buffer passed", buffer.b_s);
    }
  }
}
