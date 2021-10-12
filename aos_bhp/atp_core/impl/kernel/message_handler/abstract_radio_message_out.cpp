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
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-20    bhermans    Created
* 2016-04-03    bhermans    File renamed
* 2016-08-26    marlundg    Updated for ATP-Limited
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_radio_message_common.hpp"
#include "abstract_radio_message_out.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_brake.hpp"
#include "abstract_odometry.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_supervise.hpp"
#include "abstract_tims.hpp"
#include "abstract_log_handler.hpp"
#include "radio_channel.hpp"
#include "emergency_alert_seq.hpp"

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
    * AbstractRadioMessageOut constructor
    ******************************************************************************/
    AbstractRadioMessageOut::AbstractRadioMessageOut() : messageType(MTypePositionReportRequest),
      channelId(0U), dataProcessState(NoDataAvailable), implemented(false)
    {
      // Use the TraceInterface from Message Handler
      trace = AbstractMessageHandler::corePtr()->getTrace();
      messageData.channelId = 0U;
    }

    /******************************************************************************
    * AbstractRadioMessageOut destructor
    ******************************************************************************/
    AbstractRadioMessageOut::~AbstractRadioMessageOut()
    {
      trace = static_cast<ATC::TraceInterface*>(NULL);
    }
    
    /******************************************************************************
    * AbstractRadioMessageOut alternative constructor
    ******************************************************************************/
    AbstractRadioMessageOut::AbstractRadioMessageOut(const RadioMessageType mType) : 
      messageType(mType), channelId(0U), dataProcessState(NoDataAvailable), implemented(false)
    {
      // Use the TraceInterface from Message Handler
      trace = AbstractMessageHandler::corePtr()->getTrace();
    }
    
    /******************************************************************************
    * getImplemented
    ******************************************************************************/
    bool AbstractRadioMessageOut::getImplemented() const
    {
      return implemented;
    }
    
    /******************************************************************************
    * getMessageData
    ******************************************************************************/
    bool AbstractRadioMessageOut::getMessageData(RadioMessageToSend& mData) const
    {
      if (DataValidated == dataProcessState)
      {
        mData = messageData;

        mData.channelId = getChannelId();
      }

      return (DataValidated == dataProcessState);
    }
    
    /******************************************************************************
    * traceParseData
    ******************************************************************************/
    void AbstractRadioMessageOut::traceAssembleData(const bool parseOk) const
    {
      if (parseOk)
      {
        trace->write(ATC::veryDetailedTrace, "Assembling outgoing data Ok");
      }
      else
      {
        trace->write(ATC::veryDetailedTrace, "Assembling outgoing data failed");
      }
    }

    /******************************************************************************
    * collectTrainStatusInfo
    ******************************************************************************/
    void AbstractRadioMessageOut::collectTrainStatusInfo(uint32_t& trainStatus) const
    {
      // TODO: ATP-Limited, Collect remaining status bits when implemented
      trainStatus = 0U;

      // Bit 0, Safety Halt, AOS
      // Bit regarding Safety halt shall also be set not only in safety halt mode but also in powering down mode,
      // if ATP is being powering down after safety halt event has occurred
      trainStatus |= (ATC::AbstractEventHandler::corePtr()->isModeChangeToSafetyHalt()) ? trainStatusSafetyHalt : 0U;

      // Bit 2, TIMS Integrity Broken
      trainStatus |= (TG::AbstractTIMS::corePtr()->getTimsStatus() == TG::TIMSStatusBroken) ? trainStatusTIMSIntegrityBroken : 0U;

      // Bit 3, Braking event, AOS
      const bool ebApplied = ATC::AbstractEventHandler::corePtr()->isApplyEb();
      const bool sbApplied = ATC::AbstractEventHandler::corePtr()->isApplySb();

      trainStatus |= (ebApplied || sbApplied) ? trainStatusBrakingEvent : 0U;

      // Bit 4, Handling Done
      trainStatus |= (AbstractModeControl::corePtr()->getHandlingDone()) ? trainStatusHandlingDone : 0U;

      // Bit 5, Train idling (No MA:s to act upon)
      trainStatus |= (AbstractModeControl::corePtr()->getIdleState()) ? trainStatusTrainIdling : 0U;

      // Bit 6, TIMS function disabled by driver
      trainStatus |= (TG::AbstractTIMS::corePtr()->getTimsSupervision() == TG::TIMSInhibited) ? trainStatusIntegrityInhibitedByDriver : 0U;

      // Bit 7, MA Timeout
      trainStatus |= (AbstractModeControl::corePtr()->getMATimeOut()) ? trainStatusMaTimeOut : 0U;

      // Bit 8, ATP Reset
      trainStatus |= (AbstractModeControl::corePtr()->getATPReset()) ? trainStatusATPReset : 0U;

      //Bit 9, Not ready to drive
      trainStatus |= (AbstractModeControl::corePtr()->getATPNeedsReset()) ? trainStatusATPNeedsRestart : 0U;

      // Bit 10, ATP Intervention
      trainStatus |= (Supv::AbstractSupervise::corePtr()->getAtpIntervention()) ? trainStatusATPIntervention : 0U;

      // Bit 11, Brake release requested
      bool ebReleaseEnabled = Supv::AbstractBrake::corePtr()->getEbReleaseEnable();
      bool sbReleaseEnabled = Supv::AbstractBrake::corePtr()->getSbReleaseEnable();

      trainStatus |= (ebReleaseEnabled || sbReleaseEnabled) ? trainStatusBrakeReleaseRequested : 0U;

      // Bit 13, Slip detected
      trainStatus |= Pos::AbstractOdometry::corePtr()->isSlipping() ? trainStatusSlipDetected : 0U;

      // Bit 14, Free Rolling
      trainStatus |= AbstractModeControl::corePtr()->getFreeRolling() ? trainStatusFreeRolling : 0U;

      // Bit 15, Emergency alert active (i.e. NOT Inactive)
      trainStatus |= (AbstractModeControl::corePtr()->getEmergencyAlertSeqState() != emergencyAlertInactive) ? trainStatusEmergencyAlertActive : 0U;

      // Bit 17, Not Ready To Drive
      trainStatus |= AbstractModeControl::corePtr()->getNotReadyToDrive() ? trainStatusNotReadyToDrive : 0U;

      //Bit 19, AOS downloaded Parameters not received
      trainStatus |= (!AbstractMessageHandler::corePtr()->isConfigDataReceived()) ? trainStatusAosDownloadedParameterNotRecv : 0U;
    }

    /******************************************************************************
    * writeToLog
    ******************************************************************************/
    void AbstractRadioMessageOut::writeToLog(ATC::LogLevel const level, const char_t * const text,
      const char_t* const filepath, const int32_t line) const
    {
       ATC::AbstractLogHandler::corePtr()->writeToLog(level, text, "MH",filepath,line);
    }

    /******************************************************************************
    * getChannelId
    ******************************************************************************/
    uint16_t AbstractRadioMessageOut::getChannelId() const
    {
      /* channel id 0 means: Any channel that is Region TCC (>= 2) */
      return ATP::RadioCom::radioChannelId0;
    }

    /******************************************************************************
    * isDataAvailable
    ******************************************************************************/
    bool AbstractRadioMessageOut::isDataAvailable() const
    {
      return (DataAvailable == dataProcessState);
    }
  }
}
