/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->LCS) has an associated creator class inherited from AbstractLCSMessageOut.
* This file implements the creator for the ATP Command Message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-25    nsyed    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_types.h>
#include "lcs_message_out_atp_command.hpp"
#include "abstract_tic.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tsetup.hpp"
#include "vehicle_com.hpp"
#include "message_handler.hpp"
#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#endif

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
  namespace TG
  {
    /******************************************************************************
    * LCSMessageOutATPCommand constructor
    ******************************************************************************/
    LCSMessageOutATPCommand::LCSMessageOutATPCommand() : AbstractLCSMessageOut(LCSMTypeATPCommandMessage, 1U, true)
    {
      atpCommand.ecpbTrainCompReqStatus = EcpbTrainCompNotAsserted;
      atpCommand.holdingBrakeReqStatus = HoldingBrakeNotAsserted;
      atpCommand.totalTrainWeight = 0U; // Not asserted
      compositionRequestSentTime = 0;
    }

    /******************************************************************************
    * LCSMessageOutATPCommand::collectData
    ******************************************************************************/
    void LCSMessageOutATPCommand::collectData()
    {
      // Default is no data available
      setDataProcessState(NoDataAvailable);

      // Send the ATP Command only when "connected" to LCS
      if (VehicleCom::instance().connectedVehicleComm())
      {
        const TICConfigStatus ticStatus = AbstractTIC::corePtr()->getStatus();

        if ((TICConfigStatusPending == ticStatus) || (TICConfigStatusInProgress == ticStatus))
        {
          const bool ticAvailable = AbstractTIC::corePtr()->getTICAvailable();
          const ATP::ATPMode atpMode = ATP::Kernel::AbstractModeControl::corePtr()->getCurrentMode();

          if (ticAvailable && (ATP::ATPModeConfiguration == atpMode))
          {
            // If it is the first time to send Composition Request, do it immediately.
            // Otherwise, wait for the timer to indicate a new request to be sent.

            const int64_t timeNow = vfwGetReferenceTime();
            const bool firstRequest = (EcpbTrainCompNotRequested == atpCommand.ecpbTrainCompReqStatus) ||
              (EcpbTrainCompNotAsserted == atpCommand.ecpbTrainCompReqStatus);
            const bool timerExpired = (timeNow - compositionRequestSentTime) >= timeoutSendCompositionRequest;

            if (firstRequest || timerExpired)
            {
              atpCommand.ecpbTrainCompReqStatus = EcpbTrainCompRequested;
              setDataProcessState(DataAvailable);

              // Re-Start the 'Send message Timer' for next request
              compositionRequestSentTime = timeNow;
            }
          }
          else
          {
            //TIC System is not available or ATP mode is not configuration
            getTracer().write(ATC::briefTrace, "TIC is not available or ATP is not in Configuration mode");
          }

        }
        else
        {
          // Stop requesting the Train Composition when TIC is completed (or an Error occurred).
          if (EcpbTrainCompNotRequested != atpCommand.ecpbTrainCompReqStatus)
          {
            atpCommand.ecpbTrainCompReqStatus = EcpbTrainCompNotRequested;
            setDataProcessState(DataAvailable);
          }
        }

        int32_t trainWeight;
        const bool leaderHasRecovered = VehicleCom::instance().leaderHasRecoveredFromComLoss();
        const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

        trainWeight = DS::AbstractTSetup::corePtr()->getTrainWeight();
        // New change? -> Trig a message
        if (static_cast<uint32_t>(trainWeight) != atpCommand.totalTrainWeight)
        {
          atpCommand.totalTrainWeight = static_cast<uint32_t>(trainWeight);
          setDataProcessState(DataAvailable);
        }

        // Resend latest received weight if LEADER has recovered
        else if (leaderHasRecovered && (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL)))
        {
          setDataProcessState(DataAvailable);
        }
        else
        {
          // No action for other states.
        }
        
        // If StandStill Event is active --> HoldingBrakeRequested
        if (ATC::AbstractEventHandler::corePtr()->isStandstillEventActive())
        {
          // New change? -> Trig a message
          if (HoldingBrakeRequested != atpCommand.holdingBrakeReqStatus)
          {
            setDataProcessState(DataAvailable);
          }

          atpCommand.holdingBrakeReqStatus = HoldingBrakeRequested;

          // If StandStill Event is NOT active --> HoldingBrakeNotRequested
        }
        else
        {
          // New change? -> Trig a message
          if (HoldingBrakeNotRequested != atpCommand.holdingBrakeReqStatus)
          {
            setDataProcessState(DataAvailable);
          }

          atpCommand.holdingBrakeReqStatus = HoldingBrakeNotRequested;
        }
      }
    }
  
    /******************************************************************************
    * LCSMessageOutATPCommand::validate
    ******************************************************************************/
    bool LCSMessageOutATPCommand::validate(EmpMsg* const mData, uint16_t& length)
    {
      // Parse, validate and publish data
      if (getDataProcessState() == DataAvailable)
      {
        getTracer().write(ATC::briefTrace, "Validating Data in the ATP Command message to LCS");

        if (assembleMessageData(mData, length))
        {
          setDataProcessState(DataValidated);
        }
      }
      return (getDataProcessState() == DataValidated);
    }

    /******************************************************************************
    * LCSMessageOutATPCommand::assembleMessageData
    ******************************************************************************/
    bool LCSMessageOutATPCommand::assembleMessageData(EmpMsg* const messageData, uint16_t& appDataLength) const
    {
      bool parseDataValid = true;

      if (getDataProcessState() == DataAvailable)
      {
        VFW_Buffer buffer;

        // Initialize buffer to first byte of Application level message
        vfwInitBuffer(&buffer, messageData->getEMPBodyBuffer(), messageData->getEMPMessageMaxBodyLen());

        vfwPutU8(&buffer, static_cast<uint8_t>(atpCommand.ecpbTrainCompReqStatus));
        vfwPutU8(&buffer, static_cast<uint8_t>(atpCommand.holdingBrakeReqStatus));
        vfwPutU32(&buffer, atpCommand.totalTrainWeight);

        traceAssembleData(parseDataValid);

        // Total length of Application-message
        appDataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));
      }

      return parseDataValid;
    }

    /******************************************************************************
    *  LCSMessageOutATPCommand::invalidate
    ******************************************************************************/
    void LCSMessageOutATPCommand::invalidate()
    {
      setDataProcessState(NoDataAvailable);
    }
  }
}
