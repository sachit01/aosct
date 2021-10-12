/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (TCC->AOS) has an associated parser class inherited from AbstractRadioMessageIn.
* This file implements the parser for the AreaRequest message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-02-28    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_mode_control.hpp"
#include "radio_message_in_area_request.hpp"
#include "abstract_message_handler.hpp"


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
    * RadioMessageInAreaRequest constructor
    ******************************************************************************/
    RadioMessageInAreaRequest::RadioMessageInAreaRequest() : AbstractRadioMessageIn(MTypeAreaRequestMessage)
    {
      implemented = true;
      memset(&tccAreaData.areaId[0], 0, sizeof(tccAreaData.areaId));
    }


    /******************************************************************************
    * RadioMessageInAreaRequest::validate
    ******************************************************************************/
    bool RadioMessageInAreaRequest::validate()
    {
      trace->write(ATC::briefTrace, "Validating AreaRequest");

      // Parse, validate and publish data
      if (DataAvailable == dataProcessState)
      {
        if (parseMessageData())
        {
          if (validateMode())
          {
            //Area request received. Now, there is a need to send a reply to the TCC. Update the flag.
            AbstractMessageHandler::corePtr()->setReplyRegAreaToTCC(true);
            dataProcessState = DataValidated;
          }
        }
      }

      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageInAreaRequest::validateMode
    ******************************************************************************/
    bool RadioMessageInAreaRequest::validateMode() const
    {
      bool modeValid = false;

      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
        case ATPModePowerUp:
        case ATPModeYard:
        case ATPModeShunting:
        case ATPModePossession:
        case ATPModeConfiguration:
        case ATPModeRegistration:
        case ATPModeBaliseSearch:
        case ATPModeNormal:
        case ATPModeLocation:
        case ATPModeStaffResponsible:
        case ATPModeSplit:
        case ATPModeJoin:
        case ATPModeShuntingRoute:
        case ATPModeUnregistered:
        case ATPModeSafeBrakeToStop:
        case ATPModePoweringDown:
        {
          //Accept the incoming message from TCC when the train is not at standstill
          if (!Pos::AbstractOdometry::corePtr()->isTrainStandStill())
          {
            //Reject the incoming message from TCC
            modeValid = false;
            invalidIncmgMsgTCC.setDynamicText("Train NotStandstill");
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
          }
          else
          {
            modeValid = true;
          }
          break;
        }
        case ATPModeSleeping:
        {
          // Accept the incoming message from TCC when the sleep signal is inactive
          if (IO::AbstractLocoIO::corePtr()->getSleepingSignal())
          {
            //Reject the incoming message from TCC when the sleep signal is active
            modeValid = false;
            invalidIncmgMsgTCC.setDynamicText("AreaReq in Sleeping");
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
          }
          else
          {
            modeValid = true;
          }
          break;
        }

        case ATPModeSafetyHalt:
          break;

        case ATPModesCount:
        case ATPModeUndefined:
        default:
        {
          ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        }
        break;
      }
      traceValidateMode(modeValid);
      return modeValid;
    }
    
    /******************************************************************************
    * RadioMessageInAreaRequest::parseMessageData
    ******************************************************************************/
    bool RadioMessageInAreaRequest::parseMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));
      vfwSetReadBuffer(&buffer, sizeof(messageData.message.data));

      // Read & validate NID_MESSAGE_TYPE
      if (vfwGetU8(&buffer) != static_cast<uint8_t>(messageType))
      {
        trace->write(ATC::detailedTrace, "NID_MESSAGE_TYPE invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("NID_MESSAGE_TYPE");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

      // BlockData
      uint8_t nextMsgIdentifier = vfwGetU8(&buffer);
      uint8_t noOfAreaIndex = 0U;

      // Fetch next data-block until M_END_OF_MESSAGE
      while ((nextMsgIdentifier != M_END_OF_MESSAGE) && (parseDataValid))
      {
        switch (nextMsgIdentifier)
        {
        case BTypeArea:
        {
          if (noOfAreaIndex < regionAreaDataSize)
          {
            tccAreaData.areaId[noOfAreaIndex] = vfwGetU8(&buffer);
            if (tccAreaData.areaId[noOfAreaIndex] == 0U)
            {
              trace->write(ATC::detailedTrace, "NID_AREA invalid");
              parseDataValid = false;
              invalidDataInTCCMessage.setDynamicText("Invalid NID_AREA = 0");
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
            }
          }
          else
          {
            trace->write(ATC::detailedTrace, "Area Requested overflow");
            parseDataValid = false;
          }
          break;
        }
        default:
          parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
          break;
        } // end switch

        // increase the index
        noOfAreaIndex++;

        // Fetch next msg-type (or M_END_OF_MESSAGE)
        nextMsgIdentifier = vfwGetU8(&buffer);
      }

      if (parseDataValid)
      {
        if (noOfAreaIndex > 1U)
        {   // More than one area is needed according to FFFIS TCC-AOS
          tccAreaData.numAreas = noOfAreaIndex;
        }
        else
        {
          parseDataValid = false;
          invalidDataInTCCMessage.setDynamicText("TCCAreas < 2");
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
        }
      }
      else
      {
        tccAreaData.numAreas = 0U;
      }

      if ((!validateSizeOfParsedBytes(&buffer)) && (parseDataValid))
      {
        invalidDataInTCCMessage.setDynamicText("Msg size incorrect");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
        parseDataValid = false;
      }

      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInAreaRequest::invalidate
    ******************************************************************************/
    void RadioMessageInAreaRequest::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      // Invalidate the Area request.
      AbstractMessageHandler::corePtr()->setReplyRegAreaToTCC(false);

      memset(&tccAreaData.areaId[0], 0, sizeof(tccAreaData.areaId));
      dataProcessState = NoDataAvailable;
    }

    /******************************************************************************
    * RadioMessageInAreaRequest::getAvailableAreasFromTCC
    ******************************************************************************/
    bool RadioMessageInAreaRequest::getAvailableAreasFromTCC(TCCAreas &tccArea) const
    {
      if (DataValidated == dataProcessState)
      {
        tccArea = tccAreaData;
      }
      else
      {
        //Invalidate
        tccArea.numAreas = 0U;
      }

      return (DataValidated == dataProcessState);
    }

  }
}
