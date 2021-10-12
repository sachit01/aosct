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
* This file implements the parser for the PossessionAcknowledge message.
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
#include "radio_message_in_possession_acknowledge.hpp"
#include "abstract_odometry.hpp"
#include "abstract_tracks.hpp"
#include "abstract_message_handler_event_ids.hpp"
#include "dmi_event_codes.hpp"

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
    * RadioMessageInPossessionAcknowledge constructor
    ******************************************************************************/
    RadioMessageInPossessionAcknowledge::RadioMessageInPossessionAcknowledge()
      : AbstractRadioMessageIn(MTypePossessionAcknowledge),
      tooManyBalisesInPossessionAck(ATC::Event::createLogEvent(atpMessageHandlerId, ATC::CoreContainer, eventIdTooManyBalisesInPossessionAck,
        DMICom::msgHdlrTooManyBalisesInPossessionAck, "Nr of balises in PossessionAck >", true))
    {
      implemented = true;
      possessionAcknowledgeData.possAcknowledge = RequestNotAcknowledged;
      possessionAcknowledgeData.allowedSpeedInPossession = 0U;
      possessionAcknowledgeData.baliseIdVec.reserve(static_cast<size_t>(DS::AbstractTracks::maxPossessionBaliseCount));
    }

    /******************************************************************************
    * RadioMessageInPossessionAcknowledge::validate
    ******************************************************************************/
    bool RadioMessageInPossessionAcknowledge::validate()
    {
      trace->write(ATC::briefTrace, "Validating PossessionAcknowledge");

      bool ret = AbstractRadioMessageIn::validate();

      if (ret)
      {
        ret = false;

        // Parse, validate and publish data
        if (DataAvailable == dataProcessState)
        {
          if (parseMessageData())
          {
            if (validateMode())
            {
              if (publishData())
              {
                dataProcessState = DataValidated;
                ret = true;
              }
            }
          }
        }
      }

      return ret;
    }

    /******************************************************************************
    * RadioMessageInPossessionAcknowledge::validateMode
    ******************************************************************************/
    bool RadioMessageInPossessionAcknowledge::validateMode() const
    {
      bool modeValid = false;
      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();

      switch (mode)
      {
      case ATPModeSafeBrakeToStop:
      case ATPModeUnregistered:
      case ATPModeLocation:
      case ATPModeBaliseSearch:
      case ATPModeNormal:
      case ATPModeStaffResponsible:
      case ATPModeShuntingRoute:
      case ATPModeSplit:
      case ATPModeConfiguration:
      case ATPModeRegistration:
        if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
        {
          modeValid = true;
        }
        break;

      case ATPModePowerUp:
      case ATPModeYard:
      case ATPModeShunting:
      case ATPModeJoin:
      case ATPModeSleeping:
        {
          if (IO::AbstractLocoIO::corePtr()->getSleepingSignal())
          {
            //Reject the incoming message from TCC when the sleep signal is active
            modeValid = false;
            invalidIncmgMsgTCC.setDynamicText("Possession Ack");
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
          }
          else
          {
            // Accept the incoming message from TCC when the sleep signal is inactive
            modeValid = true;
          }
        }
        break;
      case ATPModePoweringDown:
      case ATPModeSafetyHalt:
      case ATPModePossession:
        break;
      case ATPModesCount:
      case ATPModeUndefined:
      default:
        ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        break;
      }

      traceValidateMode(modeValid);
      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInPossessionAcknowledge::parseMessageData
    ******************************************************************************/
    bool RadioMessageInPossessionAcknowledge::parseMessageData()
    {
      bool parseDataValid = true;
      uint8_t tmpValU8;

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

      // Read & validate Q_ACKNOWLEDGE
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateQ_ACKNOWLEDGE(tmpValU8))
      {
        trace->write(ATC::detailedTrace, "Q_ACKNOWLEDGE invalid");
        parseDataValid = false;
        invalidDataInTCCMessage.setDynamicText("Q_ACKNOWLEDGE");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }
      else
      {
        possessionAcknowledgeData.possAcknowledge = static_cast<Acknowledge>(tmpValU8);
      }

      // Read V_SPEED
      possessionAcknowledgeData.allowedSpeedInPossession = vfwGetU16(&buffer);

      // BlockData
      uint8_t nextMsgIdentifier = vfwGetU8(&buffer);

      while ((nextMsgIdentifier != M_END_OF_MESSAGE) && (parseDataValid))
      {
        if (BTypeBaliseIdentity == nextMsgIdentifier)
        {
          const uint16_t tmpValU16 = vfwGetU16(&buffer);
          if (validateNID_BG(tmpValU16))
          {
            baliseIdentity baliseId;
            baliseId.baliseID = tmpValU16;

            //if vector is empty; push the balise id
            if (possessionAcknowledgeData.baliseIdVec.empty())
            {
              possessionAcknowledgeData.baliseIdVec.push_back(baliseId);
            }
            else
            {//if vector is not empty
              if (possessionAcknowledgeData.baliseIdVec.size() < static_cast<size_t>(DS::AbstractTracks::maxPossessionBaliseCount))
              {
                bool isInsertPosFound = false;
                std::vector<baliseIdentity>::iterator itr = possessionAcknowledgeData.baliseIdVec.begin();
                std::vector<baliseIdentity>::iterator foundItr = possessionAcknowledgeData.baliseIdVec.begin();
                //loop over the possession balise list
                for (; (itr != possessionAcknowledgeData.baliseIdVec.end()) && (parseDataValid) && (!isInsertPosFound); ++itr)
                {
                  if (baliseId.baliseID == itr->baliseID)
                  {
                    parseDataValid = false;
                  }
                  else if (baliseId.baliseID < itr->baliseID)
                  {
                    isInsertPosFound = true;
                    foundItr = itr;
                  }
                  else
                  {
                    //do nothing
                  }
                }
                if (isInsertPosFound && (parseDataValid))
                {
                  static_cast<void>(possessionAcknowledgeData.baliseIdVec.insert(foundItr, baliseId));
                }
                //if position to insert is not found and same balise id is not there.
                else if ((!isInsertPosFound) && parseDataValid)
                {
                  possessionAcknowledgeData.baliseIdVec.push_back(baliseId);
                }
                else
                {
                  trace->write(ATC::detailedTrace, "Duplicated balises");
                }
              }
              else
              {
                trace->write(ATC::detailedTrace, "Possession balise id vector overflow");
                parseDataValid = false;
                // LogEvent instead of SafetyHalt when message discarded because of too many balises
                tooManyBalisesInPossessionAck.setDynamicText(possessionAcknowledgeData.baliseIdVec.size());
                ATC::AbstractEventHandler::corePtr()->reportEvent(tooManyBalisesInPossessionAck, __FILE__, __LINE__);
              }
            }
          }
          else
          {
            trace->write(ATC::detailedTrace, "NID_BG invalid");
            parseDataValid = false;
            invalidDataInTCCMessage.setDynamicText("NID_BG");
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
          }
        }
        else
        {
          parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
        }
        nextMsgIdentifier = vfwGetU8(&buffer);
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
    * RadioMessageInPossessionAcknowledge::invalidate
    ******************************************************************************/
    void RadioMessageInPossessionAcknowledge::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      //Reset the values
      possessionAcknowledgeData.possAcknowledge = RequestNotAcknowledged;
      possessionAcknowledgeData.allowedSpeedInPossession = 0U;
      possessionAcknowledgeData.baliseIdVec.clear();
      dataProcessState = NoDataAvailable;
    }

    /******************************************************************************
    * RadioMessageInPossessionAcknowledge::publishData
    ******************************************************************************/
    bool RadioMessageInPossessionAcknowledge::publishData()
    {
      bool isDataPublished = true;

      if (RequestAcknowledged == possessionAcknowledgeData.possAcknowledge)
      {
        std::vector<baliseIdentity>::iterator itr = possessionAcknowledgeData.baliseIdVec.begin();
        for (; itr != possessionAcknowledgeData.baliseIdVec.end(); ++itr)
        {
          if (!DS::AbstractTracks::corePtr()->addPossessionBalise(itr->baliseID))
          {
            isDataPublished = false;
          }
        }
      }

      return isDataPublished;
    }

    /******************************************************************************
    * RadioMessageInPossessionAcknowledge::getPossessionAcknowledge
    ******************************************************************************/
    const PossessionAcknowledge* RadioMessageInPossessionAcknowledge::getPossessionAcknowledge() const
    {
      const PossessionAcknowledge* possessionAcknowledgeMessage = static_cast<PossessionAcknowledge*>(NULL);

      if (DataValidated == dataProcessState)
      {
        possessionAcknowledgeMessage = &possessionAcknowledgeData;
      }

      return possessionAcknowledgeMessage;
    }
  }
}
