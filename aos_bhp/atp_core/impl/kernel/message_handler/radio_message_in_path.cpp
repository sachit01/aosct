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
* This file implements the parser for the Path message.
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
#include <algorithm>
#include "abstract_mode_control.hpp"
#include "radio_message_types.hpp"
#include "radio_message_in_path.hpp"
#include "atc_math.hpp"
#include "atc_bit_checker.hpp"

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
    * RadioMessageInPath constructor
    ******************************************************************************/
    RadioMessageInPath::RadioMessageInPath() : AbstractRadioMessageIn(MTypePath)
    {
      implemented = true;
      pathMessageData.invalidate();
      nidTracksUsed.clear();
    }

    /******************************************************************************
    * RadioMessageInPath::validate
    ******************************************************************************/
    bool RadioMessageInPath::validate()
    {
      trace->write(ATC::briefTrace, "Validating Path");

      bool ret;

      ret = AbstractRadioMessageIn::validate();

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
              if (validateTracks())
              {
                if (validateSpeedChangePosition())
                {
                  dataProcessState = DataValidated;
                  ret = true;
                }
              }
            }
          }
        }
      }
      return ret;
    }

    /******************************************************************************
    * RadioMessageInPath::validateMode
    ******************************************************************************/
    bool RadioMessageInPath::validateMode() const
    {
      //Path message should be accepted in all the modes
      bool modeValid = false;

      const ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
      case ATPModeLocation:
      case ATPModePowerUp:
      case ATPModeConfiguration:
      case ATPModeRegistration:
      case ATPModeBaliseSearch:
      case ATPModeNormal:
      case ATPModeStaffResponsible:
      case ATPModeSplit:
      case ATPModeJoin:
      case ATPModeShuntingRoute:
      case ATPModeUnregistered:
      case ATPModeSafeBrakeToStop:
          modeValid = true;
        break;

      case ATPModePossession:
      case ATPModeShunting:
      case ATPModeSleeping:
      case ATPModeYard:
      case ATPModeSafetyHalt:
      case ATPModePoweringDown:
        break;

      case ATPModeUndefined:
      case ATPModesCount:
      default:
        ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        break;
      }

      traceValidateMode(modeValid);

      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInPath::parseMessageData
    ******************************************************************************/
    bool RadioMessageInPath::parseMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));
      vfwSetReadBuffer(&buffer, sizeof(messageData.message.data));

      // Read & validate NID_MESSAGE_TYPE
      if (vfwGetU8(&buffer) != static_cast<uint8_t>(messageType))
      {
        parseDataValid = false;
        trace->write(ATC::detailedTrace, "NID_MESSAGE_TYPE invalid");
        invalidDataInTCCMessage.setDynamicText("NID_MESSAGE_TYPE");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

      //Read V_SPEED
      pathMessageData.speedAtBeginningOfPath = vfwGetU16(&buffer);

      // Read NID_TRACK
      const uint16_t nidTrack = vfwGetU16(&buffer);
      if (nidTrack == 0U)
      {
        parseDataValid = false;
        setInvalidationReason("NID_TRACK invalid (0)");
        invalidDataInTCCMessage.setDynamicText("NID_TRACK = 0");
        ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
      }

      pathMessageData.pathNextTarget.track = nidTrack;

      // Read D_POSITION
      pathMessageData.pathNextTarget.position = vfwGetU32(&buffer);

      //BlockData
      uint8_t nextMsgIdentifier = vfwGetU8(&buffer);
      bool noOfETARequestReceived = false;
      uint8_t noOfSpeedChangePositionIndex = 0U;

      // Fetch next data-block until M_END_OF_MESSAGE
      while (nextMsgIdentifier != M_END_OF_MESSAGE)
      {
        switch (nextMsgIdentifier)
        {
          case BTypeTracks:
            if (pathMessageData.numberOfTracks < maxPathTracksSize)
            {
              const uint16_t trackNid = vfwGetU16(&buffer);
              if (trackNid == 0U)
              {
                parseDataValid = false;
                setInvalidationReason("NID_TRACK in TRACKS invalid (0)");
                invalidDataInTCCMessage.setDynamicText("Path.TRACK.NID_TRACK");
                ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
              }

              pathMessageData.trackIdList[pathMessageData.numberOfTracks] = trackNid;
              pathMessageData.numberOfTracks++;
            }
            else
            {
              parseDataValid = false;
              trace->write(ATC::detailedTrace, "NID_BLOCK_TYPE of Track invalid");
              invalidDataInTCCMessage.setDynamicText("NID_BLOCK_TYPE");
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
            }
           break;

          case BTypeETARequest:
            if (!noOfETARequestReceived)
            {
              pathMessageData.etaRequestStruct.nextTargetArrivalTime = vfwGetU64(&buffer);

              if (!validateT_CLOCK(pathMessageData.etaRequestStruct.nextTargetArrivalTime))
              {
                parseDataValid = false;
                trace->write(ATC::detailedTrace, "ETA Request invalid in path message");
                invalidDataInTCCMessage.setDynamicText("ETA Request");
                ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
              }
            }
            else
            {
              parseDataValid = false;
              trace->write(ATC::detailedTrace, "Path ETA Request overflow");
              invalidDataInTCCMessage.setDynamicText("Path ETA Request");
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
            }

            noOfETARequestReceived = true;
            break;

          case BTypeSpeedChangePosition:
            if (noOfSpeedChangePositionIndex < maxSpeedChangePositionSize)
            {
              pathMessageData.speedChangePosition[noOfSpeedChangePositionIndex].trackAndPosition.track = vfwGetU16(&buffer);
              pathMessageData.speedChangePosition[noOfSpeedChangePositionIndex].trackAndPosition.position = vfwGetU32(&buffer);
              pathMessageData.speedChangePosition[noOfSpeedChangePositionIndex].newSpeed = vfwGetU16(&buffer);
            }
            else
            {
              parseDataValid = false;
              trace->write(ATC::detailedTrace, "Path Speed Change position overflow");
              invalidDataInTCCMessage.setDynamicText("Path.SPEED_CHANGE");
              ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDataInTCCMessage, __FILE__, __LINE__);
            }

            noOfSpeedChangePositionIndex++;
            pathMessageData.numberOfSpeedChanges = noOfSpeedChangePositionIndex;
            break;

          default:
            parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
            break;
        }

        // Fetch next msg-type (or M_END_OF_MESSAGE)
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
    * RadioMessageInPath::invalidate
    ******************************************************************************/
    void RadioMessageInPath::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      pathMessageData.invalidate();
      dataProcessState = NoDataAvailable;
    }

    /******************************************************************************
    * RadioMessageInPath::getPath
    ******************************************************************************/
    const Path* RadioMessageInPath::getPath() const
    {
      const Path* pathMessage = static_cast<Path*>(NULL);

      if (DataValidated == dataProcessState)
      {
        pathMessage = &pathMessageData;
      }
      return pathMessage;
    }

    /******************************************************************************
    * RadioMessageInPath::validateTracks
    ******************************************************************************/
    bool RadioMessageInPath::validateTracks()
    { 
      bool validateTrackData = false;
      bool validateStatus = true;
      bool isTrackfound = false;

      for (uint16_t i = 0U; i < pathMessageData.numberOfTracks; ++i)
      {
        // Check if the any TRACK NID is duplicated or not?
        const uint16_t trackNidToFind = pathMessageData.trackIdList[i];

        if (!nidTracksUsed.setBit(trackNidToFind))
        {
          trace->write(ATC::detailedTrace, "Track Nid duplicated");
          validateStatus = false;
        }

        //If TrackID is not present in the Tracks block, set the validate data to false
        if (pathMessageData.pathNextTarget.track == trackNidToFind)
        {
          isTrackfound = true;
        }
      }

      if (validateStatus && isTrackfound)
      {
        validateTrackData = true;
      }

      if (!validateTrackData)
      {
        //Check to confirm if the track is not present in the track list
        trace->write(ATC::detailedTrace, "Invalid TRACKS list");
      }

      nidTracksUsed.clear();

      return validateTrackData;
    }

    /******************************************************************************
    * RadioMessageInPath::validateSpeedChangePosition
    ******************************************************************************/
    bool RadioMessageInPath::validateSpeedChangePosition() const
    {  
      uint16_t trackIdSpeedChangePos = 0U;
      bool allSpeedChangePos = true;

      //Parse through the TRACKS block to check the consistency of the NID_TRACK for SPEED_CHANGE_POSITION 
      for (; (trackIdSpeedChangePos < pathMessageData.numberOfSpeedChanges) && allSpeedChangePos; ++trackIdSpeedChangePos)
      {
        bool validateSpeedChangeData = false;

        for (uint16_t i = 0U; ((i < pathMessageData.numberOfTracks) && (!validateSpeedChangeData)); ++i)
        {
          if (pathMessageData.speedChangePosition[trackIdSpeedChangePos].trackAndPosition.track == pathMessageData.trackIdList[i])
          {
            validateSpeedChangeData = true;
          }
        }
        if (!validateSpeedChangeData)
        {
          allSpeedChangePos = false;
          trace->write(ATC::detailedTrace, "NID_TRACK for SPEED_CHANGE_POSITION not found in TRACKS list");
        }
      }

      return allSpeedChangePos;
    }    

  }
}
