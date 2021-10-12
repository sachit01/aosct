/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (TCC->AOS) has an associated parser class inherited from AbstractRadioMessageIn.
* This file implements the parser for the TrainSetup message.
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
#include <cstdio>
#include <algorithm>
#include "atp_types.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_position.hpp"
#include "abstract_tic.hpp"
#include "radio_message_in_train_setup.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_message_handler_event_ids.hpp"
#include "abstract_message_handler.hpp"
#include "brake_calculations.hpp"
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
    * RadioMessageInTrainSetup constructor
    ******************************************************************************/
    RadioMessageInTrainSetup::RadioMessageInTrainSetup() :
      AbstractRadioMessageIn(MTypeTrainSetup),
      tooLowLambdaReceived(ATC::Event::createLogEvent(atpMessageHandlerId,
        ATC::CoreContainer, eventIdInvalidLambdaReceived, DMICom::msgHdlrTooLowLambdaReceived, "Received lambda lower than minimum")),
      trainSetupReceived(false)
    {
      implemented = true;

      trainSetup.nidMsg = 0U;
      trainSetup.trainSetupReason = TrainSetupRegistration;
      trainSetup.state = TrainSetupStateTemporary;
      trainSetup.maxSpeed = 0U;
      trainSetup.trainLength = 0U;
      trainSetup.timsSupervision = TimsSupNotReq;
      trainSetup.trainDirection = 0U;
      trainSetup.maxGradient = 0;
      trainSetup.noOfVehicles = 0U;
      // Setup Fixed Sizes for msg-block vectors.
      trainSetup.trainNameReceived = false;
      trainSetup.vehicleDataVec.reserve(maxVehicleCount);
      trainSetup.vehicleTypeDataVec.reserve(maxVehicleTypesSize);
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::getQSetup
    ******************************************************************************/
    bool RadioMessageInTrainSetup::getQSetup(TrainSetupReason& reason) const
    {
      if (DataValidated == dataProcessState)
      {
        reason = trainSetup.trainSetupReason;
      }

      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::getTrainSetupReceived
    ******************************************************************************/
    bool RadioMessageInTrainSetup::getTrainSetupReceived(uint8_t& id, uint16_t& replyChannelId) const
    {
      id = trainSetup.nidMsg;
      replyChannelId = messageData.channelId;

      return trainSetupReceived;
    }

    /******************************************************************************
    * getTrainSetupMaxGradient
    ******************************************************************************/
    bool RadioMessageInTrainSetup::getTrainSetupMaxGradient(int8_t& maxGradient)
    {
      bool retVal = ((dataProcessState == DataValidated) && trainSetupReceived);
      if (retVal)
      {
        maxGradient = trainSetup.maxGradient;
      }
      return retVal;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::isTrainSetupRejectedByAOS
    ******************************************************************************/
    bool RadioMessageInTrainSetup::isTrainSetupRejectedByAOS() const
    {
      //Train Setup is rejected (received but not validated)
      return ((!(dataProcessState == DataValidated)) && trainSetupReceived);
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::validate
    ******************************************************************************/
    bool RadioMessageInTrainSetup::validate()
    {
      trace->write(ATC::briefTrace, "Validating TrainSetup");

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
              if (validateVehicleTypeData())
              {
                if (validateAndCalculateVehicleData())
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
        }
        // Flag that Train Setup is received and process-state is updated
        trainSetupReceived = true;
      }

      return ret;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::publishData
    ******************************************************************************/
    bool RadioMessageInTrainSetup::publishData()
    {
      bool publishDataValid = false;

      //Parameter to define the train setup will be stored as preliminary or actual
      //Preliminary Train Setup contains the train setup data which is not agreed between TCC and Driver
      //Train setup data will be stored in actual train setup after agreement between TCC and Driver
      bool preliminaryTrainSetup = true;
      const ATPMode currentMode = AbstractModeControl::corePtr()->getCurrentMode();
      if ((ATPModeStaffResponsible == currentMode) ||
        (ATPModeNormal == currentMode))
      {
        const DS::TrainSetup* const pStoredTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();
        if (pStoredTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
        {
          bool otherChecksOK = true;
          if ((currentMode == ATPModeNormal) || (currentMode == ATPModeStaffResponsible))
          {
            otherChecksOK = (pStoredTrainSetup->state == TrainSetupStateTemporary) &&
              (trainSetup.trainLength <= pStoredTrainSetup->length) &&
              (trainSetup.noOfVehicles <= pStoredTrainSetup->vehicleCount);
          }

          const bool orientationOK = (trainSetup.trainDirection == pStoredTrainSetup->orientation);

          bool brakeAbilityOK = true;
          if (!AbstractModeControl::corePtr()->getIdleState())
          {
            brakeAbilityOK = DS::AbstractTSetup::corePtr()->isBrakeDataEqualOrBetter(
              trainSetup.trainDynamicWeightLoaded,
              trainSetup.trainDynamicWeightEmpty,
              &trainSetup.locomotiveBrakeWeightLoadedBrakeSystem[0],
              &trainSetup.locomotiveBrakeWeightEmptyBrakeSystem[0],
              &trainSetup.carsBrakeWeightLoadedBrakeSystem[0],
              &trainSetup.carsBrakeWeightEmptyBrakeSystem[0]);

            if (!brakeAbilityOK)
            {
              setInvalidationReason("Brake data not equal or better!");
            }
          }

          if (otherChecksOK && orientationOK && brakeAbilityOK)
          {
            publishDataValid = true;
            preliminaryTrainSetup = false;
          }
          else if (!orientationOK)
          {
            setInvalidationReason("Train Setup train direction not same as stored orientation.");
          }
          else if (!otherChecksOK)
          {
            setInvalidationReason("Stored train setup not correct. State not temporary, or train length or vehicle count incorrect.");
          }
          else
          {
            setInvalidationReason("Given parameters would not lead to equal or better brake ability than the stored parameters");
          }
        }
      }
      else // ATPModeConfiguration - see validateMode()
      {
        publishDataValid = true;

        const TrainConfigModeState configState = AbstractModeControl::corePtr()->getTrainConfigModeState();

        if (TrainConfigMode::trainConfigWaitSetupFrmTCC == configState)
        {
          if ((TrainSetupReconfiguration == trainSetup.trainSetupReason))
          {
            preliminaryTrainSetup = false;
          }
        }
        //If confirmation is pending with TCC then save the setup in Tsetup
        else if (TrainConfigMode::trainConfigConfirmNewConfigFrmTCC == configState)
        {
          preliminaryTrainSetup = false;
        }
        else
        {
          //do nothing
        }
      }

      if (publishDataValid)
      {
        DS::TrainSetup newTrainSetup;
        DS::TrainSetupChangeDetails changeDetails;

        changeDetails.brakeAbility = false;
        changeDetails.brakeResponseTime = false;
        LocoVsTrainDir oldOrientation = DS::AbstractTSetup::corePtr()->getLocovsTrainOrientation();

        // If vector is empty the number of cars shall be set to 0.
        if (trainSetup.vehicleDataVec.size() != 0U)
        {
          newTrainSetup.vehicleCount = trainSetup.noOfVehicles;
        }
        else
        {
          newTrainSetup.vehicleCount = 0U;
        }

        newTrainSetup.changeDetails = changeDetails; //Change details
        newTrainSetup.length = trainSetup.trainLength; //Train Length
        newTrainSetup.maxSpeed = trainSetup.maxSpeed; //Max Speed
        newTrainSetup.orientation = trainSetup.trainDirection; //locomotive orientation
        newTrainSetup.timsSupNeeded = (TimsSupReq == trainSetup.timsSupervision); //Tims Supervision Needed
        newTrainSetup.state = trainSetup.state; //Q_TS_STATE field

        if (preliminaryTrainSetup)
        {
          if (!DS::AbstractTSetup::corePtr()->setPreliminaryTrainSetup(newTrainSetup))
          {
            publishDataValid = false;
            setInvalidationReason("Failed to set preliminary train setup");
          }
        }
        else
        {
          //Set the train set
          if (!DS::AbstractTSetup::corePtr()->setTrainSetup(newTrainSetup))
          {
            publishDataValid = false;
            setInvalidationReason("Failed to set train setup");
          }
        }

        // Publish VehicleSetup if train setup is valid
        if (publishDataValid)
        {
          if (trainSetup.trainNameReceived)
          {
            if (!DS::AbstractTSetup::corePtr()->setTrainName(&(trainSetup.trainName.trainName[0])))
            {
              publishDataValid = false;
              setInvalidationReason("Failed to set train name");
            }
          }

          uint16_t i = 0U;
          // VehicleSetup
          for (uint16_t vehDataindex = 0U; (vehDataindex < trainSetup.vehicleDataVec.size()) && publishDataValid; ++vehDataindex)
          {
            const VehicleData& carVehicleData = trainSetup.vehicleDataVec[vehDataindex];
            for (uint16_t vehicleIndex = 0U; (vehicleIndex < carVehicleData.noOfVeh) && publishDataValid; ++vehicleIndex)
            {
              if (i < maxVehicleCount)
              {
                DS::VehicleSetup newVehicleSetup;
                newVehicleSetup.nodeAdress = carVehicleData.vehicleNodeAddress;
                newVehicleSetup.vehicleType = carVehicleData.vehicleType;
                static_cast<void>(vfw_strlcpy(&newVehicleSetup.vehicleName[0], &carVehicleData.vehicleName[0],
                  sizeof(newVehicleSetup.vehicleName)));
                //TODO car weight need to be taken care as current implementation is in adaptation

                if (preliminaryTrainSetup)
                {
                  if (!DS::AbstractTSetup::corePtr()->setPreliminaryVehicleSetup(i, newVehicleSetup))
                  {
                    publishDataValid = false;
                    setInvalidationReason("Failed to set preliminary vehicle setup");
                  }
                }
                else
                {
                  if (!DS::AbstractTSetup::corePtr()->setVehicleSetup(i, newVehicleSetup))
                  {
                    publishDataValid = false;
                    setInvalidationReason("Failed to set vehicle setup");
                  }
                }

                ++i;
              }
              else
              {
                publishDataValid = false;
                setInvalidationReason("Too many vehicles!");
              }
            }

          }

          if (publishDataValid)
          {
              DS::AbstractTSetup::corePtr()->setBrakeData(
                  trainSetup.trainDynamicWeightLoaded,
                  trainSetup.trainDynamicWeightEmpty,
                  &trainSetup.locomotiveBrakeWeightLoadedBrakeSystem[0],
                  &trainSetup.locomotiveBrakeWeightEmptyBrakeSystem[0],
                  &trainSetup.carsBrakeWeightLoadedBrakeSystem[0],
                  &trainSetup.carsBrakeWeightEmptyBrakeSystem[0]);
          }

          //Flip the stored tracks if the orientation is flipped. This can happen only when position is known.
          const Pos::PosAccuracyState positionAccuracy = Pos::AbstractPosition::corePtr()->getAccuracyState();

          //orientation is updated when the train setup is published
          const LocoVsTrainDir newOrientation = DS::AbstractTSetup::corePtr()->getLocovsTrainOrientation();

          if((oldOrientation != newOrientation) && (positionAccuracy == Pos::PosKnown))
          {
            DS::AbstractTracks::corePtr()->flipTrackOdoDir();
          }

        }
      }

      tracePublishData(publishDataValid);

      return publishDataValid;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::validateMode
    ******************************************************************************/
    bool RadioMessageInTrainSetup::validateMode()
    {
      bool modeValid = true;

      ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
      switch (mode)
      {
        case ATPModeConfiguration:
        {
          const TrainConfigModeState configState = AbstractModeControl::corePtr()->getTrainConfigModeState();

          if ((TrainConfigMode::trainConfigConfirmNewConfigFrmTCC == configState) ||
            ((TrainConfigMode::trainConfigWaitSetupFrmTCC == configState) &&
            ((TrainSetupReposition == trainSetup.trainSetupReason) || (TrainSetupReregistration == trainSetup.trainSetupReason))))
          {
            modeValid = validateModeConfiguration();
          }
          else
          {
            modeValid = false;
            setInvalidationReason("RadioMessageInTrainSetup, train setup received in invalid config mode state");
          }

          //The AOS shall reject the TrainSetup message from TCC if
          //Locomotive orientation is not the same as sent in the StartUpMessage message to TCC.
          DS::TrainSetup  startUpMessage;
          const bool getPreliminaryTrain = DS::AbstractTSetup::corePtr()->getPreliminaryTrainSetup(startUpMessage);
          if (getPreliminaryTrain && (TrainConfigMode::trainConfigConfirmNewConfigFrmTCC == configState) &&
              (trainSetup.trainDirection != startUpMessage.orientation))
          {
            modeValid = false;
            setInvalidationReason("RadioMessageInTrainSetup, train direction not same as stored orientation");
          }
          break;
        }

        case ATPModeNormal:
        case ATPModeStaffResponsible:
        {
          if (TrainSetupStatePermanent != trainSetup.state)
          {
            modeValid = false;
            setInvalidationReason("RadioMessageInTrainSetup, train setup state not permanent");
          }
          break;
        }

        case ATPModeSleeping:
          //Reject the incoming message from TCC 
          modeValid = false;
          invalidIncmgMsgTCC.setDynamicText("Train Setup");
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncmgMsgTCC, __FILE__, __LINE__);
          setInvalidationReason("RadioMessageInTrainSetup, mode cannot be sleeping");
          break;

        case ATPModeShuntingRoute:
        case ATPModeYard:
        case ATPModeShunting:
        case ATPModePossession:
        case ATPModeLocation:
        case ATPModeUnregistered:
        case ATPModeSafetyHalt:
        case ATPModePoweringDown:
        case ATPModeBaliseSearch:
        case ATPModeSplit:
        case ATPModeJoin:
        case ATPModeSafeBrakeToStop:
        case ATPModePowerUp:
        case ATPModeRegistration:
          modeValid = false;
          setInvalidationReason("RadioMessageInTrainSetup, incorrect mode.");
          break;
        case ATPModesCount:
        case ATPModeUndefined:
        default:
          modeValid = false;
          ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");

      }

      traceValidateMode(modeValid);
      return modeValid;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::validateMode
    ******************************************************************************/
    bool RadioMessageInTrainSetup::validateModeConfiguration()
    {
      bool modeValid = true;
      const Pos::PosAccuracyState positionAccuracy = Pos::AbstractPosition::corePtr()->getAccuracyState();
      
      switch (trainSetup.trainSetupReason)
      {
        case TrainSetupRegistration:
        {
          if (positionAccuracy != Pos::PosUnknown)
          {
            modeValid = false;
            setInvalidationReason("RadioMessageInTrainSetup, position not unknown!");
          }
          break;
        }
        case TrainSetupReregistration:
        {
          const bool newConfig = AbstractModeControl::corePtr()->isNewTrainConfiguration();

          if (newConfig || (positionAccuracy != Pos::PosUnknown)) 
          {
            modeValid = false;
            setInvalidationReason("RadioMessageInTrainSetup, Reregistration rejected for new config or position unknown");
          }
          break;
        }
        case TrainSetupReposition:
        {
          if (positionAccuracy != Pos::PosUnknown)
          {
            modeValid = false;
            setInvalidationReason("RadioMessageInTrainSetup, TrainSetupReposition rejected as position not unknown!");
          }
          break;
        }
        case TrainSetupReconfiguration:
          if (positionAccuracy != Pos::PosKnown)
          {
            modeValid = false;
            setInvalidationReason("RadioMessageInTrainSetup, position not known!");
          }
          break;

        default:
          modeValid = false;
          setInvalidationReason("RadioMessageInTrainSetup, unknown train setup reason");
          break;
      }

      return modeValid;
    }

    /******************************************************************************
    * validateL_TRAIN
    ******************************************************************************/
    bool RadioMessageInTrainSetup::validateL_TRAIN(const uint32_t lTrain) const
    {
      bool lengthValid = false;
      const uint32_t lengthOfLoco =
        static_cast<uint32_t>(AbstractConfig::corePtr()->getBalAntennaPosEnd()) +
        static_cast<uint32_t>(AbstractConfig::corePtr()->getBalAntennaPosFront());

      // L_LENGTH (cm) must at least be as long as the locomotive
      if (lTrain >= lengthOfLoco)
      {
        lengthValid = true;
      }

      return lengthValid;
    }


    /******************************************************************************
    * RadioMessageInTrainSetup::parseMessageData
    ******************************************************************************/
    bool RadioMessageInTrainSetup::parseMessageData()
    {
      bool parseDataValid = true;
      uint8_t tmpValU8;
      uint32_t tmpValU32;

      bool parseAdditionalDataValid = false;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));
      vfwSetReadBuffer(&buffer, sizeof(messageData.message.data));

      // Read & validate NID_MESSAGE_TYPE
      if (vfwGetU8(&buffer) != static_cast<uint8_t>(messageType))
      {
        parseDataValid = false;
        setInvalidationReason("NID_MESSAGE_TYPE invalid");
      }

      // Read NID_MSG
      trainSetup.nidMsg = vfwGetU8(&buffer);

      // Read & validate Q_SETUP
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateQ_SETUP(tmpValU8))
      {
        parseDataValid = false;
        setInvalidationReason("Q_SETUP invalid");
      }
      else
      {
        trainSetup.trainSetupReason = static_cast<TrainSetupReason>(tmpValU8);
      }

      // Read & validate Q_TS_STATE
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateQ_TS_STATE(tmpValU8))
      {
        parseDataValid = false;
        setInvalidationReason("Q_TS_STATE invalid");
      }
      else
      {
        trainSetup.state = static_cast<TrainSetupState>(tmpValU8);
      }

      // Read V_SPEED
      trainSetup.maxSpeed = vfwGetU16(&buffer);

      // Read L_TRAIN, 24-bit value
      tmpValU32 = 0U;
      tmpValU32 += static_cast<uint32_t>(vfwGetU8(&buffer)) << 16U;
      tmpValU32 += static_cast<uint32_t>(vfwGetU8(&buffer)) << 8U;
      tmpValU32 += static_cast<uint32_t>(vfwGetU8(&buffer));

      if (!validateL_TRAIN(tmpValU32))
      {
        parseDataValid = false;
        setInvalidationReason("L_TRAIN invalid");
      }
      else
      {
        trainSetup.trainLength = tmpValU32;
      }


      // Read & validate Q_TIMS_SUPERVISION
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateQ_TIMS_SUPERVISION(tmpValU8))
      {
        parseDataValid = false;
        setInvalidationReason("Q_TIMS_SUPERVISION invalid");
      }
      else
      {
        trainSetup.timsSupervision = static_cast<TimsSupStatus>(tmpValU8);
      }

      // Read & validate B_DIRECTION
      const uint8_t tempVal = vfwGetU8(&buffer);
      if (!validateB_DIRECTION(tempVal))
      {
        parseDataValid = false;
        setInvalidationReason("B_DIRECTION invalid");
      }
      else
      {
        trainSetup.trainDirection = (tempVal & trainLocoOrientation);
      }

      // G_GRADIENT
      trainSetup.maxGradient = vfwGetI8(&buffer);

      uint8_t nextMsgIdentifier = vfwGetU8(&buffer);

      if (parseDataValid)
      { // Only parse blocks if parsing Ok so far
        // In order to not over-write Invalidation reason

        // BlockData
        // Fetch next data-block until M_END_OF_MESSAGE
        while ((nextMsgIdentifier != M_END_OF_MESSAGE) && (parseDataValid))
        {
          switch (nextMsgIdentifier)
          {
          case BTypeTrainName:
            parseDataValid = parseTypeTrainName(buffer);
            break;

          case BTypeVehicleTypeData:
            parseDataValid = parseTypeVehicleTypeData(buffer);
            break;

          case BTypeVehicleData:
            parseDataValid = parseTypeVehicleData(buffer);
            break;

          case BTypeTrackData:
            parseDataValid = parseTypeTrackData(buffer);
            break;

          default:
            parseAdditionalDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
            if (!parseAdditionalDataValid)
            {
              setInvalidationReason("Adaptation block failed!");
            }
            break;
          }

          // Fetch next msg-type (or M_END_OF_MESSAGE)
          nextMsgIdentifier = vfwGetU8(&buffer);
        }

        const bool typeDataVecIsEmpty = trainSetup.vehicleTypeDataVec.empty();
        const bool vehicleDataVecIsEmpty = trainSetup.vehicleDataVec.empty();

        if (typeDataVecIsEmpty || vehicleDataVecIsEmpty)
        {
          parseDataValid = false;
          if (typeDataVecIsEmpty)
          {
            setInvalidationReason("typeDataVecIsEmpty empty");
          }
          else
          {
            setInvalidationReason("vehicleDataVecIsEmpty empty");
          }
        }
      }

      if ((!validateSizeOfParsedBytes(&buffer)) && (parseDataValid))
      {
        parseDataValid = false;
        setInvalidationReason("Message size incorrect");
      }

      // Log according to trace-level
      detailedLog();
      veryDetailedLog();

      traceParseData(parseDataValid);
      usedVehicleIdentities.clear();
      
      return (parseDataValid && parseAdditionalDataValid);
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::parseTypeTrainName
    ******************************************************************************/
    bool RadioMessageInTrainSetup::parseTypeTrainName(VFW_Buffer& buffer)
    {
      bool parseDataValid = true;
      TrainName trainName;

      memset(&trainName.trainName[0], 0, sizeof(trainName.trainName));
      for (uint8_t i = 0U; i < trainNameMaxLength; i++)
      {
        trainName.trainName[i] = static_cast<char_t>(vfwGetI8(&buffer));
      }

      if (!trainSetup.trainNameReceived)
      {
        trainSetup.trainNameReceived = true;
        trainSetup.trainName = trainName;
      }
      else
      {
        parseDataValid = false;
        setInvalidationReason("BTypeTrainName overflow");
      }

      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::parseTypeVehicleTypeData
    ******************************************************************************/
    bool RadioMessageInTrainSetup::parseTypeVehicleTypeData(VFW_Buffer& buffer)
    {
      VehicleTypeData vehicleTypeData;
      bool parseDataValid = true;

      const uint8_t vehicleType = vfwGetU8(&buffer);
      if (!validateNID_VEHICLE_TYPE(vehicleType))
      {
        parseDataValid = false;
        setInvalidationReason("NID_VEHICLE_TYPE invalid");
      }
      else
      {
        vehicleTypeData.vehicleType = vehicleType;
      }

      //Read W_WEIGHT
      vehicleTypeData.dynamicWeightLoaded = vfwGetU16(&buffer);
      vehicleTypeData.dynamicWeightEmpty = vfwGetU16(&buffer);
      vehicleTypeData.brakeWeightLoadedBrakeSystem1 = vfwGetU16(&buffer);
      vehicleTypeData.brakeWeightEmptyBrakeSystem1 = vfwGetU16(&buffer);
      vehicleTypeData.brakeWeightLoadedBrakeSystem2 = vfwGetU16(&buffer);
      vehicleTypeData.brakeWeightEmptyBrakeSystem2 = vfwGetU16(&buffer);
      vehicleTypeData.brakeWeightLoadedBrakeSystem3 = vfwGetU16(&buffer);
      vehicleTypeData.brakeWeightEmptyBrakeSystem3 = vfwGetU16(&buffer);

      if ((vehicleTypeData.dynamicWeightLoaded == 0U) ||
        (vehicleTypeData.dynamicWeightEmpty == 0U))
      {
        parseDataValid = false;
        setInvalidationReason("Dynamic weight 0");
      }

      if ((trainSetup.vehicleTypeDataVec.size() < maxVehicleTypesSize) && parseDataValid) // Max cars including locomotive
      {
        trainSetup.vehicleTypeDataVec.push_back(vehicleTypeData);
      }
      else
      {
        parseDataValid = false;
        setInvalidationReason("BTypeVehicleTypeData overflow");
      }

      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::parseTypeVehicleData
    ******************************************************************************/
    bool RadioMessageInTrainSetup::parseTypeVehicleData(VFW_Buffer& buffer)
    {
      bool parseDataValid = true;
      VehicleData vehicleData;
      //Get the N_Value
      uint16_t tmpValU16 = vfwGetU16(&buffer);
      if (!validateN_VALUE(tmpValU16))
      {
        parseDataValid = false;
        setInvalidationReason("N_VALUE invalid");
      }
      else
      {
        vehicleData.noOfVeh = tmpValU16;
      }

      //Vehicle type
      const uint8_t vehicleType = vfwGetU8(&buffer);
      if (!validateNID_VEHICLE_TYPE(vehicleType))
      {
        parseDataValid = false;
        setInvalidationReason("NID_VEHICLE_TYPE invalid");
      }
      else
      {
        vehicleData.vehicleType = vehicleType;
      }

      vehicleData.vehicleNodeAddress = vfwGetU16(&buffer);

      memset(&vehicleData.vehicleName[0], 0, sizeof(vehicleData.vehicleName));
      for (uint8_t i = 0U; i < vehicleNameMaxLength; i++)
      {
        vehicleData.vehicleName[i] = static_cast<char_t>(vfwGetI8(&buffer));
      }

      if ((trainSetup.vehicleDataVec.size() <= maxVehicleCount) && parseDataValid &&
        (trainSetup.noOfVehicles < maxVehicleCount))
      {
        parseDataValid = isVehicleDataValid(vehicleData);
        trainSetup.noOfVehicles += vehicleData.noOfVeh;
        trainSetup.vehicleDataVec.push_back(vehicleData);
      }
      else
      {
        parseDataValid = false;
        setInvalidationReason("BTypeVehicleIdData overflow");
      }

      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::parseTypeTrackData
    ******************************************************************************/
    bool RadioMessageInTrainSetup::parseTypeTrackData(VFW_Buffer& buffer)
    {
      bool parseDataValid = true;

      if (trainSetup.trainSetupReason == TrainSetupReconfiguration)
      {
        TrackData trackData;
        readTRACK_DATA(buffer, trackData);

        if (!validateB_DIRECTION(trackData.bdirection))
        {
          trace->write(ATC::detailedTrace, "B_DIRECTION invalid");
          parseDataValid = false;
        }

        // TODO store trackData
      }
      else
      {
        parseDataValid = false;
        setInvalidationReason("TRACK_DATA is only allowed for Reconfiguration");
      }

      return parseDataValid;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::isVehicleDataValid
    ******************************************************************************/
    bool RadioMessageInTrainSetup::isVehicleDataValid(const VehicleData& vehData)
    {
      bool retFlag = true;
      if (vehData.noOfVeh == 1U)
      {
        if (vehData.vehicleNodeAddress > 0U)
        {
          // Check if NID_VEHICLE is unique
          retFlag = usedVehicleIdentities.setBit(vehData.vehicleNodeAddress);
        }
        else
        {   // Allow node address 0 (not an identifiable vehicle)
          retFlag = true;
        }
      }
      else if (vehData.noOfVeh > 1U)
      {
        // Value shall be set to zero if N_VALUE is >1.
        if (vehData.vehicleNodeAddress != 0U)
        {
          retFlag = false;
          setInvalidationReason("NID_VEHICLE, value shall be set to zero if N_VALUE is >1.");
        }
      }
      else
      {
        // Do nothing
      }
      return retFlag;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::validateAndCalculateVehicleData
    ******************************************************************************/
    bool RadioMessageInTrainSetup::validateAndCalculateVehicleData()
    {
      bool validVehData = true;

      // Array to see if vehicle data is tested already
      const VehicleTypeData* vehicleTypeIdTested[256];

      memset(&vehicleTypeIdTested[0], 0, sizeof(vehicleTypeIdTested));

      const std::vector<VehicleTypeData>::iterator  endVehicleTypeItr = trainSetup.vehicleTypeDataVec.end();
      // Assign the iterator to beginning of vector of vehicle data
      std::vector<VehicleData>::iterator itr = trainSetup.vehicleDataVec.begin();

      if (itr != trainSetup.vehicleDataVec.end())
      {
        // check if there is a Locomotive in starting of Vehicle Data.
        const uint8_t vehicleType = itr->vehicleType;
        if (!((vehicleType >= vehicleLocomotivesMin) && (vehicleType <= vehicleLocomotivesMax)))
        {
          validVehData = false;
          setInvalidationReason("Vehicle Data does not start with loco");
        }
      }
      else
      {
        validVehData = false;
        setInvalidationReason("No Vehicle Data!");
      }

      // Iterate over the vector of vehicle Data
      for (; (itr != trainSetup.vehicleDataVec.end()) && validVehData; ++itr)
      {
        const VehicleData& vehicle = *itr;
        // Check if the vehicle type of vehicle data present in vehicle type field
        const uint8_t newVehicleTypeId = vehicle.vehicleType;
        const VehicleTypeData* currentVehicleTypeData = vehicleTypeIdTested[newVehicleTypeId];

        if (currentVehicleTypeData == static_cast<VehicleTypeData*>(NULL))
        {
          const std::vector<VehicleTypeData>::iterator currentVehicleTypeIter = find_if(
            trainSetup.vehicleTypeDataVec.begin(), endVehicleTypeItr, VehTypeComp(newVehicleTypeId));

          if (currentVehicleTypeIter == endVehicleTypeItr)
          {
            // No item found
            validVehData = false;
            setInvalidationReason("Vehicle Type not found!");
          }
          else if (count_if(currentVehicleTypeIter, endVehicleTypeItr, VehTypeComp(newVehicleTypeId)) != 1)
          {
            // We have duplicates
            validVehData = false;
            setInvalidationReason("Vehicle Type duplicated!");
          }
          else
          {
            // Found the vehicle type, save it...
            currentVehicleTypeData = &(*currentVehicleTypeIter);
            vehicleTypeIdTested[newVehicleTypeId] = currentVehicleTypeData;
          }
        }

        calculateBrakeParameters(currentVehicleTypeData, vehicle.noOfVeh);
      }


      if (validVehData)
      {
        validVehData = DS::AbstractTSetup::corePtr()->validateLambda(
          trainSetup.trainDynamicWeightLoaded,
          trainSetup.trainDynamicWeightEmpty,
          &trainSetup.locomotiveBrakeWeightLoadedBrakeSystem[0],
          &trainSetup.locomotiveBrakeWeightEmptyBrakeSystem[0],
          &trainSetup.carsBrakeWeightLoadedBrakeSystem[0],
          &trainSetup.carsBrakeWeightEmptyBrakeSystem[0]);

        if (!validVehData)
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(tooLowLambdaReceived, __FILE__, __LINE__);
          setInvalidationReason("Message rejected. Received lambda lower than minimum!");
        }
      }

      return validVehData;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::validateVehicleTypeData
    ******************************************************************************/
    bool RadioMessageInTrainSetup::validateVehicleTypeData()
    {
      bool validVehTypeData = true;
      //Assign the iterator to beginning of vector of vehicle type data
      std::vector<VehicleTypeData>::const_iterator itr = trainSetup.vehicleTypeDataVec.begin();
      //Iterate over the vector of vehicle type Data
      for (; (itr != trainSetup.vehicleTypeDataVec.end()) && validVehTypeData; ++itr)
      {//Check for duplicates
        if (count_if(trainSetup.vehicleTypeDataVec.begin(), trainSetup.vehicleTypeDataVec.end(), VehTypeComp(itr->vehicleType)) != 1)
        {
          validVehTypeData = false;
          setInvalidationReason("Vehicle Type data duplicated");
        }
      }

      return validVehTypeData;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::invalidate
    ******************************************************************************/
    void RadioMessageInTrainSetup::invalidate()
    {
      AbstractRadioMessageIn::invalidate();
      trainSetup.nidMsg = 0U;
      trainSetup.state = TrainSetupStateTemporary;
      trainSetup.trainSetupReason = TrainSetupRegistration;
      trainSetup.maxSpeed = 0U;
      trainSetup.trainLength = 0U;
      trainSetup.timsSupervision = TimsSupNotReq;
      trainSetup.trainDirection = 0U;
      trainSetup.maxGradient = 0;
      trainSetup.noOfVehicles = 0U;
      // Clear all data-block vectors
      trainSetup.vehicleDataVec.clear();
      trainSetup.trainNameReceived = false;
      trainSetup.vehicleTypeDataVec.clear();

      trainSetup.trainDynamicWeightLoaded = 0;
      trainSetup.trainDynamicWeightEmpty = 0;

      for (uint8_t i = 0U; i < 3U; ++i)
      {
        trainSetup.locomotiveBrakeWeightLoadedBrakeSystem[i] = 0;
        trainSetup.locomotiveBrakeWeightEmptyBrakeSystem[i] = 0;
        trainSetup.carsBrakeWeightLoadedBrakeSystem[i] = 0;
        trainSetup.carsBrakeWeightEmptyBrakeSystem[i] = 0;
      }

      trainSetup.numberOfBrakeSystemsInUse = 0U;

      dataProcessState = NoDataAvailable;
      trainSetupReceived = false;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::parseAdditionalBlocks
    ******************************************************************************/
    bool RadioMessageInTrainSetup::parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType)
    {
      //Writing the below trace for removing warning
      trace->write(ATC::detailedTrace, "No Adaptation block included in command message", static_cast<uint32_t>(adapBlockType));
      trace->write(ATC::detailedTrace, "Size of Buffer passed", buffer->b_s);
      return false;
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::detailedLog
    ******************************************************************************/
    void RadioMessageInTrainSetup::detailedLog(void) const
    {
      uint8_t currentLevel;
      bool isEnabled;

      trace->getTraceDetails(currentLevel, isEnabled);

      if (isEnabled && (currentLevel >= ATC::detailedMessageTrace))
      {   // No reason to assemble logStr if trace not enabled
        char_t logStr[160];

        //lint -e{586} snprintf is needed here
        const int32_t res = snprintf(&logStr[0], sizeof(logStr),
          "NID_MSG=%u, Q_SETUP=%u, Q_TS_STATE=%u, V_SPEED=%d, L_TRAIN=%u, Q_TIMS_SUPERVISION=%u, B_DIRECTION=%X, G_GRADIENT=%d",
          trainSetup.nidMsg,
          trainSetup.trainSetupReason,
          trainSetup.state,
          trainSetup.maxSpeed,
          trainSetup.trainLength,
          trainSetup.timsSupervision,
          trainSetup.trainDirection,
          trainSetup.maxGradient);

        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
        {
          traceLog(ATC::detailedMessageTrace, ATC::DetailedLog, &logStr[0]);
        }
      }
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::veryDetailedLog
    ******************************************************************************/
    void RadioMessageInTrainSetup::veryDetailedLog(void) const
    {
      uint8_t currentLevel;
      bool isEnabled;

      trace->getTraceDetails(currentLevel, isEnabled);

      if (isEnabled && (currentLevel >= ATC::veryDetailedMessageTrace))
      {
        // No reason to assemble logStr if trace not enabled

        // TRAIN_NAME
        if (trainSetup.trainNameReceived)
        {
          char_t logStr[120];
          //lint -e{586} snprintf is needed here
          const int32_t res = snprintf(&logStr[0], sizeof(logStr), "TRAIN_NAME: %s", trainSetup.trainName.trainName);

          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
          }
        }
        else
        {
          traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, "TRAIN_NAME:-");
        }

        // VEHICLE_TYPE_DATA
        if (trainSetup.vehicleTypeDataVec.size() > 0U)
        {
          char_t logStr[120];

          for (std::vector<VehicleTypeData>::const_iterator vehicleTypeDataIt = trainSetup.vehicleTypeDataVec.begin();
            vehicleTypeDataIt != trainSetup.vehicleTypeDataVec.end(); ++vehicleTypeDataIt)
          {
            //lint -e{586} snprintf is needed here
            int32_t res = snprintf(&logStr[0], sizeof(logStr),
              "VEHICLE_TYPE_DATA: NID_VEHICLE_TYPE=%u, "
              "W_WEIGHT=%u, W_WEIGHT=%u, W_WEIGHT=%u, W_WEIGHT=%u",
              static_cast<uint32_t>(vehicleTypeDataIt->vehicleType),
              static_cast<uint32_t>(vehicleTypeDataIt->dynamicWeightLoaded),
              static_cast<uint32_t>(vehicleTypeDataIt->dynamicWeightEmpty),
              static_cast<uint32_t>(vehicleTypeDataIt->brakeWeightLoadedBrakeSystem1),
              static_cast<uint32_t>(vehicleTypeDataIt->brakeWeightEmptyBrakeSystem1));

            if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
            {
              traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
            }

            //lint -e{586} snprintf is needed here
            res = snprintf(&logStr[0], sizeof(logStr),
              "VEHICLE_TYPE_DATA (cont):  W_WEIGHT=%u, W_WEIGHT=%u, W_WEIGHT=%u, W_WEIGHT=%u",
              static_cast<uint32_t>(vehicleTypeDataIt->brakeWeightLoadedBrakeSystem2),
              static_cast<uint32_t>(vehicleTypeDataIt->brakeWeightEmptyBrakeSystem2),
              static_cast<uint32_t>(vehicleTypeDataIt->brakeWeightLoadedBrakeSystem3),
              static_cast<uint32_t>(vehicleTypeDataIt->brakeWeightEmptyBrakeSystem3));
              
            if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
            {
              traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
            }
          }
        }
        else
        {
          traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, "VEHICLE_TYPE_DATA:-");
        }

        // VEHICLE_DATA
        if (trainSetup.vehicleDataVec.size() > 0U)
        {
          char_t logStr[120];

          for (std::vector<VehicleData>::const_iterator vehicleDataIt = trainSetup.vehicleDataVec.begin();
            vehicleDataIt != trainSetup.vehicleDataVec.end(); ++vehicleDataIt)
          {
            //lint -e{586} snprintf is needed here
            const int32_t res = snprintf(&logStr[0], sizeof(logStr),
              "VEHICLE_DATA: N_VALUE=%u, NID_VEHICLE_TYPE=%u, NID_VEHICLE=%u, TID_VEHICLE_NAME=%s",
              static_cast<uint32_t>(vehicleDataIt->noOfVeh),
              static_cast<uint32_t>(vehicleDataIt->vehicleType),
              static_cast<uint32_t>(vehicleDataIt->vehicleNodeAddress),
              vehicleDataIt->vehicleName);

            if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(logStr)))
            {
              traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, &logStr[0]);
            }
          }
        }
        else
        {
          traceLog(ATC::veryDetailedMessageTrace, ATC::DetailedLog, "VEHICLE_DATA:-");
        }
      }
    }

    /******************************************************************************
    * RadioMessageInTrainSetup::calculateBrakeParameters
    ******************************************************************************/
    void RadioMessageInTrainSetup::calculateBrakeParameters(const VehicleTypeData * const currentVehicleTypeData, const uint16_t numOfVeh)
    {
      if (currentVehicleTypeData != static_cast<VehicleTypeData*>(NULL))
      {
        const int32_t numberOfVehicles = static_cast<int32_t>(numOfVeh);
        const uint8_t newVehicleTypeId = currentVehicleTypeData->vehicleType;

        trainSetup.trainDynamicWeightLoaded += (static_cast<int32_t>(currentVehicleTypeData->dynamicWeightLoaded) * numberOfVehicles);
        trainSetup.trainDynamicWeightEmpty += (static_cast<int32_t>(currentVehicleTypeData->dynamicWeightEmpty) * numberOfVehicles);

        // Is the vehicle type a locomotive?
        if ((newVehicleTypeId >= vehicleLocomotivesMin) && (newVehicleTypeId <= vehicleLocomotivesMax))
        {
          trainSetup.locomotiveBrakeWeightLoadedBrakeSystem[0U] += 
            (static_cast<int32_t>(currentVehicleTypeData->brakeWeightLoadedBrakeSystem1) * numberOfVehicles);
          trainSetup.locomotiveBrakeWeightEmptyBrakeSystem[0U] += 
            (static_cast<int32_t>(currentVehicleTypeData->brakeWeightEmptyBrakeSystem1) * numberOfVehicles);
          trainSetup.locomotiveBrakeWeightLoadedBrakeSystem[1U] += 
            (static_cast<int32_t>(currentVehicleTypeData->brakeWeightLoadedBrakeSystem2) * numberOfVehicles);
          trainSetup.locomotiveBrakeWeightEmptyBrakeSystem[1U] +=
            (static_cast<int32_t>(currentVehicleTypeData->brakeWeightEmptyBrakeSystem2) * numberOfVehicles);
          trainSetup.locomotiveBrakeWeightLoadedBrakeSystem[2U] +=
            (static_cast<int32_t>(currentVehicleTypeData->brakeWeightLoadedBrakeSystem3) * numberOfVehicles);
          trainSetup.locomotiveBrakeWeightEmptyBrakeSystem[2U] +=
            (static_cast<int32_t>(currentVehicleTypeData->brakeWeightEmptyBrakeSystem3) * numberOfVehicles);
        }
        else
        {
          trainSetup.carsBrakeWeightLoadedBrakeSystem[0U] +=
            (static_cast<int32_t>(currentVehicleTypeData->brakeWeightLoadedBrakeSystem1) * numberOfVehicles);
          trainSetup.carsBrakeWeightEmptyBrakeSystem[0U] +=
            (static_cast<int32_t>(currentVehicleTypeData->brakeWeightEmptyBrakeSystem1) * numberOfVehicles);
          trainSetup.carsBrakeWeightLoadedBrakeSystem[1U] +=
            (static_cast<int32_t>(currentVehicleTypeData->brakeWeightLoadedBrakeSystem2) * numberOfVehicles);
          trainSetup.carsBrakeWeightEmptyBrakeSystem[1U] +=
            (static_cast<int32_t>(currentVehicleTypeData->brakeWeightEmptyBrakeSystem2) * numberOfVehicles);
          trainSetup.carsBrakeWeightLoadedBrakeSystem[2U] +=
            (static_cast<int32_t>(currentVehicleTypeData->brakeWeightLoadedBrakeSystem3) * numberOfVehicles);
          trainSetup.carsBrakeWeightEmptyBrakeSystem[2U] +=
            (static_cast<int32_t>(currentVehicleTypeData->brakeWeightEmptyBrakeSystem3) * numberOfVehicles);
        }
      }
    }
  } // namespace Kernel
} // namespace ATP
