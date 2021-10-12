/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->DMI) has an associated creator class inherited from AbstractDMIMessageOut.
* This file implements the creator for the outgoing Ceiling Speed List DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-21    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_ceiling_speed_list.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_targets.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tsetup.hpp"
#include "atc_math.hpp"

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
  namespace DMICom
  {
    /******************************************************************************
    * DMIMessageOutCeilingSpeedList constructor
    ******************************************************************************/
    DMIMessageOutCeilingSpeedList::DMIMessageOutCeilingSpeedList() : AbstractDMIMessageOut(MTypeCelingSpeedList)
    {
      noOfSpeedDatablocks = 0U;
      prevTravelDir = DirUndefined;
      memset(&speedList[0], 0, sizeof(speedList));
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutCeilingSpeedList::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message:Ceiling Speed List");

        if (assembleDMIMessageData())
        {
          dmiDataProcessState = DMIDataValidated;
        }
      }

      return(DMIDataValidated == dmiDataProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageOutCeilingSpeedList::invalidate()
    {
      //Clear all data
      noOfSpeedDatablocks = 0U;
      memset(&speedList[0], 0, sizeof(speedList));
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutCeilingSpeedList::collectData()
    {
      bool travelDirChangedInLocation = false;
      noOfSpeedDatablocks = 0U;
      uint16_t startupStatus;
      const bool dmiStartupStatus = AbstractDMIHandler::corePtr()->getDMIStartupStatus(startupStatus);
      const bool anyTargetDeleted = DS::AbstractTargets::corePtr()->isTargetDelInList();
      const TravelDir travelDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
      const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      uint32_t currentCeilingSpeed = 0U;

      // Need to send an updated list of ceiling speeds to DMI when changing travel direction.
      // Travel direction should change only at standstill and prevTravelDir == Undefined at first standstill
      if ((ATPModeLocation == currentMode) && (travelDir != prevTravelDir) && isStandStill)
      {
        travelDirChangedInLocation = true;
        prevTravelDir = travelDir;
      }

      //reset prev travel direction if Mode is not location.
      if(ATPModeLocation != currentMode)
      {
        prevTravelDir = DirUndefined;
      }

      if (DS::AbstractTargets::corePtr()->getTargetListChanged() || dmiStartupStatus || anyTargetDeleted || travelDirChangedInLocation)
      {
        // Get the max allowed speed for this train
        const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();
        uint32_t maxCeilingSpeed = 0U; // cm/s
        if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
        {
          maxCeilingSpeed = pTrainSetup->maxSpeed;
        }
        // Max speed to be sent to DMI
        // Ceiling speed in supervised targets are already limited by Supervise
        uint32_t maxSpeedKmH = ATC::ATCMath::convCmpsToKmph(maxCeilingSpeed);

        //Create the Static Speed Profile 
        //Converting Odometer Position from centimeter to meter 
        speedList[noOfSpeedDatablocks].odoPosition = 0;
        currentCeilingSpeed = DS::AbstractTargets::corePtr()->getCurCeilingSpeed();
        //Converting New Speed from cm/s to km/h
        speedList[noOfSpeedDatablocks].newSpeed = static_cast<uint8_t>(ATC::ATCMath::minimum(maxSpeedKmH,
          ATC::ATCMath::convCmpsToKmph(currentCeilingSpeed)));
        speedList[noOfSpeedDatablocks].ceilingSpeedChangeReason = static_cast<uint8_t>(DMICeilingSpeedChangeReasonReferenceForOdometerValue);

        ++noOfSpeedDatablocks;

        DS::ConstMaTargetIterator it = DS::AbstractTargets::corePtr()->getConstMATargetIter();
        for (; (it != DS::AbstractTargets::corePtr()->getConstMATargetIterEnd()) && (noOfSpeedDatablocks < maxNoOfSpeedDataBlocks); ++it)
        {
          const DS::BaseTarget* const target = *it;
          const TravelDir targetDir = target->getDirection();
          const DS::BaseTarget::CoreTargetType targetType = target->getTargetType();
          const uint8_t trackDataItemType = static_cast<uint8_t>(target->getTDIType());
          

          bool includeTrackDataItemInList = false;
          uint8_t speedChangeReasonForTrackDataItem = 0U;
          if (DS::BaseTarget::TrackDataItemTarget == targetType)
          {
            includeTrackDataItemInList = includeTrackDataItemInCeilingSpeedList(trackDataItemType, speedChangeReasonForTrackDataItem);
          }

          //Normal Condition
          if (((DS::BaseTarget::SpeedTarget == targetType) || (DS::BaseTarget::PrimaryTarget == targetType)
            || includeTrackDataItemInList) && (travelDir == targetDir))
          {
            CeilingSpeedListDataBlock& ceilingSpeedData = speedList[noOfSpeedDatablocks];
            if (DS::BaseTarget::PrimaryTarget == target->getTargetType())
            {
              //Fetch Route type to check if primary target is location target
              const uint8_t primTargetRouteType = target->getRouteType();
              if (static_cast<uint8_t>(DS::BaseTarget::LocationEndTargetType) != primTargetRouteType)
              {
                ceilingSpeedData.ceilingSpeedChangeReason = static_cast<uint8_t>(DMICeilingSpeedChangeReasonEndOfMA);
                //Converting Odometer Position from centimeter to meter
                ceilingSpeedData.odoPosition = (target->getOdometer() + 50) / 100;
                currentCeilingSpeed = target->getSpeedChange();
                //Converting New Speed from cm/s to km/h
                ceilingSpeedData.newSpeed = static_cast <uint8_t>(ATC::ATCMath::minimum(maxSpeedKmH, ATC::ATCMath::convCmpsToKmph(currentCeilingSpeed)));
                ++noOfSpeedDatablocks;
              }
              else if (ATPModeLocation == currentMode)
              {
                ceilingSpeedData.ceilingSpeedChangeReason = static_cast<uint8_t>(DMICeilingSpeedChangeReasonEndOfMA);
                //Converting Odometer Position from centimeter to meter
                ceilingSpeedData.odoPosition = (target->getOdometer() + 50) / 100;
                currentCeilingSpeed = target->getSpeedChange();
                //Converting New Speed from cm/s to km/h
                ceilingSpeedData.newSpeed = static_cast <uint8_t>(ATC::ATCMath::minimum(maxSpeedKmH, ATC::ATCMath::convCmpsToKmph(currentCeilingSpeed)));
                ++noOfSpeedDatablocks;
              }
              else
              {
                //Do nothing
              }
            }
            else if (includeTrackDataItemInList)
            {
              ceilingSpeedData.ceilingSpeedChangeReason = speedChangeReasonForTrackDataItem;
              //Converting Odometer Position from centimeter to meter
              ceilingSpeedData.odoPosition = (target->getOdometer() + 50) / 100;
              //Converting New Speed from cm/s to km/h
              ceilingSpeedData.newSpeed = static_cast <uint8_t>(ATC::ATCMath::minimum(maxSpeedKmH, ATC::ATCMath::convCmpsToKmph(currentCeilingSpeed)));
              ++noOfSpeedDatablocks;
            }
            else
            {
              ceilingSpeedData.ceilingSpeedChangeReason = static_cast<uint8_t>
                (target->getSpeedChangeReason());
              //Converting Odometer Position from centimeter to meter
              ceilingSpeedData.odoPosition = (target->getOdometer() + 50) / 100;
              currentCeilingSpeed = target->getSpeedChange();
              //Converting New Speed from cm/s to km/h
              ceilingSpeedData.newSpeed = static_cast <uint8_t>(ATC::ATCMath::minimum(maxSpeedKmH, ATC::ATCMath::convCmpsToKmph(currentCeilingSpeed)));

              CeilingSpeedListDataBlock& prevCeilingSpeedData = speedList[noOfSpeedDatablocks - 1U];

              if ((prevCeilingSpeedData.ceilingSpeedChangeReason == ceilingSpeedData.ceilingSpeedChangeReason) &&
                  (prevCeilingSpeedData.newSpeed == ceilingSpeedData.newSpeed))
              {
                // Same entry, do not add!
              }
              else 
              {
                ++noOfSpeedDatablocks;
              }
            }
          }
        }

        //Send List to DMI only when primary target exist
        if (noOfSpeedDatablocks == 1U)
        {
          //if Primary target removed, clear the list from DMI
          noOfSpeedDatablocks = 0U;
        }
        dmiDataProcessState = DMIDataAvailable;
      }
    }

    /******************************************************************************
    * DMIMessageOutCeilingSpeedList::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutCeilingSpeedList::assembleDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));

      //Header Type
      messageData.headerType = dmiHeaderTypeAckMsg;
      //Message Number
      messageData.msgNumber = AbstractDMIHandler::corePtr()->getNextMessageNumber();

      //Get MSB for the acknowledged DMIMessageType
      messageData.dmiData.msgType = static_cast<uint8_t>(static_cast<uint8_t>(messageType) | 0x80U);

      // Assemble all data and write in Network order
      vfwPutU8(&buffer, noOfSpeedDatablocks);

      for (uint8_t i = 0U; i < noOfSpeedDatablocks; i++)
      {
        vfwPutI32(&buffer, speedList[i].odoPosition);
        vfwPutU8(&buffer, speedList[i].newSpeed);
        vfwPutU8(&buffer, speedList[i].ceilingSpeedChangeReason);
      }

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * includeTrackDataItemInCeilingSpeedList
    ******************************************************************************/
    bool DMIMessageOutCeilingSpeedList::includeTrackDataItemInCeilingSpeedList(const uint8_t trackDataItemType, uint8_t &speedChangeReason)
    {
      speedChangeReason = static_cast<uint8_t>(DMICeilingSpeedChangeReasonUndefined);  //To avoid lint warning here
      if (trackDataItemType <= static_cast<uint8_t>(TrackDataTypeAcousticSignal)) //To avoid lint warning here
      {
      }

      return false;
    }
  }
}
