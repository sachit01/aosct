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
* This file implements the creator for the outgoing Gradient Data List DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-20    akushwah    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_gradient_data_list.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_odometry.hpp"
#include "abstract_targets.hpp"
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
    * DMIMessageOutGradientDataList constructor
    ******************************************************************************/
    DMIMessageOutGradientDataList::DMIMessageOutGradientDataList() : AbstractDMIMessageOut(MTypeGradientDataList)
    {
      noOfGradientDatablocks = 0U;
      prevTravelDir = DirUndefined;
      memset(&gradientList[0], 0, sizeof(gradientList));
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutGradientDataList::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message:Gradient Data List");

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
    void DMIMessageOutGradientDataList::invalidate()
    {
      //Clear all data
      noOfGradientDatablocks = 0U;
      memset(&gradientList[0], 0, sizeof(gradientList));
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutGradientDataList::collectData()
    {
      bool travelDirChangedInLocation = false;
      uint16_t startupStatus;
      const bool dmiStartupStatus = AbstractDMIHandler::corePtr()->getDMIStartupStatus(startupStatus);
      const bool anyTargetDeleted = DS::AbstractTargets::corePtr()->isTargetDelInList();
      const bool isTargetChanged = DS::AbstractTargets::corePtr()->getTargetListChanged();
      const TravelDir travelDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
      const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

      // Need to send an updated list of gradients to DMI when changing travel direction.
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

      if (isTargetChanged || dmiStartupStatus || anyTargetDeleted || travelDirChangedInLocation)
      {
        noOfGradientDatablocks = 0U;

        // If there are no targets, clear the gradient list in DMI
        if (!DS::AbstractTargets::corePtr()->isMATargetListEmpty())
        {
          /* Adding Current Gradient Target in list*/
          gradientList[noOfGradientDatablocks].newGradient = static_cast<int8_t>(DS::AbstractTargets::corePtr()->getCurTrackGradient());
          gradientList[noOfGradientDatablocks].odoPosition = (Pos::AbstractPosition::corePtr()->getSafeTrailingPosOdo() + 50) / 100; // cm => m
          uint8_t prevIndex = noOfGradientDatablocks;
          ++noOfGradientDatablocks;

          OdoPosition lastTargetOdo = 0;
          DS::ConstMaTargetIterator it = DS::AbstractTargets::corePtr()->getConstMATargetIter();
          for (; (it != DS::AbstractTargets::corePtr()->getConstMATargetIterEnd()) && (noOfGradientDatablocks < (maxNoOfGradientDataBlocks - 1U)); ++it)
          {
            const TravelDir targetDir = (*it)->getDirection();
            if (travelDir == targetDir)
            {
              if ((*it)->getTargetType() == DS::BaseTarget::GradientTarget)
              {
                DS::GradientTarget* gradTarget = ATC::dynamicCast<DS::BaseTarget*, DS::GradientTarget*>(*it, __FILE__, __LINE__);
                gradientList[noOfGradientDatablocks].newGradient = static_cast<int8_t>(gradTarget->getTrackGradient());
                gradientList[noOfGradientDatablocks].odoPosition = (gradTarget->getOdometer() + 50) / 100; // cm => m
                if ((gradientList[prevIndex].newGradient != gradientList[noOfGradientDatablocks].newGradient))
                {
                  prevIndex = noOfGradientDatablocks;
                  ++noOfGradientDatablocks;
                }
              }
              else
              {
                lastTargetOdo = (*it)->getOdometer();
              }
            }
          }
          gradientList[noOfGradientDatablocks].newGradient = 0;
          gradientList[noOfGradientDatablocks].odoPosition = (lastTargetOdo + 50) / 100; // cm => m
          ++noOfGradientDatablocks;
        }
        dmiDataProcessState = DMIDataAvailable;
      }
    }

    /******************************************************************************
    * DMIMessageOutGradientDataList::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutGradientDataList::assembleDMIMessageData()
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
      vfwPutU8(&buffer, noOfGradientDatablocks);

      for (uint8_t i = 0U; i < noOfGradientDatablocks; i++)
      {
        vfwPutI32(&buffer, gradientList[i].odoPosition);
        vfwPutI8(&buffer, gradientList[i].newGradient);
      }

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

  }
}
