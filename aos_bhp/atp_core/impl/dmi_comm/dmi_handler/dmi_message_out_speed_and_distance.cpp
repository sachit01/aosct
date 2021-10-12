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
* This file implements the creator for the outgoing Speed & Distance DMIMessage.
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
#include "dmi_message_out_speed_and_distance.hpp"
#include "abstract_odometry.hpp"
#include "abstract_position.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_targets.hpp"

#ifdef __GNUG__
#include <vfw_time.h>
#else
extern "C" int64_t vfwGetReferenceTime(void);
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
  namespace DMICom
  {
    /******************************************************************************
    * DMIMessageOutSpeedAndDistance_hpp constructor
    ******************************************************************************/
    DMIMessageOutSpeedAndDistance::DMIMessageOutSpeedAndDistance() : AbstractDMIMessageOut(MTypeSpeedAndDistance)
    {
      curSpeed = 0U;
      curOdometer = 0;
      leadingTrackSection = 0U;
      leadingPosition = 0U;
      trailingTrackSection = 0U;
      trailingPosition = 0U;
      currentTrackGradient = 0;
      currentGradient = 0;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutSpeedAndDistance::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message :Speed & Distance");

        if ((AbstractDMIHandler::corePtr()->getCycleCount() % sendCycleSpeedAndDistance) == 0U)
        {
          const ATPMode curMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
          if ((curMode == ATPModeBaliseSearch) || (curMode == ATPModeNormal) || (curMode == ATPModeYard)
            || (curMode == ATPModeShuntingRoute) || (curMode == ATPModeStaffResponsible)
            || (curMode == ATPModeJoin) || (curMode == ATPModeSplit) || (curMode == ATPModePossession) ||
            (curMode == ATPModeShunting) || (curMode == ATPModeSafeBrakeToStop) || (curMode == ATPModeLocation))
          {
            if (AbstractDMIHandler::corePtr()->getDMICompatibilityVersionAccepted())
            {
              if (assembleDMIMessageData())
              {
                dmiDataProcessState = DMIDataValidated;
              }
            }
          }
        }
      }

      return(DMIDataValidated == dmiDataProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageOutSpeedAndDistance::invalidate()
    {
      curSpeed = 0U;
      curOdometer = 0;
      leadingTrackSection = 0U;
      leadingPosition = 0U;
      trailingTrackSection = 0U;
      trailingPosition = 0U;
      currentTrackGradient = 0;
      currentGradient = 0;
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutSpeedAndDistance::collectData()
    {
      //Set the curSpeed (cm/s)
      curSpeed = Pos::AbstractOdometry::corePtr()->getSpeed();

      const Pos::AbstractPosition* const position = Pos::AbstractPosition::corePtr();
      //Set the curOdometer(m)
      //Converting Position from centimeter to meter
      curOdometer = (position->getLeadingPosOdo() + 50) / 100;

      const DS::AbstractTracks* const tracks = DS::AbstractTracks::corePtr();

      const TrackAndPos leadingPos = tracks->calculateTrackAndPos(position->getActualLeadingPosOdo());

      // Set the actual leadingTrackSection
      leadingTrackSection = leadingPos.track;

      // Set the actual leadingPosition
      leadingPosition = leadingPos.position;

      const TrackAndPos trailingPos = tracks->calculateTrackAndPos(position->getActualTrailingPosOdo());

      // Set the actual trailingTrackSection
      trailingTrackSection = trailingPos.track;

      // Set the actual trailingPosition
      trailingPosition = trailingPos.position;

      // Collect gradients
      currentTrackGradient = DS::AbstractTargets::corePtr()->getCurTrackGradient();
      currentGradient = DS::AbstractTargets::corePtr()->getCurGradient();

      dmiDataProcessState = DMIDataAvailable;
    }

    /******************************************************************************
    * DMIMessageOutSpeedAndDistance::parseMessageData
    ******************************************************************************/
    bool DMIMessageOutSpeedAndDistance::assembleDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));

      //Header Type
      messageData.headerType = dmiHeaderTypeUnAckMsg;
      //Message Number
      messageData.msgNumber = unacknowledgedMessageNumber;

      //Get DMIMessageType
      messageData.dmiData.msgType = static_cast<uint8_t>(messageType);

      // Assemble data and write in network order
      vfwPutU16(&buffer, curSpeed);
      vfwPutI16(&buffer, static_cast<int16_t>(currentTrackGradient)); 
      vfwPutU16(&buffer, leadingTrackSection);
      vfwPutU32(&buffer, leadingPosition);
      vfwPutU16(&buffer, trailingTrackSection);
      vfwPutU32(&buffer, trailingPosition);
      vfwPutI32(&buffer, curOdometer);
      vfwPutI16(&buffer, static_cast<int16_t>(currentGradient));

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * DMIMessageOutSpeedAndDistance::logToRU
    ******************************************************************************/
    void DMIMessageOutSpeedAndDistance::logToRU() const
    {
      static int64_t lastLogTime = 0L; // seconds

      int64_t timeNow = vfwGetReferenceTime() / 1000; // millis to seconds
      int64_t logPeriod = static_cast<int64_t>(ATC::AbstractConfigBase::basePtr()->getRuLogDynValuePeriod()); // seconds

      if ((timeNow - lastLogTime) >= logPeriod)
      {
        lastLogTime = timeNow;

        AbstractDMIMessageOut::logToRU();
      }
    }
  }
}
