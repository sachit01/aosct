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
* This file implements the creator for the outgoing Driver Info DMIMessage.
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
* 2016-10-12    arastogi    Added getting direction in collect data
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_driver_info.hpp"
#include "abstract_supervise.hpp"
#include "abstract_brake.hpp"
#include "abstract_radio_handler.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_targets.hpp"
#include "abstract_tsetup.hpp"
#include "atc_math.hpp"

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

namespace
{
  const uint8_t IndicatePermittedDirection                    = 0x01U; // D0 = Indicate Permitted direction
  const uint8_t IndicatePermittedSpeed                        = 0x02U; // D1 = Indicate Permitted speed
  const uint8_t IndicateTargetSpeed                           = 0x04U; // D2 = Indicate Target speed
  const uint8_t IndicateRemainingDistanceToTargetPoint        = 0x08U; // D3 = Indicate Remaining distance to target point
  const uint8_t IndicateRemainingDistanceToBCA                = 0x10U; // D4 = Indicate Remaining distance to BCA
  const uint8_t IndicatePredictedDistanceToStandStillLocation = 0x20U; // D5 = Indicate Predicted distance to stand still location

  const uint8_t IndicateTimeToIntervention                    = 0x80U; // D7 = Indicate Time to intervention

  const uint8_t BrakeIndicateInBCA               = 0x01U; // D0
  const uint8_t BrakeIndicateATP_Warning         = 0x02U; // D1
  const uint8_t BrakeIndicateATP_Intervention    = 0x04U; // D2
  const uint8_t BrakeIndicateFlashSB_BrakeButton = 0x08U; // D3
  const uint8_t BrakeIndicateRadioAvailable      = 0x10U; // D4
  const uint8_t BrakeIndicateSB_Applied          = 0x20U; // D5
  const uint8_t BrakeIndicateEB_Applied          = 0x40U; // D6
  const uint8_t BrakeIndicateFlashEB_BrakeButton = 0x80U; // D7


  uint8_t convertCmPerSecondToKmPerHourWithRounding(const uint32_t speedInCentimeterPerSecond)
  {
    uint32_t tempSpeed = ((speedInCentimeterPerSecond * 9U) + 125U) / 250U;

    return static_cast<uint8_t>(ATC::ATCMath::minimum(255U, tempSpeed));
  }

  uint16_t convertCmToMeterWithRounding(const int32_t lengthInCm)
  {
    int32_t lengthInMeter = (lengthInCm + 50) / 100;

    return static_cast<uint16_t>(lengthInMeter);
  }
}

namespace ATP
{
  namespace DMICom
  {
    /******************************************************************************
    * DMIMessageOutDriverInfo constructor
    ******************************************************************************/
    DMIMessageOutDriverInfo::DMIMessageOutDriverInfo() : AbstractDMIMessageOut(MTypeDriverinfo)
    {
      permittedDrivingDirection = DirUndefined;
      permittedSpeed = 0U;
      targetSpeed = 0U;
      timetoIntervention = 0U;
      remainingDistanceToTargetPoint = 0U;
      remainingDistanceToBCA = 0U;
      predDistToStandStillLoc = 0U;
      brakeRelatedData = 0U;
      indicateRelatedData = 0U;
      actualDrivingDirection = DirUndefined;
      maMargin = 0UL;
      brakeDelayEB = 0U;
      brakeDelaySB = 0U;
      brakeAbility = 0;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutDriverInfo::validate()
    {
      // Assemble, validate and publish data
      if (ATP::DMICom::DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message :Driver Info");

        if ((AbstractDMIHandler::corePtr()->getCycleCount() % sendCycleDriverInfo) == delayDriverInfo)
        {
          const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
          // Do not send driver info in modes where no driver info is updated  
          if ((currentMode != ATPModePoweringDown) && (currentMode != ATPModeSafetyHalt))
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

      return(ATP::DMICom::DMIDataValidated == dmiDataProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageOutDriverInfo::invalidate()
    {
      //Clear all the data 
      permittedDrivingDirection = DirUndefined;
      permittedSpeed = 0U;
      targetSpeed = 0U;
      timetoIntervention = 0U;
      remainingDistanceToTargetPoint = 0U;
      remainingDistanceToBCA = 0U;
      predDistToStandStillLoc = 0U;
      brakeRelatedData = 0U;
      indicateRelatedData = 0U;
      actualDrivingDirection = DirUndefined;
      maMargin = 0UL;
      brakeDelayEB = 0U;
      brakeDelaySB = 0U;
      brakeAbility = 0;

      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutDriverInfo::collectData()
    {
      // First reset all data
      invalidate();

      const ATPMode mode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      // If Idling --> Let DMI show NO Direction
      if (ATP::Kernel::AbstractModeControl::corePtr()->getIdleState())
      {
        permittedDrivingDirection = DirNone;
      }
      // If mode is Location/Shunting/Yard/Possesion --> Let DMI show BOTH directions
      else if ((ATPModeLocation == mode) || (ATPModeShunting == mode) || (ATPModeYard == mode) || (ATPModePossession == mode))
      {
        permittedDrivingDirection = DirBoth;
      }
      else
      {
        // Set Permitted Driving direction
        permittedDrivingDirection = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
      }

      // Direction is always set
      indicateRelatedData |= IndicatePermittedDirection;

      //Set Permitted Speed (km/h)
      //Converting Permitted Speed from cm/s to km/h
      uint32_t tempPermittedSpeed = Supv::AbstractSupervise::corePtr()->getPermittedSpeed();
      permittedSpeed = convertCmPerSecondToKmPerHourWithRounding(tempPermittedSpeed);
      indicateRelatedData |= IndicatePermittedSpeed;

      //if the train is in BCA
      if (Supv::AbstractSupervise::corePtr()->getInBCA())
      {
        //Set the target Speed
        uint32_t tmpTargetSpeed = Supv::AbstractSupervise::corePtr()->getSpeedAtTarget();
        targetSpeed = convertCmPerSecondToKmPerHourWithRounding(tmpTargetSpeed);
        indicateRelatedData |= IndicateTargetSpeed;

        //set the timetoIntervention
        const uint32_t supervisionTimetoIntervention = Supv::AbstractSupervise::corePtr()->getTimeToIntervention();
        timetoIntervention = static_cast<uint8_t>(ATC::ATCMath::minimum(supervisionTimetoIntervention, 255U));
        indicateRelatedData |= IndicateTimeToIntervention; // Set the indication bit

        //set the remainingDistanceToTargetPoint
        const int32_t supervisionDistanceToTarget = Supv::AbstractSupervise::corePtr()->getDistanceToTarget();
        remainingDistanceToTargetPoint = convertCmToMeterWithRounding(MAX(supervisionDistanceToTarget, 0));
        indicateRelatedData |= IndicateRemainingDistanceToTargetPoint; // Set the indication bit

        //Set the predDistToStandStillLoc
        //Converting Distance from centimeter to meter 
        const int32_t supervisionPredDistToStandStill = Supv::AbstractSupervise::corePtr()->getPredDistToStandStill();
        predDistToStandStillLoc = convertCmToMeterWithRounding(supervisionPredDistToStandStill);
        indicateRelatedData |= IndicatePredictedDistanceToStandStillLocation; // Set the indication bit

        //Set the brakeRelatedData
        //set bit for In BCA
        brakeRelatedData |= BrakeIndicateInBCA;
      }
      else
      {

        // If Idling --> The BCA indicator shall not be shown.
        if (!ATP::Kernel::AbstractModeControl::corePtr()->getIdleState())
        {
          // Set the remainingDistanceToBCA
          // Converting Distance from centimeter to meter
          remainingDistanceToBCA = convertCmToMeterWithRounding(Supv::AbstractSupervise::corePtr()->getDistanceToBCA());
          indicateRelatedData |= IndicateRemainingDistanceToBCA; // Set the indication bit
        }
      }


      // Report MA Margin (cm) to DMI in MA-controlled modes (except Location)
      // To inform driver when the train is considered to have reached the target and can be stopped
      if ((ATPModeNormal == mode) || (ATPModeStaffResponsible == mode) || (ATPModeSplit == mode) || 
          (ATPModeBaliseSearch == mode) || (ATPModeShuntingRoute == mode) || (ATPModeJoin == mode))
      {
        DS::BaseTarget* pTarget = DS::AbstractTargets::corePtr()->getPrimaryTarget();
        if (pTarget != static_cast<DS::BaseTarget*>(NULL))
        {
          // get maMargin from primary target (if any)
          const DS::PrimaryTarget* const primaryTarget =
            ATC::dynamicCast<DS::BaseTarget*, DS::PrimaryTarget*>(pTarget, __FILE__, __LINE__);

          maMargin = primaryTarget->getMAMargin();
        }
        else
        {
          maMargin = 0UL;
        }
      }
      // Report MA Margin (cm) fetched from Location End target when in Location mode.
      // MA Margin is copied to Location End/Start from MA head when the MA with Location Boundaries is received.
      // Used by AutoControl and perhaps for driver information in the future
      else if (ATPModeLocation == mode)
      {
        DS::BaseTarget* pTarget = DS::AbstractTargets::corePtr()->getLocationEndTarget();
        if (pTarget != static_cast<DS::BaseTarget*>(NULL))
        {
          // get maMargin from Location End primary target (if any)
          const DS::PrimaryTarget* const primaryTarget =
            ATC::dynamicCast<DS::BaseTarget*, DS::PrimaryTarget*>(pTarget, __FILE__, __LINE__);

          maMargin = primaryTarget->getMAMargin();
        }
        else
        {
          maMargin = 0UL;
        }
      }
      else
      {
        maMargin = 0UL;
      }

      //set bit for ATP Warning
      if (Supv::AbstractSupervise::corePtr()->getAtpWarning())
      {
        brakeRelatedData |= BrakeIndicateATP_Warning;
      }

      //set bit for ATPIntervention
      if (Supv::AbstractSupervise::corePtr()->getAtpIntervention())
      {
        brakeRelatedData |= BrakeIndicateATP_Intervention;
      }

      const Kernel::DriverLoginState atpDriverLoginState = Kernel::AbstractModeControl::corePtr()->getDriverLoginSeqState();

      //Set bit for Flash SB Brake button
      bool sbReleaseEnable = Supv::AbstractBrake::corePtr()->getSbReleaseEnable();
      bool ebReleaseEnable = Supv::AbstractBrake::corePtr()->getEbReleaseEnable();
      if (sbReleaseEnable && (!ebReleaseEnable) && (atpDriverLoginState == Kernel::DriverLoginSeq::DriverLoginSeq::driverLoggedIn))
      {
        brakeRelatedData |= BrakeIndicateFlashSB_BrakeButton;
      }

      //Set bit for Radio Available
      if (RadioCom::AbstractRadioHandler::corePtr()->getConnected())
      {
        brakeRelatedData |= BrakeIndicateRadioAvailable;
      }

      //Set bit for SB Applied
      if (Supv::AbstractBrake::corePtr()->getSbApplied())
      {
        brakeRelatedData |= BrakeIndicateSB_Applied;
      }

      //Set bit for EB Applied
      if (Supv::AbstractBrake::corePtr()->getEbApplied())
      {
        brakeRelatedData |= BrakeIndicateEB_Applied;
      }

      //Set bit for Flash EB Brake button
      if (Supv::AbstractBrake::corePtr()->getEbReleaseEnable() && (atpDriverLoginState == Kernel::DriverLoginSeq::DriverLoginSeq::driverLoggedIn))
      {
        brakeRelatedData |= BrakeIndicateFlashEB_BrakeButton;
      }

      const TravelDir travelDir = IO::AbstractLocoIO::corePtr()->getLocoDirection();
      const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      // Only applicable in location-mode.  
      if (ATPModeLocation == currentMode)
      {
        actualDrivingDirection = travelDir;
      }
      else
      {
        actualDrivingDirection = DirUndefined;
      }

      // Current brakeability
      brakeAbility = DS::AbstractTSetup::corePtr()->getBrakeability(Pos::AbstractOdometry::corePtr()->getSpeed());

      // Current brake-delay for EB
      brakeDelayEB = DS::AbstractTSetup::corePtr()->getEmergencyBrakeResponseTime();

      // Current brake-delay for SB
      brakeDelaySB = DS::AbstractTSetup::corePtr()->getServiceBrakeResponseTime();

      dmiDataProcessState = DMIDataAvailable;
    }

    /******************************************************************************
    * DMIMessageOutDriverInfo::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutDriverInfo::assembleDMIMessageData()
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
      vfwPutU8(&buffer, static_cast<uint8_t>(permittedDrivingDirection));
      vfwPutU8(&buffer, permittedSpeed);
      vfwPutU8(&buffer, targetSpeed);
      vfwPutU8(&buffer, timetoIntervention);
      vfwPutU16(&buffer, remainingDistanceToTargetPoint);
      vfwPutU16(&buffer, remainingDistanceToBCA);
      vfwPutU16(&buffer, predDistToStandStillLoc);
      vfwPutU8(&buffer, static_cast<uint8_t>(0)); // spare
      vfwPutU8(&buffer, brakeRelatedData);
      vfwPutU8(&buffer, indicateRelatedData);
      vfwPutU8(&buffer, static_cast<uint8_t>(actualDrivingDirection));
      // report MA margin to DMI in cm
      vfwPutU16(&buffer, static_cast<uint16_t>(maMargin)); 
      vfwPutU16(&buffer, static_cast<uint16_t>(brakeAbility));
      vfwPutU16(&buffer, static_cast<uint16_t>(brakeDelayEB));
      vfwPutU16(&buffer, static_cast<uint16_t>(brakeDelaySB));

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * DMIMessageOutDriverInfo::logToRU
    ******************************************************************************/
    void DMIMessageOutDriverInfo::logToRU() const
    {
      static int64_t lastLogTime = 0L; // seconds
      static TravelDir lastPermittedDrivingDirection = DirUndefined;
      static uint8_t lastPermittedSpeed = 0U;
      static uint8_t lastTargetSpeed = 0U;
      static uint8_t lastBrakeRelatedData = 0U;
      static uint8_t lastIndicateRelatedData = 0U;
      static TravelDir lastActualDrivingDirection = DirUndefined;
      static uint32_t lastMaMargin = 0U;
      static uint32_t lastBrakeDelayEB = 0U;
      static uint32_t lastBrakeDelaySB = 0U;
      static int32_t lastBrakeAbility = 0;


      int64_t timeNow = vfwGetReferenceTime() / 1000; // millis to seconds
      int64_t logPeriod = static_cast<int64_t>(ATC::AbstractConfigBase::basePtr()->getRuLogDynValuePeriod()); // seconds

      if (((timeNow - lastLogTime) >= logPeriod) ||
        (permittedDrivingDirection != lastPermittedDrivingDirection) ||
        (permittedSpeed != lastPermittedSpeed) ||
        (targetSpeed != lastTargetSpeed) ||
        (brakeRelatedData != lastBrakeRelatedData) ||
        (indicateRelatedData != lastIndicateRelatedData) || 
        (actualDrivingDirection != lastActualDrivingDirection) ||
        (maMargin != lastMaMargin) ||
        (brakeAbility != lastBrakeAbility) ||
        (brakeDelayEB != lastBrakeDelayEB) ||
        (brakeDelaySB != lastBrakeDelaySB))
      {
        lastLogTime = timeNow;
        lastPermittedDrivingDirection = permittedDrivingDirection;
        lastPermittedSpeed = permittedSpeed;
        lastTargetSpeed = targetSpeed;
        lastBrakeRelatedData = brakeRelatedData;
        lastIndicateRelatedData = indicateRelatedData;
        lastActualDrivingDirection = actualDrivingDirection;
        lastMaMargin = maMargin;
        lastBrakeAbility = brakeAbility;
        lastBrakeDelayEB = brakeDelayEB;
        lastBrakeDelaySB = brakeDelaySB;

        AbstractDMIMessageOut::logToRU();
      }
    }
  }
}
