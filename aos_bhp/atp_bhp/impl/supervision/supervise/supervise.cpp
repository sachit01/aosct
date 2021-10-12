/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This is the adaptation implementation of the supervise component
*  Supervise component, supervises the most restrictive speed. Supervise indicates
*  permitted speed, target speed and the remaining distance to the target point to
*  the driver. The component turns on/off an alarm when certain zones are passed.
*  It also requests the brakes in the case of over speeding
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-09    Hidaji      Created
* 2016-04-09    pparthib    Implementation of Extended Reverse Supervision
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "supervise.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_targets.hpp"
#include "config.hpp"
#include "abstract_message_handler.hpp"
#include "supervise_event_ids.hpp"
#include "brake_calculations.hpp"
#include "atc_math.hpp"
#include "dmi_bhp_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
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
  namespace Supv
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    Supervise::Supervise() :AbstractSupervise(),
      extRevSupvError(ATC::Event::createSBReqEvent(atpSuperviseId, ATC::AdaptationContainer, eventIdExtdRevDistExceeded, ATC::NoSB,
        DMICom::extRevDistExceededEvent, "Service Brake because Extended Reversing distance exceeded")),
      extRevEBMarginError(ATC::Event::createEBReqEvent(atpSuperviseId, ATC::AdaptationContainer, eventIdExtdRevEBMarginExceeded, ATC::NoEB,
        DMICom::extRevEBMarginExceededEvent, "Emergency Brake because Extended Reversing distance exceeded")),
      extRevSupvEmergencyBrakeCeilingSpeed(ATC::Event::createEBReqEvent(atpSuperviseId, ATC::AdaptationContainer, eventIdEBCeilingSpeed,
        ATC::NoEB, DMICom::extRevEmergencyBrakeCeilingSpeedEvent, "Emergency brake on ceiling speed supervision")),
      extRevSupvServiceBrakeCeilingSpeed(ATC::Event::createSBReqEvent(atpSuperviseId, ATC::AdaptationContainer, eventIdSBCeilingSpeed, ATC::NoSB,
        DMICom::extRevServiceBrakeCeilingSpeedEvent, "Service brake on ceiling speed supervision")),
      extRevSupvEnabled(false),
      accumulatedReverseDistance(0U),
      posAtMAFromScratch(0),
      posAtMAFromScratchValid(false)
    {
    }

    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    * (This info is not included in doxygen documentation but may be useful)
    *
    ******************************************************************************/
    Supervise& Supervise::instance()
    {
      static Supervise theOnlySuperviseInstance;

      return theOnlySuperviseInstance;
    }

    /******************************************************************************
    * superviseTims
    ******************************************************************************/
    void Supervise::superviseTims() const
    {
      // Do not issue a brake event (AOS_BHPB 2856)
    }

    /******************************************************************************
    * revSupervision
    ******************************************************************************/
    void Supervise::revSupervision()
    {
      if (isExtRevSupervisionEnabled())
      {
        //Supervise the Extended Reverse Supervision Distance
        superviseExtRevDistance();

        //Supervise the ceiling speed during reversing
        superviseExtRevCeilingSpeed();
      }
      else
      {
        AbstractSupervise::revSupervision();
      }
    }

    /******************************************************************************
    * isExtRevSupervisionEnabled
    ******************************************************************************/
    bool Supervise::isExtRevSupervisionEnabled()
    {
      if (Kernel::AbstractMessageHandler::corePtr()->isMAFromScratch())
      {
        extRevSupvEnabled = false;
        accumulatedReverseDistance = 0U;
        posAtMAFromScratch = Pos::AbstractOdometry::corePtr()->getOdoPositionWithoutOffset();
        posAtMAFromScratchValid = true;
      }

      const uint32_t extRevDist = getMaxExtndReverseDistance();
      const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      if ((extRevDist > 0U) && (currentMode == ATPModeNormal))
      {
        calculateAccumulatedReverseDistance();

        const uint32_t distanceFromScratchMA = getDistanceFromScratchMA();

        if ((distanceFromScratchMA > extRevDist) && (accumulatedReverseDistance <= extRevDist))
        {
          extRevSupvEnabled = true;
        }
      }
      else
      {
        extRevSupvEnabled = false;
        accumulatedReverseDistance = 0U;
        posAtMAFromScratchValid = false;
      }

      return extRevSupvEnabled;
    }

    /******************************************************************************
    * calculateAccumulatedReverseDistance
    ******************************************************************************/
    void Supervise::calculateAccumulatedReverseDistance()
    {
      const int32_t dNom = Pos::AbstractOdometry::corePtr()->getOdoPositionWithoutOffset();
      const int32_t dNomOld = Pos::AbstractOdometry::corePtr()->getOldOdoPositionWithoutOffset();
      const uint32_t diffPos = static_cast<uint32_t>(ATC::ATCMath::instance().absolute(dNom - dNomOld, __FILE__, __LINE__));

      if (isMovingAgainstMA())
      {
        accumulatedReverseDistance += diffPos;
      }
      else if (accumulatedReverseDistance > diffPos)
      {
        accumulatedReverseDistance -= diffPos;
      }
      else
      {
        accumulatedReverseDistance = 0U;
      }
    }

    /******************************************************************************
    * isMovingAgainstMA
    ******************************************************************************/
    bool Supervise::isMovingAgainstMA() const
    {
      const TravelDir maDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
      const TravelDir odoDir = Pos::AbstractOdometry::corePtr()->getOdoDirection();

      return (((maDir == DirForward) && (odoDir != DirForward))
        ||    ((maDir == DirReverse) && (odoDir != DirReverse)));
    }

    /******************************************************************************
    * getMaxExtndReverseDistance
    ******************************************************************************/
    uint32_t Supervise::getMaxExtndReverseDistance() const
    {
      const uint32_t extRevDist = static_cast<uint32_t>(Config::instance().getMaxExtReversingDistance());
      return (extRevDist * 100U);
    }

    /******************************************************************************
    * getExtndReverseCeilingSpeed
    ******************************************************************************/
    uint16_t Supervise::getExtndReverseCeilingSpeed() const
    {
      const uint16_t extRevCeilingSpeed = Config::instance().getMaxExtReversingSpeed();
      return extRevCeilingSpeed;
    }

    /******************************************************************************
    * superviseERSDistance
    ******************************************************************************/
    void Supervise::superviseExtRevDistance()
    {   
      // Get the maximum (non-extended) reversing distance in cm
      const uint32_t maxReverseDistance = AbstractConfig::corePtr()->getRevSupMargin();

      // Get the maximum extended reversing distance in cm
      const uint32_t maxExtdReverseDistance = getMaxExtndReverseDistance();

      // Fetch maximum reverse emergency break percentage
      const uint32_t maxExtndReverseEBMargin = AbstractConfig::corePtr()->getEBMarginAdded();

      // Calculate the maximum reverse emergency break distance from EB margin percentage applied on max reverse distance
      const uint32_t maxReverseEBDistance = ((maxExtndReverseEBMargin * maxReverseDistance) / 100U) + maxExtdReverseDistance;

      if (accumulatedReverseDistance > maxReverseEBDistance)
      {
        //Raising emergency break if train is moving reverse and exceeds EB margin
        ATC::AbstractEventHandler::corePtr()->reportEvent(extRevEBMarginError, __FILE__, __LINE__);

        if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
        {
          extRevSupvEnabled = false;
        }
      }
      else if (accumulatedReverseDistance > maxExtdReverseDistance)
      {
        //Raising service brake if train is moving reverse and exceeds extended reverse supervision distance
        ATC::AbstractEventHandler::corePtr()->reportEvent(extRevSupvError, __FILE__, __LINE__);

        if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
        {
          extRevSupvEnabled = false;
        }
      }
      else
      {
        //For Lint
      }
    }

    /******************************************************************************
    * getDistanceFromScratchMA
    ******************************************************************************/
    uint32_t Supervise::getDistanceFromScratchMA() const
    {
      uint32_t distance = 0U;

      if (posAtMAFromScratchValid)
      {
        const int32_t currentPosition = Pos::AbstractOdometry::corePtr()->getOdoPositionWithoutOffset();

        distance = static_cast<uint32_t>(ATC::ATCMath::instance().absolute(currentPosition - posAtMAFromScratch, __FILE__, __LINE__));
      }

      return distance;
    }

    /******************************************************************************
    * superviseCeilingSpeed
    ******************************************************************************/
    void Supervise::superviseExtRevCeilingSpeed() const
    {
      if (isMovingAgainstMA())
      {
        const uint16_t calcCeilingSpeed = getExtndReverseCeilingSpeed();

        if (0U != calcCeilingSpeed)
        {
          const uint32_t currentSpeed = Pos::AbstractOdometry::corePtr()->getSpeed();
          const uint32_t sbLimit = BrakeCalculations::instance().calcServiceBrakeLimitMargin(calcCeilingSpeed);
          const uint32_t ebLimit = BrakeCalculations::instance().calcEmergencyBrakeLimitMargin(calcCeilingSpeed);

          if (currentSpeed >= (calcCeilingSpeed + ebLimit)) // Check conditions for applying Emergency Brake
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(extRevSupvEmergencyBrakeCeilingSpeed, __FILE__, __LINE__);
          }
          else if ((currentSpeed >= (calcCeilingSpeed + sbLimit))) // Check conditions for applying Service Brake
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(extRevSupvServiceBrakeCeilingSpeed, __FILE__, __LINE__);
          }
          else
          {
            // Do nothing (but lint needs this else)
          }
        }
      }
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void Supervise::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization 

      AbstractSupervise::initCrossCompare();

      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&extRevSupvEnabled));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&accumulatedReverseDistance));
      crossCompare->addCrossCompareData(new Support::CrossCompareInt32(&posAtMAFromScratch));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&posAtMAFromScratchValid));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&extRevSupvError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&extRevEBMarginError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&extRevSupvEmergencyBrakeCeilingSpeed));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&extRevSupvServiceBrakeCeilingSpeed));
    }

  }
}
