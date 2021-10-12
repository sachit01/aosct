/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This is the adaptation implementation of the TargetCalculation
*  TargetCalculation is responsible for calculating the most restrictive ceiling speed
*  and the Permitted Speed for each target. The component provides supervise component
*  with all the information it needs to effectively supervise only the closest target
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-17    Hidaji      Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "target_calculation.hpp"
#include "abstract_tracks.hpp"
#include "config.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_message_handler.hpp"
#include "target_calculation_event_ids.hpp"
#include "targets.hpp"
#include "dmi_bhp_event_codes.hpp"
#include "abstract_position.hpp"
#include "supervise.hpp"

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
    TargetCalculation::TargetCalculation() :
      AbstractTargetCalculation(),
      lcsWarningCurvePointsLimit(ATC::Event::createLogEvent(atpTargetCalcId, ATC::AdaptationContainer,
        eventIdMaxCurvePointsLimit, 0x0U, "Speed Curve points exceed Max Limit")),
      nullPtrErr(ATC::Event::createSafetyHaltEvent(atpTargetCalcId, ATC::AdaptationContainer,
        eventIdNullPtr, ATC::NoEB, DMICom::nullPtr, "NULL Pointer Access"))
    {
      lastSampleOdoPos = 0;
      lastProcessedMRTId = 0U;
      samplePointIdx = 0U;
      samplePointStorage.numberOfPoints = 0U;
      memset(&samplePointStorage.speedCurvePoints[0], 0x00, sizeof(samplePointStorage.speedCurvePoints));
      clearSamplePoints();
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool TargetCalculation::init(void)
    {
      bool initPassed = true;
      lastSampleOdoPos = 0;
      lastProcessedMRTId = 0U;
      samplePointIdx = 0U;
      initCrossCompare();
      if (!(AbstractTargetCalculation::init()))
      {
        initPassed = false;
      }

      return initPassed;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void TargetCalculation::run(void)
    {
      //Check conditions to reset last processed target position
      resetLastProcessedTargetPosition();
      AbstractTargetCalculation::run();
    }

    /******************************************************************************
    * instance
    ******************************************************************************/
    TargetCalculation& TargetCalculation::instance()
    {
      static TargetCalculation theOnlyTargetCalculationInstance;

      return theOnlyTargetCalculationInstance;
    }


    /******************************************************************************
    * calcPermSpeedCurvePoints
    ******************************************************************************/
    bool TargetCalculation::calcPermSpeedCurvePoints()
    {
      bool sampleCollection = false;
      DS::SupervisedTarget* mostRestTarg = getMostRestrictiveTarget();

      if (mostRestTarg != static_cast<DS::SupervisedTarget*>(NULL))
      {
        /* Curve will be calculated till the parent target of MRT */
        const int32_t parentTargetPos = (mostRestTarg->getParentTarget())->getOdometer();
        const uint32_t parentTargetId = (mostRestTarg->getParentTarget())->getTargetId();
        TravelDir stDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
        int32_t currLeadPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();
        bool lastProcessedTargetPassed = false;

        if (((stDir == DirForward) && (currLeadPos >= lastSampleOdoPos))
          || ((stDir == DirReverse) && (currLeadPos <= lastSampleOdoPos))
          || (parentTargetId != lastProcessedMRTId))
        {
          lastProcessedTargetPassed = true;
        }

        const Pos::PosAccuracyState posAccuracyState = Pos::AbstractPosition::corePtr()->getAccuracyState();
        const bool validPosition = ((Pos::PosKnown == posAccuracyState) || (Pos::PosApprox == posAccuracyState));
        uint32_t currentFWCeilingSpeed = DS::AbstractTargets::corePtr()->getCurrFwCeilingSpeed(); //Current FW ceiling speed

        if (validPosition && (0U != currentFWCeilingSpeed) && lastProcessedTargetPassed)
        {
          //Clearing Sample Storage
          clearSamplePoints();

          uint32_t sampleSpeed = Supervise::instance().getPermittedSpeed();
          int32_t gradient = DS::AbstractTargets::corePtr()->getCurGradient();
          bool breakLoop = false;
          int32_t samplePos = currLeadPos;

          // Store the first sample for at the current position with the current sample speed.
          bool sampleStored = storeSample(samplePos, sampleSpeed);
          if (!sampleStored)
          {
            breakLoop = true;
          }

          DS::SupervisedTargetIterator it = DS::AbstractTargets::corePtr()->getSupervisedTargetIter();
          while ((it != DS::AbstractTargets::corePtr()->getSupervisedTargetIterEnd()) && (!breakLoop))
          {
            DS::SupervisedTarget* nextSupvTarg = *it;
            if (static_cast<DS::SupervisedTarget*>(NULL) != nextSupvTarg)
            {
              if (nextSupvTarg->getDirection() == stDir)
              {
                const int32_t nextSupvTargetOdo = nextSupvTarg->getOdometer();
                const uint32_t nextTargetSpeed = nextSupvTarg->getFirstWarningSpeed();
                const int32_t brakeability = DS::AbstractTSetup::corePtr()->getWorstBrakeabilityInRange(currentFWCeilingSpeed, nextTargetSpeed);
                const int32_t deceleration = BrakeCalculations::instance().calcDeceleration(brakeability, gradient);

                if (nextTargetSpeed > sampleSpeed)
                {
                  // If the next target has the higher speed than the sample speed, 2 points at same location are needed.
                  // One with lower speed and one with increased speed.
                  sampleStored = storeSample(nextSupvTargetOdo, sampleSpeed);
                  if(sampleStored)
                  {
                    sampleStored = storeSample(nextSupvTargetOdo, nextTargetSpeed);
                  }
                }
                else if (nextTargetSpeed == sampleSpeed)
                {
                  // If the next target has the same speed as the sample speed, no need to calculate any points from 
                  // current position to the next target. 
                }
                else
                {
                  sampleStored = calSamplesForTarget(samplePos, nextSupvTargetOdo, stDir, gradient, deceleration, nextTargetSpeed, sampleSpeed);
                }

                //updating the information based on next target
                if (!sampleStored)
                {
                  /*If sample count reach the max limit
                  * send as many points as possible. Last sample position is updated in storeSample().
                   More samples are sent when the last sample position is crossed*/
                  breakLoop = true;
                }
                else
                {
                  samplePos = nextSupvTargetOdo;
                  gradient = nextSupvTarg->getGradient();
                  currentFWCeilingSpeed = nextSupvTarg->getFwCeilingSpeed();
                  sampleSpeed = nextTargetSpeed;
                }

                if (sampleStored)
                {
                  //Saving last sample position
                  lastSampleOdoPos = samplePos;

                  ++it;
                  if (it != DS::AbstractTargets::corePtr()->getSupervisedTargetIterEnd())
                  {
                    if (static_cast<DS::SupervisedTarget*>(NULL) != (*it))
                    {
                      int32_t nextTargetPos = (*it)->getOdometer();
                      if (((stDir == DirForward) && (nextTargetPos >= parentTargetPos))
                        || ((stDir == DirReverse) && (nextTargetPos <= parentTargetPos)))
                      {
                        // add last target point in sample list if the fw speed is same as fw ceiling speed
                        if((nextSupvTarg->getFirstWarningSpeed()) == (nextSupvTarg->getFwCeilingSpeed()))
                        {
                          sampleStored = storeSample(samplePos, currentFWCeilingSpeed);
                        }
                        breakLoop = true;
                      }
                    }
                  }
                }

              }
              else
              {
                ++it;
              }
            }
            else
            {
              // Report An Error for Null pointer
              ATC::AbstractEventHandler::corePtr()->reportEvent(nullPtrErr, __FILE__
                , __LINE__);

              breakLoop = true;
            }
          } //End of while loop

          if (1U < samplePointIdx)
          {
            samplePointStorage.numberOfPoints = samplePointIdx;
            sampleCollection = true;
            lastProcessedMRTId = (mostRestTarg->getParentTarget())->getTargetId();
          }
        } //End of sample calculation check
      } //End of MRT target nullity check
      return sampleCollection;
    }

    /******************************************************************************
    * storeSample
    ******************************************************************************/
    bool TargetCalculation::storeSample(const int32_t sampleOdo, const uint32_t sampleSpeed)
    {
      TrackAndPos sampleTrackPos = DS::AbstractTracks::corePtr()->calculateTrackAndPos(sampleOdo);
      uint8_t maxCurvePoints = ATC::ATCMath::minimum(TG::maxCurvePoints, Config::instance().getLCSMaxSample());

      bool isStoreValid = true;

      if (samplePointIdx >= maxCurvePoints)
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(lcsWarningCurvePointsLimit, __FILE__, __LINE__);
        isStoreValid = false;
        //update the last stored position.
        static_cast<void>(DS::AbstractTracks::corePtr()->getOdoPos(samplePointStorage.speedCurvePoints[maxCurvePoints-1U].trackAndPosition,
                                                                  lastSampleOdoPos));
      }

      isStoreValid = isStoreValid && (0U != sampleTrackPos.track); // If the track is not available or reached maximum sample
      if (isStoreValid)
      {
        //Filling sample values in message
        samplePointStorage.speedCurvePoints[samplePointIdx].trackAndPosition.track = sampleTrackPos.track;
        samplePointStorage.speedCurvePoints[samplePointIdx].trackAndPosition.position = sampleTrackPos.position;
        samplePointStorage.speedCurvePoints[samplePointIdx].newSpeed = static_cast<uint16_t>(ATC::ATCMath::convCmpsTo100mph(sampleSpeed));
        ++samplePointIdx;
      }
      return isStoreValid;
    }

    /******************************************************************************
    * calSamplesForTarget
    ******************************************************************************/
    bool TargetCalculation::calSamplesForTarget(const int32_t currLeadPos, const int32_t targetOdo, const TravelDir tDir, const int32_t gradient,
      const int32_t deceleration, const uint32_t targetSpeed, uint32_t sampleSpeed)
    {

      bool storeSuccess = true;
      bool breakLoop = false;
      //Fetch configured Sample Speed Difference to calculate sample points
      const uint8_t speedDiffForNextSample = Config::instance().getLCSSpeedDiffForSample();

      // Starting from sample speed and reducing speed to target speed, calculate the samples positions and add them to the list
      while ((!breakLoop) && (storeSuccess)
        && (sampleSpeed >= targetSpeed))
      {
        //Calculate sample position for sample curve speed
        int32_t distTarget2Sample = BrakeCalculations::instance().calcFirstWarningCurveDistance(sampleSpeed, gradient, deceleration, targetSpeed);

        distTarget2Sample = ATC::ATCMath::maximum(0, distTarget2Sample);

        int32_t sampleOdo = (tDir == DirForward) ? (targetOdo - distTarget2Sample) : (targetOdo + distTarget2Sample);
        // don't add sample if it is before current train front
        if (((tDir == DirForward) && (sampleOdo >= currLeadPos)) || ((tDir == DirReverse) && (sampleOdo <= currLeadPos)))
        {
          storeSuccess = storeSample(sampleOdo, sampleSpeed);
        }

        if (sampleSpeed == targetSpeed)
        {
          breakLoop = true;
        }
        if (sampleSpeed > speedDiffForNextSample)
        {
          sampleSpeed -= static_cast<uint32_t>(speedDiffForNextSample);
          sampleSpeed = ATC::ATCMath::maximum(targetSpeed, sampleSpeed);
        }
        else
        {
          sampleSpeed = targetSpeed;
        }
      }

      return storeSuccess;
    }

    /******************************************************************************
    * getWarningCurveMessage
    ******************************************************************************/
    bool TargetCalculation::getWarningCurveMessage(TG::LCSWarningCurve &lcsWarningCurveMessage)
    {
      bool messageReady = false;

      if (calcPermSpeedCurvePoints())
      {
        lcsWarningCurveMessage = samplePointStorage;
        messageReady = true;
      }

      return messageReady;
    }

    /******************************************************************************
    * clearSamplePoints
    ******************************************************************************/
    void TargetCalculation::clearSamplePoints()
    {
      samplePointIdx = 0U;
      samplePointStorage.numberOfPoints = 0U;
      memset(&samplePointStorage.speedCurvePoints[0], 0x00, sizeof(samplePointStorage.speedCurvePoints));
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void TargetCalculation::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&lcsWarningCurvePointsLimit));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&nullPtrErr));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&lastSampleOdoPos));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&lastProcessedMRTId));
    }

    /******************************************************************************
    * calcModeDependentCeilingSpeed
    ******************************************************************************/
    uint32_t TargetCalculation::calcModeDependentCeilingSpeed(const uint32_t currCeilSpeed) const
    {
      //Check if approaching to level crossing
      uint16_t approachingSpeedLevelCrossing = 0U;
      uint32_t calcCS = AbstractTargetCalculation::calcModeDependentCeilingSpeed(currCeilSpeed);

      if (DS::Targets::instance().getApproachingLevelCrossing(approachingSpeedLevelCrossing) && (0U != approachingSpeedLevelCrossing))
      {
        calcCS = ATC::ATCMath::minimum(calcCS, static_cast<uint32_t>(approachingSpeedLevelCrossing));
      }
      else
      {
        //Do Nothing
      }

      return calcCS;
    }

    /******************************************************************************
    * resetLastProcessedTargetPosion
    ******************************************************************************/
    void TargetCalculation::resetLastProcessedTargetPosition()
    {
      //reset the last processed supervised target odo
      const bool trainInIdleState = Kernel::AbstractModeControl::corePtr()->getIdleState();
      const Pos::PosAccuracyState posAccuracyState = Pos::AbstractPosition::corePtr()->getAccuracyState();
      //check if a valid MA is received
      const bool isMaReceived = ATP::Kernel::AbstractMessageHandler::corePtr()->isValidMAReceived();

      const bool positionToReset = ((Pos::PosUnknown == posAccuracyState) || (Pos::PosDoubtfull == posAccuracyState));
      if (trainInIdleState || positionToReset)
      {
        //reset to antenna odo value if the position is not known or train is idling.
        lastSampleOdoPos = Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo();
        lastProcessedMRTId = 0U;
      }
      else if (!DS::AbstractTargets::corePtr()->isMATargetListEmpty())
      {
        const bool targetListChanged = DS::AbstractTargets::corePtr()->getTargetListChanged();
        const bool brakeResTimeChanged = DS::AbstractTSetup::corePtr()->isBrakeResponseTimeChanged();
        const bool brakeAbilityChanged = DS::AbstractTSetup::corePtr()->isBrakeAbilityChanged();

        const bool targetListReversed = DS::AbstractTargets::corePtr()->isTargetListReversed();
        const bool isModeLocation = (ATPModeLocation == Kernel::AbstractModeControl::corePtr()->getCurrentMode());
        const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
        const bool isCSFWDiff = (DS::AbstractTargets::corePtr()->getCurCeilingSpeed() != DS::AbstractTargets::corePtr()->getCurrFwCeilingSpeed());
        const bool targetListChangeInLocation = targetListReversed || (isModeLocation && isStandStill && isCSFWDiff);

        //if brake response has changed or target list changed without an MA or in location mode target changed due to direction change, 
        if (brakeResTimeChanged || brakeAbilityChanged || (targetListChanged && (!isMaReceived)) || targetListChangeInLocation)
        {
          lastSampleOdoPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();
        }
      }
      else
      {
        //To avoid lint warning
      }

    }

  }
}
