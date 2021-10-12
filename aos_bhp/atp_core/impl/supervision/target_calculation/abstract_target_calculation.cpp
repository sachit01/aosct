/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This is the core implementation of the TargetCalculation
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
* 2016-09-30    Hidaji      First implementation of functions
* 2016-10-05    Hidaji      Added maxTrainSpeed to updateGBC
* 2016-12-05    rquensel    Added one attribute to be cross compared
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_target_calculation.hpp"
#include "abstract_config.hpp"
#include "abstract_cross_compare.hpp"
#include <vfw_checkpoints.h>
#include "abstract_tracks.hpp"
#include "abstract_analyzer_if.hpp"
#include "abstract_message_handler.hpp"
#include "brake_calculations_event_ids.hpp"
#include "cross_compare_complex.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

namespace
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  bool isOdometerValueLessThan(const ATP::TravelDir tDir, const ATP::OdoPosition odometerValue1, const ATP::OdoPosition odometerValue2)
  {
    bool result;
    if (tDir == ATP::DirForward)
    {
      result = (odometerValue1 < odometerValue2);
    }
    else
    {
      result = (odometerValue1 > odometerValue2);
    }

    return result;
  }
}

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
    AbstractTargetCalculation::AbstractTargetCalculation() : ATC::ProcComponent(atpTargetCalcId, "TargetCalculation", "TC"),
      contSizeExceeded(ATC::Event::createSafetyHaltEvent(atpTargetCalcId, ATC::CoreContainer, eventIdExceededContSize,
        ATC::NoEB, 0x0U, "Exceeding internal container size!")),
      maHeadInvalid(ATC::Event::createSafetyHaltEvent(atpTargetCalcId, ATC::CoreContainer, eventIdMaHeadInvalid,
        ATC::NoEB, 0x0U, "Ma received no Ma head available!"))
    {
      if (coreTargetCalculationInstancePtr != 0)
      {
        // Error handler
        ATC::aosHalt(__FILE__, __LINE__, "Target calculation constructor already instantiated");
      }

      initialized = false;
      maStartFromScratch = false;
      calcCeilingSpeed = 0U;
      // Setup single instance pointer for core access
      coreTargetCalculationInstancePtr = this;
      maStartPos = 0;

      //Initialize the size of the vectors.
      fwVector.reserve(maxSizeCurveVectors);
      swVector.reserve(maxSizeCurveVectors);
      sbVector.reserve(maxSizeCurveVectors);
      ebVector.reserve(maxSizeCurveVectors);

      releaseSpSBPos = 0;
      releaseSpEBPos = 0;

      lastSmTDI = static_cast<DS::BaseTarget*>(NULL);
      smDelta = 0U;
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractTargetCalculation::init(void)
    {
      if (!initialized)
      {
        bool resCSpeedAIF = ATC::AbstractAnalyzerIF::corePtr()->registerMeasurement("ceilingSpeed", "Ceiling Speed", "cm/s",
          0U, ATC::uint32Max, &calcCeilingSpeed);

        if (!resCSpeedAIF)
        {
          writeToLog(ATC::BriefLog,"Register measurement failed for analyzerIF in target calculation", __FILE__, __LINE__);
        }

        initialized = true;
        initCrossCompare();
      }
      return true;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void AbstractTargetCalculation::run(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "TC_run");

      //reset the values to avoid cross compare
      lastSmTDI = static_cast<DS::BaseTarget*>(NULL);
      smDelta = 0U;

      uint32_t trainLength = 0U;
      
      //clear all vectors
      fwVector.clear();
      swVector.clear();
      sbVector.clear();
      ebVector.clear();

      // Calculate the ceiling speed

      DS::AbstractTargets* const targets = DS::AbstractTargets::corePtr();

      calcCeilingSpeed = calcModeDependentCeilingSpeed(targets->getCurCeilingSpeed());

      const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();
      const bool isMaReceived = ATP::Kernel::AbstractMessageHandler::corePtr()->isValidMAReceived();

      if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
      {
        //if MA received in not idle state then it is MA Extension
        if (isMaReceived)
        {
          //Calculate the odometer at the start of MA.
          MAHead maHead;
          if (!(Kernel::AbstractMessageHandler::corePtr()->getMAHead(maHead)))
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(maHeadInvalid, __FILE__, __LINE__);
          }

          if (!DS::AbstractTracks::corePtr()->getOdoPos(maHead.startOfMATrackAndPos, maStartPos))
          {
            if (!targets->isMATargetListEmpty())
            {
              //Set the MA start pos to the odometer of the first target in the ma target list if the track of ma start was deleted.
              DS::ConstMaTargetIterator it = targets->getConstMATargetIter();
              maStartPos = (*it)->getOdometer();
            }
          }

          bool isMAfromScratch = Kernel::AbstractMessageHandler::corePtr()->isMAFromScratch();
          // If MA from scratch is received update the ceiling speeds
          if(isMAfromScratch)
          {
            maStartFromScratch = true;
            updateCeilingSpeeds(targets->getCurCeilingSpeed());
            targets->setCurGradient(targets->getCurTrackGradient());
          }
          else
          {
            maStartFromScratch = false;
          }
        }

        if (!targets->isMATargetListEmpty())
        {
          const bool targetListChanged = targets->getTargetListChanged();
          const bool brakeResTimeChanged = DS::AbstractTSetup::corePtr()->isBrakeResponseTimeChanged();
          const bool brakeAbilityChanged = DS::AbstractTSetup::corePtr()->isBrakeAbilityChanged();

          const bool targetListReversed = targets->isTargetListReversed();
          const bool isModeLocation = (ATPModeLocation == Kernel::AbstractModeControl::corePtr()->getCurrentMode());
          const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
          const bool isCSFWDiff = (targets->getCurCeilingSpeed() != targets->getCurrFwCeilingSpeed());
          const bool targetListChangeLocation = targetListReversed || (isModeLocation && isStandStill && isCSFWDiff);
          const bool isAtpModeSplit = Kernel::AbstractModeControl::corePtr()->getCurrentMode() == ATPModeSplit;

          // If brake response has changed or target list changed without an MA or in Split mode and MA is received (needed
          // as MA is never cleared in Split mode) or target list changed without an MA (e.g. :conditional target becomes active)
          // remove all supervised targets.
          if(brakeResTimeChanged || (targetListChanged && (!isMaReceived)) || (isAtpModeSplit && isMaReceived))
          {
            targets->removeAllSupervisedTargets();

            //set the speeds based on the current ceiling speed
            updateCeilingSpeeds(targets->getCurCeilingSpeed());

            //Set the MA start pos to the odometer of the first target in the ma target list.
            DS::ConstMaTargetIterator it = targets->getConstMATargetIter();
            maStartPos = (*it)->getOdometer();
          }

          // recalculate all supervised targets if the target list has changed
          // or brake response time has changed.
          if (targetListChanged || brakeResTimeChanged || (isAtpModeSplit && isMaReceived))
          {

            trainLength = pTrainSetup->length;

            //Add supervised gradient target and calculate their gradient
            addSuperviseGradientTargets(trainLength);

            //Add supervised ceiling speed targets
            addSupvSpeedAndPrimaryTargets();

            // Update gradient for all new non gradient supervised targets
            // and update fw ceiling, sw ceiling, sb ceiling, eb ceiling and ceiling speed for all new targets
            updateGradientAndCeilingSpeed(static_cast<uint32_t>(pTrainSetup->maxSpeed));

            // For all new targets and other targets that are affected by change, calculate FW, SW, SB and EB speed
            updateCurveSpeedsForTargets();

            //trace the targets when they are updated.
            bool traceEnabled = false;
            uint8_t level = 0U;
            getTrace()->getTraceDetails(level, traceEnabled);
            if (traceEnabled && (level >= 2U))
            {
              DS::AbstractTargets::corePtr()->displayMaTarg();
              DS::AbstractTargets::corePtr()->displaySupTarg();
              getTrace()->write(level, "release speed SB pos", releaseSpSBPos);
              getTrace()->write(level, "release speed EB pos", releaseSpEBPos);
            }
          }
          //if the brake ability has changed
          else if(brakeAbilityChanged)
          {
            //recalculate the FW, SW, SB and EB speed. CS are not affected by brake ability change.
            updateCurveSpeedsForTargets();
          }
          //if target list is reversed in location mode or at first standstill in location when CS is updated.
          else if(targetListChangeLocation)
          {
            //update the current ceiling speeds.
            updateCeilingSpeeds(targets->getCurCeilingSpeed());

            //Set the MA start pos to the odometer of the first target in the ma target list.
            DS::ConstMaTargetIterator it = targets->getConstMATargetIter();
            maStartPos = (*it)->getOdometer();

            // Update gradient for all new non gradient supervised targets
            // and update fw ceiling, sw ceiling, sb ceiling, eb ceiling and ceiling speed for all new targets
            updateGradientAndCeilingSpeed(static_cast<uint32_t>(pTrainSetup->maxSpeed));

            // For all new targets and other targets that are affected by change, calculate FW, SW, SB and EB speed
            updateCurveSpeedsForTargets();
          }
          else
          {
            ; // do nothing
          }
        }
      }
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractTargetCalculation* AbstractTargetCalculation::corePtr(void)
    {
      return coreTargetCalculationInstancePtr;
    }

    /******************************************************************************
    * calcModeDependentCeilingSpeed
    ******************************************************************************/
    uint32_t AbstractTargetCalculation::calcModeDependentCeilingSpeed(const uint32_t currCeilSpeed) const
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "TC_calcModeDependentCeilingSpeed_begin");

      ATPMode atpMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      uint32_t modeDependentCeilingSpeed = 0U;

      switch (atpMode)
      {
        // ATP modes on which, the train is not allowed to move:
      case ATPModePoweringDown:
      case ATPModePowerUp:
      case ATPModeUnregistered:
      case ATPModeSafetyHalt:
      case ATPModeRegistration:
      case ATPModeConfiguration:
        modeDependentCeilingSpeed = 0U;
        break;

      case ATPModeBaliseSearch:
        if (!DS::AbstractTargets::corePtr()->isMATargetListEmpty())
        {
          modeDependentCeilingSpeed = AbstractConfig::corePtr()->getBalSearchSpeed();
        }
        break;

      case ATPModeSplit:
      case ATPModeStaffResponsible:
      case ATPModeYard:
      case ATPModeShunting:
      case ATPModePossession:
      case ATPModeNormal:
      case ATPModeLocation:
      case ATPModeShuntingRoute:
      case ATPModeJoin:
      case ATPModeSafeBrakeToStop:
        modeDependentCeilingSpeed = currCeilSpeed;
        break;

      case ATPModeSleeping: // No supervision shall be performed in sleeping
        modeDependentCeilingSpeed = 0U;
        break;

      case ATPModeUndefined:
      case ATPModesCount:
      default:
        ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        break;
      }

      uint32_t smSpeedRest = DS::AbstractTargets::corePtr()->getSMSpeedRestriction();
      if(smSpeedRest != 0U)
      {
        modeDependentCeilingSpeed = ATC::ATCMath::minimum(modeDependentCeilingSpeed, smSpeedRest);
      }

      vfwVisitCheckPoint(&endCp, "TC_calcModeDependentCeilingSpeed_end");

      return modeDependentCeilingSpeed;
    }

    /******************************************************************************
    * addSupvSpeedAndPrimaryTargets
    ******************************************************************************/
    void AbstractTargetCalculation::addSupvSpeedAndPrimaryTargets()
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "TC_addSupvSpeedAndPrimaryTargets_begin");

      //only call this function if target list has changed.
      const DS::AbstractTargets* const targets = DS::AbstractTargets::corePtr();

      //set the gradient value to current track gradient
      int32_t currTrackGrad = targets->getCurTrackGradient();

      //set the min gradient value to current track gradient
      int32_t minTrackGrad = targets->getCurTrackGradient();

      //set the ceiling speed value to check ceiling speed target
      uint32_t currCS = targets->getCurCeilingSpeed();

      uint16_t currSM = targets->getSafetyMargin();

      uint16_t maxSM = currSM;

      DS::TrackDataItemTarget* smTarg = static_cast<DS::TrackDataItemTarget*>(NULL);

      for (DS::ConstMaTargetIterator it = targets->getConstMATargetIter();
        it != targets->getConstMATargetIterEnd(); ++it)
      {
        DS::BaseTarget* const target = *it;
        if (target->getTargetType() == DS::BaseTarget::PrimaryTarget)
        {
          DS::PrimaryTarget* priTarg = ATC::dynamicCast<DS::BaseTarget*, DS::PrimaryTarget*>(target, __FILE__, __LINE__);
          priTarg->addRelatedSuperviseTargets(currCS);
        }
        if(target->getTargetType() == DS::BaseTarget::GradientTarget)
        {
          DS::GradientTarget* gradTarg = ATC::dynamicCast<DS::BaseTarget*, DS::GradientTarget*>(target, __FILE__, __LINE__);
          //update current gradient when iterating through the list
          currTrackGrad = gradTarg->getTrackGradient();
          // TODO: Using minTrackGrad is safe but it is too conservative. The correct way is to calculate the gradient
          // based on effect of all supervised gradients that can be between the target and the supervised targets
          minTrackGrad = ATC::ATCMath::minimum(minTrackGrad, currTrackGrad);
        }
        else if(target->getTargetType() == DS::BaseTarget::SpeedTarget)
        {
          //check that the target is ahead of the MA start.
          bool isNewTarget = (target->getDirection() == DirForward)?(target->getOdometer() >= maStartPos):(target->getOdometer() <= maStartPos);

          if(isNewTarget)
          {
            DS::SpeedTarget* speedTarg = ATC::dynamicCast<DS::BaseTarget*, DS::SpeedTarget*>(target, __FILE__, __LINE__);
            speedTarg->addRelatedSuperviseTargets(currCS, minTrackGrad);
          }

          //update the current ceiling speed
          currCS = target->getSpeedChange();
        }
        else if(target->getTargetType() == DS::BaseTarget::TrackDataItemTarget)
        {
          DS::TrackDataItemTarget* tdiTarg = ATC::dynamicCast<DS::BaseTarget*, DS::TrackDataItemTarget*>(target, __FILE__, __LINE__);
          if(tdiTarg->getTDIType() == TrackDataTypeSafetyMarginChange)
          {
            smTarg = tdiTarg;
            currSM = smTarg->getTDIValue();
            maxSM = ATC::ATCMath::maximum(maxSM, currSM);
          }
        }
        else
        {
          //Do Nothing
          ;
        }
      }

      smDelta = maxSM - currSM;
      lastSmTDI = smTarg;

      vfwVisitCheckPoint(&endCp, "TC_addSupvSpeedAndPrimaryTargets_end");
    }

    /******************************************************************************
    * addSuperviseGradientTargets
    ******************************************************************************/
    void AbstractTargetCalculation::addSuperviseGradientTargets(const uint32_t trainLength) const
    {
      //Fetch all the gradient targets and store the pointers of targets in local pointer array
      DS::GradientTarget* gradientTargets[maxNumberOfGradientTargets];
      memset(&gradientTargets[0], 0, sizeof(gradientTargets));
      const uint8_t gradTargCount = DS::AbstractTargets::corePtr()->getGradientTargetList(&gradientTargets[0]);

      SupGradInfo supGradTargetsInfo[maxNumberOfGradientTargets * maxNrSupvGradTargets];
      uint8_t supGradTargCount = 0U;

      OdoPosition firstSupTargOdo = 0;
      uint8_t firstGradTargIndex = 0U;

      // Find the first gradient target that needs to be processed, also the location of its first supervise target to be added.
      // The gradient target might already have supervise targets
      bool targetFound = findGradAndSupTargToProcess(&gradientTargets[0], gradTargCount, firstGradTargIndex, firstSupTargOdo, trainLength);

      if (targetFound)
      {
        // Add all possible supervised targets for all gradient targets from "first gradient to process" to the last gradient
        createSuperviseGradTargets(&gradientTargets[0], gradTargCount, &supGradTargetsInfo[0], supGradTargCount,
          firstGradTargIndex, firstSupTargOdo, trainLength);

        // For all created supervise gradient targets calculate the gradient and add the targets to the target list
        calcGradient(&gradientTargets[0], gradTargCount, &supGradTargetsInfo[0], supGradTargCount, trainLength);
      }
    }

    /******************************************************************************
    * findGradAndSupTargToProcess
    ******************************************************************************/
    bool AbstractTargetCalculation::findGradAndSupTargToProcess(DS::GradientTarget* const gradientTargets[], const uint8_t gradTargCount,
      uint8_t &firstGradTargIndex, OdoPosition &firstSupTargOdo, const uint32_t trainLength) const
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "TC_findGradAndSupTargToProcess_begin");

      //Find firstGrad and the position of its first supervised gradient target to process.
      uint8_t targetIndex = 0U;
      bool targetFound = false;

      if (!maStartFromScratch)  // if extension ma
      {
        // Fast forward to the last gradient target that has been processed, which is the last gradient before previous oldMaEnd
        bool lastTargFound = false;
        bool breakFFLoop = false;
        uint8_t lastPIndex = 0U;
        const TravelDir tDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
        while ((lastPIndex < gradTargCount) && (!breakFFLoop))
        {
          const bool beforeOldMa = isOdometerValueLessThan(tDir, gradientTargets[lastPIndex]->getOdometer(), maStartPos);

          if (beforeOldMa) // gradient target is before the previous Ma end
          {
            targetIndex = lastPIndex;
            lastTargFound = true;
            ++lastPIndex;
          }
          else
          {
            breakFFLoop = true;
          }
        }

        if (lastTargFound)
        {
          const uint32_t distGradToOldMa = static_cast<uint32_t>(ATC::ATCMath::instance().absolute(
            (gradientTargets[targetIndex]->getOdometer() - maStartPos), __FILE__, __LINE__));

          if (distGradToOldMa >= trainLength)
          {
            // The distance of the last target to the previous maEnd is more than train length; Last target was fully processed.
            targetIndex++; // look into the next target
          }
          else // Not fully processed.. then use the ma start as first target position.
          {
            firstGradTargIndex = targetIndex;
            firstSupTargOdo = maStartPos;
            targetFound = true;
          }
        } // if target is not found (already removed) continue as ma start from scratch
      }

      const TravelDir tDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
      OdoPosition currLeadPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();

      //set the current lead position to MA start pos if the position is Unknown.
      if(Pos::AbstractPosition::corePtr()->getAccuracyState() == Pos::PosUnknown)
      {
        currLeadPos = maStartPos;
      }

      while ((targetIndex < gradTargCount) && (!targetFound))
      {
        DS::GradientTarget *curGradTarget = gradientTargets[targetIndex];

        // Is the gradient is under the train?
        if (isOdometerValueLessThan(tDir, curGradTarget->getOdometer(), currLeadPos))
        {
          bool nextGradIsUnderTrain = false;
          if ((targetIndex + 1U) < gradTargCount)  // If next target exists
          {
            const DS::GradientTarget* const nextGradTarget = gradientTargets[targetIndex + 1U];
            nextGradIsUnderTrain = isOdometerValueLessThan(tDir, nextGradTarget->getOdometer(), currLeadPos);
          }

          if (!nextGradIsUnderTrain) // Current gradient is the last gradient that is under the train,
          {
            firstSupTargOdo = currLeadPos;
            firstGradTargIndex = targetIndex;
            targetFound = true; // target found
          } // If next gradient target is also under the train we are not interested in the current gradient and will continue to the next loop
        }
        else // No target is under the train
        {
          firstGradTargIndex = targetIndex;
          firstSupTargOdo = curGradTarget->getOdometer();
          targetFound = true; // target found
        }
        ++targetIndex;
      }

      vfwVisitCheckPoint(&endCp, "TC_findGradAndSupTargToProcess_end");
      return targetFound;
    }

    /******************************************************************************
    * findGradAndSupTargToProcess
    ******************************************************************************/
    void AbstractTargetCalculation::createSuperviseGradTargets(DS::GradientTarget* const gradientTargets[], const uint8_t gradTargCount,
      SupGradInfo supGradTargets[], uint8_t &supGradTargCount, const uint8_t firstGradTargIndex, const OdoPosition firstSupTargOdo,
      const uint32_t trainLength) const
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "TC_createSuperviseGradTargets_begin");

      const TravelDir tDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
      uint8_t targetIndex = firstGradTargIndex;
      OdoPosition supTargOdo = firstSupTargOdo;
      OdoPosition curMAend = DS::AbstractTargets::corePtr()->getPrimaryTarget()->getOdometer();
      const uint32_t sectionLength = getSectionLength(trainLength);

      bool breakGradLoop = false;
      while ((targetIndex < gradTargCount) && (!breakGradLoop)) // loop through gradients
      {
        DS::GradientTarget *curGradTarget = gradientTargets[targetIndex];

        bool breakSupLoop = false;
        while (!breakSupLoop) // loop to create supervise targets for current gradient
        {
          bool supIsAfterMaEnd = (tDir == DirForward) ? (supTargOdo >= curMAend) : (supTargOdo <= curMAend);
          bool supIsAfterNextGrad = false;
          if ((targetIndex + 1U) < gradTargCount)  // If next target exists
          {
            DS::GradientTarget* nextGradTarget = gradientTargets[targetIndex + 1U];

            if (tDir == DirForward)
            {
              supIsAfterNextGrad = (supTargOdo >= nextGradTarget->getOdometer());
            }
            else
            {
              supIsAfterNextGrad = (supTargOdo <= nextGradTarget->getOdometer());
            }
          }

          if (supIsAfterMaEnd)  // Don't add any more targets.
          {
            breakSupLoop = true;
            breakGradLoop = true;
          }
          else if (supIsAfterNextGrad)
          {
            // Don't add any more supervised gradients for this gradient, go to next gradient target
            breakSupLoop = true;
          }
          else // The conditions for adding the supervise target are correct
          {
            if (supGradTargCount < (maxNumberOfGradientTargets * maxNrSupvGradTargets))
            {
              supGradTargets[supGradTargCount].odoPos = supTargOdo;
              supGradTargets[supGradTargCount].parentTarget = curGradTarget;
              supGradTargets[supGradTargCount].tDir = tDir;
              supGradTargCount++;
            }
            else
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(contSizeExceeded, __FILE__, __LINE__);
            }

            // If the distance of the current supervise target to its parent is more than train length, there is no need to continue adding 
            // more supervise target for the same parent
            const uint32_t distToParent = static_cast<uint32_t>(ATC::ATCMath::instance().absolute((curGradTarget->getOdometer() - supTargOdo),
              __FILE__, __LINE__));

            if (distToParent >= trainLength)
            {
              breakSupLoop = true;
            }
            // Update the position of the next potential supervise target
            if (tDir == DirForward)
            {
              supTargOdo += static_cast<int32_t>(sectionLength);
            }
            else
            {
              supTargOdo -= static_cast<int32_t>(sectionLength);
            }
          }
        } // End of add supervise targets for current gradient loop

        // All possible supervised targets for the current gradient are added
        // Check if there is a next gradient to process and set the position of next supervised target
        if ((targetIndex + 1U) < gradTargCount)  // If next target exists
        {
          DS::GradientTarget* nextGradTarget = gradientTargets[targetIndex + 1U];
          supTargOdo = nextGradTarget->getOdometer();
          targetIndex++;
        }
        else
        {
          breakGradLoop = true;
        }
      }

      vfwVisitCheckPoint(&endCp, "TC_createSuperviseGradTargets_end");
    }

    /******************************************************************************
    * calcGradient
    ******************************************************************************/
    void AbstractTargetCalculation::calcGradient(DS::GradientTarget* const gradientTargets[], const uint8_t gradTargCount,
      SupGradInfo supGradTargets[], const uint8_t supGradTargCount, const uint32_t trainLength) const
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "TC_calcGradient_begin");

      const TravelDir tDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
      const OdoPosition curMaEnd = DS::AbstractTargets::corePtr()->getPrimaryTarget()->getOdometer();
      uint8_t curTargIndex = 0U;

      while (curTargIndex < supGradTargCount) // loop through gradients
      {
        SupGradInfo& supGradTarg = supGradTargets[curTargIndex];
        const OdoPosition curSupTargOdo = supGradTarg.odoPos;
        const uint32_t distToParentTarg = static_cast<uint32_t>(
          ATC::ATCMath::instance().absolute((curSupTargOdo - supGradTarg.parentTarget->getOdometer()), __FILE__, __LINE__));

        // If the distance to parent gradient target is more than the train length then the gradient for the supervised target
        // is the same as the parent's track gradient
        if (distToParentTarg >= trainLength)
        {
          DS::GradientTarget* parent = ATC::dynamicCast<DS::BaseTarget*, DS::GradientTarget*>(supGradTarg.parentTarget, __FILE__, __LINE__);
          supGradTarg.gradient = parent->getTrackGradient();
        }
        else
        {
          // Find the region length: this is the distance from current supervised gradient target to the next supervised gradient target or ma end
          int32_t curSupRegion = 0;
          if ((curTargIndex + 1U) == supGradTargCount) // this is the last supervised target
          {
            curSupRegion = ATC::ATCMath::instance().absolute((curSupTargOdo - curMaEnd), __FILE__, __LINE__);
          }
          else
          {
            curSupRegion = ATC::ATCMath::instance().absolute((curSupTargOdo - supGradTargets[curTargIndex + 1U].odoPos), __FILE__, __LINE__);
          }

          // Create 3 areas to calculate the gradient.
          // 1. "Minimum gradient area" starts from (curSupTargOdo - trainLength) to beginning of the product area. We are only interested in the 
          //    minimum of all gradients in this area
          // 2. "Product calculation area" starts form (curSupTargOdo - trainLength + curSupRegion) to curSupTargOdo. We need to calculate the sum
          //    of a product of all 
          // track gradients times the length of the gradient region.
          // 3. "Current supervise target region", of which we are interested in the track gradient

          OdoPosition minRegionStart;
          OdoPosition proRegionStart;

          if (tDir == DirForward)
          {
            minRegionStart = curSupTargOdo - static_cast<int32_t>(trainLength);
            proRegionStart = minRegionStart + curSupRegion;
          }
          else
          {
            minRegionStart = curSupTargOdo + static_cast<int32_t>(trainLength);
            proRegionStart = minRegionStart - curSupRegion;
          }

          //////// calculate the minimum gradient of "Minimum gradient area" and "Current supervise target region"
          bool breakFirstLoop = false;
          uint8_t targetIndex = 0U;

          // Find the closest gradient before start of "Minimum gradient area"
          // The first gradient to use for the minimum is track gradient of the closest target before the "Minimum gradient area" start,
          // If there are no targets before region start use the current track gradient
          int32_t lastAccessedGradient = DS::AbstractTargets::corePtr()->getCurTrackGradient();
          while ((targetIndex < gradTargCount) && (!breakFirstLoop))
          {
            OdoPosition gradTargOdo = gradientTargets[targetIndex]->getOdometer();
            bool gradBeforeMinRegionStart = (tDir == DirForward) ? (gradTargOdo <= minRegionStart) : (gradTargOdo >= minRegionStart);
            if (gradBeforeMinRegionStart)
            {
              lastAccessedGradient = gradientTargets[targetIndex]->getTrackGradient(); // reset to the closest gradient to the minRegionStart
              targetIndex++;
            }
            else
            {
              breakFirstLoop = true;
            }
          }

          int32_t minGradient = lastAccessedGradient;

          // Find the rest of gradient targets on the "Minimum gradient area" and continue to use them for the minimum
          bool breakSecondLoop = false;
          while ((targetIndex < gradTargCount) && (!breakSecondLoop))
          {
            OdoPosition gradTargOdo = gradientTargets[targetIndex]->getOdometer();
            bool gradInMinRegion = (tDir == DirForward) ? ((gradTargOdo > minRegionStart) && (gradTargOdo <= proRegionStart)) :
              ((gradTargOdo < minRegionStart) && (gradTargOdo >= proRegionStart));
            if (gradInMinRegion)
            {
              lastAccessedGradient = gradientTargets[targetIndex]->getTrackGradient();
              minGradient = ATC::ATCMath::minimum(minGradient, lastAccessedGradient);
              targetIndex++;
            }
            else
            {
              breakSecondLoop = true;
            }
          }

          // The minimum shall also include the track gradient of "Current supervise target region"
          DS::GradientTarget* parent = ATC::dynamicCast<DS::BaseTarget*, DS::GradientTarget*>(supGradTarg.parentTarget, __FILE__, __LINE__);
          minGradient = ATC::ATCMath::minimum(minGradient, parent->getTrackGradient());

          int64_t gradLenProduct = ATC::ATCMath::instance().signMul(minGradient, curSupRegion, __FILE__, __LINE__);

          //////// calculate the product of all gradient times their respective subregion size in the  "Product calculation area"
          bool breakProdRegionLoop = false;
          while (!breakProdRegionLoop)
          {
            uint32_t curSubRegionSize = 0U;
            int32_t curSubRegionGradient = lastAccessedGradient;

            if (targetIndex < gradTargCount) // If next Gradient available (index is already pointing at next gradient)
            {
              OdoPosition nextGradientOdo = gradientTargets[targetIndex]->getOdometer();

              bool nextGradInProReagion = (tDir == DirForward) ? ((nextGradientOdo >= proRegionStart) && (nextGradientOdo < curSupTargOdo)) :
                ((nextGradientOdo <= proRegionStart) && (nextGradientOdo > curSupTargOdo));
              if (nextGradInProReagion)
              {
                curSubRegionSize = static_cast<uint32_t>(ATC::ATCMath::instance().absolute((proRegionStart - nextGradientOdo), __FILE__, __LINE__));
                proRegionStart = nextGradientOdo; // Update the start of the product region for next loop
                lastAccessedGradient = gradientTargets[targetIndex]->getTrackGradient();
              }
              else // The whole remaining region belongs to the current gradient
              {
                curSubRegionSize = static_cast<uint32_t>(ATC::ATCMath::instance().absolute((proRegionStart - curSupTargOdo), __FILE__, __LINE__));
                breakProdRegionLoop = true;
              }
            }
            else // The whole remaining region belongs to the current gradient
            {
              curSubRegionSize = static_cast<uint32_t>(ATC::ATCMath::instance().absolute((proRegionStart - curSupTargOdo), __FILE__, __LINE__));
              breakProdRegionLoop = true;
            }
            gradLenProduct += ATC::ATCMath::instance().signMul(curSubRegionGradient, curSubRegionSize, __FILE__, __LINE__);
            targetIndex++;
          }
          int32_t calcGrad = ATC::ATCMath::instance().signDiv(gradLenProduct, static_cast<int32_t>(trainLength), __FILE__, __LINE__);
          supGradTarg.gradient = calcGrad;
        }

        // Add the supervised gradient target only if it is not equal to previous gradient and then proceed to the next gradient
        // In case of Ma extension it is possible that the last supervise gradient of the previous Ma, is the same as the first gradient of this Ma

        // Create the target and add it to supGradTargets array
        SupGradInfo curSupvGradInfo = supGradTarg;
        TrackAndPos supTargetTrack = DS::AbstractTracks::corePtr()->calculateTrackAndPos(curSupvGradInfo.odoPos);

        DS::SupervisedTarget sTarget(DS::SupervisedTarget::GradientSupvTarget, curSupvGradInfo.parentTarget, supTargetTrack,
          curSupvGradInfo.tDir, curSupvGradInfo.odoPos);

        sTarget.setGradient(curSupvGradInfo.gradient);

        if (curTargIndex == 0U)
        {
          DS::AbstractTargets::corePtr()->addTarget(sTarget);
        }
        else
        {
          if (curSupvGradInfo.gradient != supGradTargets[curTargIndex - 1U].gradient)
          {
            DS::AbstractTargets::corePtr()->addTarget(sTarget);
          }
        }

        curTargIndex++;
      }

      vfwVisitCheckPoint(&endCp, "TC_calcGradient_end");
    }

    /******************************************************************************
    * update Gradient and Ceiling speed for targets
    ******************************************************************************/
    void AbstractTargetCalculation::updateGradientAndCeilingSpeed(const uint32_t maxTrainSpeed)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "TC_updateGradientAndCeilingSpeed_begin");

      TravelDir suppTravelDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();

      const BrakeCalculations& bc = BrakeCalculations::instance();

      //calculate limits for max speed
      const uint32_t maxSWCeilingSpeed = bc.calcWarningLimitMargin(maxTrainSpeed) + maxTrainSpeed;
      const uint32_t maxSBCeilingSpeed = bc.calcServiceBrakeLimitMargin(maxTrainSpeed) + maxTrainSpeed;
      const uint32_t maxEBCeilingSpeed = bc.calcEmergencyBrakeLimitMargin(maxTrainSpeed) + maxTrainSpeed;

      //initialize the variables with current speed and gradient
      int32_t currGradient = DS::AbstractTargets::corePtr()->getCurGradient();

      // Use current FW CS for current CS as it might happen that the train is in between EB CS target and CS target and there are
      // no supervised targets ahead till MA end. So when an MA extension is received, it should use the CS of the ceiling speed target,
      // not current CS for new supervised targets.
      uint32_t currCeilingSpeed = DS::AbstractTargets::corePtr()->getCurrFwCeilingSpeed();

      uint32_t currCalcFWCeilingSpeed = DS::AbstractTargets::corePtr()->getCurrFwCeilingSpeed();
      uint32_t currCalcSWCeilingSpeed = DS::AbstractTargets::corePtr()->getCurrSwCeilingSpeed();
      uint32_t currCalcSBCeilingSpeed = DS::AbstractTargets::corePtr()->getCurrSbCeilingSpeed();
      uint32_t currCalcEBCeilingSpeed = DS::AbstractTargets::corePtr()->getCurrEbCeilingSpeed();

      DS::SupervisedTargetIterator supTargetItr = DS::AbstractTargets::corePtr()->getSupervisedTargetIter();
      //Iterate over the old supervised targets
      while (supTargetItr != DS::AbstractTargets::corePtr()->getSupervisedTargetIterEnd())
      {
        DS::SupervisedTarget* sTarget = *supTargetItr;

        //if the target has direction same as current travel direction.
        if (sTarget->getDirection() == suppTravelDir)
        {
          /* If the supervised target is ahead of MA start it is new target 
          * OR in case of MA extension if target are ahead of the position where the sb speed is equal to release speed,
          *   it will be treated as new target and CS will be recalculated (release speed sb position is updated when curve speeds are updated).
          * OR If the parent of the supervised target is ahead of the MA start even though the supervised target is behind MA start,
          *  it is a new target.
          * CS target can have supervised targets behind MA start.*/
          const  OdoPosition supervisedTargetOdo = sTarget->getOdometer();
          const bool isNewSupvTarget = (suppTravelDir == DirForward) ?
            ((sTarget->getParentTarget()->getOdometer() >= maStartPos) || (supervisedTargetOdo >= releaseSpSBPos)) :
            ((sTarget->getParentTarget()->getOdometer() <= maStartPos) || (supervisedTargetOdo <= releaseSpSBPos));

          if (!isNewSupvTarget)
          {
            //update the gradient and cs to the last supervised target speeds if this MA is an extension
            currGradient = sTarget->getGradient();

            // Use current FW CS for current CS as it might happen that the train is in between EB CS target and CS target and there are
            // no supervised targets ahead till MA end. So when an MA extension is received, it should use the CS of the ceiling speed target,
            // not current CS for new supervised targets.
            currCeilingSpeed = sTarget->getFwCeilingSpeed();

            currCalcFWCeilingSpeed = sTarget->getFwCeilingSpeed();
            currCalcSWCeilingSpeed = sTarget->getSwCeilingSpeed();
            currCalcSBCeilingSpeed = sTarget->getSbCeilingSpeed();
            currCalcEBCeilingSpeed = sTarget->getEbCeilingSpeed();
          }
          else
          {
            break;
          }
        }

        ++supTargetItr;
      }

      //Find the next CS MA target. If there are no CS target, set it to MA end primary target.
      //the CS MA target is found starting from the position of the first supervised target to process.
      //Note that this can be before the MA start position if the MA extension has CS target which creates supervised targets before MA start
      DS::MaTargetIterator csMATargetItr = DS::AbstractTargets::corePtr()->getMATargetIter();
      if(supTargetItr != DS::AbstractTargets::corePtr()->getSupervisedTargetIterEnd())
      {
        csMATargetItr = findNextCSTarget(DS::AbstractTargets::corePtr()->getMATargetIter(),
            (*supTargetItr)->getOdometer(), currCalcFWCeilingSpeed);
      }

      //Iterate over the newly added supervised targets
      while (supTargetItr != DS::AbstractTargets::corePtr()->getSupervisedTargetIterEnd())
      {
        DS::SupervisedTarget* sTarget = *supTargetItr;

        //if the target has direction same as current travel direction.
        if(sTarget->getDirection() == suppTravelDir)
        {
          DS::BaseTarget* csMaTarget = (*csMATargetItr);
          OdoPosition supvTargetOdo = sTarget->getOdometer();
          //get next CS target if the old one is passed
          bool isCSMATargetPassed = (csMaTarget->getDirection() == DirForward)?
                                  (csMaTarget->getOdometer() <= supvTargetOdo):
                                  (csMaTarget->getOdometer() >= supvTargetOdo);

          //remove all speed targets between CS MA target and supervised target
          while((csMaTarget->getTargetType() != DS::BaseTarget::PrimaryTarget) && isCSMATargetPassed)
          {
            //remove the CS target from the vectors except if it is a CS increase target. They are not added to vector.
            //The CS target should be in the vectors as the items are pushed back into vector when the supervised target is processed.
            //Safety Halt if the target is not found in the vector.
            removeCsFromVector(fwVector, csMaTarget->getTargetId());
            removeCsFromVector(swVector, csMaTarget->getTargetId());
            removeCsFromVector(sbVector, csMaTarget->getTargetId());
            removeCsFromVector(ebVector, csMaTarget->getTargetId());

            //update the ceiling speed.
            currCeilingSpeed = csMaTarget->getSpeedChange();

            // find the next CS target which reduces the speed.
            csMATargetItr = findNextCSTarget(csMATargetItr, csMaTarget->getOdometer(),csMaTarget->getSpeedChange());

            csMaTarget = (*csMATargetItr);

            isCSMATargetPassed = (csMaTarget->getDirection() == DirForward)?
                                (csMaTarget->getOdometer() <= supvTargetOdo):
                                (csMaTarget->getOdometer() >= supvTargetOdo);
          }

          switch (sTarget->getSupervisedTargetType())
          {
            case DS::SupervisedTarget::GradientSupvTarget:
            {
              currGradient = sTarget->getGradient();
              break;
            }

            case DS::SupervisedTarget::FWSpeedTarget:
            {
              if (fwVector.size() < maxSizeCurveVectors)
              {
                fwVector.push_back(sTarget->getParentTarget());
              }
              else
              {
                ATC::AbstractEventHandler::corePtr()->reportEvent(contSizeExceeded, __FILE__, __LINE__);
              }

              const uint32_t fwCeilingSpeed = sTarget->getParentTarget()->getSpeedChange();
              currCalcFWCeilingSpeed =  ATC::ATCMath::minimum(currCalcFWCeilingSpeed, fwCeilingSpeed);
              break;
            }

            case DS::SupervisedTarget::SWSpeedTarget:
            {
              if (swVector.size() < maxSizeCurveVectors)
              {
                swVector.push_back(sTarget->getParentTarget());
              }
              else
              {
                ATC::AbstractEventHandler::corePtr()->reportEvent(contSizeExceeded, __FILE__, __LINE__);
              }

              uint32_t swCeilingSpeed = sTarget->getParentTarget()->getSpeedChange();
              swCeilingSpeed += bc.calcWarningLimitMargin(swCeilingSpeed);
              currCalcSWCeilingSpeed =  ATC::ATCMath::minimum(currCalcSWCeilingSpeed, swCeilingSpeed);
              break;
            }

            case DS::SupervisedTarget::SBSpeedTarget:
            {
              if (sbVector.size() < maxSizeCurveVectors)
              {
                sbVector.push_back(sTarget->getParentTarget());
              }
              else
              {
                ATC::AbstractEventHandler::corePtr()->reportEvent(contSizeExceeded, __FILE__, __LINE__);
              }

              uint32_t sbCeilingSpeed = sTarget->getParentTarget()->getSpeedChange();
              sbCeilingSpeed += bc.calcServiceBrakeLimitMargin(sbCeilingSpeed);
              currCalcSBCeilingSpeed =  ATC::ATCMath::minimum(currCalcSBCeilingSpeed, sbCeilingSpeed);
              break;
            }

            case DS::SupervisedTarget::EBSpeedTarget:
            {
              if (ebVector.size() < maxSizeCurveVectors)
              {
                ebVector.push_back(sTarget->getParentTarget());
              }
              else
              {
                ATC::AbstractEventHandler::corePtr()->reportEvent(contSizeExceeded, __FILE__, __LINE__);
              }

              uint32_t ebCeilingSpeed = sTarget->getParentTarget()->getSpeedChange();
              ebCeilingSpeed += bc.calcEmergencyBrakeLimitMargin(ebCeilingSpeed);
              currCalcEBCeilingSpeed =  ATC::ATCMath::minimum(currCalcEBCeilingSpeed, ebCeilingSpeed);
              break;
            }

            case DS::SupervisedTarget::FWPrimaryTarget:
            {
              currCalcFWCeilingSpeed = 0U;
              break;
            }

            case DS::SupervisedTarget::SWPrimaryTarget:
            {
              currCalcFWCeilingSpeed = 0U;
              currCalcSWCeilingSpeed = 0U;
              break;
            }

            case DS::SupervisedTarget::SBPrimaryTarget:
            {
              currCalcFWCeilingSpeed = 0U;
              currCalcSWCeilingSpeed = 0U;
              currCalcSBCeilingSpeed = 0U;
              break;
            }

            case DS::SupervisedTarget::EBPrimaryTarget:
            {
              currCalcFWCeilingSpeed = 0U;
              currCalcSWCeilingSpeed = 0U;
              currCalcSBCeilingSpeed = 0U;
              currCalcEBCeilingSpeed = 0U;
              break;
            }

            case DS::SupervisedTarget::SpeedIncreaseTarget:
            {
              //calculate the speed limits for the speed increase.
              const uint32_t fwCeilingSpeed = sTarget->getParentTarget()->getSpeedChange();
              const uint32_t swCeilingSpeed = bc.calcWarningLimitMargin(fwCeilingSpeed) + fwCeilingSpeed;
              const uint32_t sbCeilingSpeed = bc.calcServiceBrakeLimitMargin(fwCeilingSpeed) + fwCeilingSpeed;
              const uint32_t ebCeilingSpeed = bc.calcEmergencyBrakeLimitMargin(fwCeilingSpeed) + fwCeilingSpeed;

              // calculate minimums of the current speed and all targets in the vector.
              // update the ceiling speeds for speed increase if the current CS is not zero.
              // this can happen if the speed increase is in between the SB primary target and primary target.

              currCalcFWCeilingSpeed = (currCalcFWCeilingSpeed > 0U) ?
                                       (ATC::ATCMath::minimum(fwCeilingSpeed, getMinCSinVector(fwVector))) : 0U;

              uint32_t minSpeedVector = getMinCSinVector(swVector);
              minSpeedVector += bc.calcWarningLimitMargin(minSpeedVector);
              currCalcSWCeilingSpeed = (currCalcSWCeilingSpeed > 0U) ?
                                       (ATC::ATCMath::minimum(swCeilingSpeed, minSpeedVector)) : 0U;

              minSpeedVector = getMinCSinVector(sbVector);
              minSpeedVector += bc.calcServiceBrakeLimitMargin(minSpeedVector);
              currCalcSBCeilingSpeed = (currCalcSBCeilingSpeed > 0U) ?
                                       (ATC::ATCMath::minimum(sbCeilingSpeed, minSpeedVector)) : 0U;

              minSpeedVector = getMinCSinVector(ebVector);
              minSpeedVector += bc.calcEmergencyBrakeLimitMargin(minSpeedVector);
              currCalcEBCeilingSpeed = (currCalcEBCeilingSpeed > 0U) ?
                                       (ATC::ATCMath::minimum(ebCeilingSpeed, minSpeedVector)) : 0U;

              //update ceiling speed
              currCeilingSpeed = sTarget->getParentTarget()->getSpeedChange();

              break;
            }

            case DS::SupervisedTarget::SMSpeedRestrictionTarget:
            case DS::SupervisedTarget::Undefined:
            default:
            {
              break;
            }

          }

          //update the current gradient if the supervised target gradient is already valid.
          if(sTarget->isGradientValid())
          {
            currGradient = sTarget->getGradient();
          }

          //set speeds and gradient for supervised target
          sTarget->setCeilingSpeed(ATC::ATCMath::minimum(currCeilingSpeed,maxTrainSpeed));
          sTarget->setFwCeilingSpeed(ATC::ATCMath::minimum(currCalcFWCeilingSpeed, maxTrainSpeed));
          sTarget->setSwCeilingSpeed(ATC::ATCMath::minimum(currCalcSWCeilingSpeed, maxSWCeilingSpeed));
          sTarget->setSbCeilingSpeed(ATC::ATCMath::minimum(currCalcSBCeilingSpeed, maxSBCeilingSpeed));
          sTarget->setEbCeilingSpeed(ATC::ATCMath::minimum(currCalcEBCeilingSpeed, maxEBCeilingSpeed));
          sTarget->setGradient(currGradient);
        }

        ++supTargetItr;
      }

      vfwVisitCheckPoint(&endCp, "TC_updateGradientAndCeilingSpeed_end");
    }

    /******************************************************************************
    * calcDecSpeedAndSetCurveSpeeds
    ******************************************************************************/
    uint32_t AbstractTargetCalculation::calcDecSpeedAndSetCurveSpeeds(DS::SupervisedTarget* const sTarget, const ATP::TravelDir stDir, const int32_t gradient, const int32_t deceleration, const DS::SupervisedTarget * const nextTarget) const
    {
      const int32_t distTP = ATC::ATCMath::instance().absolute((nextTarget->getOdometer() - sTarget->getOdometer()), __FILE__, __LINE__);
      const BrakeCalculations& bc = BrakeCalculations::instance();

      //calculate the deceleration curve speed at the target
      const uint32_t decSpeed = bc.calcDecelerationCurveSpeed(gradient, bc.getEBCurveDelay(), static_cast<uint32_t>(distTP), static_cast<int32_t>(nextTarget->getEBSpeed()), deceleration);

      uint32_t curveSpeed = bc.calcFirstWarningCurveSpeed(sTarget->getOdometer(), stDir, gradient,
        deceleration, sTarget->getFwCeilingSpeed(), *nextTarget);
      // The fw speed is the minimum value between the curveSpeed and ceilingSpeed
      sTarget->setFirstWarningSpeed(curveSpeed);

      curveSpeed = bc.calcSecondWarningCurveSpeed(sTarget->getOdometer(), stDir, gradient,
        deceleration, sTarget->getSwCeilingSpeed(), *nextTarget);
      // The sw speed is the minimum value between the curveSpeed and ceilingSpeed
      sTarget->setSecondWarningSpeed(curveSpeed);

      curveSpeed = bc.calcServiceBrakeCurveSpeed(sTarget->getOdometer(), stDir, gradient,
        deceleration, sTarget->getSbCeilingSpeed(), *nextTarget);
      // The sb speed is the minimum value between the curveSpeed and ceilingSpeed
      sTarget->setSBSpeed(curveSpeed);

      curveSpeed = bc.calcEmergencyBrakeCurveSpeed(sTarget->getOdometer(), stDir, gradient,
        deceleration, sTarget->getEbCeilingSpeed(), *nextTarget);
      // The eb speed is the minimum value between the curveSpeed and ceilingSpeed
      sTarget->setEBSpeed(curveSpeed);

      return decSpeed;
    }

    /******************************************************************************
    * updateCurveSpeedsForTargets
    ******************************************************************************/
    void AbstractTargetCalculation::updateCurveSpeedsForTargets()
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "TC_updateCurveSpeedsForTargets_begin");

      TravelDir stDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();

      bool isModeLocation = (Kernel::AbstractModeControl::corePtr()->getCurrentMode() == ATPModeLocation);
#ifdef _SYS_INT
      uint32_t releaseSpeed = 0U;
#else
      uint32_t releaseSpeed = AbstractConfig::corePtr()->getReleaseSpeed();
#endif

      DS::SupervisedTargetRevIterator it = DS::AbstractTargets::corePtr()->getSupervisedRevTargetIter();
      DS::SupervisedTarget* nextTarget = static_cast<DS::SupervisedTarget*>(NULL);
      uint32_t decSpeed = 0U;
      bool relSpEBPosCalc = false;
      bool relSpSBPosCalc = false;
      bool relSpFWPosCalc = false;

      if(smDelta == 0U)
      {
        relSpFWPosCalc = true;
      }

      const BrakeCalculations& bc = BrakeCalculations::instance();

      if (it != DS::AbstractTargets::corePtr()->getSupervisedRevTargetIterEnd())
      {
        // nextGradTarget is the next "correct" target in travel direction
        nextTarget = *it;
        int32_t gradient = 0;
        int32_t deceleration = 0;

        while (it != DS::AbstractTargets::corePtr()->getSupervisedRevTargetIterEnd())
        {
          DS::SupervisedTarget* sTarget = *it;

          // Only work with targets that have the same direction as travel Direction
          if ((stDir == sTarget->getDirection()))
          {
            if (DS::BaseTarget::SupervisedTarget == sTarget->getTargetType()) // Do not set and use permitted speed for TrackDataItemTarget
            {
              DS::SupervisedTarget::SupervisedTargetType sTargetType = sTarget->getSupervisedTargetType();
              //// Setting the permitted speed ////
              if (DS::SupervisedTarget::EBPrimaryTarget == sTargetType)
              {
                // set all the speed for primary target to zero
                sTarget->setFirstWarningSpeed(0U);
                sTarget->setSecondWarningSpeed(0U);
                sTarget->setSBSpeed(0U);
                sTarget->setEBSpeed(0U);

                if(isModeLocation)
                {
                  releaseSpEBPos = sTarget->getOdometer();
                  relSpEBPosCalc = true;
                }
              }
              else if (DS::SupervisedTarget::SMSpeedRestrictionTarget == sTargetType)
              {
                //Do nothing as the curve speed is set when adding the target.
              }
              else
              {
                if ((DS::SupervisedTarget::SBPrimaryTarget == sTargetType) && (isModeLocation))
                {
                  releaseSpSBPos = sTarget->getOdometer();
                  relSpSBPosCalc = true;
                }

                uint32_t curveSpeed;

                gradient = sTarget->getGradient();

                /*
                * Worst brakeability will be used for the different curve calculation
                * Worst brakeability will be calculated between:
                * EB ceiling speed (max speed) at current target and FW speed ((minimum speed) at next target.
                */
                int32_t brakeability = DS::AbstractTSetup::corePtr()->getWorstBrakeabilityInRange(
                                                        nextTarget->getFirstWarningSpeed(), sTarget->getEbCeilingSpeed());

                deceleration = bc.calcDeceleration(brakeability, gradient);

                // update the curve speeds of the last target if the gradient is different
                // if the last target was EB primiary target, no need to update the speeds
                DS::SupervisedTarget::SupervisedTargetType nextTargetType = nextTarget->getSupervisedTargetType();

                if((gradient != nextTarget->getGradient()) && 
                  ((DS::SupervisedTarget::EBPrimaryTarget != nextTargetType) && 
                  (DS::SupervisedTarget::SMSpeedRestrictionTarget != nextTargetType)))
                {
                  //no need to reduce the speeds for next target if the curve speeds are already at ceiling speeds
                  if (nextTarget->getFirstWarningSpeed() < nextTarget->getFwCeilingSpeed())
                  {
                    curveSpeed = calcCurveSpeedUpdate(nextTarget->getFirstWarningSpeed(), gradient, deceleration, decSpeed,
                      bc.get1stwarnCurveDelay(), nextTarget->getFwCeilingSpeed());
                    nextTarget->setFirstWarningSpeed(curveSpeed);
                  }

                  if (nextTarget->getSecondWarningSpeed() < nextTarget->getSwCeilingSpeed())
                  {
                    curveSpeed = calcCurveSpeedUpdate(nextTarget->getSecondWarningSpeed(), gradient, deceleration, decSpeed,
                      bc.get2ndwarnCurveDelay(), nextTarget->getSwCeilingSpeed());
                    nextTarget->setSecondWarningSpeed(curveSpeed);
                  }

                  if (nextTarget->getSBSpeed() < nextTarget->getSbCeilingSpeed())
                  {
                    curveSpeed = calcCurveSpeedUpdate(nextTarget->getSBSpeed(), gradient, deceleration, decSpeed,
                      bc.getSBCurveDelay(), nextTarget->getSbCeilingSpeed());
                    nextTarget->setSBSpeed(curveSpeed);
                  }

                  if (nextTarget->getEBSpeed() < nextTarget->getEbCeilingSpeed())
                  {
                    curveSpeed = calcCurveSpeedUpdate(nextTarget->getEBSpeed(), gradient, deceleration, decSpeed,
                        bc.getEBCurveDelay(), nextTarget->getEbCeilingSpeed());
                    nextTarget->setEBSpeed(curveSpeed);
                  }
                }

                //calculate the deceleration curve speed at the target
                decSpeed = calcDecSpeedAndSetCurveSpeeds(sTarget, stDir, gradient, deceleration, nextTarget);

                if((!isModeLocation) && (!relSpSBPosCalc))
                {
                  uint32_t sbSpeed = sTarget->getSBSpeed();
                  uint32_t nextSbSpeed = nextTarget->getSBSpeed();
                  if((nextSbSpeed <= releaseSpeed) && (sbSpeed > releaseSpeed))
                  {
                    int32_t dist = bc.calcCurveDistance(releaseSpeed, bc.getSBCurveDelay(), gradient, deceleration, nextTarget->getSBSpeed());
                    releaseSpSBPos = (stDir == DirForward)?(nextTarget->getOdometer() - dist):(nextTarget->getOdometer() + dist);
                    relSpSBPosCalc = true;
                  }
                }

                if((!isModeLocation) && (!relSpEBPosCalc))
                {
                  uint32_t ebSpeed = sTarget->getEBSpeed();
                  uint32_t nextEbSpeed = nextTarget->getEBSpeed();
                  if((nextEbSpeed <= releaseSpeed) && (ebSpeed > releaseSpeed))
                  {
                    int32_t dist = bc.calcCurveDistance(releaseSpeed, bc.getEBCurveDelay(), gradient, deceleration, nextTarget->getEBSpeed());
                    releaseSpEBPos = (stDir == DirForward)?(nextTarget->getOdometer() - dist):(nextTarget->getOdometer() + dist);
                    relSpEBPosCalc = true;
                  }
                }

                if((!isModeLocation) && (!relSpFWPosCalc))
                {
                  uint32_t fwSpeed = sTarget->getFirstWarningSpeed();
                  uint32_t nextFwSpeed = nextTarget->getFirstWarningSpeed();

#ifdef _SYS_INT
                  releaseSpeed = AbstractConfig::corePtr()->getReleaseSpeed();
#endif
                  if((nextFwSpeed <= releaseSpeed) && (fwSpeed > releaseSpeed))
                  {
                    OdoPosition dist = bc.calcCurveDistance(releaseSpeed, bc.get1stwarnCurveDelay(), gradient, deceleration, nextTarget->getFirstWarningSpeed());
                    dist = dist + static_cast<OdoPosition>(smDelta);
                    OdoPosition releaseSpFwPos = (stDir == DirForward)?(nextTarget->getOdometer() - dist):(nextTarget->getOdometer() + dist);
                    relSpFWPosCalc = true;

                    //add supervised target
                    TrackAndPos supTargetTrack = DS::AbstractTracks::corePtr()->calculateTrackAndPos(releaseSpFwPos);
                    DS::SupervisedTarget smTarget(DS::SupervisedTarget::SMSpeedRestrictionTarget, lastSmTDI, supTargetTrack,
                      stDir, releaseSpFwPos);

                    smTarget.setFwCeilingSpeed(releaseSpeed);
                    smTarget.setSwCeilingSpeed(bc.calcWarningLimitMargin(releaseSpeed) + releaseSpeed);
                    smTarget.setSbCeilingSpeed(bc.calcServiceBrakeLimitMargin(releaseSpeed) + releaseSpeed);
                    smTarget.setEbCeilingSpeed(bc.calcEmergencyBrakeLimitMargin(releaseSpeed) + releaseSpeed);

                    smTarget.setFirstWarningSpeed(releaseSpeed);
                    smTarget.setSecondWarningSpeed(bc.calcWarningLimitMargin(releaseSpeed) + releaseSpeed);
                    smTarget.setSBSpeed(bc.calcServiceBrakeLimitMargin(releaseSpeed) + releaseSpeed);
                    smTarget.setEBSpeed(bc.calcEmergencyBrakeLimitMargin(releaseSpeed) + releaseSpeed);

                    DS::AbstractTargets::corePtr()->addTarget(smTarget);

                    if (isOdometerValueLessThan(stDir, sTarget->getOdometer(), smTarget.getOdometer()))
                    {
                      decSpeed = calcDecSpeedAndSetCurveSpeeds(sTarget, stDir, gradient, deceleration, &smTarget);
                    }
                  }
#ifdef _SYS_INT
                  releaseSpeed = 0;
#endif
                }

              }

              // Keep the iterator as the next target, so next round that we have entered the loop and also have the correct
              // direction and type, NextTarget will be pointing to the "next target" in the direction of the train
              nextTarget = sTarget;
            } // end if not TrackDataItemTarget

          }
          ++it;
        }

        //calculate the release speed positions if not calculated within the loop
        if(!relSpSBPosCalc)
        {
          int32_t dist = bc.calcCurveDistance(releaseSpeed, bc.getSBCurveDelay(), gradient, deceleration, nextTarget->getSBSpeed());
          releaseSpSBPos = (stDir == DirForward)?(nextTarget->getOdometer() - dist):(nextTarget->getOdometer() + dist);
        }

        if(!relSpEBPosCalc)
        {
          int32_t dist = bc.calcCurveDistance(releaseSpeed, bc.getEBCurveDelay(), gradient, deceleration, nextTarget->getEBSpeed());
          releaseSpEBPos = (stDir == DirForward)?(nextTarget->getOdometer() - dist):(nextTarget->getOdometer() + dist);
        }

        if(!relSpFWPosCalc)
        {
          OdoPosition dist = bc.calcCurveDistance(releaseSpeed, bc.get1stwarnCurveDelay(), gradient, deceleration, nextTarget->getFirstWarningSpeed());
          dist = dist + static_cast<OdoPosition>(smDelta);
          OdoPosition releaseSpFwPos = (stDir == DirForward)?(nextTarget->getOdometer() - dist):(nextTarget->getOdometer() + dist);

          //add supervised target
          TrackAndPos supTargetTrack = DS::AbstractTracks::corePtr()->calculateTrackAndPos(releaseSpFwPos);
          DS::SupervisedTarget smTarget(DS::SupervisedTarget::SMSpeedRestrictionTarget, lastSmTDI, supTargetTrack,
            stDir, releaseSpFwPos);
#ifdef _SYS_INT
          releaseSpeed = AbstractConfig::corePtr()->getReleaseSpeed();
#endif

          smTarget.setFwCeilingSpeed(releaseSpeed);
          smTarget.setSwCeilingSpeed(bc.calcWarningLimitMargin(releaseSpeed) + releaseSpeed);
          smTarget.setSbCeilingSpeed(bc.calcServiceBrakeLimitMargin(releaseSpeed) + releaseSpeed);
          smTarget.setEbCeilingSpeed(bc.calcEmergencyBrakeLimitMargin(releaseSpeed) + releaseSpeed);

          smTarget.setFirstWarningSpeed(releaseSpeed);
          smTarget.setSecondWarningSpeed(bc.calcWarningLimitMargin(releaseSpeed) + releaseSpeed);
          smTarget.setSBSpeed(bc.calcServiceBrakeLimitMargin(releaseSpeed) + releaseSpeed);
          smTarget.setEBSpeed(bc.calcEmergencyBrakeLimitMargin(releaseSpeed) + releaseSpeed);

          DS::AbstractTargets::corePtr()->addTarget(smTarget);

#ifdef _SYS_INT
          releaseSpeed = AbstractConfig::corePtr()->getReleaseSpeed();
#endif

        }

      }

      vfwVisitCheckPoint(&endCp, "TC_updateCurveSpeedsForTargets_end");
    }

    /******************************************************************************
    * updateCurveSpeedsForTarget
    ******************************************************************************/
    uint32_t AbstractTargetCalculation::calcCurveSpeedUpdate(const uint32_t currSpeed,const int32_t gradient,
        const int32_t deceleration, const uint32_t decSpeed, const uint32_t delay, const uint32_t ceilSpeed) const
    {
      uint32_t curveSpeed = 0U;

      if(ceilSpeed != 0U)
      {
        const BrakeCalculations& bc = BrakeCalculations::instance();

        //calculate curve speed with the new gradient.
        int32_t curveSp = bc.calcAllowedCurveSpeed(gradient, delay, decSpeed, deceleration);

        //curveSpeed should not be less than 0
        curveSp = ATC::ATCMath::maximum(curveSp, 0);

        // Update curve speed to minimum of the new and old curve speed.
        // No need to take min for ceil speed as currSpeed is always lower than ceil speed.
        curveSpeed = ATC::ATCMath::minimum(static_cast<uint32_t>(curveSp), currSpeed);
      }

      return curveSpeed;

    }

    /******************************************************************************
    * getMostRestrictiveTarget
    ******************************************************************************/
    DS::SupervisedTarget* AbstractTargetCalculation::getMostRestrictiveTarget() const
    {
      bool targFound = false;
      DS::SupervisedTarget* supvTarget = static_cast<DS::SupervisedTarget*>(NULL);
      TravelDir tDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
      const int32_t currLeadPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();

      DS::SupervisedTargetIterator it = DS::AbstractTargets::corePtr()->getSupervisedTargetIter();

      while((it != DS::AbstractTargets::corePtr()->getSupervisedTargetIterEnd()) && (!targFound))
      {
        supvTarget = *it;
        bool isTargetAhead = (supvTarget->getDirection() == DirForward)?(supvTarget->getOdometer() > currLeadPos):(supvTarget->getOdometer() < currLeadPos);

        // If the travel direction matches target direction and is ahead of current position
        if ((tDir == supvTarget->getDirection()) && isTargetAhead)
        {
          DS::SupervisedTarget::SupervisedTargetType sTargetType = supvTarget->getSupervisedTargetType();
          //Restrictive target cannot be a gradient and speed increase target
          if ((sTargetType != DS::SupervisedTarget::GradientSupvTarget) &&
            (sTargetType != DS::SupervisedTarget::SpeedIncreaseTarget))
          {
            // target is restrictive when ceilingSpeed and permitted speed are the same, which means that it is not restricted by any other target
            if (supvTarget->getFirstWarningSpeed() == supvTarget->getFwCeilingSpeed())
            {
              targFound = true;
            }
          }
        }
        ++it;
      }
      return supvTarget;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractTargetCalculation::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&calcCeilingSpeed));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&initialized));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&maStartFromScratch));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&maStartPos));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&contSizeExceeded));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&maHeadInvalid));
    }

    /******************************************************************************
    * getCeilingSpeed
    ******************************************************************************/
    uint32_t AbstractTargetCalculation::getCalcCeilingSpeed(void) const
    {
      return calcCeilingSpeed;
    }

    /******************************************************************************
    * getSectionLength
    ******************************************************************************/
    uint32_t AbstractTargetCalculation::getSectionLength(const uint32_t trainLen) const
    {
      const uint8_t configMaxNrSupervisedTargets = AbstractConfig::corePtr()->getMaxNrSupvGradTarget();

      // For any gradient minimum number "maximum" Supervised gradient targets should be 2 (One at the gradient
      // target point and one in a distance of train length from it)
      // i.e if the process of adding the supervised gradients is not interrupted by an Ma end or a new gradient
      const uint32_t nrOfSupervisedTargets = ATC::ATCMath::maximum(static_cast<uint32_t>(configMaxNrSupervisedTargets), 2U);

      uint32_t sectionLen = trainLen / nrOfSupervisedTargets;
      sectionLen = ATC::ATCMath::maximum(sectionLen, AbstractConfig::corePtr()->getMinSupvGradTargetDist());

      //section length cannot be more than the train length
      sectionLen = ATC::ATCMath::minimum(sectionLen, trainLen);

      return sectionLen;
    }

    /******************************************************************************
    * getMinCSinVector
    ******************************************************************************/
    uint32_t AbstractTargetCalculation::getMinCSinVector(std::vector<DS::BaseTarget*>& speedVector) const
    {
      uint32_t minValue = ATC::uint32Max;

      const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

      if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
      {
        minValue = pTrainSetup->maxSpeed;
      }

      for (std::vector<DS::BaseTarget*>::iterator it = speedVector.begin(); it != speedVector.end(); ++it)
      {
        minValue = ATC::ATCMath::minimum(minValue, (*it)->getSpeedChange());
      }

      return minValue;
    }

    /******************************************************************************
    * findNextCSTarget
    ******************************************************************************/
    DS::MaTargetIterator AbstractTargetCalculation::findNextCSTarget(DS::MaTargetIterator startItr,
        const OdoPosition startOdo, uint32_t currFWSpeed) const
    {
      DS::MaTargetIterator csItr  = DS::AbstractTargets::corePtr()->getMATargetIterEnd();
      TravelDir tDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();

      while(startItr != DS::AbstractTargets::corePtr()->getMATargetIterEnd())
      {
        if ((*startItr)->getDirection() == tDir)
        {
          if ((*startItr)->getTargetType() == DS::BaseTarget::SpeedTarget)
          {
            //check that the target is ahead of the MA start.
            bool isNewTarget = ((*startItr)->getDirection() == DirForward) ? ((*startItr)->getOdometer() > startOdo) :
              ((*startItr)->getOdometer() < startOdo);

            if (isNewTarget)
            {
              if ((*startItr)->getSpeedChange() < currFWSpeed)
              {
                csItr = startItr;
                break;
              }
              currFWSpeed = (*startItr)->getSpeedChange();
            }
          }
          else if ((*startItr)->getTargetType() == DS::BaseTarget::PrimaryTarget)
          {
            csItr = startItr;
          }
          else
          {
            //Do Nothing
          }
        }
        ++startItr;
      }

      return csItr;
    }

    /******************************************************************************
    * updateCeilingSpeeds
    ******************************************************************************/
    void AbstractTargetCalculation::updateCeilingSpeeds(const uint32_t currentCS) const
    {
      const BrakeCalculations& bc = BrakeCalculations::instance();
      const uint32_t currCalcFWCeilingSpeed = currentCS;
      const uint32_t currCalcSWCeilingSpeed = bc.calcWarningLimitMargin(currCalcFWCeilingSpeed) + currCalcFWCeilingSpeed;
      const uint32_t currCalcSBCeilingSpeed = bc.calcServiceBrakeLimitMargin(currCalcFWCeilingSpeed) + currCalcFWCeilingSpeed;
      const uint32_t currCalcEBCeilingSpeed = bc.calcEmergencyBrakeLimitMargin(currCalcFWCeilingSpeed) + currCalcFWCeilingSpeed;

      DS::AbstractTargets::corePtr()->setCurrFwCeilingSpeed(currCalcFWCeilingSpeed);
      DS::AbstractTargets::corePtr()->setCurrSwCeilingSpeed(currCalcSWCeilingSpeed);
      DS::AbstractTargets::corePtr()->setCurrSbCeilingSpeed(currCalcSBCeilingSpeed);
      DS::AbstractTargets::corePtr()->setCurrEbCeilingSpeed(currCalcEBCeilingSpeed);
    }

    /******************************************************************************
    * removeCsFromVector
    ******************************************************************************/
    void AbstractTargetCalculation::removeCsFromVector(std::vector<DS::BaseTarget*>& speedVector,const uint32_t csTargetId) const
    {
      bool isErased = false;
      for (std::vector<DS::BaseTarget*>::iterator it = speedVector.begin(); it != speedVector.end(); ++it)
      {
        if((*it)->getTargetId() == csTargetId)
        {
          static_cast<void>(speedVector.erase(it));
          isErased = true;
          break;
        }
      }

      if((speedVector.size() > 0U) && (!isErased))
      {
        writeToLog(ATC::BriefLog,"CS target not found in vector: ", csTargetId, __FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * getReleaseSpTarget
    ******************************************************************************/
    int32_t AbstractTargetCalculation::getReleaseSpSBPos() const
    {
      return releaseSpSBPos;
    }

    /******************************************************************************
    * getReleaseSpTarget
    ******************************************************************************/
    int32_t AbstractTargetCalculation::getReleaseSpEBPos() const
    {
      return releaseSpEBPos;
    }

  }
}
