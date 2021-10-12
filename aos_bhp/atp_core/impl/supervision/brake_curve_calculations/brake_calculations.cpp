/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The methods in this file are not scheduled and are executed upon request
*  from TargetCalculation or Supervise.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-13    Hidaji      Created
* 2016-09-26    Hidaji      Added functions and implementations
* 2016-09-30    Hidaji      changed calcSecondWarningCurveSpeed, atpBrakeCalId => atpBrakeCalcId
*                           Adjusted some hard-coded time delays
* 2016-10-05    Hidaji      Changed brakeReactionTime => brakeResponseTime
* 2016-10-10    Hidaji      Added calcDeceleration, and adjustGradient
* 2016-10-10    rquensel    Added calcPredictedDistanceToStandStillLocation
* 2016-10-12    rquensel    Added calcPredictedSpeedAtTarget, calcTimeToIntervention
* 2016-10-14    rquensel    Updated after review comments
* 2016-10-25    rquensel    Added config parameters
* 2016-12-28    nsyed       Re-factored into a class
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "brake_calculations.hpp"
#include "atc_math.hpp"
#include "abstract_config.hpp"
#include "atc_util.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_tsetup.hpp"
#include "brake_calculations_event_ids.hpp"
#include <vfw_checkpoints.h>
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_supervise.hpp"
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
    BrakeCalculations::BrakeCalculations() :
      errorSquareNegative(ATC::Event::createSafetyHaltEvent(atpBrakeCalcId, ATC::CoreContainer, eventIdErrSquareNeg, ATC::NoEB,
        DMICom::brakeCalInternalFailure, "Negative value used for square root!")),
      errorDecNegative(ATC::Event::createSafetyHaltEvent(atpBrakeCalcId, ATC::CoreContainer, eventIdDecNeg, ATC::NoEB,
        DMICom::brakeCalInternalFailure, "Negative value for deceleration + gradient"))
    {
      //Nothing to be done here
    }

    /******************************************************************************
    * instance
    ******************************************************************************/
    BrakeCalculations& BrakeCalculations::instance(void)
    {
      static BrakeCalculations brakeCalculationsInstance;

      return brakeCalculationsInstance;
    }

    /******************************************************************************
    * instance
    ******************************************************************************/
    void BrakeCalculations::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorSquareNegative));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorDecNegative));
    }

    /******************************************************************************
    * calcSpeedLimitMargin
    ******************************************************************************/
    uint32_t BrakeCalculations::calcSpeedLimitMargin(const uint32_t ceilingSpeed, const uint32_t permil, const uint32_t minValue,
      const uint32_t maxValue) const
    {
      return ATC::ATCMath::minimum(ATC::ATCMath::maximum((ceilingSpeed * permil) / 1000U, minValue), maxValue);
    }

    /******************************************************************************
    * calcWarningLimitMargin
    ******************************************************************************/
    uint32_t BrakeCalculations::calcWarningLimitMargin(const uint32_t ceilingSpeed) const
    {
      const uint32_t permilWarning = AbstractConfig::corePtr()->getSpeedMarginWarn();
      const uint32_t minValueWarning = AbstractConfig::corePtr()->getMinSpeedMarginWarn();
      const uint32_t maxValueWarning = AbstractConfig::corePtr()->getMaxSpeedMarginWarn();

      return calcSpeedLimitMargin(ceilingSpeed, permilWarning, minValueWarning, maxValueWarning);
    }

    /******************************************************************************
    * calcServiceBrakeLimitMargin
    ******************************************************************************/
    uint32_t BrakeCalculations::calcServiceBrakeLimitMargin(const uint32_t ceilingSpeed) const
    {
      const uint32_t permilSB = AbstractConfig::corePtr()->getSpeedMarginSB();
      const uint32_t minValueSB = AbstractConfig::corePtr()->getMinSpeedMarginSB();
      const uint32_t maxValueSB = AbstractConfig::corePtr()->getMaxSpeedMarginSB();

      return calcSpeedLimitMargin(ceilingSpeed, permilSB, minValueSB, maxValueSB);
    }

    /******************************************************************************
    * calcEmergencyBrakeLimitMargin
    ******************************************************************************/
    uint32_t BrakeCalculations::calcEmergencyBrakeLimitMargin(const uint32_t ceilingSpeed) const
    {
      const uint32_t permilEB = AbstractConfig::corePtr()->getSpeedMarginEB();
      const uint32_t minValueEB = AbstractConfig::corePtr()->getMinSpeedMarginEB();
      const uint32_t maxValueEB = AbstractConfig::corePtr()->getMaxSpeedMarginEB();

      return calcSpeedLimitMargin(ceilingSpeed, permilEB, minValueEB, maxValueEB);
    }

    /******************************************************************************
    * calcCurveDistance
    ******************************************************************************/
    int32_t BrakeCalculations::calcCurveDistance(const uint32_t currSpeed,const uint32_t delay,const int32_t gradient,
      const int32_t deceleration,const uint32_t targetSp) const
    {
      return calcCurveDistance(static_cast<int32_t>(currSpeed), delay, gradient,
          deceleration, static_cast<int32_t>(targetSp));
    }

    /******************************************************************************
    * calcCurveDistance
    ******************************************************************************/
    int32_t BrakeCalculations::calcCurveDistance(const int32_t currSpeed, uint32_t delay, int32_t gradient,
      const int32_t deceleration,const int32_t targetSp) const
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_calcCurveDistance_begin");

      //Formula to calculate distance
       /* modTargetSp = targetSp - (delay * gradient)
        * vax = currSpeed - (delay * gradient)
          distance = ((vax * vax) - (modTargetSp * modTargetSp))/(2 * deceleration) + (currSpeed * delay) - (targetSp * delay)
       */

      int32_t distance = 0;

      //The distance should be calculated only when the target speed is less than current speed.
      if(currSpeed > targetSp)
      {
        delay = (delay + 5U) / 10U; // convert 0.1s to s

        // For positive gradients, the effect of gradient during delay time is not considered.
        // The effect of gradient in deceleration is already included in the deceleration value
        if(gradient > 0)
        {
          gradient = 0;
        }

        const ATC::ATCMath& math = ATC::ATCMath::instance();
        int32_t v1 = static_cast<int32_t>(math.signMul(delay, gradient, __FILE__, __LINE__));

        int32_t modTargetSp = targetSp - v1;
        int32_t vax = currSpeed - v1;
        int64_t v_sqr = math.signMul(vax, vax, __FILE__, __LINE__) - math.signMul(modTargetSp, modTargetSp, __FILE__, __LINE__);

        //add deceleration to v_square in order to round it up after division.
        v_sqr += deceleration;
        distance = math.signDiv(v_sqr, (2 * deceleration), __FILE__, __LINE__);

        //multiplication of delay and current speed will not exceed 32 bit
        distance = (distance + static_cast<int32_t>(math.signMul(currSpeed, delay, __FILE__, __LINE__)))
                        - static_cast<int32_t>(math.signMul(targetSp, delay, __FILE__, __LINE__));
      }

      vfwVisitCheckPoint(&endCp, "SUP_calcCurveDistance_end");

      return distance;
    }

    /******************************************************************************
    * calcFirstWarningCurveDistance
    ******************************************************************************/
    int32_t BrakeCalculations::calcFirstWarningCurveDistance(const uint32_t currSpeed, const int32_t gradient, const int32_t deceleration,
      const uint32_t targetSpeed) const
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_calcFirstWarningCurveDistance_begin");

      uint32_t delayFirstWarning = get1stwarnCurveDelay();

      vfwVisitCheckPoint(&endCp, "SUP_calcFirstWarningCurveDistance_end");
      return calcCurveDistance(currSpeed, delayFirstWarning, gradient, deceleration, targetSpeed);
    }


    /******************************************************************************
    * calcCurveSpeed
    ******************************************************************************/
    int32_t BrakeCalculations::calcCurveSpeed(const int32_t distance, uint32_t delay, int32_t gradient, const int32_t deceleration,
      const int32_t targetSp) const
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_calcCurveSpeed_begin");

      /********** Formula to calculate speed **********
      ** v1 = delay * gradient
      ** modTargetSp = targetSp - v1
      ** v2 = (delay * (deceleration-gradient))
      ** w =  (modTargetSp * modTargetSp) - (v1 * v1) + (v2*v2)
      ** d1 = delay  * targetSp
      ** square =  w + 2 deceleration(distance + d1)
      ** speed = sqrt(square) - v2
      */

      // For positive gradients, the effect of gradient during delay time is not considered.
      // The effect of gradient in deceleration is already included in the deceleration value
      if(gradient > 0)
      {
        gradient = 0;
      }

      int32_t speed = 0;

      //if the deceleration value is less than or equal to 0, the train cannot stop. Hence permitted speed is 0.
      if(deceleration > 0)
      {
        const ATC::ATCMath& math = ATC::ATCMath::instance();
        delay = (delay + 5U) / 10U; // 0.1s => s

        int32_t v1 = static_cast<int32_t>(math.signMul(delay, gradient, __FILE__, __LINE__));

        if(distance > 0)
        {
          const int32_t modTargetSp = targetSp - v1;
          const int32_t v2 = static_cast<int32_t>(math.signMul(delay, (deceleration - gradient), __FILE__, __LINE__));

          const int64_t targetSpeedSquare = math.signMul(modTargetSp, modTargetSp, __FILE__, __LINE__);
          const int64_t v1Square = math.signMul(v1, v1, __FILE__, __LINE__);
          const int64_t v2Square = math.signMul(v2, v2, __FILE__, __LINE__);

          const int64_t w = (targetSpeedSquare - v1Square) + v2Square;
          const int32_t d1 = static_cast<int32_t>(math.signMul(delay, targetSp, __FILE__, __LINE__));
          int64_t square = w + math.signMul((2 * deceleration), (distance + d1), __FILE__, __LINE__);

          if (square < 0)
          {
            // The square cannot be negative.
            ATC::AbstractEventHandler::corePtr()->reportEvent(errorSquareNegative, __FILE__, __LINE__);
            speed = 0;
          }
          else
          {
            //multiply by 100 to get the speed in 0.1cm/s
            square = square * 100;
            uint32_t sqroot = ATC::ATCMath::sqrtr(static_cast<uint64_t>(square));
            //round down the speed to cm/s. divide by 10 is ok as speed is always positive after sqrt
            sqroot = sqroot / 10U;

            speed = static_cast<int32_t>(sqroot) - v2;
          }
        }
      }

      vfwVisitCheckPoint(&endCp, "SUP_calcCurveSpeed_end");
      return speed;
    }

    /******************************************************************************
    * calcCurveSpeed
    ******************************************************************************/
    uint32_t BrakeCalculations::calcCurveSpeed(const int32_t startPointOdo, const int32_t targetOdo, const TravelDir tDir,
      const int32_t gradient, const int32_t deceleration, const uint32_t targetSpeed, const uint32_t delay) const
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_calculateCurveSpeed_begin");

      // calculate distance
      int32_t distance = 0;
      if (tDir == DirForward)
      {
        distance = (targetOdo - startPointOdo);
      }
      else if (tDir == DirReverse)
      {
        distance = (startPointOdo - targetOdo);
      }
      else
      {
        distance = 0;
      }

      // If the target point odometer (in this case, with SB margin) is passed return target speed
      uint32_t speed = 0U;
      if (distance <= 0)
      {
        speed = targetSpeed;
      }
      else
      {
        int32_t calcSpeed = calcCurveSpeed(distance, delay, gradient, deceleration, static_cast<int32_t>(targetSpeed));
        speed = (calcSpeed < 0)?(0U):(static_cast<uint32_t>(calcSpeed));
        speed = ATC::ATCMath::maximum(speed, targetSpeed);
      }

      vfwVisitCheckPoint(&endCp, "SUP_calculateCurveSpeed_end");
      return speed;
    }

    /******************************************************************************
    * calcAllowedCurveSpeed
    ******************************************************************************/
    int32_t BrakeCalculations::calcAllowedCurveSpeed(int32_t gradient, uint32_t delay, const uint32_t decSpeed, const int32_t deceleration) const
    {

      /********** Formula to calculate speed **********
      ** v1 = delay * deceleration
      ** v2 = delay * gradient
      ** square =  (decSpeed * decSpeed) + v1*(v1 - v2)
      ** speed = sqrt(square) - v1 + v2
      */

      // For positive gradients, the effect of gradient during delay time is not considered.
      // The effect of gradient in deceleration is already included in the deceleration value
      if(gradient > 0)
      {
        gradient = 0;
      }

      delay = (delay + 5U) / 10U; // 0.1s => s

      int32_t speed = 0;

      //if the deceleration value is less than or equal to 0, the train cannot stop. Hence permitted speed is 0.
      if(deceleration > 0)
      {
        const ATC::ATCMath& math = ATC::ATCMath::instance();

        const int32_t v1 = static_cast<int32_t>(math.signMul(delay, deceleration, __FILE__, __LINE__));
        const int32_t v2 = static_cast<int32_t>(math.signMul(delay, gradient, __FILE__, __LINE__));
        const int64_t decSpeedSquare = math.signMul(decSpeed, decSpeed, __FILE__, __LINE__);

        int64_t square = math.signMul(v1, v1-v2, __FILE__, __LINE__) + decSpeedSquare;

        if (square < 0)
        {
          // The square cannot be negative.
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorSquareNegative, __FILE__, __LINE__);
          speed = 0;
        }
        else
        {
          //multiply by 100 to get the speed in 0.1cm/s
          square = square * 100;
          speed = static_cast<int32_t>(ATC::ATCMath::sqrtr(static_cast<uint64_t>(square)));
          //round down the speed to cm/s. divide by 10 is ok as speed is always positive after sqrt
          speed = speed/10;

          speed = (speed + v2) - v1;
        }

      }

      return speed;
    }

    /******************************************************************************
    * calcDecelerationCurveSpeed
    ******************************************************************************/
    uint32_t BrakeCalculations::calcDecelerationCurveSpeed(int32_t gradient, uint32_t delay, const uint32_t distance, const int32_t curveSpeed, const int32_t deceleration) const
    {

      /********** Formula to calculate speed **********
      ** v1 = delay * deceleration
      ** v2 = delay * gradient
      ** square1 = 2* distance * deceleration
      ** square2 = (v1-v2) * (2* curveSpeed - v2)
      ** square =  (curveSpeed * curveSpeed) + square1 + square2
      ** speed = sqrt(square)
      */

      // For positive gradients, the effect of gradient during delay time is not considered.
      // The effect of gradient in deceleration is already included in the deceleration value
      if(gradient > 0)
      {
        gradient = 0;
      }

      delay = (delay + 5U) / 10U; // 0.1s => s

      uint32_t speed = 0U;

      //if the deceleration value is less than or equal to 0, the train cannot stop. Hence permitted speed is 0.
      if(deceleration > 0)
      {
        const ATC::ATCMath& math = ATC::ATCMath::instance();

        const int32_t v1 = static_cast<int32_t>(math.signMul(delay, deceleration, __FILE__, __LINE__));
        const int32_t v2 = static_cast<int32_t>(math.signMul(delay, gradient, __FILE__, __LINE__));

        const int64_t square1 = math.signMul(2U * distance, deceleration, __FILE__, __LINE__);
        const int64_t square2 = math.signMul(v1 - v2, (2*curveSpeed) - v2, __FILE__, __LINE__);

        int64_t square = math.signMul(curveSpeed, curveSpeed, __FILE__, __LINE__) + square1 + square2;

        if (square < 0)
        {
          // The square cannot be negative.
          // No error is raised as this scenario can happen when trying to calculate deceleration speed from curve speed.
          // The deceleration curve speed can never be less than 0 so its always safe to set it to 0.
          speed = 0U;
        }
        else
        {
          //multiply by 100 to get the speed in 0.1cm/s
          square = square * 100;
          speed = ATC::ATCMath::sqrtr(static_cast<uint64_t>(square));

          //round down the speed to cm/s. divide by 10 is ok as speed is always positive after sqrt
          speed = speed / 10U;
        }

      }

      return speed;
    }

    /******************************************************************************
    * calcAccDeaccDistance
    ******************************************************************************/
    uint32_t BrakeCalculations::calcAccDeaccDistance(const uint32_t startSpeed, const uint32_t decCurveTargetSpd, uint32_t delay, const int32_t gradient) const
    {
      ATC::ATCMath& math = ATC::ATCMath::instance();
      int32_t adjGrad = gradient;

      if(gradient > 0)
      {
        adjGrad = 0;
      }

      delay = (delay + 5U) / 10U;

      //vg is speed increase due to gradient during the delay.
      int32_t vg = - static_cast<int32_t>(math.signMul(adjGrad, delay, __FILE__, __LINE__));
      //v1 is the speed after the delay time. The speed increases due to the gradient during the delay time.
      int32_t v1 = static_cast<int32_t>(startSpeed) + vg;

      /*
      * Worst brakeability will be used for the different curve calculation
      * Worst brakeability will be calculated between:
      * EB ceiling speed (max speed) at current target and FW speed ((minimum speed) at next target.
      */
      int32_t brakeability = DS::AbstractTSetup::corePtr()->getWorstBrakeabilityInRange(static_cast<uint32_t>(v1), decCurveTargetSpd);
      int32_t deceleration =  Supv::BrakeCalculations::instance().calcDeceleration(brakeability, gradient);

      //Distance traveled during delay due to gradient acceleration = startSpeed * delay + (gradient * delay^2)/2.
      //this value is subtracted as it is negative for downhill gradients.
      int32_t delaySpeedDist = static_cast<int32_t>(math.signMul(startSpeed, delay, __FILE__, __LINE__));

      int32_t delayGradDist = static_cast<int32_t>(math.signMul(vg, delay, __FILE__, __LINE__) / 2);

      //Distance traveled when the brakes are applied after the delay time and the time the speed reduces to targetSP.
      // this is equal to ((speedLimit - gradient*delay)^2 - speedLimit^2)/(2*deceleration)
      int32_t deaccDist = static_cast<int32_t>(math.signDiv(
                          (math.signMul(v1, v1, __FILE__, __LINE__) - math.signMul(decCurveTargetSpd, decCurveTargetSpd, __FILE__, __LINE__)),
                                  2 * deceleration,__FILE__, __LINE__));

      int32_t odoVal = (delaySpeedDist + delayGradDist + deaccDist);

      return static_cast<uint32_t>(odoVal);
    }

    /******************************************************************************
    * calcSupTargOdoOffset
    ******************************************************************************/
    int32_t BrakeCalculations::findSupvTargetOdo(const TravelDir tDir,const int32_t tpOdo,
      const uint32_t targetSpeed,const uint32_t delay, const uint32_t ceilSpeed) const
    {
      int32_t odoVal = 0;
      int32_t curveSpeedOld = 0;
      int32_t curveSpeedNew = 0;
      uint32_t decCurveSpeedNew = targetSpeed;

      int32_t brakeability = DS::AbstractTSetup::corePtr()->getWorstBrakeabilityInRange(targetSpeed, ceilSpeed);

      DS::SupervisedTargetRevIterator it = DS::AbstractTargets::corePtr()->getSupervisedRevTargetIter();
      DS::SupervisedTargetRevIterator itEnd = DS::AbstractTargets::corePtr()->getSupervisedRevTargetIterEnd();

      bool isFirstRun = true;

      int32_t oldGradient = 0;
      int32_t gradient = 0;
      int32_t supvTargOdo = tpOdo;
      int32_t supvTargOdoOld = tpOdo;

      while (it != itEnd)
      {
        if (DS::BaseTarget::SupervisedTarget == (*it)->getTargetType())
        {
          DS::SupervisedTarget* sTarget = (*it);
          bool isTargetbefore = (tDir == DirForward)?(sTarget->getOdometer() < tpOdo):(sTarget->getOdometer() > tpOdo);
          bool isTargetGradient = sTarget->getSupervisedTargetType() == DS::SupervisedTarget::GradientSupvTarget;

          bool isParentLocation = false;
          if(sTarget->getParentTarget()->getTargetType() == DS::BaseTarget::PrimaryTarget)
          {
            DS::PrimaryTarget* pTarg = ATC::dynamicCast<DS::BaseTarget*, DS::PrimaryTarget*>(sTarget->getParentTarget(), __FILE__, __LINE__);
            if(pTarg->getLocationType() != UndefinedLocationType)
            {
              isParentLocation = true;
            }
          }

          // use gradient for gradient targets or supervised targets that have valid gradient except Location targets.
          isTargetGradient = (sTarget->isGradientValid() && (!isParentLocation)) || isTargetGradient;

          // Only work with targets that have the same direction as travel Direction and before the target.
          if ((tDir == sTarget->getDirection()) && isTargetbefore && isTargetGradient)
          {
            gradient = sTarget->getGradient();
            int32_t deceleration = calcDeceleration(brakeability, gradient);

            if(isFirstRun)
            {
              //calculate curve speed from deceleration curve speed
              curveSpeedNew = calcAllowedCurveSpeed(gradient, delay, decCurveSpeedNew, deceleration);
              oldGradient = gradient;
            }

            //if the gradient changed, update the curve speed and deceleration curve speed
            if(oldGradient != gradient)
            {
              //calculate curve speed with the new gradient.
              int32_t curveSp = calcAllowedCurveSpeed(gradient, delay, decCurveSpeedNew, deceleration);

              //update curve speed to minimum of the new and old curve speed
              curveSpeedNew = ATC::ATCMath::minimum(curveSp, curveSpeedNew);

              // Calculate the curve speed with target deceleration curve speed.
              // The curve speed cannot be below this value.
              curveSp = calcAllowedCurveSpeed(gradient, delay, targetSpeed, deceleration);

              //update curve speed to maximum of the calculated curve speed and worst curve speed.
              curveSpeedNew = ATC::ATCMath::maximum(curveSp, curveSpeedNew);
            }

            //break from the loop if the updated curve speed is more than target speed.
            if((!isFirstRun) && (curveSpeedNew > static_cast<int32_t>(targetSpeed)))
            {
              break;
            }

            // calculate distance between targets
            int32_t distTP = (tDir == DirForward)?(supvTargOdo - sTarget->getOdometer()):(sTarget->getOdometer() - supvTargOdo);

            curveSpeedOld = curveSpeedNew;
            //calculate curve speed and deceleration curve speed.
            decCurveSpeedNew = calcDecelerationCurveSpeed(gradient, delay, static_cast<uint32_t>(distTP), curveSpeedNew, deceleration);
            curveSpeedNew = calcCurveSpeed(distTP, delay, gradient, deceleration, curveSpeedNew);

            supvTargOdoOld = supvTargOdo;
            supvTargOdo = sTarget->getOdometer();

            oldGradient = gradient;

            //set first run to false
            isFirstRun = false;
          }
        }
        ++it;
      }

      if (isFirstRun)
      {
        gradient = DS::AbstractTargets::corePtr()->getCurGradient();
        int32_t deceleration = calcDeceleration(brakeability, gradient);
        //calculate curve speed from deceleration curve speed
        curveSpeedNew = calcAllowedCurveSpeed(gradient, delay, targetSpeed, deceleration);
      }
      else if(it == itEnd)
      {
        gradient = DS::AbstractTargets::corePtr()->getCurGradient();
        //if the gradient changed, update the curve speed and deceleration curve speed
        if(oldGradient != gradient)
        {
          int32_t deceleration = calcDeceleration(brakeability, gradient);

          //calculate curve speed with the new gradient.
          int32_t curveSp = calcAllowedCurveSpeed(gradient, delay, decCurveSpeedNew, deceleration);

          //update curve speed to minimum of the new and old curve speed
          curveSpeedNew = ATC::ATCMath::minimum(curveSp, curveSpeedNew);

          // Calculate the curve speed with targetSpeed deceleration curve speed.
          // The curve speed cannot be below this value.
          curveSp = calcAllowedCurveSpeed(gradient, delay, targetSpeed, deceleration);

          //update curve speed to maximum of the calculated curve speed and worst curve speed.
          curveSpeedNew = ATC::ATCMath::maximum(curveSp, curveSpeedNew);
        }
      }
      else
      {
        //Do Nothing
      }

      //if there are no supervised gradient targets
      if(curveSpeedNew < static_cast<int32_t>(targetSpeed))
      {
        //use the current gradient to calculate the distance to accelerate and decelerate from target speed.
        gradient = DS::AbstractTargets::corePtr()->getCurGradient();
        int32_t deceleration = calcDeceleration(brakeability, gradient);

        int32_t dist = calcCurveDistance(static_cast<int32_t>(targetSpeed), delay, gradient, deceleration, curveSpeedNew);

        //the supervised target odo is then this distance closer to the target point.
        odoVal = (tDir == DirForward)?(supvTargOdo - dist):(supvTargOdo + dist);
      }
      else if(curveSpeedNew == static_cast<int32_t>(targetSpeed))
      {
        odoVal = supvTargOdo;
      }
      else
      {
        // The target is found with speed greater than target speed. Calculate curve distance to 0 speed.
        int32_t deceleration = calcDeceleration(brakeability, oldGradient);
        int32_t dist = calcCurveDistance(static_cast<int32_t>(targetSpeed), delay, oldGradient, deceleration, curveSpeedOld);

        //the supervised target odo is ahead of the supervised gradient target found
        odoVal = (tDir == DirForward)?(supvTargOdoOld - dist):(supvTargOdoOld + dist);
      }

      return odoVal;
    }

    /******************************************************************************
    * calcFirstWarningCurveSpeed
    ******************************************************************************/
    uint32_t BrakeCalculations::calcFirstWarningCurveSpeed(const int32_t startPointOdo,const TravelDir tDir, const int32_t gradient,
        const int32_t deceleration, const uint32_t ceilingSpeed, const DS::SupervisedTarget &nextTarget) const
    {
      uint32_t curveSpeed = calcCurveSpeed(startPointOdo,nextTarget.getOdometer(), tDir, gradient, deceleration,
                  nextTarget.getFirstWarningSpeed(),get1stwarnCurveDelay());

      return ATC::ATCMath::minimum(curveSpeed, ceilingSpeed);
    }

    /******************************************************************************
    * calcSecondWarningCurveSpeed
    ******************************************************************************/
    uint32_t BrakeCalculations::calcSecondWarningCurveSpeed(const int32_t startPointOdo,const TravelDir tDir, const int32_t gradient,
        const int32_t deceleration, const uint32_t ceilingSpeed, const DS::SupervisedTarget &nextTarget) const
    {
      uint32_t curveSpeed = calcCurveSpeed(startPointOdo,nextTarget.getOdometer(), tDir, gradient, deceleration,
                  nextTarget.getSecondWarningSpeed(),get2ndwarnCurveDelay());

      return ATC::ATCMath::minimum(curveSpeed, ceilingSpeed);
    }

    /******************************************************************************
    * calcServiceBrakeCurveSpeed
    ******************************************************************************/
    uint32_t BrakeCalculations::calcServiceBrakeCurveSpeed(const int32_t startPointOdo,const TravelDir tDir, const int32_t gradient,
        const int32_t deceleration, const uint32_t ceilingSpeed, const DS::SupervisedTarget &nextTarget) const
    {
      uint32_t curveSpeed = calcCurveSpeed(startPointOdo,nextTarget.getOdometer(), tDir, gradient, deceleration,
                  nextTarget.getSBSpeed(), getSBCurveDelay());

      return ATC::ATCMath::minimum(curveSpeed, ceilingSpeed);
    }

    /******************************************************************************
    * calcEmergencyBrakeCurveSpeed
    ******************************************************************************/
    uint32_t BrakeCalculations::calcEmergencyBrakeCurveSpeed(const int32_t startPointOdo,const TravelDir tDir, const int32_t gradient,
        const int32_t deceleration, const uint32_t ceilingSpeed, const DS::SupervisedTarget &nextTarget) const
    {
      uint32_t curveSpeed = calcCurveSpeed(startPointOdo,nextTarget.getOdometer(), tDir, gradient, deceleration,
                  nextTarget.getEBSpeed(), getEBCurveDelay());

      return ATC::ATCMath::minimum(curveSpeed, ceilingSpeed);
    }


    /******************************************************************************
    * calcDeceleration
    ******************************************************************************/
    int32_t BrakeCalculations::calcDeceleration(const int32_t brakeability, const int32_t gradient) const
    {
      const int32_t deceleration = brakeability + gradient;

      //effective deceleration cannot be less than or equal to 0. Safety Halt.
      if(deceleration <= 0)
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(errorDecNegative, __FILE__, __LINE__);
      }

      return deceleration;
    }


    /******************************************************************************
    * calcPredictedDistanceToStandStillLocation
    ******************************************************************************/
    int32_t BrakeCalculations::calcPredictedDistanceToStandStillLocation(const uint32_t vA, const int32_t rCurr) const
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_calcPredictedDistanceToStandStillLocation_begin");

      int32_t predictedDistanceToStandStillLocation = ATC::maxDistance;
      if (rCurr >= 0)
      {
        // If the train is accelerating the result of will be negative.
        // The predicted value will be MaxValue
      }
      else
      {
        const ATC::ATCMath& math = ATC::ATCMath::instance();

        // dpds = - (va * va) / (2 * rcurr)
        int64_t vaSqr = math.signMul(static_cast<int32_t>(vA), static_cast<int32_t>(vA), __FILE__, __LINE__);
        predictedDistanceToStandStillLocation = math.signDiv(
          vaSqr, static_cast<int32_t>(math.signMul(-2, rCurr, __FILE__, __LINE__)), __FILE__, __LINE__);
      }

      vfwVisitCheckPoint(&endCp, "SUP_calcPredictedDistanceToStandStillLocation_end");
      return predictedDistanceToStandStillLocation;
    }


    /******************************************************************************
    * calcTimeToIntervention
    ******************************************************************************/
    uint32_t BrakeCalculations::calcTimeToIntervention(const uint32_t currSpeed, const int32_t currAcceleration, const int32_t startPointOdo,
      const int32_t targetOdo, const TravelDir tDir, const int32_t deceleration, const uint32_t targetSpeed, const uint32_t releaseSpeed, const int32_t gradient,
      const uint32_t currSbSpeed, const DS::BaseTarget::CoreTargetType targetType) const
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0

      const ATC::ATCMath& math = ATC::ATCMath::instance();

      vfwVisitCheckPoint(&beginCp, "SUP_calcTimeToIntervention_begin");

      const uint32_t maxOfReleaseAndTargetSpeed = ATC::ATCMath::maximum(releaseSpeed, targetSpeed);

      int32_t dtpsb = 0;  // dtpsb is the distance to service brake target point odometer

      uint32_t timeToIntervention = ATC::maxTimeValue;
      bool calcDone = false;

      if (tDir == DirForward)
      {
        dtpsb = (targetOdo - startPointOdo);
      }
      else if (tDir == DirReverse)
      {
        dtpsb = (startPointOdo - targetOdo);
      }
      else
      {
        dtpsb = 0;
      }

      if (AbstractSupervise::corePtr()->getInReleaseSpeedArea() && (targetType == DS::BaseTarget::PrimaryTarget))
      {
        //if not accelerating nor decelerating
        if ((currAcceleration == 0) && (currSpeed > 0U))
        {
          timeToIntervention = static_cast<uint32_t>(math.signDiv(dtpsb, static_cast<int32_t>(currSpeed), __FILE__, __LINE__));
        }
        else
        {
          //calculate the speed at the sb target point
          int64_t vaSqr = math.signMul(currSpeed, currSpeed, __FILE__, __LINE__);
          int64_t accDist = math.signMul(2 * currAcceleration, dtpsb, __FILE__, __LINE__);

          uint32_t vSbtp = 0U;
          if ((vaSqr + accDist) > 0)
          {
            vSbtp = ATC::ATCMath::sqrtr(static_cast<uint64_t>(vaSqr) + static_cast<uint64_t>(accDist));
          }

          //if the speed at the SB target point is greater than release speed, time for intervention is when the speed crosses release speed
          if (vSbtp > releaseSpeed)
          {
            timeToIntervention = static_cast<uint32_t>(math.signDiv(static_cast<int32_t>(releaseSpeed) - static_cast<int32_t>(currSpeed),
              currAcceleration, __FILE__, __LINE__));
          }
          //If the train will stop before SB target point, there is no intervention
          else if (vSbtp == 0U)
          {
            timeToIntervention = ATC::maxTimeValue;
          }
          //if the speed at the SB target point is greater than 0 but less than release speed,
          // time for intervention is when the train crosses the SB target point
          else
          {
            timeToIntervention = static_cast<uint32_t>(math.signDiv(static_cast<int32_t>(vSbtp) - static_cast<int32_t>(currSpeed),
              currAcceleration, __FILE__, __LINE__));
          }
        }
        calcDone = true;
      }
      else if ((currSpeed == 0U) && (currAcceleration == 0))
      {
        // If va = 0 & rcurr = 0; Current speed and current acceleration are zero
        timeToIntervention = ATC::maxTimeValue;
        calcDone = true;
      }
      else if ((currAcceleration + deceleration) <= 0)
      {
        // The train is decelerating faster than the deceleration used to calculate the curves.
        // It will never intersect the SB curve.
        timeToIntervention = ATC::maxTimeValue;
        calcDone = true;
      }
      else if ((currAcceleration <= 0) && (currSpeed <= maxOfReleaseAndTargetSpeed))
      {
        // Current speed is less than or equal target speed and current acceleration is not positive
        timeToIntervention = ATC::maxTimeValue;
        calcDone = true;
      }
      else if ((currAcceleration == 0) && (currSpeed > maxOfReleaseAndTargetSpeed)) // rcurr + rsb
      {
        //If current acceleration is 0, time to intervention is distance to curve divided by current speed.
        const int32_t distToIntv = dtpsb - calcCurveDistance(currSpeed, getSBCurveDelay(),
                                                             gradient,deceleration, targetSpeed);
        const int32_t timeToIntv = math.signDiv(distToIntv, static_cast<int32_t>(currSpeed), __FILE__, __LINE__);

        if(timeToIntv < 0)
        {
          timeToIntervention = ATC::maxTimeValue;
        }
        else
        {
          timeToIntervention = static_cast<uint32_t>(timeToIntv);
        }

        calcDone = true;
      }
      else if (dtpsb < 0)
      {
        // If the train has passed the service brake target point odometer (dtpsb is negative) already passed Service Brake margin
        const int32_t vdelta = static_cast<int32_t>(maxOfReleaseAndTargetSpeed) - static_cast<int32_t>(currSpeed);
        const int32_t timeToIntv = math.signDiv(vdelta, static_cast<int32_t>(currAcceleration), __FILE__, __LINE__);

        if (timeToIntv > 0)
        {
          timeToIntervention = static_cast<uint32_t>(timeToIntv);
        }
        else
        {
          timeToIntervention = ATC::maxTimeValue;
        }
        calcDone = true;
      }
      else
      {
        // Just keep Lint happy, calculate below
      }

      if (!calcDone)
      {
        const uint32_t delayInSeconds = (getSBCurveDelay() + 5U) / 10U; // convert 0.1s to s
        const int32_t v1 = static_cast<int32_t>(math.signMul(delayInSeconds, deceleration - gradient, __FILE__, __LINE__));
        const int32_t vSbm = static_cast<int32_t>(currSbSpeed) + v1;

        const int32_t vaSqr = static_cast<int32_t>(math.signMul(currSpeed, currSpeed, __FILE__, __LINE__));   //va * va
        const int32_t vSbSqr = static_cast<int32_t>(math.signMul(currSbSpeed, currSbSpeed, __FILE__, __LINE__));
        const int32_t vSbmSqr = static_cast<int32_t>(math.signMul(vSbm, vSbm, __FILE__, __LINE__));
        const int32_t accSqr = static_cast<int32_t>(math.signMul(currAcceleration, currAcceleration, __FILE__, __LINE__));
        const int32_t decSqr = static_cast<int32_t>(math.signMul(deceleration, deceleration, __FILE__, __LINE__));
        const int32_t decAcc = static_cast<int32_t>(math.signMul(currAcceleration, deceleration, __FILE__, __LINE__));
        const int32_t v1Vt2 = static_cast<int32_t>(math.signMul(2 * v1, currSbSpeed, __FILE__, __LINE__));

        const int64_t square = math.signMul(accSqr, vSbmSqr, __FILE__, __LINE__) +
                        math.signMul(decAcc, vSbSqr + vaSqr + v1Vt2, __FILE__, __LINE__) +
                        math.signMul(vaSqr, decSqr, __FILE__, __LINE__);

        if(square <= 0)
        {
          timeToIntervention = ATC::maxTimeValue;
        }
        else
        {
          const int32_t w = static_cast<int32_t>(ATC::ATCMath::sqrtr(static_cast<uint64_t>(square)))
               - (static_cast<int32_t>(math.signMul(static_cast<int32_t>(currSpeed), (deceleration + currAcceleration), __FILE__, __LINE__))
               + static_cast<int32_t>(math.signMul(currAcceleration, v1, __FILE__, __LINE__)));

          const int32_t timeToIntv = math.signDiv(w, accSqr + decAcc, __FILE__, __LINE__);

          if (timeToIntv <= 0)
          {
            timeToIntervention = ATC::maxTimeValue;
          }
          else
          {
            timeToIntervention = static_cast<uint32_t>(timeToIntv);
          }
        }

        //If time to intervention is calculated, find the speed of intervention.
        if (timeToIntervention != ATC::maxTimeValue)
        {
          const int32_t dv = static_cast<int32_t>(math.signMul(
            static_cast<int32_t>(timeToIntervention), currAcceleration, __FILE__, __LINE__));
          const int32_t interventionSpeed = static_cast<int32_t>(currSpeed) + dv;
          
          //If speed of intervention is below the target speed or release speed, calculate time to intervention based on 
          //intervention speed MAX(target speed, release speed) with current acceleration.
          if (interventionSpeed < static_cast<int32_t>(ATC::ATCMath::maximum(targetSpeed, releaseSpeed)))
          {
            const int32_t vdelta = static_cast<int32_t>(maxOfReleaseAndTargetSpeed) - static_cast<int32_t>(currSpeed);
            const int32_t timeToIntv = math.signDiv(vdelta, static_cast<int32_t>(currAcceleration), __FILE__, __LINE__);
            
            if (timeToIntv > 0)
            {
              timeToIntervention = static_cast<uint32_t>(timeToIntv);
            }
            else
            {
              timeToIntervention = ATC::maxTimeValue;
            }
          }
        }
      }

      vfwVisitCheckPoint(&endCp, "SUP_calcTimeToIntervention_end");

      return timeToIntervention;
    }

    /******************************************************************************
    * getEBCurveDelay
    ******************************************************************************/
    uint32_t BrakeCalculations::getEBCurveDelay() const
    {
      //Effective delay for EB Curve = EB delay + Brake Response Time + Traction Cut off delay
      uint32_t ebDelay = AbstractConfig::corePtr()->getEffectiveEbDelay();
      ebDelay += DS::AbstractTSetup::corePtr()->getEmergencyBrakeResponseTime();
      ebDelay += AbstractConfig::corePtr()->getTractionCutoffDelay();

      return ebDelay;
    }

    /******************************************************************************
    * getSBCurveDelay
    ******************************************************************************/
    uint32_t BrakeCalculations::getSBCurveDelay() const
    {
      uint32_t sbDelay = AbstractConfig::corePtr()->getSbToEbDelay();
      sbDelay += (getEBCurveDelay() + DS::AbstractTSetup::corePtr()->getServiceBrakeResponseTime());

      return sbDelay;
    }

    /******************************************************************************
    * get2ndwarnCurveDelay
    ******************************************************************************/
    uint32_t BrakeCalculations::get2ndwarnCurveDelay() const
    {
      //To adapt 0.1 sec for calculation
      uint32_t delay2ndWarn = static_cast<uint32_t>(AbstractConfig::corePtr()->getSecondToSBCurveDelay()) * 10U;
      delay2ndWarn += getSBCurveDelay();

      return delay2ndWarn;
    }

    /******************************************************************************
    * get1stwarnCurveDelay
    ******************************************************************************/
    uint32_t BrakeCalculations::get1stwarnCurveDelay() const
    {
      //To adapt 0.1 sec for calculation
      uint32_t delay1stWarn = static_cast<uint32_t>(AbstractConfig::corePtr()->getFirstToSecondWarnCurveDelay()) * 10U;
      delay1stWarn += get2ndwarnCurveDelay();

      return delay1stWarn;
    }

  }
}
