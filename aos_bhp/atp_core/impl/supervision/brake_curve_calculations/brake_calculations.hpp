#ifndef BrakeCalculations_hpp
#define BrakeCalculations_hpp
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
* 2016-09-26    Hidaji      Added new functions and updated the signature for
*                           some existing functions
* 2016-10-05    Hidaji      Changed brakeReactionTime => brakeResponseTime
* 2016-10-10    Hidaji      Added calcDeceleration, and adjustGradient
* 2016-10-10    rquensel    Added calcPredictedDistanceToStandStillLocation
* 2016-10-12    rquensel    Added calcPredictedSpeedAtTarget, calcTimeToIntervention
* 2016-10-14    rquensel    Removed not used incude files (Lint)
* 2016-10-25    rquensel    Added access functions for DMI
* 2016-12-28    nsyed       Re-factored into a class
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "abstract_event_handler.hpp"
#include "base_target.hpp"
#include "supervised_target.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Supv
  {
    /**
    * The class ATCMath implements the math functions used by the ATP components
    */
    class BrakeCalculations
    {
    public:

      /**
      * Singleton instance.
      * Only one instance of this class is allowed.
      *
      * @return the one and only one instance.
      */
      static BrakeCalculations& instance(void);

      /**
      * Initializes the cross compare module. Called by the AbstractSupervise.
      */
      virtual void initCrossCompare() const;

      /** Calculate Speed Limit Margin
      *
      * Implements the calcSpeedLimitMargin() function.
      * determines the Speed limit.
      *
      * @param[in]  ceilingSpeed    ceiling Speed of the part of the track for
      * which the speed limit is calculated
      * @param[in]  permil          Permil of the ceiling speed
      * @param[in]  minValue        Is the minimum value for the speed margin
      * @param[in]  maxValue        Is the maximum value for the speed margin
      * @return     Speed limit margin
      */
      uint32_t calcSpeedLimitMargin(const uint32_t ceilingSpeed, const uint32_t permil, const uint32_t minValue, const uint32_t maxValue) const;

      /** Calculate Warning Limit Margin
      *
      * Implements the calcWarningLimitMargin() function.
      * determines the Warning limit margin.
      *
      * @param[in]  ceilingSpeed   Is the ceiling Speed of the part of the track for
      * which the speed limit is calculated
      * @return     Warning limit margin
      */
      uint32_t calcWarningLimitMargin(const uint32_t ceilingSpeed) const;

      /** Calculate Service brake Limit Margin
      *
      * Implements the calcServiceBrakeLimitMargin() function.
      * determines the service brake limit margin.
      *
      * @param[in]  ceilingSpeed  Is the ceiling Speed of the part of the track for
      * which the speed limit is calculated
      * @return     service brake limit margin
      */
      uint32_t calcServiceBrakeLimitMargin(const uint32_t ceilingSpeed) const;

      /** Calculate Emergency brake Limit Margin
      *
      * Implements the calcEmergencyBrakeLimitMargin() function.
      * determines the emergency brake limit margin.
      *
      * @param[in]  ceilingSpeed    Is the ceiling Speed of the part of the track for
      * which the speed limit is calculated
      * @return     emergency brake limit margin
      */
      uint32_t calcEmergencyBrakeLimitMargin(const uint32_t ceilingSpeed) const;

      /** Calculate curve distance
      *
      * Implements the calcCurveDistance() function.
      * The function will calculate the distance to the emergency, service brake, or warnings curves.
      *
      * @param[in]  currSpeed           is the current speed of the train cm/s
      * @param[in]  delay               is the total time delays Unit: 0.1 s
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration        is the effective deceleration Unit: cm / s^2
      * @param[in]  targetSp            is the target speed cm/s
      * @return     distance to the brake curve in cm
      */
      int32_t calcCurveDistance(const uint32_t currSpeed,const uint32_t delay,const int32_t gradient, const int32_t deceleration, const uint32_t targetSp) const;

      /** Calculate first warning curve distance
      *
      * Implements the calcFirstWarningCurveDistance() function.
      * The function will calculate the distance first warning curve.
      *
      * @param[in]  currSpeed           is the current speed of the train cm/s
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration        is the effective deceleration Unit: cm / s^2
      * @param[in]  targetSpeed         is the target speed: cm/s
      *
      * @return     distance to the first warning curve in cm
      */
      int32_t calcFirstWarningCurveDistance(const uint32_t currSpeed, const int32_t gradient, const int32_t deceleration,
        const uint32_t targetSpeed) const;

      /** Calculate distance to accelerate and decelerate
      *
      * @param[in]  startSpeed          is the speed to accelerate from cm/s
      * @param[in]  decCurveTargetSpd   is the target speed after deceleration cm/s
      * @param[in]  delay               is the total time delays Unit: 0.1 s
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @return     distance to accelerate with gradient and decelerate with brake curve in cm
      */
      uint32_t calcAccDeaccDistance(const uint32_t startSpeed, const uint32_t decCurveTargetSpd, uint32_t delay, const int32_t gradient) const;

      /** Calculate allowed curve speed from deceleration curve speed at the same point
      *
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  delay               is the total time delays Unit: 0.1 s
      * @param[in]  decSpeed            is the deceleration curve speed cm/s
      * @param[in]  deceleration        is the effective deceleration Unit: cm / s^2
      *
      * @return     allowed curve speed calculated from deceleration curve speed
      */
      int32_t calcAllowedCurveSpeed(int32_t gradient, uint32_t delay, const uint32_t decSpeed, const int32_t deceleration) const;

      /** Calculate deceleration curve speed from brake curve speed at the specified distance
      *
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  delay               is the total time delays Unit: 0.1 s
      * @param[in]  distance            is the distance to the target
      * @param[in]  curveSpeed          is the brake curve speed cm/s
      * @param[in]  deceleration        is the effective deceleration Unit: cm / s^2
      *
      * @return     deceleration curve speed calculated from brake curve speed at the distance specified
      */
      uint32_t calcDecelerationCurveSpeed(int32_t gradient, uint32_t delay, const uint32_t distance, const int32_t curveSpeed, const int32_t deceleration) const;

      /** Find supervised target odometer position
      *
      * @param[in]  tDir                is the travel direction
      * @param[in]  tpOdo               is the odometer of the MA target cm
      * @param[in]  targetSpeed         is the curve speed at the MA target cm/s
      * @param[in]  delay               is the total time delays Unit: 0.1 s
      * @param[in]  ceilSpeed           is the ceiling Speed at the MA target cm/s
      * @return     Odometer value where to place the supervised target based on the gradients in the MA.
      */
      int32_t findSupvTargetOdo(const TravelDir tDir, const int32_t tpOdo, const uint32_t targetSpeed, const uint32_t delay, const uint32_t ceilSpeed) const;

      /** Calculate first warning curve speed
      *
      * Implements the calcFirstWarningCurveSpeed() function.
      * The function calculates speed of the first warning curve
      *
      * @param[in]  startPointOdo       is the odometer of the point where curve speed needs to be calculated
      * @param[in]  tDir                is the travel direction
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration        is the effective deceleration Unit: cm / s^2
      * @param[in]  ceilingSpeed        is the ceiling Speed of the part of the track for which the speed limit is calculated
      * @param[in]  nextTarget          is the target from which to calculate the brake curves.
      * @return     Speed of the first warning brake curve
      */
      uint32_t calcFirstWarningCurveSpeed(const int32_t startPointOdo,const TravelDir tDir, const int32_t gradient,
          const int32_t deceleration, const uint32_t ceilingSpeed, const DS::SupervisedTarget &nextTarget) const;

      /** Calculate second warning curve speed
      *
      * Implements the calcSecondWarningCurveSpeed() function.
      * The function calculates speed of the second warning curve
      *
      * @param[in]  startPointOdo       is the odometer of the point where curve speed needs to be calculated
      * @param[in]  tDir                is the travel direction
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration        is the effective deceleration Unit: cm / s^2
      * @param[in]  ceilingSpeed        is the ceiling Speed of the part of the track for which the speed limit is calculated
      * @param[in]  nextTarget          is the target from which to calculate the brake curves.
      * @return     Speed of the second warning brake curve
      */
      uint32_t calcSecondWarningCurveSpeed(const int32_t startPointOdo,const TravelDir tDir, const int32_t gradient,
          const int32_t deceleration, const uint32_t ceilingSpeed, const DS::SupervisedTarget &nextTarget) const;

      /** Calculate service brake curve speed
      *
      * Implements the calcServiceBrakeCurveSpeed() function.
      * The function calculates speed of the service brake curve
      *
      * @param[in]  startPointOdo       is the odometer of the point where curve speed needs to be calculated
      * @param[in]  tDir                is the travel direction
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration        is the effective deceleration Unit: cm / s^2
      * @param[in]  ceilingSpeed        is the ceiling Speed of the part of the track for which the speed limit is calculated
      * @param[in]  nextTarget          is the target from which to calculate the brake curves.
      * @return     Speed of the service brake curve
      */
      uint32_t calcServiceBrakeCurveSpeed(const int32_t startPointOdo,const TravelDir tDir, const int32_t gradient,
          const int32_t deceleration, const uint32_t ceilingSpeed, const DS::SupervisedTarget &nextTarget) const;

      /** Calculate emergency brake curve speed
      *
      * Implements the calcEmergencyBrakeCurveSpeed() function.
      * Implements the calcServiceBrakeCurveSpeed() function.
      * The function calculates speed of the emergency brake curve
      *
      * @param[in]  startPointOdo       is the odometer of the point where curve speed needs to be calculated
      * @param[in]  tDir                is the travel direction
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration        is the effective deceleration Unit: cm / s^2
      * @param[in]  ceilingSpeed        is the ceiling Speed of the part of the track for which the speed limit is calculated
      * @param[in]  nextTarget          is the target from which to calculate the brake curves.
      * @return     Speed of the emergency brake curve
      */
      uint32_t calcEmergencyBrakeCurveSpeed(const int32_t startPointOdo,const TravelDir tDir, const int32_t gradient,
          const int32_t deceleration, const uint32_t ceilingSpeed, const DS::SupervisedTarget &nextTarget) const;

      /** calculate deceleration
      *
      * Implements the calcDeceleration() function.
      * The calculates the deceleration of the train
      *
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  brakeability        is the effective brakeability Unit: cm / s^2
      * @return     adjusted gradient
      */
      int32_t calcDeceleration(const int32_t brakeability, const int32_t gradient) const;

      /** Calculate Predicted distance to stand still location
      *
      * Implements the calcPredictedDistanceToStandStillLocation() function.
      * The function will calculate the Predicted distance to stand still location.
      *
      * @param[in]  vA                  is the current Train speed. Unit: cm / s
      * @param[in]  rCurr               is the current acceleration value. Unit: cm / s^2
      * @return     Predicted distance to stand still location
      */
      int32_t calcPredictedDistanceToStandStillLocation(const uint32_t vA, const int32_t rCurr) const;

      /** Calculate time to intervention
      *
      * Implements the calcTimeToIntervention() function.
      * The function will calculate time to intervention.
      *
      * @param[in]  currSpeed                  is the current Train speed (va). Unit: cm / s
      * @param[in]  currAcceleration           is the current acceleration value (rCurr). Unit: cm / s^2
      * @param[in]  startPointOdo              Odometer value from where we calculate. Unit: cm
      * @param[in]  targetOdo                  Odometer value of the service brake target point odometer. Unit: cm
      * @param[in]  tDir                       is the travel direction
      * @param[in]  deceleration               is the effective deceleration (rsb). Unit: cm / s^2
      * @param[in]  targetSpeed                is the target speed (vt) . Unit cm/s
      * @param[in]  releaseSpeed               is the release speed. Unit cm/s
      * @param[in]  gradient                   is the effective gradient Unit: cm / s^2
      * @param[in]  currSbSpeed                is the sb curve speed at current position . Unit cm/s
      * @param[in]  targetType                 is the currently supervised target type
      * @return     Time to intervention in s
      */
      uint32_t calcTimeToIntervention(const uint32_t currSpeed, const int32_t currAcceleration, const int32_t startPointOdo,
        const int32_t targetOdo, const TravelDir tDir, const int32_t deceleration, const uint32_t targetSpeed, const uint32_t releaseSpeed, const int32_t gradient,
        const uint32_t currSbSpeed, const DS::BaseTarget::CoreTargetType targetType) const;

      /**
      * get value of time delay for Emergency Brake curve
      * @return     effective time delay for the Emergency Brake curve  (0.1 seconds)
      */
      uint32_t getEBCurveDelay() const;

      /**
      * get value of time delay for Service Brake curve
      * @return     effective time delay for the Service Brake curve (0.1 seconds)
      */
      uint32_t getSBCurveDelay() const;

      /**
      * get value of time delay for Second Warning curve
      * @return     effective time delay for the Second Warning curve (0.1 seconds)
      */
      uint32_t get2ndwarnCurveDelay() const;

      /**
      * get value of time delay for First Warning curve
      * @return     effective time delay for the First Warning curve (0.1 seconds)
      */
      uint32_t get1stwarnCurveDelay() const;

    protected:

      /** Calculate curve speed
      *
      * Implements the calcCurveSpeed() function.
      * The function will calculate the speed of a curve based on the input parameters
      *
      * @param[in]  distance            is the distance to the target
      * @param[in]  delay               is the total time delays Unit: 0.1 s
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration        is the effective deceleration Unit: cm / s^2
      * @param[in]  targetSp            is the target speed: cm/s
      *
      * @return     curve speed
      */
      int32_t calcCurveSpeed(const int32_t distance, uint32_t delay, int32_t gradient, const int32_t deceleration, const int32_t targetSp) const;

      /** Calculate curve speed
      *
      * Implements the calcCurveSpeed() function.
      * The function will calculate the speed of the curve based on the input parameters
      *
      * @param[in]  startPointOdo       is the odometer of the point where curve speed needs to be calculated
      * @param[in]  targetOdo           is the target odometer position
      * @param[in]  tDir                is the travel direction
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration        is the effective deceleration Unit: cm / s^2
      * @param[in]  targetSpeed         is the target speed: cm/s
      * @param[in]  delay               is the delay for the curve Unit: 0.1 s
      * @return     speed of the curve
      */
      uint32_t calcCurveSpeed(const int32_t startPointOdo, const int32_t targetOdo, const TravelDir tDir,
        const int32_t gradient, const int32_t deceleration, const uint32_t targetSpeed, const uint32_t delay) const;

      /** Calculate curve distance
      *
      * Implements the calcCurveDistance() function.
      * The function will calculate the distance to the emergency, service brake, or warnings curves.
      *
      * @param[in]  currSpeed           is the current speed of the train cm/s
      * @param[in]  delay               is the total time delays Unit: 0.1 s
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration        is the effective deceleration Unit: cm / s^2
      * @param[in]  targetSp            is the target speed cm/s
      * @return     distance to the brake curve in cm
      */
      int32_t calcCurveDistance(const int32_t currSpeed, uint32_t delay, int32_t gradient, const int32_t deceleration, const int32_t targetSp) const;

      /**
      * Constructor for brakeCalculations
      */
      BrakeCalculations();

    private:
      /**
      * Event to report errors occurring during brake calculations
      */
      const ATC::Event errorSquareNegative;

      /**
      * Event to report errors occurring during brake calculations
      */
      const ATC::Event errorDecNegative;

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      BrakeCalculations(const BrakeCalculations&);
    };
  }
}
#endif

