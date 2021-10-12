#ifndef BrakeabilityBrakeSystemSteps_hpp
#define BrakeabilityBrakeSystemSteps_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration of the adaptation class BrakeabilityBrakeSystemSteps.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-03-23    rquensel    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "brakeability_brake_system.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DS
  {
    /**
    * The class Targets instantiates the abstract class and implements
    * the interfaces needed for both inherited classes and component.
    *
    */
    class BrakeabilityBrakeSystemSteps : public BrakeabilityBrakeSystem
    {
    public:

      /**
      * Initializes the cross compare module.
      */
      virtual void initCrossCompare() const;

      /**
      * Get the most restrictive brakeability between two speed segments speed
      *
      * @param[in]  v1 speed segment 1 (cm/s) for the brakeability.
      * @param[in]  v2 speed segment 2 (cm/s) for the brakeability.
      * @return The most restrictive brakeability for the provided speed segment (cm/s2)
      */
      virtual int32_t getBrakeabilityInRange(const uint32_t v1, const uint32_t v2) const;

      /**
      * Get the brakeability for a given speed
      *
      * @param[in]  v Speed segment (cm/s) for the brakeability.
      * @return The brakeability for the provided speed segment (cm/s2)
      */
      virtual int32_t getBrakeability(const uint32_t v) const;

      /**
      * Get service brake response time
      *
      * @return The brake response time (in 0.1 seconds) for the current train
      */
      virtual uint32_t getServiceBrakeResponseTime() const;

      /**
      * Get emergency brake response time
      *
      * @return The brake response time (in 0.1 seconds) for the current train
      */
      virtual uint32_t getEmergencyBrakeResponseTime() const;

      /**
      * Validate the brake data
      *
      * @param[in]  trainDynamicWeightLoaded Dynamic weight for loaded locomotive plus cars.
      * @param[in]  trainDynamicWeightEmpty Dynamic weight for empty locomotive plus cars.
      * @param[in]  locomotiveBrakeWeightLoadedBrakeSystem Loaded brake weight for the locomotive
      * @param[in]  locomotiveBrakeWeightEmptyBrakeSystem Empty brake weight for the locomotive
      * @param[in]  carsBrakeWeightLoadedBrakeSystem Loaded brake weight for the cars
      * @param[in]  carsBrakeWeightEmptyBrakeSystem Empty brake weight for the cars
      * @return true if the calculated brake lambda is within the limit
      */
      virtual bool validateLambda(
        const int32_t trainDynamicWeightLoaded,
        const int32_t trainDynamicWeightEmpty,
        const int32_t locomotiveBrakeWeightLoadedBrakeSystem,
        const int32_t locomotiveBrakeWeightEmptyBrakeSystem,
        const int32_t carsBrakeWeightLoadedBrakeSystem,
        const int32_t carsBrakeWeightEmptyBrakeSystem) const;

      /**
      * Implement isValidLambda
      * @return true if the calculated  lambda is higher than configured minimum limit
      */
      virtual bool isValidLambda() const;

      /**
      * Checks if the given parameters would lead to equal or better brake ability than the stored parameters.
      *
      * @param[in]  trainDynamicWeightLoaded  dynamic weight for loaded locomotive plus cars
      * @param[in]  trainDynamicWeightEmpty  dynamic weight for empty locomotive plus cars
      * @param[in]  locomotiveBrakeWeightLoadedBrakeSystem  Loaded brake weight for the locomotive
      * @param[in]  locomotiveBrakeWeightEmptyBrakeSystem  Empty brake weight for the locomotive
      * @param[in]  carsBrakeWeightLoadedBrakeSystem  Loaded brake weight for the cars
      * @param[in]  carsBrakeWeightEmptyBrakeSystem  Empty brake weight for the cars
      * @return true if the given parameters would give equal or better brake ability
      */
      virtual bool isBrakeDataEqualOrBetter(
        const int32_t trainDynamicWeightLoaded,
        const int32_t trainDynamicWeightEmpty,
        const int32_t locomotiveBrakeWeightLoadedBrakeSystem,
        const int32_t locomotiveBrakeWeightEmptyBrakeSystem,
        const int32_t carsBrakeWeightLoadedBrakeSystem,
        const int32_t carsBrakeWeightEmptyBrakeSystem) const;

      /**
      * Set the brake data
      *
      * @param[in]  trainDynamicWeightLoaded Dynamic weight for loaded locomotive plus cars.
      * @param[in]  trainDynamicWeightEmpty  Dynamic weight for empty locomotive plus cars.
      * @param[in]  locomotiveBrakeWeightLoadedBrakeSystem  Loaded brake weight for the locomotive
      * @param[in]  locomotiveBrakeWeightEmptyBrakeSystem   Empty brake weight for the locomotive
      * @param[in]  carsBrakeWeightLoadedBrakeSystem        Loaded brake weight for the cars
      * @param[in]  carsBrakeWeightEmptyBrakeSystem         Empty brake weight for the cars
      */
      virtual void setBrakeData(
        const int32_t    trainDynamicWeightLoaded,
        const int32_t    trainDynamicWeightEmpty,
        const int32_t    locomotiveBrakeWeightLoadedBrakeSystem,
        const int32_t    locomotiveBrakeWeightEmptyBrakeSystem,
        const int32_t    carsBrakeWeightLoadedBrakeSystem,
        const int32_t    carsBrakeWeightEmptyBrakeSystem);

      /**
      * Set if the train is loaded or not
      * @param[in]  trainLoaded set if the train is loaded or empty
      */
      virtual void setBrakeDataTrainLoaded(const TrainLoaded trainLoaded);

      /**
      * Explicit constructor
      * @param[in]  brakeSystemType Brake system type for this brake system.
      */
      explicit BrakeabilityBrakeSystemSteps(
        const BrakeSystemType brakeSystemType);

      /**
      * Sets the percentage of operative brakes for the cars, note that validatePercentageOfOperativeBrakes
      * must have been called prior to this function to make sure it is a valid value.
      * @param[in]  newPercentageOfOperativeBrakes The percentage of operative brakes for the cars
      */
      void setPercentageOfOperativeBrakes(
        const uint8_t newPercentageOfOperativeBrakes);

      /**
      * Validates the percentage of operative brakes for the cars
      * @param[in]  newPercentageOfOperativeBrakes The percentage of operative brakes for the cars to be validated
      * @return true if validation is successful.
      */
      bool validatePercentageOfOperativeBrakes(
        const uint8_t newPercentageOfOperativeBrakes) const;

      /**
      * Invalidate the brake data
      */
      virtual void invalidate();

    protected:

      /**
      * Invalidate the brakeability
      */
      virtual uint8_t getBrakeStep(const uint32_t v) const;

      /**
      * Calculate and update the brakeability steps
      */
      virtual void updateBrakeabilitySteps();

      /**
      * Calculate and update the brake delay
      */
      virtual void updateBrakeDelay();

      /**
      * Calculates lambda for the given weights.
      * @param[in] trainDynamicWeight    The dynamic weights for the locomotive and cars
      * @param[in] locomotiveBrakeWeight The brake weight of the locomotive
      * @param[in] carsBrakeWeight       The brake weight of the cars
      * @return Lambda in percent.
      */
      virtual int32_t calculateLambdaPercentage(
        const int32_t trainDynamicWeight,
        const int32_t locomotiveBrakeWeight,
        const int32_t carsBrakeWeight) const;

      /**
      * The minimum percentage of operative brakes for the cars
      * @return Minimum percentage of operative brakes for the cars
      */
      virtual int32_t getMinimumPercentageOfOperativeBrakes() const;

    private:

      /**
      * Default constructor (disabled).
      */
      BrakeabilityBrakeSystemSteps();

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      BrakeabilityBrakeSystemSteps(const BrakeabilityBrakeSystemSteps&);

      /**
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      BrakeabilityBrakeSystem& operator = (const BrakeabilityBrakeSystemSteps&);

      /** The loaded and empty brake weights for the locomotive */
      int32_t brakeWeightLocomotiveArray[2];

      /** The loaded and empty brake weights for the cars */
      int32_t sumOfbrakeWeightCarsArray[2];

      /** The loaded and empty dynamic weights for the locomotive and cars */
      int32_t dynamicWeightTrainArray[2];

      /** The pre-calculated and stored brakeability for the speed ranges */
      int32_t currentBrakeabilityStepsArray[3];

      /** The pre-calculated and stored brake delay (0.1 seconds) */
      uint32_t currentServiceBrakeDelay;

      /** The pre-calculated and stored brake delay (0.1 seconds) */
      uint32_t currentEmergencyBrakeDelay;

      /** If the train is loaded or unloaded */
      TrainLoaded loadedIndex;

      /** Percentage of operative brakes */
      int32_t percentageOfOperativeBrakes;

      /** If valid */
      bool validBrakeData;
    };
  }
}
#endif
