#ifndef BrakeabilityBrakeSystemDefault_hpp
#define BrakeabilityBrakeSystemDefault_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration of the class BrakeabilityBrakeSystemDefault.
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
    class BrakeabilityBrakeSystemDefault : public  BrakeabilityBrakeSystem
    {
    public:

      /**
      * Implements the virtual init function.
      *
      */
      virtual void init();

      /**
      * Get the most restrictive brakeability between two speed segments speed
      *
      * @param[in]  v1  Speed segment 1 (cm/s) for the brakeability.
      * @param[in]  v2  Speed segment 2 (cm/s) for the brakeability.
      * @return The most restrictive brakeability for the provided speed segment (cm/s2)
      */
      virtual int32_t getBrakeabilityInRange(const uint32_t v1, const uint32_t v2) const;

      /**
      * Get the brakeability for a given speed
      *
      * @param[in]  v  Speed segment (cm/s) for the brakeability.
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
      * @param[in]  trainDynamicWeightLoaded  Dynamic weight for loaded locomotive plus cars.
      * @param[in]  trainDynamicWeightEmpty   Dynamic weight for empty locomotive plus cars.
      * @param[in]  locomotiveBrakeWeightLoadedBrakeSystem  Loaded brake weight for the locomotive
      * @param[in]  locomotiveBrakeWeightEmptyBrakeSystem   Empty brake weight for the locomotive
      * @param[in]  carsBrakeWeightLoadedBrakeSystem        Loaded brake weight for the cars
      * @param[in]  carsBrakeWeightEmptyBrakeSystem         Empty brake weight for the cars
      * @return true if the calculated brake lambda is within the limit
      */
      virtual bool validateLambda(
        const int32_t    trainDynamicWeightLoaded,
        const int32_t    trainDynamicWeightEmpty,
        const int32_t    locomotiveBrakeWeightLoadedBrakeSystem,
        const int32_t    locomotiveBrakeWeightEmptyBrakeSystem,
        const int32_t    carsBrakeWeightLoadedBrakeSystem,
        const int32_t    carsBrakeWeightEmptyBrakeSystem) const;

      /**
      * Checks if the given parameters would lead to equal or better brake ability than the stored parameters.
      *
      * @param[in]  trainDynamicWeightLoaded   Dynamic weight for loaded locomotive plus cars
      * @param[in]  trainDynamicWeightEmpty   Dynamic weight for empty locomotive plus cars
      * @param[in]  locomotiveBrakeWeightLoadedBrakeSystem  Loaded brake weight for the locomotive
      * @param[in]  locomotiveBrakeWeightEmptyBrakeSystem   Empty brake weight for the locomotive
      * @param[in]  carsBrakeWeightLoadedBrakeSystem        Loaded brake weight for the cars
      * @param[in]  carsBrakeWeightEmptyBrakeSystem         Empty brake weight for the cars
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
      * @param[in]  trainDynamicWeightLoaded  Dynamic weight for loaded locomotive plus cars.
      * @param[in]  trainDynamicWeightEmpty   Dynamic weight for empty locomotive plus cars.
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
      * @param[in]  trainLoaded  Set if the train is loaded or empty
      */
      virtual void setBrakeDataTrainLoaded(const TrainLoaded trainLoaded);

      /**
      * Constructor
      */
      BrakeabilityBrakeSystemDefault();

    protected:


    private:

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      BrakeabilityBrakeSystemDefault(const BrakeabilityBrakeSystemDefault&);

      /** 
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      BrakeabilityBrakeSystemDefault& operator = (const BrakeabilityBrakeSystemDefault&);
    };
  }
}
#endif
