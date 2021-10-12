#ifndef BrakeabilityBrakeSystem_hpp
#define BrakeabilityBrakeSystem_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration of the class BrakeabilityBrakeSystem.
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
#include "atp_types.hpp"

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
    class BrakeabilityBrakeSystem
    {
    public:

      /**
      * Implements the virtual init function.
      *
      */
      virtual void init();

      /**
      * Initializes the cross compare module.
      */
      virtual void initCrossCompare() const;

      /**
      * Get the most restrictive brakeability between two speed segments speed
      *
      * @param[in]  v1  Speed segment 1 (cm/s) for the brakeability.
      * @param[in]  v2  Speed segment 2 (cm/s) for the brakeability.
      * @return The most restrictive brakeability for the provided speed segment (cm/s2)
      */
      virtual int32_t getBrakeabilityInRange(const uint32_t v1, const uint32_t v2) const = 0;

      /**
      * Get the brakeability for a given speed
      *
      * @param[in]  v  Speed segment (cm/s) for the brakeability.
      * @return The brakeability for the provided speed segment  (cm/s2)
      */
      virtual int32_t getBrakeability(const uint32_t v) const = 0;

      /**
      * Get service brake response time
      *
      * @return The service brake response time (in 0.1 seconds) for the current train
      */
      virtual uint32_t getServiceBrakeResponseTime() const = 0;
      
      /**
      * Get emergency brake response time
      *
      * @return The brake emergency response time (in 0.1 seconds) for the current train
      */
      virtual uint32_t getEmergencyBrakeResponseTime() const = 0;

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
        const int32_t    carsBrakeWeightEmptyBrakeSystem) const = 0;

      /**
      * Checks if the given parameters would lead to equal or better brake ability than the stored parameters.
      *
      * @param[in]  trainDynamicWeightLoaded  Dynamic weight for loaded locomotive plus cars
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
        const int32_t carsBrakeWeightEmptyBrakeSystem) const = 0;

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
        const int32_t    carsBrakeWeightEmptyBrakeSystem) = 0;

      /**
      * Set if the train is loaded or not
      * @param[in]  trainLoaded  Set if the train is loaded or empty
      */
      virtual void setBrakeDataTrainLoaded(const TrainLoaded trainLoaded) = 0;

      /**
      * Get the change counter
      */
      uint32_t getChangeCounter() const;


      /**
      * Get the brake system type
      */
      BrakeSystemType getBrakeSystemType() const;

      /**
      * Invalidate the brake data
      */
      virtual void invalidate();

    protected:

      /**
      * Virtual Destructor
      */
      virtual ~BrakeabilityBrakeSystem();

      /**
      * Constructor
      * @param[in]  brakeSystemType  The brake system type
      */
      explicit BrakeabilityBrakeSystem(const BrakeSystemType brakeSystemType);

      /**
      * Increase the change counter
      */
      void increaseChangeCounter();

    private:

      /**
      * Constructor (not used, declare it private)
      */
      explicit BrakeabilityBrakeSystem();

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      BrakeabilityBrakeSystem(const BrakeabilityBrakeSystem&);

      /**
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      BrakeabilityBrakeSystem& operator = (const BrakeabilityBrakeSystem&);

      /**
      * The brake system type
      */
      const BrakeSystemType thisBrakeSystemType;

      /**
      * The change counter increased when a change is done
      */
      uint32_t changeCounter;
    };
  }
}
#endif
