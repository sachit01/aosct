#ifndef Brakeability_hpp
#define Brakeability_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration of the class Brakeability.
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
    class Brakeability
    {
    public:

      /**
      * Destructor. Placeholder for derived implementations.
      */
      virtual ~Brakeability();

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
      * Update the brake system type
      *
      * @param[in]  newBrakeSystemType  the new brake system type
      */
      void updateBrakeSystemType(const BrakeSystemType newBrakeSystemType);

      /**
      * Validate the brake data
      *
      * @param[in]  trainDynamicWeightLoaded  Dynamic weight for loaded locomotive plus cars.
      * @param[in]  trainDynamicWeightEmpty   Dynamic weight for empty locomotive plus cars.
      * @param[in]  locomotiveBrakeWeightLoadedBrakeSystem  Array for the three loaded brake weights for the locomotive
      * @param[in]  locomotiveBrakeWeightEmptyBrakeSystem   Array for the three empty brake weights for the locomotive
      * @param[in]  carsBrakeWeightLoadedBrakeSystem        Array for the three loaded brake weights for the cars
      * @param[in]  carsBrakeWeightEmptyBrakeSystem         Array for the three empty brake weights for the cars
      * @return true if the calculated brake lambda is within the limit
      */
      bool validateLambda(
        const int32_t    trainDynamicWeightLoaded,
        const int32_t    trainDynamicWeightEmpty,
        const int32_t    locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems]) const;

      /**
      * Checks if the given parameters would lead to equal or better brake ability than the stored parameters.
      *
      * @param[in]  trainDynamicWeightLoaded Dynamic weight for loaded locomotive plus cars.
      * @param[in]  trainDynamicWeightEmpty  Dynamic weight for empty locomotive plus cars.
      * @param[in]  locomotiveBrakeWeightLoadedBrakeSystem  Array for the three loaded brake weights for the locomotive
      * @param[in]  locomotiveBrakeWeightEmptyBrakeSystem   Array for the three loaded brake weights for the locomotive
      * @param[in]  carsBrakeWeightLoadedBrakeSystem        Array for the three loaded brake weights for the cars
      * @param[in]  carsBrakeWeightEmptyBrakeSystem         Array for the three loaded brake weights for the cars
      * @return true if the given parameters would give equal or better brake ability
      */
      bool isBrakeDataEqualOrBetter(
        const int32_t    trainDynamicWeightLoaded,
        const int32_t    trainDynamicWeightEmpty,
        const int32_t    locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems]) const;

      /**
      * Set the brake data
      *
      * @param[in]  trainDynamicWeightLoaded  Dynamic weight for loaded locomotive plus cars.
      * @param[in]  trainDynamicWeightEmpty   Dynamic weight for empty locomotive plus cars.
      * @param[in]  locomotiveBrakeWeightLoadedBrakeSystem  Array for the three loaded brake weights for the locomotive
      * @param[in]  locomotiveBrakeWeightEmptyBrakeSystem   Array for the three empty brake weights for the locomotive
      * @param[in]  carsBrakeWeightLoadedBrakeSystem        Array for the three loaded brake weights for the cars
      * @param[in]  carsBrakeWeightEmptyBrakeSystem         Array for the three empty brake weights for the cars
      */
      void setBrakeData(
        const int32_t    trainDynamicWeightLoaded,
        const int32_t    trainDynamicWeightEmpty,
        const int32_t    locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems]);

      /**
      * Invalidate the brakeability
      */
      virtual void invalidate();

      /**
      * Set if the train is loaded or not
      * @param[in]  trainLoaded  Set if the train is loaded or empty
      */
      void setBrakeDataTrainLoaded(const TrainLoaded trainLoaded) const;

      /**
      * Get the current brakeability brake system in use
      * @return The current brakeability brake system in use
      */
      BrakeabilityBrakeSystem* getBrakeabilityBrakeSystemInUse();

      /**
      * Get the current const brakeability brake system in use
      * @return The current brakeability brake system in use
      */
      const BrakeabilityBrakeSystem* getBrakeabilityBrakeSystemInUse() const;

      /**
      * Get the current const brakeability brake system type in use
      * @return The current brakeability brake system type in use
      */
      BrakeSystemType getBrakeSystemInUse() const;

      /**
      * Get the current change counter (increased at every change)
      * @return The current change counter value
      */
      virtual uint32_t getChangeCounter() const;

    protected:

      /**
      * Maximum number of brakeability systems including the default (default is used when no configuration is available).
      */
      static const uint8_t maxNumberOfBrakeabilitySystems = maxBrakeSystems + 1U;

      /**
      * Constructor
      */
      Brakeability();

      /**
      * Set the default brake system
      * @param [in] defaultBrakeabilityBrakeSystem  Default brake system
      */
      void setDefaultBrakeabilityBrakeSystem(BrakeabilityBrakeSystem* const defaultBrakeabilityBrakeSystem);

      /**
      * Set brake systems for the different types
      * @param [in] defaultBrakeabilityBrakeSystemType1 Brake system 1
      * @param [in] defaultBrakeabilityBrakeSystemType2 Brake system 2
      * @param [in] defaultBrakeabilityBrakeSystemType3 Brake system 3
      */
      void setBrakeabilityBrakeSystems(
        BrakeabilityBrakeSystem* const defaultBrakeabilityBrakeSystemType1,
        BrakeabilityBrakeSystem* const defaultBrakeabilityBrakeSystemType2,
        BrakeabilityBrakeSystem* const defaultBrakeabilityBrakeSystemType3);

    private:

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      Brakeability(const Brakeability&);

      /**
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      Brakeability& operator = (const Brakeability&);


      /** The brakeability brake systems (including default) */
      BrakeabilityBrakeSystem* brakeAbilitySystems[maxNumberOfBrakeabilitySystems];

      /**
      * Number of brakeability systems in use.
      */
      uint8_t numberOfBrakeabilitySystemsInUse;

      /** Current brake system in use */
      BrakeSystemType currentBrakeSystemInUse;

      /** Change counter, increased when data is changed */
      uint32_t changeCounter;
    };
  }
}
#endif
