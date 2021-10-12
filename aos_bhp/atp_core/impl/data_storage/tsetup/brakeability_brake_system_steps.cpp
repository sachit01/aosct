/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of the BrakeabilityBrakeSystemSteps class
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-03-23    rquensel    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "brakeability_brake_system_steps.hpp"
#include "abstract_config.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_array.hpp"
#include "abstract_targets.hpp"
#include "abstract_tsetup.hpp"
#include "atp_types.hpp"
#include "atc_math.hpp"

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
  namespace DS
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    BrakeabilityBrakeSystemSteps::BrakeabilityBrakeSystemSteps(
      const BrakeSystemType brakeSystemType) :
      BrakeabilityBrakeSystem(brakeSystemType),
      currentServiceBrakeDelay(0U),
      currentEmergencyBrakeDelay(0U),
      loadedIndex(TrainIsLoaded),
      percentageOfOperativeBrakes(100),
      validBrakeData(false)
    {
      memset(&brakeWeightLocomotiveArray[0], 0, sizeof(brakeWeightLocomotiveArray));
      memset(&sumOfbrakeWeightCarsArray[0], 0, sizeof(sumOfbrakeWeightCarsArray));
      memset(&dynamicWeightTrainArray[0], 0, sizeof(dynamicWeightTrainArray));
      memset(&currentBrakeabilityStepsArray[0], 0, sizeof(currentBrakeabilityStepsArray));
    }

    /******************************************************************************
    * validatePercentageOfOperativeBrakes()
    ******************************************************************************/
    bool BrakeabilityBrakeSystemSteps::validatePercentageOfOperativeBrakes(const uint8_t newPercentageOfOperativeBrakes) const
    {
      return (static_cast<int32_t>(newPercentageOfOperativeBrakes) >= getMinimumPercentageOfOperativeBrakes());
    }

    /******************************************************************************
    * setPercentageOfOperativeBrakes()
    ******************************************************************************/
    void BrakeabilityBrakeSystemSteps::setPercentageOfOperativeBrakes(const uint8_t newPercentageOfOperativeBrakes)
    {
      if (percentageOfOperativeBrakes != static_cast<int32_t>(newPercentageOfOperativeBrakes))
      {
        percentageOfOperativeBrakes = static_cast<int32_t>(newPercentageOfOperativeBrakes);
        updateBrakeabilitySteps();
      }
    }

    /******************************************************************************
    * invalidate()
    ******************************************************************************/
    void BrakeabilityBrakeSystemSteps::invalidate()
    {
      BrakeabilityBrakeSystem::invalidate();
      percentageOfOperativeBrakes = 100;
      loadedIndex = TrainIsLoaded;
      validBrakeData = false;
    }

    /******************************************************************************
    * initCrossCompare()
    ******************************************************************************/
    void BrakeabilityBrakeSystemSteps::initCrossCompare() const
    {
      BrakeabilityBrakeSystem::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareArray<int32_t>(&brakeWeightLocomotiveArray[0], 2U));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareArray<int32_t>(&sumOfbrakeWeightCarsArray[0], 2U));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareArray<int32_t>(&dynamicWeightTrainArray[0], 2U));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareArray<int32_t>(&currentBrakeabilityStepsArray[0], 3U));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&currentServiceBrakeDelay));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&currentEmergencyBrakeDelay));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TrainLoaded>(&loadedIndex));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&percentageOfOperativeBrakes));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&validBrakeData));
    }

    /******************************************************************************
    * getMostRestrictiveBrakeability()
    ******************************************************************************/
    int32_t BrakeabilityBrakeSystemSteps::getBrakeabilityInRange(const uint32_t v1, const uint32_t v2) const
    {
      int32_t brakeAbility = 0;

      // Check valid train setup is available
      const bool validTrainSetup = DS::AbstractTSetup::corePtr()->isTrainSetupValid();
      if(!validTrainSetup)
      {
        // Set the current brake ability to value defined in the configuration when a valid train setup is not present
        brakeAbility = static_cast<int32_t>(AbstractConfig::corePtr()->getDefaultBrakeability());
      }
      else
      {
        uint8_t brakeStepV1 = getBrakeStep(v1);
        uint8_t brakeStepV2 = getBrakeStep(v2);

        if(brakeStepV1 > brakeStepV2)
        {
          // Swap brakeStepV1 and V2
          const uint8_t prevBrakeStepV1 = brakeStepV1;
          brakeStepV1 = brakeStepV2;
          brakeStepV2 = prevBrakeStepV1;
        }

        // Set the brakeability to the last value
        brakeAbility = currentBrakeabilityStepsArray[brakeStepV2];

        for(uint8_t i = brakeStepV1; i < brakeStepV2; ++i)
        {
          const int32_t brakeAbilityAtStep = currentBrakeabilityStepsArray[i];
          brakeAbility = ATC::ATCMath::minimum(brakeAbility, brakeAbilityAtStep);
        }
      }
      return brakeAbility;
    }

    /******************************************************************************
    * getBrakeability()
    ******************************************************************************/
    int32_t BrakeabilityBrakeSystemSteps::getBrakeability(const uint32_t v) const
    {
      int32_t brakeAbility = 0;

      // Check valid train setup is available
      const bool validTrainSetup = DS::AbstractTSetup::corePtr()->isTrainSetupValid();
      if(!validTrainSetup)
      {
        // Set the current brake ability to value defined in the configuration when a valid train setup is not present
        brakeAbility = static_cast<int32_t>(AbstractConfig::corePtr()->getDefaultBrakeability());
      }
      else
      {
        const int32_t adhesion = static_cast<int32_t>(DS::AbstractTargets::corePtr()->getAdhesionValue());
        const uint8_t brakeStep = getBrakeStep(v);
        brakeAbility = (currentBrakeabilityStepsArray[brakeStep] * adhesion) / 100;
      }
      return brakeAbility;
    }

    /******************************************************************************
    * updateBrakeabilitySteps()
    ******************************************************************************/
    void BrakeabilityBrakeSystemSteps::updateBrakeabilitySteps()
    {
      // The lambda calculation is done by : ("brake weight of the locomotive" + "sum of the brake weight of all cars" 
      // "minimum percentage of 1 and 2") / "dynamic weight of the locomotive + sum of dynamic weight of all cars"
      const int32_t totalBrakeWeight = (brakeWeightLocomotiveArray[loadedIndex] * 100) + 
        (sumOfbrakeWeightCarsArray[loadedIndex] * percentageOfOperativeBrakes);
      const int32_t totalDynamicWeight = dynamicWeightTrainArray[loadedIndex];

      const BrakeSystemType currentBrakeSystemInUse = getBrakeSystemType();
      int32_t lambdaPercentage = 0;
      if(totalDynamicWeight > 0)
      {
        lambdaPercentage = (totalBrakeWeight / totalDynamicWeight);
      }

      const int32_t lambdaMax = static_cast<int32_t>(AbstractConfig::corePtr()->getBrakeLambdaMax(currentBrakeSystemInUse));

      // Check if lambda is higher than max
      if(lambdaPercentage > lambdaMax)
      {
        lambdaPercentage = lambdaMax;
        AbstractTSetup::corePtr()->getTrace()->write(1U, "Lambda higher than max", lambdaPercentage);
      }

      AbstractTSetup::corePtr()->getTrace()->write(1U, "Lambda ", lambdaPercentage);
      const int32_t a1 = static_cast<int32_t>(AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterA1));
      const int32_t b1 = static_cast<int32_t>(AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterB1));
      const int32_t a2 = static_cast<int32_t>(AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterA2));
      const int32_t b2 = static_cast<int32_t>(AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterB2));
      const int32_t a3 = static_cast<int32_t>(AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterA3));
      const int32_t b3 = static_cast<int32_t>(AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterB3));

      currentBrakeabilityStepsArray[0] = ((a1 * lambdaPercentage) + (b1)) / 100;
      currentBrakeabilityStepsArray[1] = ((a2 * lambdaPercentage) + (b2)) / 100;
      currentBrakeabilityStepsArray[2] = ((a3 * lambdaPercentage) + (b3)) / 100;

      AbstractTSetup::corePtr()->getTrace()->write(1U, "currentBrakeabilityStepsArray[0]  ", currentBrakeabilityStepsArray[0]);
      AbstractTSetup::corePtr()->getTrace()->write(1U, "currentBrakeabilityStepsArray[1]  ", currentBrakeabilityStepsArray[1]);
      AbstractTSetup::corePtr()->getTrace()->write(1U, "currentBrakeabilityStepsArray[2]  ", currentBrakeabilityStepsArray[2]);
      // Indicate  to the world that we have changed brakeability
      increaseChangeCounter();
    }

    /******************************************************************************
    * updateBrakeDelay()
    ******************************************************************************/
    void BrakeabilityBrakeSystemSteps::updateBrakeDelay()
    {
      // Brake delay (Td) shall be calculated based on the following formula. In the formula t1, t2 and t3 are configured
      // parameters: Td = t1 + (t2 * L) + ((t3 * L * L)/1000)
      // L is in meters. t3 is in µs/m2, so it is divided by 1000 to convert to ms.
      const BrakeSystemType currentBrakeSystemInUse = getBrakeSystemType();
      
      uint32_t currentTrainLength = DS::AbstractTSetup::corePtr()->getMaxConsecutiveCarLength(); // In CM
      uint32_t trainLengthinMeters = (currentTrainLength / 100U);

      const uint32_t t1_SB = AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterT1SB);
      const uint32_t t2_SB = AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterT2SB);
      const uint32_t t3_SB = AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterT3SB);

      currentServiceBrakeDelay = (t1_SB + (t2_SB * trainLengthinMeters)
                                 + (((t3_SB * trainLengthinMeters * trainLengthinMeters) + 500U) / 1000U));

      // Stored in 0.1 second resolution (rounded)...
      currentServiceBrakeDelay = (currentServiceBrakeDelay + 50U) / 100U;

      const uint32_t t1_EB = AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterT1EB);
      const uint32_t t2_EB = AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterT2EB);
      const uint32_t t3_EB = AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterT3EB);

      currentEmergencyBrakeDelay = (t1_EB + (t2_EB * trainLengthinMeters)
                                   + (((t3_EB * trainLengthinMeters * trainLengthinMeters) + 500U) / 1000U));

      // Stored in 0.1 second resolution (rounded)...
      currentEmergencyBrakeDelay = (currentEmergencyBrakeDelay + 50U) / 100U;
   
    }

    /******************************************************************************
    * getBrakeStep()
    ******************************************************************************/
    uint8_t BrakeabilityBrakeSystemSteps::getBrakeStep(const uint32_t v) const
    {
      const BrakeSystemType currentBrakeSystemInUse = getBrakeSystemType();

      const uint32_t brakeParameterV1 = AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterV1);
      const uint32_t brakeParameterV2 = AbstractConfig::corePtr()->getBrakeParameter(currentBrakeSystemInUse, BrakeParameterV2);

      uint8_t brakeStep = 0U;

      // brakeStep = 0: v <= V1
      // brakeStep = 1: V1 < v <= V2
      // brakeStep = 2: V2 < v

      if((v <= brakeParameterV1) || (brakeParameterV1 == 0U))
      {
        // brakeStep = 0U; // Already set.
      }
      else if((v <= brakeParameterV2) || (brakeParameterV2 == 0U))
      {
        brakeStep = 1U;
      }
      else
      {
        brakeStep = 2U;
      }

      return brakeStep;
    }

    /******************************************************************************
    * getServiceBrakeResponseTime()
    ******************************************************************************/
    uint32_t BrakeabilityBrakeSystemSteps::getServiceBrakeResponseTime() const
    {
      uint32_t val = 0U;

      // Check valid train setup is available
      const bool validTrainSetup = DS::AbstractTSetup::corePtr()->isTrainSetupValid();
      if(!validTrainSetup)
      {
        // Set the brake delay to value defined in the configuration while no valid train setup is present
        val = AbstractConfig::corePtr()->getDefaultBrakeDelay();

        //Converting brake response delay from ms to 0.1s
        val = (val + 50U) / 100U;
      }
      else
      {
        val = currentServiceBrakeDelay;
      }
      return val;
    }

    /******************************************************************************
    * getEmergencyBrakeResponseTime()
    ******************************************************************************/
    uint32_t BrakeabilityBrakeSystemSteps::getEmergencyBrakeResponseTime() const
    {
      uint32_t val = 0U;

      // Check valid train setup is available
      const bool validTrainSetup = DS::AbstractTSetup::corePtr()->isTrainSetupValid();
      if(!validTrainSetup)
      {
        // Set the brake delay to value defined in the configuration while no valid train setup is present
        val = AbstractConfig::corePtr()->getDefaultBrakeDelay();

        //Converting brake response delay from ms to 0.1s
        val = (val + 50U) / 100U;
      }
      else
      {
        val = currentEmergencyBrakeDelay;
      }
      return val;
    }

    /******************************************************************************
    * getMinimumPercentageOfOperativeBrakes()
    ******************************************************************************/
    int32_t BrakeabilityBrakeSystemSteps::getMinimumPercentageOfOperativeBrakes() const
    {
      return 100;
    }

    /******************************************************************************
    * calculateLambdaPercentage()
    ******************************************************************************/
    int32_t BrakeabilityBrakeSystemSteps::calculateLambdaPercentage(
      const int32_t trainDynamicWeight,
      const int32_t locomotiveBrakeWeight,
      const int32_t carsBrakeWeight) const
    {
      // The lambda calculation is done by : ("brake weight of the locomotive" + "sum of the brake weight of all cars" 
      // "minimum percentage of 1 and 2") / "dynamic weight of the locomotive + sum of dynamic weight of all cars"
      const int32_t totalBrakeWeight = (locomotiveBrakeWeight * 100) + (carsBrakeWeight * getMinimumPercentageOfOperativeBrakes());

      int32_t lambda = 0;
      if(trainDynamicWeight > 0)
      {
        lambda = totalBrakeWeight / trainDynamicWeight;
      }

      return lambda;
    }

    /******************************************************************************
    * validateLambda()
    ******************************************************************************/
    bool
      BrakeabilityBrakeSystemSteps::validateLambda(
      const int32_t trainDynamicWeightLoaded,
      const int32_t trainDynamicWeightEmpty,
      const int32_t locomotiveBrakeWeightLoadedBrakeSystem,
      const int32_t locomotiveBrakeWeightEmptyBrakeSystem,
      const int32_t carsBrakeWeightLoadedBrakeSystem,
      const int32_t carsBrakeWeightEmptyBrakeSystem) const
    {
      const int32_t lambdaPercentageLoaded = calculateLambdaPercentage(
        trainDynamicWeightLoaded, locomotiveBrakeWeightLoadedBrakeSystem, carsBrakeWeightLoadedBrakeSystem);
      const int32_t lambdaPercentageEmpty = calculateLambdaPercentage(
        trainDynamicWeightEmpty, locomotiveBrakeWeightEmptyBrakeSystem, carsBrakeWeightEmptyBrakeSystem);

      const BrakeSystemType currentBrakeSystemInUse = getBrakeSystemType();
      const int32_t lambdaMin = static_cast<int32_t>(AbstractConfig::corePtr()->getBrakeLambdaMin(currentBrakeSystemInUse));

      bool valid;

      if((0 == locomotiveBrakeWeightLoadedBrakeSystem) && (0 == carsBrakeWeightLoadedBrakeSystem)
        && (0 == locomotiveBrakeWeightEmptyBrakeSystem) && (0 == carsBrakeWeightEmptyBrakeSystem))
      {
        //All 0s indicate brake system is not used. This valid lambda value.
        valid = true;  
      }
      else
      {
        valid = (lambdaPercentageLoaded >= lambdaMin) && (lambdaPercentageEmpty >= lambdaMin);
      }

      return valid;
    }

    /******************************************************************************
    * isValidLambda()
    ******************************************************************************/
    bool BrakeabilityBrakeSystemSteps::isValidLambda() const
    {
      bool valid = true;
      //Lambda calculation for loaded train
      const int32_t lambdaPercentageLoaded = calculateLambdaPercentage(
        dynamicWeightTrainArray[loadedIndex], brakeWeightLocomotiveArray[loadedIndex], sumOfbrakeWeightCarsArray[loadedIndex]);

      //Lambda calculation for empty train
      const int32_t lambdaPercentageEmpty = calculateLambdaPercentage(
        dynamicWeightTrainArray[TrainIsEmpty], brakeWeightLocomotiveArray[TrainIsEmpty], sumOfbrakeWeightCarsArray[TrainIsEmpty]);

      const BrakeSystemType currentBrakeSystemInUse = getBrakeSystemType();

      const int32_t lambdaMin = static_cast<int32_t>(AbstractConfig::corePtr()->getBrakeLambdaMin(currentBrakeSystemInUse));

      // Check if lambda is lower then minimum configured value
      if((lambdaPercentageLoaded < lambdaMin) || (lambdaPercentageEmpty < lambdaMin))
      {
        valid = false;
      }

      return valid;
    }

    /******************************************************************************
    * isBrakeDataEqualOrBetter()
    ******************************************************************************/
    bool BrakeabilityBrakeSystemSteps::isBrakeDataEqualOrBetter(
      const int32_t trainDynamicWeightLoaded,
      const int32_t trainDynamicWeightEmpty,
      const int32_t locomotiveBrakeWeightLoadedBrakeSystem,
      const int32_t locomotiveBrakeWeightEmptyBrakeSystem,
      const int32_t carsBrakeWeightLoadedBrakeSystem,
      const int32_t carsBrakeWeightEmptyBrakeSystem) const
    {
      bool equalOrBetter;

      if(!validBrakeData)
      {
        equalOrBetter = true;
      }
      else
      {
        // If brake data is already valid, new brake data must be better than the existing

        const int32_t lambdaPercentageLoaded = calculateLambdaPercentage(
          trainDynamicWeightLoaded, locomotiveBrakeWeightLoadedBrakeSystem, carsBrakeWeightLoadedBrakeSystem);
        const int32_t lambdaPercentageEmpty = calculateLambdaPercentage(
          trainDynamicWeightEmpty, locomotiveBrakeWeightEmptyBrakeSystem, carsBrakeWeightEmptyBrakeSystem);

        const int32_t currentLambdaLoadedPercentage = calculateLambdaPercentage(
          dynamicWeightTrainArray[TrainIsLoaded], brakeWeightLocomotiveArray[TrainIsLoaded],
          sumOfbrakeWeightCarsArray[TrainIsLoaded]);
        const int32_t currentLambdaEmptyPercentage = calculateLambdaPercentage(
          dynamicWeightTrainArray[TrainIsEmpty], brakeWeightLocomotiveArray[TrainIsEmpty],
          sumOfbrakeWeightCarsArray[TrainIsEmpty]);

        equalOrBetter = ((lambdaPercentageLoaded >= currentLambdaLoadedPercentage) &&
          (lambdaPercentageEmpty >= currentLambdaEmptyPercentage));
      }

      return equalOrBetter;
    }

    /******************************************************************************
    * setBrakeData()
    ******************************************************************************/
    void BrakeabilityBrakeSystemSteps::setBrakeData(
      const int32_t trainDynamicWeightLoaded,
      const int32_t trainDynamicWeightEmpty,
      const int32_t locomotiveBrakeWeightLoadedBrakeSystem,
      const int32_t locomotiveBrakeWeightEmptyBrakeSystem,
      const int32_t carsBrakeWeightLoadedBrakeSystem,
      const int32_t carsBrakeWeightEmptyBrakeSystem)
    {
      // Convert train length from cm to meters.

      dynamicWeightTrainArray[TrainIsLoaded] = trainDynamicWeightLoaded;
      dynamicWeightTrainArray[TrainIsEmpty] = trainDynamicWeightEmpty;

      // Locomotive
      brakeWeightLocomotiveArray[TrainIsLoaded] = locomotiveBrakeWeightLoadedBrakeSystem;
      brakeWeightLocomotiveArray[TrainIsEmpty] = locomotiveBrakeWeightEmptyBrakeSystem;

      // Cars
      sumOfbrakeWeightCarsArray[TrainIsLoaded] = carsBrakeWeightLoadedBrakeSystem;
      sumOfbrakeWeightCarsArray[TrainIsEmpty] = carsBrakeWeightEmptyBrakeSystem;

      updateBrakeDelay();
      updateBrakeabilitySteps();

      validBrakeData = true;
    }


    /******************************************************************************
    * setBrakeDataTrainLoaded()
    ******************************************************************************/
    void BrakeabilityBrakeSystemSteps::setBrakeDataTrainLoaded(const TrainLoaded trainLoaded)
    {
      if(loadedIndex != trainLoaded)
      {
        loadedIndex = trainLoaded;
        updateBrakeabilitySteps();
      }
    }

  }
}
