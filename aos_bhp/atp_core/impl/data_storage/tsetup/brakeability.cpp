/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of the Brakeability adaptation class
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
#include "brakeability.hpp"
#include "brakeability_brake_system.hpp"
#include "abstract_config.hpp"
#include "abstract_cross_compare.hpp"
#include "abstract_tsetup.hpp"
#include "atp_types.hpp"
#include "atc_math.hpp"
#include "atc_util.hpp"
#include "vehicle_com.hpp"

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
    Brakeability::Brakeability() :
      numberOfBrakeabilitySystemsInUse(0U),
      currentBrakeSystemInUse(BrakeSystemTypeUndefined),
      changeCounter(0U)
    {
      for (uint8_t i = 0U; i < maxNumberOfBrakeabilitySystems; ++i)
      {
        brakeAbilitySystems[i] = static_cast<BrakeabilityBrakeSystem*>(NULL);
      }
    }

    /******************************************************************************
    * Destructor. Placeholder for derived implementations.
    ******************************************************************************/
    Brakeability::~Brakeability()
    {
    }

    /******************************************************************************
    * setDefaultBrakeabilityBrakeSystem()
    ******************************************************************************/
    void Brakeability::setDefaultBrakeabilityBrakeSystem(BrakeabilityBrakeSystem* const defaultBrakeabilityBrakeSystem)
    {
      brakeAbilitySystems[BrakeSystemTypeUndefined] = defaultBrakeabilityBrakeSystem;
    }

    /******************************************************************************
    * setBrakeabilityBrakeSystems()
    ******************************************************************************/
    void Brakeability::setBrakeabilityBrakeSystems(
      BrakeabilityBrakeSystem* const defaultBrakeabilityBrakeSystemType1,
      BrakeabilityBrakeSystem* const defaultBrakeabilityBrakeSystemType2,
      BrakeabilityBrakeSystem* const defaultBrakeabilityBrakeSystemType3)
    {
      brakeAbilitySystems[BrakeSystemType1] = defaultBrakeabilityBrakeSystemType1;
      brakeAbilitySystems[BrakeSystemType2] = defaultBrakeabilityBrakeSystemType2;
      brakeAbilitySystems[BrakeSystemType3] = defaultBrakeabilityBrakeSystemType3;

      if (defaultBrakeabilityBrakeSystemType1 == static_cast<BrakeabilityBrakeSystem*>(NULL))
      {
        ATC::aosHalt(__FILE__, __LINE__, "No brake system defined!"); // This will exit our system!
      }

      if (defaultBrakeabilityBrakeSystemType2 == static_cast<BrakeabilityBrakeSystem*>(NULL))
      {
        numberOfBrakeabilitySystemsInUse = 1U;
      }
      else if (defaultBrakeabilityBrakeSystemType3 == static_cast<BrakeabilityBrakeSystem*>(NULL))
      {
        numberOfBrakeabilitySystemsInUse = 2U;
      }
      else
      {
        numberOfBrakeabilitySystemsInUse = 3U;
      }
    }

    /******************************************************************************
    * init()
    ******************************************************************************/
    void Brakeability::init()
    {
      invalidate();
    }

    /******************************************************************************
    * initCrossCompare()
    ******************************************************************************/
    void Brakeability::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&numberOfBrakeabilitySystemsInUse));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<BrakeSystemType>(&currentBrakeSystemInUse));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&changeCounter));

      for (uint8_t i = 0U; i <= numberOfBrakeabilitySystemsInUse; ++i)
      {
        brakeAbilitySystems[i]->initCrossCompare();
      }
    }

    /******************************************************************************
    * updateBrakeSystemType()
    ******************************************************************************/
    void Brakeability::updateBrakeSystemType(const BrakeSystemType newBrakeSystemType)
    {
      if (currentBrakeSystemInUse != newBrakeSystemType)
      {
        currentBrakeSystemInUse = newBrakeSystemType;
        ++changeCounter;
      }
    }

    /******************************************************************************
    * validateLambda()
    ******************************************************************************/
    bool
    Brakeability::validateLambda(
      const int32_t    trainDynamicWeightLoaded,
      const int32_t    trainDynamicWeightEmpty,
      const int32_t    locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
      const int32_t    locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems],
      const int32_t    carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
      const int32_t    carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems]) const
    {
      bool result = true;

      uint8_t brakeSystemTypeIndex = static_cast<uint8_t>(BrakeSystemType1);

      for (uint8_t i = 0U; i < numberOfBrakeabilitySystemsInUse; ++i)
      {
        result = brakeAbilitySystems[brakeSystemTypeIndex]->validateLambda(
          trainDynamicWeightLoaded,
          trainDynamicWeightEmpty,
          locomotiveBrakeWeightLoadedBrakeSystem[i],
          locomotiveBrakeWeightEmptyBrakeSystem[i],
          carsBrakeWeightLoadedBrakeSystem[i],
          carsBrakeWeightEmptyBrakeSystem[i]) && result;

        ++brakeSystemTypeIndex;
      }
      return result;
    }

    /******************************************************************************
    * isBrakeDataEqualOrBetter()
    ******************************************************************************/
    bool Brakeability::isBrakeDataEqualOrBetter(
      const int32_t    trainDynamicWeightLoaded,
      const int32_t    trainDynamicWeightEmpty,
      const int32_t    locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
      const int32_t    locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems],
      const int32_t    carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
      const int32_t    carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems]) const
    {
      bool result = true;

      uint8_t brakeSystemTypeIndex = static_cast<uint8_t>(BrakeSystemType1);

      for (uint8_t i = 0U; i < numberOfBrakeabilitySystemsInUse; ++i)
      {
        result = brakeAbilitySystems[brakeSystemTypeIndex]->isBrakeDataEqualOrBetter(
          trainDynamicWeightLoaded,
          trainDynamicWeightEmpty,
          locomotiveBrakeWeightLoadedBrakeSystem[i],
          locomotiveBrakeWeightEmptyBrakeSystem[i],
          carsBrakeWeightLoadedBrakeSystem[i],
          carsBrakeWeightEmptyBrakeSystem[i]) && result;

        ++brakeSystemTypeIndex;
      }
      return result;
    }

    /******************************************************************************
    * setBrakeData()
    ******************************************************************************/
    void Brakeability::setBrakeData(
      const int32_t    trainDynamicWeightLoaded,
      const int32_t    trainDynamicWeightEmpty,
      const int32_t    locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
      const int32_t    locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems],
      const int32_t    carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
      const int32_t    carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems])
    {
      uint8_t brakeSystemTypeIndex = static_cast<uint8_t>(BrakeSystemType1);

      for (uint8_t i = 0U; i < numberOfBrakeabilitySystemsInUse; ++i)
      {
        brakeAbilitySystems[brakeSystemTypeIndex]->setBrakeData(
          trainDynamicWeightLoaded,
          trainDynamicWeightEmpty,
          locomotiveBrakeWeightLoadedBrakeSystem[i],
          locomotiveBrakeWeightEmptyBrakeSystem[i],
          carsBrakeWeightLoadedBrakeSystem[i],
          carsBrakeWeightEmptyBrakeSystem[i]);

        ++brakeSystemTypeIndex;
      }
    } //lint !e1762 A setter function shouldn't be const

    /******************************************************************************
    * invalidate()
    ******************************************************************************/
    void Brakeability::invalidate()
    {
      updateBrakeSystemType(BrakeSystemTypeUndefined);

      uint8_t brakeSystemTypeIndex = static_cast<uint8_t>(BrakeSystemType1);

      for (uint8_t i = 0U; i < numberOfBrakeabilitySystemsInUse; ++i)
      {
        brakeAbilitySystems[brakeSystemTypeIndex]->invalidate();

        ++brakeSystemTypeIndex;
      }
      ++changeCounter;
    }

    /******************************************************************************
    * setBrakeDataTrainLoaded()
    ******************************************************************************/
    void
    Brakeability::setBrakeDataTrainLoaded(const TrainLoaded trainLoaded) const
    {
      uint8_t brakeSystemTypeIndex = static_cast<uint8_t>(BrakeSystemType1);

      for (uint8_t i = 0U; i < numberOfBrakeabilitySystemsInUse; ++i)
      {
        brakeAbilitySystems[brakeSystemTypeIndex]->setBrakeDataTrainLoaded(trainLoaded);

        ++brakeSystemTypeIndex;
      }
    }

    /******************************************************************************
    * getBrakeabilityBrakeSystemInUse()
    ******************************************************************************/
    BrakeabilityBrakeSystem*
    Brakeability::getBrakeabilityBrakeSystemInUse()
    {
      return brakeAbilitySystems[currentBrakeSystemInUse];
    } //lint !e1762 A const function shouldn't return a non-const pointer

    /******************************************************************************
    * getBrakeabilityBrakeSystemInUse()
    ******************************************************************************/
    const BrakeabilityBrakeSystem*
    Brakeability::getBrakeabilityBrakeSystemInUse() const
    {
      return brakeAbilitySystems[currentBrakeSystemInUse];
    }

    /******************************************************************************
    * getBrakeSystemInUse()
    ******************************************************************************/
    BrakeSystemType
    Brakeability::getBrakeSystemInUse() const    
    {
      return currentBrakeSystemInUse;
    }

    /******************************************************************************
    * getChangeCounter()
    ******************************************************************************/
    uint32_t Brakeability::getChangeCounter() const
    {
      uint32_t sumOfBrakeCounters = changeCounter;

      for (uint8_t i = 0U; i <= numberOfBrakeabilitySystemsInUse; ++i)
      {
        const BrakeabilityBrakeSystem* const pBrakeSystem = brakeAbilitySystems[i];

        sumOfBrakeCounters += pBrakeSystem->getChangeCounter();
      }

      return sumOfBrakeCounters;
    }

  }
}
