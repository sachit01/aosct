/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of the BrakeabilityBrakeSystemDefault adaptation class
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
#include "brakeability_brake_system_default.hpp"
#include "abstract_config.hpp"
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
    * setBrakeData
    ******************************************************************************/
    void BrakeabilityBrakeSystemDefault::setBrakeData(
      const int32_t  /*trainDynamicWeightLoaded*/,
      const int32_t  /*trainDynamicWeightEmpty*/,
      const int32_t  /*locomotiveBrakeWeightLoadedBrakeSystem*/,
      const int32_t  /*locomotiveBrakeWeightEmptyBrakeSystem*/,
      const int32_t  /*carsBrakeWeightLoadedBrakeSystem*/,
      const int32_t  /*carsBrakeWeightEmptyBrakeSystem*/)
    {
    }

    /******************************************************************************
    * setBrakeDataTrainLoaded
    ******************************************************************************/
    void BrakeabilityBrakeSystemDefault::setBrakeDataTrainLoaded(const TrainLoaded /*trainLoaded*/)
    {
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    BrakeabilityBrakeSystemDefault::BrakeabilityBrakeSystemDefault()
      :
      BrakeabilityBrakeSystem(BrakeSystemTypeUndefined)
    {
    }


    /******************************************************************************
    * init()
    ******************************************************************************/
    void BrakeabilityBrakeSystemDefault::init()
    {
    }

    /******************************************************************************
    * getBrakeabilityInRange()
    ******************************************************************************/
    int32_t BrakeabilityBrakeSystemDefault::getBrakeabilityInRange(const uint32_t /*v1*/, const uint32_t /*v2*/) const
    {
      const int32_t brakeAbility = static_cast<int32_t>(AbstractConfig::corePtr()->getDefaultBrakeability());

      return brakeAbility;
    }

    /******************************************************************************
    * getBrakeability()
    ******************************************************************************/
    int32_t BrakeabilityBrakeSystemDefault::getBrakeability(const uint32_t /*v*/) const
    {
      const int32_t brakeAbility = static_cast<int32_t>(AbstractConfig::corePtr()->getDefaultBrakeability());

      return brakeAbility;
    }

    /******************************************************************************
    * getServiceBrakeResponseTime()
    ******************************************************************************/
    uint32_t BrakeabilityBrakeSystemDefault::getServiceBrakeResponseTime() const
    {
      return AbstractConfig::corePtr()->getDefaultBrakeDelay();
    }

    /******************************************************************************
    * getEmergencyBrakeResponseTime()
    ******************************************************************************/
    uint32_t BrakeabilityBrakeSystemDefault::getEmergencyBrakeResponseTime() const
    {
      return AbstractConfig::corePtr()->getDefaultBrakeDelay();
    }

    /******************************************************************************
    * validateLambda()
    ******************************************************************************/
    bool BrakeabilityBrakeSystemDefault::validateLambda(
      const int32_t /*trainDynamicWeightLoaded*/,
      const int32_t /*trainDynamicWeightEmpty*/,
      const int32_t /*locomotiveBrakeWeightLoadedBrakeSystem*/,
      const int32_t /*locomotiveBrakeWeightEmptyBrakeSystem*/,
      const int32_t /*carsBrakeWeightLoadedBrakeSystem*/,
      const int32_t /*carsBrakeWeightEmptyBrakeSystem*/) const
    {
      return true;
    }

    /******************************************************************************
    * isBrakeDataEqualOrBetter()
    ******************************************************************************/
    bool BrakeabilityBrakeSystemDefault::isBrakeDataEqualOrBetter(
      const int32_t /*trainDynamicWeightLoaded*/,
      const int32_t /*trainDynamicWeightEmpty*/,
      const int32_t /*locomotiveBrakeWeightLoadedBrakeSystem*/,
      const int32_t /*locomotiveBrakeWeightEmptyBrakeSystem*/,
      const int32_t /*carsBrakeWeightLoadedBrakeSystem*/,
      const int32_t /*carsBrakeWeightEmptyBrakeSystem*/) const
    {
      return true;
    }
  }
}
