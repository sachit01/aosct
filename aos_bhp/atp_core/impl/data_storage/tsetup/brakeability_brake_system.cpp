/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of the BrakeabilityBrakeSystem class
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
#include "brakeability_brake_system.hpp"
#include "abstract_config.hpp"
#include "abstract_cross_compare.hpp"
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
    uint32_t BrakeabilityBrakeSystem::getChangeCounter() const
    {
      return changeCounter;
    }

    /******************************************************************************
    * getBrakeSystemType()
    ******************************************************************************/
    BrakeSystemType BrakeabilityBrakeSystem::getBrakeSystemType() const
    {
      return thisBrakeSystemType;
    }

    /******************************************************************************
    * Destructor
    ******************************************************************************/
    BrakeabilityBrakeSystem::~BrakeabilityBrakeSystem()
    {
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    BrakeabilityBrakeSystem::BrakeabilityBrakeSystem(
      const BrakeSystemType brakeSystemType) :
      thisBrakeSystemType(brakeSystemType),
      changeCounter(0U)
    {
    }

    /******************************************************************************
    * increaseChangeCounter
    ******************************************************************************/
    void BrakeabilityBrakeSystem::increaseChangeCounter()
    {
      ++changeCounter;
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void BrakeabilityBrakeSystem::invalidate()
    {
    }


    /******************************************************************************
    * init()
    ******************************************************************************/
    void BrakeabilityBrakeSystem::init()
    {
    }

    /******************************************************************************
    * initCrossCompare()
    ******************************************************************************/
    void BrakeabilityBrakeSystem::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<BrakeSystemType>(&thisBrakeSystemType));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&changeCounter));
    }
    
  }
}
