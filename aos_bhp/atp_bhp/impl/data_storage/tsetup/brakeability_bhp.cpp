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
#include "brakeability_bhp.hpp"
#include "brakeability_brake_system_steps_bhp.hpp"
#include "config.hpp"
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
    BrakeabilityBhp::BrakeabilityBhp() :
      Brakeability(),
      pSystem1(static_cast<BrakeabilityBrakeSystemSteps*>(NULL)),
      pSystem2(static_cast<BrakeabilityBrakeSystemSteps*>(NULL)),
      bhpDefaultBrakeSystemPneumatic(BrakeSystemType1)  // For BHP default is Pneumatic/1
    {
      setDefaultBrakeabilityBrakeSystem(&defaultBrakeSystem);
    }

    /******************************************************************************
    * validatePercentageOfOperativeBrakes()
    ******************************************************************************/
    bool BrakeabilityBhp::validatePercentageOfOperativeBrakes(const uint8_t percentageOfOperativeBrakes) const
    {
      bool result = false;
      if (pSystem2 != static_cast<BrakeabilityBrakeSystemSteps*>(NULL))
      {
        result = pSystem2->validatePercentageOfOperativeBrakes(percentageOfOperativeBrakes);
      }

      return result;
    }

    /******************************************************************************
    * setPercentageOfOperativeBrakes()
    ******************************************************************************/
    void BrakeabilityBhp::setPercentageOfOperativeBrakes(const uint8_t percentageOfOperativeBrakes)
    {
      if (pSystem2 != static_cast<BrakeabilityBrakeSystemSteps*>(NULL))
      {
        // This is the maximum used brake perc, the value cannot get higher than this.
        const uint8_t maximumPercentageOfPercentageOfBrakedCarsUsed = Config::instance().getECPBCarsInBrakeCalc();
        const uint8_t brakePercUsed = ATC::ATCMath::minimum(percentageOfOperativeBrakes, maximumPercentageOfPercentageOfBrakedCarsUsed);
        pSystem2->setPercentageOfOperativeBrakes(brakePercUsed);
      }
    }

    /******************************************************************************
    * isValidLambda()
    ******************************************************************************/
    bool BrakeabilityBhp::isValidLambda() const
    {
      bool result = false;
      if (pSystem2 != static_cast<BrakeabilityBrakeSystemSteps*>(NULL))
      {
        result = pSystem2->isValidLambda();
      }

      return result;
    }


    /******************************************************************************
    * init()
    ******************************************************************************/
    void BrakeabilityBhp::init()
    {
      //lint --e{586} 'new' is acceptable during initialization

      pSystem1 = new BrakeabilityBrakeSystemSteps(BrakeSystemType1);
     
      const LocoTypeAdap locoType = static_cast<LocoTypeAdap>(Config::instance().getLocoType());

      // Check if we have an EMD loco, only EMD has brake system type 2...
      if (locoType == EMD)
      {
        pSystem2 = new BrakeabilityBrakeSystemStepsBhp();
      }

      setBrakeabilityBrakeSystems(pSystem1, pSystem2, static_cast<BrakeabilityBrakeSystemSteps*>(NULL));
      
      Brakeability::init();

      // BHP default brake system is Pneumatic
      updateBrakeSystemType(bhpDefaultBrakeSystemPneumatic);
    }

    /******************************************************************************
    * invalidate()
    ******************************************************************************/
    void BrakeabilityBhp::invalidate()
    {
      Brakeability::invalidate();

      // BHP default brake system is Pneumatic
      updateBrakeSystemType(bhpDefaultBrakeSystemPneumatic);
    }
  }
}
