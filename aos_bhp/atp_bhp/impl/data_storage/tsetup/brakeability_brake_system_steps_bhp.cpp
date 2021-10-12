/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2020
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of the BrakeabilityBrakeSystemStepsBhp class
*
******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "brakeability_brake_system_steps_bhp.hpp"
#include "config.hpp"

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
    BrakeabilityBrakeSystemStepsBhp::BrakeabilityBrakeSystemStepsBhp()
      :
      BrakeabilityBrakeSystemSteps(BrakeSystemType2)
    {
    }

    /******************************************************************************
    * getMinimumPercentageOfOperativeBrakes()
    ******************************************************************************/
    int32_t BrakeabilityBrakeSystemStepsBhp::getMinimumPercentageOfOperativeBrakes() const
    {
      return static_cast<int32_t>(Config::instance().getECPBMinPercBrakeCars());
    }

  }
}
