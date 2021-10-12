#ifndef BrakeabilityBrakeSystemStepsBhp_hpp
#define BrakeabilityBrakeSystemStepsBhp_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2020
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration of the adaptation class BrakeabilityBrakeSystemStepsBhp.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2020-04-23    rquensel    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "brakeability_brake_system_steps.hpp"

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
    class BrakeabilityBrakeSystemStepsBhp : public BrakeabilityBrakeSystemSteps
    {
    public:

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      explicit BrakeabilityBrakeSystemStepsBhp();

    protected:

      /**
      * The minimum percentage of operative brakes for the cars
      * @return Minimum percentage of operative brakes for the cars
      */
      virtual int32_t getMinimumPercentageOfOperativeBrakes() const;

    private:

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      BrakeabilityBrakeSystemStepsBhp(const BrakeabilityBrakeSystemStepsBhp&);

      /**
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      BrakeabilityBrakeSystemStepsBhp& operator = (const BrakeabilityBrakeSystemStepsBhp&);

    };
  }
}
#endif
