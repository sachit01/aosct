#ifndef BrakeabilityBhp_hpp
#define BrakeabilityBhp_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration of the adaptation class Brakeability.
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
#include "brakeability.hpp"
#include "brakeability_brake_system_steps.hpp"
#include "brakeability_brake_system_default.hpp"

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
    class BrakeabilityBhp : public Brakeability
    {
    public:

      /**
      * Constructor
      *
      */
      BrakeabilityBhp();
      
      /**
      * Validates the percentage of operative brakes for the cars
      * @param[in]  percentageOfOperativeBrakes The percentage of operative brakes for the cars to be validated
      * @return true if validation is successful.
      */
      bool validatePercentageOfOperativeBrakes(const uint8_t percentageOfOperativeBrakes) const;

      /**
      * Sets the percentage of operative brakes for the cars
      * @param[in]  percentageOfOperativeBrakes The percentage of operative brakes for the cars
      */
      void setPercentageOfOperativeBrakes(const uint8_t percentageOfOperativeBrakes);

      /**
      * Implements isValidLambda
      * @return true if the calculated lambda is higher than minimum configured limit
      */
      bool isValidLambda() const;

      /**
      * Implements the virtual init function.
      *
      */
      virtual void init();

      /**
      * Invalidate the brakeability
      */
      virtual void invalidate();

    private:

      /** 
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      BrakeabilityBhp(const BrakeabilityBhp&);

      /** 
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      BrakeabilityBhp& operator = (const BrakeabilityBhp&);

      /** The default brake system */
      BrakeabilityBrakeSystemDefault defaultBrakeSystem;

      /** Brake system type 1 */
      BrakeabilityBrakeSystemSteps* pSystem1;

      /** Brake system type 2 */
      BrakeabilityBrakeSystemSteps* pSystem2;

      /** Default BHP BrakeSystemType, set to 1 in the constructor */
      const BrakeSystemType bhpDefaultBrakeSystemPneumatic;
    };
  }
}
#endif
