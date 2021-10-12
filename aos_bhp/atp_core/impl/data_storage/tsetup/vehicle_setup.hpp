#ifndef VehicleSetup_hpp
#define VehicleSetup_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class declares the core VehicleSetup as it is stored by TSetup for each vehicle
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-18   bhermans    Created
* 2016-05-05   bhermans    Design continued
*
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
    * The class VehicleSetup declares the core vehicle setup as it is stored by TSetup for each vehicle
    */
    class VehicleSetup
    {
    public:
      VehicleSetup();

      /**
      * Virtual destructor
      */
      virtual ~VehicleSetup();

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

      virtual void invalidate();                        //!< Set all fields to default values 

      uint8_t           vehicleType;                    //!< Vehicle type
      uint16_t          nodeAdress;                     //!< Vehicle node address
      char_t            vehicleName[vehicleNameMaxLength + 1U];  //!< Name of this vehicle
    };
  }
}

#endif
