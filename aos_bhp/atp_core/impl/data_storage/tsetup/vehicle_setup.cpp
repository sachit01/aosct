/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of the storage of the core Vehicle setup.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-05-05    bhermans    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "vehicle_setup.hpp"
#include "string.h"
#include "abstract_cross_compare.hpp"
#include "cross_compare_array.hpp"


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
    VehicleSetup::VehicleSetup()
    {
      vehicleType = vehicleUndefined;
      nodeAdress = 0U;
      memset(&vehicleName[0], 0, sizeof(vehicleName)); // Clear all space available for vehicleName
    }

    /******************************************************************************
    * Destructor
    ******************************************************************************/
    VehicleSetup::~VehicleSetup()
    {
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void VehicleSetup::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&vehicleType));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&nodeAdress));
      crossCompare->addCrossCompareData(new Support::CrossCompareArray<char_t>(&vehicleName[0], vehicleNameMaxLength + 1U));
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void VehicleSetup::invalidate()
    {
      vehicleType = vehicleUndefined;
      nodeAdress = 0U;                                  //!< Vehicle node address
      memset(&vehicleName[0], 0, sizeof(vehicleName));  // Clear all space available for vehicleName
    }
  }
}
