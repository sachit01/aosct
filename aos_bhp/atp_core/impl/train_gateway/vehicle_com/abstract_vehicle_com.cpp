/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the AbstractVehicleCom class
* which contains the core functionality of the VehicleCom.
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-24    marlundg    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "atc_util.hpp"
#include "abstract_vehicle_com.hpp"
#include "abstract_cross_compare.hpp"

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
  namespace TG
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractVehicleCom::AbstractVehicleCom()
      : ATC::IOComponent(atpVehicleComId, "VehicleCom", "VC"),
      abstractBaseInitialized(false)
    {
      if (coreVehicleComInstancePtr != 0)
      {
        // Error handler
        ATC::aosHalt(__FILE__, __LINE__, "VehicleCom Constructor already instantiated");
      }

      // Setup single instance pointer for core access
      coreVehicleComInstancePtr = this;
    }

    /******************************************************************************
    * connected
    ******************************************************************************/
    bool AbstractVehicleCom::connectedVehicleComm() const
    {
      return false;
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractVehicleCom* AbstractVehicleCom::corePtr()
    {
      return coreVehicleComInstancePtr;
    }

    /******************************************************************************
    * startupAndHealthSupTest
    ******************************************************************************/
    bool AbstractVehicleCom::startupAndHealthSupTest() const
    {
      return true;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    bool AbstractVehicleCom::init(void)
    {
      if (!abstractBaseInitialized)
      {
        abstractBaseInitialized = true;
        initCrossCompare();
      }
      return true;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractVehicleCom::initCrossCompare() const
    {
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&abstractBaseInitialized));
    }
  }
}
