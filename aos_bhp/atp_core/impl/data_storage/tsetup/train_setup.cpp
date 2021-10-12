/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class declares the Train setup as it is stored by TSetup
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-02-08    ragquens    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "train_setup.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_array.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace DS
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    TrainSetup::TrainSetup()
    {
      maxSpeed = 0U;
      length = 0U;
      orientation = 0U;
      timsSupNeeded = false;
      vehicleCount = 0U;
      changeDetails.brakeAbility = false;
      changeDetails.brakeResponseTime = false;
      state = TrainSetupStateTemporary;
    }

    /******************************************************************************
    * destructor
    ******************************************************************************/
    TrainSetup::~TrainSetup()
    {
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void TrainSetup::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      // variables for cross-compare.
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&maxSpeed));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&length));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&orientation));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&timsSupNeeded));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&vehicleCount));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&changeDetails.brakeResponseTime));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&changeDetails.brakeAbility));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TrainSetupState>(&state));
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void TrainSetup::invalidate()
    {
      maxSpeed = 0U;
      length = 0U;
      orientation = 0U;
      timsSupNeeded = false;
      vehicleCount = 0U;
      changeDetails.brakeAbility = false;
      changeDetails.brakeResponseTime = false;
      state = TrainSetupStateTemporary;
    }
  }
}
