/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the Adaptation class of location mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-05-15    spandita    Created
*
*******************************************************************************/
#include "location_mode_bhp.hpp"
#include "abstract_mode_control.hpp"
#include "vehicle_com.hpp"
#include "config.hpp"
#include "abstract_dmi_handler.hpp"

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
  namespace Kernel
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    LocationModeBHP::LocationModeBHP() : LocationMode()
    {

    }

    /******************************************************************************
    * runLocFinishOK
    ******************************************************************************/
    void LocationModeBHP::runLocFinishOK(CommonDataForModes &commonData)
    {
      //Reset the freeRolling
      commonData.freeRolling = false;
      //Set the next mode
      LocationMode::runLocFinishOK(commonData);
    }

    /******************************************************************************
    * manageHandlingDone
    ******************************************************************************/
    void LocationModeBHP::manageHandlingDone(CommonDataForModes & commonData)
    {
      LocationMode::manageHandlingDone(commonData);

      if (!commonData.handlingDone)
      {
        //Get the button status
        const bool rclHandlingDone = TG::VehicleCom::instance().getHandlingDoneRequestReceived();
        //is ATO mode Manual?
        const bool isManualAtoMode = (ATOModeManual == AbstractModeControl::corePtr()->getATOMode());
        if ((Pos::AbstractOdometry::corePtr()->isTrainStandStill())
          && rclHandlingDone && isManualAtoMode)
        {
          setHandlingDone(commonData);
        }
      }
    }

    /******************************************************************************
    * manageFreeRolling
    ******************************************************************************/
    void LocationModeBHP::manageFreeRolling(CommonDataForModes &commonData)
    {
      //Get the ATO mode
      const bool isAtoModeManual = (ATOModeManual == AbstractModeControl::corePtr()->getATOMode());
      //Is standstill?
      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      //Is free rolling button pressed?
      const DMICom::DMIButtonStatus buttonStatus = DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus();
      //Check is loco type is EMD?
      const bool isEMDLoco = (EMD == static_cast<LocoTypeAdap>(AbstractConfig::corePtr()->getLocoType())) ? true : false;

      TG::FreeRollingStatusType freeRollingStatus;
      //Check if train status message field ->Free rolling
      if (TG::VehicleCom::instance().getFreeRollingStatus(freeRollingStatus))
      {//If status if free rolling
        if ((TG::FreeRollingStatusFreeRoll == freeRollingStatus) &&
          (!commonData.freeRolling) && (isStandStill) && isAtoModeManual)
        {
          commonData.freeRollingButton.freeRollingDisplayToDMI = true;
          commonData.freeRollingButton.freeRollingConfirmToDMI = false;
        }
        //if status is normal
        else if ((TG::FreeRollingStatusNormalOperation == freeRollingStatus) &&
          (commonData.freeRolling))
        {
          commonData.freeRollingButton.freeRollingDisplayToDMI = false;
          commonData.freeRollingButton.freeRollingConfirmToDMI = true;
        }
        else
        {
          commonData.freeRollingButton.freeRollingDisplayToDMI = false;
        }

        if (DMICom::DMIButtonConfirmFreeRollingCleared == buttonStatus)
        {
          commonData.freeRolling = false;
          commonData.freeRollingButton.freeRollingConfirmToDMI = false;
        }
        else if ((DMICom::DMIButtonRequestFreeRolling == buttonStatus) && isStandStill
          && isAtoModeManual && isEMDLoco)
        {
          commonData.freeRolling = true;
          commonData.freeRollingButton.freeRollingDisplayToDMI = false;
        }
        else
        {
          //do nothing
        }
      }
    }
  }
}
