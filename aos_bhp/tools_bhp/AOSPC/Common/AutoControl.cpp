#pragma once
#include "stdafx.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec: %"

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2019
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:    Later.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2019-01-12    bhermans    File created
*
*******************************************************************************/

#include "AutoControl.h"
using namespace System;
using namespace System::Net;
using namespace System::Net::Sockets;


namespace AOSPC
{
  /******************************************************************************
  * Function:     AutoControl constructor
  ******************************************************************************/
  AutoControl::AutoControl(void)
  {
    delayTicks = 0;
    state = AutoControlInactive;
  }


  /******************************************************************************
  * Function:     run
  ******************************************************************************/
  void AutoControl::run(UInt16 locoSpeed, bool AOSSBReq, bool AOSEBReq , bool systemRunning, Int16 & locoThrottle, EnumDriveDir & locoDriveDir, ATPModeEnum  atpMode, UInt16 permittedSpeed, EnumDriveDir permittedDriveDir, UInt16 distanceToTarget, UInt16 distanceToBCA, UInt16 targetSpeed)
  {
    // Keep speed between limit 1 and limit 2
    UInt16 BCAMargin = (locoSpeed * autoControlBCAMarginSecs) / 100U; // m (start decelerate 5 seconds before BCA)
    UInt16 factor1 = (distanceToBCA < BCAMargin) ? (autoControlSpeedLimit1Perc - 10U) : autoControlSpeedLimit1Perc; // %
    UInt16 factor2 = (distanceToBCA < BCAMargin) ? (autoControlSpeedLimit2Perc - 10U) : autoControlSpeedLimit2Perc; // %

    UInt16 driverSpeedLimit1 = (permittedSpeed * factor1) / 100U; // cm/s
    UInt16 driverSpeedLimit2 = (permittedSpeed * factor2) / 100U; // cm/s

    // Not enabled, quit
    if (!autoControlEnabled || !validAutoControlMode(atpMode) || AOSSBReq || AOSEBReq)
    {
      state = AutoControlInactive;
      return;
    }

    // AOS still running, if not quit any attempt to run AutoControl
    if (!systemRunning)
    {
      state = AutoControlInactive;
      locoThrottle = 0;
      locoDriveDir = DDNeutral;
      return;
    }

    bool lowSpeed = (locoSpeed < 150U) ? true : false; // (cm/s) Accelerate less at low speed to make driving smoother at low speeds

    int autoControlAccThrottle = 50; // (int)(((double)locoParams->AutoRegAcceleration / (double)locoParams->MaxAcc) * 100);
    int autoControlDecThrottle = -50;
    int autoControlSlowAccThrottle = 12;

    bool withinMAMargin = false;
    // AutoControl disregards MA Margin when in BaliseSearch mode
    if (atpMode != ATPModeBaliseSearch)
    {
      // evaluate only when primary target (targetspeed is 0)
      if (targetSpeed == 0)
      {
        // evaluate only when distance to target is not 0 because distance to target may be 0 when passing a speed-target
        if (distanceToTarget > 0)
        {
          // within MA margin from primary target?
          withinMAMargin = ((distanceToBCA == 0) && (distanceToTarget < autoControlMarginToTarget)) ? true : false;
        }
      }
    }

    // Always keep Loco at ready state
    locoThrottle = 0;

    // Assign driving direction, but not if both 
    if (permittedDriveDir != DDBoth)
    {
      locoDriveDir = permittedDriveDir;
    }

    // Set actions depending on current state
    switch (state)
    {
    case AutoControlInactive:
      state = AutoControlIdle;
      delayTicks = 0;
      break;
    case AutoControlIdle:
      if (permittedSpeed > 0)
      {
        delayTicks++;
        // Wait for standstill-event to clear before moving
        if (delayTicks > 20)
        {
          state = AutoControlAccelerate;
          delayTicks = 0;
        }
      }
      break;

    case AutoControlAccelerate:
      if ((distanceToBCA < BCAMargin) || lowSpeed)
        locoThrottle = autoControlSlowAccThrottle;
      else
        locoThrottle = autoControlAccThrottle;

      if (locoSpeed > driverSpeedLimit1)
        state = AutoControlCoasting;
      break;

    case AutoControlCoasting:

      if ((locoSpeed > driverSpeedLimit2) || withinMAMargin)
        state = AutoControlDecelerate;
      else if (locoSpeed < driverSpeedLimit1)
        state = AutoControlAccelerate;

      break;

    case AutoControlDecelerate:
      locoThrottle = autoControlDecThrottle;
      // Decelerated enough if speed too low and not within MA margin
      if ((locoSpeed < driverSpeedLimit1) && !withinMAMargin)
        state = AutoControlCoasting;
      break;

    }
  }
  /******************************************************************************
  * Function:     setEnabled
  ******************************************************************************/
  void AutoControl::setEnabled(bool enabled)
  {
    autoControlEnabled = enabled;
  }

  /******************************************************************************
  * Function:     setParam
  ******************************************************************************/
  void AutoControl::setParam(UInt16 bcaMarginSecs, UInt16 speedLimit1Perc, UInt16 speedLimit2Perc)
  {
    autoControlBCAMarginSecs = bcaMarginSecs;
    autoControlSpeedLimit1Perc = speedLimit1Perc;
    autoControlSpeedLimit2Perc = speedLimit2Perc;

  }
  /******************************************************************************
  * Function:     setMAMargin (m)
  ******************************************************************************/
  void AutoControl::setMAMargin(UInt16 marginToTarget)
  {
    autoControlMarginToTarget = marginToTarget; // m
  }
  /******************************************************************************
  * Function:     getMAMargin (m)
  ******************************************************************************/
  UInt16 AutoControl::getMAMargin(void)
  {
    return autoControlMarginToTarget;
  }
  /******************************************************************************
  * Function:     validAutoControlMode
  * Description:  Check that ATP mode is valiud for AutoControl
  ******************************************************************************/

  bool AutoControl::validAutoControlMode(ATPModeEnum  atpMode)
  {     // We may need to support more modes when needed
    if ((ATPModeBaliseSearch == atpMode) || (ATPModeNormal == atpMode) || (ATPModeStaffResponsible == atpMode) || (ATPModeLocation == atpMode))
      return true;
    else
      return false;
  }

  /******************************************************************************
  * Function:     getState
  * Description:  get AutoControl state
  ******************************************************************************/
  EnumAutoControlState AutoControl::getState(void)
  {
    return state;
  }
}

