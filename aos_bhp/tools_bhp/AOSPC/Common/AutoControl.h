#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:    Later.
*
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
#include "LocoConsts.h"

using namespace System;
using namespace System::Net;
using namespace System::Net::Sockets;
using namespace System::Text;

namespace AOSPC
{
  typedef enum
  {
    AutoControlInactive,
    AutoControlIdle,
    AutoControlAccelerate,
    AutoControlCoasting,
    AutoControlDecelerate
  } EnumAutoControlState;

  public ref class AutoControl
  {
  public:
    AutoControl(void);
    void setEnabled(bool enabled);
    void setParam(UInt16 bcaMarginSecs, UInt16 speedLimit1Perc, UInt16 speedLimit2Perc);
    void setMAMargin(UInt16 marginToTarget);
    UInt16 getMAMargin(void);
    void run(UInt16 locoSpeed, bool AOSSBReq, bool AOSEBReq, bool systemRunning, Int16 & locoThrottle, EnumDriveDir & locoDriveDir, ATPModeEnum  atpMode, UInt16 permittedSpeed, EnumDriveDir permittedDriveDir, UInt16 distanceToTarget, UInt16 distanceToBCA, UInt16 targetSpeed);
    EnumAutoControlState getState(void);
  private:
    bool          autoControlEnabled;
    UInt16        autoControlBCAMarginSecs;
    UInt16        autoControlSpeedLimit1Perc;
    UInt16        autoControlSpeedLimit2Perc;
    UInt16        autoControlMarginToTarget; // m

    EnumAutoControlState state;
    UInt16        delayTicks;

    bool validAutoControlMode(ATPModeEnum  atpMode);
  };
}
