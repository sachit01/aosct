#pragma once
#include "stdafx.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec:  LocoEngine.cpp-4:c++:arn_006#3 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LocoEngine.cpp %
*
*  %version:       4 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2016-12-09 16:36 %
*
*  DESCRIPTION: 
*              
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2013-04-22    Antbäck     File created
* 2013-06-10    Antbäck     Use MaxRet during retardation, not MaxAcc
* 2014-03-07    Antbäck     Imported to AOS-PC
* 2015-02-26    Antbäck     Use SB/EB as brake force reduced from throttle value
* 2016-10-04    Marlundg    Changes in I/O for BHP
* 2016-10-16    Marlundg    Set acceleration to 0 when speed reached max or min.
* 2016-12-09    Marlundg    Do not add contribution from throttle value if SB or EB is set.
*
*******************************************************************************/


using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;

#include <Mmsystem.h>
#include "LocoEngine.h"

/******************************************************************************
* Function:     Tick
* Description:  
******************************************************************************/
void LocoSimDLL::LocoEngine::Tick(enum TBSystemStatus  systemStatus, 
                      int             lThrottle,
                      EnumDriveDir    lDriveDir, 
                      bool            lCabin1,
                      bool            lCabin2,
                      bool            lLCSReady,
                      bool            lEmerStopActive,
                      bool            lSBActive,
                      Int16           effectiveGradient,
                      UInt16          brakeability,
                      UInt16          brakeDelayEB,
                      UInt16          brakeDelaySB
)

{
    PlatformTime deltaTime;
    PlatformTime currTime, currTimeB;
    UInt16 brakeabSB, brakeabEB;

    currTime = timeGetTime();
    //if its the first execution time.
    if (LastCurrTime == 0)
    {
      LastCurrTime = currTime;
      sbOld = lSBActive;
      ebOld = lEmerStopActive;
    }

    if(brakeability == 0)
    {
      brakeabSB = locoParams->SBRet;
      brakeabEB = locoParams->EBRet;
    }
    else
    {
      brakeabSB = brakeability;
      brakeabEB = brakeability;
    }

    // Select current acceleration value
    // If CabinOn and LCSReady and SB and EB are not applied use throttle
    // SB applied is checked if SB deceleration is more than 0. This is to check if SB is present or not.
    if ((lCabin1 || lCabin2) && lLCSReady && !lEmerStopActive && !(lSBActive && (locoParams->SBRet != 0)))
    {
      if ((DDForward == lDriveDir) ||
        (DDReverse == lDriveDir))
      {
        if (lThrottle >= 0)
        {
          localAcc = (lThrottle / 100.0) * locoParams->MaxAcc * 10;    // Convert to mm/s
        }
        else
        {
          localAcc = (lThrottle / 100.0) * locoParams->MaxRet * 10;   // Convert to mm/s
        }
      }
      else
      {
        localAcc = 0;
      }
    }
    else
    {
      localAcc = 0;
    }

    // Check if any EB is requested
    if (lEmerStopActive)
    {
      //if EB changed from not applied to applied
      if (ebOld != lEmerStopActive)
      {
        //set the time to start deceleration
        ebAppTime = currTime + brakeDelayEB*100; //0.1s to ms
      }

      //if delay time elapsed for EB
      if (currTime >= ebAppTime)
      {
        //add gradient to brakeability
        localAcc = -(brakeabEB + effectiveGradient) * 10;   // Convert to mm/s
      }
      else
      {
        if (localSpeed > 0)
        {
          //if delay for EB is not elapsed, accelerate due to gradient.
          // this assumes traction is cutout once EB is applied.
          localAcc = - effectiveGradient * 10; // Convert to mm/s
        }
      }
    }
    // If locomotive not active, apply EB
    else if ((!lCabin1 && !lCabin2) || !lLCSReady)
    {
      localAcc = localAcc - brakeabEB * 10;   // Convert to mm/s
    }
    // Check if SB is requested and SB has greater than 0 deceleration
    else if (lSBActive && (locoParams->SBRet != 0))
    {
      //if SB changed from not applied to applied
      if (sbOld != lSBActive)
      {
        //set the time to start deceleration
        sbAppTime = currTime + brakeDelaySB*100; //0.1s to ms
      }

      //if delay time elapsed for SB
      if (currTime >= sbAppTime)
      {
        //add gradient to brakeability
        localAcc = -(brakeabSB + effectiveGradient) * 10;   // Convert to mm/s
      }
      else
      {
        if (localSpeed > 0)
        {
          //if delay for SB is not elapsed, accelerate due to gradient.
          // this assumes traction is cutout once EB is applied.
          localAcc = - effectiveGradient * 10; // Convert to mm/s
        }
      }
    }
    else if (DDNeutral == lDriveDir)
    {
      localAcc = localAcc - (brakeabSB + effectiveGradient) * 10;   // Convert to mm/s
    }

    // Manage time since last call
    currTimeB = ~(currTime);

    deltaTime = currTime - LastCurrTime;
    LastCurrTime = currTime;

    /*************************/
    /* Calculate a new speed */
    /*************************/
    if ((localSpeed < (locoParams->MaxSpeed * 10)) &&
      (localAcc >= 0))
    {
      localSpeed = localSpeed + (localAcc * deltaTime) / 1000.0; /* mm/s */
      localSpeed = min(localSpeed, (locoParams->MaxSpeed * 10));

      // No acceleration if maximum speed is reached
      if (localSpeed >= locoParams->MaxSpeed * 10)
      {
        localAcc = 0;
      }

    }
    else if ((localSpeed > 0) &&
      (localAcc < 0))
    {
      localSpeed = localSpeed + (localAcc * deltaTime) / 1000.0; /* mm/s */
      localSpeed = max(localSpeed, 0);
    }

    // No speed if not running
    if (TBSystemRunning != systemStatus)
    {
      localAcc = -locoParams->EBRet * 10;
      localSpeed = 0;
    }

    // Export values
    LocoSpeed = localSpeed / 10;

    // No acceleration if minimum speed is reached
    LocoAcc = localSpeed <= 0 ? 0 : localAcc / 10;

    //update eb and sb old values
    ebOld = lEmerStopActive;
    sbOld = lSBActive;

}

