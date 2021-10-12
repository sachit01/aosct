#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LocoEngine.h %
*
*  %version:       2 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2016-10-04 13:24 %
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
* 2013-04-21    Antbäck     File created
* 2014-03-07    Antbäck     Imported to AOS-PC
* 2016-10-04    Marlundg    Changes in I/O for BHP
*
*******************************************************************************/

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;


#include "LocoConsts.h"
#include "LocoParams.h"

namespace LocoSimDLL {
/// Summary for LocoEngine
public ref class LocoEngine 
{
public:
    // Outputs
    double          LocoSpeed;              // cm/s
    double          LocoAcc;                // cm/s2

    // Constructor
    // ===========
    LocoEngine(LocoParams^ lP)
    {
        // Keep pointer to params
        locoParams  = lP;

        // Initialize values
        LocoSpeed = 0;
        LocoAcc = 0;

        LastCurrTime        = 0;
        localAcc            = 0;
        localSpeed          = 0;
    }

    void Tick(enum TBSystemStatus  systemStatus, 
        int       lThrottle,
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
    );

protected:
    LocoParams^ locoParams;
    PlatformTime            LastCurrTime;
    double                  localAcc;       // mm/s2
    double                  localSpeed;     // mm/s
    bool                    sbOld;
    bool                    ebOld;
    PlatformTime            sbAppTime;
    PlatformTime            ebAppTime;

    // Destructor
    // ==========
    ~LocoEngine()
    {
    }
};
}