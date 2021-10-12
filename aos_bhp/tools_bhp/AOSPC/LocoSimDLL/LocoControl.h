#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LocoControl.h %
*
*  %version:       4 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-03-20 17:40 %
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
* 2017-03-10    Marlundg    Support for new I/O
*
*******************************************************************************/

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;

#include "LocoConsts.h"
#include "LocoEngine.h"
#include "LocoIO.h"
#include "LCSCom.h"
#include "RailDevCom.h"

namespace LocoSimDLL {
/// Summary for LocoControl
public ref class LocoControl 
{
public:

    // Selected controls/data:s
    EnumSelectedController  SelectedController;
    int                     LocoThrottle;
    EnumATOMODE             LocoATOSwitch;
    EnumDriveDir            LocoDriveDir; 
    bool                    LocoCabin1;
    bool                    LocoCabin2;
    bool                    LocoLCSReady;
    bool                    LocoEmerStopActive;
    bool                    LocoAOSOff;
    bool                    LocoNcu;
    bool                    LocoEb1a;
    bool                    LocoEb1b;
    bool                    LocoEb2a;
    bool                    LocoEb2b;
    bool                    LocoIsolA;
    bool                    LocoIsolB;
    bool                    LocoRoadM;
    bool                    LocoRailM;
    bool                    LocoTcoFb;
    UInt16                  tcoFbOffsetCounter;

    // Constructor
    // ===========
    LocoControl(void)
    {
        // Create dynamic objects

        // Initialize values
        LocoATOSwitch = ATOSwitchMan;
        LocoDriveDir = DDNeutral; 
        oldSelectedController = SelCtrlDriver;

        tcoFbOffsetCounter = 0;
    }

    void Tick(enum TBSystemStatus  systemStatus, 
        int             lsThrottle,
        EnumATOMODE     lsATOSwitch,
        EnumDriveDir    lsDriveDir,
        bool            lsCabin1Click,
        bool            lsCabin2Click,
        bool            lsLCSReadyClick,
        bool            lsEmerStopActiveClick,
        bool            lsAOSOffClick,
        bool            lsNcuClick,
        bool            lsEb1aClick,
        bool            lsEb1bClick,
        bool            lsEb2aClick,
        bool            lsEb2bClick,
        bool            lsIsolAClick,
        bool            lsIsolBClick,
        bool            lsRoadMClick,
        bool            lsRailMClick,
        LCSCom^         lcsCom,
        LocoSimDLL::RailDevCom^ rdCom,
        EnumTcoFb                LocoSimTcoFeedbackChoice,
        bool                     TcoOrdered,
        bool                     EbOrdered,
        UInt16                   tcoFbOffset
      );

protected:
    EnumSelectedController  oldSelectedController;

    // Destructor
    // ==========
    ~LocoControl()
    {
    }
};
}