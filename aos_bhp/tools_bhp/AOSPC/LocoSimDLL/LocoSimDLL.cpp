#pragma once
#include "stdafx.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec:  LocoSimDLL.cpp-7:c++:arn_006#2 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LocoSimDLL.cpp %
*
*  %version:       7 %
*
*  %created_by:    akushwah %
*
*  %date_created:  2017-07-13 15:06 %
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
* 2013-05-03    Antbäck     Handle driver intervention of AutoReg and better start/stop control
* 2013-07-10    Blomqvist   Added startup argument when starting ATx.
* 2013-11-19    Antbäck     Quit ATx processes when power down requested by AOS
* 2014-03-07    Antbäck     Imported to AOS-PC
* 2014-03-10    Antbäck     Added balise id handling, Reg + ReReg
* 2014-03-13    Antbäck     Removed TimsSim properties
* 2014-03-23    Antbäck     Added logic to support process handling in AOSPC
* 2014-03-25    Antbäck     DriverEB (GUI) to force EB active in call to LocoEngine
* 2014-04-14    Antbäck     Added LastReadBaliseForward
* 2015-02-26    Antbäck     AutoReg: Keep LocoReady off until initial delay ended to sort "no brake" problem
* 2016-10-04    Marlundg    Changes in I/O for BHP
* 2016-10-16    Marlundg    Rework of the start/stop mechanism without SQL.
* 2017-03-10    Marlundg    Support for new I/O
* 2018-11-13    Bo H        AutoControl
*
*******************************************************************************/


using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;
using namespace System::IO;
using namespace System::Diagnostics;
using namespace System::Runtime::InteropServices;

#include <timeapi.h>
#include "LocoSimDLL.h"

/******************************************************************************
* Function:     Init
* Description:  
******************************************************************************/
bool LocoSimDLL::LocoSimulation::Init(void)
{
    SimulatedUnitStatus SystemStatus;

    // Get setup parameters first 
    locoParams->ReadFromIniFile();

    // Initialise
    LocoIo->Init();
    lcsCom->Init();
    rdCom->Init();

    autoControl->setParam(locoParams->autoControlBCAMarginSecs, locoParams->autoControlSpeedLimit1Perc, locoParams->autoControlSpeedLimit2Perc);
    autoControl->setMAMargin(0);
    autoControl->setEnabled(locoParams->autoControlEnabled);


    // Set system status to pending startup initially
    SystemStatus.Status.TBSystem = TBSystemPendingStartUp;

    // Ready to run
    Initialised = true;

    return true;
}

/******************************************************************************
* Function:     ShutDown
* Description:  
******************************************************************************/
void LocoSimDLL::LocoSimulation::ShutDown(void)
{
    Process^ process;

    // TODO: Only ATP file in ATP-Limited
    if (GetProcessByName(locoParams->ATPFile, process))
    {
        process->Kill();
    }
}

/******************************************************************************
* Function:     Tick
* Description:  
******************************************************************************/
void LocoSimDLL::LocoSimulation::Tick(int           LocoSimThrottle,
                          EnumATOMODE   LocoSimATOSwitch,
                          EnumDriveDir  LocoSimDriveDir,
                          bool          LocoSimCabin1Click,
                          bool          LocoSimCabin2Click,
                          bool          LocoSimLCSReadyClick,
                          bool          LocoSimEmerStopActive,
                          bool          LocoSimAOSOffClick,  
                          bool          AOSStartRequested,
                          bool          AOSStopRequested,
                          bool          ATPStartRequested,
                          bool          ATOStartRequested,
                          bool          ATPToFatalFailureClicked,
                          bool          ThrottleChanged,
                          bool          LocoSimNCUClick,
                          bool          LocoSimEb1aClick,
                          bool          LocoSimEb1bClick,
                          bool          LocoSimEb2aClick,
                          bool          LocoSimEb2bClick,
                          bool          LocoSimIsolAClick,
                          bool          LocoSimIsolBClick,
                          bool          LocoSimRoadMClick,
                          bool          LocoSimRailMClick,
                          bool          automaticSimClick,
                          bool          manualSimClick,
                          int           bp1valueEntered,
                          int           bp2valueEntered,
                          EnumTcoFb     LocoSimTcoFeedbackChoice,
                          UInt16        tcoFbOffset,
                          UInt16        permittedSpeed,
                          EnumDriveDir  permittedDriveDir,
                          ATPModeEnum   atpMode,
                          UInt16        distanceToTarget,  
                          UInt16        distanceToBCA,
                          UInt16        targetSpeed,
                          Int16         trackGradient,
                          Int16         effectiveGradient,
                          UInt16        brakeability,
                          UInt16        brakeDelayEB,
                          UInt16        brakeDelaySB
)
{
    static DWORD lastTimeSendReceiveIOData;

    // Do not run unless initialized
    if (!Initialised)
    {
        return;
    }


    // Run simulation control
    SimulationControl(AOSStartRequested,
                      AOSStopRequested,
                      ATPStartRequested,
                      ATOStartRequested,
                      AOSPowerOff);

    // Read from any available LCS communication link
    lcsCom->ReadFromLCSClient();

    // Read from any available RailDev communication link
    rdCom->ReadFromRDClient();


    // Call LocoCtrl
    LocoCtrl->Tick(SystemStatus, 
        LocoSimThrottle, 
        LocoSimATOSwitch, 
        LocoSimDriveDir, 
        LocoSimCabin1Click,
        LocoSimCabin2Click,
        LocoSimLCSReadyClick, 
        LocoSimEmerStopActive,
        LocoSimAOSOffClick,
        LocoSimNCUClick,
        LocoSimEb1aClick,
        LocoSimEb1bClick,
        LocoSimEb2aClick,
        LocoSimEb2bClick,
        LocoSimIsolAClick,
        LocoSimIsolBClick,
        LocoSimRoadMClick,
        LocoSimRailMClick,
        lcsCom,
        rdCom,
        LocoSimTcoFeedbackChoice,
        !AOSTCO,  // Active Low
        (!AOSEB1) || (!AOSEB2), // Active Low
        tcoFbOffset
        );

    // Handle LocoCtrl output data
    LocoThrottle        = LocoCtrl->LocoThrottle;
    LocoATOSwitch       = LocoCtrl->LocoATOSwitch;
    LocoDriveDir        = LocoCtrl->LocoDriveDir; 
    LocoCabin1          = LocoCtrl->LocoCabin1;
    LocoCabin2          = LocoCtrl->LocoCabin2;
    LocoLCSReady        = LocoCtrl->LocoLCSReady;
    LocoAOSOff          = LocoCtrl->LocoAOSOff;
    LocoEmerStopActive  = LocoCtrl->LocoEmerStopActive;
    LocoNCU             = LocoCtrl->LocoNcu;
    LocoEb1a            = LocoCtrl->LocoEb1a;
    LocoEb1b            = LocoCtrl->LocoEb1b;
    LocoEb2a            = LocoCtrl->LocoEb2a;
    LocoEb2b            = LocoCtrl->LocoEb2b;
    LocoIsolA            = LocoCtrl->LocoIsolA;
    LocoIsolB           = LocoCtrl->LocoIsolB;
    LocoRoadM           = LocoCtrl->LocoRoadM;
    LocoRailM           = LocoCtrl->LocoRailM;
    SelectedController  = LocoCtrl->SelectedController;
    LocoTcoFb           = LocoCtrl->LocoTcoFb;

    // Set EB Cut-outs at Start up
    // Set Isolation Switch to "Run" mode
    if ((TBSystemRunning != oldSystemStatus) &&
      (TBSystemRunning == SystemStatus))
    {
      LocoCtrl->LocoEb1a = true;
      LocoCtrl->LocoEb2a = true;

      LocoCtrl->LocoIsolA = true;
      LocoCtrl->LocoIsolB = false;
    }
    oldSystemStatus = SystemStatus;
    
    // Handle automatic registration
    HandleAutoReg(ThrottleChanged);

    // Handle auto control
    // Can not take reference of LocoThrottle and LocoDriveDir, so local variables are declared
    Int16 autoControlLocoThrottle = LocoThrottle;
    EnumDriveDir autoControlLocoDriveDir = LocoDriveDir;
    autoControl->run((UInt16)LocoSpeed, AOSSBReq, AOSEBReq, (TBSystemRunning == SystemStatus), autoControlLocoThrottle, autoControlLocoDriveDir, atpMode, permittedSpeed, permittedDriveDir, distanceToTarget, distanceToBCA, targetSpeed);
    // Update LocoSim variables
    LocoThrottle = autoControlLocoThrottle;
    LocoDriveDir = autoControlLocoDriveDir;
    AutoControlInProgress = (autoControl->getState() == AOSPC::AutoControlInactive) ? false : true ;

    // Call LocoEngine
    LocoEng->Tick(SystemStatus, 
        LocoThrottle,
        LocoDriveDir,
        LocoCabin1,
        LocoCabin2,
        LocoLCSReady,
        AOSEBReq || LocoEmerStopActive,
        AOSSBReq,
        effectiveGradient,
        brakeability,
        brakeDelayEB,
        brakeDelaySB);
    
    // Handle LocoEngine output data
    LocoSpeed   = LocoEng->LocoSpeed;
    LocoAcc     = LocoEng->LocoAcc;

    // Call LocoIO each 100ms
    // Time to send new movement data?
    if ((timeGetTime() - lastTimeSendReceiveIOData) >= 100)
    {
        lastTimeSendReceiveIOData = timeGetTime();

        LocoIo->Tick(SystemStatus,
            LocoSpeed,
            LocoAcc,
            LocoDriveDir,
            LocoATOSwitch,
            LocoCabin1,
            LocoCabin2,
            LocoLCSReady,
            LocoAOSOff,
            LocoEmerStopActive,
            ATPToFatalFailureClicked,
            LocoNCU,
            LocoEb1a,
            LocoEb1b,
            LocoEb2a,
            LocoEb2b,
            LocoIsolA,
            LocoIsolB,
            LocoRoadM,
            LocoRailM,
            automaticSimClick,
            manualSimClick,
            bp1valueEntered,
            bp2valueEntered,
            LocoTcoFb
            );
    }

    LocoOutputs         = LocoIo->receivedOutputValuesA;
    LocoOutputNames     = LocoIo->outputNames;

    brakePressureValue  = LocoIo->inputAutoBPvalueInKpa;


    AOSEBReq            = !(LocoIo->AOSEB1 || LocoIo->AOSEB2); //EB is active low
    AOSEB1              = LocoIo->AOSEB1;
    AOSEB2              = LocoIo->AOSEB2;
    AOSEmerBrakeActive  = LocoIo->AOSEmerBrakeActive;
    AOSSBReq            = !(LocoIo->AOSPenaltyBreak); // SB is active low
    AOSTCO              = LocoIo->AOSTCO;
    AOSATPOk            = LocoIo->AOSATPOk;
    AOSBuzzer           = LocoIo->AOSBuzzer;
    AOSLamp             = LocoIo->AOSLamp;
    AOSPowerOff         = LocoIo->AOSPowerOff;

    // Manage any new LCS and RailDriver clients 
    lcsCom->Tick();
    rdCom->Tick();

    // Transmit to LCSClient if connected
    lcsCom->SendToLCSClient(LocoATOSwitch, LocoSpeed, AOSSBReq);

    // Transmit to RailDriver if connected
    rdCom->SendToRDClient(LocoATOSwitch, LocoDriveDir, LocoSpeed);
}

/******************************************************************************
* Function:     SimulationControl
* Description:  
******************************************************************************/
void LocoSimDLL::LocoSimulation::SimulationControl(bool AOSStartRequested,
                          bool          AOSStopRequested,
                          bool          ATPStartRequested,
                          bool          ATOStartRequested,
                          bool          AOSPowerDownRequest)
{
    // Fetch status of the processes
    Process^ process;
    
    // In HIL/VSIM -mode -> Just assume the ATP/ATO is running.
    if (SimulationSil == locoParams->SimMode)
    {

        if (GetProcessByName(locoParams->ATPFile, process))
        {
            ATPStatus = ATxRunning;
        }
        else
        {
            ATPStatus = ATxNoProcess;
        }
    }
    else
    {
        ATPStatus = ATxRunning;
        ATPBStatus = ATxRunning;
        SystemStatus = TBSystemRunning;

        return;
    }
    
    // Initiate startup if all units pending for start
    if (ATxRunning == ATPStatus)
    {
        SystemStatus = TBSystemRunning;
    }

    // Check execution of files
    // ATP1
    ATPReadyToRun = false;
    if ((ATxNoProcess == ATPStatus) ||
        (TBSystemPendingStartUp == SystemStatus) ||
        (TBSystemQuitCmd == SystemStatus))
    {
        if (File::Exists(locoParams->ATPFile))
        {
            ATPReadyToRun = true;
        }
    }

    // ATO
    ATOReadyToRun = false;
    if ((ATxNoProcess == ATOStatus) ||
        (TBSystemPendingStartUp == SystemStatus) ||
        (TBSystemQuitCmd == SystemStatus))
    {
        if (File::Exists(locoParams->ATOFile))
        {
            ATOReadyToRun = true;
        }
    }

    // Start processes when requested
    if ((TBSystemPendingStartUp == SystemStatus) ||
        (TBSystemQuitCmd == SystemStatus))
    {
        if (ATPReadyToRun &&
            (ATPStartRequested ||
                AOSStartRequested ||
                rdCom->rdAOSStartRequested))
        {
            if (internalProcesses)
            {
                reqStartATP = true;
            }
            else
            {
                ProcessStartInfo^ startInfo = gcnew ProcessStartInfo(locoParams->ATPFile, locoParams->ATPArgs);
                startInfo->WorkingDirectory = locoParams->ATPFile->Substring(0, locoParams->ATPFile->LastIndexOf("\\"));
                startInfo->UseShellExecute = false;
                Process::Start(startInfo);
            }
            SystemStatus = TBSystemPendingStartUp;
        }
    }

    // Whole AOS
    AOSReadyToRun = false;
    if ((TBSystemPendingStartUp == SystemStatus &&
        ATxNoProcess == ATPStatus)
        ||
        (TBSystemQuitCmd == SystemStatus))
    {
        if (ATPReadyToRun)
        {
            AOSReadyToRun = true;
        }
    }

    if (AOSStopRequested ||
        AOSPowerDownRequest ||
        rdCom->rdAOSStopRequested)
    {
        SystemStatus = TBSystemQuitCmd;

        // TODO: Only ATP file in ATP-Limited
        if (GetProcessByName(locoParams->ATPFile, process))
        {
            process->Kill();
        }
    }


    // If quitting and all units powered down, set pending new start
    if (TBSystemQuitCmd == SystemStatus &&
        ATxNoProcess == ATPStatus)
    {
        SystemStatus = TBSystemPendingStartUp;
    }

    // Pending new start if any unit leaves running state
    if (TBSystemRunning == SystemStatus &&
        (ATxRunning != ATPStatus))
    {
        SystemStatus = TBSystemPendingStartUp;
    }
   

#ifdef OLD_STATUS_CODE_KEEP_AS_REFERENCE_UNTIL_STATUS_IS_FULLY_IMPLEMENTED_WITH_HIL_AS_WELL

    // Read current status from units
    ReadSimulatedStatus("System",&systemStatus);
    ReadSimulatedStatus("ATP1",&atp1Status);
    ReadSimulatedStatus("ATP2",&atp2Status);
    ReadSimulatedStatus("ATO",&atoStatus);

    // Update status to full system
    ATP1Status = atp1Status.Status.ATx;
    ATP2Status = atp2Status.Status.ATx;
    ATOStatus = atoStatus.Status.ATx;

    // Check execution of files
    // ATP1
    ATP1ReadyToRun = false;
    if ((ATxNoProcess == ATP1Status) ||
        (TBSystemPendingStartUp == SystemStatus) ||
        (TBSystemQuitCmd == SystemStatus))
    {
        if (File::Exists(locoParams->ATPFile))
        {
            ATP1ReadyToRun = true;
        }
    }
    // ATP2
    ATP2ReadyToRun = false;
    if ((ATxNoProcess == ATP2Status) ||
        (TBSystemPendingStartUp == SystemStatus) ||
        (TBSystemQuitCmd == SystemStatus))
    {
        if (File::Exists(locoParams->ATP2File))
        {
            ATP2ReadyToRun = true;
        }
    }
    // ATO
    ATOReadyToRun = false;
    if ((ATxNoProcess == ATOStatus) ||
        (TBSystemPendingStartUp == SystemStatus) ||
        (TBSystemQuitCmd == SystemStatus))
    {
        if (File::Exists(locoParams->ATOFile))
        {
            ATOReadyToRun = true;
        }
    }

    // Start processes when requested
    if ((TBSystemPendingStartUp == SystemStatus) ||
        (TBSystemQuitCmd == SystemStatus))
    {
        if (ATP1ReadyToRun &&
            (ATPStartRequested || 
             AOSStartRequested ||
             rdCom->rdAOSStartRequested))
        {
            if (internalProcesses)
            {
                reqStartATP1 = true;
            }
            else
            {
                ProcessStartInfo^ startInfo = gcnew ProcessStartInfo(locoParams->ATPFile, locoParams->ATPArgs);
                startInfo->WorkingDirectory = locoParams->ATPFile->Substring(0, locoParams->ATPFile->LastIndexOf("\\"));
                startInfo->UseShellExecute = false;
                Process::Start(startInfo);
            }
            SystemStatus = TBSystemPendingStartUp;
        }
       
        if (ATOReadyToRun &&
            (ATOStartRequested || 
            AOSStartRequested ||
            rdCom->rdAOSStartRequested))
        {
            if (internalProcesses)
            {
                reqStartATO = true;
            }
            else
            {
                ProcessStartInfo^ startInfo = gcnew ProcessStartInfo(locoParams->ATOFile, locoParams->ATOArgs);
                startInfo->WorkingDirectory = locoParams->ATOFile->Substring(0, locoParams->ATOFile->LastIndexOf("\\"));
                startInfo->UseShellExecute = false;
                Process::Start(startInfo);
                SystemStatus = TBSystemPendingStartUp;
            }
        }
    }

    // Whole AOS
    AOSReadyToRun = false;
    if ((TBSystemPendingStartUp == SystemStatus &&
         ATxNoProcess == ATP1Status &&
         ATxNoProcess == ATP2Status &&
         ATxNoProcess == ATOStatus)
         ||
        (TBSystemQuitCmd == SystemStatus))
    {
        if (ATP1ReadyToRun &&
            ATP2ReadyToRun && 
            ATOReadyToRun)
        {
            AOSReadyToRun = true;
        }
    }


    // Fix to handle situation when ATX's terminated without setting proper Status
    //if (AOSStopRequested && 
    //    (TBSystemQuitCmd == SystemStatus))
    //{
    //    SystemStatus = TBSystemPendingStartUp;
    //}
    // Request programs to quit if PowerOffRelay active
    if (AOSStopRequested ||
        AOSPowerDownRequest ||
        rdCom->rdAOSStopRequested)
    {
        SystemStatus = TBSystemQuitCmd;
    }

    // If quitting and all units powered down, set pending new start
    if (TBSystemQuitCmd == SystemStatus &&
        ATxNoProcess == ATP1Status &&
        ATxNoProcess == ATP2Status &&
        ATxNoProcess == ATOStatus)
    {
        SystemStatus = TBSystemPendingStartUp;
    }

    // Initiate startup if all units pending for start
    if (ATxReadyToRun == ATP1Status &&
        ATxReadyToRun == ATP2Status &&
        ATxReadyToRun == ATOStatus)
    {
        SystemStatus = TBSystemStartUp;
    }

    // Step to running when all units have started
    if (TBSystemStartUp == SystemStatus &&
        ATxRunning == ATP1Status &&
        ATxRunning == ATP2Status &&
        ATxRunning == ATOStatus)
    {
        SystemStatus = TBSystemRunning;
    }

    // Request system stop if any unit leaves running state
    if (TBSystemRunning == SystemStatus &&
        (ATxRunning != ATP1Status ||
         ATxRunning != ATP2Status ||
         ATxRunning != ATOStatus))
    {
        SystemStatus = TBSystemStopped;
    }

    // Send SystemStatus to all units
    systemStatus.Status.TBSystem = SystemStatus;

    // Add ReRegistration balise id to ATP1
    if (ReRegUsingSpecBaliseId)
    {
        systemStatus.ExtStatus[0] = 0x00;        // Use specific balise
    }
    else
    {
        systemStatus.ExtStatus[0] = 0x01;        // Find next balise in balise list
    }
    systemStatus.ExtStatus[0] |= ReRegForward ? 0x02 : 0x00; // Forward if selected
    systemStatus.ExtStatus[1] = (locoParams->RegDefaultBaliseId & 0xFF00) >> 8;
    systemStatus.ExtStatus[2] = (locoParams->RegDefaultBaliseId & 0x00FF);
    systemStatus.ExtStatus[3] = (ReRegBaliseId & 0xFF00) >> 8;
    systemStatus.ExtStatus[4] = (ReRegBaliseId & 0x00FF);
    WriteSimulatedStatus("System", &systemStatus);
#endif
}

/******************************************************************************
* Function:     HandleAutoReg
* Description:  
******************************************************************************/
void LocoSimDLL::LocoSimulation::HandleAutoReg(bool ThrottleChanged)
{
    // Not enabled, quit
    if (!locoParams->AutoRegEnabled)
    {
        AutoRegState = AutoRegInactive;
        AOSAutomaticRegistrationInProgress = false;
        return;
    }

    // Trigger automatic registration
    if ((TBSystemRunning != oldSystemStatus) &&
        (TBSystemRunning == SystemStatus))
    {
        AOSAutomaticRegistrationInProgress = true;
        AutoRegState = AutoRegInit;
        AutoRegDelayTime = System::DateTime::Now.AddSeconds(20);
    }
    oldSystemStatus = SystemStatus;

    // AOS still running, if not quit any attempt to run AutoReg
    if ((AOSAutomaticRegistrationInProgress) &&
        (TBSystemRunning != SystemStatus))
    {
        AutoRegState    = AutoRegInactive;
        AOSAutomaticRegistrationInProgress = false;
        LocoThrottle    = 0;
        LocoATOSwitch   = ATOSwitchMan;
        LocoDriveDir    = DDNeutral; 
        return;
    }

    // Throttle changed on GUI, abort AutoReg
    if (AOSAutomaticRegistrationInProgress &&
        ThrottleChanged)
    {
        AutoRegState    = AutoRegInactive;
        AOSAutomaticRegistrationInProgress = false;
    }

    // In operation ?
    if (AOSAutomaticRegistrationInProgress)
    {
        int autoRegThrottle = (int)(((double)locoParams->AutoRegAcceleration / (double)locoParams->MaxAcc) * 100);

        // Always keep Loco at ready state
        LocoThrottle            = 0;
        LocoATOSwitch           = ATOSwitchMan;
        LocoDriveDir            = locoParams->AutoRegDirection; 
        LocoCabin1             = true;
        if ((AutoRegInit         == AutoRegState) ||
            (AutoRegStartUpDelay == AutoRegState))
        {
            LocoLCSReady            = false;
        }
        else
        {
            LocoLCSReady            = true;
        }
        LocoAOSOff              = false;
        LocoEmerStopActive      = false;
        SelectedController      = SelCtrlDriver;
        // Also tweak LocoControl to the AutoReg settings
        LocoCtrl->LocoCabin1    = LocoCabin1;
        LocoCtrl->LocoCabin2    = LocoCabin2;
        LocoCtrl->LocoLCSReady  = LocoLCSReady;
        LocoCtrl->LocoAOSOff    = LocoAOSOff;
        LocoCtrl->LocoEmerStopActive = LocoEmerStopActive;

        // Set actions depending on current state
        switch(AutoRegState)
        {
        case AutoRegInit:
            AutoRegState = AutoRegStartUpDelay;
            break;
        case AutoRegStartUpDelay:

            if(System::DateTime::Now.CompareTo(AutoRegDelayTime) >= 0)
            {
                AutoRegState = AutoRegFirstWaitingBrakeRelease;
            }
            break;
        case AutoRegFirstWaitingBrakeRelease:
            if (!(AOSSBReq || AOSEBReq))
            {
                AutoRegState = AutoRegFirstAcceleration;
            }
            break;
        case AutoRegFirstAcceleration:
            if ((LocoSpeed >= locoParams->AutoRegSpeed) ||
                (AOSSBReq || AOSEBReq))
            {
                AutoRegState = AutoRegFirstCoasting;
            }
            else
            {
                LocoThrottle = autoRegThrottle;
            }
            break;
        case AutoRegFirstCoasting:
            if (AOSSBReq || AOSEBReq)
            {
                AutoRegState = AutoRegSecondWaitingBrakeRelease;
            }
            break;
        case AutoRegSecondWaitingBrakeRelease:
            if (!(AOSSBReq || AOSEBReq))
            {
                AutoRegState = AutoRegSecondAcceleration;
            }
            break;
        case AutoRegSecondAcceleration:
            if ((LocoSpeed >= locoParams->AutoRegSpeed) ||
                (AOSSBReq || AOSEBReq))
            {
                AutoRegState = AutoRegSecondCoasting;
            }
            else
            {
                LocoThrottle = autoRegThrottle;
            }
            break;
        case AutoRegSecondCoasting:
            if ((AOSSBReq || AOSEBReq) &&
                (LocoSpeed == 0))
            {
                // Wait 5s after stopped before switching states
                AutoRegDelayTime = System::DateTime::Now.AddSeconds(5);
                AutoRegState = AutoRegStopDelay;
            }
            break;
        case AutoRegStopDelay:
            if (System::DateTime::Now.CompareTo(AutoRegDelayTime) >= 0)
            {
                // Setup state after AutoReg
                LocoATOSwitch   = locoParams->AutoRegATOMode;
                LocoDriveDir    = DDNeutral; 

                // End AutoReg
                AutoRegState = AutoRegInactive;
                AOSAutomaticRegistrationInProgress = false;
            }
            break;
        case AutoRegInactive:
        default:
                AOSAutomaticRegistrationInProgress = false;
            break;
        }
    }  
}

bool LocoSimDLL::LocoSimulation::getSBReq(void)
{
  return AOSSBReq;
}
bool LocoSimDLL::LocoSimulation::getEBReq(void)
{
  return AOSEBReq;
}

bool LocoSimDLL::LocoSimulation::GetProcessByName(String^ atxName, Process^ &process)
{
    bool retVal = false;

    // Find application name
    int lastSlashIndex = atxName->LastIndexOf('\\');
    String^ appToFind = atxName->Substring(lastSlashIndex + 1);

    // Strip .xxx
    int lastPointIndex = appToFind->LastIndexOf('.');
    String^ processToFind;
    if (lastPointIndex != -1)
    {
        processToFind = appToFind->Remove(lastPointIndex);
    }
    else
    {
        processToFind = appToFind;
    }

    try
    {
      // Find process
      atpByName = Process::GetProcessesByName(processToFind);

      // There might be several ATP:s executing in parallel. Find the proper one started by this AOS-PC.
      for (int i = 0; i < atpByName->Length; i++)
      {
        // The ATP-File can be described either as a relative path (to AOS-PC) or a full path. This will work for both.
        String^ fullPath = Path::GetFullPath(atxName);

        if (String::Equals(atpByName[i]->MainModule->FileName, fullPath))
        {
          process = atpByName[i];
          retVal = true;
        }
      }
    }
    catch (System::NullReferenceException^)
    {
      retVal = false;
    }
    catch (System::ComponentModel::Win32Exception^)
    {
      retVal = false;
    }

    return retVal;
}
