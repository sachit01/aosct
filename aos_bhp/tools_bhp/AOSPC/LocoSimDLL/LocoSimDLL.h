#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LocoSimDLL.h %
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
* 2013-04-21    Antbäck     File created
* 2013-05-03    Antbäck     Handle driver intervention of AutoReg
* 2014-03-07    Antbäck     Imported to AOS-PC
* 2014-03-10    Antbäck     Added balise id handling, Reg + ReReg
* 2014-03-13    Antbäck     Removed TimsSim properties
* 2013-03-13    Antbäck     Added DLL version
* 2014-03-23    Antbäck     Added logic to support process handling in AOSPC
* 2014-03-25    Antbäck     Handle ATOEnable from AOS
* 2014-04-14    Antbäck     Added LastReadBaliseForward
* 2016-10-04    Marlundg    Changes in I/O for BHP
* 2016-10-16    Marlundg    Rework of the start/stop mechanism without SQL.
* 2017-03-10    Marlundg    Support for new I/O
*
*******************************************************************************/

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;
using namespace System::Reflection;

#include "LocoParams.h"
#include "LocoConsts.h"
#include "LocoControl.h"
#include "LocoEngine.h"
#include "LocoIO.h"
#include "LCSCom.h"
#include "RailDevCom.h"
#include "AutoControl.h"


typedef enum 
{
    AutoRegInit,
    AutoRegStartUpDelay,
    AutoRegFirstWaitingBrakeRelease,
    AutoRegFirstAcceleration,
    AutoRegFirstCoasting,
    AutoRegSecondWaitingBrakeRelease,
    AutoRegSecondAcceleration,
    AutoRegSecondCoasting,
    AutoRegStopDelay,
    AutoRegInactive,
} EnumAutoRegState;


/// Summary for LocoSimulation
namespace LocoSimDLL 
{

    public ref class LocoSimulation 
    {
    public:
        // Simulator setup (Windows)
        bool            AOSReadyToRun;
        bool            ATPReadyToRun;
        bool            ATPBReadyToRun;
        bool            ATOReadyToRun;

        // Request external process control actions
        bool            reqStartATP;
        bool            reqStartATP2;
        bool            reqStartATO;

        // Simulation system state
        enum TBSystemStatus  SystemStatus;
        enum ATxStatus       ATPStatus;
        enum ATxStatus       ATPBStatus;
        enum ATxStatus       ATOStatus;
        enum TBSystemStatus  oldSystemStatus;

        // AOS setup
        bool                ATOEnable;

        // AutoReg variables
        bool                 AOSAutomaticRegistrationInProgress;
        EnumAutoRegState     AutoRegState;
        System::DateTime^    AutoRegDelayTime;

        // AutoControl variables
        bool                 AutoControlInProgress;



        // Selected controls
        EnumSelectedController  SelectedController;
        EnumATOMODE             LocoATOSwitch;
        EnumDriveDir            LocoDriveDir; 
        bool                    LocoCabin1;
        bool                    LocoCabin2;
        bool                    LocoLCSReady;
        bool                    LocoEmerStopActive;
        bool                    LocoAOSOff;
        int                     LocoThrottle;
        bool                    LocoNCU;
        bool                    LocoEb1a;
        bool                    LocoEb1b;
        bool                    LocoEb2a;
        bool                    LocoEb2b;
        bool                    LocoIsolA;
        bool                    LocoIsolB;
        bool                    LocoRoadM;
        bool                    LocoRailM;
        bool                    LocoTcoFb;

        // LocoEnging selected output data
        double                  LocoSpeed;  // cm/s
        double                  LocoAcc;    // cm/s2

        // Current status on outputs for display
        array<bool>^            LocoOutputs;
        array<String^>^         LocoOutputNames;

        int                     brakePressureValue;

        // Current AOS requested status for use by simulation unit
        bool AOSEBReq;
        bool AOSEB1;
        bool AOSEB2;
        bool AOSEmerBrakeActive;
        bool AOSSBReq;
        bool AOSTCO;
        bool AOSATPOk;
        bool AOSBuzzer;
        bool AOSLamp;
        bool AOSPowerOff;

        // Balise simulation
        int                     LastReadBaliseId;       // Data from AOS
        bool                    LastReadBaliseForward;  // - " -
        int                     ReRegBaliseId;          // Indata from GUI
        bool                    ReRegForward;           // - " -
        bool                    ReRegUsingSpecBaliseId; // - " -
        int                     RegUseBaliseId;         // - " - 

        // Internal classes made available for GUI, read only please ...
        LCSCom^                 lcsCom;
        RailDevCom^             rdCom;
        LocoParams^             locoParams;
        LocoIO^                 LocoIo;

        // DLLVersion
        String^     DLLVersion;

        // Processes
        array<Process^>^    atpByName;

        LocoControl^            LocoCtrl;

        AOSPC::AutoControl^     autoControl;

        // Constructor
        // ===========
        LocoSimulation(String^ IniFile, bool intProcesses, OBRDSimDLL::OBRDSimulation^ obrdSim)
        {
            internalProcesses = intProcesses;
            SystemStatus = TBSystemPendingStartUp;
            ATPStatus = ATxNoProcess;
            ATPBStatus = ATxNoProcess;
            ATOStatus = ATxNoProcess;
            ATOEnable = true;

            // Create dynamic objects
            locoParams  = gcnew LocoParams(IniFile);
            LocoCtrl    = gcnew LocoControl;
            LocoEng     = gcnew LocoEngine(locoParams);
            LocoIo      = gcnew LocoIO(locoParams, obrdSim);
            lcsCom      = gcnew LCSCom(locoParams);
            rdCom       = gcnew RailDevCom(locoParams);
            autoControl = gcnew AOSPC::AutoControl();

            // Output data
            SelectedController = SelCtrlDriver;
            LocoATOSwitch = ATOSwitchMan;
            LocoDriveDir = DDNeutral; 
            LocoCabin1 = false;
            LocoCabin2 = false;
            LocoLCSReady = false;
            LocoEmerStopActive = false;
            LocoAOSOff = false;
            LocoSpeed = 0;
            LocoAcc = 0;
            LocoOutputs = gcnew array<bool>(RECEIVED_OUTPUT_SIZE);
            LocoOutputNames = gcnew array<String^>(RECEIVED_OUTPUT_SIZE);
            brakePressureValue = 0;

            // Get DLL version
            DLLVersion = " - v" +
                Assembly::GetExecutingAssembly()->GetName()->Version->Major.ToString() + "." + 
                Assembly::GetExecutingAssembly()->GetName()->Version->Minor.ToString() + "." + 
                Assembly::GetExecutingAssembly()->GetName()->Version->Build.ToString();

            // Protected
            Initialised = false;
        }

        bool Init(void);
        void Tick(int       LocoSimThrottle,
            EnumATOMODE     LocoSimATOSwitch,
            EnumDriveDir    LocoSimDriveDir,
            bool            LocoSimCabin1Click,           // To be set when clicked in GUI !
            bool            LocoSimCabin2Click,           // To be set when clicked in GUI !
            bool            LocoSimLCSReadyClick,         // To be set when clicked in GUI !
            bool            LocoSimDriverEBClick,         // To be set when clicked in GUI !
            bool            LocoSimAOSOffClick,           // To be set when clicked in GUI !
            bool            AOSStartRequested,
            bool            AOSStopRequested,
            bool            ATPStartRequested,
            bool            ATOStartRequested,
            bool            ATP1ToFatalFailureClicked,
            bool            ThrottleChanged,
            bool            LocoSimNCUClick,                   // To be set when clicked in GUI !
            bool            LocoSimEb1aClick,                  // To be set when clicked in GUI !
            bool            LocoSimEb1bClick,                  // To be set when clicked in GUI !
            bool            LocoSimEb2aClick,                  // To be set when clicked in GUI !
            bool            LocoSimEb2bClick,                  // To be set when clicked in GUI !
            bool            LocoSimIsolAClick,                  // To be set when clicked in GUI !
            bool            LocoSimIsolBClick,                  // To be set when clicked in GUI !
            bool            LocoSimRoadMClick,                 // To be set when clicked in GUI !
            bool            LocoSimRailMClick,                  // To be set when clicked in GUI !
            bool            automaticSimClick,                  // To be set when clicked in GUI !
            bool            manualSimClick,                  // To be set when clicked in GUI !
            int             bp1valueEntered,                    // To be set when clicked in GUI !
            int             bp2valueEntered,                    // To be set when clicked in GUI !
            EnumTcoFb       LocoSimTcoFeedbackChoice,
            UInt16          tcoFbOffset,
            UInt16          permittedSpeed,                     // From DMI Interface
            EnumDriveDir    permittedDriveDir,                  // From DMI Interface
            ATPModeEnum     atpMode,                            // From DMI Interface
            UInt16          distanceToTarget,                   // From DMI Interface
            UInt16          distanceToBCA,                      // From DMI Interface
            UInt16          targetSpeed,                        // From DMI Interface 
            Int16           trackGradient,                      // From DMI Interface 
            Int16           effectiveGradient,                  // From DMI Interface 
            UInt16          brakeability,                       // From DMI Interface 
            UInt16          brakeDelayEB,                       // From DMI Interface 
            UInt16          brakeDelaySB                        // From DMI Interface
        );
        void ShutDown(void);

        /**********************************************************
        * Function:     clearOutputs
        * Description: When starting the ATP connection, the LocoIO
        * outputs receive buffer should  be cleared
        **********************************************************/
        public: void clearOutputBuffer(void)
        {
            if (LocoIo)
                LocoIo->clearOutputBuffer();

            // clear the last ticks update of the ouput values
            AOSEBReq = false;
            AOSEB1 = false;
            AOSEB2 = false;
            AOSEmerBrakeActive = false;
            AOSSBReq = false;
            AOSTCO = false;
            AOSATPOk = false;
            AOSBuzzer = false;
            AOSLamp = false;
            AOSPowerOff = false;
        }
    public:
        bool getSBReq(void);
        bool getEBReq(void);


    protected:
        bool            Initialised;
        bool            internalProcesses;

        LocoEngine^     LocoEng;

        void SimulationControl(bool AOSStartRequested,
            bool          AOSStopRequested,
            bool          ATPStartRequested,
            bool          ATOStartRequested,
            bool          AOSPowerDownRequest);

        void HandleAutoReg(bool ThrottleChanged);

        bool GetProcessByName(String^ atxName, Process^ &process);

        // Destructor
        // ==========
        ~LocoSimulation()
        {
            // Don't save, only on user request !
            //if (Initialised)
            //{
            //    locoParams->SaveToIniFile();
            //}
        }
    };
}
