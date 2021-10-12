#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LocoParams.h %
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
* 2013-07-10    Blomqvist   Added ATx arguments
* 2013-11-26    Bo H        LocoName
* 2014-03-07    Antbäck     Imported to AOS-PC
* 2014-03-10    Antbäck     Added balise id handling, Reg + ReReg
* 2014-03-13    Antbäck     Removed TimsSim properties
* 2014-03-28    Antbäck     Added UseBalisSimFile/BaliseSimFileName
* 2016-10-04    Marlundg    Changes in I/O for BHP
* 2016-10-04    Marlundg    Read simulation-mode parameter
* 2017-03-10    Marlundg    Support for new I/O
*
*******************************************************************************/

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;

#include "LocoConsts.h"
  
namespace LocoSimDLL {

/// Summary for LocoParams
public ref class LocoParams 
{
public:

    EnumSimulationMode   SimMode;
    String^              DISPIP;

    // Parameters
    // Acc/ret 
    double          MaxAcc;                 // cm/s2
    double          MaxRet;                 // cm/s2
    double          EBRet;                  // cm/s2
    double          SBRet;                  // cm/s2
    double          MaxSpeed;               // cm/s
    int             MaxBrakePressure;       // Kpa

    double          SensorMaxError;         // %
    double          SensorMinError;         // %

    double          MovementTelegramFreq;   // ms between movement messages

    // Executable files
    String^         ATPFile;
    String^         ATOFile;

    // Arguments for ATP/ATO
    bool            UseBaliseSimFile;
    String^         BaliseSimFileName;
    String^         ATOArgs;
    String^         ATPArgs;

    // LocoSim
    String^         LocoName;

    // LocoSim server setup
    int             LocoSimServerPort;

    // RailDriver interface 
    bool            LocoSimUseRailDev;
    int             RailDriverServerPort;

    // VIOHSim port 
    int             VIOHSimPortToConnect;

    // CODSim port 
    int             CODSimPortToConnect;

    // OPCSim port
    int             OPCSimPortToConnect;

    // Mapping of Input signals
    enum Inputs mapInputIsolationA;
    enum Inputs mapInputIsolationB;
    enum Inputs mapInputATOManual;
    enum Inputs mapInputATOSupervised;
    enum Inputs mapInputATOAutomatic;
    enum Inputs mapInputCab1;
    enum Inputs mapInputCab2;
    enum Inputs mapInputForward;
    enum Inputs mapInputReverse;
    enum Inputs mapInputLCSReady;
    enum Inputs mapInputEmerStopActive;
    enum Inputs mapInputATPOff;
    enum Inputs mapInputRoadM;
    enum Inputs mapInputRailM;
    enum Inputs mapInputTcoFeedback;
    enum Inputs mapInputNonControlUnit;
    enum Inputs mapInputEmerBrakeCutOut1A;
    enum Inputs mapInputEmerBrakeCutOut1B;
    enum Inputs mapInputEmerBrakeCutOut2A;
    enum Inputs mapInputEmerBrakeCutOut2B;
    enum AnalogInputs mapInputBrakePressureSensor1;
    enum AnalogInputs mapInputBrakePressureSensor2;
    enum AnalogInputs mapInputUndefined;

    int minimumBrakePressureRange;
    int maximumBrakePressureRange;

    int noBrakeAppliedPressure;
    int serviceBrakeAppliedPercentage;
    int emergencyBrakeAppliedPercentage;

    int ebBrakePressureDecreaseRate;
    int sbBrakePressureDecreaseRate;
    int pressureIncreaseRate;

    // Mapping of Output signals
    enum Outputs mapOutputEB1;
    enum Outputs mapOutputEB2;
    enum Outputs mapOutputEmerBrakeActive;
    enum Outputs mapOutputPenaltyBreak;
    enum Outputs mapOutputTCO;
    enum Outputs mapOutputATPOk;
    enum Outputs mapOutputBuzzer;
    enum Outputs mapOutputLamp;
    enum Outputs mapOutputPowerOff;

    // AutoReg settings
    bool            AutoRegEnabled;
    int             AutoRegAcceleration;
    int             AutoRegSpeed;
    EnumDriveDir    AutoRegDirection;
    EnumATOMODE     AutoRegATOMode;

    // AutoControl settings
    bool          autoControlEnabled;
    UInt16        autoControlBCAMarginSecs;
    UInt16        autoControlSpeedLimit1Perc;
    UInt16        autoControlSpeedLimit2Perc;

    // Sound settings
    bool            UseBuzzerSound;

    // Balise defaults
    int             RegDefaultBaliseId;

    // Constructor
    // ===========
    LocoParams(String^ iniF)
    {
        SimMode = SimulationSil;

        iniFile = String::Copy(iniF);

        // Initialize values
        ATPFile = "";
        ATOFile = "";

        UseBaliseSimFile = false;
        BaliseSimFileName = "";
        ATPArgs = "";
        ATOArgs = "";

        // IO map default

        // Mapping of Input signals
        mapInputIsolationA      = Input1;
        mapInputIsolationB      = Input2;
        mapInputATOManual       = Input5;
        mapInputATOSupervised   = Input6;
        mapInputATOAutomatic    = Input7;
        mapInputCab1            = Input8;
        mapInputCab2            = Input9;
        mapInputForward         = Input10;
        mapInputReverse         = Input11;
        mapInputLCSReady        = Input12;
        mapInputEmerStopActive  = Input13;
        mapInputATPOff          = Input14;
        mapInputRoadM           = Input15;
        mapInputRoadM           = Input16;
        mapInputTcoFeedback     = Input17;
        mapInputNonControlUnit = Input18;
        mapInputEmerBrakeCutOut1A = Input19;
        mapInputEmerBrakeCutOut1B = Input20;
        mapInputEmerBrakeCutOut2A = Input21;
        mapInputEmerBrakeCutOut2B = Input22;

        //Mapping of Analog Inputs
        mapInputBrakePressureSensor1 = AnalogInput1;
        mapInputBrakePressureSensor2 = AnalogInput2;

        minimumBrakePressureRange = 0;
        maximumBrakePressureRange = 1379;

        noBrakeAppliedPressure = 1000;
        serviceBrakeAppliedPercentage = 50;
        emergencyBrakeAppliedPercentage = 0;


        // Mapping of Output signals
        mapOutputEB1              = Output1;
        mapOutputEB2              = Output2;
        mapOutputEmerBrakeActive  = Output3;
        mapOutputPenaltyBreak     = Output4;
        mapOutputTCO              = Output5;
        mapOutputATPOk            = Output7;
        mapOutputBuzzer           = Output8;
        mapOutputLamp             = Output9;
        mapOutputPowerOff         = Output10;

        AutoRegEnabled                  = false;
        AutoRegAcceleration             = 8;
        AutoRegSpeed                    = 85;
        AutoRegDirection                = DDForward;    
        AutoRegATOMode                  = ATOSwitchSupv; 
          
          // Default values
        autoControlEnabled              = false;
        autoControlBCAMarginSecs        = 5;
        autoControlSpeedLimit1Perc      = 90;
        autoControlSpeedLimit2Perc      = 95;

        UseBuzzerSound                  = true;
    }

    void ReadFromIniFile(void);
    void SaveToIniFile(void);

protected:
    String^     iniFile;
    // Destructor
    // ==========
    ~LocoParams()
    {
    }
};
} // end of namespace LocoSimDLL