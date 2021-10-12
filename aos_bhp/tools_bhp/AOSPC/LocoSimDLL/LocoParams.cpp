#pragma once
#pragma ident "@(#) Bombardier Transportation %full_filespec:  LocoParams.cpp-7:c++:arn_006#3 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LocoParams.cpp %
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
* 2013-07-10    Blomqvist   Added ATx arguments
* 2013-11-26    Bo H        Added LocoName
* 2014-03-07    Antbäck     Imported to AOS-PC
* 2014-03-10    Antbäck     Added balise id handling, Reg + ReReg
* 2014-03-13    Antbäck     Removed TimsSim properties
* 2014-03-28    Antbäck     Added UseBalisSimFile/BaliseSimFileName
* 2016-10-04    Marlundg    Changes in I/O for BHP
* 2016-10-04    Marlundg    Read simulation-mode parameter
* 2017-03-10    Marlundg    Support for new I/O
* 2018-03-02    Marlundg    TCO Feedback defined
*******************************************************************************/
#include "stdafx.h"

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;
using namespace System::Runtime::InteropServices;


#include "LocoParams.h"

/******************************************************************************
* Function:     ReadFromIniFile
* Description:  
******************************************************************************/
void LocoSimDLL::LocoParams::ReadFromIniFile(void)
{
    char tmpStr[1000];
    
    // Create temporary file name
    char *tmpIniFile = (char *) Marshal::StringToHGlobalAnsi(iniFile).ToPointer();

    SimMode = (EnumSimulationMode)GetPrivateProfileIntA("AOSPC", "SimulationMode", SimulationSil, tmpIniFile);
   
    GetPrivateProfileStringA("AOSPC", "DISPIP", "192.168.2.12", tmpStr, sizeof(tmpStr), tmpIniFile);
    DISPIP = Marshal::PtrToStringAnsi((IntPtr)tmpStr);

    GetPrivateProfileStringA("LocoSim", "LocoName", "Loco 1", tmpStr, sizeof(tmpStr), tmpIniFile);
    LocoName = Marshal::PtrToStringAnsi((IntPtr)tmpStr);

    LocoSimServerPort       = GetPrivateProfileIntA("LocoSim", "LocoSimServerPort", 55190, tmpIniFile);
    LocoSimUseRailDev       = GetPrivateProfileIntA("LocoSim", "LocoSimUseRailDev", 0, tmpIniFile) != 0 ? true : false;
    RailDriverServerPort    = GetPrivateProfileIntA("LocoSim", "RailDriverServerPort", 55201, tmpIniFile);
    VIOHSimPortToConnect    = GetPrivateProfileIntA("LocoSim", "VIOHSimPortToConnect", 30190, tmpIniFile);
    CODSimPortToConnect     = GetPrivateProfileIntA("LocoSim", "CODSimPortToConnect", 30191, tmpIniFile);
    OPCSimPortToConnect     = GetPrivateProfileIntA("LocoSim", "OPCSimPortToConnect", 30192, tmpIniFile);


    MaxSpeed    = (double)GetPrivateProfileIntA("LocoSim", "MaxSpeed", 1111, tmpIniFile);
    MaxAcc      = (double)GetPrivateProfileIntA("LocoSim", "AccParam_MaxAcc", 25, tmpIniFile);
    MaxRet      = (double)GetPrivateProfileIntA("LocoSim", "AccParam_MaxRet", 25, tmpIniFile);
    EBRet       = (double)GetPrivateProfileIntA("LocoSim", "AccParam_EB", 25, tmpIniFile);
    SBRet       = (double)GetPrivateProfileIntA("LocoSim", "AccParam_SB", 25, tmpIniFile);
    
    SensorMaxError = (double)GetPrivateProfileIntA("LocoSim", "SensorMaxError", 90, tmpIniFile);
    SensorMinError = (double)GetPrivateProfileIntA("LocoSim", "SensorMinError", 10, tmpIniFile);

    MovementTelegramFreq = (double)GetPrivateProfileIntA("LocoSim", "MovementTelegramFreq", 1000, tmpIniFile);

    GetPrivateProfileStringA("LocoSim", "ATPFile", ".\\TestATP.exe", tmpStr, sizeof(tmpStr), tmpIniFile);
    ATPFile = Marshal::PtrToStringAnsi((IntPtr)tmpStr);
    
    GetPrivateProfileStringA("LocoSim", "ATOFile", ".\\TestATO.exe", tmpStr, sizeof(tmpStr), tmpIniFile);
    ATOFile = Marshal::PtrToStringAnsi((IntPtr)tmpStr);

    GetPrivateProfileStringA("LocoSim", "ATPArgs", "", tmpStr, sizeof(tmpStr), tmpIniFile);
    ATPArgs = Marshal::PtrToStringAnsi((IntPtr)tmpStr);
    
    GetPrivateProfileStringA("LocoSim", "ATOArgs", "", tmpStr, sizeof(tmpStr), tmpIniFile);
    ATOArgs = Marshal::PtrToStringAnsi((IntPtr)tmpStr);

    UseBaliseSimFile = GetPrivateProfileIntA("LocoSim", "UseBaliseSimFile", 0, tmpIniFile) != 0 ? true : false;
    GetPrivateProfileStringA("LocoSim", "BaliseSimFileName", "", tmpStr, sizeof(tmpStr), tmpIniFile);
    BaliseSimFileName = Marshal::PtrToStringAnsi((IntPtr)tmpStr);

    // Inputs
    mapInputIsolationA          = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputIsolationA", Input1, tmpIniFile);
    mapInputIsolationB          = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputIsolationB", Input2, tmpIniFile);
    mapInputATOManual           = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputATOManual", Input5, tmpIniFile);
    mapInputATOManual           = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputATOManual", Input5, tmpIniFile);
    mapInputATOSupervised       = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputATOSupervised", Input6, tmpIniFile);
    mapInputATOAutomatic        = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputATOAutomatic", Input7, tmpIniFile);
    mapInputCab1                = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputCab1", Input8, tmpIniFile);
    mapInputCab2                = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputCab2", Input9, tmpIniFile);
    mapInputForward             = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputForward", Input10, tmpIniFile);
    mapInputReverse             = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputReverse", Input11, tmpIniFile);
    mapInputLCSReady            = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputLCSReady", Input12, tmpIniFile);
    mapInputEmerStopActive      = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputEmerStopActive", Input13, tmpIniFile);
    mapInputATPOff              = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputATPOff", Input14, tmpIniFile);
    mapInputRoadM               = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputRoadM", Input15, tmpIniFile);
    mapInputRailM               = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputRailM", Input16, tmpIniFile);
    mapInputTcoFeedback         = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputTcoFeedback", Input17, tmpIniFile);
    mapInputNonControlUnit      = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputNonControlUnit", Input18, tmpIniFile);
    mapInputEmerBrakeCutOut1A   = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputEmerBrakeCutOut1A", Input19, tmpIniFile);
    mapInputEmerBrakeCutOut1B   = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputEmerBrakeCutOut1B", Input20, tmpIniFile);
    mapInputEmerBrakeCutOut2A   = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputEmerBrakeCutOut2A", Input21, tmpIniFile);
    mapInputEmerBrakeCutOut2B   = (enum Inputs)GetPrivateProfileIntA("LocoSim", "mapInputEmerBrakeCutOut2B", Input22, tmpIniFile);

    //Analog Inputs
    mapInputBrakePressureSensor1 = (enum AnalogInputs)GetPrivateProfileIntA("LocoSim", "mapInputBrakePressureSensor1", AnalogInput1, tmpIniFile);
    mapInputBrakePressureSensor2 = (enum AnalogInputs)GetPrivateProfileIntA("LocoSim", "mapInputBrakePressureSensor2", AnalogInput2, tmpIniFile);

    minimumBrakePressureRange =  (int)GetPrivateProfileIntA("LocoSim", "minimumBrakePressureRange", 0, tmpIniFile);
    maximumBrakePressureRange = (int)GetPrivateProfileIntA("LocoSim", "maximumBrakePressureRange", 1379, tmpIniFile);;

    noBrakeAppliedPressure = (int)GetPrivateProfileIntA("LocoSim", "noBrakeAppliedPressure", 800, tmpIniFile);;
    serviceBrakeAppliedPercentage = (int)GetPrivateProfileIntA("LocoSim", "serviceBrakeAppliedPercentage", 50, tmpIniFile);;
    emergencyBrakeAppliedPercentage = (int)GetPrivateProfileIntA("LocoSim", "emergencyBrakeAppliedPercentage", 0, tmpIniFile);;

    ebBrakePressureDecreaseRate = (int)GetPrivateProfileIntA("LocoSim", "ebPressureDecreaseRate", 300, tmpIniFile);;
    sbBrakePressureDecreaseRate = (int)GetPrivateProfileIntA("LocoSim", "sbPressureDecreaseRate", 20, tmpIniFile);;
    pressureIncreaseRate     = (int)GetPrivateProfileIntA("LocoSim", "pressureIncreaseRate", 60, tmpIniFile);;
    MaxBrakePressure          = (int)GetPrivateProfileIntA("LocoSim", "MaxBrakePressure", 1379, tmpIniFile);

    // Outputs
    mapOutputEB1                = (enum Outputs)GetPrivateProfileIntA("LocoSim", "mapOutputEB1", Output1, tmpIniFile);
    mapOutputEB2                = (enum Outputs)GetPrivateProfileIntA("LocoSim", "mapOutputEB2", Output2, tmpIniFile);
    mapOutputEmerBrakeActive    = (enum Outputs)GetPrivateProfileIntA("LocoSim", "mapOutputEmerBrakeActive", Output3, tmpIniFile);
    mapOutputPenaltyBreak       = (enum Outputs)GetPrivateProfileIntA("LocoSim", "mapOutputPenaltyBreak", Output4, tmpIniFile);
    mapOutputTCO                = (enum Outputs)GetPrivateProfileIntA("LocoSim", "mapOutputTCO", Output5, tmpIniFile);
    mapOutputATPOk              = (enum Outputs)GetPrivateProfileIntA("LocoSim", "mapOutputATPOk", Output7, tmpIniFile);
    mapOutputBuzzer             = (enum Outputs)GetPrivateProfileIntA("LocoSim", "mapOutputBuzzer", Output8, tmpIniFile);
    mapOutputLamp               = (enum Outputs)GetPrivateProfileIntA("LocoSim", "mapOutLamp", Output9, tmpIniFile);
    mapOutputPowerOff           = (enum Outputs)GetPrivateProfileIntA("LocoSim", "mapOutputPowerOff", Output10, tmpIniFile);

    // AutoRun
    AutoRegEnabled                  = GetPrivateProfileIntA("LocoSim", "AutoRegEnabled", 0, tmpIniFile) == 0 ? false : true;
    AutoRegAcceleration             = GetPrivateProfileIntA("LocoSim", "AutoRegAcceleration", 8, tmpIniFile);
    AutoRegSpeed                    = GetPrivateProfileIntA("LocoSim", "AutoRegSpeed", 85, tmpIniFile);
    AutoRegDirection                = (EnumDriveDir)GetPrivateProfileIntA("LocoSim", "AutoRegDirection", (int)DDForward, tmpIniFile);    
    AutoRegATOMode                  = (EnumATOMODE)GetPrivateProfileIntA("LocoSim", "AutoRegATOMode", (int)ATOSwitchSupv, tmpIniFile); 

    // AutoControl
    autoControlEnabled              = GetPrivateProfileIntA("LocoSim", "AutoControlEnabled", 0, tmpIniFile) == 0 ? false : true;
    autoControlBCAMarginSecs        = GetPrivateProfileIntA("LocoSim", "AutoControlBCAMarginSecs", 5, tmpIniFile);
    autoControlSpeedLimit1Perc      = GetPrivateProfileIntA("LocoSim", "AutoControlSpeedLimit1Perc", 90, tmpIniFile);
    autoControlSpeedLimit2Perc      = GetPrivateProfileIntA("LocoSim", "AutoControlSpeedLimit2Perc", 95, tmpIniFile);

    // Sound settings
    UseBuzzerSound                  = GetPrivateProfileIntA("LocoSim", "UseBuzzerSound", 1, tmpIniFile) == 0 ? false : true;

    // Balise defaults
    RegDefaultBaliseId              = GetPrivateProfileIntA("LocoSim", "RegistrationDefaultBaliseId", 4513, tmpIniFile);

    // Free temporary buffer again
    Marshal::FreeHGlobal(IntPtr(tmpIniFile));
}

/******************************************************************************
* Function:     SaveToIniFile
* Description:  
******************************************************************************/
void LocoSimDLL::LocoParams::SaveToIniFile(void)
{
    // Only save parameters accessible from GUI, i.e. acc/ret params
    // Create "old time" strings
    char *tmpIniFile    = (char *) Marshal::StringToHGlobalAnsi(iniFile).ToPointer();
    char *tmpMaxSpeed   = (char *) Marshal::StringToHGlobalAnsi(" " + MaxSpeed.ToString()).ToPointer();
    char *tmpMaxAcc     = (char *) Marshal::StringToHGlobalAnsi(" " + MaxAcc.ToString()).ToPointer();
    char *tmpMaxRet     = (char *) Marshal::StringToHGlobalAnsi(" " + MaxRet.ToString()).ToPointer();
    char *tmpEBRet      = (char *) Marshal::StringToHGlobalAnsi(" " + EBRet.ToString()).ToPointer();
    char *tmpSBRet      = (char *) Marshal::StringToHGlobalAnsi(" " + SBRet.ToString()).ToPointer();

    char *tmpAREnable   = (char *) Marshal::StringToHGlobalAnsi(" " + ((int)AutoRegEnabled ? 1 : 0).ToString()).ToPointer();
    char *tmpARAcc      = (char *) Marshal::StringToHGlobalAnsi(" " + AutoRegAcceleration.ToString()).ToPointer();
    char *tmpARSpeed    = (char *) Marshal::StringToHGlobalAnsi(" " + AutoRegSpeed.ToString()).ToPointer();
    char *tmpARDir      = (char *) Marshal::StringToHGlobalAnsi(" " + ((int)AutoRegDirection).ToString()).ToPointer();
    char *tmpARATOMode  = (char *) Marshal::StringToHGlobalAnsi(" " + ((int)AutoRegATOMode).ToString()).ToPointer();

    char *tmpUseBuzz    = (char *) Marshal::StringToHGlobalAnsi(" " + ((int)UseBuzzerSound ? 1 : 0).ToString()).ToPointer();

    char *tmpATPArgs   = (char *) Marshal::StringToHGlobalAnsi(" " + ATPArgs).ToPointer();
    char *tmpATOArgs    = (char *) Marshal::StringToHGlobalAnsi(" " + ATOArgs).ToPointer();

    char *tmpUseBaliseSimFile = (char *) Marshal::StringToHGlobalAnsi(" " + ((int)UseBaliseSimFile ? 1 : 0)).ToPointer();
    char *tmpBaliseSimFileName = (char *) Marshal::StringToHGlobalAnsi(" " + BaliseSimFileName).ToPointer();

    char *tmpRegBaliseId = (char *) Marshal::StringToHGlobalAnsi(" " + RegDefaultBaliseId).ToPointer();

    char *tmpAutoControlBCAMarginSecs = (char *)Marshal::StringToHGlobalAnsi(autoControlBCAMarginSecs.ToString()).ToPointer();
    char *tmpAutoControlSpeedLimit1Perc = (char *)Marshal::StringToHGlobalAnsi(autoControlSpeedLimit1Perc.ToString()).ToPointer();
    char *tmpAutoControlSpeedLimit2Perc = (char *)Marshal::StringToHGlobalAnsi(autoControlSpeedLimit2Perc.ToString()).ToPointer();

    try 
    {

        WritePrivateProfileStringA("LocoSim", "MaxSpeed", tmpMaxSpeed, tmpIniFile);
        WritePrivateProfileStringA("LocoSim", "AccParam_MaxAcc", tmpMaxAcc, tmpIniFile);
        WritePrivateProfileStringA("LocoSim", "AccParam_MaxRet", tmpMaxRet, tmpIniFile);
        WritePrivateProfileStringA("LocoSim", "AccParam_EB", tmpEBRet, tmpIniFile);
        WritePrivateProfileStringA("LocoSim", "AccParam_SB", tmpSBRet, tmpIniFile);

        // AutoRun
        WritePrivateProfileStringA("LocoSim", "AutoRegEnabled", tmpAREnable, tmpIniFile);
        WritePrivateProfileStringA("LocoSim", "AutoRegAcceleration", tmpARAcc, tmpIniFile);
        WritePrivateProfileStringA("LocoSim", "AutoRegSpeed", tmpARSpeed, tmpIniFile);
        WritePrivateProfileStringA("LocoSim", "AutoRegDirection", tmpARDir, tmpIniFile);    
        WritePrivateProfileStringA("LocoSim", "AutoRegATOMode", tmpARATOMode, tmpIniFile); 

        // AutoControl
        WritePrivateProfileStringA("LocoSim", "AutoControlEnabled", autoControlEnabled ? "1":"0" , tmpIniFile);
        WritePrivateProfileStringA("LocoSim", "AutoControlBCAMarginSecs", tmpAutoControlBCAMarginSecs, tmpIniFile);
        WritePrivateProfileStringA("LocoSim", "AutoControlSpeedLimit1Perc", tmpAutoControlSpeedLimit1Perc, tmpIniFile);
        WritePrivateProfileStringA("LocoSim", "AutoControlSpeedLimit2Perc", tmpAutoControlSpeedLimit2Perc, tmpIniFile);

        // Sound settings
        WritePrivateProfileStringA("LocoSim", "UseBuzzerSound", tmpUseBuzz, tmpIniFile); 

        // Arguments for ATP/ATO
        WritePrivateProfileStringA("LocoSim", "ATPArgs", tmpATPArgs, tmpIniFile); 
        WritePrivateProfileStringA("LocoSim", "ATOArgs", tmpATOArgs, tmpIniFile); 

        WritePrivateProfileStringA("LocoSim", "UseBaliseSimFile", tmpUseBaliseSimFile, tmpIniFile); 
        WritePrivateProfileStringA("LocoSim", "BaliseSimFileName", tmpBaliseSimFileName, tmpIniFile); 

        // Balise defaults
        WritePrivateProfileStringA("LocoSim", "RegistrationDefaultBaliseId", tmpRegBaliseId, tmpIniFile);
    }
    catch(...)
    {
        //System::Windows::Forms::MessageBox::Show("Failed to save parameters, file write protected?",
        //    "Save error",
        //    (MessageBoxButtons)0,
        //    MessageBoxIcon::Error);
    }

    // Free temporary buffer again
    Marshal::FreeHGlobal(IntPtr(tmpIniFile));
    Marshal::FreeHGlobal(IntPtr(tmpMaxSpeed));
    Marshal::FreeHGlobal(IntPtr(tmpMaxAcc));
    Marshal::FreeHGlobal(IntPtr(tmpMaxRet));
    Marshal::FreeHGlobal(IntPtr(tmpEBRet));
    Marshal::FreeHGlobal(IntPtr(tmpSBRet));

    Marshal::FreeHGlobal(IntPtr(tmpAREnable));
    Marshal::FreeHGlobal(IntPtr(tmpARAcc));
    Marshal::FreeHGlobal(IntPtr(tmpARSpeed));
    Marshal::FreeHGlobal(IntPtr(tmpARDir));
    Marshal::FreeHGlobal(IntPtr(tmpARATOMode));

    Marshal::FreeHGlobal(IntPtr(tmpUseBuzz));

    Marshal::FreeHGlobal(IntPtr(tmpATPArgs));
    Marshal::FreeHGlobal(IntPtr(tmpATOArgs));

    Marshal::FreeHGlobal(IntPtr(tmpUseBaliseSimFile));
    Marshal::FreeHGlobal(IntPtr(tmpBaliseSimFileName));

    Marshal::FreeHGlobal(IntPtr(tmpRegBaliseId));
}

