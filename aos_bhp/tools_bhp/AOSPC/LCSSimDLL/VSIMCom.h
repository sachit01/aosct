#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          VSIMCom.h %
*
*  %version:       1 %
*
*  %created_by:    bhermans %
*
*  %date_created:  2016-09-16 16:16 %
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
* 2013-10-26    Antbäck     File created
*
*******************************************************************************/

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;
using namespace System::Net;
using namespace System::Net::Sockets;
using namespace System::Text;
using namespace System::IO;
using namespace System::Globalization;

#include "LCSSimConsts.h"

namespace LCSSimDLL {
public ref class VSIMCom
{
public:
    VSIMComStatusEnum   VSIMConnected;
    DateTime^           VSIMLastRecSpdMsgTime;
    DateTime^           VSIMLastRecCmdMsgTime;
    String^             VSIMLastSndSpdMsgTime;
    String^             VSIMLastSndCmdMsgTime;
    String^             VSIMLastSndSpdString;
    String^             VSIMLastSndCmdString;

    // Data from LCSSim to VSIM
    DirectionEnum       sndDirection;       // Forward = A, Reverse = B
    int                 sndAccelerator;
    ATOModeEnum         sndATOMode;         // Controls when to send data


    // Data to LCSSim from VISM
    ATOModeEnum         recATOSwitchPos;
    bool                recATOSwitchPosValid;
    int                 recSpeed;           // [mm/s]
    bool                recSpeedValid;
    bool                recBrakesApplied;
    bool                recBrakesAppliedValid;

    // Log data for GUI
    int                  guiFromVSIMCnt;
    array<String^>^      guiFromVSIMHeader;
    array<String^>^      guiFromVSIM;
    int                  guiToVSIMCnt;
    array<String^>^      guiToVSIMHeader;
    array<String^>^      guiToVSIM;

    int                  dbgStrCnt;
    array<String^>^      dbgStr;


    // Constructor
    // ===========
    VSIMCom(String^ vsimIP, int vsimSpdPort, int vsimCmdPort)
    {
        VSIMIP      = vsimIP;
        VSIMSpdPort = vsimSpdPort;
        VSIMCmdPort = vsimCmdPort;
        VSIMConnected = VSIMCon_NotConnected;

        // Create dynamic objects and set default values
        vsimSpdSocket           = nullptr;
        vsimCmdSocket           = nullptr;
        socketCmdSetupCompleted = false;

        autoModeValid      = false;
        autoMode           = false;
        fullAutoModeValid  = false;
        fullAutoMode       = false;
        speedValid         = false;
        speed              = 0;
        brakeValid         = false;
        brake              = false;

        oldSec                  = -1;
        oldSendSpdSec              = -1;
        oldSendCmdSec              = -1;
        VSIMLastRecSpdMsgTime      = DateTime();
        VSIMLastRecCmdMsgTime      = DateTime();
        oldSndDirection         = DirectionUndef;
        cmdInitCnt              = 0;

        guiFromVSIMCnt       = 0;
        guiFromVSIMHeader    = gcnew array<String^>(50);
        guiFromVSIM          = gcnew array<String^>(50);
        guiToVSIMCnt         = 0;
        guiToVSIMHeader      = gcnew array<String^>(50);
        guiToVSIM            = gcnew array<String^>(50);
        dbgStrCnt            = 0;
        dbgStr               = gcnew array<String^>(50);
        vsimSetup            = gcnew array<String^>(20);
        vsimSetupTotal       = 0;

        // Setup string defaults
        stringSeparator = gcnew array<wchar_t>(1);
        stringSeparator[0] = ' ';
        stringCoding = Encoding::UTF8;
        cultureInfo = gcnew CultureInfo("en-US");

        vsimClearData();
    }

    void Init(void);
    void Tick(void);

protected:
    bool            socketSpdConnected;
    bool            socketCmdConnected;
    bool            socketCmdSetupCompleted;
    int             socketCmdSetupCnt;
    String^         VSIMIP;
    int             VSIMSpdPort;
    int             VSIMCmdPort;
    Socket^         vsimSpdSocket;
    Socket^         vsimCmdSocket;
    array<wchar_t>^ stringSeparator;
    Encoding^       stringCoding;
    CultureInfo^    cultureInfo;
    array<String^>^ vsimSetup;
    int             vsimSetupTotal;

    // Statuses as received from VSIM
    bool            autoModeValid;
    bool            autoMode;
    bool            fullAutoModeValid;
    bool            fullAutoMode;
    bool            speedValid;
    int             speed;
    bool            brakeValid;
    bool            brake;

    int             oldSec;
    int             oldSendSpdSec;
    int             oldSendCmdSec;
    int             cmdInitCnt;

    DirectionEnum   oldSndDirection;       // Forward = A, Reverse = B
    int             oldSndAccelerator;    



    void InterpreteSpdMessage(String^ buffer);
    void InterpreteCmdMessage(String^ buffer);
    void ReadFromVSIM(void);
    void SendToVSIM(void);
    void vsimClearData(void);
    // Destructor
    // ==========
    ~VSIMCom()
    {
    }
};
}
