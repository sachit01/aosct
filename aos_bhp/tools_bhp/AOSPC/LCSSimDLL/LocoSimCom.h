#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LocoSimCom.h %
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
* 2014-04-03    Antbäck     Added locosimLinkEnabled
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

#include "LCSSimConsts.h"

namespace LCSSimDLL {
public ref class LocoSimCom
{
public:
    bool                locosimLinkEnabled;

    bool                LSConnected;
    DateTime^           LSLastRecMsgTime;
    String^             LSLastSndMsgTime;
    String^             LSLastSndString;


    // Data from LCSSim to LocoSim
    DirectionEnum       sndDirection;       // Forward = A, Reverse = B
    int                 sndAccelerator;

    // Data to LCSSim from LocoSim
    ATOModeEnum         recATOSwitchPos;
    bool                recATOSwitchPosValid;
    int                 recSpeed;           // [mm/s]
    bool                recSpeedValid;
    bool                recBrakesApplied;
    bool                recBrakesAppliedValid;

    // Log data for GUI
    int                  guiFromLSCnt;
    array<String^>^      guiFromLSHeader;
    array<String^>^      guiFromLS;
    int                  guiToLSCnt;
    array<String^>^      guiToLSHeader;
    array<String^>^      guiToLS;


    // Constructor
    // ===========
    LocoSimCom(String^ lsIP, int lsPort)
    {
        LocoSimIP   = lsIP;
        LocoSimPort = lsPort;

        // Create dynamic objects and set default values
        lsSocket                = nullptr;
        oldSec                  = -1;
        oldSendSec              = -1;
        LSLastRecMsgTime        = DateTime();

        guiFromLSCnt       = 0;
        guiFromLSHeader    = gcnew array<String^>(50);
        guiFromLS          = gcnew array<String^>(50);
        guiToLSCnt         = 0;
        guiToLSHeader      = gcnew array<String^>(50);
        guiToLS            = gcnew array<String^>(50);

        // Setup string defaults
        stringSeparator = gcnew array<wchar_t>(1);
        stringSeparator[0] = ';';
        stringCoding = Encoding::UTF8;

        lsClearData();
    }

    void Init(void);
    void InterpreteMessage(String^ buffer);
    void ReadFromLocoSim(void);
    void SendToLocoSim(void);
    void Tick(void);

protected:
    bool            socketConnected;
    String^         LocoSimIP;
    int             LocoSimPort;
    Socket^         lsSocket;
    array<wchar_t>^ stringSeparator;
    Encoding^       stringCoding;

    int     oldSec;
    int     oldSendSec;

    // Destructor
    // ==========
    ~LocoSimCom()
    {
    }
    void lsClearData(void);
};
}
