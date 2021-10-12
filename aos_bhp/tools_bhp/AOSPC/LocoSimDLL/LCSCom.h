#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LCSCom.h %
*
*  %version:       1 %
*
*  %created_by:    bhermans %
*
*  %date_created:  2016-09-16 16:19 %
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
* 2013-11-19    Antbäck     Reworked for new protocol to LCSSim/AOSPC
* 2014-03-07    Antbäck     Imported to AOS-PC
* 2014-04-03    Antbäck     Added handling to clear input from LCS if not msg received
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


#include "LocoConsts.h"
#include "LocoParams.h"
//#include "tcpip.h"

namespace LocoSimDLL {
/// Summary for LCSCom
public ref class LCSCom
{
public:
    bool                LCSConnected;
    String^             LCSConnetedTime;
    DateTime^           lastLCSRecTime;
    String^             LCSLastRecMsgTime;
    String^             LCSLastRecString;
    String^             LCSLastSndMsgTime;
    String^             LCSLastSndString;
    unsigned long int   LCSLinesReceived;

    // Driving commands from LCSCom
    StructLCSDrivDirCmd*    LCSSelDrivDirCmd;
    StructLCSSpeedCmd*      LCSSpeedCmd;
    StructLCSAbsAccCmd*     LCSAbsAccCmd;

    array<wchar_t>^ stringSeparator;
    Encoding^       stringCoding;

    // Constructor
    // ===========
    LCSCom(LocoParams^ lP)
    {
        // Keep pointer to parameters
        locoParams = lP;

        // Create dynamic objects and set default values
        LCSSelDrivDirCmd    = new StructLCSDrivDirCmd;
        LCSSpeedCmd         = new StructLCSSpeedCmd;
        LCSAbsAccCmd        = new StructLCSAbsAccCmd;

        // Setup string defaults
        stringSeparator = gcnew array<wchar_t>(1);
        stringSeparator[0] = ';';
        stringCoding = Encoding::UTF8;

        serverSocket = nullptr;
        lcsSocket = nullptr;
        oldSec = -1;
        lastLCSRecTime = DateTime::Now;

        lcsClearData();
    }

    void Init(void);
    void InterpreteMessage(void);
    void ReadFromLCSClient(void);
    void SendToLCSClient(EnumATOMODE    lsATOSwitch, 
                         double         locoSpeed, 
                         bool           aosSBReq);
    void Tick(void);

protected:
    Socket^                 serverSocket;
    Socket^                 lcsSocket;
    LocoParams^ locoParams;

    int     oldSec;

    // Destructor
    // ==========
    ~LCSCom()
    {
        free(LCSSelDrivDirCmd);
        free(LCSSpeedCmd);
        free(LCSAbsAccCmd);
    }
    void lcsClearData(void);
};
}