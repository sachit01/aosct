#pragma once
#pragma ident "@(#) Bombardier Transportation %full_filespec:  RailDevCom.h-1:incl:arn_006#3 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          RailDevCom.h %
*
*  %version:       1 %
*
*  %created_by:    bhermans %
*
*  %date_created:  2016-09-16 16:18 %
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
/// Summary for RailDevCom
public ref class RailDevCom
{
public:
    bool                RDConnected;
    String^             RDConnetedTime;
    String^             RDLastRecMsgTime;
    String^             RDLastRecString;
    String^             RDLastSndMsgTime;
    String^             RDLastSndString;
    unsigned long int   RDLinesReceived;

    bool                rdAOSStartRequested;
    bool                rdAOSStopRequested;
    bool                rdEBReq;
    bool                rdAOSPowerDownReq;

    // Driving commands from RailDevCom
    bool                RDDriverPanelActive;
    EnumATOMODE         RDATOMode;
    EnumDriveDir        RDDriveDir;
    int                 RDThrottle;

    // Constructor
    // ===========
    RailDevCom(LocoParams^ lP)
    {
        // Keep pointer to parameters
        locoParams = lP;

        // Create dynamic objects and set default values

        // Setup string defaults
        stringSeparator = gcnew array<wchar_t>(1);
        stringSeparator[0] = ';';
        stringCoding = Encoding::UTF8;

        serverSocket = nullptr;
        rdSocket = nullptr;
        oldSec = -1;
        oldSendSec = -1;

        rdClearData();
    }

    void Init(void);
    void InterpreteBuffer(String^ buffer);
    void ReadFromRDClient(void);
    void SendToRDClient(EnumATOMODE lsATOSwitch, 
                        EnumDriveDir lsDriveDir,
                        double locoSpeed);
    void Tick(void);

protected:
    Socket^                 serverSocket;
    Socket^                 rdSocket;
    array<wchar_t>^         stringSeparator;
    Encoding^               stringCoding;
    LocoParams^ locoParams;

    int             oldSec;
    int             oldSendSec;

    // Destructor
    // ==========
    ~RailDevCom()
    {
        if (RDConnected)
        {
            rdSocket->Close();
        }
    }
    void rdClearData(void);
};
}