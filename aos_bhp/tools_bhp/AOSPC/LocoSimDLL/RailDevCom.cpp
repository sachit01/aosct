#pragma once
#include "stdafx.h"
#using <System.dll>
#pragma ident "@(#) Bombardier Transportation %full_filespec:  RailDevCom.cpp-1:c++:arn_006#3 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          RailDevCom.cpp %
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
* 2013-04-22    Antbäck     File created
* 2013-10-31    Antbäck     Cleanup unused variables
* 2014-03-07    Antbäck     Imported to AOS-PC
*
*******************************************************************************/

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;

#include "RailDevCom.h"


/******************************************************************************
* Function:     Init
* Description:  Set up listening port for socket connection
******************************************************************************/
void LocoSimDLL::RailDevCom::Init(void)
{
    // Check if enabled
    if (!locoParams->LocoSimUseRailDev)
    {
        return;
    }

    // Setup localhost address
    array<Byte>^ tmp = gcnew array<Byte>(4);
    tmp[0] = 127;
    tmp[1] = 0;
    tmp[2] = 0;
    tmp[3] = 1;

    //initSocketEnvironment();
    try
    {
        serverSocket = gcnew Socket(AddressFamily::InterNetwork,
            SocketType::Stream,
            ProtocolType::Tcp );

        // Bind the listening socket to the port
        Net::IPAddress^ hostIP = gcnew Net::IPAddress(tmp);
        Net::IPEndPoint^ ep = gcnew IPEndPoint( hostIP, locoParams->RailDriverServerPort);
        serverSocket->Bind( ep );

        // start listening
        //s->SetSocketOption(SocketOptionLevel::Socket, SocketOptionName::BlockSource
        serverSocket->Blocking = false;
        serverSocket->Listen(1);
    }
    catch(...)
    {
    }
}

/******************************************************************************
* Function:     InterpreteBuffer
* Description:  
******************************************************************************/
void LocoSimDLL::RailDevCom::InterpreteBuffer(String^ buffer)
{
    //"RD;4;ON;M;F;10;END";
    array<String^>^ list = RDLastRecString->Split(stringSeparator);
    bool validMsg = false;
    int items = 0;
    int i = 0;

    if ((list[0] == "RD") &&
        (list->Length > 3))
    {
        items = System::Convert::ToInt32(list[1]);

        // Check that full message is received
        if (list->Length >= (items + 3))
        {
            if (list[items + 2] == "END")
            {
                validMsg = true;
            }
        }
    }
    if (validMsg)
    {
        // PanelActive [1]
        RDDriverPanelActive = list[2] == "ON";

        // ATOMode [2]
        if (list[3] == "M")
        {
            RDATOMode = ATOSwitchMan;
        }
        else if (list[3] == "S")
        {
            RDATOMode = ATOSwitchSupv;
        }
        else if (list[3] == "A")
        {
            RDATOMode = ATOSwitchAuto;
        }

        // DriveDir [3]
        if (list[4] == "N")
        {
            RDDriveDir = DDNeutral;
        }
        else if (list[4] == "F")
        {
            RDDriveDir = DDForward;
        }
        else if (list[4] == "R")
        {
            RDDriveDir = DDReverse;
        }

        // DriveDir [4]
        int tmpInt = Convert::ToInt32(list[5]);
        if ((tmpInt >= -100) &&
            (tmpInt <= 100))
        {
            RDThrottle = tmpInt;
        }
        
        // Commands [5]
        if (list[6]->IndexOf("START") >= 0)
        {
            rdAOSStartRequested = true;
        }
        if (list[6]->IndexOf("STOP") >= 0)
        {
            rdAOSStopRequested = true;
        }
        if (list[6]->IndexOf("EBREQ") >= 0)
        {
            rdEBReq = true;
        }
        if (list[6]->IndexOf("POWERDOWN") >= 0)
        {
            rdAOSPowerDownReq = true;
        }
    }
}
/******************************************************************************
* Function:     ReadFromRDClient
* Description:  
******************************************************************************/
void LocoSimDLL::RailDevCom::ReadFromRDClient(void)
{
    // Check if enabled
    if (!locoParams->LocoSimUseRailDev)
    {
        return;
    }

    // Clear any "single sample" flags from last run
    rdAOSStartRequested = false;
    rdAOSStopRequested  = false;
    rdEBReq             = false;
    rdAOSPowerDownReq   = false;

    // Read from LCS socket
    if (RDConnected)
    {
        array<Byte>^    buffer = gcnew array<Byte>(256);
        int             recCnt;

        try
        {
            recCnt = rdSocket->Receive(buffer, SocketFlags::None);
            if (recCnt > 0)
            {
                // Update data for GUI
                RDLinesReceived++;
                RDLastRecMsgTime = DateTime::Now.ToString();
                RDLastRecString = stringCoding->GetString(buffer);

                // Interprete message
                InterpreteBuffer(RDLastRecString);
            }
        }
        catch(SocketException^ e)
        {
            // Close if socket is disconnected by client
            if (e->ErrorCode != WSAEWOULDBLOCK)
            {
                rdSocket->Close();
                rdClearData();
            }
        }
    }
}

/******************************************************************************
* Function:     SendToRDClient
* Description:  
* Note:         Only used to send a dummy message each second to capture dead sockets
******************************************************************************/
void LocoSimDLL::RailDevCom::SendToRDClient(EnumATOMODE lsATOSwitch, 
                                EnumDriveDir lsDriveDir,
                                double locoSpeed)
{
    // Check if enabled
    if (!locoParams->LocoSimUseRailDev)
    {
        return;
    }

    // Transmit every second to RailDevIF, if connected
    if (RDConnected)
    {
        try
        {
            // Build string to send
            String^ tmpStr = "LS;"; //"LD;END";
            // Length of msg
            tmpStr += "3;";
            // ATOMode
            switch (lsATOSwitch)
            {
            case ATOSwitchMan:    tmpStr += "M;"; break;
            case ATOSwitchSupv:   tmpStr += "S;"; break;
            case ATOSwitchAuto:   tmpStr += "A;"; break;
            case ATOSwitchUndef: 
            default:              tmpStr += "U;"; break;
            }
            // DriveDir
            switch (lsDriveDir)
            {
            case DDNeutral: tmpStr += "N;"; break;
            case DDForward: tmpStr += "F;"; break;
            case DDReverse: tmpStr += "R;"; break;
            case DDUndef: 
            default:        tmpStr += "U;"; break;
            }
            // Speed 
            tmpStr += String::Format("{0:0.0}", (locoSpeed*3.6/100.0));
            tmpStr += ";";
            // End of send string
            tmpStr += "END;";

            if ((RDLastSndString != tmpStr) ||
                (oldSendSec != DateTime::Now.Second))
            {
                oldSendSec = DateTime::Now.Second;

                // Send to LCSClient
                rdSocket->Send(stringCoding->GetBytes(tmpStr), stringCoding->GetByteCount(tmpStr), SocketFlags::None);

                // Update transmit string for GUI
                RDLastSndMsgTime = DateTime::Now.ToString();
                RDLastSndString = tmpStr;
            }
        }
        // Socket error, close and let LCSClient reconnect
        catch(...)
        {
            rdSocket->Close();
            rdClearData();
        }
    }
}

/******************************************************************************
* Function:     Tick
* Description:  
******************************************************************************/
void LocoSimDLL::RailDevCom::Tick(void)
{
    // Check if enabled
    if (!locoParams->LocoSimUseRailDev)
    {
        return;
    }

    // Check for new connections from LCSClient
    try 
    {
        Socket^ tmpSocket;

        tmpSocket = serverSocket->Accept();

        // Some one waiting ...
        if (false == RDConnected)
        {
            rdSocket = tmpSocket;
            rdSocket->Blocking = false;
            RDConnected = true;
            RDConnetedTime = DateTime::Now.ToString();
        }
        else
        {
            // Close socket, already got connection 
            tmpSocket->Close();
        }
    }
    catch(...)//(SocketException ^e)
    {
        // No one waiting
    }
}

/******************************************************************************
* Function:     rdClearData
* Description:  
******************************************************************************/
void LocoSimDLL::RailDevCom::rdClearData(void)
{
    RDConnected        = false;
    RDConnetedTime     = "";
    RDLastRecMsgTime   = "";
    RDLastRecString    = "";
    RDLastSndMsgTime   = "";
    RDLastSndString    = "";
    RDLinesReceived    = 0;
    oldSendSec         = -1;

    RDDriverPanelActive = false;
    RDATOMode           = ATOSwitchUndef;
    RDDriveDir          = DDUndef;
    RDThrottle          = 0;

    rdAOSStartRequested = false;
    rdAOSStopRequested  = false;
    rdEBReq             = false;
    rdAOSPowerDownReq   = false;


}