#pragma once
#include "stdafx.h"
#using <System.dll>
#pragma ident "@(#) Bombardier Transportation %full_filespec:  LCSCom.cpp-1:c++:arn_006#3 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LCSCom.cpp %
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
* 2013-04-22    Antbäck     File created
* 2013-11-19    Antbäck     Reworked for new protocol to LCSSim/AOSPC
* 2014-03-07    Antbäck     Imported to AOS-PC
* 2014-04-03    Antbäck     Added handling to clear input from LCS if not msg received
* 2015-02-26    Antbäck     Do not try to close a socket that is not open ...
*
*******************************************************************************/


using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;

#include "LCSCom.h"


/******************************************************************************
* Function:     Init
* Description:  Set up listening port for socket connection
******************************************************************************/
void LocoSimDLL::LCSCom::Init(void)
{
    // Setup localhost address
    array<Byte>^ tmp = gcnew array<Byte>(4);
    tmp[0] = 127;
    tmp[1] = 0;
    tmp[2] = 0;
    tmp[3] = 1;

    try
    {
        serverSocket = gcnew Socket(AddressFamily::InterNetwork,
            SocketType::Stream,
            ProtocolType::Tcp );

        // Bind the listening socket to the port
        Net::IPAddress^ hostIP = gcnew Net::IPAddress(tmp);
        Net::IPEndPoint^ ep = gcnew IPEndPoint( hostIP, locoParams->LocoSimServerPort);
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
void LocoSimDLL::LCSCom::InterpreteMessage(void)
{
    // "LCSSIM;2;DIRECTION;ACCELERATORVALUE;END"
    // DIRECTION            - FORW/NEUT/REV
    // ACCELERATOR VALUE    - +/- 100 (%)

    array<String^>^ list = LCSLastRecString->Split(stringSeparator);
    bool validMsg = false;
    int items = 0;
    int i = 0;

    // Check that full message is received
    if ((list[0] == "LCSDATA") &&
        (list->Length > 3))
    {
        try
        {
            items = System::Convert::ToInt32(list[1]);

            if (list->Length >= (items + 3))
            {
                if (list[items + 2] == "END")
                {
                    validMsg = true;
                }
            }
        }
        catch (...)
        {
            validMsg = true;
        }
    }

    int itemStart = 1;
    if (validMsg &&
        (items >= 2))
    {
        // [1] Driving direction
        if (list[itemStart + 1] == "FORW")
        {
            LCSSelDrivDirCmd->dataValid = true;
            LCSSelDrivDirCmd->value = DDForward;
        }
        else if (list[itemStart + 1] == "NEUT")
        {
            LCSSelDrivDirCmd->dataValid = true;
            LCSSelDrivDirCmd->value = DDNeutral;
        }
        else if (list[itemStart + 1] == "REV")
        {
            LCSSelDrivDirCmd->dataValid = true;
            LCSSelDrivDirCmd->value = DDReverse;
        }
        else 
        {
            LCSSelDrivDirCmd->dataValid = false;
            LCSSelDrivDirCmd->value = DDNeutral;
        }

        // [2] Accelerator request
        try
        {
            LCSAbsAccCmd->dataValid = true;
            LCSAbsAccCmd->value = min(max(Convert::ToInt32(list[itemStart + 2]), -100), 100);
        }
        catch (...)
        {
            LCSAbsAccCmd->dataValid = false;
            LCSAbsAccCmd->value = 0;
        }
    }
}
/******************************************************************************
* Function:     ReadFromLCSClient
* Description:  
******************************************************************************/
void LocoSimDLL::LCSCom::ReadFromLCSClient(void)
{
    // Read from LCS socket
    if (LCSConnected)
    {
        array<Byte>^    buffer = gcnew array<Byte>(256);
        int             recCnt;

        try
        {
            recCnt = lcsSocket->Receive(buffer, SocketFlags::None);
            if (recCnt > 0)
            {
                // Update data for GUI
                LCSLinesReceived++;
                LCSLastRecMsgTime = DateTime::Now.ToString();
                lastLCSRecTime = DateTime::Now;
                LCSLastRecString = stringCoding->GetString(buffer);
                // Interprete message
                InterpreteMessage();
            }
        }
        catch(SocketException^ e)
        {
            // Close if socket is disconnected by client
            if (e->ErrorCode != WSAEWOULDBLOCK)
            {
                lcsSocket->Close();
                lcsClearData();
            }
        }
    }

    // Check if message has been received lately, discad incoming otherwise
    DateTime^ connectedLimit = lastLCSRecTime->Add(System::TimeSpan(0, 0, 3));
    DateTime^ currTime = DateTime::Now;
    // No message last 3s, close and cancel input from LCS
    if (DateTime::Compare(*currTime, *connectedLimit) > 0)
    {
        if (LCSConnected)
        {
            lcsSocket->Close();
        }
        lcsClearData();
        LCSSelDrivDirCmd->value = DDNeutral;
        LCSAbsAccCmd->value     = 0;
    }
}

/******************************************************************************
* Function:     SendToLCSClient
* Description:  
******************************************************************************/
void LocoSimDLL::LCSCom::SendToLCSClient(EnumATOMODE lsATOSwitch, 
                             double locoSpeed, 
                             bool aosSBReq)
{
    // LCSDATA;3;ATOSWITCH;CURRSPEED;SBREQ;END;
    // ATOSWITCH    - MAN/AUTO/SUPV/UNDEF
    // CURRSPEED    - Current speed [cm/s]
    // SBREQ        - SB/noSB

    // Transmit every second to LCSClient, if connected
    if (LCSConnected)
    {
        int currentSec = DateTime::Now.Second;

        try
        {
            // Time to send ?
            if (oldSec != currentSec)
            {
                array<Byte>^    buffer = gcnew array<Byte>(256);
                int             sndCnt = 0;

                String^ tmpMsg; 
 
                // Header
                tmpMsg = "LCSDATA;";

                // Number of items
                tmpMsg += "3;";

                // ATOSwitch
                switch (lsATOSwitch)
                {
                case ATOSwitchMan:  tmpMsg += "MAN;"; break;
                case ATOSwitchSupv: tmpMsg += "SUP;"; break;
                case ATOSwitchAuto: tmpMsg += "AUT;"; break;
                default:            tmpMsg += ";"; break;
                }

                // Speed
                tmpMsg += ((int)locoSpeed).ToString() + ";";      // [cm/s]

                // SB requested
                tmpMsg += aosSBReq ? "SB;" : "noSB;";

                // Message end
                tmpMsg += "END;";

                // Send to LCSClient
                lcsSocket->Send(stringCoding->GetBytes(tmpMsg), stringCoding->GetByteCount(tmpMsg), SocketFlags::None);

                // Update transmit string for GUI
                LCSLastSndMsgTime = DateTime::Now.ToString();
                LCSLastSndString = tmpMsg;
            }
        }
        // Socket error, close and let LCSClient reconnect
        catch(...)
        {
            lcsSocket->Close();
            lcsClearData();
        }
        oldSec = currentSec;
    }
}

/******************************************************************************
* Function:     Tick
* Description:  
******************************************************************************/
void LocoSimDLL::LCSCom::Tick(void)
{
    // Check for new connections from LCSClient
    try 
    {
        Socket^ tmpSocket;

        tmpSocket = serverSocket->Accept();

        // Some one waiting ...
        if (false == LCSConnected)
        {
            lcsSocket = tmpSocket;
            lcsSocket->Blocking = false;
            LCSConnected = true;
            LCSConnetedTime = DateTime::Now.ToString();
        }
        else
        {
            // Close socket, already got connection 
            tmpSocket->Close();
        }
    }
    catch(...)
    {
        // No one waiting
    }
}

/******************************************************************************
* Function:     lcsClearData
* Description:  
******************************************************************************/
void LocoSimDLL::LCSCom::lcsClearData(void)
{
    LCSConnected        = false;
    LCSConnetedTime     = "";
    LCSLastRecMsgTime   = "";
    LCSLastRecString    = "";
    LCSLastSndMsgTime   = "";
    LCSLastSndString    = "";
    LCSLinesReceived    = 0;

    LCSSelDrivDirCmd->dataValid = false;
    LCSSpeedCmd->dataValid      = false;
    LCSAbsAccCmd->dataValid     = false;
}