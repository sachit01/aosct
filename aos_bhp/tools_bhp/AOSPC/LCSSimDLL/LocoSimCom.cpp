#pragma once
#include "StdAfx.h"
#include "LocoSimCom.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec:  LocoSimCom.cpp-1:c++:arn_006#3 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LocoSimCom.cpp %
*
*  %version:       1 %
*
*  %created_by:    bhermans %
*
*  %date_created:  2016-09-16 16:17 %
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
* 2014-04-03    Antbäck     Handle locosimLinkEnabled
*
*******************************************************************************/

// Defines
#define max(a,b)            (((a) > (b)) ? (a) : (b))
#define min(a,b)            (((a) < (b)) ? (a) : (b))

/******************************************************************************
* Function:     Init
* Description:  Set up listening port for socket connection
******************************************************************************/
void LCSSimDLL::LocoSimCom::Init(void)
{
    lsClearData();

    // Setup GUI data
    guiFromLSCnt = 0;
    guiFromLSHeader[guiFromLSCnt++] = "Last msg time";
    guiFromLSHeader[guiFromLSCnt++] = "Last msg";
    guiFromLSHeader[guiFromLSCnt++] = "ATO mode switch";
    guiFromLSHeader[guiFromLSCnt++] = "Current speed";
    guiFromLSHeader[guiFromLSCnt++] = "Brake status";

    guiToLSCnt = 0;
    guiToLSHeader[guiToLSCnt++] = "Last msg time";
    guiToLSHeader[guiToLSCnt++] = "Last msg";
    guiToLSHeader[guiToLSCnt++] = "Driving direction";
    guiToLSHeader[guiToLSCnt++] = "Accelerator";
}

/******************************************************************************
* Function:     Tick
* Description:  
******************************************************************************/
void LCSSimDLL::LocoSimCom::Tick(void)
{
    // If link not enabled, close and return
    if (!locosimLinkEnabled)
    {
        if (socketConnected)
        {
            try 
            {
                lsSocket->Close();
            }
            catch (...)
            {
            }
            lsClearData();
        }        
        LSConnected = false;
        return;
    }

    // If not connected to LocoSim, try to connect
    if ((!socketConnected) && 
        (oldSec != DateTime::Now.Second))
    {
        try 
        {
            // Create socket
            lsSocket = nullptr;
            lsSocket = gcnew Socket(AddressFamily::InterNetwork,
                SocketType::Stream,
                ProtocolType::Tcp );

            // Setup connection end point
            Net::IPEndPoint^ ep = gcnew IPEndPoint( IPAddress::Parse(LocoSimIP), LocoSimPort );
            lsSocket->Connect(ep);

            // Nonblocking !
            lsSocket->Blocking = false;

            socketConnected = true;

        }
        catch(SocketException ^e)
        {
            // No connection
        }
    }
    oldSec = DateTime::Now.Second;

    ReadFromLocoSim();
    SendToLocoSim();

    // Update outgoing status
    DateTime^ connectedLimit = LSLastRecMsgTime->Add(System::TimeSpan(0, 0, 2));
    DateTime^ currTime = DateTime::Now;
    LSConnected = DateTime::Compare(*currTime, *connectedLimit) < 0? true : false;
}

/******************************************************************************
* Function:     ReadFromLocoSim
* Description:  
******************************************************************************/
void LCSSimDLL::LocoSimCom::ReadFromLocoSim(void)
{
    // Read from LCS socket
    if (socketConnected)
    {
        array<Byte>^    buffer = gcnew array<Byte>(256);
        int             recCnt;

        try
        {
            recCnt = lsSocket->Receive(buffer, SocketFlags::None);
            if (recCnt > 0)
            {
                //int i;

                // Update data for GUI
                LSLastRecMsgTime = DateTime::Now;
                // Interprete message
                InterpreteMessage(stringCoding->GetString(buffer));
            }
        }
        catch(SocketException^ e)
        {
            // Close if socket is disconnected by client
            if (e->ErrorCode != 10035L)//WSAEWOULDBLOCK)
            {
                lsSocket->Close();
                lsClearData();
            }
        }
    }
}

/******************************************************************************
* Function:     InterpreteMessage
* Description:  
******************************************************************************/
void LCSSimDLL::LocoSimCom::InterpreteMessage(String^ buffer)
{
    array<String^>^ list = buffer->Split(stringSeparator);
    bool validMsg = false;
    int items = 0;
    int i = 0;

    // LCSDATA;3;ATOSWITCH;SBREQ;END;
    // ATOSWITCH    - MAN/AUT/SUP/UNDEF
    // CURRSPEED    - Current speed [cm/s]
    // SBREQ        - SB/noSB

    // Check that full message is received
    if ((list[0] == "LCSDATA") &&
        (list->Length > 3))
    {
        try
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
        catch (...)
        {
            validMsg = true;
        }
    }

    int itemStart = 1;
    if (validMsg &&
        (items >= 3))
    {
        guiFromLS[0] = LSLastRecMsgTime->ToString();
        guiFromLS[1] = buffer;

        // [1] Driving direction
        if (list[itemStart + 1] == "MAN")
        {
            recATOSwitchPosValid           = true;
            recATOSwitchPos                = ATOModeManual;
            guiFromLS[2] = "Manual";
        }
        else if (list[itemStart + 1] == "SUP")
        {
            recATOSwitchPosValid           = true;
            recATOSwitchPos                = ATOModeSupervised;
            guiFromLS[2] = "Supervised";
        }
        else if (list[itemStart + 1] == "AUT")
        {
            recATOSwitchPosValid           = true;
            recATOSwitchPos                = ATOModeAutomatic;
            guiFromLS[2] = "Automatic";
        }
        else 
        {
            recATOSwitchPosValid           = false;
            recATOSwitchPos                = ATOModeUndefined;
            guiFromLS[2] = "Undefined";
        }

        // [2] recent speed
        guiFromLS[3]= "-";
        try
        {
            recSpeedValid  = true;
            recSpeed       = Convert::ToInt32(list[itemStart + 2]) * 10;
            guiFromLS[3]= list[itemStart + 2];
        }
        catch (...)
        {
            recSpeedValid  = true;
            recSpeed       = 0;
        }

        // [3] Brakes applied
        guiFromLS[4]= "-";
        try
        {
            recBrakesAppliedValid  = true;
            recBrakesApplied       = (list[itemStart + 3] == "SB") ? true : false;
            guiFromLS[4]= recBrakesApplied ? "Applied" : "";
        }
        catch (...)
        {
            recBrakesAppliedValid  = false;
            recBrakesApplied       = true;
        }
    }
}

/******************************************************************************
* Function:     SendToLocoSim
* Description:  
******************************************************************************/
void LCSSimDLL::LocoSimCom::SendToLocoSim(void)
{
    // Transmit every second to LCSClient, if connected
    if (socketConnected)
    {
        try
        {
            // Build string to send
            String^ tmpStr = "LCSDATA;"; 
            // Length of all parameters
            tmpStr += "2;";

            // [1] Driving direction
            switch (sndDirection)
            {
            case DirectionForward:
                tmpStr += "FORW;";
                guiToLS[2] = "FORW";
                break;
            case DirectionReverse:
                tmpStr += "REV;";
                guiToLS[2] = "REV";
                break;
            case DirectionNeutral:
            case DirectionUndef:
            default:
                tmpStr += "NEUT;";
                guiToLS[2] = "NEUT";
                break;
            }

            // [2] Accelerator request
            tmpStr += sndAccelerator.ToString() + ";";
            guiToLS[3] = sndAccelerator.ToString() + ";";

            // End of send string
            tmpStr += "END;";

            if ((LSLastSndString != tmpStr) ||
                (oldSendSec != DateTime::Now.Second))
            {
                oldSendSec = DateTime::Now.Second;

                // Send to LCSClient
                lsSocket->Send(stringCoding->GetBytes(tmpStr), stringCoding->GetByteCount(tmpStr), SocketFlags::None);

                // Update transmit string for GUI
                guiToLS[0] = DateTime::Now.ToString();
                guiToLS[1] = LSLastSndString = tmpStr;
            }
        }
        // Socket error, close and let LCSClient reconnect
        catch(...)
        {
            lsSocket->Close();
            lsClearData();
        }
    }
}



/******************************************************************************
* Function:     atoClearData
* Description:  
******************************************************************************/
void LCSSimDLL::LocoSimCom::lsClearData(void)
{
    socketConnected    = false;
    LSLastSndString    = "";
    oldSendSec         = -1;
}