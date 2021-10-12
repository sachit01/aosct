#pragma once
#include "StdAfx.h"
#include "VSIMCom.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec:  VSIMCom.cpp-1:c++:arn_006#2 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          VSIMCom.cpp %
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
* 2013-10-26   Antbäck     File created
*
*******************************************************************************/

/******************************************************************************
* Function:     Init
* Description:  
******************************************************************************/
void LCSSimDLL::VSIMCom::Init(void)
{
    vsimClearData();

    // Setup commands to send to VSIM 
    vsimSetupTotal = 0;
    vsimSetup[vsimSetupTotal++] = "stoplog autoMode";
    vsimSetup[vsimSetupTotal++] = "stoplog fullAutoMode";
    vsimSetup[vsimSetupTotal++] = "stoplog vehicleSpeed";
    vsimSetup[vsimSetupTotal++] = "stoplog brakePressure";
    vsimSetup[vsimSetupTotal++] = "startlog VS2K_LocoHWSim.AUTOMode autoMode";
    vsimSetup[vsimSetupTotal++] = "startlog VS2K_LocoHWSim.FULLAUTOMode fullAutoMode";
    vsimSetup[vsimSetupTotal++] = "startlog VS2K_LocoSim.VehicleSpeed vehicleSpeed";
    vsimSetup[vsimSetupTotal++] = "startlog VS2K_LocoSim.BrakePressure brakePressure";

    // Setup GUI data
    guiFromVSIMCnt = 0;
    guiFromVSIMHeader[guiFromVSIMCnt++] = "(Spd) Last msg time";
    guiFromVSIMHeader[guiFromVSIMCnt++] = "(Spd) Last msg";
    guiFromVSIMHeader[guiFromVSIMCnt++] = "(Cmd) Last msg time";
    guiFromVSIMHeader[guiFromVSIMCnt++] = "(Cmd) Last msg";
    guiFromVSIMHeader[guiFromVSIMCnt++] = "ATO mode switch";
    guiFromVSIMHeader[guiFromVSIMCnt++] = "Current speed";
    guiFromVSIMHeader[guiFromVSIMCnt++] = "Brake status";

    guiToVSIMCnt = 0;
    guiToVSIMHeader[guiToVSIMCnt++] = "(Spd) Last msg time";
    guiToVSIMHeader[guiToVSIMCnt++] = "(Spd) Last msg";
    guiToVSIMHeader[guiToVSIMCnt++] = "(Cmd) Last msg time";
    guiToVSIMHeader[guiToVSIMCnt++] = "(Cmd) Last msg";
    guiToVSIMHeader[guiToVSIMCnt++] = "Driving direction";
    guiToVSIMHeader[guiToVSIMCnt++] = "Accelerator";
}

/******************************************************************************
* Function:     Tick
* Description:  
******************************************************************************/
void LCSSimDLL::VSIMCom::Tick(void)
{

    // Clear debug strings
    dbgStrCnt = 0;

    // If not connected to VSIM speed port, try to connect
    if ((!socketSpdConnected) && 
        (oldSec != DateTime::Now.Second))
    {
        try 
        {
            // Create socket
            vsimSpdSocket = nullptr;
            vsimSpdSocket = gcnew Socket(AddressFamily::InterNetwork,
                SocketType::Stream,
                ProtocolType::Tcp );

            // Setup connection end point
            Net::IPEndPoint^ ep = gcnew IPEndPoint( IPAddress::Parse(VSIMIP), VSIMSpdPort);
            //Net::IPEndPoint^ ep = gcnew IPEndPoint( IPAddress::Parse("127.0.0.1"/*VSIMIP*/), 40207/*VSIMSpdPort*/);
            vsimSpdSocket->Connect(ep);

            // Nonblocking !
            vsimSpdSocket->Blocking = false;

            socketSpdConnected = true;

        }
        catch(SocketException ^e)
        {
            // No connection
        }
    }

    // If not connected to VSIM command port, try to connect
    if ((!socketCmdConnected) && 
        (oldSec != DateTime::Now.Second))
    {
        try 
        {
            // Create socket
            vsimCmdSocket = nullptr;
            vsimCmdSocket = gcnew Socket(AddressFamily::InterNetwork,
                SocketType::Stream,
                ProtocolType::Tcp );

            // Setup connection end point
            Net::IPEndPoint^ ep = gcnew IPEndPoint( IPAddress::Parse(VSIMIP), VSIMCmdPort);
            //Net::IPEndPoint^ ep = gcnew IPEndPoint( IPAddress::Parse("127.0.0.1"/*VSIMIP*/), 40208/*VSIMCmdPort*/);
            vsimCmdSocket->Connect(ep);

            // Nonblocking !
            vsimCmdSocket->Blocking = false;

            socketCmdConnected      = true;
            socketCmdSetupCompleted = false;
            socketCmdSetupCnt       = 0;
            cmdInitCnt              = 0;
        }
        catch(SocketException ^e)
        {
            // No connection
        }
    }

    oldSec = DateTime::Now.Second;

    if (socketCmdSetupCompleted)
    {
        ReadFromVSIM();
        SendToVSIM();
    }
    else if (socketCmdConnected)
    {
        String^ cmdStr = "";

        if (socketCmdSetupCnt < vsimSetupTotal)
        {
            // Delay before sending first command
            cmdInitCnt++;
            if (cmdInitCnt > 10)
            {
                cmdInitCnt = 0;
                cmdStr = vsimSetup[socketCmdSetupCnt++] + "\r\n";

                try
                {
                    if (cmdStr->Length > 0)
                    {
                        // Send command string
                        vsimCmdSocket->Send(stringCoding->GetBytes(cmdStr), stringCoding->GetByteCount(cmdStr), SocketFlags::None);
                        dbgStr[dbgStrCnt++] = "S>Cmd - " + cmdStr;

                        // Update transmit string for GUI
                        guiToVSIM[2] = DateTime::Now.ToString();
                        guiToVSIM[3] = VSIMLastSndCmdString = cmdStr;
                    }
                }
                // Socket error, close and let LCSClient reconnect
                catch(...)
                {
                    vsimCmdSocket->Close();
                    socketCmdConnected = false;
                    //vsimClearData();
                }
            }
        }
        else
        {
            cmdInitCnt = 0;
            socketCmdSetupCompleted = true;
        }
        // Still check for received messages
        ReadFromVSIM();
    }

    // Update outgoing status
    if (socketSpdConnected &&
        socketCmdConnected &&
        socketCmdSetupCompleted)
    {
        VSIMConnected = VSIMCon_Running;
    }
    else if (socketSpdConnected ||
             socketCmdConnected)
    {
        VSIMConnected = VSIMCon_Connected;
    }
    else
    {
        VSIMConnected = VSIMCon_NotConnected;
    }

    //&&
    //                (socketCmdSetupCompleted)
    //                ? true : false;
    //DateTime^ currTime          = DateTime::Now;
    //DateTime^ connectedLimitSpd = VSIMLastRecSpdMsgTime->Add(System::TimeSpan(0, 0, 2));
    //DateTime^ connectedLimitCmd = VSIMLastRecCmdMsgTime->Add(System::TimeSpan(0, 0, 2));
    //VSIMConnected = (DateTime::Compare(*currTime, *connectedLimitSpd) < 0) && 
    //                (DateTime::Compare(*currTime, *connectedLimitCmd) < 0) &&
    //                (socketCmdSetupCompleted)
    //                ? true : false;
}

/******************************************************************************
* Function:     ReadFromVSIM
* Description:  
******************************************************************************/
void LCSSimDLL::VSIMCom::ReadFromVSIM(void)
{
    // Read from VSIM speed socket
    if (socketSpdConnected)
    {
        array<Byte>^    buffer = gcnew array<Byte>(256);
        int             recCnt;

        try
        {
            do 
            {
                recCnt = vsimSpdSocket->Receive(buffer, SocketFlags::None);
                if (recCnt > 0)
                {
                    //int i;

                    // Update data for GUI
                    VSIMLastRecSpdMsgTime = DateTime::Now;
                    // Interprete message
                    InterpreteSpdMessage(stringCoding->GetString(buffer));
                }
            } while (recCnt > 0);
        }
        catch(SocketException^ e)
        {
            // Close if socket is disconnected by client
            if (e->ErrorCode != 10035L)//WSAEWOULDBLOCK)
            {
                vsimSpdSocket->Close();
                socketSpdConnected = false;
                //vsimClearData();
            }
        }
        catch(...)
        {
            vsimSpdSocket->Close();
            socketSpdConnected = false;
        }
    }

    // Read from LCS socket
    if (socketCmdConnected)
    {
        array<Byte>^    buffer = gcnew array<Byte>(256);
        int             recCnt;

        try
        {
            do 
            {
                recCnt = vsimCmdSocket->Receive(buffer, SocketFlags::None);
                if (recCnt > 0)
                {
                    // Update data for GUI
                    VSIMLastRecCmdMsgTime = DateTime::Now;
                    // Interprete message
                    InterpreteCmdMessage(stringCoding->GetString(buffer));
                }
            } while (recCnt > 0);
        }
        catch(SocketException^ e)
        {
            // Close if socket is disconnected by client
            if (e->ErrorCode != 10035L)//WSAEWOULDBLOCK)
            {
                vsimSpdSocket->Close();
                socketCmdConnected = false;
                //vsimClearData();
            }
        }
        catch(...)
        {
            vsimCmdSocket->Close();
            socketCmdConnected = false;
        }
    }
}

/******************************************************************************
* Function:     InterpreteSpdMessage
* Description:  
******************************************************************************/
void LCSSimDLL::VSIMCom::InterpreteSpdMessage(String^ buffer)
{
    // Run some "cleaning" to remove unwanted characters
    buffer = buffer->Replace("\r\n", " ");
    buffer = buffer->Replace("\n\r", " ");
    buffer = buffer->Replace("\r", " ");
    buffer = buffer->Replace("\n", " ");

    // Not interested, save for GUI and discard
    guiFromVSIM[0] = VSIMLastRecSpdMsgTime->ToString();
    guiFromVSIM[1] = buffer;


    dbgStr[dbgStrCnt++] = "R>Spd - " + buffer;

}

/******************************************************************************
* Function:     InterpreteCmdMessage
* Description:  
******************************************************************************/
void LCSSimDLL::VSIMCom::InterpreteCmdMessage(String^ buffer)
{
    int currIdx     = 0;
    int brakePress  = 0;

    // Run some "cleaning" to remove unwanted characters
    buffer = buffer->Replace("\r\n", " ");
    buffer = buffer->Replace("\n\r", " ");
    buffer = buffer->Replace("\r", " ");
    buffer = buffer->Replace("\n", " ");

    // To GUI
    guiFromVSIM[2] = VSIMLastRecCmdMsgTime->ToString();
    guiFromVSIM[3] = buffer;
    dbgStr[dbgStrCnt++] = "R>Cmd - " + buffer;

    // "autoMode xx timestamp fullAutoMode xx timestamp vehicleSpeed xx timestamp"

    // Read and interprete message
    array<String^>^ list = buffer->Split(stringSeparator);


    // Scan all received strings
    while ((currIdx + 2) < list->Length)
    {
        if (list[currIdx]->ToLower() == "automode")
        {
            try
            {
                bool tmp        = Convert::ToInt16(list[currIdx + 1]) != 0 ? true : false;
                autoMode        = tmp;
                autoModeValid   = true;
            }
            catch (...)
            {
            }
        }
        if (list[currIdx]->ToLower() == "fullautomode")
        {
            try
            {
                bool tmp            = Convert::ToInt16(list[currIdx + 1]) != 0 ? true : false;
                fullAutoMode        = tmp;
                fullAutoModeValid   = true;
            }
            catch (...)
            {
            }
        }
        if (list[currIdx]->ToLower() == "vehiclespeed")
        {
            try
            {
                int tmp     = Convert::ToInt32(Convert::ToSingle(list[currIdx + 1], cultureInfo) * 1000, cultureInfo);
                speed       = tmp;
                speedValid  = true;
            }
            catch (...)
            {
            }
        }
        if (list[currIdx]->ToLower() == "brakepressure")
        {
            try
            {
                int tmp     = Convert::ToInt32(Convert::ToSingle(list[currIdx + 1], cultureInfo), cultureInfo);
                brakePress  = tmp;
                brakeValid  = true;
            }
            catch (...)
            {
            }
        }
        currIdx += 3;
    }

    // Update communication status based on result
    if (autoModeValid && fullAutoModeValid)
    {
        // ATO switch
        if (fullAutoMode)
        {
            recATOSwitchPos = ATOModeAutomatic;
            guiFromVSIM[4]  = "ATOAuto";
        }
        else if (autoMode)
        {
            recATOSwitchPos = ATOModeSupervised;
            guiFromVSIM[4]  = "ATOSupv";
        }
        else
        {
            recATOSwitchPos = ATOModeManual;
            guiFromVSIM[4]  = "ATOMan";
        }
        recATOSwitchPosValid = true;
    }

    // Vehicle speed
    if (speedValid)
    {
        recSpeedValid = true;
        recSpeed      = abs(speed);
        try
        {
            guiFromVSIM[5]  = String::Format("{0:0.00}", (float)((speed * 3.6) / 1000));
        }
        catch (...)
        {
            guiFromVSIM[5]  = "Convertion error";
        }
    }


    // Brake status
    if (brakeValid)
    {
        String^ tmpStr;

        recBrakesAppliedValid = true;
        //if (brakePress < 1000)
        //{
        //    recBrakesApplied = true;
        //}
        //else
        //{
            recBrakesApplied = false;
        //}
        try
        {
            tmpStr  = String::Format("{0:0}", brakePress);
        }
        catch (...)
        {
            tmpStr = "error";
        }
        guiFromVSIM[6]  = recBrakesApplied ? "Applied" : "Released";
        guiFromVSIM[6] += " (" + tmpStr + ")";
    }
}

/******************************************************************************
* Function:     SendToVSIM
* Description:  
******************************************************************************/
void LCSSimDLL::VSIMCom::SendToVSIM(void)
{
    // Transmit every second to LCSClient, if connected
    // Speed port
    if (socketSpdConnected)
    {
        if ((ATOModeAutomatic  == sndATOMode) ||
            (ATOModeSupervised == sndATOMode))
        {
            try
            {
                // Build string to send
                String^ tmpStr = ""; 

                // Send direction if changed
                if (oldSndDirection != sndDirection)
                {
                    oldSndDirection = sndDirection;
                    switch (sndDirection)
                    {
                    case DirectionForward:
                        tmpStr = "Forward";
                        break;
                    case DirectionReverse:
                        tmpStr = "Backward";
                        break;
                    case DirectionNeutral:
                    case DirectionUndef:
                    default:
                        tmpStr = "Neutral";
                        break;
                    }
                }
                // else send throttle command
                else 
                {
                    if(sndAccelerator > 0) 
                    {
                        tmpStr = "Throttle " + sndAccelerator.ToString();
                    }
                    else if(sndAccelerator < 0)
                    {
                        tmpStr = "Brake " + (-sndAccelerator).ToString();
                    }
                    else
                    {
                        tmpStr = "Coast";
                    }
                }
                if ((VSIMLastSndSpdString != tmpStr)/* ||
                    (oldSendSpdSec != DateTime::Now.Second)*/)
                {
                    oldSendSpdSec = DateTime::Now.Second;

                    // Send to LCSClient
                    String^ sndStr = tmpStr + "\r\n";
                    vsimSpdSocket->Send(stringCoding->GetBytes(sndStr), stringCoding->GetByteCount(sndStr), SocketFlags::None);
                    dbgStr[dbgStrCnt++] = "S>Spd - " + tmpStr;
                    VSIMLastSndSpdString = tmpStr;

                    // Update transmit string for GUI
                    guiToVSIM[0] = DateTime::Now.ToString();
                    guiToVSIM[1] = VSIMLastSndSpdString = tmpStr;
                }
            }
            // Socket error, close and let LCSClient reconnect
            catch(...)
            {
                vsimSpdSocket->Close();
                vsimClearData();
            }
        }
        else
        {
            guiToVSIM[0] = DateTime::Now.ToString();
            guiToVSIM[1] = "No transmission, manual mode";
        }
    }

    // Command socket
    if (socketCmdConnected)
    {
        try
        {
            // Build string to send
            String^ tmpStr = ""; 

            if ((VSIMLastSndCmdString != tmpStr) &&
                (VSIMLastSndCmdString->Length > 0)) 
                //(oldSendCmdSec != DateTime::Now.Second))
            {
                oldSendCmdSec = DateTime::Now.Second;

                // Send to LCSClient
                vsimCmdSocket->Send(stringCoding->GetBytes(tmpStr), stringCoding->GetByteCount(tmpStr), SocketFlags::None);
                dbgStr[dbgStrCnt++] = "S>Cmd - " + tmpStr;

                // Update transmit string for GUI
                guiToVSIM[2] = DateTime::Now.ToString();
                guiToVSIM[3] = VSIMLastSndCmdString = tmpStr;
            }
        }
        // Socket error, close and let LCSClient reconnect
        catch(...)
        {
            vsimCmdSocket->Close();
            vsimClearData();
        }
    }
}



/******************************************************************************
* Function:     vsimClearData
* Description:  
******************************************************************************/
void LCSSimDLL::VSIMCom::vsimClearData(void)
{
    socketSpdConnected    = false;
    socketCmdConnected    = false;
    socketCmdSetupCompleted = false;
    VSIMLastSndSpdString  = "";
    VSIMLastSndCmdString  = "";
    oldSendSpdSec            = -1;
    oldSendCmdSec            = -1;
}