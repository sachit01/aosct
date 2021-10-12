#pragma once
#include "stdafx.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec:  ConsoleConnection.cpp-3:c++:arn_006#1 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          ConsoleConnection.cpp %
*
*  %version:       3 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-03-20 17:34 %
*
*  DESCRIPTION:    Implementation of Console Connection-class to create and handle communication with 
*                  TCP based console server on ATP.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-04    Marlundg    File created
*
*******************************************************************************/
#include "ConsoleConnection.h"

using namespace System;
using namespace System::Net;
using namespace System::Net::Sockets;

namespace AOSPC {


    /******************************************************************************
    * Function:     Connect
    * Description:
    ******************************************************************************/
    void ConsoleConnection::Connect(IPAddress^ ip, int port)
    {
        if (nullptr == endPointRemote)
        {
            endPointRemote = gcnew IPEndPoint(ip, port);
        }

        // If socket not opened, try to open
        if (nullptr == socket)
        {
            try
            {
                socket = gcnew Socket(AddressFamily::InterNetwork, SocketType::Stream, ProtocolType::Tcp);

                // Allow reuse of addresses on this socket
                socket->SetSocketOption(SocketOptionLevel::Socket, SocketOptionName::ReuseAddress, true);
            }
            catch (SocketException ^e)
            {
                // No connection
                ClearData();
            }

            if (nullptr != socket)
            {
                try
                {
                    socket->Blocking = false;
                    socket->Connect(endPointRemote);
                }
                catch (SocketException ^e)
                {
                    // WSAEWOULDBLOCK is normal if no message is received
                    if (e->ErrorCode != WSAEWOULDBLOCK)
                    {
                        ClearData();
                    }
                }
            }
        }
    }

    /******************************************************************************
    * Function:     SendData
    * Description:
    ******************************************************************************/
    bool ConsoleConnection::SendData(unsigned int size, array<unsigned char>^ data)
    {
        bool retval = true;

        if (nullptr != socket)
        {
            try
            {
                socket->SendTo(data, size, SocketFlags::None, endPointRemote);
            }

            // Socket error, close and reconnect
            catch (SocketException^ e)
            {
                // WSAEWOULDBLOCK is normal if no message is received
                if (e->ErrorCode != WSAEWOULDBLOCK)
                {
                    ClearData();
                    retval = false;
                }
            }
        }

        return retval;
    }


    /******************************************************************************
    * Function:     ReadData
    * Description:
    ******************************************************************************/
    unsigned short ConsoleConnection::ReadData(array<unsigned char>^ receivedBytes)
    {
        int	recCnt = 0;

        if (nullptr != socket)
        {
            // Check if still connected
            if (!((socket->Poll(1000, SelectMode::SelectRead) && (socket->Available == 0)) || !socket->Connected))
            {
                try
                {
                    // Read any buffered messages/bytes from socket and use latest valid message
                    do
                    {
                        recCnt = socket->Receive(receivedBytes, SocketFlags::None);

                    } while (recCnt > 0);
                }

                // Any error, close and try again
                catch (SocketException ^e)
                {
                    // WSAEWOULDBLOCK is normal if no message is received
                    if (e->ErrorCode != WSAEWOULDBLOCK)
                    {
                        ClearData();
                    }
                }
            }
            else
            {
                ClearData();
            }
        }
        return recCnt;
    }

    /******************************************************************************
    * Function:     ClearData
    * Description:
    ******************************************************************************/
    void ConsoleConnection::ClearData(void)
    {
        if (socket)
        {
            socket->Close();
        }

        socket = nullptr;
    }

    /******************************************************************************
    * Function:     isConnected
    * Description:
    ******************************************************************************/
    bool ConsoleConnection::isConnected()
    {
        bool connected = false;

        if (socket)
        {
            connected = socket->Connected;
        }

        return connected;
    }
}