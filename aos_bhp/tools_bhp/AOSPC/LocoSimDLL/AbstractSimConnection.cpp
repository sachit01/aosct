#pragma once
#include "stdafx.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec:  AbstractSimConnection.cpp-4:c++:arn_006#1 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          AbstractSimConnection.cpp %
*
*  %version:       4 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-03-21 12:27 %
*
*  DESCRIPTION: Implementation of Abstract base-class to create and handle communication with 
*               UDP-servers according to AOS Simulator-Interface Interflo 150.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-04    Marlundg    File created
* 2017-03-10    Marlundg    Support for Sim-connections other than localhost 
*
*******************************************************************************/
#include "AbstractSimConnection.h"

static const unsigned char STX = 0x02;
static const unsigned char ETX = 0x01;

static const unsigned char PROTOCOL_VERSION_AOSPC_AOS = 0x01;

static const unsigned char LINK_LAYER_STX_OFFSET      = 0;
static const unsigned char LINK_LAYER_PROTOCOL_OFFSET = 1;
static const unsigned char LINK_LAYER_LENGTH_OFFSET   = 2;
static const unsigned char LINK_LAYER_DATA_OFFSET     = 4;


/******************************************************************************
* Function:     Connect
* Description:
******************************************************************************/
void LocoSimDLL::AbstractSimConnection::Connect(IPAddress^ ip, int port)
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
            socket = gcnew Socket(AddressFamily::InterNetwork, SocketType::Dgram, ProtocolType::Udp);

            // Allow reuse of addresses on this socket
            socket->SetSocketOption(SocketOptionLevel::Socket, SocketOptionName::ReuseAddress, true);
        }
        catch (SocketException ^)
        {
            // No connection
            ClearData();
        }

        if (nullptr != socket)
        {
            try
            {
                socket->Connect(endPointRemote);
                socket->Blocking = false;
            }
            catch (SocketException ^e)
            {
                // WSAEWOULDBLOCK is normal if no message is received
                if (e->ErrorCode != WSAEWOULDBLOCK)
                {
                    socket->Close();
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
bool LocoSimDLL::AbstractSimConnection::SendData(unsigned int size, array<Byte>^ data)
{
    bool retval = true;
    array<Byte>^    sndMsgBytes;

    sndMsgBytes = gcnew array<Byte>(256);

    if (nullptr != socket)
    {
        try
        {
            // Message start, Link layer
            sndMsgBytes[LINK_LAYER_STX_OFFSET]        = STX;
            sndMsgBytes[LINK_LAYER_PROTOCOL_OFFSET]   = PROTOCOL_VERSION_AOSPC_AOS;
            sndMsgBytes[LINK_LAYER_LENGTH_OFFSET]     = (htons(size) & 0xff);
            sndMsgBytes[LINK_LAYER_LENGTH_OFFSET + 1] = (htons(size) & 0xff00) >> 8;

            // Build array to send
            int cnt = LINK_LAYER_DATA_OFFSET;

            // Application Message
            for (unsigned int i = 0U; i < size; i++)
            {
                sndMsgBytes[cnt++] = data[i];
            }

            // Message end, Link layer
            sndMsgBytes[cnt++] = ETX;

            socket->SendTo(sndMsgBytes, cnt, SocketFlags::None, endPointRemote);
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
bool LocoSimDLL::AbstractSimConnection::ReadData()
{
    if (nullptr != socket)
    {
        array<Byte>^    buffer = gcnew array<Byte>(256);
        int             recCnt;

        try
        {
            // Read any buffered messages/bytes from socket and use latest valid message
            do
            {
                recCnt = socket->Receive(buffer, SocketFlags::None);

                if (recCnt > 0)
                {
                    // Check STX and Protocol version
                    if (STX == buffer[LINK_LAYER_STX_OFFSET] && 
                        (PROTOCOL_VERSION_AOSPC_AOS == buffer[LINK_LAYER_PROTOCOL_OFFSET]))
                    {
                        WORD msgLen = (buffer[LINK_LAYER_LENGTH_OFFSET] << 8) + buffer[LINK_LAYER_LENGTH_OFFSET + 1];

                        // Check ETX
                        if (ETX == buffer[LINK_LAYER_DATA_OFFSET + (msgLen)])
                        {
                            array<Byte>^ appDataBuffer = gcnew array<Byte>(256);

                            // Copy application data to be used for interpretation
                            System::Array::ConstrainedCopy(buffer, LINK_LAYER_DATA_OFFSET, appDataBuffer, 0, msgLen);

                            // Interpret buffer according to provided method
                            if (InterpretApplicationData(appDataBuffer, msgLen))
                            {
                                return true;
                            }
                        }
                    }
                }
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
    return false;
}

/******************************************************************************
* Function:     ClearData
* Description:
******************************************************************************/
void LocoSimDLL::AbstractSimConnection::ClearData(void)
{
    if (socket)
    {
        socket->Close();
    }

    socket = nullptr;
}
