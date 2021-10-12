#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          ConsoleConnection.h %
*
*  %version:       2 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-03-10 16:33 %
*
*  DESCRIPTION:    Definition of Console Connection-class to create and handle communication with 
*                  TCP based console server on ATP.
*
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-04    Marlundg	File created
*
*******************************************************************************/
using namespace System;
using namespace System::Net;
using namespace System::Net::Sockets;

namespace AOSPC {
    

    public ref class ConsoleConnection
    {
    public:

        ConsoleConnection() : socket(nullptr), endPointRemote(nullptr) {}
        
        // Connect to TCP server
        void Connect(IPAddress^ ip, int port);

        // Sends data if connection is established.
        bool SendData(unsigned int size, array<unsigned char>^ data);

        // Reads any potential data
        unsigned short ReadData(array<unsigned char>^ receivedBytes);

        // Check if connection is established
        bool isConnected();

    protected:
        // Reset socket
        void ClearData();

        Socket^         socket;
        EndPoint^       endPointRemote;
    };

}