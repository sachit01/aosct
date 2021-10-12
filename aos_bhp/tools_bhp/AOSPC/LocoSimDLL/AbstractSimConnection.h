#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          AbstractSimConnection.h %
*
*  %version:       3 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-03-10 16:33 %
*
*  DESCRIPTION: Definition of Abstract base-class to create and handle communication with 
*               UDP-servers according to AOS Simulator-Interface Interflo 150.
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
* 2017-03-10    Marlundg    Support for Sim-connections other than localhost
*
*******************************************************************************/
#include "LocoConsts.h"
#include "LocoParams.h"

using namespace System::Net;
using namespace System::Net::Sockets;


namespace LocoSimDLL {

    public ref class AbstractSimConnection abstract
    {
    public:

        AbstractSimConnection() : socket(nullptr), endPointRemote(nullptr) {}
        
        // Connect to UDP server
        void Connect(IPAddress^ ip, int port);

    protected:

        // Encapsulates data in link-layer and sends message if connection is established.
        bool SendData(unsigned int size, array<Byte>^ data);

        // Reads any potential data and calls InterpretApplicationData() for parsing of application data.
        bool ReadData();

        // Reset socket
        void ClearData();

        // Method how to parse application data to be implemented by derived class.
        virtual bool InterpretApplicationData(array<Byte>^ buffer, unsigned int size) = 0;

        Socket^         socket;
        EndPoint^       endPointRemote;
    };

}