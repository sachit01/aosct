#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:    Later.
*
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-26    akushwah   	File created
*
*******************************************************************************/

using namespace System;
using namespace System::Net;
using namespace System::Net::Sockets;
using namespace System::Text;

namespace AOSPC 
{
   public ref class RemoteConnection
   {
   public:
      RemoteConnection(int listenPort);
      void sendResponseToRemote(unsigned int size, array<unsigned char>^ data);
      //kill the process with ID
      void terminateAOSPCProcess(DWORD dwProcessId, int uExitCode);
      String^ recString;
      void Tick(void);
      bool commandReceived(void);

   private:
      // Internal functions     
      void readCommandFromRemote(void);
      void closeConnection(void);

      // Receive buffer for incoming messages 
      array<Byte>^    recvBuffer;
      unsigned int    recBufCnt;
      Socket^         remoteServerSocket;
      Socket^         remoteSocket;
      Encoding^       stringCoding;
      bool            remoteConnected;

      // Maximum Buffer Size 
      static const int maxBufferSize = 256;

      //maximum length of the pending connections queue
      static const int maxlengthOfPendingConnection = 1;

   };
}
