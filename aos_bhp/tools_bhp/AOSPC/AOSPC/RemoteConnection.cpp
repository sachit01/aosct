#pragma once
#include "stdafx.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec: %"

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
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-26    akushwah    File created
* 2019-05-25    marlundg    Fixed connection/disconnection problem
*
*******************************************************************************/

#include "RemoteConnection.h"
using namespace System;
using namespace System::Net;
using namespace System::Net::Sockets;


namespace AOSPC
{
   /******************************************************************************
   * Function:     RemoteConnection
   ******************************************************************************/
   RemoteConnection::RemoteConnection(int listenPort)
   {
      remoteSocket = nullptr;
      remoteConnected = false;

      try
      {
         remoteServerSocket = gcnew Socket(AddressFamily::InterNetwork, SocketType::Stream, ProtocolType::Tcp);

         // Bind the listening socket to the port
         Net::IPAddress^ hostIP = gcnew Net::IPAddress(INADDR_ANY);
         Net::IPEndPoint^ ep = gcnew IPEndPoint(hostIP, listenPort);
         remoteServerSocket->Bind(ep);

         // Start listening
         remoteServerSocket->Blocking = false;
         remoteServerSocket->Listen(maxlengthOfPendingConnection);
      }
      catch (...)
      {
      }
      recBufCnt = 0;
      recvBuffer = gcnew array<Byte>(maxBufferSize);
   }

   /******************************************************************************
   * Function:     Tick
   ******************************************************************************/
   void RemoteConnection::Tick(void)
   {
      // Check for new connections from remote
      try
      {
        Socket^ tmpSocket;

        tmpSocket = remoteServerSocket->Accept();

        // Someone waiting...
        if (false == remoteConnected)
        {
          remoteSocket = tmpSocket;
          remoteSocket->Blocking = false;
          remoteConnected = true;
        }
        else
        {
          // Close socket, already got connection 
          tmpSocket->Close();
        }

      }
      catch (...)
      {
        // No one waiting
      }

      // Read Command from remote Connection
      readCommandFromRemote();
   }

   /******************************************************************************
   * Function:     readFromRemote
   ******************************************************************************/
   void RemoteConnection::readCommandFromRemote(void)
   {
      recBufCnt = 0;

      // Read from remote socket
      if (nullptr != remoteSocket)
      {
        // Check if still connected
        if (!((remoteSocket->Poll(1000, SelectMode::SelectRead) && (remoteSocket->Available == 0)) || !remoteSocket->Connected))
        {
          try
          { 
            recBufCnt = remoteSocket->Receive(recvBuffer, SocketFlags::None);
            
            // Anything received?
            if (recBufCnt > 0)
            {
              String^ tmpRecString(System::Text::UTF8Encoding::UTF8->GetString(recvBuffer));
              array <wchar_t>^ charsToTrim = { L'\r', L'\n', L'\0'};
              
              tmpRecString = tmpRecString->TrimEnd(charsToTrim);
              recString = tmpRecString->ToLower();

              //Reset the buffer 
              for (int index = 0; index < maxBufferSize; index++)
              {
                 recvBuffer[index] = '\0';
              }

            }
          }

          catch (SocketException^ e)
          {
            // Close if socket is disconnected by client
            if (e->ErrorCode != 10035L)//WSAEWOULDBLOCK)
            {
              closeConnection();
            }
          }

          // Any other error, close and try again
          catch (...)
          {
            closeConnection();
          }
        }
        else
        {
          closeConnection();
        }
      }
   }


   /******************************************************************************
   * Function:     closeConnection
   ******************************************************************************/
   void RemoteConnection::closeConnection()
   {
     remoteSocket->Close();
     remoteSocket = nullptr;
     remoteConnected = false;
   }


   /******************************************************************************
   * Function:     sendResponseToRemote
   ******************************************************************************/
   void RemoteConnection::sendResponseToRemote(unsigned int size, array<unsigned char>^ data)
   {
     if (remoteSocket->Connected)
       remoteSocket->Send(data, size, SocketFlags::None);
   }

   /******************************************************************************
   * Function:     terminateMyProcess
   ******************************************************************************/
   void RemoteConnection::terminateAOSPCProcess(DWORD dwProcessId, int uExitCode)
   {
      DWORD dwDesiredAccess = PROCESS_TERMINATE;
      BOOL  bInheritHandle = FALSE;
      HANDLE hProcess = OpenProcess(dwDesiredAccess, bInheritHandle, dwProcessId);
      if (hProcess == NULL)
         return;

      BOOL result = TerminateProcess(hProcess, uExitCode);

      CloseHandle(hProcess);
   }
   /******************************************************************************
   * Function:     commandReceived, returns true if any command from remote 
   *                
   ******************************************************************************/
   bool RemoteConnection::commandReceived(void)
   {
     return (recBufCnt > 0) ? true : false;
   }


}
