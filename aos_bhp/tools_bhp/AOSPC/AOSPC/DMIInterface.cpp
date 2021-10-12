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
* 2018-10-26    bhermans    Created
*
*******************************************************************************/

#include "DMIInterface.h"
using namespace System;
using namespace System::Net;
using namespace System::Net::Sockets;


namespace AOSPC
{
  /******************************************************************************
  * Function:     DMI Interface constructor
  ******************************************************************************/
  DMIInterface::DMIInterface(IPAddress^ ip, int dmiPort, int dmiInternalPort)
  {

    ipToDMI = ip;
    portToDMI = dmiPort;
    internalPortToDMI = dmiInternalPort;

    connect(dmiSocket, ipToDMI, portToDMI);
    connect(dmiInternalSocket, ipToDMI, internalPortToDMI);

    ticks = 0;
    recvBuffer = gcnew array<Byte>(maxBufferSize);

    recInternalBufCnt = 0;
    recBufCnt = 0;

    releaseEBReq = false;
    releaseSBReq = false;
  }

  
  /******************************************************************************
  * Function:     connect
  ******************************************************************************/
  void DMIInterface::connect(Socket^ %socket, IPAddress^ ip, unsigned short int port)
  {
    // If not connected -> Try to connect.
    if (!connected(socket))
    {
      try
      {
        // Create socket
        socket = nullptr;
        socket = gcnew Socket(AddressFamily::InterNetwork, SocketType::Stream, ProtocolType::Tcp);

        socket->Blocking = false;

        // Setup connection end point
        Net::IPEndPoint^ ep = gcnew IPEndPoint(ip, port);
        socket->Connect(ep);

      }
      catch (SocketException ^)
      {
        // No connection
      }
    }
  }

  /******************************************************************************
  * Function:     connected
  ******************************************************************************/
  bool DMIInterface::connected(Socket^ socket)
  {
    bool connected = false;

    if (socket != nullptr)
    {
      if (!((socket->Poll(1000, SelectMode::SelectRead) && (socket->Available == 0)) || !socket->Connected))
      {
        connected = true;
      }
    }

    return connected;
  }
  
  /******************************************************************************
  * Function:     Tick
  ******************************************************************************/
  void DMIInterface::Tick(void)
  {
    bool periodExpired = false;
    if ((ticks % dmiPollPeriod) == 0)
    {  
      periodExpired = true;
    }
    ticks++;

    if (connected(dmiSocket))
    {
      readResponseFromDMI();
    }
    else
    {
      if (periodExpired)
      {
        connect(dmiSocket, ipToDMI, portToDMI);
      }
    }

    if (connected(dmiInternalSocket))
    {
      if (periodExpired)
      {   // request ATPinfo from DMI
        String^ commandString = gcnew String("getATPInfo\r\n");
        sendInternalCommandToDMI(commandString->Length, Encoding::UTF8->GetBytes(commandString));
      }
      else if (releaseEBReq)
      {
        String^ commandString = gcnew String("dmiselect releaseeb\r\n");
        sendInternalCommandToDMI(commandString->Length, Encoding::UTF8->GetBytes(commandString));
        releaseEBReq = false;
      }
      else if (releaseSBReq)
      {
        String^ commandString = gcnew String("dmiselect releasesb\r\n");
        sendInternalCommandToDMI(commandString->Length, Encoding::UTF8->GetBytes(commandString));
        releaseSBReq = false;
      }
      readInternalResponseFromDMI();
    }
    else
    {
      if (periodExpired)
      {
        connect(dmiInternalSocket, ipToDMI, internalPortToDMI);
      }
    }
  }

  /******************************************************************************
  * Function:     readResponseFromDMI
  ******************************************************************************/
  void DMIInterface::readResponseFromDMI(void)
  {
    try
    {
      if (dmiSocket->Available)
      {
        System::Array::Clear(recvBuffer, 0, maxBufferSize);
        recBufCnt = dmiSocket->Receive(recvBuffer, SocketFlags::None);

        if (recBufCnt > 0)
        {
          recvBuffer[recBufCnt] = '\0';
          String^ tmpRecString = System::Text::UTF8Encoding::UTF8->GetString(recvBuffer, 0, recBufCnt);
          array <wchar_t>^ charsToTrim = { L'\r', L'\n', L'\0' };
          tmpRecString = tmpRecString->TrimEnd(charsToTrim);
          recString = tmpRecString->ToLower();
        }
      }
      else
      {
        recBufCnt = 0;
      }
    }
    catch (...)
    {
      // Do Nothing
      recBufCnt = 0;
    }
  }

  /******************************************************************************
  * Function:     readInternalResponseFromDMI
  ******************************************************************************/
  void DMIInterface::readInternalResponseFromDMI(void)
  {
    try
    {
      if (dmiInternalSocket->Available)
      {
        System::Array::Clear(recvBuffer, 0, maxBufferSize);
        recInternalBufCnt = dmiInternalSocket->Receive(recvBuffer, SocketFlags::None);

        if (recInternalBufCnt > 0)
        {
          recvBuffer[recInternalBufCnt] = '\0';
          String^ tmpRecString = System::Text::UTF8Encoding::UTF8->GetString(recvBuffer, 0, recInternalBufCnt);
          array <wchar_t>^ charsToTrim = { L'\r', L'\n', L'\0' };
          tmpRecString = tmpRecString->TrimEnd(charsToTrim);
          String^ recInternalString = tmpRecString->ToLower();
          fields = recInternalString->Split(';');
        }
      }
      else
      {
        recInternalBufCnt = 0;
      }
    }
    catch (...)
    {
      // Nothing received
      recInternalBufCnt = 0;
    }
  }

  /******************************************************************************
  * Function:     sendCommandToDMI
  ******************************************************************************/
  void DMIInterface::sendCommandToDMI(unsigned int size, array<unsigned char>^ data)
  {
    // Send to DMI
    try
    {
      if (dmiSocket->Connected)
        dmiSocket->Send(data, size, SocketFlags::None);
    }
    catch (...)
    {
      // Do nothing
      ;
    }

  }
  /******************************************************************************
  * Function:     sendInternalCommandToDMI
  ******************************************************************************/
  void DMIInterface::sendInternalCommandToDMI(unsigned int size, array<unsigned char>^ data)
  {
    // Send to DMI

    try
    {
      if (dmiInternalSocket->Connected)
        dmiInternalSocket->Send(data, size, SocketFlags::None);
    }
    catch (...)
    {
      // Do nothing
      ;
    }

  }
  /******************************************************************************
  * Function:     responseReceived, returns true if any response received from DMI
  *
  ******************************************************************************/
  bool DMIInterface::responseReceived(void)
  {
    return (recBufCnt > 0) ? true : false;
  }
  /******************************************************************************
  * Function:     internalResponseReceived, returns true if any response received from DMI
  *
  ******************************************************************************/
  bool DMIInterface::internalResponseReceived(void)
  {
    return (recInternalBufCnt > 0) ? true : false;
  }

  /******************************************************************************
  * Function:   atpInfoFieldIndex
  * Returns the index of the field in the array of response arguments received from DMI
  ******************************************************************************/
  UInt16 DMIInterface::atpInfoFieldIndex(enum atpInfoField infoField)
  {
    return ((UInt16)infoField + 1U);
  }
    /******************************************************************************
  * Function:   getPermittedSpeed, cm/s
  *
  ******************************************************************************/
  UInt16 DMIInterface::getPermittedSpeed(void)
  {
    try
    {
      double tempValue = Convert::ToUInt16(fields[atpInfoFieldIndex(atpInfoPermittedSpeed)]) * 100.0/3.6;  // from km/h to cm/s
      return (UInt16)tempValue;
    }
    catch (...)
    {
      return 0U;
    }
  }

  /******************************************************************************
  * Function:   getPermittedDriveDir
  *
  ******************************************************************************/
  EnumDriveDir DMIInterface::getPermittedDriveDir(void)
  {
    try
    {
      return (EnumDriveDir)Convert::ToByte(fields[atpInfoFieldIndex(atpInfoPermittedDrivingDirection)]);
    }
    catch (...)
    {
      return DDUndef;
    }

  }
  /******************************************************************************
  * Function:   getAtpMode
  *
  ******************************************************************************/
  ATPModeEnum DMIInterface::getAtpMode(void)
  {
    try
    {
      return (ATPModeEnum)Convert::ToByte(fields[atpInfoFieldIndex(atpInfoATPMode)]);
    }
    catch (...)
    {
      return ATPModeUndefined;
    }

  }

  /******************************************************************************
  * Function:   getDistanceToTarget (m)
  *
  ******************************************************************************/
  UInt16 DMIInterface::getDistanceToTarget(void)
  {
    try
    {
      return Convert::ToUInt16(fields[atpInfoFieldIndex(atpInfoRemainingDistanceTotarget)]);
    }
    catch (...)
    {
      return 0;
    }

  }
  /******************************************************************************
  * Function:   getDistanceToBCA (m)
  *
  ******************************************************************************/
  UInt16 DMIInterface::getDistanceToBCA(void)
  {
    try
    {
      return Convert::ToUInt16(fields[atpInfoFieldIndex(atpInfoRemainingDistanceToBCA)]);
    }
    catch (...)
    {
      return 0;
    }

  }
  /******************************************************************************
  * Function:   getTargetSpeed (cm/s)
  *
  ******************************************************************************/
  UInt16 DMIInterface::getTargetSpeed(void)
  {
    try
    {
      return (UInt16)(Convert::ToUInt16(fields[atpInfoFieldIndex(atpInfoTargetSpeed)]) * 100.0 / 3.6);  // from km/h to cm/s;
    }
    catch (...)
    {
      return 0;
    }
  }
  /******************************************************************************
  * Function:   getMAMargin (cm)
  *
  ******************************************************************************/
  UInt16 DMIInterface::getMAMargin(void)
  {
    try
    {
      return Convert::ToUInt16(fields[atpInfoFieldIndex(atpInfoMAMargin)]);
    }
    catch (...)
    {
      return 0;
    }
  }
  /******************************************************************************
  * Function:   getTrailingTrack
  *
  ******************************************************************************/
  UInt16 DMIInterface::getTrailingTrack(void)
  {
    try
    {
      return Convert::ToUInt16(fields[atpInfoFieldIndex(atpInfoTrailingTrack)]);  
    }
    catch (...)
    {
      return 0;
    }
  }

  /******************************************************************************
  * Function:   getTrailingPos
  *
  ******************************************************************************/
  UInt32 DMIInterface::getTrailingPos(void)
  {
    try
    {
      return Convert::ToUInt32(fields[atpInfoFieldIndex(atpInfoTrailingPos)]);
    }
    catch (...)
    {
      return 0;
    }
  }

  /******************************************************************************
  * Function:   getTrainLength (m)
  *
  ******************************************************************************/
  UInt16 DMIInterface::getTrainLength(void)
  {
    try
    {
      return Convert::ToUInt16(fields[atpInfoFieldIndex(atpInfoTrainLength)]);
    }
    catch (...)
    {
      return 0;
    }
  }

  /******************************************************************************
  * Function:   getTrackGradient (permil)
  *
  ******************************************************************************/
  Int16 DMIInterface::getTrackGradient(void)
  {
    try
    {
      return Convert::ToInt16(fields[atpInfoFieldIndex(atpInfoTrackGradient)]);
    }
    catch (...)
    {
      return 0;
    }
  }
  /******************************************************************************
  * Function:   getEffectiveGradient (permil)
  *
  ******************************************************************************/
  Int16 DMIInterface::getEffectiveGradient(void)
  {
    try
    {
      return Convert::ToInt16(fields[atpInfoFieldIndex(atpInfoEffectiveGradient)]);
    }
    catch (...)
    {
      return 0;
    }
  }
  /******************************************************************************
  * Function:   getBrakeability (cm/s2)
  *
  ******************************************************************************/
  UInt16 DMIInterface::getBrakeability(void)
  {
    try
    {
      return Convert::ToUInt16(fields[atpInfoFieldIndex(atpInfoBrakeability)]);
    }
    catch (...)
    {
      return 0;
    }
  }
  /******************************************************************************
  * Function:   getBrakeDelayEB (0.1 s)
  *
  ******************************************************************************/
  UInt16 DMIInterface::getBrakeDelayEB(void)
  {
    try
    {
      return Convert::ToUInt16(fields[atpInfoFieldIndex(atpInfoBrakeDelayEB)]);
    }
    catch (...)
    {
      return 0;
    }
  }
  /******************************************************************************
  * Function:   getBrakeDelaySB (0.1 s)
  *
  ******************************************************************************/
  UInt16 DMIInterface::getBrakeDelaySB(void)
  {
    try
    {
      return Convert::ToUInt16(fields[atpInfoFieldIndex(atpInfoBrakeDelaySB)]);
    }
    catch (...)
    {
      return 0;
    }
  }
  /******************************************************************************
  * Function:   getIsAllowedToLogin
  *
  ******************************************************************************/
  bool DMIInterface::getIsAllowedToLogin(void)
  {
     bool retValue = false;
     try
     {
        if ((Byte)(Convert::ToByte(fields[atpInfoFieldIndex(atpInfoAllowedTo)])) & 0x01)
        {
           retValue = true;
        }
     }
     catch (...)
     {
        //Do Nothing
     }

     return retValue;
  }

  /******************************************************************************
  * Function:   getReleaseServiceBrakeFlashes
  *
  ******************************************************************************/
  bool DMIInterface::getReleaseServiceBrakeFlashes(void)
  {
     bool retValue = false;
     try
     {
        if ((Byte)(Convert::ToByte(fields[atpInfoFieldIndex(atpInfoBrakeRelatedIndications)])) & 0x08)
        {
           retValue = true;
        }
     }
     catch (...)
     {
        //Do Nothing
     }

     return retValue;
  }

  /******************************************************************************
  * Function:   getConfirmAcceptAutoConfig
  *
  ******************************************************************************/
  bool DMIInterface::getConfirmAcceptAutoConfig(void)
  {
     bool retValue = false;
     try
     {
        if ((Byte)(Convert::ToByte(fields[atpInfoFieldIndex(atpInfoAdditionalConfirm)]) & 0x08))
        {
           retValue = true;
        }
     }
     catch (...)
     {
        //Do Nothing
     }

     return retValue;
  }

  /******************************************************************************
  * Function:   getConfigModeSubState
  *
  ******************************************************************************/
  ConfigModeSubState DMIInterface::getConfigModeSubState(void)
  {
     try
     {
        return (ConfigModeSubState)Convert::ToByte(fields[atpInfoFieldIndex(atpInfoConfigModeSubstate)]);
     }
     catch (...)
     {
        return ConfigModeSubStateUndefined;
     }
  }
  
  /******************************************************************************
  * Function:   releaseEB
  *
  ******************************************************************************/
  void DMIInterface::releaseEB()
  {
    releaseEBReq = true;
  }
  /******************************************************************************
  * Function:   releaseSB
  *
  ******************************************************************************/
  void DMIInterface::releaseSB()
  {
    releaseSBReq = true;
  }


}

