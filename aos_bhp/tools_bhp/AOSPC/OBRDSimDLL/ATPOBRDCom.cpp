#pragma once
#include "StdAfx.h"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          ATPOBRDCom.cpp %
*
*  %version:       4 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-03-21 12:27 %
*
*  DESCRIPTION: OBRD Protocol Layers according to FFFIS AOS-OBRD
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-11-02    Marlundg    File created
* 2018-11-19    Marlundg    Changed socket to being non-blocking
*
*******************************************************************************/
#include "ATPOBRDCom.h"

using namespace System::Text;

/******************************************************************************
* Function:     ATPOBRDCom
* Description:
******************************************************************************/
OBRDSimDLL::ATPOBRDCom::ATPOBRDCom(
  IPAddress^ ip,
  unsigned short port,
  unsigned short mdlSiteId,
  String^ mdlReceiverId,
  String^ mdlSenderId,
  unsigned short mdlLocoId)
{
  aosIp = ip;
  aosObrdPort = port;

  atpObrdSocket = nullptr;

  siteId = mdlSiteId;
  receiverId = mdlReceiverId;
  senderId = mdlSenderId;
  locoId = mdlLocoId;
}

/******************************************************************************
* Function:     updateSafetyParams
* Description: Updates the safety parameters
******************************************************************************/
void OBRDSimDLL::ATPOBRDCom::UpdateSafetyParams(unsigned short mdlSiteId, String^ mdlReceiverId, String^ mdlSenderId, unsigned short mdlLocoId)
{
  siteId = mdlSiteId;
  receiverId = mdlReceiverId;
  senderId = mdlSenderId;
  locoId = mdlLocoId;
}


/******************************************************************************
* Function:     CalculateCRC64
* Description: Calculate 64-bit CRC from data-array.
******************************************************************************/
unsigned long long OBRDSimDLL::ATPOBRDCom::CalculateCRC64(array<Byte>^ data, UInt16 startIndex, UInt16 dataSize)
{
  UInt64 crc = CRC_INIT_VAL;
  UInt16 dataIndex = startIndex;

  while (dataIndex < (dataSize + startIndex))
  {
    Int64 temp = data[dataIndex] ^ (Byte)(crc >> 56);
    crc = crcTable[data[dataIndex] ^ (Byte)(crc >> 56)] ^ (crc << 8);
    dataIndex++;
  }

  return crc;
}

/******************************************************************************
* Function:     Connect
* Description: Connect to AOS
******************************************************************************/
void OBRDSimDLL::ATPOBRDCom::Connect()
{
  // If not connected to OBRD port, try to connect.
  if (!Connected())
  {
    try
    {
      // Create socket
      atpObrdSocket = nullptr;
      atpObrdSocket = gcnew Socket(AddressFamily::InterNetwork, SocketType::Stream, ProtocolType::Tcp);

      atpObrdSocket->Blocking = false;

      // Setup connection end point
      Net::IPEndPoint^ ep = gcnew IPEndPoint(aosIp, aosObrdPort);
      atpObrdSocket->Connect(ep);

    }
    catch (SocketException ^)
    {
      // No connection
    }
  }
}

/******************************************************************************
* Function:     Disconnect
* Description: Disconnect from AOS
******************************************************************************/
void OBRDSimDLL::ATPOBRDCom::Disconnect()
{
  if (atpObrdSocket)
  {
    atpObrdSocket->Close();
  }

  atpObrdSocket = nullptr;

}

/******************************************************************************
* Function:     Connected
* Description: Check if connected to AOS
******************************************************************************/
bool OBRDSimDLL::ATPOBRDCom::Connected()
{
  bool connected = false;

  if (atpObrdSocket != nullptr)
  {
    if (!((atpObrdSocket->Poll(1000, SelectMode::SelectRead) && (atpObrdSocket->Available == 0)) || !atpObrdSocket->Connected))
    {
      connected = true;
    }
  }
  
  return connected;
}

/******************************************************************************
* Function:     SendProtocolVersion
* Description: Send the ProtocolVersion to AOS
******************************************************************************/
bool OBRDSimDLL::ATPOBRDCom::SendProtocolVersion(Byte majorVersion, Byte minorVersion, bool injectCRCError)
{
  array<Byte>^ sendBuffer;

  sendBuffer = gcnew array<Byte>(RL_PROTOCOL_PACKET);

  unsigned short sendIndex = 0U;

  // RNID_PACKET
  BitConverter::GetBytes(htons(RNID_PROTOCOL_PACKET))->CopyTo(sendBuffer, sendIndex);
  sendIndex += sizeof(RNID_PROTOCOL_PACKET);

  // RL_PACKET
  BitConverter::GetBytes(htons(RL_PROTOCOL_PACKET))->CopyTo(sendBuffer, sendIndex);
  sendIndex += sizeof(RL_PROTOCOL_PACKET);

  // RM_PROTOCOL_VERSION
  sendBuffer[sendIndex++] = majorVersion;

  // RM_PROTOCOL_VERSION
  sendBuffer[sendIndex++] = minorVersion;

  // Add Safety-layer and send message on socket
  return (SendData(sendBuffer, injectCRCError));
}

/******************************************************************************
* Function:     SendStatusReport
* Description: Send the OBRDStatusReport to AOS
******************************************************************************/
bool OBRDSimDLL::ATPOBRDCom::SendStatusReport(posItem rearPosition, Byte lastCarBrakePressure, bool injectCRCError)
{
  array<Byte>^ sendBuffer;

  sendBuffer = gcnew array<Byte>(RL_STATUS_PACKET);

  unsigned short sendIndex = 0U;

  // RNID_PACKET
  BitConverter::GetBytes(htons(RNID_STATUS_PACKET))->CopyTo(sendBuffer, sendIndex);
  sendIndex += sizeof(RNID_STATUS_PACKET);
  
  // RL_PACKET
  BitConverter::GetBytes(htons(RL_STATUS_PACKET))->CopyTo(sendBuffer, sendIndex);
  sendIndex += sizeof(RL_STATUS_PACKET);
  
  // RNID_TRACK
  BitConverter::GetBytes(htons(rearPosition.trackId))->CopyTo(sendBuffer, sendIndex);
  sendIndex += sizeof(rearPosition.trackId);

  // RNID_OFFSET
  BitConverter::GetBytes(static_cast<unsigned int>(htonl(rearPosition.position)))->CopyTo(sendBuffer, sendIndex);
  sendIndex += sizeof(rearPosition.position);

  // RT_TIMESTAMP
  BitConverter::GetBytes(HTONLL(rearPosition.timeStamp))->CopyTo(sendBuffer, sendIndex);
  sendIndex += sizeof(rearPosition.timeStamp);

  // RN_BPLASTCAR
  sendBuffer[sendIndex++] = lastCarBrakePressure;

  // Add Safety-layer and send message on socket
  return (SendData(sendBuffer, injectCRCError));
}

/******************************************************************************
* Function:     SendData
* Description: Add Safety-header and send application data to AOS.
******************************************************************************/
bool OBRDSimDLL::ATPOBRDCom::SendData(array<Byte>^ data, bool injectCRCError)
{
  bool            retval = true;
  array<Byte>^    sendBuffer = gcnew array<unsigned char>(256);

  unsigned short sendIndex = 0U;
  unsigned short totalMessageLen = 1 + 2 + 2 + (receiverId->Length + 1) + (senderId->Length + 1) + 2 + 8 + data->Length + 8;
   
  if (nullptr != atpObrdSocket)
  {
    try
    {
      // Message start, Safety layer
      sendBuffer[sendIndex++] = STX;
      
      // Total Length
      BitConverter::GetBytes(htons(totalMessageLen - 1))->CopyTo(sendBuffer, sendIndex);
      sendIndex += sizeof(totalMessageLen);

      // Site ID
      BitConverter::GetBytes(htons(siteId))->CopyTo(sendBuffer, sendIndex);
      sendIndex += sizeof(siteId);

      // RXID_RECEIVER
      Encoding::UTF8->GetBytes(receiverId)->CopyTo(sendBuffer, sendIndex);
      sendIndex += Encoding::UTF8->GetCharCount(Encoding::UTF8->GetBytes(receiverId));
      sendBuffer[sendIndex++] = '\0';

      // RXID_SENDER
      Encoding::UTF8->GetBytes(senderId)->CopyTo(sendBuffer, sendIndex);
      sendIndex += Encoding::UTF8->GetCharCount(Encoding::UTF8->GetBytes(senderId));
      sendBuffer[sendIndex++] = '\0';

      // RNID_LOCOMOTIVE
      BitConverter::GetBytes(htons(locoId))->CopyTo(sendBuffer, sendIndex);
      sendIndex += sizeof(locoId);

      // RT_TIMESTAMP
      BitConverter::GetBytes(HTONLL(getUTCTime()))->CopyTo(sendBuffer, sendIndex);
      sendIndex += sizeof(getUTCTime());
  
      // Message data
      data->CopyTo(sendBuffer, sendIndex);
      sendIndex += data->Length;

      // CRC
      unsigned long long crc = CalculateCRC64(sendBuffer, 1, totalMessageLen - 1 - 8);

      // Inject CRC error? --> Add one to checksum.
      if (injectCRCError)
      {
        ++crc;
      }

      BitConverter::GetBytes(HTONLL(crc))->CopyTo(sendBuffer, sendIndex);
      sendIndex += sizeof(crc);

      atpObrdSocket->Send(sendBuffer, totalMessageLen, SocketFlags::None);
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
* Function:     ReceiveProtocolVersion
* Description: Check (and get data) if last message from AOS was ProtocolVersion.
******************************************************************************/
bool OBRDSimDLL::ATPOBRDCom::ReceiveProtocolVersion(Byte &majorVersion, Byte &minorVersion)
{
  bool retVal = false;

  if (RNID_PROTOCOL_PACKET == rnidPacket)
  {
    majorVersion = rmPrprotocolVersionMajor;
    minorVersion = rmPrprotocolVersionMinor;

    retVal = true;
  }

  return retVal;
}

/******************************************************************************
* Function:     ReceiveRejection
* Description: Check (and get data) if last message from AOS was MessageRejection.
******************************************************************************/
bool OBRDSimDLL::ATPOBRDCom::ReceiveRejection(Byte &reason, Byte &majorVersion, Byte &minorVersion)
{
  bool retVal = false;

  if (RNID_REJECT_PACKET == rnidPacket)
  {
    reason = rqRejection;
    majorVersion = rmPrprotocolVersionMajor;
    minorVersion = rmPrprotocolVersionMinor;

    retVal = true;
  }

  return retVal;
}

/******************************************************************************
* Function:     ReceiveData
* Description: Check if any data is received from AOS. Fetch actual data with specific ReceiveXX method.
******************************************************************************/
bool OBRDSimDLL::ATPOBRDCom::ReceiveData(unsigned short &receivedRnid)
{
  bool  retVal = false;
  
  if (nullptr != atpObrdSocket)
  {
    array<Byte>^    buffer = gcnew array<Byte>(256);
    int             recCnt;

    try
    {
      // Read any buffered messages/bytes from socket and use latest valid message
      do
      {
        recCnt = atpObrdSocket->Receive(buffer, SocketFlags::None);

        if (recCnt > 0)
        {
          unsigned short recIndex = 0;

          Byte stx = buffer[recIndex++];
          
          unsigned short rlMessage = ntohs(BitConverter::ToUInt16(buffer, recIndex));
          recIndex += sizeof(rlMessage);

          unsigned short rnidSite = ntohs(BitConverter::ToUInt16(buffer, recIndex));
          recIndex += sizeof(rnidSite);

          Byte rxidReceiverLength = 1;
          Byte rxidSenderLength = 1;

          // rxidReceiver
          while (buffer[recIndex++] != '\0')
          {
            rxidReceiverLength++;
          }

          // rxidSender
          while (buffer[recIndex++] != '\0')
          {
            rxidSenderLength++;
          }

          unsigned short rnidLocomotive = ntohs(BitConverter::ToUInt16(buffer, recIndex));
          recIndex += sizeof(rnidLocomotive);

          unsigned long long rtTimestamp = NTOHLL(BitConverter::ToUInt64(buffer, recIndex));
          recIndex += sizeof(rtTimestamp);

          array<Byte>^ appDataBuffer = gcnew array<Byte>(256);

          // Copy application data to be used for interpretation
          // The length is calculated as RL_MESSAGE - all other header fields.
          System::Array::Copy(buffer, recIndex, appDataBuffer, 0, rlMessage - (2 + 2 + rxidReceiverLength + rxidSenderLength + 2 + 8 + 8));

          unsigned short receivedAppDataLen = 0;
          unsigned short rnId = 0;

          // Interpret buffer according to provided method
          bool appDataOk = InterpretApplicationData(appDataBuffer, receivedAppDataLen, rnId);
         
          recIndex += receivedAppDataLen;

          // CRC
          unsigned long long crc = NTOHLL(BitConverter::ToUInt64(buffer, recIndex));
          recIndex += sizeof(crc);

          // Check CRC
          unsigned long long calculatedCrc = CalculateCRC64(buffer, 1, rlMessage - 8);

          // Application-data received properly
          if (appDataOk && (crc == calculatedCrc))
          {
            retVal = true;
            receivedRnid = rnId;

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

  return retVal;
}

/******************************************************************************
* Function:     InterpretApplicationData
* Description: Interpret the Application Protocol.
******************************************************************************/
bool OBRDSimDLL::ATPOBRDCom::InterpretApplicationData(array<Byte>^ buffer, unsigned short &interpretedlength, unsigned short &receivedRnid)
{
  bool retVal = true;

  unsigned short recIndex = 0;
  
  rnidPacket = ntohs(BitConverter::ToUInt16(buffer, recIndex));
  recIndex += sizeof(rnidPacket);

  // Parse & store information from received messages
  switch (rnidPacket)
  {
  case RNID_PROTOCOL_PACKET:

    rlPacket = ntohs(BitConverter::ToUInt16(buffer, recIndex));
    recIndex += sizeof(rlPacket);

    rmPrprotocolVersionMajor = buffer[recIndex++];
    rmPrprotocolVersionMinor = buffer[recIndex++];
    
    interpretedlength = rlPacket;
    receivedRnid = rnidPacket;

    break;

  case RNID_REJECT_PACKET:

    rlPacket = ntohs(BitConverter::ToUInt16(buffer, recIndex));
    recIndex += sizeof(rlPacket);

    rqRejection = buffer[recIndex++];
    rmPrprotocolVersionMajor = buffer[recIndex++];
    rmPrprotocolVersionMinor = buffer[recIndex++];

    interpretedlength = rlPacket;
    receivedRnid = rnidPacket;

    break;

  default:
    retVal = false;
    break;
  }

  return retVal;
}

/******************************************************************************
* Function:     ClearData
* Description: Clears the communication variables.
******************************************************************************/
void OBRDSimDLL::ATPOBRDCom::ClearData(void)
{ 
  if (atpObrdSocket)
  {
    atpObrdSocket->Close();
  }

  atpObrdSocket = nullptr;
}
