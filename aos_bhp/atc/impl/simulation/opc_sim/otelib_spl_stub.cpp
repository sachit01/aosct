/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This simulates the functionality of the SPL Library
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-04-26    rquensel    Created from P8/OTE code
*
*******************************************************************************/


#include <algorithm>    // std::find_if
#include <vector>
#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "spl_a_errorcodes.h"
#include "otelib_splstub.h"
#include "spl_handler.hpp"  // just for the type Profibus_Type

extern "C" void _IOsetProfibusOutputData(Profibus_Type* pBuff);

oteDllSplStub_splConnectionsListType oteDllSplStub_connectionsList;

/******************************************************************************
* oteDllSplStub_addSplConnection
******************************************************************************/
void
oteDllSplStub_addSplConnection(OteLib_SPL_Connection *splConnection)
{
  oteDllSplStub_connectionsList.push_back(splConnection);
}

/******************************************************************************
* oteDllSplStub_findSplConnection
******************************************************************************/
OteLib_SPL_Connection *
oteDllSplStub_findSplConnection(const uint32_t My_Profibus_Address, const uint32_t targetAddress, const uint32_t sap)
{
  oteDllSplStub_splConnectionsListType::iterator iter = std::find_if(oteDllSplStub_connectionsList.begin(), oteDllSplStub_connectionsList.end(),
  OteLibFindP2PConnection(My_Profibus_Address, targetAddress, sap));
  if (iter != oteDllSplStub_connectionsList.end())
  {
    // Connection exists
    return *iter;
  }

  // Connection does not exist
  return NULL;
}


/******************************************************************************
* oteDllSplStub_removeSplConnection
******************************************************************************/
void
oteDllSplStub_removeSplConnection(OteLib_SPL_Connection *splConnection)
{
  oteDllSplStub_splConnectionsListType::iterator iter = oteDllSplStub_connectionsList.begin();

  if (iter != oteDllSplStub_connectionsList.end())
  {
    if (*iter == splConnection)
    {
      delete *iter;
      iter = oteDllSplStub_connectionsList.erase(iter);
    }
    else
    {
      ++iter;
    }
  }
}


/******************************************************************************
* oteDllSplStub_removeSplConnections
******************************************************************************/
void
oteDllSplStub_removeSplConnections()
{
  oteDllSplStub_splConnectionsListType::iterator iter = oteDllSplStub_connectionsList.begin();

  if (iter != oteDllSplStub_connectionsList.end())
  {
    // All objects created by SPL_SIM_Create_P2P shall be deleted by someone. Doing that here.
    delete *iter;
    iter = oteDllSplStub_connectionsList.erase(iter);
  }
}


/******************************************************************************
* oteDllSplStub_appSpecificWriteSplDataToAtpBuf
******************************************************************************/
int32_t
oteDllSplStub_appSpecificWriteSplDataToAtpBuf(const uint8_t* writeBuffer, int32_t sap)
{
  Profibus_Type pbWriteBuffer;

  // What about recvAddress and sendAddress? They are not initialized to anything here. They may exist in the beginning of the message,
  // as part of the 9 byte fake SPL header, so setting them might be possible. See also SplWrapper::receive(), SPL_SIM_Queue_send,
  // OTEDllInterface::readBuffers() and vfwChannelWriteBuffer (the one in vfwStub.cpp).
  pbWriteBuffer.SAP_no = static_cast<uint8_t>(sap);
  memcpy(pbWriteBuffer.Profibus_Data, writeBuffer, sizeof(pbWriteBuffer.Profibus_Data));
  _IOsetProfibusOutputData(&pbWriteBuffer);

  return 0;
}




/******************************************************************************
* Constructor OteLib_SPL_Connection
******************************************************************************/
OteLib_SPL_Connection::OteLib_SPL_Connection(const char_t* /*pName*/,
  const uint32_t SourceProfibusAddress,
  const uint32_t TargetProfibusAddress,
  const uint32_t SAP,
  const ConnectionMode connectionMode,
  const uint32_t SafetyLevel,
  const uint32_t IdleTime,
  const /*SPL_P2P_Protocol_Parameters*/void* const pProtocolParameters,
  const /*SPL_Connection*/void * const pP2PConnection)
  :
  SourceProfibusAddress_(SourceProfibusAddress),
  TargetProfibusAddress_(TargetProfibusAddress),
  SAP_(SAP),
  connectionMode_(connectionMode),
  SafetyLevel_(SafetyLevel),
  IdleTime_(IdleTime),
  pProtocolParameters_(pProtocolParameters),
  pP2PConnection_(pP2PConnection),
  state_(SPL_IS_CLOSED)
{

}


/******************************************************************************
* Destructor OteLib_SPL_Connection
******************************************************************************/
OteLib_SPL_Connection::~OteLib_SPL_Connection()
{
  // Delete all remaining messages in the queue.
  while (queueForMessagesReceivedFromExe_.size() > 0)
  {
    OteLibSplMRT *m = queueForMessagesReceivedFromExe_.front();
    queueForMessagesReceivedFromExe_.pop();
    delete m;
  }
}


/******************************************************************************
* putSplMessage
******************************************************************************/
int32_t OteLib_SPL_Connection::putSplMessage(const uint8_t* const messageBuffer, uint32_t messageLength, const uint64_t receptionTime)
{
  // Doing new here, remember to delete it, in takeSplMessage and in ~OteLib_SPL_Connection.
  OteLibSplMRT *m = new OteLibSplMRT((const char_t * const)messageBuffer, messageLength, receptionTime);
  //m->log("putSplMessage"); // Some logging used for debugging and reverse engineering, can be commented out before check in.
  queueForMessagesReceivedFromExe_.push(m);
  return 0;
}

/******************************************************************************
* takeSplMessage
* Returns: >0 if a message was found, -1 if no message was found, less than -1 if something is wrong.
******************************************************************************/
int32_t OteLib_SPL_Connection::takeSplMessage(uint8_t* const messageBuffer, uint32_t bufferSize, uint64_t* receptionTime)
{
  if (queueForMessagesReceivedFromExe_.size() > 0)
  {
    OteLibSplMRT *m = queueForMessagesReceivedFromExe_.front();
    queueForMessagesReceivedFromExe_.pop();
    const uint32_t len = m->getMessageLength();
    const uint8_t* ptr = (uint8_t*)m->getMessageBuffer();

    //m->log("takeSplMessage"); // Some logging used for debugging and reverse engineering, can be commented out before check in.

    if (len > bufferSize)
    {
      return -2;
    }

    memcpy(messageBuffer, ptr, len);
    *receptionTime = m->getReceptionTime();
    delete m;
    return static_cast<int32_t>(len);
  }
  return -1;
}


/******************************************************************************
* resetQueue
******************************************************************************/
void OteLib_SPL_Connection::resetQueue()
{
  uint8_t tmpBuf[1024];
  uint64_t receptionTime;
  for (;;)
  {
    int32_t n = takeSplMessage(tmpBuf, sizeof(tmpBuf), &receptionTime);
    if (n <= 0)
    {
      break;
    }
  }
}


/******************************************************************************
* setState
******************************************************************************/
void OteLib_SPL_Connection::setState(int32_t state)
{
  state_ = state;

  // TODO: How will ATPCU find out about this? Is there some callback we must call if state changes?
}


/******************************************************************************
* Constructor OteLibSplMRT
******************************************************************************/
OteLibSplMRT::OteLibSplMRT(const char_t* const messageBuffer, uint32_t messageLength, const uint64_t receptionTime)
  : messageLength_(messageLength),
  receptionTime_(receptionTime)
{
  messageBuffer_ = malloc(messageLength);
  memcpy(messageBuffer_, messageBuffer, messageLength);
}

/******************************************************************************
* Destructor OteLibSplMRT
******************************************************************************/
OteLibSplMRT::~OteLibSplMRT()
{
  free(messageBuffer_);
  messageBuffer_ = NULL;
}


/******************************************************************************
* OteLibFindP2PConnection
******************************************************************************/
OteLibFindP2PConnection::OteLibFindP2PConnection(
  const uint32_t source,
  const uint32_t target,
  const uint32_t sap)
  :
  source_(source),
  target_(target),
  sap_(sap)
{
}


/******************************************************************************
* operator()
******************************************************************************/
bool
OteLibFindP2PConnection::operator()(const OteLib_SPL_Connection* conn)
{
  if (conn)
  {
    if (((source_ == conn->getSourceProfibusAddress()) || (source_ == WildCard)) &&
      ((target_ == conn->getTargetProfibusAddress()) || (target_ == WildCard)) &&
      ((sap_ == conn->getSAP()) || (sap_ == WildCard)))
    {
      return true;
    }
  }

  return false;
}

