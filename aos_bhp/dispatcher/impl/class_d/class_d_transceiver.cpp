/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*
* DESCRIPTION:
* Defines base-class for Class D protocol Library.
*
*****************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-01-06   adgupta     Created
* 2017-02-06   spandita    Created & updated the functions
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "class_d_transceiver.hpp"
#include <vfw_buffer.h>

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ClassD
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  TransceiverClassD::TransceiverClassD()
  {
    transceiver.prevRecvCommId = 0U;  //Prev Received CommID 
    transceiver.recvCommId = 0U;      //Received CommID 
    transceiver.sendCommId = 1U;      //Send CommID
    transceiver.protocolVersion = classDProtocolVer;  //Protocol Version 
    transceiver.messageType = classDMsgType;          //Message type 
    transceiver.messageVersion = classDMsgVer;        //Message version
    transceiver.sendBuffSize = maxMessageLen;         //Max buffer size
    transceiver.recvBuffSize = maxMessageLen;        //Max buffer size
  }

  /******************************************************************************
  * parseMessage
  ******************************************************************************/
  ErrorType TransceiverClassD::parseMessage(uint8_t* const receivedData, const uint32_t receivedLength)
  {
    ErrorType errorType = NoError;

    if (receivedLength < (maxClassDHeaderSize + maxClassDFooterSize))
    {
      errorType = MessageIncomplete;
    }
    else
    {
      VFW_Buffer vfwBuffer;
      //Init the VFW buffer
      vfwInitBuffer(&vfwBuffer, receivedData, receivedLength);
      //Set it to read mode
      vfwSetReadBuffer(&vfwBuffer, receivedLength);
      //STX
      uint8_t start = vfwGetU8(&vfwBuffer);
      //Protocol Version
      transceiver.protocolVersion = vfwGetU8(&vfwBuffer);
      //Command ID
      transceiver.prevRecvCommId = transceiver.recvCommId;
      transceiver.recvCommId = vfwGetU32(&vfwBuffer);
      //Message type
      uint8_t msgType = vfwGetU8(&vfwBuffer);
      //Message Version
      transceiver.messageVersion = vfwGetU8(&vfwBuffer);
      //message length
      transceiver.messageLength = vfwGetU32(&vfwBuffer);

      if (stx != start)
      {
        errorType = MissingSTX;
      }
      else if (classDProtocolVer != transceiver.protocolVersion)
      {
        errorType = IncorrectMessageFormat;
      }
      else if (msgType != static_cast<uint8_t>(DataMsg))
      {
        errorType = NotDataMsg;
      }
      else if (getClassDMessageMaxBodyLen() < transceiver.messageLength)
      {
        errorType = IncorrectMessageFormat;
      }
      else if (getClassDMessageActualLen() > receivedLength)
      {
        errorType = MessageIncomplete;
      }
      else
      {
        //Copy Class D body
        vfwCpyToRawBuffer(&transceiver.buffer[maxClassDHeaderSize], &vfwBuffer, transceiver.messageLength);
        //ETX
        uint8_t end = vfwGetU8(&vfwBuffer);
        if (end != etx)
        {
          errorType = MissingETX;
        }
        else if ((transceiver.recvCommId != (transceiver.prevRecvCommId + 1U)) || (transceiver.recvCommId == 0U))
        {
          errorType = CommIdIncorrect;
        }
        else
        {
          errorType = NoError;
        }
      }
    }

    return errorType;
  }

  /******************************************************************************
  * buildMessage
  ******************************************************************************/
  bool TransceiverClassD::buildMessage(const uint8_t* const body, const uint32_t length)
  {
    VFW_Buffer vfwBuffer;
    bool retVal = true;

    if (length > getClassDMessageMaxBodyLen())
    {
      retVal = false;
    }
    else
    {
      //Assign the length 
      transceiver.messageLength = length;
      //Init the VFW Buffer
      vfwInitBuffer(&vfwBuffer, &transceiver.buffer[0], maxMessageLen);
      //STX
      vfwPutU8(&vfwBuffer, stx);
      //protocol version 
      vfwPutU8(&vfwBuffer, transceiver.protocolVersion);
      //CommID
      vfwPutU32(&vfwBuffer, transceiver.sendCommId);
      //Message type
      vfwPutU8(&vfwBuffer, static_cast<uint8_t>(transceiver.messageType));
      //Message version 
      vfwPutU8(&vfwBuffer, transceiver.messageVersion);

      //Data Length
      vfwPutU32(&vfwBuffer, length);
      //Message Body
      vfwCpyFromRawBuffer(&vfwBuffer, body, length);
      //ETX
      vfwPutU8(&vfwBuffer, etx);
      //Increment the Send CommId
      ++transceiver.sendCommId;

      if (0U == transceiver.sendCommId)
      {
        //Handle the Roll-off to 0. 
        ++transceiver.sendCommId;
      }

    }
    return retVal;
  }

  /******************************************************************************
  * getClassDBufferPtr
  ******************************************************************************/
  const uint8_t* TransceiverClassD::getClassDBufferPtr() const
  {
    return transceiver.buffer;
  }

  /******************************************************************************
  * getClassDBodyPtr
  ******************************************************************************/
  const uint8_t* TransceiverClassD::getClassDBodyPtr() const
  {
    return &transceiver.buffer[maxClassDHeaderSize];
  }

  /******************************************************************************
  * getClassDMessageActualLen
  ******************************************************************************/
  uint32_t TransceiverClassD::getClassDMessageActualLen() const
  {
    return (maxClassDHeaderSize + transceiver.messageLength + maxClassDFooterSize);
  }
  /******************************************************************************
  * getActualMsgLen
  ******************************************************************************/
  uint32_t TransceiverClassD::getActualMsgLen() const
  {
    return transceiver.messageLength;
  }
  /******************************************************************************
  * resetCommID
  ******************************************************************************/
  void TransceiverClassD::resetCommID() 
  {
    transceiver.sendCommId = 1U;
    transceiver.recvCommId = 0U;
    transceiver.prevRecvCommId = 0U;
  }

  /******************************************************************************
  * getClassDMessageMaxBodyLen
  ******************************************************************************/
  uint32_t TransceiverClassD::getClassDMessageMaxBodyLen() const
  {
    return(maxMessageLen - (maxClassDFooterSize + maxClassDHeaderSize));
  }

  /******************************************************************************
  * getEMPMessageMaxTotalLen
  ******************************************************************************/
  uint32_t TransceiverClassD::getClassDMessageMaxTotalLen() const
  {
      return(maxMessageLen);
  }
}
