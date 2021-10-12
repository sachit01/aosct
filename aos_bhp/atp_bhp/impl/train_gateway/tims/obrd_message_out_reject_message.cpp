/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the OBRDMessageOutRejectMessage class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-25    sunilk    Created

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "obrd_message_out_reject_message.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace TG
  {

    /******************************************************************************
    * constructor
    ******************************************************************************/
    OBRDMessageOutRejectMessage::OBRDMessageOutRejectMessage(ATC::TraceInterface* const trace_) :
      OBRDMessageOut(trace_, obrdMTypeOutRejectMessage)
    {
      rejectReason = OBRDRejectReasonUndefined;
    }

    /******************************************************************************
    * setRejectReason
    ******************************************************************************/
    void OBRDMessageOutRejectMessage::setRejectReason(RejectReason const rejectReason_)
    {
      rejectReason = rejectReason_;
    }

    /******************************************************************************
    * assemblePacketData
    ******************************************************************************/
    bool OBRDMessageOutRejectMessage::assemblePacketData(OBRDDataPacket& messageData)
    {
      VFW_Buffer buffer;

      vfwInitBuffer(&buffer, &messageData.messageData[0], maxObrdDataSize);
      vfwPutU8(&buffer, static_cast<uint8_t>(rejectReason));
      vfwPutU8(&buffer, obrdProtocolVersionMajor);
      vfwPutU8(&buffer, obrdProtocolVersionMinor);

      messageData.dataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));
      messageData.packetSize = OBRDDataPacket::headerSize + messageData.dataLength;
      messageData.packetType = obrdPTypeMessageReject;

      trace->write(ATC::briefTrace, "OBRD Reject Message Packed with Reject Reason", static_cast<uint32_t>(rejectReason));

      return true;
    }
  }
}
