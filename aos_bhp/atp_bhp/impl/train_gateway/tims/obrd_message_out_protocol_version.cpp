/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the OBRDMessageOutProtocolVersion class.

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
#include "obrd_message_out_protocol_version.hpp"

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
    OBRDMessageOutProtocolVersion::OBRDMessageOutProtocolVersion(ATC::TraceInterface* const trace_) :
      OBRDMessageOut(trace_, obrdMTypeOutProtocolVersion)
    {
    }

    /******************************************************************************
    * assemblePacketData
    ******************************************************************************/
    bool OBRDMessageOutProtocolVersion::assemblePacketData(OBRDDataPacket& messageData)
    {
      VFW_Buffer buffer;
      vfwInitBuffer(&buffer, &messageData.messageData[0], sizeof(messageData.messageData));

      vfwPutU8(&buffer, obrdProtocolVersionMajor);
      vfwPutU8(&buffer, obrdProtocolVersionMinor);

      messageData.dataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));
      messageData.packetSize = OBRDDataPacket::headerSize + messageData.dataLength;
      messageData.packetType = obrdPTypeProtocolVersion;
      trace->write(ATC::briefTrace, "OBRD Protocol Version Message Packed");
      return true;
    }
  }
}
