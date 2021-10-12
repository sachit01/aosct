/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the OBRDMessageInProtocolVersion class.

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
#include "obrd_message_in_protocol_version.hpp"
#include <stdio.h>

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
    * OBRDMessageInProtocolVersion constructor
    ******************************************************************************/
    OBRDMessageInProtocolVersion::OBRDMessageInProtocolVersion(ATC::TraceInterface* const trace_) :
      OBRDMessageIn(trace_, obrdMTypeInProtocolVersion),
      majorProVersion(0U),
      minorProVersion(0U)
    {
    }

    /******************************************************************************
    * OBRDMessageInProtocolVersion::validate
    ******************************************************************************/
    bool OBRDMessageInProtocolVersion::validate(OBRDDataPacket* const messageData)
    {
      bool success = false;
      invalidate();
      if (parseMessageData(messageData))
      {
        if (majorProVersion == obrdProtocolVersionMajor)
        {
          success = true;
          trace->write(ATC::briefTrace, "OBRD Protocol Version Matched");
        }
      }

      return success;
    }

    /******************************************************************************
    * parseMessageData
    ******************************************************************************/
    bool OBRDMessageInProtocolVersion::parseMessageData(OBRDDataPacket* const messageData)
    {
      bool parseDataValid = true;
      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &(messageData->messageData[0]), messageData->dataLength);
      vfwSetReadBuffer(&buffer, messageData->dataLength);
      majorProVersion = vfwGetU8(&buffer);
      minorProVersion = vfwGetU8(&buffer);

      char_t buf[100];
      const int32_t ret = snprintf(&buf[0], sizeof(buf), "OBRD Protocol Version message:: Major: %d, Minor: %d",
        majorProVersion, minorProVersion);

      if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buf)))
      {
        trace->write(ATC::briefTrace, &buf[0]);
      }

      return parseDataValid;
    }

    /******************************************************************************
    * OBRDMessageInProtocolVersion::invalidate
    ******************************************************************************/
    void OBRDMessageInProtocolVersion::invalidate()
    {
      majorProVersion = 0U;
      minorProVersion = 0U;
    }

  }
}
