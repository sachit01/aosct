#ifndef OBRDMessageInProtocolVersion_hpp
#define OBRDMessageInProtocolVersion_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines the OBRD Message In Protocol Version class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-25    skothiya    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "obrd_message_common.hpp"
#include "obrd_message_in.hpp"

namespace ATP
{
  namespace TG
  {
    /**
    * OBRD message for receiving protocol version.
    */
    class OBRDMessageInProtocolVersion : public OBRDMessageIn
    {

    public:
      /**
      * Constructor for OBRDMessageInProtocolVersion which is a parser for the incoming OBRD Protocol Version message
      */
      OBRDMessageInProtocolVersion(ATC::TraceInterface* const trace_);

      /**
      * Validates the extracted data
      *
      * @param[in] messageData  The incoming message data to be parsed
      *
      * @return true if data is valid as a result of a valid incoming message
      */
      virtual bool validate(OBRDDataPacket * const messageData);

    protected:
      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

    private:

      /**
      * Default constructor (disabled)
      */
      OBRDMessageInProtocolVersion();

      /**
      * Parses the extracted data
      *
      * @param[in] messageData  The incoming message data to be parsed
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseMessageData(OBRDDataPacket* const messageData);

      /**
      * place holder for protocol version (major)
      */
      uint8_t majorProVersion;

      /**
      * place holder for protocol version (minor)
      */
      uint8_t minorProVersion;

    };
  }
}

#endif
