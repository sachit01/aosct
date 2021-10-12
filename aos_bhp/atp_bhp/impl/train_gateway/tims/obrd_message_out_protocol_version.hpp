#ifndef OBRDMessageOutProtocolVersion_hpp
#define OBRDMessageOutProtocolVersion_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines the OBRD Message Out Protocol Version class.

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
#include "obrd_message_out.hpp"

namespace ATP
{
  namespace TG
  {
    /**
    * OBRD message for sending protocol version.
    */
    class OBRDMessageOutProtocolVersion : public OBRDMessageOut
    {

    public:
      /**
      * Constructor for OBRDMessageOutProtocolVersion which is a parser for the incoming OBRD Protocol Version message
      */
      OBRDMessageOutProtocolVersion(ATC::TraceInterface* const trace_);

      /**
      * Assembles the collected data
      *
      * @param[out] messageData  The outgoing message data to be assembled
      *
      * @return true if data is valid with respect to assembling
      */
      virtual bool assemblePacketData(OBRDDataPacket& messageData);

    private:

      /**
      * Default constructor (disabled)
      */
      OBRDMessageOutProtocolVersion();
    };
  }
}

#endif
