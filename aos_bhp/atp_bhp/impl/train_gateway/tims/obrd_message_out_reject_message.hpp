#ifndef OBRDMessageOutRejectMessage_hpp
#define OBRDMessageOutRejectMessage_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines the OBRD Message Out Reject Message class.

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
    * OBRD message for sending reject.
    */
    class OBRDMessageOutRejectMessage : public OBRDMessageOut
    {

    public:

      /**
      * Constructor for OBRDMessageOutRejectMessage
      */
      OBRDMessageOutRejectMessage(ATC::TraceInterface* const trace_);

      /**
      * Assembles the collected data
      *
      * @param[out] messageData  The outgoing message data to be assembled
      *
      * @return true if data is valid with respect to assembling
      */
      virtual bool assemblePacketData(OBRDDataPacket& messageData);

      /**
      * Sets the rejection reason to be sent.
      */
      void setRejectReason(RejectReason const rejectReason_);

    private:

      /**
      * Default constructor (disabled)
      */
      OBRDMessageOutRejectMessage();

      /**
      * Reject Reason
      */
      RejectReason rejectReason;
    };
  }
}

#endif

