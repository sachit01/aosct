#ifndef OBRDMessageOut_hpp
#define OBRDMessageOut_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines the OBRD Message Out class.

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
#include "trace_interface.hpp"

namespace ATP
{
  namespace TG
  {
    /**
    * Base class for outgoing OBRD messages.
    */
    class OBRDMessageOut
    {

    public:

      /**
      * Alternative constructor for the parser base class
      *
      * @param[in] trace_  The component's trace object
      * @param[in] mType   The messageType supported for this creator
      */
      OBRDMessageOut(ATC::TraceInterface* const trace_, const OBRDMessageType mType);

      /**
      * Virtual Destructor for the parser base class
      */
      virtual ~OBRDMessageOut();

      /**
      * Assembles the collected data
      *
      * @param[out] messageData  The outgoing message data to be assembled
      *
      * @return true if data is valid with respect to assembling
      */
      virtual bool assemblePacketData(OBRDDataPacket& messageData) = 0;

    protected:

      /**
      * Trace Interface to be used
      */
      ATC::TraceInterface *trace;

    private:

      /**
      * The messageType supported by this parser
      */
      const OBRDMessageType messageType;

      /**
      * Default constructor (disabled)
      */
      OBRDMessageOut();
    };
  }
}
#endif

