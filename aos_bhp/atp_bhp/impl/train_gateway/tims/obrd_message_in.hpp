#ifndef OBRDMESSAGEIN_hpp
#define OBRDMESSAGEIN_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines the OBRD Message In class.

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
    * Base class for incoming OBRD messages.
    */
    class OBRDMessageIn
    {

    public:

      /**
      * Alternative constructor for the parser base class
      *
      * @param[in] trace_  The component's trace object
      * @param[in] mType   The messageType supported for this creator
      */
      OBRDMessageIn(ATC::TraceInterface* const trace_, const OBRDMessageType mType);

      /**
      * Virtual Destructor for the parser base class
      */
      virtual ~OBRDMessageIn();

      /**
      * Validates the extracted data
      *
      * @param[in] messageData  The incoming message data to be parsed
      *
      * @return true if data is valid as a result of a valid incoming message
      */
      virtual bool validate(OBRDDataPacket* const messageData) = 0;

    protected:
      /**
      * Trace Interface to be used
      */
      ATC::TraceInterface *trace;

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate() = 0;

    private:
      /**
      * The messageType supported by this parser
      */
      const OBRDMessageType messageType;

      /**
      * Default constructor (disabled)
      */
      OBRDMessageIn();
    };
  }
}
#endif
