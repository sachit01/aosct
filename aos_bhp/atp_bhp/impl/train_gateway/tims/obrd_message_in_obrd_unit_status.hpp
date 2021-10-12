#ifndef OBRDMessageInOBRDUnitStatus_hpp
#define OBRDMessageInOBRDUnitStatus_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines the OBRD Message In OBRD Unit Status class.

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
#include "obrd_message_in.hpp"

namespace ATP
{
  namespace TG
  {
    /**
    * OBRD message for receiving OBRD status.
    */
    class OBRDMessageInUnitStatus : public OBRDMessageIn
    {

    public:
      /**
      * Constructor for OBRDMessageInProtocolVersion which is a parser for the incoming OBRD Protocol Version message
      */
      OBRDMessageInUnitStatus(ATC::TraceInterface* const trace_);

      /**
      * Validates the extracted data
      *
      * @param[in] messageData  The incoming message data to be parsed
      *
      * @return true if data is valid as a result of a valid incoming message
      */
      virtual bool validate(OBRDDataPacket* const messageData);

      /**
      * Fetch get OBRD Unit Status Report
      */
      void getStatusReport(OBRDUnitStatusReport &report) const;

    protected:

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

    private:

      /**
      * Default constructor (disabled)
      */
      OBRDMessageInUnitStatus();

      /**
      * Parses the extracted data
      *
      * @param[in] messageData  The incoming message data to be parsed
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseMessageData(OBRDDataPacket* const messageData);

      /**
      * OBRD Unit Status Message
      */
      OBRDUnitStatusReport obrdUnitStatusReport;

      /**
      * Maximum valid brake pressure
      */
      static const uint8_t maxBrakePressure = 125U;
    };
  }
}

#endif
