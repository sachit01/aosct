/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->DMI) has an associated creator class inherited from AbstractDMIMessageOut.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-13    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"

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
  namespace DMICom
  {
    /******************************************************************************
    * AbstractDMIMessageOut constructor
    ******************************************************************************/
    AbstractDMIMessageOut::AbstractDMIMessageOut(DMIMessageType const mType)
    {
      messageType = mType;
      dataValid = false;
      dmiDataProcessState = DMINoDataAvailable;
      // Use the TraceInterface from DMI Handler
      trace = AbstractDMIHandler::corePtr()->getTrace();
    }

    /******************************************************************************
    * getMessageData
    ******************************************************************************/
    bool AbstractDMIMessageOut::getMessageData(DMIMessage &dmiMessageData) const
    {
      //dmiMessageData = messageData;
      if (DMIDataValidated == dmiDataProcessState)
      {
        dmiMessageData = messageData;
      }
      return (DMIDataValidated == dmiDataProcessState);
    }

    /******************************************************************************
    * logToRU
    ******************************************************************************/
    void AbstractDMIMessageOut::logToRU() const
    {
      ATC::AbstractLogHandler::corePtr()->logRU(
        ATC::AbstractLogHandler::Ifc_DMI, ATC::AbstractLogHandler::Ifc_Out,
        &(messageData.dmiData.msgType), messageData.msgLen);
    }

    /******************************************************************************
    * traceParseData
    ******************************************************************************/
    void AbstractDMIMessageOut::traceParseData(const bool parseOk) const
    {
      if (parseOk)
      {
        trace->write(ATC::veryDetailedTrace, "Parsing outgoing data for DMI Message: OK");
      }
      else
      {
        trace->write(ATC::veryDetailedTrace, "Parsing outgoing data for DMI Message: Failed");
      }
    }

    /******************************************************************************
    * writeToLog
    ******************************************************************************/
    void AbstractDMIMessageOut::writeToLog(ATC::LogLevel const level, const char_t* const text,
      const char_t* const filepath, const int32_t line) const
    {
      ATC::AbstractLogHandler::corePtr()->writeToLog(level, text, "DH",filepath,line);
    }

    /******************************************************************************
    * getMessageType
    ******************************************************************************/
    DMIMessageType AbstractDMIMessageOut::getMessageType() const
    {
      return messageType;
    }

  }
}
