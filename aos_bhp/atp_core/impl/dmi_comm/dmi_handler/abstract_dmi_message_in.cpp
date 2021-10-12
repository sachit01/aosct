/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (DMI->AOS) has an associated parser class inherited from AbstractDMIMessageIn.
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

#include "abstract_dmi_message_in.hpp"
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
    * AbstractDMIMessageIn constructor
    ******************************************************************************/
    AbstractDMIMessageIn::AbstractDMIMessageIn(DMIMessageType const mType)
    {
      messageType = mType;
      dataValid = false;
      dmiDataInProcessState = DMINoDataIncomingAvailable;
      // Use the TraceInterface from DMI Handler
      trace = AbstractDMIHandler::corePtr()->getTrace();
    }

    /******************************************************************************
    * setMessageData
    ******************************************************************************/
    void AbstractDMIMessageIn::setMessageData(const DMIMessage &dmiMessageData)
    {
      messageData = dmiMessageData;

      // Data is now available to parse and validate
      dmiDataInProcessState = DMIDataIncomingAvailable;
    }

    /******************************************************************************
    * logToRU
    ******************************************************************************/
    void AbstractDMIMessageIn::logToRU() const
    {
      ATC::AbstractLogHandler::corePtr()->logRU(
        ATC::AbstractLogHandler::Ifc_DMI, ATC::AbstractLogHandler::Ifc_In,
        &(messageData.dmiData.msgType), messageData.msgLen);
    }

    /******************************************************************************
    * traceValidateMode
    ******************************************************************************/
    void AbstractDMIMessageIn::traceValidateMode(const bool validateModeOk) const
    {
      if (validateModeOk)
      {
        trace->write(ATC::veryDetailedTrace, "Validating Mode for DMI Message: OK");
      }
      else
      {
        trace->write(ATC::veryDetailedTrace, "Validating Mode for DMI Message: Failed");
      }
    }

    /******************************************************************************
    * traceParseData
    ******************************************************************************/
    void AbstractDMIMessageIn::traceParseData(const bool parseOk) const
    {
      if (parseOk)
      {
        trace->write(ATC::veryDetailedTrace, "Parsing incoming data for DMI Message: OK");
      }
      else
      {
        trace->write(ATC::veryDetailedTrace, "Parsing incoming data for DMI Message: Failed");
      }
    }

    /******************************************************************************
    * writeToLog
    ******************************************************************************/
    void AbstractDMIMessageIn::writeToLog(ATC::LogLevel const level, const char_t* const text,
      const char_t* const filepath, const int32_t line) const
    {
      ATC::AbstractLogHandler::corePtr()->writeToLog(level, text, "DH",filepath, line);
    }

    /******************************************************************************
    * writeToLog
    ******************************************************************************/
    void AbstractDMIMessageIn::writeToLog(ATC::LogLevel const level, const char_t* const text,
      int32_t const val, const char_t* const filepath, const int32_t line) const
    {
      ATC::AbstractLogHandler::corePtr()->writeToLog(level, text, val, "DH",filepath, line);
    }

  }
}
