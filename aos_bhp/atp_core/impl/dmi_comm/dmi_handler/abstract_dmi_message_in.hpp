#ifndef AbstractDMIMessageIn_hpp
#define AbstractDMIMessageIn_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The parsers for incoming messages are inherited from AbstractDMIMessageIn.
*  One parser per message-type.
*  Each parser is responsible for the validation and publishing of the incoming DMI data.
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
#include "dmi_message.hpp"
#include "abstract_log_handler.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * Enum of the data processing state
    */
    enum DMIDataInProcessState
    {
      DMINoDataIncomingAvailable = 0,    //<! No data is available for processing
      DMIDataIncomingAvailable,          //<! Data is available for validation
      DMIDataIncomingValidated           //<! Data is validated and published
    };

    /**
    * AbstractDMIMessageIn is the base class of parsers for incoming DMI messages
    */
    class AbstractDMIMessageIn
    {
    public:

      /**
      * Alternative constructor for the parser base class
      *
      * @param[in] mType   The DMI messageType supported for this creator
      */
      AbstractDMIMessageIn(DMIMessageType const mType);

      /**
       * Validates the extracted data
       *
       * @return true if data is valid as a result of a valid DMI incoming message
       */
      virtual bool validate() = 0;

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate() = 0;

      /**
      * Set the incoming DMI messageData in network byte-order to be parsed
      *
      * @param[in] dmiMessageData   The incoming DMI messageData to be parsed
      *
      */
      void setMessageData(const DMIMessage &dmiMessageData);

      /**
      * Logs self->messageData to RU. Assumes that setMessageData() has been called.
      */
      virtual void logToRU() const;

    private:

      /**
      * Default constructor (disabled)
      */
      AbstractDMIMessageIn();

    protected:
      /**
      * The DMI messageType supported by this parser
      */
      DMIMessageType messageType;

      /**
      * The incoming messageData
      */
      DMIMessage messageData;

      /**
      * dataValid is true if the collected data is checked and the created outgoing DMI message is successfully created
      */
      bool      dataValid;

      /**
      * Keeping track of the status of data-processing
      */
      DMIDataInProcessState  dmiDataInProcessState;

      /**
      * Trace Interface to be used
      */
      ATC::TraceInterface *trace;

      /**
      * Helper function to trace after parsing data
      *
      * @param[in] parseOk   True if parsing data is OK
      */
      void traceParseData(const bool parseOk) const;

      /**
      * Helper function to trace after validating mode
      *
      * @param[in] validateModeOk True if validating mode is OK
      */
      void traceValidateMode(const bool validateModeOk) const;

      /**
      * Writes the Log onto the External interfaces viz.- N-JRU and BDS.
      *
      * @param[in]    level         The Log level for this to be logged.
      * @param[in]    text          The text log to be logged.
      * @param[in]    filepath      Pointer to the path of the file from where this is logged.
      * @param[in]    line          Line number from where this is logged.
      */
      void writeToLog(ATC::LogLevel const level, const char_t* const text, const char_t* const filepath = static_cast<const char_t* const>(NULL), int32_t const line = 0) const;

      /**
      * Writes the Log onto the External interfaces viz.- N-JRU and BDS.
      *
      * @param[in]    level          The Log level for this to be logged.      
      * @param[in]    text           The text log to be logged.
      * @param[in]    val            The integer value to be logged
      * @param[in]    filepath       Pointer to the path of the file from where this is logged.
      * @param[in]    line           Line number from where this is logged.
      */
      void writeToLog(ATC::LogLevel const level, const char_t* const text, int32_t const val, const char_t* const filepath = static_cast<const char_t* const>(NULL), int32_t const line = 0) const;

    private:

    };
  }
}
#endif
