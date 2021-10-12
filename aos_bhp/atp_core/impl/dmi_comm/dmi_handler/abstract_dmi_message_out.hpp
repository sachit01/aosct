#ifndef AbstractDMIMessageOut_hpp
#define AbstractDMIMessageOut_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creators for outgoing messages are inherited from AbstractDMIMessageOut.
*  One parser per DMI message-type.
*  Each parser is responsible for collecting necessary data from other components
*  and validation and creation of the DMI outgoing data in network order.
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
#include "trace_interface.hpp"

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
    enum DMIDataProcessState
    {
      DMINoDataAvailable = 0,    //<! No data is available for processing
      DMIDataAvailable,          //<! Data is available for validation
      DMIDataValidated           //<! Data is validated and published
    };

    /**
    * AbstractDMIMessageOut is the base class of creators of outgoing DMI messages
    */
    class AbstractDMIMessageOut
    {
    public:
      /**
      * Alternative constructor for the creator base class
      *
      * @param[in] mType   The messageType supported for this creator
      */
      AbstractDMIMessageOut(DMIMessageType const mType);

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate() = 0;

      /**
      * Invalidates the outgoing message (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate() = 0;

      /**
      * Collects the DMIMessageType- and mode-dependent data from other components
      */
      virtual void collectData() = 0;

      /**
      * Get the created outgoing messageData in network byte-order
      *
      * @param[out] dmiMessageData   The messageData created
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      bool getMessageData(DMIMessage &dmiMessageData) const;
     
      /**
      * Get the message type
      * @return type of message
      */
      DMIMessageType getMessageType() const;

      /**
      * Logs self->messageData to RU. Assumes that collectData() and validate()
      * have been successfully called.
      */
      virtual void logToRU() const;

    private:

      /**
      * Default constructor (disabled)
      */
      AbstractDMIMessageOut();

    protected:

      /**
      * writeToLog
      *
      * Writes the Log onto the External interfaces viz.- N-JRU and BDS.
      *
      * @param[in]    level         The Log level for this to be logged.
      * @param[in]    text          The text log to be logged.
      * @param[in]    filepath      Pointer to the path of the file from where this is logged.
      * @param[in]    line          Line number from where this is logged.
      */
      void writeToLog(ATC::LogLevel const level, const char_t* const text, const char_t* const filepath = static_cast<const char_t* const>(NULL), int32_t const line = 0) const;

      /**
      * Helper function to trace after parsing data
      *
      * @param[in] parseOk   True if parsing data is OK
      */
      void traceParseData(const bool parseOk) const;

      /**
      * The messageType supported by this creator
      */
      DMIMessageType messageType;

      /**
      * The outgoing messageData
      */
      DMIMessage messageData;

      /**
      * dataValid is true if the incoming data is parsed and validated OK
      */
      bool      dataValid;

      /**
      * Keeping track of the status of data-processing
      */
      DMIDataProcessState  dmiDataProcessState;

      /**
      * Trace Interface to be used
      */
      ATC::TraceInterface *trace;

    };
  }
}
#endif
