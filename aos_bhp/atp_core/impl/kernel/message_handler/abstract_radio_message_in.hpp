#ifndef AbstractRadioMessageIn_hpp
#define AbstractRadioMessageIn_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
*  The parsers for incoming messages are inherited from AbstractRadioMessageIn.
*  One parser per message-type.
*  Each parser is responsible for the validation and publishing of the incoming data.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-20    bhermans    Created
* 2016-03-28    bhermans    Split into separate files per parser
* 2016-04-03    bhermans    Removed radio_message_defs.hpp
* 2016-04-03    bhermans    File renamed
* 2016-08-26    marlundg    Updated for ATP-Limited
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "radio_message.hpp"
#include "radio_message_types.hpp"
#include "abstract_radio_message_common.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * AbstractRadioMessageIn is the base class of parsers for incoming radio messages
    */
    class AbstractRadioMessageIn : public AbstractRadioMessageCommon
    {
    public:

      /**
      * Virtual Destructor for the parser base class
      */
      virtual ~AbstractRadioMessageIn();

      /**
      * Alternative constructor for the parser base class
      *
      * @param[in] mType   The messageType supported for this creator
      */
      AbstractRadioMessageIn(const RadioMessageType mType);

     /**
      * Validates the extracted data
      *
      * @return true if data is valid as a result of a valid incoming message
      */
      virtual bool validate();

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Set the incoming messageData in network byte-order to be parsed
      *
      * @param[in] mData   The incoming messageData to be parsed
      *
      */
      void setMessageData(const RadioMessageReceived& mData);

      /**
      * Get the implemented flag for this messageType
      * The messageType will be ignored if the parser is not implemented.
      *
      * @return true if the messageType is implemented
      */
      bool getImplemented() const;

      /**
      * Set invalidate reason description
      */
      void setInvalidationReason(const char_t* const description);

      /**
      * Get invalidate reason description
      */
      const char_t* const getInvalidationReason() const;

    private:

      /**
      * Disabled default constructor
      */
      AbstractRadioMessageIn();

    protected:

      /**
      * Validates received number of bytes are the same as parsed number of bytes
      *
      * @param[in] buffer The buffer that has been parsed
      *
      * @return True if size of parsed bytes are equal to size of received bytes
      */
      bool validateSizeOfParsedBytes(const VFW_Buffer* const buffer) const;

      /**
      * Helper function to trace after publishing data
      *
      * @param[in] publishOk   True if publishing data is ok
      */
      void tracePublishData(const bool publishOk) const;

      /**
      * Helper function to trace after parsing data
      *
      * @param[in] parseOk   True if parsing data is ok
      */
      void traceParseData(const bool parseOk) const;

      /**
      * Function for brief log of incoming message
      *
      */
      void briefLog(void) const;

      /**
      * Helper function to trace after validating mode
      *
      * @param[in] validateModeOk True if validating mode is ok
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
      void writeToLog(ATC::LogLevel const level, const char_t* const text, const char_t* const filepath = static_cast<const char_t* const>(NULL), const int32_t line = 0) const;

      /**
      * Helper method to Write both to trace and NJRU/BDS
      *
      * @param[in]    traceLevel    The Trace level for this to be traced.
      * @param[in]    logLevel      The Log level for this to be logged.
      * @param[in]    text          The text log to be logged.
      */
      void traceLog(uint8_t const traceLevel, ATC::LogLevel const logLevel, const char_t* const text) const;

      /**
      * Function to parse adaptation blocks.
      * Should always be called, for consistency and adaptability.
      * If called when there is no adaptation available it will send a safety halt event.
      * Should this not be preferable, override this in the core message to send log event instead. 
      *
      * @param[in]    buffer        The buffer to be parsed
      * @param[in]    adapBlockType The identifier for the block to be read
      * 
      * @return       true if successfully parsed block          
      */
      virtual bool parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType);

      /**
      * The messageType supported by this parser
      */
      static const uint8_t rejectReasonTextLength = 100U;

      /**
      * The messageType supported by this parser
      */
      RadioMessageType      messageType;

      /**
      * The incoming messageData
      */
      RadioMessageReceived  messageData;

      /**
      * Keeping track of the status of data-processing
      */
      DataProcessState  dataProcessState;

      /**
      * Implemented is true if the creator is supported for this messageType
      * The only reason to set implemented = false is when creating a "place-holder" for NotYetImplemented messageTypes.
      *
      */
      bool  implemented;

      /**
      * Traceinterface
      */
      ATC::TraceInterface *trace;

      /**
      * Invalid Incoming message from TCC
      */
      const ATC::Event invalidIncmgMsgTCC;

      /**
      * Invalid data in message from TCC
      */
      const ATC::Event invalidDataInTCCMessage;

      /**
      * Invalid NID_BLOCK_TYPE in message from TCC
      */
      const ATC::Event invalidBlockTypeInTCCMessage;

      /**
      * Invalid NID_BLOCK_TYPE in rejected message from TCC
      */
      const ATC::Event invalidBlockTypeInRejectedTCCMessage;

      /**
      * Invalidation reason
      */
      char_t invalidationReason[rejectReasonTextLength];
    };
  }
}
#endif
