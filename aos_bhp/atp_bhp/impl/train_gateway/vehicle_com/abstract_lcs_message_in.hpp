#ifndef AbstractLCSMessageIn_hpp
#define AbstractLCSMessageIn_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
*  The parsers for incoming messages are inherited from AbstractLcsMessageIn.
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
* 2016-11-23    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "lcs_message_common.hpp"
#include "emp_message.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace TG
  {
    /**
    * AbstractLCSMessageIn is the base class of parsers for incoming lcs messages
    */
    class AbstractLCSMessageIn
    {
    public:

      /**
      * Alternative constructor for the parser base class
      *
      * @param[in] mType   The messageType supported for this creator
      */
      AbstractLCSMessageIn(const LCSMessageType mType, const uint8_t version, const bool isImplemented);

      /**
      * Virtual Destructor for the parser base class
      */
      virtual ~AbstractLCSMessageIn();

     /**
      * Validates the extracted data
      *
      * @param[in] mData   The incoming messageData to be parsed
      *
      * @return true if data is valid as a result of a valid incoming message
      */
      virtual bool validate(EmpMsg* const mData)=0;

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate()=0;

      /**
      * Logs the given message to RU. Assumes that validate() has been called
      * successfully for this message.
      */
      virtual void logToRU(const EmpMsg* const mData) const;

      /**
      * Get version of message
      *
      * @return Version of message
      *
      */
      uint8_t getVersion() const;

      /**
      * Get the implemented flag for this messageType
      * The messageType will be ignored if the parser is not implemented.
      *
      * @return true if the messageType is implemented
      */
      bool getImplemented() const;


    protected:

      /**
      * Default constructor
      */
      AbstractLCSMessageIn();

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
      * Helper function to trace after validating mode
      *
      * @param[in] validateModeOk True if validating mode is ok
      */
      void traceValidateMode(const bool validateModeOk) const;

      /**
      * Get the data process state
      */
      DataProcessState getDataProcessState() const;

      /**
      * Set the data process state
      */
      void setDataProcessState(const DataProcessState newState);

      /**
      * Get the tracer for this class
      */
      const ATC::TraceInterface& getTracer() const;

    private:

      /**
      * Implemented is true if the creator is supported for this messageType
      * The only reason to set implemented = false is when creating a "place-holder" for NotYetImplemented messageTypes.
      *
      */
      const bool  implemented;

      /**
      * The messageType supported by this parser
      */
      LCSMessageType   messageType;

      /**
      * Keeping track of the status of data-processing
      */
      DataProcessState  dataProcessState;

      /**
      * Trace interface
      */
      ATC::TraceInterface *trace;

      /**
      * The message version for a specific message
      */
      const uint8_t messageVersion;
    };
  }
}
#endif
