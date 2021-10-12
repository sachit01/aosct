#ifndef AbstractLCSMessageOut_hpp
#define AbstractLCSMessageOut_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
*  The creators for outgoing messages are inherited from AbstractLCSMessageOut.
*  One parser per message-type.
*  Each parser is responsible for collecting necessary data from other components 
*  and validation and creation of the outgoing data in network order.
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
    * AbstractLCSMessageOut is the base class of creators of outgoing LCS messages
    */
    class AbstractLCSMessageOut
    {
    public:

      /**
      * Default Constructor
      */
      AbstractLCSMessageOut();

      /**
      * Constructor for the creator base class
      *
      * @param[in] mType   The messageType supported for this creator
      */
      AbstractLCSMessageOut(const LCSMessageType mType, const uint8_t version, const bool isImplemented);
      
      /**
      * Virtual Destructor for the parser base class
      */
      virtual ~AbstractLCSMessageOut();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      * 
      * @param[in]  mData   Buffer to be used for validated message in network order
      * @param[out] length  The length of the created output data
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate(EmpMsg* const mData, uint16_t& length) = 0;

      /**
      * Invalidates the outgoing message (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate()=0;

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData()=0;

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
      * Get the message type
      *
      * @return message type of outgoing message
      *
      */
      LCSMessageType getMessageType() const;

    protected:

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
      ATC::TraceInterface& getTracer();

      /**
      * Helper function to trace after assembling data
      *
      * @param[in] assembleOk   True if assembling data is ok
      */
      void traceAssembleData(const bool assembleOk) const;

    private:

      /**
      * Implemented is true if the creator is supported for this messageType
      * The only reason to set implemented = false is when creating a "place-holder" for NotYetImplemented messageTypes.
      *
      */
      const bool implemented;

      /**
      * The messageType supported by this creator
      */
      LCSMessageType messageType;

      /**
      * Keeping track of the status of data-processing
      */
      DataProcessState  dataProcessState;

      /**
      * Trace interface to be used
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
