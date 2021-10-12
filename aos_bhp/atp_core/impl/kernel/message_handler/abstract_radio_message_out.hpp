#ifndef AbstractRadioMessageOut_hpp
#define AbstractRadioMessageOut_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
*  The creators for outgoing messages are inherited from AbstractRadioMessageOut.
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
* 2016-03-20    bhermans    Created
* 2016-03-24    bhermans    Separate files for abstract creator and each creator
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
    * AbstractRadioMessageOut is the base class of creators of outgoing radio messages
    */
    class AbstractRadioMessageOut : public AbstractRadioMessageCommon
    {
    public:
      /**
      * Constructor for the creator base class
      */
      AbstractRadioMessageOut();

      /**
      * Virtual Destructor for the parser base class
      */
      virtual ~AbstractRadioMessageOut();

      /**
      * Alternative constructor for the creator base class 
      *
      * @param[in] mType   The messageType supported for this creator
      */
      AbstractRadioMessageOut(const RadioMessageType mType);

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *      
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate()=0;

      /**
      * Invalidates the outgoing message (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate()=0;

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData()=0;

      /**
      * Get the created outgoing messageData in network byte-order
      *
      * @param[out] mData   The messageData created 
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      bool getMessageData(RadioMessageToSend& mData) const;

      /**
      * Get the implemented flag for this messageType
      * The messageType will be ignored if the creator is not implemented.
      *
      * @return true if the messageType is implemented
      */
      bool getImplemented() const;

      /**
      * Get output channel id
      *
      * @return 0 if channel >=2 or otherwise channelID
     */
      virtual uint16_t getChannelId() const;

      /**
      * Check if the data is available to be assembled. 
      * If this is the case, the data must have failed validation during
      * assembly of the message.
      * 
      * @return true if creator is in dataAvailable state
      */
      virtual bool isDataAvailable() const;

    protected:

      /**
      * Helper function to trace after Assembling data
      *
      * @param[in] parseOk   True if parsing data is OK
      */
      void traceAssembleData(const bool parseOk) const;

      /**
      * Collect train status information (B_TRAIN_CORE_STATUS) from different sources
      *
      *  @param[out] trainStatus  The collected train status according to B_TRAIN_CORE_STATUS
      *
      */
      void collectTrainStatusInfo(uint32_t& trainStatus) const;

      /**
      * The messageType supported by this creator
      */
      RadioMessageType messageType;

      /**
      * The outgoing messageData
      */
      RadioMessageToSend messageData;

      /**
      * Destination where to send the created message.
      *
      * destinationId identifies on which RadioChannel the message shall be sent
      *
      */
      uint16_t channelId;

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
      * Traceinterface to be used
      */
      ATC::TraceInterface *trace;

      /**
      * Writes the Log onto the External interfaces viz.- N-JRU and BDS.
      *
      * @param[in]    level         The Log level for this to be logged.
      * @param[in]    text          The text log to be logged.
      * @param[in]    filepath      Pointer to the path of the file from where this is logged.
      * @param[in]    line          Line number from where this is logged.
      */
      void writeToLog(ATC::LogLevel const level, const char_t* const text, const char_t* const filepath = static_cast<const char_t* const>(NULL), int32_t const line = 0) const;

    private:
    };
  }
}
#endif
