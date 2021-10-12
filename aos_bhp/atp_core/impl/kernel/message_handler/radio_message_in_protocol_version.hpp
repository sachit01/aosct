#ifndef RadioMessageInProtocolVersion_hpp
#define RadioMessageInProtocolVersion_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
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
* 2017-02-28    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_radio_message_in.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * Defines the storage of the parsed data-information of an arriving ProtocolVersion
    *
    */
    struct ProtocolVersionIn
    {
      ProtocolResponse       incomingProtocolResponse; //!< Response of check
      ProtocolVersion        incomingProtocolVersion;  //!< Version of this protocol
    };

    /**
    * RadioMessageInProtocolVersion is a parser for the incoming ProtocolVersion message
    */
    class RadioMessageInProtocolVersion : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInProtocolVersion which is a parser for the incoming ProtocolVersion message
      */
      RadioMessageInProtocolVersion();

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
      * Access-function for any published incoming ProtocolVersion info
      *
      *  @param[out] protocolVersionFromTCC   The version info associated with the ProtocolVersion
      *  @param[out] protocolVersionRequest  The response associated with the ProtocolVersion
      *
      *  @return true if any Protocol Version From TCC is published
      */
      virtual bool getProtocolVersionFromTCC(ProtocolVersion &protocolVersionFromTCC, 
        ProtocolResponse &protocolVersionRequest) const;

    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseMessageData();

      /**
      * The storage of ProtocolVersion message which is a result of a successful parse
      * of the incoming ProtocolVersion message.
      */
      ProtocolVersionIn incomingProtocolVersionData;

      /**
      * Validates the mode
      *
      * @return true if data is valid with respect to mode
      */
      bool validateMode() const;

      /**
      * Publishes the received data as match/mismatch status
      *
      */
      void publishData() const;
      
      /**
      * Protocol Version mismatch with unrecoverable message received
      */
      const ATC::Event protocolVerUnrecovMismatch;

    };
  }
}
#endif
