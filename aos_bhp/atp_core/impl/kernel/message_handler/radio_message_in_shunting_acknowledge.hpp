#ifndef RadioMessageInShuntingAcknowledge_hpp
#define RadioMessageInShuntingAcknowledge_hpp
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
    * RadioMessageInShuntingAcknowledge is a parser for the incoming ShuntingAcknowledge message
    */
    class RadioMessageInShuntingAcknowledge : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInShuntingAcknowledge which is a parser for the incoming ShuntingAcknowledge message
      */
      RadioMessageInShuntingAcknowledge();

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
      * Access-function for any published ShuntingAcknowledge
      *
      *  @param[out] shuntingAck   The published Shunting acknowledge
      *
      *  @return true if any ShuntingAcknowledge message is published
      */
      virtual bool getShuntingAcknowledge(ShuntingAcknowledge &shuntingAck) const;

    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseMessageData();

      /**
      * Validates the mode
      *
      * @return true if data is valid with respect to mode
      */
      bool validateMode() const;

      /**
      * The storage of ShuntingAcknowledge which is a result of a successful parse of the incoming
      * ShuntingAcknowledge message.
      */
      ShuntingAcknowledge shuntingAcknowledgeData;

    };
  }
}
#endif
