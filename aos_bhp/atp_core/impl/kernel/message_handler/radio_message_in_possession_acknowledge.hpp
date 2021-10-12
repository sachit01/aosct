#ifndef RadioMessageInPossessionAcknowledge_hpp
#define RadioMessageInPossessionAcknowledge_hpp
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
    * RadioMessageInPossessionAcknowledge is a parser for the incoming PossessionAcknowledge message
    */
    class RadioMessageInPossessionAcknowledge : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInPossessionAcknowledge which is a parser for the incoming PossessionAcknowledge message
      */
      RadioMessageInPossessionAcknowledge();

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
      * Access-function for any published PossessionAcknowledge
      *
      * @return PossessionAcknowledge if any PossessionAcknowledge message is published, NULL otherwise
      */
      virtual const PossessionAcknowledge* getPossessionAcknowledge() const;

    private:

      /**
      * Too many balises in Possession Acknowledge
      */
      const ATC::Event tooManyBalisesInPossessionAck;

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
      * Publish the data
      *
      * @return true if data is published
      */

      bool publishData();

      /**
      * The storage of PossessionAcknowledge which is a result of a successful parse of the incoming
      * PossessionAcknowledge message.
      */
      PossessionAcknowledge possessionAcknowledgeData;
    };
  }
}
#endif
