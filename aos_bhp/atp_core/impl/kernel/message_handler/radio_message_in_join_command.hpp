#ifndef RadioMessageInJoinCommand_hpp
#define RadioMessageInJoinCommand_hpp
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
    * RadioMessageInJoinCommand is a parser for the incoming JoinCommand message
    */
    class RadioMessageInJoinCommand : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInJoinCommand which is a parser for the incoming JoinCommand message
      */
      RadioMessageInJoinCommand();

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
      * Access-function for any published JoinCommand Message
      *
      *  @return true if any JoinCommand message is published
      */
      virtual bool getJoinCommand() const;

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

    };
  }
}
#endif
