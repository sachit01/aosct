#ifndef RadioMessageInCommandMessage_hpp
#define RadioMessageInCommandMessage_hpp
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
* 2016-03-20    marlundg    Created
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
    * RadioMessageInCommandMessage is a parser for the incoming CommandMessage message
    */
    class RadioMessageInCommandMessage : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInCommandMessage which is a parser for the incoming CommandMessage message
      */
      RadioMessageInCommandMessage();

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
      * Access-function for any published Text Message
      *
      *  @return true if any text message published
      */
      virtual const char_t* getTextMessage() const;

   protected:

      /**
      * Publishes all the extracted data
      *
      * @return true if publish is successful
      */
      virtual bool publishData();

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
      * Update of train name by TCC
      */
      TrainName       trainName;

      /**
      * Train name block received
      */
      bool trainNameReceived;

      /**
      * SetTime Message block
      */
      SetTime         setTime;

      /**
      * SetTime Message block received
      */
      bool setTimeReceived;

      /**
      * Release brake block received
      */
      bool releaseBrakeReceived;

      /**
      * TextMessage block
      */
      TextMessage     textMessage;

      /**
      * TextMessage block received
      */
      bool textMessageReceived;
    };
  }
}
#endif
