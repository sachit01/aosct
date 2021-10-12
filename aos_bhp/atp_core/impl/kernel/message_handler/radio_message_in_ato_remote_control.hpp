#ifndef RadioMessageInATORemoteControl_hpp
#define RadioMessageInATORemoteControl_hpp
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
    * Defines the storage of the parsed information of an arriving ATORemoteControl Message
    *
    */
    struct ATORemoteControl
    {
      uint16_t             requestedSpeed;     //!< Requested speed (0 for stop) 
      uint8_t              timeLimit;          //!< Time limit for remote order
      uint8_t              trainDirection;     //!< Direction of movement
      std::vector<uint8_t> loadFinishedVec;    //!< Indicates load process completion
    };

    /**
    * RadioMessageInATORemoteControl is a parser for the incoming ATORemoteControl message
    */
    class RadioMessageInATORemoteControl : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInATORemoteControl which is a parser for the incoming ATORemoteControl message
      */
      RadioMessageInATORemoteControl();

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
      * Publishes all the extracted data
      *
      * @return true if publish is successful
      */
      bool publishData() const;

      /**
      * Max number of LOAD_FINISHED blocks
      */
      static const uint8_t loadFinishedDataSize = 10U;

      /**
      * The storage of ATORemoteControl which is a result of a successful parse of the incoming ATORemoteControl message.
      */
      ATORemoteControl atoRemoteControlData;
    };
  }
}
#endif
