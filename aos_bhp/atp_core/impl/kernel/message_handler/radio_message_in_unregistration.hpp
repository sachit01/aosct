#ifndef RadioMessageInUnregistration_hpp
#define RadioMessageInUnregistration_hpp
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
* 2016-08-26    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "radio_message.hpp"
#include "abstract_radio_message_in.hpp"


/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * RadioMessageInUnregistration is a parser for the incoming Unregistration message
    */
    class RadioMessageInUnregistration : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInUnregistration which is a parser for the incoming Unregistration message
      */
      RadioMessageInUnregistration();

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
      * Access-function for any published Unregistration and its info 
      *
      *  @param[out] unregInfo   The info associated with the Unregistration
      *    
      *  @return true if any Unregistration published
      */
      virtual bool getUnregInfo(UnregInfo & unregInfo) const;

    protected:
      /**
      * Check the Q_UNREGISTRATION parameter
      *
      * @param[in] val Value to be check
      *
      * @return True if value is in the unknown reasson range
      *
      */
      virtual bool isQ_UNREGISTRATIONUnknown(const UnregInfo val) const;

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
      * The storage of UnregInfo which is a result of a successful parse of the incoming UnregInfo message.
      * The reason may be accessed with the access-function during one ATP execution cycle 
      * until invalidated by a call to invalidate().
      */
      UnregInfo localUnregInfo;

      /**
      * Log Event for Unregistration message received
      */
      const ATC::Event unRegMsgReceivedLogEvent;

      /**
      * Log Event for Unregistration message with unknown reason received
      */
      const ATC::Event unRegMsgUnknownReason;
     
      /**
      * Safety Halt Event for Unregistration message received
      */
      const ATC::Event unRegMsgSafetyHalt;
    };
  }
}
#endif
