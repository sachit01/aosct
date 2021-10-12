#ifndef RadioMessageInEmAlert_hpp
#define RadioMessageInEmAlert_hpp
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
* 2016-03-20    bhermans    Created
* 2016-03-28    bhermans    Split into separate file per parser
* 2016-04-03    bhermans    Removed radio_message_defs.hpp
* 2016-04-03    bhermans    File renamed
* 2016-08-26    marlundg    Updated for ATP-Limited
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
    * RadioMessageInEmAlert is a parser for the incoming EmAlert message
    */
    class RadioMessageInEmAlert : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInEmAlert which is a parser for the incoming EmAlert message
      */
      RadioMessageInEmAlert();

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
      * Access-function for any published EmAlert and its reason 
      *
      *  @param[out] reason   The reason for EmergencyAlert
      *    
      *  @return true if any EmergencyAlert published
      */
      bool getEmAlertReason(EmAlertReasonInfo & reason) const;

    protected:
      /**
      * Function for detailed log of incoming message
      *
      */
      virtual void detailedLog(void) const;

      /**
      * To check the Q_ALERT parameter
      *
      *  @param[in] val Value to be checked
      *
      *  @return True if value is within the unknown reason range
      *
      */
      virtual bool isQ_ALERTUnknown(const EmAlertReasonInfo val) const;


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
      * The storage of EmAlertReason which is a result of a successful parse of the incoming EmergencyAlert message.
      * The reason may be accessed with the access-function during one ATP execution cycle 
      * until invalidated by a call to invalidate().
      */
      EmAlertReasonInfo emAlertReason;

      /**
      * Log Event for emergency alert message with unknown reason received
      */
      const ATC::Event emAlertMsgUnknownReason;
    };
  }
}
#endif
