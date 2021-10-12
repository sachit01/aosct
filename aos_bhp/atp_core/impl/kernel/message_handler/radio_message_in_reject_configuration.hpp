#ifndef RadioMessageInRejectConfiguration_hpp
#define RadioMessageInRejectConfiguration_hpp
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
    * RadioMessageInRejectConfiguration is a parser for the incoming RejectConfiguration message
    */
    class RadioMessageInRejectConfiguration : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInRejectConfiguration which is a parser for the incoming RejectConfiguration message
      */
      RadioMessageInRejectConfiguration();

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
      * Access-function for any published RejectConfiguration info
      *
      *  @param[out] rejectConfigurationReason   The info associated with the RejectConfiguration
      *
      *  @return true if any RejectConfiguration published
      */
      virtual bool getRejectConfigurationReason(RejectConfigInfo & rejectConfigurationReason) const;

    protected:
      /**
      * Validating the Q_REJECTCONFIGURATION parameter
      *
      *  @param[in] val Value to be checked
      *
      *  @return True if value is in the unknown reason range
      *
      */
      bool isQ_REJECTCONFIGURATIONUnknown(const RejectConfigInfo val) const;

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
      * The storage of RejectConfigInfo which is a result of a successful parse of the incoming RejectConfiguration message.
      * The reason may be accessed with the access-function during one ATP execution cycle
      * until invalidated by a call to invalidate().
      */
      RejectConfigInfo rejectConfigReason;

      /**
      * Log event for invalid rejection reason of configuration data from sTCC
      */
      const ATC::Event unknownReasonRejectConfigByTCC;
    };
  }
}
#endif
