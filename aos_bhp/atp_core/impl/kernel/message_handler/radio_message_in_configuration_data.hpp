#ifndef RadioMessageInConfigurationData_hpp
#define RadioMessageInConfigurationData_hpp
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
    * Defines the storage of the parsed data-information of an arriving Configuration Data
    *
    */
    struct ConfigurationData
    {
      uint8_t                           configDataId;  //!< Identity of this message, used for acknowledging of message.
      std::vector<ConfigDataStruct>     configDataVec; //!< Vector of received CONFIGURATION_DATA
    };

    /**
    * RadioMessageInConfigurationData is a parser for the incoming Configuration Data message
    */
    class RadioMessageInConfigurationData : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInConfigurationData which is a parser for the incoming Configuration Data message
      */
      RadioMessageInConfigurationData();

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
      * Access function for to check if configuration data is accepted
      *
      * @return true if any configData message published
      */
      virtual bool getConfigDataReceived() const;

      /**
      * Access-function for ID of the configuration data message
      *
      *  @param[out] id     ID of the configuration data message
      *  @param[out] replyChannelId   The channel to send the reply to
      *  @return true if any configData message is received.
      */
      virtual bool getConfigDataReceived(uint8_t& id, uint16_t& replyChannelId) const;

    protected:

      /**
      * Validates the parsed data.
      *
      * @return true if and only if validation succeeded
      */
      virtual bool validateData();

      /**
      * Invalid Configuration Data from TCC
      */
      const ATC::Event invalidConfigDataFromTCC;

      /**
      * Log event for ConfigurationData message values replaced with received values
      */
      const ATC::Event configValueReplacedByTCC;

      /**
      * The storage of ConfigurationData message which is a result of a successful parse
      * of the incoming ConfigurationData message.
      */
      ConfigurationData configData;

      /**
      * Function for detailed log of incoming message
      *
      */
      virtual void detailedLog() const;

      /**
      * Function for very detailed log of incoming message
      *
      */
      virtual void veryDetailedLog() const;

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
      bool validateMode();

      /**
      * Max number of ConfigurationDataSize blocks
      */
      static const uint8_t ConfigurationDataSize = 100U;

      /**
      * Configuration Data was received this execution cycle
      */
      bool  configDataReceived;

      /**
      * Set the configuration data message to config component
      *
      */
      void setValidatedConfigData() const;

    };
  }
}
#endif
