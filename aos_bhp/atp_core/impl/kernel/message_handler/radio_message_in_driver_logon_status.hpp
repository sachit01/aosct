#ifndef RadioMessageInDriverLogonStatus_hpp
#define RadioMessageInDriverLogonStatus_hpp
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
* 2017-04-11    skothiya    Changed for implementation of cabin handling and authorization requirement
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
    * Defines the storage of the parsed information of an arriving DriverLogonStatus
    *
    */
    struct DriverLogonStatus
    {
      LogonStatus       logonStatus; //!< Result of driver logon
      SetTime           setTime; //!< Set clock time in train
    };

    /**
    * RadioMessageInDriverLogonStatus is a parser for the incoming DriverLogonStatus message
    */
    class RadioMessageInDriverLogonStatus : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInDriverLogonStatus which is a parser for the incoming DriverLogonStatus message
      */
      RadioMessageInDriverLogonStatus();

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
      * Access-function for any published DriverLogonStatus and its associated info 
      *
      * @param[out] status   The info associated with the DriverLogon
      *    
      * @return true if any DriverLogonStatus published
      */
      virtual bool getDriverLogonStatus(LogonStatus & status) const;

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
      * The storage of DriverLogonStatus which is a result of a successful parse of the incoming DriverLogonStatus message.
      */
      DriverLogonStatus driverLogonStatus;

      /**
      * Event for wrong Driver Logon message sequence
      */
      const ATC::Event invalidDriverLogonStatusSeq;
    };
  }
}
#endif
