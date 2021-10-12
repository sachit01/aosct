#ifndef RadioMessageOutDriverInformation_hpp
#define RadioMessageOutDriverInformation_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
*  The creators for outgoing messages are inherited from AbstractRadioMessageOut.
*  One creator per message-type.
*  The RadioMessageOutDriverInformation creator is responsible for collecting 
*  driver-information data (considering the mode etc..)
*  from other components and validation and creation of the outgoing data in network order.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-20    bhermans    Created
* 2016-03-24    bhermans    Separate files for each type of creator
* 2016-04-03    bhermans    Moved declaration of storage here
* 2016-04-03    bhermans    File renamed
* 2016-08-26    marlundg    Updated for ATP-Limited
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_radio_message_out.hpp"
#include <vector>

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace Kernel
  {
    /**
    * Defines the storage of the collected information for DriverInformation
    */
    struct DriverInformation
    {
      char_t                        driver[driverIdMaxLength];        //!< Driver Identification, note: no null-termination byte
      char_t                        password[tccPasswordIdMaxLength]; //!< Driver password, note: no null-termination byte
    };

    /**
    * Key for password obfuscation
    */
    static const uint8_t key[tccPasswordIdMaxLength] =
    {
      0x23U, 0x74U, 0x4EU, 0x71U, 0x38U, 0x2FU, 0x11U, 0xFBU, 0xC3U, 0xD9U,
      0x23U, 0x74U, 0x4EU, 0x71U, 0x38U, 0x2FU, 0x11U, 0xFBU, 0xC3U, 0xD9U
    };

    /**
    * RadioMessageOutDriverInformation is a creator for the outgoing DriverInformation message 
    */
    class RadioMessageOutDriverInformation : public AbstractRadioMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing DriverInformation message
      */
      RadioMessageOutDriverInformation();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *      
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();

      /**
      * Get output channel id
      *
      * @return channelID
      */
      virtual uint16_t getChannelId() const;

    private:

      /**
      * Assemble the collected data
      *
      * @return true if data is valid with respect to parsing
      */
      bool assembleMessageData();

      /**
      * Encrypts the driver password
      */
      virtual void encryptPassword(char_t password[tccPasswordIdMaxLength]);

      /**
      * The collected data used to create the outgoing message
      * Will be cleared each ATP execution-cycle by invalidate()
      */
      bool    driverInfoSent;

     /**
      * The collected data used to create the outgoing message
      * Will be cleared each ATP execution-cycle by invalidate()
      */
      DriverInformation driverInfo;
    };
  }
}
#endif
