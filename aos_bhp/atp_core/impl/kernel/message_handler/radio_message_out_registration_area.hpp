#ifndef RadioMessageOutRegistrationArea_hpp
#define RadioMessageOutRegistrationArea_hpp
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
*  The RadioMessageOutRegistrationArea creator is responsible for collecting 
*  registartion-area data (considering the mode etc..)
*  from other components and validation and creation of the outgoing data in network order.
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
#include <vector>

#include "abstract_radio_message_out.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * RadioMessageOutRegistrationArea is a creator for the outgoing RegistrationArea message 
    */
    class RadioMessageOutRegistrationArea : public AbstractRadioMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing RegistrationArea message
      */
      RadioMessageOutRegistrationArea();

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
      * @return 0 if channel >=2 or otherwise channelID
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
      * The collected data used to create the outgoing message
      * Will be cleared each ATP execution-cycle by invalidate()
      */
      uint8_t registrationArea;
    };
  }
}
#endif
