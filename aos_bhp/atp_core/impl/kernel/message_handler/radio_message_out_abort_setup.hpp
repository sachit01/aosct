#ifndef RadioMessageOutAbortSetup_hpp
#define RadioMessageOutAbortSetup_hpp
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
*  The RadioMessageOutAbortSetup creator is creator is responsible for collecting 
*  abortSetup data (considering the mode etc..)
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

#include "atp_types.hpp"
#include "abstract_radio_message_out.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * RadioMessageOutAbortSetup is a creator for the outgoing AbortSetup message 
    */
    class RadioMessageOutAbortSetup : public AbstractRadioMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing AbortSetup message
      */
      RadioMessageOutAbortSetup();

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
      * The collected data used to create the outgoing message
      * Will be cleared each ATP execution-cycle by invalidate()
      */
      AbortReason abortSetup;
    };
  }
}
#endif
