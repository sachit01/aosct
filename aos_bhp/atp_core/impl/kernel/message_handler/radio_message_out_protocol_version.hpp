#ifndef RadioMessageOutProtocolVersion_hpp
#define RadioMessageOutProtocolVersion_hpp
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
*  The RadioMessageOutProtocolVersion creator is responsible for collecting 
*  Protocol Version-message data (considering the mode etc..)
*  from other components and validation and creation of the outgoing data in network order.
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
    * Defines the storage of the assembled data-information of an Outgoing ProtocolVersion
    *
    */
    struct ProtocolVersionOut
    {
      ProtocolResponse       outgoingProtocolResponse; //!< Response of check
      ProtocolVersion        outgoingProtocolVersion;  //!< Version of this protocol
    };

    /**
    * RadioMessageOutProtocolVersion is a creator for the outgoing Protocol Version message 
    */
    class RadioMessageOutProtocolVersion : public AbstractRadioMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing ProtocolVersion message
      */
      RadioMessageOutProtocolVersion();

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
      ProtocolVersionOut outgoingProtocolVersionData;

    };
  }
}
#endif
