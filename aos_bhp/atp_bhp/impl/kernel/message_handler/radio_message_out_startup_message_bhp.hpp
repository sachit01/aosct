#ifndef RadioMessageOutStartUpMessageBHP_hpp
#define RadioMessageOutStartUpMessageBHP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2019
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creators for outgoing messages are inherited from AbstractRadioMessageOut.
*  One creator per message-type.
*  The RadioMessageOutStartUpMessage creator is responsible for collecting
*  startup-message data (considering the mode etc..)
*  from other components and validation and creation of the outgoing data in network order.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2019-01-21    csundin     Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "radio_message_out_startup_message.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    /**
    * RadioMessageOutPositionReport is a creator for the outgoing StartUpMessage
    */
    class RadioMessageOutStartUpMessageBHP : public RadioMessageOutStartUpMessage
    {

    public:

      /**
      * Constructor for RadioMessageOutStartUpMessageBHP which is a creator for the
      * outgoing StartUpMessage
      */
      RadioMessageOutStartUpMessageBHP();

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the mode-dependent data from other components
      */
      virtual void collectData();

   protected:

     /**
      * Assemble the blocks related to adaptation in StartUpMessage
      *
      * @param[out] buffer  The buffer to be assembled
      */
      virtual void assembleAdditionalBlocks(VFW_Buffer& buffer);

    private:

      /**
      * Corresponds to the field Q_LOAD_STATUS.
      */
      uint8_t bhpbLoadStatus;
    };
  }
}
#endif
