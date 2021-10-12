#ifndef DMIMessageOutPredefinedTxtMessage_hpp
#define DMIMessageOutPredefinedTxtMessage_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creators for outgoing messages are inherited from AbstractDMIMessageOut.
*  This class describes the creators of Predefined text message
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-13   akushwah     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "radio_message_types.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * DMIMessageOutPredefinedTextMessage is a creator for the outgoing predefined text Message DMIMessage
    */
    class DMIMessageOutPredefinedTextMessage : public AbstractDMIMessageOut
    {

    public:
      /**
      * Constructor for a creator of a predefined text message
      */
      DMIMessageOutPredefinedTextMessage();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the outgoing error message(shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and data from event handler components
      */
      virtual void collectData();

    protected:

    private:
      /**
      * Assemble the collected data
      *
      * @return true if data is valid with respect to parsing
      */
      bool assembleDMIMessageData();

      /**
      * Emergency Alert Reason
      */
      Kernel::EmAlertReasonInfo eaReasonToDMI;

    };
  }
}
#endif
