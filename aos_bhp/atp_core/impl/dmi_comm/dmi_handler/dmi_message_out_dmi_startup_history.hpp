#ifndef DMIMessageInDMIStartupHistory_hpp
#define DMIMessageInDMIStartupHistory_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The parsers for outgoing DMI messages are inherited from AbstractDMIMessageOut.
*  One parser per message-type.
*  Each parser is responsible for the validation and publishing of the outgoing DMI data.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-29    adgupta     Created
* 2017-04-10    spandita    Added addStartupMsg() function
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    class DMIMessageOutDMIStartupHistory : public AbstractDMIMessageOut
    {
    public:
      /**
      * Constructor for a creator of a outgoing Startup History DMIMessage
      */
      DMIMessageOutDMIStartupHistory();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the outgoing Startup History DMIMessage(shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();

      /**
      * Add start up message to output messages
      * @param[in]  Message to add the buffer
      */
      void addStartupMsg(const char_t *const str);

    private:

      /** Maximum length of the Startup history text. */
      static const uint16_t maxStartupHistoryText = 150U;

      /**
      * Assemble the collected data
      *
      * @return true if data is valid with respect to parsing
      */
      bool assembleDMIMessageData();

      /** Text buffer to be sent as startup History message. */
      char_t textBuff[maxStartupHistoryText];

      /** Data available flag */
      bool isDataAvailable;

    };
  }
}
#endif
