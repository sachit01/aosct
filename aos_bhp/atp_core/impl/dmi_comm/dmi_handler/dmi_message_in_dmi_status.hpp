#ifndef DMIMessageInDMIStatus_hpp
#define DMIMessageInDMIStatus_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The parsers for incoming DMI messages are inherited from AbstractDMIMessageIn.
*  One parser per message-type.
*  Each parser is responsible for the validation and publishing of the incoming DMI data.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-13    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * DMIMessageInDMIStatus is a "placeholder" in the parsers representing a DMI Status messageType
    */
    class DMIMessageInDMIStatus : public AbstractDMIMessageIn
    {
    public:

      /**
      * Constructor for a parser of a DMI Status messageType message
      */
      DMIMessageInDMIStatus();

      /**
      * Validates the extracted data
      *
      * @return true if the parser is implemented for DMI Status messageType message
      */
      virtual bool validate();

      /**
      * Invalidates the extracted data
      *
      */
      virtual void invalidate();

      /**
      * Logs self->messageData to RU. Assumes that setMessageData() and
      * parseDMIMessageData() have been called successfully.
      *
      * Only logs the message if self->dmiStatusWord has changed since the last call.
      */
      virtual void logToRU() const;

      /**Access-function for any published DMIStatusWord
      *
      * @return DMI Status Word
      */
      uint16_t getDMIStatusWord() const;


    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseDMIMessageData();

      /**
      * DMI status word
      *
      */
      uint16_t dmiStatusWord;

    };
  }
}
#endif
