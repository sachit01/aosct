#ifndef DMIMessageInConfirmation_hpp
#define DMIMessageInConfirmation_hpp
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
    * DMIMessageInConfirmation is a "placeholder" in the parsers representing a DMI Confirmation messageType
    */
    class DMIMessageInConfirmation : public AbstractDMIMessageIn
    {
    public:

      /**
      * Constructor for a parser of a DMI Confirmation messageType message
      */
      DMIMessageInConfirmation();

      /**
      * Validates the extracted data
      *
      * @return true if the parser is implemented for DMI Confirmation messageType message
      */
      virtual bool validate();

      /**
      * Invalidates the extracted data
      *
      */
      virtual void invalidate();

      /**
      *  Access-function for any published ConfirmationStatus
      *
      * @return confirmation Status
      */
      Confirmation getConfirmation() const;


    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseDMIMessageData();

      /**
      * Validates the mode
      *
      * @return true if data is valid with respect to mode
      */
      bool validateMode() const;

      /**
      * MMI to ATP Data Confirmation
      */
      Confirmation confirmationData;

    };
  }
}
#endif
