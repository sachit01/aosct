#ifndef DMIMessageInDMIToATPData_hpp
#define DMIMessageInDMIToATPData_hpp
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
    * DMIMessageInDMIToATPData is a "placeholder" in the parsers representing a DMI ATP Data messageType
    */
    class DMIMessageInDMIToATPData : public AbstractDMIMessageIn
    {
    public:

      /**
      * Constructor for a parser of a DMI ATP Data messageType message
      */
      DMIMessageInDMIToATPData();

      /**
      * Validates the extracted data
      *
      * @return true if the parser is implemented for DMI ATP Data messageType message
      */
      virtual bool validate();

      /**
      * Invalidates the extracted data
      *
      */
      virtual void invalidate();

      /**
      * Access-function for any published DMI Button Status
      *
      * @return DMI Button Status
      */
      DMIButtonStatus getDMIButtonStatus() const;


    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseDMIMessageData();

      /**
      * Validates the actual button
      *      
      * @param[in] button   The button to be validated
      *
      * @return true if button is allowed
      */
      bool validateAllowedButtonStatus(const DMIButtonStatus button) const;

      /**
      * DMI Button Status
      */
      DMIButtonStatus buttonStatus;

      /**
      * Event to log all driver interactions. 
      */
      const ATC::Event driverInteractions;
    };
  }
}
#endif
