#ifndef DMIMessageInDMIStartup_hpp
#define DMIMessageInDMIStartup_hpp
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
    * DMIMessageInDMIStartup is a "placeholder" in the parsers representing a DMI StartUp messageType
    */
    class DMIMessageInDMIStartup : public AbstractDMIMessageIn
    {
    public:

      /**
      * Constructor for a parser of a DMI StartUp messageType message
      */
      DMIMessageInDMIStartup();

      /**
      * Validates the extracted data
      *
      * @return true if the parser is implemented for DMI StartUp messageType message
      */
      virtual bool validate();

      /**
      * Invalidates the extracted data
      *
      */
      virtual void invalidate();

      /** 
      * Access-function for any published DMIStartup Status
      *
      * @param[out] Status received in DMIStartup - message
      * @return DMIStartup message received
      */
      bool getDMIStartupStatus(uint16_t &status) const;

      /**
      * Access-function for any published DMI Compatibility Version
      *
      * @return DMI Compatibility Version
      */
      bool getDMICompatibilityVersion(uint8_t &compatibilityVersion) const;

    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseDMIMessageData();

      /**
      * DMI Startup Status
      */
      uint16_t dmiStartupStatus;

      /**
      * DMI Compatibility Version
      */
      uint8_t dmiCompatibilityVersion;


    };
  }
}
#endif
