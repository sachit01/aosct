#ifndef RadioMessageInPathBHP_hpp
#define RadioMessageInPathBHP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The parsers for incoming messages are inherited from AbstractRadioMessageIn.
*  One parser per message-type.
*  Each parser is responsible for the validation and publishing of the incoming data.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-03-21    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "radio_message_in_path.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    /**
    * RadioMessageInPathBHP is a parser for the incoming Path message
    */
    class RadioMessageInPathBHP : public RadioMessageInPath
    {

    public:

      /**
      * Constructor for RadioMessageInPathBHP which is a parser for the incoming Path Message
      */
      RadioMessageInPathBHP();

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Access-function for any published static configuration version from Path message
      *
      *  @param[out] configMajorVersion   Major version of static configuration
      *  @param[out] configMinorVersion   Minor version of static configuration
      *
      *  @return true, if any static configuration is published
      */
      bool getStaticConfigurationVersionInPath(uint8_t &configMajorVersion, uint8_t &configMinorVersion) const;

    protected:
      /**
      * Parses the blocks related to adaptation in Movement Authority
      *
      * @param[in] buffer            The buffer to be parsed
      * @param[in] adapBlockType     BlockType for which buffer needs to be parsed 
      *
      * @return true if data is valid with respect to parsing
      */
      virtual bool parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType);

    private:
      /**
      * The storage of BHPBConfigVersion which is a result of a successful parse of the incoming BHPB_CONFIG_VERSION.
      */
      BHPBConfigVersion bhpConfigVersion;

      /**
      * Size of Application Data in BHPB_CONFIG_VERSION block
      *
      */
      static const uint8_t bhpbConfigVersionInPathBlockSize = 2U;
      
    };
  }
}
#endif
