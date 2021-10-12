#ifndef RadioMessageInConfigurationDataBHP_hpp
#define RadioMessageInConfigurationDataBHP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2019
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
* 2019-02-27    csundin     Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "radio_message_in_configuration_data.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * RadioMessageInConfigurationDataBHP is a parser for the incoming Configuration Data message
    */
    class RadioMessageInConfigurationDataBHP : public RadioMessageInConfigurationData
    {
    public:
      /**
      * Constructor for RadioMessageInConfigurationDataBHP which is a parser for the incoming Configuration Data message
      */
      RadioMessageInConfigurationDataBHP();

    protected:

      /**
      * Validates the parsed data.
      *
      * @return true if and only if validation succeeded
      */
      virtual bool validateData();
    };
  }
}
#endif
