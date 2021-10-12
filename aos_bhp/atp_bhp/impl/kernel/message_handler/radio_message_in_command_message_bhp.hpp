#ifndef RadioMessageInCommandMessageBHP_hpp
#define RadioMessageInCommandMessageBHP_hpp
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

#include "radio_message_in_command_message.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * Defines the storage of the parsed additional BHP information of an arriving CommandMessage
    *
    */
    struct BhpCommandMessage
    {
      bool                   bhpSafeForBoardingReceived; //!< BHPBSafeForBoarding received
      BHPBSafeForBoarding    bhpSafeForBoarding;         //!< BHPBSafeForBoarding corresponds with BHPB_SAFE_FOR_BOARDING
      bool                   bhpRadioChannelReceived;    //!< BHPBRadioChannel received
      BHPBRadioChannel       bhpRadioChannel;            //!< BHPBRadioChannel corresponds with BHPB_RADIO_CHANNEL
    };

    /**
    * RadioMessageInCommandMessageBHP is a parser for the incoming Command Message
    */
    class RadioMessageInCommandMessageBHP : public RadioMessageInCommandMessage
    {

    public:

      /**
      * Constructor for RadioMessageInCommandMessageBHP which is a parser for the incoming Command Message
      */
      RadioMessageInCommandMessageBHP();

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Validates the extracted data
      *
      * @return true if data is valid as a result of a valid incoming message
      */
      virtual bool validate();

      /**
      * Get the Safe for Boarding
      *
      *  @return true, if it is safe for boarding
      */
      bool getSafeForBoarding(void) const;

      /** Get Radio Channel Name
      *
      * @param[in] radioChannel       Pointer to radio channel name
      * @return true, if Radio Channel Message received
      */
      bool getRadioChannelName(char_t* const radioChannel);

      /** Access Function to get Radio Channel Enable(TODO will be removed in DMI implementation)
      *
      * @return true if Radio Channel is Enabled
      */
      bool getRadioChannelEnable() const;
 
    protected:

      /**
      * Parses the blocks related to adaptation in Command Message
      *
      * @param[in] buffer            The buffer to be parsed
      * @param[in] adapBlockType     BlockType for which buffer needs to be parsed 
      *
      * @return true if data is valid with respect to parsing
      */
      virtual bool parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType);

    private:

      /**
      * The storage of BhpCommandMessage which is a result of a successful parse of the incoming CommandMessage message.
      */
      BhpCommandMessage bhpCommandMessage;
      
      /**
      * Size of Application Data in BHPB_SAFE_FOR_BOARDING block
      *
      */
      static const uint8_t bhpbSafeForBoardingBlockSize = 0U;

      /**
      * Size of Application Data in BHPB_RADIO_CHANNEL block
      *
      */
      static const uint8_t bhpbRadioChannelBlockSize = 20U;

      /**
      * Flag to enable the Radio Channel (TODO will be removed in future development of DMI)
      */
      bool isRadioChannelEnable;

      
    };
  }
}
#endif
