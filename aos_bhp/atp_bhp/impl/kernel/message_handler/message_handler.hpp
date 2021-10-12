#ifndef MessageHandler_hpp
#define MessageHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The declaration of the adaptation of the MessageHandler.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-09    bhermans    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-08-26    marlundg    Updated for ATP-Limited
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_message_handler.hpp"
#include "radio_message_types_bhp.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * The class MessageHandler instantiates the abstract class and implements 
    * the interfaces needed for both inherited classes and component.
    *
    */
    class MessageHandler : public AbstractMessageHandler
    {
    public:
      /**
      * Implements the init function.
      *
      * Create the RadioMessageIn/Out parsers/creating objects and store in container
      *
      * @return Returns true when initialization completed
      */
      virtual bool init(void);

      /** 
      * Singleton instance.
      * Only one instance of this class is allowed.
      *
      * @return the one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static MessageHandler& instance(void);

      /**
      * Get the BHPB Safe for Boarding
      *
      *  @return true, if it is safe for boarding
      */
      bool getBHPBSafeForBoarding(void) const;

      /**
      * Access-function for any published static configuration version from Path message
      *
      *  @param[out] configMajorVersion   Major version of static configuration
      *  @param[out] configMinorVersion   Minor version of static configuration
      *
      *  @return true, if any static configuration is published
      */
      bool getStaticConfigurationVersionInPath(uint8_t &configMajorVersion, uint8_t &configMinorVersion) const;

      /** Get Radio Channel Name
      *
      * @param[in]  radioChannel       Pointer to radio channel name
      * @return true, if Radio Channel Message received
      */
      bool getRadioChannelName(char_t* const radioChannel) const;

      /** Access Function to get Radio Channel Enable(TODO will be removed in DMI implementation)
      *
      * @return true if Radio Channel is Enabled
      */
      bool getRadioChannelEnable() const;

    protected:

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:
      /** 
      * Allocate the message-parsers needed for the adaptation
      * The size of this array must be increased if new messageTypes are introduced
      */
      AbstractRadioMessageIn *parsers[MTypeTCCToAOSMax];

      /** 
      * Allocate the message-creators needed for the adaptation
      * The size of this array must be increased if new messageTypes are introduced
      */
      AbstractRadioMessageOut *creators[MTypeAOSToTCCMax - MTypeDriverInformation];

      /** 
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      MessageHandler();

      /** 
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      MessageHandler(const MessageHandler&);

      /** 
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      MessageHandler& operator = (const MessageHandler&);

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;
    };
  }
}
#endif
