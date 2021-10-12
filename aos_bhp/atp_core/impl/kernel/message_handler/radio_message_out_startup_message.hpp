#ifndef RadioMessageOutStartUpMessage_hpp
#define RadioMessageOutStartUpMessage_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creators for outgoing messages are inherited from AbstractRadioMessageOut.
*  One creator per message-type.
*  The RadioMessageOutStartUpMessage creator is responsible for collecting
*  startup-message data (considering the mode etc..)
*  from other components and validation and creation of the outgoing data in network order.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-26    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vector>

#include "abstract_radio_message_out.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * Defines the storage of the collected information for StartUpMessage
    */
    struct StartUpMessage
    {
      uint32_t    trainCoreStatus;           //!< Train core status (Bit field)
      uint16_t    locomotiveLength;          //!< Locomotive length
      ConfigSource  configSource;            //!< Train configuration entered by driver or automated TIC equipment
      TimsStatus  timsStatus;                //!< TIMS equipment available
      uint8_t     directionAndOrientation;   //!< Locomotive orientation
      BrakeSystemType brakeSystem;           //!< Defines the type of brake system currently active

      bool configConfirmationReceived;       //!< If ConfigConfirmation is received
      ConfigConfirmation configConfirmation; //!< During re-registration, used to confirm or reject the configuration from TCC.
      std::vector<VehicleData> vehicleDataVec; //!< Each connected vehicle in the train included
    };

    /**
    * RadioMessageOutStartUpMessage is a creator for the outgoing StartUpMessage message
    */
    class RadioMessageOutStartUpMessage : public AbstractRadioMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing StartUpMessage message
      */
      RadioMessageOutStartUpMessage();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();

      /**
      * Access function to check if startup message is sent to TCC or not in current cycle
      *
      * @return true if Startup message is send to TCC
      */
      virtual bool isStartUpMessageSent() const;

      /** Get Radio Channel Id
      *
      *  @return the unique Id of Radio Channel.
      */
      virtual uint16_t getChannelId() const;

    protected:

      /**
      * Assemble the additional blocks in adaptation
      *
      * @param[out] buffer  The buffer to be assembled
      */
      virtual void assembleAdditionalBlocks(VFW_Buffer& buffer);

    private:

      /**
      * Invalid StartupMessage event
      */
      const ATC::Event invalidStartupMessage;

      /**
      * Assemble the collected data
      *
      * @return true if data is valid with respect to parsing
      */
      bool assembleMessageData();

      /**
       * The collected data used to create the outgoing message
       * Will be cleared each ATP execution-cycle by invalidate()
       */
      StartUpMessage startUpMessage;

    };
  }
}
#endif
