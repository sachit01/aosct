#ifndef AbstractRadioHandler_hpp
#define AbstractRadioHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2015
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*
*
******************************************************************************/


/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2015-11-13    bhermans    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-04-21    lantback    Implemented corePtr()
* 2016-04-21    lantback    Make abstract constructor protected
* 2016-06-15    akushwah    Radio Handler Implementation
* 2016-09-26    bhermans    Unused ATC::Event variables removed
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vector>
#include "atc_base.hpp"
#include "radio_channel.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace RadioCom
  {
    class AbstractRadioHandler;
    /**
    * Static variable to store the single instance of AbstractRadioHandler
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractRadioHandler* coreRadioHandlerInstancePtr = static_cast<AbstractRadioHandler*>(NULL);

    /** The type of container used for the RadioChannel objects
    */
    typedef std::vector<RadioChannelPtr> RadioChannelVector;

    /** The AbstractRadioHandler implements the core Radio Handler functionality
    *  but the adaptation is implemented in the RadioHandler class
    */
    class AbstractRadioHandler : public ATC::ProcComponent
    {
    public:
      /** Called by the scheduler each execution cycle
      *  run()
      *    calls runIn() for each RadioChannel object
      *    process any incoming messages
      *    creates any immediate response messages
      *    calls runOut() for each RadioChannel object
      *
      */
      virtual void run(void);

      /** Read message
      *
      *  @param[out] msg        The available radio-message(including the TTC ID) from any radio-channel.
      *  @param[out] channelId  Id of the RadioChannel
      *
      *  @return true if message available
      */
      bool readMessage(RadioMessage & msg, uint16_t & channelId);

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractRadioHandler* corePtr();

      /** Get status if any radio channel is connected
      *
      *  @return true if any radio channel is connected
      */
      bool getConnected() const;
      
      /** Get status of a specific radio channel is connected
      *
      *  @return true if a specific radio channel is connected
      */
      bool getConnected(const uint16_t chId) const;

      /**
      * Access-function for getting yard mode counter is expired or not
      *
      *  @return true if yard mode counter is expired
      */
      bool isYardModeTimerExpired() const;

      /**
      * Get the number of TCC connected
      *
      *  @return number of TCC connected
      */
      uint8_t getNumOfTCCConnected();

      /**
      * Get the number of position Messages sent so far
      *
      *  @return number of position Messages sent
      */
      uint32_t getNumPositionMessages() const;

      /**
      * Get reference time at which TCC has lost the connection
      *
      *  @return time  Reference time for TCC timeout
      */
      int64_t getTCCTimeoutVal() const;

      /**
      * Get status of TCC time out
      *
      *  @return true  If TCC has timeout
      */
      bool getTCCTimeoutStatus() const;

    protected:
      /**
      * Constructor
      */
      AbstractRadioHandler();

      /** The container used for the RadioChannel objects
      */
      RadioChannelVector radioChannels;

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /** Handles the radio message for central TCC
      *
      *  @param[in]  radioChannel The radio-channel to be processed
      *  @param[in] radioMessageToPeek The available radio-message(including the TTC ID) from any radio-channel.
      */
      void handleCentral(RadioChannel* const radioChannel, const RadioMessage& radioMessageToPeek) const;
      
      /** Handles the radio messages for regional TCC
      *
      *  @param[in]  radioChannel         The radio-channel to be processed
      *  @param[in]  radioMessageToPeek   The available radio-message(including the TTC ID) from any radio-channel.
      *  @param[out] writeMessageStatus   True if writing to radio channel went OK
      */
      void handleRegion(RadioChannel* const radioChannel, const RadioMessage& radioMessageToPeek, bool& writeMessageStatus);

      /**
      * Index of the Response byte in the Protocol Version message.
      * This is used to update the field to be sent back to TCC.
      */
      static const uint8_t idxResponseInProVerMsg = 1U;

      /**
      * Number of position messages sent so far
      */
      uint32_t numPositionMessages;

      /**
      * current counter
      */
      uint16_t yardModeCounter;

      /**
      *Last time at which TCC has lost connection
      */
      int64_t lastRefTimeTCCTimeout;

      /**
      * TCC timeout Status
      */
      bool tccTimeoutStatus;

      /**
      * Flag for Yard Mode counter expired or not
      */
      bool isYardModeCounterExpired;
    };
  }
}

#endif
