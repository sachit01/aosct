#ifndef RadioMessageInUnconditionalShortening_hpp
#define RadioMessageInUnconditionalShortening_hpp

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The parsers for incoming unconditional shortening messages are inherited from AbstractRadioMessageIn.
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
* 2018-08-30    spandita    Created

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "radio_message.hpp"
#include "abstract_radio_message_in.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * RadioMessageInUnconditionalShortening is a parsers for the unconditional shortening message
    */
    class RadioMessageInUnconditionalShortening : public AbstractRadioMessageIn
    {
    public:

      /**
      * Constructor for a RadioMessageInUnconditionalShortening which is a parser for the incoming unconditional shortening message
      */
      RadioMessageInUnconditionalShortening();

      /**
      * Validates the extracted data
      *
      * @return true if data is valid as a result of a valid incoming message
      */
      virtual bool validate();

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      *
      */
      virtual void invalidate();

      /**
      * Get the status of emergency alert if set by unconditional shortening message
      *
      * @return true if emergency alert is set by unconditional shortening message
      */
      bool getActiveEmergencyAlert() const;

    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseMessageData();

      /**
      * Validate the incoming data against the ATP modes
      *
      * @return true if data is valid with respect to ATP modes
      */
      bool validateMode() const;

      /**
      * Validate the incoming data against the primary target in storage
      *
      * @return true if data is valid with respect to stored target
      */
      bool validateTargetInStorage() const;

      /**
      * Validate the end position field against the storage in AOS
      *
      *@param[in] trackId   track ID
      *
      * @return true if data is valid
      */
      bool handleTargetsInStorage(const uint16_t trackId) const;

      /**
      * Validate the end position against the brake distance required to stop the train
      *
      * @return true if data is valid
      */
      bool validatePositionInStorage() const;

      /**
      * Publishes all the extracted data
      *
      * @return true if publish is successful
      */
      bool publishData() const;

      /**
      * Object of unconditional shortening message
      */
      UnconditionalShortening uncondShortMsg;

      /**
      * Brake event for discarded unconditional Message, safe brake to stop
      */
      const ATC::Event unCondDiscardedSafeBrakeToStop;

      /**
      * To indicate the emergency alert is required to set by unconditional Shortening message.
      */
      bool activeEmergencyAlert;

    };
  }
}
#endif
