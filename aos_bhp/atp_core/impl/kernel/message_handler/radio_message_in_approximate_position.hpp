#ifndef RadioMessageInApproximatePosition_hpp
#define RadioMessageInApproximatePosition_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
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
* 2017-02-28    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_radio_message_in.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * RadioMessageInApproximatePosition is a parser for the incoming ApproximatePosition message
    */
    class RadioMessageInApproximatePosition : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInApproximatePosition which is a parser for the incoming ApproximatePosition message
      */
      RadioMessageInApproximatePosition();

      /**
      * Validates the extracted data
      *      
      * @return true if data is valid as a result of a valid incoming message
      */
      virtual bool validate();

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Access-function for any published Approximate position 
      *
      * @return true if any ApproximatePosition is accepted
      */
      virtual bool getApproximatePosition() const;

      /**
      * Check if an Approximate Position message was received in this execution cycle
      *
      *  @param[out] id   The ID of the last received Approximate Position message
      *  @param[out] replyChannelId channel the reply should be sent
      *
      *  @return true if any Approximate Position message is received during this cycle
      */
      virtual bool getApproxPosReceived(uint8_t & id, uint16_t &replyChannelId) const;

      /**
      *  Returns the approximate front train position
      *
      *  @param[out] tnp   Front Track and position 
      *
      *  @return true if any Approximate Position message is received during this cycle
      */
      virtual bool getApproxFrontPos(TrackAndPos& tnp) const;

    protected:

      /**
      * Function for detailed log of incoming message
      *
      */
      virtual void detailedLog(void) const;

      /**
      * Function for very detailed log of incoming message
      *
      */
      virtual void veryDetailedLog(void) const;

      /**
      * Parses the additional blocks of the message
      *
      * @param[in] buffer            The buffer to be parsed
      * @param[in] adapBlockType     BlockType for which buffer needs to be parsed
      *
      * @return true if data is valid with respect to parsing
      */
      virtual bool parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType);

    private:

      /**
      * Defines the storage of the parsed data-information of an arriving Approximate Position
      *
      */
      struct ApproxPos
      {
        uint8_t                     approxPosId;           //!< Identity of this message, used for acknowledging of message
        TrackAndPos                 approxTrackAndpos;     //!< Train front position
        bool                        approxPartlyTrackData; //!< Partly track data received
        std::vector<TrackData>      approxTrackDataVec;    //!< TrackData corresponds with TRACK_DATA
      };

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseMessageData();

      /**
      * Validates the mode
      *
      * @return true if data is valid with respect to mode
      */
      bool validateMode();

      /**
      * Publishes the track data
      *
      * @return true if publish track data is successful
      */
      bool publishTracks();

      /**
      * Validates the data received in ApproximatePosition message
      *
      * @return true if data is valid with respect to track data
      */
      bool validateApproxPosMessage();

      /**
      * The storage of ApproximatePosition message which is a result of a successful parse 
      * of the incoming ApproximatePosition message.
      */
      ApproxPos approxPosData;

      /**
      * An Approximate Position message is received this execution cycle
      */
      bool  approxPosReceived;

      /**
      * flag to indicate whether Partial Approximate Message has been accepted or not previously.
      */
      bool  partialApproxMessageAcceptedEarlier;

     };
  }
}
#endif
