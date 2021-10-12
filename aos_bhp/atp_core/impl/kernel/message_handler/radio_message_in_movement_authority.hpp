#ifndef RadioMessageInMovementAuthority_hpp
#define RadioMessageInMovementAuthority_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
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
* 2016-08-26    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_radio_message_in.hpp"
#include "train_setup.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * Defines the storage of the parsed data-information of an arriving MA
    *
    */
    struct MAData
    {
      DepartureWarning               departureWarning;         //!< DepartureWarning corresponds with DEPARTURE_WARNING
      bool                           departureWarningReceived; //!< DepartureWarning received
      bool                           partlyMaReceived;         //!< Partly MA Received
      MaxSearchDist                  maxSearchDist;            //!< MaxSearchDist corresponds with MAX_SEARCH_DIST
      bool                           maxSearchDistReceived;    //!< MaxSearchDist received
      LocationData                   locationData;             //!< LocationData corresponds with LOCATION_DATA
      bool                           locationDataReceived;     //!< LocationData received
      ATOStopPosition                atoStopPosition;          //!< ATOStopPosition corresponds with ATO_STOP_POSITION
      bool                           atoStopPositionReceived;  //!< ATOStopPosition received
      std::vector<TrackData>         trackDataVec;             //!< TrackData corresponds with TRACK_DATA
      std::vector<BaliseData>        baliseDataVec;            //!< BaliseData corresponds with BALISE_DATA
      std::vector<GradientData>      gradientDataVec;          //!< GradientData corresponds with GRADIENT_DATA
      std::vector<CeilingSpeedData>  ceilingSpeedDataVec;      //!< CeilingSpeedData corresponds with CEILING_SPEED_DATA
      std::vector<TrackDataItem>     trackDataItemVec;         //!< TrackDataItem corresponds with TRACK_DATA_ITEM
      LocationBorder                 locationBorders;          //!< LocationBorder corresponds with LOCATION_BORDERS
      bool                           locationBordersReceived;  //!< LocationBorder received
    };

    /**
    * RadioMessageInMovementAuthority is a parser for the incoming EmAlert message
    */
    class RadioMessageInMovementAuthority : public AbstractRadioMessageIn
    {
      /**
      * Event raised if the first balise was not found in MA
      */
      const ATC::Event firstBaliseNotInMA;

      /**
      * Event raised if an invalid MA was received in Registration-Reposition
      */
      const ATC::Event invalidMaInReposition;

      /**
      * Event raised if an invalid MA was received in Registration-Reregistration
      */
      const ATC::Event invalidMaInReregistration;

      /**
      * Event raised at unexpected critical states
      */
      const ATC::Event criticalInvalidStateSafetyHalt;

    public:

      /**
      * Constructor for RadioMessageInMovementAuthority which is a parser for the incoming EmAlert message
      */
      RadioMessageInMovementAuthority();

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
      * Access-function for any published Movement Authority and its associated header information
      *
      *  @param[out] head   The info associated with the header of the Movement Authority
      *
      *  @return true if any MovementAuthority published
      */
      virtual bool getMAHead(MAHead & head) const;

      /**
      * Check if an MA was received in this execution cycle
      *
      *  @param[out] id   The ID of the last received MA
      *  @param[out] replyChannelId Channel the message was received
      *
      *  @return true if any MovementAuthority is received during this cycle
      */
      virtual bool getMAReceived(uint8_t& id, uint16_t& replyChannelId) const;

      /**
      * Get the status of partly MA status
      *
      *  @return true if any PARTLY MA is received
      */
      virtual bool getPartlyMaReceived() const;

      /**
      * Get the max search distance for Balise in Re-registration
      *
      * @param[out] maxDist   Maximum distance to search in re-reg
      *
      *  @return true if max search distance received in MA
      */
      virtual bool getMaxSearchDistInReReg(uint32_t& maxDist) const;

      /**
      * Get the status of if MA is from Scratch
      *
      *  @return true if MA is from scratch is received
      */
      virtual bool isMAFromScratch(void) const;

      /**
      * Get the departure signal
      *
      * @param[out] acousticSignal   type of acoustic signal
      *
      *  @return true, if qSignal is received in MA
      */
      bool getDepartureSignal(AcousticSignal& acousticSignal) const;

    protected:

      /**
      * Parses the additional blocks of Movement Authority
      *
      * @param[in] buffer            The buffer to be parsed
      * @param[in] adapBlockType     BlockType for which buffer needs to be parsed
      *
      * @return true if data is valid with respect to parsing
      */
      virtual bool parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType);

      /**
      * The storage of MAHead which is a result of a successful parse of the incoming MA message.
      * The MAHead may be accessed with the access-function during one ATP execution cycle
      * until invalidated by a call to invalidate().
      */
      MAHead maHead;

      /**
      * The storage of MAData which is a result of a successful parse of the incoming MA message.
      * (The MAData may be accessed with the access-function during one ATP execution cycle
      * until invalidated by a call to invalidate()).
      */
      MAData maData;

      /**
      * Publishes the N_ADHESION data
      */
      virtual void publishAdhesion() const;

      /**
      * Publishes the Track Data Item target data
      */
      virtual void publishTrackDataItemTarget() const;

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

    private:

      /**
      * Helper function
      *
      * @return true if data is valid
      */
      bool publishLocationBorder(const LocationBorder& locationBorder, const LocationData& locData) const;

      /**
      * Helper function
      *
      * @return true if data is valid
      */
      bool validateTrackDataIdleMode();

      /**
      * Helper function
      *
      * @return true if data is valid
      */
      bool validateTrackDataNonIdleMode();

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
      * Validates the track data
      *
      * @return true if data is valid with respect to track data
      */
      bool validateTrackData();

      /**
      * Validates the balise data
      *
      * @return true if data is valid with respect to balise data
      */
      bool validateBaliseData();

      /**
      * Validates the target data
      *
      * @return true if data is valid with respect to target data
      */
      template<typename TargetData>
      bool validateTargetsData(std::vector<TargetData> &targetVector);

      /**
      * Validates the primary target data
      *
      * @return true if data is valid with respect to primary target data
      */
      bool validatePrimaryTargetData();

      /**
      * Checks if maHead.startOfMATrackAndPos matches the earlier end of MA
      *
      * @param[in] endTrack       the track of the earlier end of MA
      * @param[in] endPosition    the position in track of the earlier end of MA
      * @param[in] endTrackLength the length of the track of the earlier end of MA
      * @param[in] trkTrvDir      the travel direction of the earlier end of MA
      *
      * @return true if maHead.startOfMATrackAndPos matches
      */
      bool validateStartOfMA(
        const uint16_t endTrack, const uint32_t endPosition, const uint32_t endTrackLength, const TravelDir trkTrvDir);

      /**
      * Validates the location target data
      *
      * @return true if data is valid with respect to location target data
      */
      bool validateLocationTargetData();

      /**
      * Validates the NotReadyToDrive status
      *
      * @return true if NotReadyToDriveStatus is NOT set
      */
      bool validateNotReadyToDriveStatus();

      /**
      * Validates the Emergency Alert status
      *
      * @return true if Emergency Alert is Inactivated
      */
      bool validateEmergencyAlertStatus();

      /**
      * Validates the Maximum search distance in MA ReReg
      *
      * @return true if Maximum Search distance is set during Reregistration
      */
      bool validateMaxSearchDistance();

      /**
      * Publishes all the extracted data
      *
      * @return true if publish is successful
      */
      bool publishData();

      /**
      * Publishes the track data
      *
      * @return true if publish track data is successful
      */
      bool publishTracks();

      /**
      * Publishes the balises data
      *
      * @return true if publish balise data is successful
      */
      bool publishBalises();

      /**
      * Publishes the primary target data
      *
      * @return true if publish primary target is successful
      */
      bool publishPrimaryTarget();

      /**
      * Publishes the Speed target data
      */
      void publishSpeedTarget() const;

      /**
      * Publishes the Gradient target data
      */
      void publishGradientTarget() const;

      /**
      * Publishes the location target data
      */
      void publishLocationTarget() const;

      /**
      * Publishes the Safety margin
      */
      void publishSafetyMargin();


      /**
      * Get the direction of train movement in track.
      * Forward is from 0 to 1 and Reverse is 1 to 0.
      * @param[in] trainDir Direction of movement of train.
      * @param[in] trackOdoDir Odometer increases/decreases from 0 to 1.
      * @return Forward if train moves from 0 to 1 and Reverse if from 1 to 0.
      */
      TravelDir getTrackTravelDir(const TravelDir trainDir, const OdoDir trackOdoDir) const;

      /**
      * Validates BaliseData with Balise information in DataStorage.
      *
      * @param[in] baliseData         BaliseData to validate
      *
      * @return False is validation of baliseData failed, True if valid
      */
      bool validateBaliseDataInStorage(const BaliseData& baliseData);

      /**
      * Validates train length in location border.
      *
      * @param[in]  locBorder                     object of location border
      * @return true if  validation is OK
      */
      bool validateTrainFootprintAndLocationBorder(const LocationBorder &locBorder);

      /**
      * Validates location border against the storage.
      *
      * @param[out] locationStartTrackFound       Flag for start position
      * @param[out] locationEndTrackFound         Flag for end position
      * @param[out] trackLen                      Train length
      * @param[in]  locBorder                     object of location border
      * @return true                              if location border is valid
      */
      bool validateLocationBorderInStorage(bool &locationStartTrackFound, bool &locationEndTrackFound, uint32_t &trackLen,
        const LocationBorder &locBorder);

      /**
      * Validate the balises in idle condition
      */
      bool validateBaliseDataIdleMode();

      /**
      * An MA is received this execution cycle
      */
      bool  maReceived;

      /**
      * A flag to keep information if previously received MA was a valid PARTLY-MA.
      */
      bool isValidPartlyMAInPrevMa;

      /**
      * A flag to keep information if current received MA is a valid PARTLY-MA.
      */
      bool isValidPartlyMAInCurrMa;

      /**
      * The storage of MAHead which is a result of a successful parse of the incoming PARTLY MA message.
      */
      MAHead maPartlyHead;
    };
  }
}
#endif
