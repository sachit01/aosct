#ifndef RadioMessageInTrainSetup_hpp
#define RadioMessageInTrainSetup_hpp
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
* 2016-03-20    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "radio_message.hpp"
#include "abstract_radio_message_in.hpp"
#include "atc_bit_checker.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * RadioMessageInTrainSetup is a parser for the incoming TrainSetup message
    */
    class RadioMessageInTrainSetup : public AbstractRadioMessageIn
    {
    public:

      /**
      * Constructor for RadioMessageInTrainSetup which is a parser for the incoming TrainSetup message
      */
      RadioMessageInTrainSetup();

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
      * Access-function for any published Q_SETUP
      *
      *  @param[out] reason  Reason for Train Setup
      *
      *  @return true if any text message published
      */
      virtual bool getQSetup(TrainSetupReason& reason) const;

      /**
      * Check if a Train Setup was received in this execution cycle
      *
      *  @param[out] id   The ID of the last received Train Setup
      *  @param[out] replyChannelId   The channel ID of the last received Train Setup
      *
      *  @return true if any Train Setup is received during this cycle
      */
      virtual bool getTrainSetupReceived(uint8_t& id, uint16_t& replyChannelId) const;

      /**
      * Return the maximum gradient for a received train setup
      *
      *  @param[out] maxGradient the maximum gradient of train setup
      *
      *  @return true if any Train Setup Data is accepted during this cycle
      */
      virtual bool getTrainSetupMaxGradient(int8_t& maxGradient);

      /**
      * Access-function for getting whether Train Setup message has been rejected by AOS
      *
      *  @return true, if AOS reject the incoming Train Setup message from TCC
      */
      virtual bool isTrainSetupRejectedByAOS() const;


    protected:

      /**
      * Defines the storage of the parsed information of an arriving TrainSetup
      *
      */
      struct RadioMessageTrainSetup
      {
        uint8_t           nidMsg;             //!< Identity of this message, used for acknowledging of the message
        TrainSetupReason  trainSetupReason;   //!< Reason for message
        TrainSetupState   state;              //!< Indicates if TS is permanent or temporary
        uint16_t          maxSpeed;           //!< Maximum permitted train speed
        uint32_t          trainLength;        //!< Total length of train (locomotive and all cars)
        TimsSupStatus     timsSupervision;    //!< Define if TIMS supervision shall be used or not in combination with this train setup
        uint8_t           trainDirection;     //!< Locomotive orientation
        int8_t            maxGradient;        //!< Maximal encountered gradient in system, sent as positive value
        uint16_t          noOfVehicles;       //!< Number of vehicles
        bool              trainNameReceived;  //!< Is TrainName received
        TrainName         trainName;          //!< TrainName corresponds with TRAIN_NAME
        std::vector<VehicleTypeData> vehicleTypeDataVec; //!< Vehicle Type Data corresponds with VEHICLE_TYPE_DATA
        std::vector<VehicleData> vehicleDataVec;         //!< Vehicle Data corresponds with VEHICLE_DATA 

        int32_t    trainDynamicWeightLoaded;                               //!< Dynamic weight loaded
        int32_t    trainDynamicWeightEmpty;                                //!< Dynamic weight empty
        int32_t    locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems];//!< Loco Brake weight loaded for all brake types
        int32_t    locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems]; //!< Loco Brake weight empty for all brake types
        int32_t    carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems];      //!< Cars Brake weight loaded for all brake types
        int32_t    carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems];       //!< Cars Brake weight empty for all brake types

        uint8_t   numberOfBrakeSystemsInUse;                               //!< Number of brake types in use
      };

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
      * Function to calculate brake parameters
      *
      * @param[in] currentVehicleTypeData            Current VehicleType Data to check VehicleType
      * @param[in] numOfVeh                          The number of cars with the current vehicle type
      */
      virtual void calculateBrakeParameters(const VehicleTypeData * const currentVehicleTypeData, const uint16_t numOfVeh);

      /**
      * Publishes all the extracted data
      *
      * @return true if publish is successful
      */
      virtual bool publishData();
   
      /**
      * The storage of TrainSetup which is a result of a successful parse of the incoming TrainSetup message.
      */
      RadioMessageTrainSetup trainSetup;

      /**
      * Parses the additional blocks of Train Setup Message
      *
      * @param[in] buffer            The buffer to be parsed
      * @param[in] adapBlockType     BlockType for which buffer needs to be parsed
      *
      * @return true if data is valid with respect to parsing
      */
      virtual bool parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType);

    private:

      /**
      * If calculated lambda is lower than minimum allowed config value
      */
      const ATC::Event tooLowLambdaReceived;

      /**
      * Max number of vehicle types
      */
      static const uint8_t maxVehicleTypesSize = 10U;

      /**
      * A Train Setup is received this execution cycle
      */
      bool  trainSetupReceived;

      /**
      * Structure to keep track of used vehicle identities
      */
      ATC::BitChecker<uint16_t, 65535U> usedVehicleIdentities;

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseMessageData();

      /**
      * Parses the TRAIN_NAME part of the TrainSetup
      *
      * @param[in] buffer            The buffer to be parsed
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseTypeTrainName(VFW_Buffer& buffer);

      /**
      * Parses the VEHICLE_TYPE_DATA part of the TrainSetup
      *
      * @param[in] buffer            The buffer to be parsed
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseTypeVehicleTypeData(VFW_Buffer& buffer);

      /**
      * Parses the VEHICLE_DATA part of the TrainSetup
      *
      * @param[in] buffer            The buffer to be parsed
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseTypeVehicleData(VFW_Buffer& buffer);

      /**
      * Parses the TRACK_DATA part of the TrainSetup
      *
      * @param[in] buffer            The buffer to be parsed
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseTypeTrackData(VFW_Buffer& buffer);

      /**
      * Validates the mode
      *
      * @return true if data is valid with respect to mode
      */
      bool validateMode();

      /**
      * Validate Vehicle data
      *
      * @return true if data is valid
      */
      bool validateAndCalculateVehicleData();

      /**
      * Validate Vehicle type data 
      *
      * @return true if data is valid
      */
      bool validateVehicleTypeData();

      /**
      * Check if vehicle data block is unique.
      *
      * @param[in]  vehData  The vehicle data to search for 
      *
      * @return true if vehicle block is unique.
      */
      bool isVehicleDataValid(const VehicleData& vehData);

      /**
      * Validates the mode configuration
      *
      * @return true if data is valid with respect to mode configuration
      */
      bool validateModeConfiguration();

      /**
      * Validates the L_TRAIN received
      *
      * @param[in]  lTrain  The length of the train (cm) 
      *
      * @return true if L_TRAIN is valid
      */
      bool validateL_TRAIN(const uint32_t lTrain) const;
    };
  }
}
#endif
