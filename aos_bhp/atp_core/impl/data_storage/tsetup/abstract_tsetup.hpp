#ifndef AbstractTSetup_hpp
#define AbstractTSetup_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the behaviour of the abstract class to handle train-setup
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-14    bhermans    Created
* 2016-04-26    lantback    Use ATC::ProcComponent, init to return bool, corePtr()
* 2016-05-05    bhermans    Design continued
* 2016-08-13    saprasad    Added event name,function prototypes for core part
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-19    arastogi    Removed include Abstract_mode_control.hpp
*                           Added console call.
* 2016-10-18    arastogi    Added function getLocovsTrainOrientation
* 2017-03-29    skothiya    Modified the prototype of getTrainName for AOS 689
* 2017-04=25    skothiya    Modified temporary train setup to preliminary train setup
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "train_setup.hpp"
#include "vehicle_setup.hpp"
#include "abstract_event_handler.hpp"
#include "brakeability.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace DS
  {
    /**
    * The class AbstractTSetup implements the interface defined by the ProcComponent class.
    *
    */
    class AbstractTSetup : public ATC::ProcComponent
    {
    public:
      /**
      * Implements the virtual init function.
      *
      */
      virtual bool init(void);

      /**
      * Implements the virtual run function.
      *
      */
      virtual void run(void);

      /**
      * Interface to call different level of Console Command
      *
      * @param[in] argc  Number of arguments in the argument array argv
      * @param[in] argv  Arguments array
      *
      * @return true if the Call is successful.
      */
      virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

      /***************************************/
      /* Interface functions for Train Setup */
      /***************************************/
      /** Read train setup
      *
      * @return       Train Setup
      */
      const TrainSetup* const getTrainSetup() const;

      /** Write train setup
      *
      * @param[in]    trainSetup New train configuration to store
      * @return       true if successful, false if rejected
      */
      bool setTrainSetup(const TrainSetup &trainSetup);

      /** Get train name
      *
      * @param[out]   trainName Name of train, buffer should be at least size trainNameMaxLength+1
      * @return       true if successful, false if train name is not valid
      */
      bool getTrainName(char_t* const trainName) const;

      /** Set train name
      *
      * @param[in]    trainName Pointer to location which will be filled with Train Name
      * @return       true if successful, false if failed to write train-name
      */
      bool setTrainName(const char_t* const trainName);

      /** To Check Train Name changed
      *
      * @return true or false based on train name changed/unchanged
      */
      bool trainNameChanged(void) const;

      /** To set Train Name changed status
       *
       *   @param[in] statusChange true or false based on train name changed/unchanged
       *
       */
      void setTrainNameChanged(const bool statusChange);

      /** Read vehicle setup
       *
       * @param[in]    vehicleIndex Index to vehicle to retrieve data from
       * @param[out]   vehicleSetup Vehicle configuration retrieved
       * @return       true if the valid vehicle setup retrieved from the vehicle setup, false if vehicle setup is not valid
       */
      bool getVehicleSetup(const uint16_t vehicleIndex, VehicleSetup &vehicleSetup) const;

      /** Write vehicle setup
      *
      * @param[in]    vehicleIndex Index to vehicle to write data to
      * @param[in]    vehicleSetup Vehicle configuration to write
      * @return       true if successful, false if write rejected
      */
      bool setVehicleSetup(const uint16_t vehicleIndex, const VehicleSetup &vehicleSetup);

      /*************************************************/
      /* Interface functions for Temporary Train Setup */
      /*************************************************/
      /** Get preliminary train setup
      *
      * @param[out]   trainSetup Preliminary train configuration retrieved
      * @return       true if successful, false if preliminary trainSetup data is not valid
      */
      bool getPreliminaryTrainSetup(TrainSetup &trainSetup) const;

      /** Set orientation in Preliminary train setup only when accepting the automatic configuration from DMI
      *
      * @param[in]    orientation new orienatation train configuration to store in Preliminary TSetup
      * @return       true if successful, false if rejected
      */
      bool setOrientationinPreliminaryTSetup(const uint8_t orientation) ;

      /** Set Preliminary train setup
      *
      * @param[in]    trainSetup new preliminary train configuration to store
      * @return       true if successful, false if rejected
      */
      bool setPreliminaryTrainSetup(const TrainSetup &trainSetup);

      /** Get preliminary vehicle setup
      *
      * @param[in]    vehicleIndex Index to vehicle to retrieve data from
      * @param[out]   vehicleSetup Vehicle configuration retrieved
      * @return       true if successful, false if vehicle setup is not valid
      */
      bool getPreliminaryVehicleSetup(const uint16_t vehicleIndex, VehicleSetup &vehicleSetup) const;

      /** Set preliminary vehicle setup
      *
      * @param[in]    vehicleIndex Index to vehicle to write data to
      * @param[in]    vehicleSetup Temporary vehicle configuration to write
      * @return       true if successful, false if write rejected
      */
      bool setPreliminaryVehicleSetup(const uint16_t vehicleIndex, const VehicleSetup &vehicleSetup);

      /**
      *  Get the locomotive vs Train orientation
      *
      * @return orientation of Locomotive with respective of train
      *  that is which side(A or B) will be front facing
      */
      LocoVsTrainDir getLocovsTrainOrientation(void) const;

      /**
      * Remove train setup
      */
      virtual void removeTrainSetup();

      /**
      *  Get status of train setup
      * @return true if valid train setup exist
      */
      bool isTrainSetupValid() const;

      /**
      * Validate the brake data
      *
      * @param[in]  trainDynamicWeightLoaded dynamic weight for loaded locomotive plus cars.
      * @param[in]  trainDynamicWeightEmpty dynamic weight for empty locomotive plus cars.
      * @param[in]  locomotiveBrakeWeightLoadedBrakeSystem Array for the three loaded brake weights for the locomotive
      * @param[in]  locomotiveBrakeWeightEmptyBrakeSystem Array for the three loaded brake weights for the locomotive
      * @param[in]  carsBrakeWeightLoadedBrakeSystem Array for the three loaded brake weights for the cars
      * @param[in]  carsBrakeWeightEmptyBrakeSystem Array for the three loaded brake weights for the cars
      * @return true if the calculated brake lambda is within the limit
      */
      bool validateLambda(
        const int32_t    trainDynamicWeightLoaded,
        const int32_t    trainDynamicWeightEmpty,
        const int32_t    locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems]) const;

      /**
      * Checks if the given parameters would lead to equal or better brake ability than the stored parameters.
      *
      * @param[in]  trainDynamicWeightLoaded dynamic weight for loaded locomotive plus cars.
      * @param[in]  trainDynamicWeightEmpty dynamic weight for empty locomotive plus cars.
      * @param[in]  locomotiveBrakeWeightLoadedBrakeSystem Array for the three loaded brake weights for the locomotive
      * @param[in]  locomotiveBrakeWeightEmptyBrakeSystem Array for the three loaded brake weights for the locomotive
      * @param[in]  carsBrakeWeightLoadedBrakeSystem Array for the three loaded brake weights for the cars
      * @param[in]  carsBrakeWeightEmptyBrakeSystem Array for the three loaded brake weights for the cars
      * @return true if the given parameters would give equal or better brake ability
      */
      bool isBrakeDataEqualOrBetter(
        const int32_t    trainDynamicWeightLoaded,
        const int32_t    trainDynamicWeightEmpty,
        const int32_t    locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems]) const;

      /**
      * Set the brake data
      *
      * @param[in]  trainDynamicWeightLoaded Dynamic weight for loaded locomotive plus cars.
      * @param[in]  trainDynamicWeightEmpty Dynamic weight for empty locomotive plus cars.
      * @param[in]  locomotiveBrakeWeightLoadedBrakeSystem Array for the three loaded brake weights for the locomotive
      * @param[in]  locomotiveBrakeWeightEmptyBrakeSystem Array for the three loaded brake weights for the locomotive
      * @param[in]  carsBrakeWeightLoadedBrakeSystem Array for the three loaded brake weights for the cars
      * @param[in]  carsBrakeWeightEmptyBrakeSystem Array for the three loaded brake weights for the cars
      */
      virtual void setBrakeData(
        const int32_t    trainDynamicWeightLoaded,
        const int32_t    trainDynamicWeightEmpty,
        const int32_t    locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems]);

      /**
      * Get the most restrictive brakeability between two speed segments speed
      *
      * @param[in]  v1 speed segment 1 (cm/s) for the brakeability.
      * @param[in]  v2 speed segment 2 (cm/s) for the brakeability.
      * @return The most restrictive brakeability for the provided speed segment (cm/s2)
      */
      int32_t getWorstBrakeabilityInRange(const uint32_t v1, const uint32_t v2) const;

      /**
      * Get the brakeability for a given speed
      *
      * @param[in]  v speed segment (cm/s) for the brakeability.
      * @return The brakeability for the provided speed segment (cm/s2)
      */
      int32_t getBrakeability(const uint32_t v) const;

      /**
      * Get service brake response time
      *
      * @return The service brake response time (in 0.1 seconds) for the current train
      */
      uint32_t getServiceBrakeResponseTime() const;

      /**
      * Get emergency brake response time
      *
      * @return The emergency brake response time (in 0.1 seconds) for the current train
      */
      uint32_t getEmergencyBrakeResponseTime() const;
      
      /**
      * Get the brake system in use
      *
      * @return The brake system in use
      */
      BrakeSystemType getBrakeSystemInUse() const;

      /** Get TrainLoad Status
      *
      * @return whether Train is Loaded or Empty
      */
      TrainLoaded getTrainLoadStatus() const;

      /** Set the current train load status
      *
      * @param[in]    loadStatus M_LOADED parameter which will be filled when MA from scratch is received
      */
      void setTrainLoadStatus(const TrainLoaded loadStatus);

      /**
      * Returns the train weight, considering whether the train is loaded or empty
      *
      * @return the train weight
      */
      int32_t getTrainWeight() const;

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractTSetup* corePtr();

      /**
      * Has the brake ability changed in the last cycle.
      *
      * @return true if brake ability has changed. false otherwise.
      */
      bool isBrakeAbilityChanged() const;

      /**
      * Has the brake response time changed in the last cycle.
      *
      * @return true if brake response time has changed. false otherwise.
      */
      bool isBrakeResponseTimeChanged() const;

      /**
      * getMaxConsecutiveCarLength, returns the maximum consecutive cars length
      *
      * @return length(in cm) of the maximum consecutive cars in the train
      */
      virtual uint32_t getMaxConsecutiveCarLength();
 

    protected:

      /**
      * Default Constructor
      */
      AbstractTSetup();

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

      /**
      * Init called only once.
      */
      virtual void initOnce();

      /**
      * Storage of vehicle setup
      */
      class VehicleSetupStorage
      {
      public:
        /**
        * Initializes the cross compare module. Called by the init function.
        */
        void initCrossCompare() const;
        void invalidate();      //!< Set dataValid false and members to default
        VehicleSetupStorage();  //!< default constructor

        bool      dataValid;    //!< true when vehicle setup is valid
        VehicleSetup* data;     //!< Pointer to the data being stored as vehicle setup
      };

      /** Storage of vehicle setup */
      VehicleSetupStorage vehicleSetupStorage[maxVehicleCount];

      /** Preliminary storage of vehicle setup */
      VehicleSetupStorage vehiclePreliminarySetupStorage[maxVehicleCount];

      /** Storage of train setup */
      class TrainSetupStorage
      {
      public:
        void invalidate();      //!< Set dataValid false and members to default
        TrainSetupStorage();    //!< default constructor

        bool dataValid;         //!< true when train setup is valid
        TrainSetup *data;       //!< The data being stored as train setup
        bool changed;           //!< true when train setup is changed
      };

      /** Storage of train setup */
      TrainSetupStorage trainSetupStorage;

      /** Preliminary storage of train setup */
      TrainSetupStorage trainPreliminarySetupStorage;

      /**
      * getBrakeabilityObject, this returns the const brakeability object.
      */
      virtual const Brakeability& getBrakeabilityObject() const = 0;

      /**
      *Get the brake system translated to text.
      *
      *@param[in] brakeSystem New brake system enum to get string for.
      *@param[in] buffer the Brake system string is copied to the buffer
      */
      virtual void getBrakeSystemAsText(const BrakeSystemType brakeSystem, char_t* const buffer) const = 0;

      /**
      *  Orientation of Locomotive with respective of train
      *  that is which side (A or B) will be front facing
      */
      LocoVsTrainDir locoOrientation;

    private:

      /** Storage of train name*/
      class TrainNameStorage
      {
      public:
        void invalidate();      //!< Set dataValid false and members to default

        bool       dataValid;   //!< true when trainName is valid
        char_t     trainName[trainNameMaxLength + 1U]; //!< NUL-terminated name of train 
        bool changed;          //!<true when train name will changed
      };


      /** Check any train set up field is change or not
      *
      * @param[in]  trainSetup Train set up configuration
      * @return     true if successful else false
      */
      bool isAnyTSetUpFieldChange(const TrainSetup& trainSetup) const;

      /** Check any train set up field is out of range or not
      *
      * @param[in]  trainSetup Train set up configuration
      * @return     true if successful else false
      */
      bool isAnyFieldOutOfRange(const TrainSetup& trainSetup) const;

      /**
      * An event to reportevent when mode config is failed
      */
      const ATC::Event eventModConfFailAbstTSetup;
      /**
      * An event to reportevent when out of range
      */
      const ATC::Event eventOutOfRangeAbstTSetup;

      /**
      * An event to reportevent when value is not valid
      */
      const ATC::Event eventStorgeNotValAbstTSetup;

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * counter to check if brake ability has changed.
      */
      uint32_t oldChangeCounter;

      /**
      * old brake response time to check if it has changed.
      */
      uint32_t oldSbResponseTime;

      /** Storage of train name*/
      TrainNameStorage trainNameStorage;

      /** M_LOADED parameter,defines whether Train is loaded or empty */
      TrainLoaded trainLoadedStatus;

      /** The loaded weight of the whole train */
      int32_t trainWeightLoaded;

      /** The unloaded weight of the whole train */
      int32_t trainWeightEmpty;

    };

    /**
    * Static variable to store the single instance of AbstractTSetup
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarante that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractTSetup* coreTSetupInstancePtr = static_cast<AbstractTSetup*>(NULL);
  }
}

#endif
