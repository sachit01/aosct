#ifndef AbstractOdometry_hpp
#define AbstractOdometry_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Abstract Odometry is the core component responsible for interfacing with
*  Common Odometry (COD). Its job is to configure the COD and get the
*  measurement data. It will then process the data into AOS readable format
*  and provide interfaces for other components to read it.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-01    arastogi      Created
* 2016-06-22    akushwah      Odometry Implementation
* 2016-07-01    akushwah      Incorporated Review Comments
* 2016-09-19    arastogi      Renamed getVelocity function to getSpeed. Returns uint16 now.
*                             Added console call.
* 2016-10-10    saprasad      Incorporated Integration fixes,lint fixes.
* 2017-04-11    skothiya      Updated for isTrainStandStill function implementation
* 2017-05-24    skothiya      Updated for slip and slide requirement implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "event.hpp"
#include "cross_compare_output_channel.hpp"
#include <vfw_identity.h>
#include <vfw_macro.h>
#include <vfw_sync.h>

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Pos
  {
    /**
    * Odometer configuration state
    */
    enum OdometerConfigState
    {
      SendStaticConfig = 1U,        //!< Sent Static Config
      WaitForStaticConfigResponse,  //!< Static Config has been sent
      SendDynamicConfig,            //!< Static Config Response has been received
      WaitForDynamicConfigResponse, //!< Dynamic Config has been sent
      ConfigComplete,               //!< Dynamic Config Response has been received
      ConfigFailed                  //!< Correct Config Response has not been received
    };

    /**
    * Defines the bits for Slip/Slide
    *
    * See 3NSS012264D0033
    * Bit 0 Axle # (tachometer #) is slipping as detected by high
    *   acceleration
    * Bit 1 Axle # (tachometer #) is sliding as detected by high
    *   deceleration
    * Bit 2 Axle # (tachometer #) is over-speeding as detected by
    *   high speed difference against Doppler radar
    *   (internally in the odometer Bit 0-1 overrides this sensor
    *   state)
    * Bit 3 Axle # (tachometer #) is under-speeding as detected by
    *   high speed difference against Doppler radar
    *   (internally in the odometer Bit 0-1 overrides this sensor
    *   state)
    * Bit 4 Axle # (tachometer #) is under-speeding as detected by
    *   high speed difference against the other axle
    *   (tachometer) (internally in the odometer Bit 0-3 overrides
    *   this sensor state)
    */
    static const uint8_t slipStatus = 0x05U;        // Bits: 0 and 2 gives slide status. Thus value = 0x05
    static const uint8_t slideStatus = 0x1AU;       // Bits: 1, 3 and 4 gives slide status. Thus value = 0x1A
    static const uint8_t slideDueToDoppler = 0x04U; //Bt 2 is for over speeding as detected by high speed difference against Doppler radar
    static const uint32_t minAcceptableWheelSizebyCOD = 500U;
    /** 1: Interface version supported
    */
    static const uint8_t interfaceVerSupported = 1U;

    /** 1: Full Service
    */
    static const uint8_t fullService = 1U;

    /** 1: Direction known
    */
    static const uint8_t directionKnown = 1U;

    /** Max Pulse Counter Frequency (1=4kHz and 2=10kHz)
    */
    static const uint8_t maxPulseCounterFreq = 2U;

    /** Number of Odometer cycles between Odometer Measurement Data Telegrams sent on VFW channel
    *   SIL/HIL will always assume 2 regardless of this setting, but target needs a lower value to get more frequent updates from COD
    */
    static const uint8_t noOfOdoCycles = 1U;

    /** Travel Direction is positive
    */
    static const uint8_t positiveDir = 1U;

    /** Not Used in the project
    */
    static const uint8_t notUsed = 0U;

    /** Not Used in the project
    */
    static const uint16_t notUsedINT = 0U;

    /** brake Level Not Supplied by the user application
    */
    static const uint8_t brakeLevNotSupplied = 255U;

    /** OSU Power supply
    */
    static const uint8_t noOSUPowerSupply = 0U;
    static const uint8_t osuPowerSupplyTacho1AndTacho2 = 0x03U;

    /** Positive Direction
    */
    static const int8_t posDir = 1;

    /** Negative Direction
    */
    static const int8_t negDir = -1;

    /** Direction unknown
    */
    static const int8_t unknownDir = 0;

    /**configuration status
    */
    static const uint8_t configNOK = 0U; //!< Configuration Not OK
    static const uint8_t invalidStaticConfigTelRec = 1U; //!< Invalid Static Config Telegram received
    static const uint8_t validStatWaitDyn = 2U; //!< Valid Static Config Telegram received, waiting for Dynamic  config telegram
    static const uint8_t invalidDynConfigTelRec = 3U; //!< Invalid Dynamic Config Telegram received
    static const uint8_t configOK = 4U; //!< Configuration OK
    static const uint8_t unexpectedStatic = 5U; //!< Unexpected Static config telegram received
    static const uint8_t unexpectedDyn = 6U; //!< Unexpected Dynamic config telegram received

    /** Static configuration telegramType value
    */
    static const uint8_t staticConfigTelType = 1U;

    /** Dynamic configuration telegramType value
    */
    static const uint8_t dynConfigTelType = 2U;

    /** configuration Response telegramType value
    */
    static const uint8_t configResTelType = 3U;

    /** Odometer Measurement Data telegramType value
    */
    static const uint8_t odoMeasDataTelType = 4U;

    /** Odometer Measurement Data Q_CONTROL bits not used value
    */
    static const uint16_t odoMeasDataQControlBitsNotUsed = 0x8800U; // Bits 11 and 15 must be 0

    /** Odometer Measurement Data Q_CONTROL bitmask for Speed Sensor failures
    */
    static const uint16_t odoMeasDataQControlSpeedSensorBitmask = 0xFF3FU; // Bits 6 and 7 are 0

    /** Odometer Measurement Data odoTRadarPlausibleIllegalMin
    */
    static const uint8_t odoTRadarPlausibleIllegalMin = 151U; // 151-250 Not used. Illegal values. 

    /** Odometer Measurement Data odoTRadarPlausibleIllegalMax
    */
    static const uint8_t odoTRadarPlausibleIllegalMax = 250U; // 151-250 Not used. Illegal values. 

    /** Odometer Measurement Data SLIP_SLIDE_STATUS bits not used value
    */
    static const uint8_t odoMeasDataSlipSlideStatusBitsNotUsed = 0xE0U; // Bits 5-7

    /** Odometer interface version
    */
    static const uint8_t odoInterfaceVersion = 1U;

    /**
    * Total size of the static config Telegram parameters
    */
    static const uint8_t sizeStaticConfigTelegram = 58U;

    /**
    * Total size of dynamic config Telegram parameters
    */
    static const uint8_t sizeDynamicConfigTelegram = 31U;

    /**
    * Gradient Positive Direction
    */
    static const uint8_t gradUphillDir = 1U;

    /**
    * Gradient Opposite Direction
    */
    static const uint8_t gradDownhillDir = 0U;


    class AbstractOdometry;
    /**
    * Static variable to store the single instance of AbstractOdometry
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractOdometry* coreOdometryInstancePtr = static_cast<AbstractOdometry*>(NULL);
    /**
    * The class AbstractOdometry implements the interface defined by the ComponentBase class.
    *
    */
    class AbstractOdometry : public ATC::ProcComponent
    {
    public:

      /** Structure to store the measurement data read from COD.
      *  It stores the current data
      */
      struct OdoData
      {
        uint16_t qControl;      //!< Used for Maintenance or driver warning
        bool isOdoSafe;         //!< Level of safety
        bool dirError;          //!< Direction of movement is unknown
        uint32_t timestamp;     //!< Time-stamp (milliseconds)
        uint32_t tProdTime;     //!< Time-stamp production (milliseconds)
        int16_t acceleration;   //!< Acceleration (0.01 cm/s2)
        int16_t vNom;           //!< Nominal Speed (1 cm/s)
        int16_t vMax;           //!< Maximum Speed (1 cm/s)
        int16_t vMin;           //!< Minimum Speed (1 cm/s)
        int16_t vMaxFiltered;   //!< Maximum Speed (1 cm/s) filtered over 1 second
        int16_t vMinFiltered;   //!< Minimum Speed (1 cm/s) filtered over 1 second
        int32_t dNom;           //!< Nominal Distance (1 cm)
        int32_t dMax;           //!< Maximum Distance (1 cm)
        int32_t dMin;           //!< Minimum Distance (1 cm)
        bool isSlipping;        //!< Slipping Status
        bool isSliding;         //!< Sliding Status
        TravelDir odoDir;       //!< Odometer Direction 
        TravelDir odoDirOld;    //!< previous Odometer Direction 
      };

      /** Structure that is used to save values used in 
      * odometry calculation to be read by console command 'odoRaw' 
      */
      struct OdoDataRaw
      {
        bool lastValid;             //!< previous Odometer Calculations values Status
        uint8_t slipSlideStatus1;   //!< SlipSlideStatus for Slip Calculations
        uint8_t slipSlideStatus2;   //!< SlipSlideStatus for Slide Calculations
        int16_t vTacho1;            //!< vTacho1 Calculations
        int16_t vTacho2;            //!< vTacho2 Calculations
        int16_t vDoppl;             //!< vDoppler Calculations
        int32_t dTacho1;            //!< dTacho1 Calculations  
        int32_t dTacho2;            //!< dTacho2 Calculations  
        int32_t dDoppl;             //!< dDoppler Calculations
      };

      /** Structure that is used to save status-values
      *   to be read by console command 'odoStat'.
      */
      struct OdoDataStatus
      {
        uint8_t qOdoSafe;           //!< dDoppler Calculations
        uint16_t qControl;          //!< qControl value
      };

      /**
      * Implements the virtual preInit function.
      *
      * Register VFW channels in sync handler.
      */
      virtual void preInit();

      /**
      * Implements the virtual init function.
      */
      virtual bool init();

      /**
      * Implements the virtual run function.
      */
      virtual void run();

      /**
      * Interface to call different level of Console Command
      *
      * @param[in] argc  Number of arguments in the argument array argv
      * @param[in] argv  Arguments array
      *
      * @return true if the Call is successful.
      */
      virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

      /**
      * Returns the front half of the balise window.
      *
      * @return front balise window value in cm
      */
      uint32_t getLocoEndBaliseWindow() const;

      /**
      * Returns the rear half of the balise window.
      *
      * @return rear balise window value in cm
      */
      uint32_t getLastCarBaliseWindow() const;

      /**
      * Get the current travel direction from odometer.
      *
      * @return travel direction reported by odometer
      */
      TravelDir getOdoDirection() const;

      /**
      * Get the current position reading from odometer.
      *
      * The reading from the odometer is converted into the AOS reference.
      *
      * @return odometer position in cm.
      */
      OdoPosition getOdoPosition() const;

      /**
      * Get the current position reading from odometer.
      *
      * The reading from the odometer is converted into the AOS reference.
      *
      * @return odometer time-stamp in ms.
      */
      uint32_t getOdoTimeStamp() const;

      /**
      * Get the current position reading from odometer.
      *
      * The reading from the odometer is converted into the AOS reference but is not corrected using @ref offset.
      *
      * @return odometer position in cm.
      */
      OdoPosition getOdoPositionWithoutOffset() const;

      /**
      * Get the previous position reading from odometer.
      *
      * The reading from the odometer is converted into the AOS reference but is not corrected using @ref offset.
      *
      * @return odometer position in cm.
      */
      OdoPosition getOldOdoPositionWithoutOffset() const;

      /**
      * Get the current velocity.
      *
      * The velocity from COD but in AOS reference
      *
      * @return velocity in cm/s
      */
      uint16_t getSpeed() const;

      /**
      * Get the filtered absolute max speed.
      *
      * The filtered absolute maximum speed from COD.
      * It is referenced to vmax for vNom >=0 and vMin for vNom < 0
      *
      * @return velocity in cm/s
      */
      uint16_t getFilteredMaxSpeed() const;

      /**
      * Get the current acceleration.
      *
      * The acceleration from COD but in AOS reference
      *
      * @return acceleration in cm/s2.
      */
      int16_t getAcceleration() const;

      /**
      * Returns the maximum expected acceleration compensated for the current gradient.
      *
      * @return acceleration in cm/s2.
      */
      uint16_t getMaxAccelComp() const;

      /**
      * Indicates if the train is slipping.
      *
      * @return true if train is slipping, false otherwise
      */
      bool isSlipping() const;

      /**
      * Indicates if the train is sliding.
      *
      * @return true if train is sliding, false otherwise
      */
      bool isSliding() const;

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractOdometry* corePtr();

      /**
      * Start Up and health supervision test
      *
      * @return true if successful
      */
      bool startupAndHealthSupTest() const;

      /**
      * Indicates if the train is standstill.
      *
      * @return true if train has speed 0 for N number of cycle, false otherwise
      */
      bool isTrainStandStill() const;

      /**
      * Gets the current value of flag isSafetyMarginCrossed.
      *
      * @return true if current balise window crossed safety margin 
      */
      bool isSafetyMarginCrossed() const;

      /**
      * Gets the current value of flag isSlipClear.
      *
      * @return true if slip cleared
      */
      bool isSlipStatusClear() const;


      /**
      * Gets the odometer data.
      *
      * @return the odometer data
      */
      const OdoData& getOdometerData() const;

      /**
      * Gets the SDP Version.
      *
      * @param[in] sdpMajorVer    Major SDP SW Version
      * @param[in] sdpMiddleVer   Middle SDP SW Version
      * @param[in] sdpMinorVer    Minor SDP SW Version
      */
      void getSDPVersion(uint8_t &sdpMajorVer, uint8_t &sdpMiddleVer, uint8_t &sdpMinorVer) const;

      /**
      * log COD detailed info
      *
      * @param[in] logNow   Log immediately. Otherwise logging will only happen when a certain distance travelled since last log.         
      */
      void logCODDetails(const bool logNow);

      /**
      * Get the odometer speed sensor failure flag
      * @return true if some kind of odometer speed sensor failure is active, false otherwise.
      */
      bool getOdometerSpeedSensorFailure() const;


      /**
      * Get Tachometer1 Failure Status.
      *
      * @return true if tachometer1 has failed.
      */
      bool getTachometer1Failure() const;


      /**
      * Get Tachometer2 Failure Status.
      *
      * @return true if tachometer2 has failed.
      */
      bool getTachometer2Failure() const;

      /**
      * Get doppler Failure Status.
      *
      * @return true if doppler has failed.
      */
      bool getDopplerFailure() const;


    protected:

       /** Config response from SDP */
      struct ConfigResponse
      {
        uint8_t     telegramType;         //!< 3= config response
        uint8_t     qVersion;             //!< Interface version
        uint8_t     sdpVerMajor;          //!< Major SDP SW Version
        uint8_t     sdpVerMid;            //!< Middle SDP SW Version
        uint8_t     sdpVerMinor;          //!< Minor SDP SW Version
        uint8_t     configStatus;         //!< Configuration Status
        uint8_t     calStatus;            //!< Calibration Status
        uint32_t    tDvTrain;             //!< TimeStamp
        uint32_t    calTachoDistance1;    //!< Measured distance (Tacho 1)
        uint8_t     tachoCalResultStatus1;//!< Calibration result status (Tacho 1)
        uint32_t    calTachoDistance2;    //!< Measured distance (Tacho 2)
        uint8_t     tachoCalResultStatus2;//!< Calibration result status (Tacho 2)
        uint32_t    calDopplerDistance;   //!< Measured Doppler radar distance
        uint8_t     dopplerCalResultStatus;//!< Calibration result status (Doppler radar)
      };

      /** Structure to store the dynamic configuration to be sent */
      class OdoDynamicConfig
      {
      public:
        uint8_t gradCurrent;    //!< Current Value of Gradient(1 percent)
        uint8_t gradType;       //!< Type of gradient value.
        uint8_t gradDir;        //!< Qualifier for gradient slope (0= Downhill, 1= Uphill)
        int32_t gradPrev;       //!< Store Current value to check if Config is changed.

        /**
        * Check if the dynamic configuration parameters have changed.
        *
        * This will fetch the values needed for dynamic configuration from
        * other components. It will update the values if changed and return
        * true. If nothing has changed it will return false.
        *
        * @return true if values have changed, false otherwise.
        */
        bool isConfigChanged();
      };

      /**
      * Constructor
      */
      AbstractOdometry();

    private:
       /**
       * Set the SDP Version.
       *
       *  @param[in] sdpMajorVer  Major SDP SW Version
       *  @param[in] sdpMiddleVer Middle SDP SW Version
       *  @param[in] sdpMinorVer  Major SDP SW Version
       */
      void setSDPVersion(const uint8_t sdpMajorVer, const uint8_t sdpMiddleVer, const uint8_t sdpMinorVer);

      /**
      * Reads measurement telegrams.
      *
      * This reads all available measurement telegrams from COD through the data channel.
      */
      void readMeasurementTelegrams();

      /**
      * Reads a measurement telegram.
      *
      * This reads one measurement telegram from COD through the data channel.
      * It will update the data in odoData.
      *
      * @param[in] messageBuffer  Buffer to read the telegram from
      *
      * @return true if the telegram was read successfully
      */
      bool readMeasurementTelegram(VFW_Buffer& messageBuffer);

      /**
      * Read the configuration response.
      *
      * This reads the configuration response from COD through the configResponseChannel.
      *
      *  @param[in] configResponseValue  Config Response telegram to be Read
      *
      *  @return true if the reading is successful for Config Response telegram
      */
      bool readConfigResponse(ConfigResponse &configResponseValue);

      /**
      * Send the static configuration.
      *
      * This sends the static configuration to COD over the configChannel.
      */
      void sendStaticConfig();

      /**
      * Send the dynamic configuration.
      *
      * This sends the dynamic configuration to COD over the configChannel.
      */
      void sendDynamicConfig();

      /**
      * Updates the dynamic configuration if needed, by sending it to COD.
      * Also handles the config response from COD.
      */
      void updateDynamicConfig();

      /**
      * Resets the attributes used when calculating the balise windows.
      */
      void resetBaliseWindow();

      /**
      * Update the balise windows based on the latest measurements.
      */
      void updateBaliseWindow();

      /**
      * Perform calculations on measured data.
      *
      * This will calculate and update the internal variables based on the latest
      * measurement data.
      */
      void processMeasurementData();

      /**
      * Performs checks to detect stand still.
      */
      void checkTrainStandStill(void);

      /**
      * Performs slip/slide checks based on slip/slide data from COD 
      * and measurements taken at each evaluation checkpoint
      */
      void processSlipSlideCheck(void);

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      void initCrossCompare() const;

      /**
      * Implements the version validation of the SDP
      * @return true if the version is validated
      */
      virtual bool validateSdpVersion(const uint8_t sdpMajorVer, const uint8_t sdpMiddleVer, const uint8_t sdpMinorVer) const;

      /**
      * Nominal odometry position when last COD details were logged
      */
      int32_t lastLogCODDetails;

      /** Number of VFW channels in Odometry component. */
      static const uint8_t numVfwChannelsOdo = 3U;

      /**
      * Maximum Count for Train stand still counter
      * Train will be considered standstill if train has speed 0 for 10 cycle
      */
      static const uint8_t maxCountTrainStandStill = 10U;

      /**
      * array of VFW channel statistics for Odo component
      */
      ATC::ChannelStats chstat[numVfwChannelsOdo];

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /** Configuration failed Error
      */
      const ATC::Event configFailure;

      /** Invalid State Error
      */
      const ATC::Event invalidState;

      /** NO Config Response Received Error
      */
      const ATC::Event noConfigResponse;

      /** NO Measurement Data Telegram Received Error
      */
      const ATC::Event noMeasurementData;

      /** Error found in Measurement Data telegram
      */
      const ATC::Event invalidMeasurementData;

      /** Incorrect measurement safety critical data
      */
      const ATC::Event safetyIssueCodData;

      /** COD version mismatch
      */
      const ATC::Event versionCODMismatch;

      /**
      * Exceeded balise window error
      */
      const ATC::Event exceededSafetyMargin;

      /** Event for odometer speed sensor failure occurred.
      */
      const ATC::Event odometerSpeedSensorFailureOccurred;

      /** Event for odometer speed sensor failure recovered
      */
      const ATC::Event odometerSpeedSensorFailureRecovered;

      /** Event for tachometer1  sensor failure occurred.
      */
      const ATC::Event tachometer1FailureOccured;

      /** Event for tachometer2  sensor failure occurred.
      */
      const ATC::Event tachometer2FailureOccured;

      /** Event for doppler sensor failure occurred.
      */
      const ATC::Event dopplerFailureOccured;

      /** Event for doppler sensor failure needs maintenance.
      */
      const ATC::Event dopplerRadarNeedsMaintenance;

      /** Event for tachometer1 sensor failure recovered
      */
      const ATC::Event tachometer1FailureRecovered;

      /** Event for tachometer2 sensor failure recovered
      */
      const ATC::Event tachometer2FailureRecovered;

      /** Event for doppler sensor failure recovered
      */
      const ATC::Event dopplerFailureRecovered;

      /** Event when Slip is detected
      */
      const ATC::Event slipDetected;

      /** Event when Slide is detected
      */
      const ATC::Event slideDetected;

      /** Event when the difference between nominal speed and max/min speed is too high
      */
      const ATC::Event vNomDiffFailureOccured;

      /**
      * Odometer configuration state
      */
      OdometerConfigState odometerConfigState;

      /**
      * Indicates whether a Dynamic Config telegram has been sent and we're waiting
      * for a Config Response telegram. (Note: Not used during initialization)
      */
      bool odometerConfigSent;

      /**
      * The maximum allowed response time for odometer configuration (ms)
      */
      static const int64_t odometerConfigMaxTime = 1000;

      /**
      * The time when odometer config was sent (reference time, ms)
      */
      int64_t odometerConfigSentTime;

      /** The positive part of the balise window [cm] */
      uint32_t locoEndBaliseWindow;

      /** The negative part of the balise window [cm] */
      uint32_t lastCarBaliseWindow;

      /** The positive part of the raw balise window [cm] */
      uint32_t rawLocoEndBaliseWindow;

      /** The negative part of the raw balise window [cm] */
      uint32_t rawLastCarBaliseWindow;

      /** Traveled distance since last passed balise in cm (based on dNom) */
      OdoPosition dNomAccumulated;

      /** Traveled distance since last passed balise in cm (based on dMax) */
      OdoPosition dMaxAccumulated;

      /** Traveled distance since last passed balise in cm (based on dMin) */
      OdoPosition dMinAccumulated;

      /** The nominal reading of odometer */
      OdoPosition odoNom;

      /** The previous nominal reading of odometer */
      OdoPosition odoNomOld;

      /** Offset to keep track of difference between actual and calculated
      * odometer reading  */
      int32_t offset;

      /** Direction multiplier to convert from raw odometer reading to OdoPosition */
      int8_t dirMultiplier;

      /** Maximum acceleration compensated for gradient (0.01 cm/s2) */
      uint16_t maxAccelComp;

      /** Channel to send configuration to COD */
      Support::CrossCompareOutputChannel crossCompareConfigChannel;

      /** Channel to receive configuration response from COD */
      VFW_SyncChannel syncConfigResponseChannel;

      /** Channel to receive measured data from COD */
      VFW_SyncChannel syncDataChannel;

      /** Measurement data from COD */
      OdoData odoData;

      /** Absolute value of odoData.vNom */
      uint16_t absNomSpeed;

      /** Raw values used for odometry calculation */
      OdoDataRaw odoDataRaw;

      /** Status values used for odometry measurement */
      OdoDataStatus odoDataStatus;

      /** Dynamic configuration data for COD */
      OdoDynamicConfig odoDynamicConfig;

      /**
      * Flag to indicate train is stand still for 10 cycle
      * Train will be considered standstill if train has speed 0 for 10 cycle
      */
      bool isTrainStandStillFlag;

      /**
      * Counter to calculate the train is stand still
      */
      uint8_t trainStandStillCounter;

      /**
      * Counter to calculate if the train is NOT at stand still
      */
      uint8_t trainNotAtStandStillCounter;

      /**
      * Flag to indicate if train crossed safety margin
      */
      bool safetyMarginCrossedFlag;

      /**
      * Flag to indicate if slip status cleared, when an expected balise found
      */
      bool isSlipClear;

      /**
      * Flag to indicate if the train is free rolling.
      */
      bool isFreeRolling;

      /** Previous dNom */
      int32_t dNomOld;

      /** Previous dMax */
      int32_t dMaxOld;

      /** Previous dMin */
      int32_t dMinOld;

      /** Major SDP SW Version */
      uint8_t sdpMajorVersion;

      /** Middle SDP SW Version */
      uint8_t sdpMiddleVersion;

      /** Minor SDP SW Version */
      uint8_t sdpMinorVersion;
      
      /** Odo value which will be used to register with AOS Analyzer */
      int32_t odoValForAnalyser;

      /** Flag indicating that the expected balise was passed */
      bool expectedBalisePassed;

      /**
      * Flag for odometer speed sensor failure
      */
      bool odometerSpeedSensorFailure;

      /**
      * Flag for Tachometer1 failure
      */
      bool tachometer1Failure;

      /**
      * Flag for Tachometer2 failure
      */
      bool tachometer2Failure;

      /**
      * Flag for Doppler radar failure
      */
      bool dopplerFailure;

      /**
      * Caluculate any Odometer failure
      */
      void evaluateOdometerSensorFailure();

      /**
      * Maximum time the averaged vMax/vMin is allowed to be above vNom
      * 20 seconds
      */
      uint32_t vMaxDiffTime;

      /**
      * Maximum time the averaged vMax/vMin is allowed to be above vNom
      * 20 seconds
      */
      uint32_t vMinDiffTime;

      /**
      * Maximum time the averaged vMax/vMin is allowed to be above vNom
      * 20 seconds (200 cycles)
      */
      static const uint8_t vNomDiffTimeMax = 200U;

      /**
      * Maximum cache size for vMax and vMin averages over a second
      */
      static const uint8_t vCacheMaxSize = 10U;

      /**
      * Cache for vMax values to get the average over a second
      */
      int16_t vMax[vCacheMaxSize];

      /**
      * Cache for vMax values to get the average over a second
      */
      int16_t vMin[vCacheMaxSize];

      /**
      * Previous position in the vMax and vMin caches
      */
      uint8_t vPos;
    };
  }
}

#endif
