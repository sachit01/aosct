#ifndef CODSim_hpp
#define CODSim_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class contained the header files for Common Odometer(COD) Simulation.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-28    akushwah    Created
* 2016-05-06    akushwah    Added the corePtr function
* 2016-05-10    akushwah    corrected timeStamp Variable name
* 2016-05-25    akushwah    Implementation of CODSim functionality
* 2016-06-21    akushwah    Corrected the Config Response Channel name
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-23    arastogi    Removed ATC::
* 2016-10-05    saprasad    Fixed Integration issue and  optimize code
* 2016-10-03    arastogi    Changed time-stamp to int64
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_sync.h>
#include "atc_base.hpp"
#include "simulated_movement.hpp"
#include "sim_types.hpp"
#include "event.hpp"
#include "atc_util.hpp"
#include "abstract_console.hpp"
#include "channel_config.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATC
{
  namespace Sim
  {
    class CODSim;
    /**
    * Static variable to store the single instance of CODSim
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * the core class.
    *
    * Note: During construction the variable shall be checked to guarante that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static CODSim* coreCODSimPtr = static_cast<CODSim*>(NULL);

    /**
    * The class CODSim implements the interface defined by the ComponentBase class and IOComponent class.
    *
    */
    class CODSim : public IOComponent
    {
    public:

      /**
      * Implements the virtual preInit function.
      *
      * Register vfw channels in sync handler.
      */
      virtual void preInit(void);

      /**
      * Implements the init function.
      *
      * @return Returns true when initialization completed
      */
      virtual bool init(void);

      /**
      * Implements the runIn function.
      */
      virtual void runIn(void);

      /**
      * Implements the runOut function.
      */
      virtual void runOut(void);

      /**
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      */
      static CODSim& instance(void);

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static CODSim* corePtr(void);

      /**
      * Interface to call different level of Console Command
      *
      * @param[in] argc  Number of arguments in the argument array argv
      * @param[in] argv  Arguments array
      *
      * @return true if the Call is successful.
      */
      virtual bool consoleCall(const uint32_t argc, const ConsoleArguments argv);

    private:

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      //Config response to ATP
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

      /** Extracted simulated movement data along with timestamp and flag for checking the
      *   validity of extracted data
      */
      struct SimulatedMovementData
      {
        int16_t   speed;              //!< Speed (1 cm/s)
        int16_t   acceleration;       //!< Acceleration (0.01 cm/s2)
        uint16_t  nSensorMaxError;    //!< Sensor Max Error (0.01 %)
        uint16_t  nSensorMinError;    //!< Sensor Min Error (0.01 %)
        int64_t  timeStampSimMovData; //!< Timestamp (milliseconds)
        bool      dataValid;          //!< true = Valid, false = Invalid
      };
      /** Extracted Data from SimulatedMovement Message
      */
      SimulatedMovementData simulatedData;

      /** To make lint happy
      */
      typedef double double_t;

      /** Estimated simulated movement data along with timestamp
      */
      struct EstimatedSimulatedMovementData
      {
        int16_t   acceleration;        //!< Acceleration  (1 cm/s2)
        int16_t   nomSpeed;            //!< Nominal Speed (1 cm/s)
        int16_t   maxSpeed;            //!< Maximum Speed (1 cm/s)
        int16_t   minSpeed;            //!< Minimum Speed (1 cm/s)
        int32_t   nomDistance;         //!< Nominal Distance (1 cm)
        int32_t   maxDistance;         //!< Maximum Distance (1 cm)
        int32_t   minDistance;         //!< Minimum Distance (1 cm)
        int64_t   prevTimeStamp;       //!< Timestamp (milliseconds)
        double_t  floatNomDistance;    //!< Nominal Distance (1 cm)
        double_t  floatMinDistance;    //!< Minimum Distance (1 cm)
        double_t  floatMaxDistance;    //!< Maximum Distance (1 cm)
      };

      /** Estimated Data from SimulatedMovement Message
      */
      EstimatedSimulatedMovementData estimatedSimulatedData;

      /** Controls the simulation of slip.
      */
      bool simulateSlip;

      /** Controls the simulation of slide.
      */
      bool simulateSlide;

      /**Buffer for the SimulatedMovement telegram
      */
      uint8_t simMovementTelegram[speedSimChannelTelSize];

      /**Buffer for the config telegram
      */
      uint8_t configReadTelegram[odoConfigTeleMsgSize];

      /** Counter for the No of cycle in CODSim
      */
      uint32_t cycleCount;

      /** Flag for Connection Establishment
      */
      bool connectionEstablished;

      /** Interval (ms) at which COD is running
      */
      static const uint8_t codInterval = 50U;

      /** Interval (ms) at which CODSim is running
      */
      static const uint8_t codSimInterval = 100U;

      /** No of cycles between calling codSimValueWrite()
      */
      uint8_t sendCycle;

      /** Protocol version of the last decoded dynamic config message
      */
      uint8_t dynamicConfig_qVersion;

      /** Protocol version of the last decoded static config message
      */
      uint8_t staticConfig_qVersion;

      /** Measurement interval from the last decoded static config message
      */
      uint8_t staticConfig_ch1OdoMeasInterval;

      /** Buffer to store the Odometer measurement data telegram
      */
      ConfigResponse  outConfigResponseBuf;

      /** Channel handle returned by vfwChannelOpenRead()
      */
      VFW_SyncChannel syncCodSimChannelRead;

      /** Channel handle returned by vfwChannelOpenRead()
      */
      VFW_SyncChannel syncConfigChannelRead;

      /** Channel handle returned by vfwChannelOpenWrite()
      */
      VFW_ChannelDesc codSimValueChannelWrite;

      /** Channel handle returned by vfwChannelOpenWrite()
      */
      VFW_ChannelDesc configResponseChannelWrite;

      /** time stamp when Config telegram is read
      */
      int64_t timeStampConfigTelRec;

      /** Used to induce errors into vMax
      */
      int16_t vMaxError;

      /** Used to induce errors into vMin
      */
      int16_t vMinError;

      /**
      * Implements the codSimRead function.
      * @return the number of characters received
      */
      void codSimRead();

      /**
      * Implements the configRead function.
      */
      void configRead();

      /**
      * Parse the Static Configuration Telegram
      */
      void parseStaticConfig();

      /**
      * Parse the Dynamic Configuration Telegram
      */
      void parseDynamicConfig();

      /**
      * Implements the codSimValueWrite function.
      */
      void codSimValueWrite();

      /**
      * Implements the ConfigResponse function.
      */
      void configResponse();

      /**
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      CODSim();

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      CODSim(const CODSim&);

      /**
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      CODSim& operator = (const CODSim&);
    };
  }
}

#endif
