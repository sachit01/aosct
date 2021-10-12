#ifndef OPCSim_hpp
#define OPCSim_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This defines the interface of simulated OPC
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-06    spandita     Created
* 2016-09-12    spandita     Replaced four space with 2 space
* 2016-09-23    arastogi     Removed ATC::
* 2016-09-15    spandita     Updated with Member Variables
* 2016-09-26    spandita     Updated with review comments
* 2016-09-26    spandita     Removed ATC:: and moved constants from btm_message file
* 2016-10-05    spandita     Updated the initial Odo value
* 2016-10-24    arastogi     Added to save the odometer direction from last cycle.
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "event.hpp"
#include <vfw_sync.h>
#include "btm_types.hpp"
#include "simulated_btm_messages.hpp"
#include "channel_config.hpp"

namespace ATC
{
  namespace Sim
  {
    class OPCSim;
    /**
    * Static variable to store the single instance of OPCSim
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static OPCSim* coreOPCSimPtr = 0;

    /**
    * The class OPCSim implements the interface defined by the ComponentBase class and IOComponent class.
    *
    */

    class OPCSim : public IOComponent
    {
    public:

      /**
      * Implements the virtual preInit function.
      *
      * Register vfw channels in sync handler.
      */
      virtual void preInit();

      /**
      *Implements the init function.
      *
      * @return Returns true when initialization completed
      */
      virtual bool init();
      /**
      * Implements the runIn function.
      */
      virtual void runIn();

      /**
      * Implements the runOut function.
      */
      virtual void runOut();

      /**
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      */
      static OPCSim& instance();

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static OPCSim* corePtr();

      /**
      * Interface to call different level of Console Command
      *
      * @param[in] argc  Number of arguments in the argument array argv
      * @param[in] argv  Arguments array
      *
      * @return true if the Call is successful.
      */
      virtual bool consoleCall(const uint32_t argc, const ConsoleArguments argv);

    protected:

      /**
      * Channel write Error
      */

      const Event errorWriteChannel;

      /**
      * Channel Read Error
      */
      const Event errorReadChannel;

    private:

      /**
      *  Read the current Odo value check whether the mode is Balise Search
      *  if balise search mode update the end ranges of balise detected range
      *  Update the start time stamp and end time stamp once
      *  Update the field of telegram packet and reset the start and end time stamp flag
      */
      void verifyBalisePassage();

      /**
      *  Read the balise from balise list
      *  Compare it with previous detected Balise
      *  Update the start and end Balise Detected Range
      *  Update the next detected Balise
      */

      void getNextBaliseToDetect();

      /**
      *  Read from VFW channels
      *
      */
      void opcSimChannelRead();

      /**
      *  Read from VFW channels
      *
      */
      void syncChannelRead();

      /**
      *  Write to VFW channels
      *
      *
      */
      void writeToChannel();

      /**
      *  Write to VFW OPCSim channel
      */
      void writeToOPCSimChannel();

      /**
      *  Write to VFW BTM channel
      */
      void writeToBTMChannel();

      /**
      *  Write Application status
      *
      *
      */
      void writeAppStatus();

      /**
      *  Write Tigris Time Offset
      *
      *
      */
      void writeTigrisTimeOffset();

      /**
      *  Pack fake SPL header
      *
      *
      */
      void packSplHeader(VFW_Buffer& buffer, uint16_t dataSize);

      /**
      *  Pack version reply
      *
      *
      */
      void packVersionReply(VFW_Buffer& buffer);

      /**
      *  Pack life sign packet
      *
      *
      */
      void packLifeSign(VFW_Buffer& buffer);

      /**
      *  Pack time packet
      *
      *
      */
      void packStatusPacket(VFW_Buffer& buffer);

      /**
      *  Pack time packet
      *
      *
      */
      void packBtmTelegramPacket(VFW_Buffer& buffer);


      /**
      * Pack the output data in predefined buffers
      *
      */
      void pack();

      /**
       * Current simulated Status Message(Opc to Atp)
       */
      SimulatedBtmStatus simulatedBtmStatusMsg;


      /**
      * Update baliseObj according to current odo-offset
      */
      void updateOdoValForBaliseObj();

      /**
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      OPCSim();

      /**
      * Out Packet for Status Message from OPC to ATp
      **/
      ATP::IO::GP_10ByteVitalSinkDataA  statusMsg;

      /**
      * Status message telegram valid
      **/
      bool telegramValid;

      /**
      * Status message telegram status
      **/
      ATP::IO::TIOS_FailureInformationA telegramStatus;

      /**
      * Status message timestamp
      **/
      uint32_t safetyTimeStamp;

      /**
      * Out Packet for Telegram Message from OPC to ATp from Port 1
      **/
      GP_26ByteVitalSinkDataA telgramMsg;
      /**
      * Out Packet for Telegram Message from OPC to ATp from Port 2 - 5
      **/
      GP_26ByteVitalSinkDataA telgramMsg2;

      /**
      * Data structure for storing Simulated BTM telegram message(OPC to ATP)
      */
      struct SimulatedBtmTelegram
      {
        uint8_t status;                   //!< Validity of telegram message from Port 1
        TelegramMessage msgData1;          //!< data from port 1 
      };

      /**
      * Current simulated BTM telegram Message (OPC to ATP)
      */
      SimulatedBtmTelegram simulatedBtmTelMsg;

      /**
       * Current Antenna Odo value
       */
      ATP::OdoPosition currentReceivedOdo;

      /**
       * Next detected balise ID data
       */
      SimBaliseInfo baliseObj;

      /**
       * Previous detected Balise ID
       */
      uint16_t previousExpectedBaliseId;

      /**
      * Indicates whether balise detection has started
      */
      bool startOdoFlag;

      /**
      * Indicates whether balise detection has ended
      */
      bool stopOdoFlag;

      /**
      * Indicates whether @ref baliseObj must be updated
      */
      bool updateBaliseObj;

      /**
      * Flag to check if Connected to Loco/AOSPC
      **/
      bool isConnected;

      /**
       * VFW Channel descriptor for writing the all messages
       *  except telegram message
       */
      VFW_ChannelDesc opcSimWriteChannelDesc;

      /**
      *  Channel handle returned by vfwChannelOpenWrite()
      */
      VFW_ChannelDesc opcSimAOSPCWriteChannelDesc;

      /**
      * VFW Channel descriptor for writing application status
      */
      VFW_ChannelDesc opcSimWriteAppStatusChannelDesc;

      /**
      * VFW Channel descriptor for time synchronization packet
      */
      VFW_ChannelDesc opcSimWriteTigrisTimeOffsetChannelDesc;

      /**
      *  Channel handle returned by vfwSyncAddChannel()
      */
      VFW_SyncChannel syncChannelReadDesc;

      /** Channel handle returned by vfwChannelOpenRead()
      */
      VFW_SyncChannel syncOPCSimChannelReadDesc;

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * Odometer direction from previous cycle.
      */
      ATP::TravelDir oldOdoDir;

      /**
      * Representing the time difference between the SPL ref time and the TigrisReftime in the OPC in milliseconds.
      */
      uint32_t refTimeOffset;

      /**
      * Simulate some drift
      */
      uint16_t refTimeOffsetDrift;

      /*
      *  Send version request
      */
      bool sendVersionReply;

      /**
      * Counter for the routine test
      */
      uint8_t currentRoutineTestCounter;

      /**
      * Hold BSA value counter...
      */
      uint32_t holdTemporaryBsaValueCounter;

      /**
      * Variable if true, set the balise present flag (only used for debugging)
      */
      bool setBalisePresent;

      /**
      * Variable if true, set the overheated flag (only used for debugging)
      */
      bool setOverHeated;

      /**
      * Balise Number to be used in Balise Search Mode(ATP mode)
      */ 
      uint16_t initialBaliseNo;

      /**
      * Reg balise Id received from AOSPC
      */
      bool regBaliseIdReceived;

      /**
      * Timer used for cyclic transmission of the simulated ATP Ready message
      */
      VFW_Timer timerWriteToOPCSimChannel;

      /**
      * Lifesign status
      */
      uint8_t lifesign;
    };

    /**
    * Timout (ms) for cyclic transmission of the simulated ATP Ready message
    */
    const int64_t timeoutWriteToOPCSimChannel = 5000;

    /**
    * Initial Balise Odo to be used in Balise Search Mode(ATP mode)
    */
    const ATP::OdoPosition initialBaliseOdo = 1000;

    /**
    * Balise detection  distance
    * in future it will get moved into to con-fig parameter
    */
    const ATP::OdoPosition baliseDetectionRange = 10;

    /**
    * length of status message packet
    */
    const uint16_t statusMsgLength = 18U;

    /**
    * length of BTM Telegram message packet
    */
    const uint16_t telegramMsgLength = 166U;

    const uint16_t nidLifesign = 266U;
    const uint16_t nidAppStatus = 270U;
    const uint16_t nidTigirisTimeOffset = 300U;
    const uint16_t nidBtmTgm2BtmH = 513U;
    const uint16_t nidBtmStatus2BtmH = 514U;
    const uint16_t nidVersionReply = 522U;

    const uint16_t nidBtmH2Btm = 256U;
    const uint16_t nidOdoH2Opc = 267U;
    const uint16_t nidVersionRequest = 268U;
    const uint16_t nidAtpService = 269U;

    // Packet lengths
    const uint16_t packetLengthLifesign = 14U;
    const uint16_t packetLengthAppStatus = 11U;
    const uint16_t packetLengthTigrisTimeOffset = 13U;
    const uint16_t packetLengthBtmTgm2BtmH = 170U;
    const uint16_t packetLengthBtmStatus2BtmH = 22U;
    const uint16_t packetLengthVersionReply = 26U;

    const uint16_t packetLengthBtmH2Btm = 16U;
    const uint16_t packetLengthOdoH2Opc = 36U;
    const uint16_t packetLengthVersionRequest = 4U;
    const uint16_t packetLengthAtpService = 11U;

    const uint32_t splHeaderSize = 7U;

    /** Max  no of messages available for reading from ATP
    */
    const uint32_t   readMessageInQueueSize = 3U;

    /** Max  no of messages possible to write to ATP Component BTM handler
    */
    const uint32_t   opcSimValueMessageOutQueueSize = 3U;

    /** Max  no of messages possible to write to ATP Component BTM handler
    */
    const uint32_t   opcSimValueBtmMessageOutQueueSize = 3U;

    /**
    * Defines Type of telegram (Class field in BTM Telegram from OPC to ATP)
    */
    const uint8_t btmValid830Telegram = 4U;

    /**
    *   Input message length (from ATP to OPC)
    **/
    const uint16_t simInrMsgLen = 11U;

    /**
    * BTM start up or routine test is in progress
    */
    const uint8_t btmStartUpOrRoutineTestIsInProgress = 1U;

    /**
    * BTM Balise Service Available
    */
    const uint8_t  btmBaliseServiceAvailable = 0U;

    /**
    * BSA sporadic failure.
    */
    const uint8_t bsaSporadicFailure = 2U;

    /**
    * Preliminary BTM Status Green: The test indicates that the BTM + antenna are likely to give full performance when the train starts rolling.
    */
    const uint8_t btmPreliminaryBtmStatusGreen = 4U;

    /**
    * Preliminary BTM Status Unavailable: The test is not available, e.g., due to the antenna being switched off or an on - going routine test.
    */
    const uint8_t btmPreliminaryBtmStatusUnavailable = 2U;
  }
}

#endif
