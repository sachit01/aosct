#ifndef VehicleCom_hpp
#define VehicleCom_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines the VehicleCom class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-28    marlundg    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "abstract_vehicle_com.hpp"
#include "lcs_message_handler.hpp"
#include "lcs_message_common.hpp"
#include "radio_message_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace TG
  {

    /** LIG Communication Configuration state
    */
    enum VComConfigState
    {
      VComConfigStartup = 0,
      VComConfigSendTCRequest,
      VComConfigSendWaitTC,
      VComConfigWaitForTC,
      VComConfigSendTCToTCC,
      VComConfigSendFinalTC,
      VComConfigFinished
    };

    /**
    * The class VehicleCom instantiates the abstract class and implements
    * the interfaces needed for both inherited classes and component.
    *
    */
    class VehicleCom : public AbstractVehicleCom
    {
    public:

      /**
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static VehicleCom& instance(void);

      /**
      * Constructor
      */
      VehicleCom();

      /**
      * Implements the preInit function.
      *
      */
      virtual void preInit(void);

      /**
      * Implements the init function.
      *
      * @return true when initialization completed
      */
      virtual bool init(void);

      /**
      * Processes incoming messages from LCS in protocol stack Class-D/EMP/LCSMessageHandler
      *
      */
      virtual void runIn(void);

      /**
      * Processes logic when to send outgoing messages to LCS through protocol stack LCSMessageHandler/EMP/Class-D
      */
      virtual void runOut(void);

      /**
      * Get connection status towards vehicle
      *
      * @return true if connected to vehicle
      */
      virtual bool connectedVehicleComm() const;

      /**
      * Access-function for car-List
      *
      *  @param[out] ECPBTrainCompositionType  Received car-list from LCS/TIC
      *
      *  @return true if car-list is received since connection with LCS
      */
      bool getLatestCarList(ECPBTrainCompositionType & ecpbTrainComposition) const;

      /**
      * Access-function for ECPBTrainComposition
      *
      *  @param[out] ECPBTrainCompositionType  Received car-list from LCS/TIC
      *
      *  @return true if car-list is received this cycle
      */
      bool getECPBTrainComposition(ECPBTrainCompositionType & ecpbTrainComposition) const;

      /**
      * Access-function for VCom ConfigState
      *
      *  @return The current state of the VehicleCom configuration
      */
      VComConfigState getVComConfigModeState() const;

      /**
      * Access-function for ATO-Mode cabin selector
      *
      *  @param[out] mode   Selected ATO Mode
      *
      *  @return true if status is valid
      */
      bool getATOModeCabinSelector(ATOModeCabinSelectorType & status) const;

      /**
      * Access-function for ATO Driving Mode
      *
      *  @param[out] status   Selected ATO Driving Mode
      *
      *  @return true if mode is valid
      */
      bool getATODrivingMode(ATODrivingModeType & mode) const;

      /**
      * Access-function for FreeRolling status
      *
      *  @param[out] status   FreeRolling status
      *
      *  @return true if mode is valid
      */
      bool getFreeRollingStatus(FreeRollingStatusType & status) const;

      /**
      * Access-function for Blueflag-status
      *
      *  @param[out] status  Blueflag status
      *
      *  @return true if status is valid
      */
      bool getBlueflagStatus(BlueFlagStatusType & status) const;

      /**
      * Access-function for Blueflag-requested
      *
      *  @param[out] requested  Blueflag requested
      *
      *  @return true if requested is valid
      */
      bool getBlueFlagRequested(BlueFlagRequestType & requested) const;

      /**
      * Access-function for ADS ETA Status
      *
      *  @param[out] status ADS ETA status
      *
      *  @return true if status is valid
      */
      bool getAdsEtaStatus(AdsEtaStatus & status) const;

      /**
      * Access-function for LEADER ETA
      *
      *  @param[out] eta   ETA for next target
      *
      *  @return true if eta is valid
      */
      bool getAdsEta(uint32_t & eta) const;

      /**
      * Access-function for ATO Ready
      *
      *  @param[out] status   ATO Ready (locomotive and train are ready for ATO commands)
      *
      *  @return true if status is valid
      */
      bool getLCSATOReady(LcsAtoStatusType & status) const;

      /**
      * Access-function for ECPB Sequence Status
      *
      *  @param[out] status   ECPB Sequence Status
      *
      *  @return true if status is valid
      */
      bool getEcpbSequenceStatus(EcpbSequenceStatusType & status) const;

      /**
      * Access-function for Ready for Precision Stop
      *
      *  @param[out] status   Locomotive and train are ready for precision stop
      *
      *  @return true if status is valid
      */
      bool getReadyForPrecisionStop(ReadyForPrecisionStopType & status) const;

      /**
      * Access-function for Brake System (not necessary the one in use), call tsetup in order to get the one in use.
      *
      *  @return brake system reported by Vehicle
      */
      BrakeSystemType getBrakeSystem() const;

      /**
      * Access-function for ECPB Operating Modes
      *
      *  @param[out] mode   Current ECPB Operating Mode
      *
      *  @return true if mode is valid
      */
      bool getEcpbOperatingModes(EcpbOperatingModesType & mode) const;

      /**
      * Access-function for Train Integrity Status ECPB
      *
      *  @param[out] status     Train Integrity Status ECPB
      *  @param[out] timeStamp  Time when integrity status was sent
      *
      *  @return true if status is valid
      */
      bool getTrainIntegrityStatusEcpb(TrainIntegrityStatusEcpbType& status, uint64_t& timeStamp) const;

      /**
      * Access-function for Percentage Of Operative Brakes ECPB
      *
      *  @return    Percentage Of Operative Brakes ECPB, 0 if not valid
      */
      uint8_t getPercentageOfOperativeBrakesEcpb() const;

      /**
      * Access-function for Version Of ADS Map
      *
      *  @param[out] pressure   Version Of ADS Map (MSB = Major Version, LSB = Minor Version)
      *
      *  @return true if version is valid
      */
      bool getVersionOfAdsMap(uint16_t & version) const;

      /**
      * Access-function for Penalty Break Status
      *
      *  @param[out] status   Penalty Break Status
      *
      *  @return true if weight is valid
      */
      bool getLocoPenaltyBreakStatus(PenaltyBreakActiveType & status) const;

      /**
      * Access-function for locomotive system faults
      *
      *  @param[out] faultBitField   Bitfield for system faults
      *
      *  @return true if faultBitField is valid
      */
      bool getLocoSystemFaults(uint32_t & faultBitField) const;

      /**
      * Access-function for ADS status
      *
      *  @param[out] statusBitField   Bitfield for ADS status
      *
      *  @return true if statusBitField is valid
      */
      bool getAdsStatus(uint32_t & statusBitField) const;

      /**
      * Access-function for LEADER recovery from communication loss.
      *
      *  @return true if LEADER has recovered
      */
      bool leaderHasRecoveredFromComLoss() const;

      /**
      * To handle console calls for Vehicle Com.
      * This functions parses the arguments searches for the "help"  or any other Console
      * component specific command calls and handles it. Returns true if completely handled
      * else returns false. returning false will let other componets handle the call. help always returns false.
      *
      * @param[in] argc - Number of arguments in the argument array argv
      *
      * @param[in] argv - Arguments array
      *
      * @return - returns true if handled successfully, except "help"(it always returns false)
      */
      virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

      /**
      * Start up and health supervision test for vehicle interface
      *
      * @return true if successful
      */
      virtual bool startupAndHealthSupTest() const;

      /**
      * Access-function for Rcl status
      *
      *  @return true if RCL status is done
      */
      bool getHandlingDoneRequestReceived() const;

    protected:

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * Value if not asserted Percentage Of Operative Brakes when ECPB
      */
      static const uint8_t notAssertPercentageOfOperativeBrakesEcpb = 255U;

      /**
      * Event for logging Locomotive System Fault type Loss of LEADER Comm.
      */
      const ATC::Event sysFaultLossOfLeaderComm;

      /**
      * Event for logging Locomotive System Fault type Loss of Air Brake Comm.
      */
      const ATC::Event sysFaultLossOfAirBrakeComm;

      /**
      * Event for logging Locomotive System Fault type Recovery of LEADER Comm.
      */
      const ATC::Event sysFaultRecoveryOfLeaderComm;

      /**
      * Event for logging Locomotive System Fault type Recovery of Air Brake Comm.
      */
      const ATC::Event sysFaultRecoveryOfAirBrakeComm;

      /**
      * Event for logging if LIG-communication is established on a non EMD Loco.
      */
      const ATC::Event faultyLigConfig;

      /**
      * Connection status towards LCS
      */
      bool connectedToLcs;

      /**
      * Latest received Train-Status from LCS
      */
      LCSTrainStatusType currentTrainStatus;

      /**
      * Latest stored value of brake system in use
      */
      BrakeSystemType currBrakeSystemInUse;

      /* Train Composition is valid
      */
      bool trainCompositionValid;

      /**
      * Train status is valid
      */
      bool trainStatusValid;

      /**
      * Flag to remember previous value of LossOfLeaderRCom status.
      */
      bool previousLossOfLeaderComm;

      /**
      * Flag to remember previous value of LossOfAirBrakeCOMM status.
      */
      bool previousLossOfAirBrakeCom;
      
      /**
      * Flag if LEADER has recovered from communication loss.
      */
      bool leaderHasRecovered;

      /**
      * Received Train-Composition from LCS
      */
      ECPBTrainCompositionType currentTrainComposition;

      /**
      * The LCS Message Handler
      */
      LCSMessageHandler lcsMessageHandler;

      /**
      * The current state of the VehicleCom configuration
      */
      VComConfigState configState;

      /** Previous locomotive system fault on LCS
      */
      uint32_t prevSysFault;
    };
  }
}

#endif
