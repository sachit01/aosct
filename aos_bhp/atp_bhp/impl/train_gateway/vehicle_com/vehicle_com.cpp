/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the VehicleCom class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-24    marlundg    Created

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_identity.h>

#include "atc_types.hpp"
#include "atp_types.hpp"
#include "atc_base.hpp"
#include "config.hpp"
#include "channel_config.hpp"
#include "abstract_event_handler.hpp"
#include "vehicle_com.hpp"
#include "lcs_message_in_train_status.hpp"
#include "abstract_tsetup.hpp"
#include "lcs_message_common.hpp"
#include "abstract_console.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "atc_util.hpp"
#include "dmi_bhp_event_codes.hpp"
#include "vehicle_com_event_ids.hpp"
#include <vfw_checkpoints.h>
#include "tsetup.hpp"

#include <cstdio>

/******************************************************************************
* LINT SUPPRESSIONS
******************************************************************************/
//lint -esym(586,snprintf) snprintf is needed here

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace TG
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    VehicleCom::VehicleCom() : AbstractVehicleCom(),
      sysFaultLossOfLeaderComm(ATC::Event::createLogEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdSysFaultLossOfLeaderComm, DMICom::leaderCommLoss, "Loss of LEADER Comm")),
      sysFaultLossOfAirBrakeComm(ATC::Event::createLogEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdSysFaultLossOfAirBrakeComm, DMICom::airBrakeCommLoss, "Loss of Air Brake Comm")),
      sysFaultRecoveryOfLeaderComm(ATC::Event::createLogEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdSysFaultRecoveryOfLeaderComm, DMICom::leaderCommEstablished, "Recovered from Loss of LEADER Comm")),
      sysFaultRecoveryOfAirBrakeComm(ATC::Event::createLogEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdSysFaultRecoveryOfAirBrakeComm, DMICom::airBrakeCommEstablished, "Recovered from Loss of Air Brake Comm")),
      faultyLigConfig(ATC::Event::createSafetyHaltEvent(atpVehicleComId, ATC::AdaptationContainer,
        eventIdFaultyLigConfig, ATC::NoEB, DMICom::faultyLigConfig, "LIG-communication cannot be established on a non EMD Loco.")),
      connectedToLcs(false),
      currBrakeSystemInUse(BrakeSystemType1),
      trainCompositionValid(false),
      trainStatusValid(false),
      lcsMessageHandler((vfwGetSide() == VFW_A_SIDE) ? ATC::lcsChannelDispToATPA : ATC::lcsChannelDispToATPB,
        (vfwGetSide() == VFW_A_SIDE) ? ATC::lcsChannelATPAToDisp : ATC::lcsChannelATPBToDisp),
      configState(VComConfigStartup)
    {
      currentTrainComposition.numberOfDetectedVehiclesByECP = 0U;
      currentTrainComposition.numberOfDetectedVehiclesUnknownPos = 0U;
      currentTrainComposition.numberOfNotDetectedVehicles = 0U;
      prevSysFault = 0U;

      memset(&currentTrainComposition.rollingStockPosition[0], 0, sizeof(currentTrainComposition.rollingStockPosition));

      previousLossOfLeaderComm = false;
      previousLossOfAirBrakeCom = false;
      leaderHasRecovered = false;
    }

    /******************************************************************************
    * preInit
    ******************************************************************************/
    void VehicleCom::preInit(void)
    {
      // Setup VFWChannels towards dispatcher.
      lcsMessageHandler.preInit();
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool VehicleCom::init()
    {
      const bool baseInit = AbstractVehicleCom::init();

      // Setup the Creators/Parsers for LCS Message Handler
      const bool lcsMessageHandlerInit = lcsMessageHandler.init();
      return (baseInit && lcsMessageHandlerInit);
    }

    /******************************************************************************
    * run_in
    ******************************************************************************/
    void VehicleCom::runIn(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "VC_runIn");

      // Process the lcsMessageHandler 'run_in()' to read incoming LCS messages and publish data.
      lcsMessageHandler.runIn();

      // Get connection status towards LCS
      connectedToLcs = lcsMessageHandler.connectedLCS();

      // Get Train Status, it will be valid until next status arrives or connection is down.
      if (lcsMessageHandler.getTrainStatus(currentTrainStatus))
      {
        trainStatusValid = true;
      }
      
      // Check if a train-composition is available, it will be valid until the connection is down.
      if (lcsMessageHandler.getECPBTrainComposition(currentTrainComposition))
      {
        trainCompositionValid = true;
      }

      // Fetch Loco-type
      LocoTypeAdap locoType = static_cast<LocoTypeAdap>(Config::instance().getLocoType());

      //Check if Locomotive system fault is present in train status message
      uint32_t locoSysFault = 0U;

      bool locoSysFaultValid = VehicleCom::instance().getLocoSystemFaults(locoSysFault);

      if ((locoSysFault != prevSysFault) && locoSysFaultValid)
      {
         if (((lossOfLeaderComm & locoSysFault) > 0U) && (EMD == locoType))
         {
            // Locomotive system fault event is present to the driver if loco type is EMD and loco system fault is Loss of LEADER Comm
            ATC::AbstractEventHandler::corePtr()->reportEvent(sysFaultLossOfLeaderComm, __FILE__, __LINE__);
         }

         if (((lossOfAirBrakeComm & locoSysFault) > 0U) && (EMD == locoType))
         {
            // Locomotive system fault event is present to the driver if loco type is EMD and loco system fault is Loss of LEADER Comm
            ATC::AbstractEventHandler::corePtr()->reportEvent(sysFaultLossOfAirBrakeComm, __FILE__, __LINE__);
         }

         //Set the previous locomotive system fault
         prevSysFault = locoSysFault;
      }

      // Consider trainComposition and train-status invalid if connections to LCS is down.
      if (!connectedToLcs)
      {
        trainCompositionValid = false;
        trainStatusValid = false;
      }
      
      // Update the flag for LEADER recovery
      if (trainStatusValid)
      {
        // Get current state for Loss of LEADER Communication.
        bool currentLossOfLEADERCom = (currentTrainStatus.locomotiveSystemFaults & lossOfLeaderComm) > 0U;

        // Get current state for Loss of Air Brake Communication.
        const bool currentLossOfAirBrakeCom = (currentTrainStatus.locomotiveSystemFaults & lossOfAirBrakeComm) > 0U;

        // LEADER has recovered?
        if (previousLossOfLeaderComm && (!currentLossOfLEADERCom))
        {
          leaderHasRecovered = true;
        }
        else
        {
          leaderHasRecovered = false;
        }

        if (leaderHasRecovered)
        {
          // Leader has recovered from Communication Loss and report the event
          ATC::AbstractEventHandler::corePtr()->reportEvent(sysFaultRecoveryOfLeaderComm, __FILE__, __LINE__);
        }
        previousLossOfLeaderComm = currentLossOfLEADERCom;


        // AirBrake has recovered?
        if (previousLossOfAirBrakeCom && (!currentLossOfAirBrakeCom))
        {
          // AIRBRAKE has recovered from Communication Loss and report the event
          ATC::AbstractEventHandler::corePtr()->reportEvent(sysFaultRecoveryOfAirBrakeComm, __FILE__, __LINE__);
        }
        previousLossOfAirBrakeCom = currentLossOfAirBrakeCom;
      }

    }

    /******************************************************************************
    * runOut
    ******************************************************************************/
    void VehicleCom::runOut(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "VC_runOut");

      // Process the lcsMessageHandler 'run_out()'-method to collect, validate, add EMP Header and write to VFWChannel.
      lcsMessageHandler.runOut();
    }
    
    /******************************************************************************
    * getECPBTrainComposition
    ******************************************************************************/
    bool VehicleCom::getECPBTrainComposition(ECPBTrainCompositionType & ecpbTrainComposition) const
    {
      return lcsMessageHandler.getECPBTrainComposition(ecpbTrainComposition);
    }
    
    /******************************************************************************
    * getLatestCarList
    ******************************************************************************/
    bool VehicleCom::getLatestCarList(ECPBTrainCompositionType & ecpbTrainComposition) const
    {
      // Car list is valid if train composition is received
      if (trainCompositionValid)
      {
        ecpbTrainComposition = currentTrainComposition;
      }

      return trainCompositionValid;
    }

    /******************************************************************************
    * getVComConfigModeState
    ******************************************************************************/
    VComConfigState VehicleCom::getVComConfigModeState() const
    {
      return configState;
    }

    /******************************************************************************
    * getATOModeCabinSelector
    ******************************************************************************/
    bool VehicleCom::getATOModeCabinSelector(ATOModeCabinSelectorType & status) const
    {
      if (trainStatusValid)
      {
        status = currentTrainStatus.atoModeCabinSelectorStatus;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getATODrivingMode
    ******************************************************************************/
    bool VehicleCom::getATODrivingMode(ATODrivingModeType & mode) const
    {
      if (trainStatusValid)
      {
        mode = currentTrainStatus.atoDrivingMode;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getFreeRollingStatus
    ******************************************************************************/
    bool VehicleCom::getFreeRollingStatus(FreeRollingStatusType & status) const
    {
      if (trainStatusValid)
      {
        status = currentTrainStatus.freeRollingStatus;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getBlueflagStatus
    ******************************************************************************/
    bool VehicleCom::getBlueflagStatus(BlueFlagStatusType & status) const
    {
      if (trainStatusValid)
      {
        status = currentTrainStatus.blueFlagStatus;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getBlueFlagRequested
    ******************************************************************************/
    bool VehicleCom::getBlueFlagRequested(BlueFlagRequestType & requested) const
    {
      if (trainStatusValid)
      {
        requested = currentTrainStatus.blueFlagRequest;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getAdsEtaStatus
    ******************************************************************************/
    bool VehicleCom::getAdsEtaStatus(AdsEtaStatus & status) const
    {
      if (trainStatusValid)
      {
        status = currentTrainStatus.adsEtaStatus;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getLeaderETA
    ******************************************************************************/
    bool VehicleCom::getAdsEta(uint32_t & eta) const
    {
      if (trainStatusValid)
      {
        eta = currentTrainStatus.adsEta;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getLCSATOReady
    ******************************************************************************/
    bool VehicleCom::getLCSATOReady(LcsAtoStatusType & status) const
    {
      if (trainStatusValid)
      {
        status = currentTrainStatus.lcsAtoStatus;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getEcpbSequenceStatus
    ******************************************************************************/
    bool VehicleCom::getEcpbSequenceStatus(EcpbSequenceStatusType & status) const
    {
      if (trainStatusValid)
      {
        status = currentTrainStatus.ecpbSequenceStatus;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getReadyForPrecisionStop
    ******************************************************************************/
    bool VehicleCom::getReadyForPrecisionStop(ReadyForPrecisionStopType & status) const
    {
      if (trainStatusValid)
      {
        status = currentTrainStatus.readyForPrecisionStop;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getBrakeSystemInUse
    ******************************************************************************/
    BrakeSystemType VehicleCom::getBrakeSystem() const
    {
      BrakeSystemType type = BrakeSystemType1; // EMD: Brake system type 1, if the stored brake system in use is Pneumatic
      if (trainStatusValid  &&  (currentTrainStatus.reportedEcpbBrakeSystem == BrakeSystemEcpb))
      {
        type = BrakeSystemType2; // Brake system type 2 if the stored brake system in use is ECPB
      }

      return type;
    }
    
    /******************************************************************************
    * getEcpbOperatingModes
    ******************************************************************************/
    bool VehicleCom::getEcpbOperatingModes(EcpbOperatingModesType & mode) const
    {
      if (trainStatusValid)
      {
        mode = currentTrainStatus.ecpbOperatingModes;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getTrainIntegrityStatusEcpb
    ******************************************************************************/
    bool VehicleCom::getTrainIntegrityStatusEcpb(TrainIntegrityStatusEcpbType& status, uint64_t& timeStamp) const
    {
      if (trainStatusValid)
      {
        status = currentTrainStatus.trainIntegrityStatusEcpb;
        timeStamp = currentTrainStatus.timeStamp;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getPercentageOfOperativeBrakesEcpb
    ******************************************************************************/
    uint8_t VehicleCom::getPercentageOfOperativeBrakesEcpb() const
    {
      uint8_t percentage = 0U;

      if (trainStatusValid)
      {
        if (currentTrainStatus.percentageOfOperativeBrakesEcpb != notAssertPercentageOfOperativeBrakesEcpb)
        {
          percentage = currentTrainStatus.percentageOfOperativeBrakesEcpb;
        }
      }

      return percentage;
    }

    /******************************************************************************
    * getVersionOfLeaderMap
    ******************************************************************************/
    bool VehicleCom::getVersionOfAdsMap(uint16_t & version) const
    {
      if (trainStatusValid)
      {
        version = currentTrainStatus.versionOfAdsMap;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getLocoPenaltyBreakStatus
    ******************************************************************************/
    bool VehicleCom::getLocoPenaltyBreakStatus(PenaltyBreakActiveType & status) const
    {
      if (trainStatusValid)
      {
        status = currentTrainStatus.penaltyBreakStatus;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getLocoSystemFaults
    ******************************************************************************/
    bool VehicleCom::getLocoSystemFaults(uint32_t & faultBitField) const
    {
      if (trainStatusValid)
      {
        faultBitField = currentTrainStatus.locomotiveSystemFaults;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * getAdsStatus
    ******************************************************************************/
    bool VehicleCom::getAdsStatus(uint32_t & statusBitField) const
    {
      if (trainStatusValid)
      {
        statusBitField = currentTrainStatus.adsStatus;
      }

      return trainStatusValid;
    }

    /******************************************************************************
    * leaderHasRecoveredFromComLoss
    ******************************************************************************/
    bool VehicleCom::leaderHasRecoveredFromComLoss() const
    {
      return leaderHasRecovered;
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool VehicleCom::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      bool retVal = false;
      static char_t  buffer[2048];

      // Check if argument passed is "help"
      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        ATC::AbstractConsole::corePtr()->writeWithNewline("vc stat       To get status for last incoming status-data to VehicleCom.");
        ATC::AbstractConsole::corePtr()->writeWithNewline("vc comp       To get status for last incoming train-composition-data to VehicleCom.");
        ATC::AbstractConsole::corePtr()->writeWithNewline("vc tstamp     To get the last sent/received time to/from LCS.");
      }
      // Check if argument passed is "vc"
      else if (ATC::isTextMatch(&argv[0][0], "vc", sizeof("vc")))
      {
        switch (argc)
        {
        case 2:
        {
          // Check if 2nd argument passed is "stat"
          if (ATC::isTextMatch(&argv[1][0], "stat", sizeof("stat")))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline("---------------------------------------");

            int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%-30s%-10s", "Signal-name", "Raw-value");

            if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
            }

            ATC::AbstractConsole::corePtr()->writeWithNewline("----------------------------------------");

            // Collect and print all status values
            ret = snprintf(&buffer[0], sizeof(buffer),
              "%-35s%-10d\n"

              "%-35s%-10d\n"
              "%-35s%-10d\n"
              "%-35s%-10d\n"
              "%-35s%-10d\n"
              "%-35s%-10d\n"

              "%-35s%-10u\n"
              "%-35s%-10u\n"
              "%-35s%-10d\n"
              "%-35s%-10d\n"
              "%-35s%-10d\n"
              "%-35s%-10d\n"
              "%-35s%-10d\n"
              "%-35s%-10d\n"
              
              "%-35s%-10u\n"
              "%-35s%-10u\n"

              "%-35s0x%-10X\n"
              "%-35s%-10u\n"
              "%-35s0x%-10X\n"
              "%-35s0x%-10X\n",
              
              "Connected LCS", connectedVehicleComm(),
              "ATOModeCabinSelectorType", currentTrainStatus.atoModeCabinSelectorStatus,
              "ATODrivingMode", currentTrainStatus.atoDrivingMode,
              "FreeRollingStatus", currentTrainStatus.freeRollingStatus,
              "BlueFlagStatus", currentTrainStatus.blueFlagStatus,
              "BlueFlagRequest", currentTrainStatus.blueFlagRequest,
              "ADS ETA Status", currentTrainStatus.adsEtaStatus,
              "ADS ETA", currentTrainStatus.adsEta,
              "LcsAtoStatus", currentTrainStatus.lcsAtoStatus,
              "EcpbSequenceStatus", currentTrainStatus.ecpbSequenceStatus,
              "ReadyForPrecisionStop", currentTrainStatus.readyForPrecisionStop,
              "BrakeSystemInUse", currentTrainStatus.reportedEcpbBrakeSystem,
              "EcpbOperatingModes", currentTrainStatus.ecpbOperatingModes,
              "TrainIntegrityStatusEcpb", currentTrainStatus.trainIntegrityStatusEcpb,
              "Percentage of Op Brakes", currentTrainStatus.percentageOfOperativeBrakesEcpb,
              "Last Car Pressure", currentTrainStatus.lastCarBrakePressure,  
              "Version of ADS Map", currentTrainStatus.versionOfAdsMap,
              "Loco Penalty Brake Active", currentTrainStatus.penaltyBreakStatus,
              "Loco System Faults", currentTrainStatus.locomotiveSystemFaults,
              "ADS Status", currentTrainStatus.adsStatus);

            if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
            }

            retVal = true;
          }
          else if (ATC::isTextMatch(&argv[1][0], "comp", sizeof("comp")))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline("---------------------------------------------------------------");

            int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%-45s%-10s", "Description", "Number of Vehicles");

            if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
            }

            ATC::AbstractConsole::corePtr()->writeWithNewline("---------------------------------------------------------------");

            // Collect and print all status values
            ret = snprintf(&buffer[0], sizeof(buffer),
              "%-45s%-10d\n"
              "%-45s%-10d\n"
              "%-45s%-10d\n",
              "Number Of Detected Vehicles By ECP", currentTrainComposition.numberOfDetectedVehiclesByECP,
              "Number Of Detected Vehicles Unknown Pos", currentTrainComposition.numberOfDetectedVehiclesUnknownPos,
              "Number Of Not Detected Vehicles", currentTrainComposition.numberOfNotDetectedVehicles);

            if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
            }

            ATC::AbstractConsole::corePtr()->writeWithNewline("---------------------------------------");

            ret = snprintf(&buffer[0], sizeof(buffer), "%-10s%-15s%-20s", "Index", "Road-number", "Vehicle type");

            if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
            }

            ATC::AbstractConsole::corePtr()->writeWithNewline("---------------------------------------");

            for (uint16_t i = 0U; i < currentTrainComposition.numberOfDetectedVehiclesByECP; ++i)
            {
              ret = snprintf(&buffer[0], sizeof(buffer), "%-10d%-15d%-20d",
                i,
                currentTrainComposition.rollingStockPosition[i].roadNumber,
                currentTrainComposition.rollingStockPosition[i].vechicleType);

              if ((ret > 0) && (ret < 2048))
              {
                ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
              }
            }

            retVal = true;
          }
          else if (ATC::isTextMatch(&argv[1][0], "tstamp", sizeof("tstamp")))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline("---------------------------------------------------------------");

            uint32_t lastSentUTCTimeToLCS = lcsMessageHandler.getLastSentUTCTimeToLCS();
            uint32_t lastReceivedUTCTimeFromLCS = lcsMessageHandler.getLastReceivedUTCTimeFromLCS();
            
            const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "LastSentUTCTimeToLCS       : %u\nLastReceivedUTCTimeFromLCS : %u", 
              lastSentUTCTimeToLCS, lastReceivedUTCTimeFromLCS);
            
            if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
            }
            ATC::AbstractConsole::corePtr()->writeWithNewline("---------------------------------------------------------------");

            retVal = true;
          }
          else
          {
            char_t toWrite[] = "Illegal Arguments to vc.";

            ATC::AbstractConsole::corePtr()->writeWithNewline(&toWrite[0]);
          }
          break;
        }

        default:
        {
          char_t toWrite[] = "Illegal Arguments: vc needs an argument.";

          ATC::AbstractConsole::corePtr()->writeWithNewline(&toWrite[0]);

          break;
        }
        }
      }
      else
      {
        // Process the lcsMessageHandler 'consoleCall()' to handle ConsoleCall() in LCS Message handler
        retVal = lcsMessageHandler.consoleCall(argc, argv);
      }

      return retVal;
    }

    /******************************************************************************
    * getHandlingDoneRequestReceived
    ******************************************************************************/
    bool VehicleCom::getHandlingDoneRequestReceived() const
    {
      HandlingDoneType handlingDone = HandlingDoneUndefined;

      const bool result = lcsMessageHandler.getHandlingDone(handlingDone);

      return result && (handlingDone == HandlingDoneRequest);
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void VehicleCom::initCrossCompare() const
    {
      AbstractVehicleCom::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&connectedToLcs));
 
      currentTrainStatus.initCrossCompare();

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&sysFaultLossOfLeaderComm));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&sysFaultLossOfAirBrakeComm));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&sysFaultRecoveryOfLeaderComm));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&sysFaultRecoveryOfAirBrakeComm));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&faultyLigConfig));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&trainCompositionValid));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&trainStatusValid));

      currentTrainComposition.initCrossCompare();

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<VComConfigState>(&configState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<BrakeSystemType>(&currBrakeSystemInUse));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&previousLossOfLeaderComm));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&previousLossOfAirBrakeCom));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&leaderHasRecovered));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&prevSysFault));
    }

    /******************************************************************************
    * connectedVehicleComm
    ******************************************************************************/
    bool VehicleCom::connectedVehicleComm() const
    {
      return connectedToLcs;
    }

    /******************************************************************************
    * startupAndHealthSupTest
    ******************************************************************************/
    bool VehicleCom::startupAndHealthSupTest() const
    {
      // Default to true if not an EMD loco
      bool startUpTestOk = true;

      bool connected = connectedVehicleComm();

      const LocoTypeAdap locoType = static_cast<LocoTypeAdap>(Config::instance().getLocoType());

      // Check if we have an EMD loco (LIG installed) -> Return true if connection with LIG is established
      if (locoType == EMD)
      {
        startUpTestOk = connected;
      }
      else
      {
        if (connected)
        {
          // Something in configuration is wrong if connection with LIG is established and not an EMD loco.
          ATC::AbstractEventHandler::corePtr()->reportEvent(faultyLigConfig, __FILE__, __LINE__);
          startUpTestOk = false;
        }
      }

      return(startUpTestOk);
    }

    /******************************************************************************
    * instance
    ******************************************************************************/
    VehicleCom& VehicleCom::instance(void)
    {
      static VehicleCom theOnlyVehicleComInstance;

      return theOnlyVehicleComInstance;
    }
  }
}

//lint +esym(586,snprintf)
