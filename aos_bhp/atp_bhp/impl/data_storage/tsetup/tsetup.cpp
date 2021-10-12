/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of the TSetup adaptation class
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-16    lantback    Created
* 2016-04-26    lantback    Renamed "only" instance
* 2016-08-13    saprasad    Implemented core function in Adaptation parts
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-20    arastogi    Removed the run function
* 2016-10-14    nsyed       Set contanier to ATC::AdaptationContainer during new event creation
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include <vfw_string.h>

#include "tsetup.hpp"
#include "mode_control.hpp"
#include "dmi_bhp_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "brakeability.hpp"
#include "config.hpp"
#include "cross_compare_complex.hpp"
#include "message_handler.hpp"
#include "vehicle_com.hpp"
#include "tsetup_event_ids.hpp"

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
  namespace DS
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    TSetup::TSetup(void) :
      AbstractTSetup(),
      // Creating different set of objects for different type of events
      eventDataValidTSetup(ATC::Event::createSafetyHaltEvent(atpTSetupId, ATC::AdaptationContainer, eventIdDataValidTSetup,
        ATC::NoEB, DMICom::tSetupNotValid, "TSetup  Data is not valid !")),
      eventOutOfIndexTSetup(ATC::Event::createSafetyHaltEvent(atpTSetupId, ATC::AdaptationContainer, eventIdOutOfIndexTSetup,
        ATC::NoEB, DMICom::tSetupVehicleIndexNotInRange, "Vehicle Index is out of Limit !, Car Index:", true)),
      eventOutOfRangeTSetup(ATC::Event::createSafetyHaltEvent(atpTSetupId, ATC::AdaptationContainer, eventIdOutOfRangeTSetup,
        ATC::NoEB, DMICom::tSetupOutOfRange, "Parameter is out of range !")),
      standstillEcpbReportedChangedRunMode(ATC::Event::createStandstillEvent(atpTSetupId, ATC::AdaptationContainer, eventIdStandstillEcpbReportedChangedRunMode,
        ATC::NoSB, 0x0U,"Standstill Event when ECPB Mode reported is not Run Mode! ")),
      brakeHandlingEvent(ATC::Event::createSBReqEvent(atpTSetupId, ATC::AdaptationContainer, eventIdBrakeHandlingEvent,
        ATC::NoSB, DMICom::brakeHandlingEvent, "Brake System changed from ECPB to PB!")),
      ecpbReportedTooLowPercentageOfWorkingBrakes(ATC::Event::createSBReqEvent(atpTSetupId, ATC::AdaptationContainer,
        eventIdEcpbReportedTooLowPercentageOfWorkingBrakes, ATC::DriverSB, DMICom::percentageOfEcpbCarsWithOperationalBrakesTooLow,
        "ECPB Reported Too Low Percentage Of Working Brakes! ")),
      standStillTooLowPercentageOfWorkingBrakes(ATC::Event::createStandstillEvent(atpTSetupId, ATC::AdaptationContainer,
        eventIdStandStillTooLowPercentageOfWorkingBrakes, ATC::DriverSB, DMICom::percentageOfEcpbCarsWithOperationalBrakesTooLow,
        "ECPB Reported Too Low Percentage Of Working Brakes! ")),
      sbDueToLowLambdaPercentage(ATC::Event::createSBReqEvent(atpTSetupId, ATC::AdaptationContainer, eventIdSBLowLambdaPercentage, ATC::DriverSB,
        DMICom::percentageOfEcpbCarsWithOperationalBrakesTooLow, "Low lambda calculated for new percentage of working brakes!")),
      standStillDueToLowLambdaPercentage(ATC::Event::createStandstillEvent(atpTSetupId, ATC::AdaptationContainer,
        eventIdStandStillLowLambdaPercentage, ATC::DriverSB, DMICom::percentageOfEcpbCarsWithOperationalBrakesTooLow,
        "Low lambda calculated for new percentage of working brakes!")),
      sbEcpbReportedChangedRunMode(ATC::Event::createSBReqEvent(atpTSetupId, ATC::AdaptationContainer, eventIdSBEcpbReportedChangedRunMode,
        ATC::NoSB, DMICom::ecpbReportedChangedRunMode, "SB Event when ECPB Mode reported is not Run Mode! ")),     
      newBrakeSystemUpdated(ATC::Event::createLogEvent(atpTSetupId, ATC::AdaptationContainer, eventIdBrakeSystemUpdated, DMICom::brakeSystemUpdated,
        "New Brake System updated:",true))
    {
      maxConsecutiveCarsLength = 0U;
    }


    /******************************************************************************
    * Destructor
    ******************************************************************************/
    TSetup::~TSetup()
    {
      //lint --e{1551} Lint is wrong, these destructors don't call any functions that throw exceptions

      for(uint16_t vehicleCount = 0U; vehicleCount < maxVehicleCount; vehicleCount++)
      {
        delete vehicleSetupStorage[vehicleCount].data;
        delete vehiclePreliminarySetupStorage[vehicleCount].data;
      }

      delete trainSetupStorage.data;
      delete trainPreliminarySetupStorage.data;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void TSetup::run(void)
    {
      AbstractTSetup::run();

      handleBrakeBrakeSystemInUse();
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void TSetup::initCrossCompare() const
    {
      AbstractTSetup::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      // Add all events...
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&eventDataValidTSetup));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&eventOutOfIndexTSetup));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&eventOutOfRangeTSetup));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&standstillEcpbReportedChangedRunMode));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&brakeHandlingEvent));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&ecpbReportedTooLowPercentageOfWorkingBrakes));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&standStillTooLowPercentageOfWorkingBrakes));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&sbDueToLowLambdaPercentage));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&standStillDueToLowLambdaPercentage));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&sbEcpbReportedChangedRunMode));
    }

    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    * (This info is not included in doxygen documentation but may be usefull)
    *
    ******************************************************************************/
    TSetup& TSetup::instance(void)
    {
      static TSetup theOnlyTSetupInstance;

      return theOnlyTSetupInstance;
    }

    /******************************************************************************
    * initOnce
    ******************************************************************************/
    void TSetup::initOnce(void)
    {
      AbstractTSetup::initOnce();

      // Instantiate and init brakeability
      brakeAbilityBhp.init();

      for (uint16_t vehicleCount = 0U; vehicleCount < maxVehicleCount; vehicleCount++)
      {
        vehicleSetupStorage[vehicleCount].data = new VehicleSetup;
        vehiclePreliminarySetupStorage[vehicleCount].data = new VehicleSetup;

        vehicleSetupStorage[vehicleCount].invalidate();
        vehiclePreliminarySetupStorage[vehicleCount].invalidate();
      }

      trainSetupStorage.data = new TrainSetup;
      trainSetupStorage.invalidate();

      trainPreliminarySetupStorage.data = new TrainSetup;
      trainPreliminarySetupStorage.invalidate();
    }

    /******************************************************************************
    * getBrakeabilityObject()
    ******************************************************************************/
    const Brakeability& TSetup::getBrakeabilityObject() const
    {
      return brakeAbilityBhp;
    }

    /******************************************************************************
    * setMaxConsectiveCarLength()
    ******************************************************************************/
    void TSetup::setMaxConsectiveCarLength(const uint32_t maxCarLength)
    {
      maxConsecutiveCarsLength = maxCarLength;
    }

    /******************************************************************************
    * getMaxConsecutiveCarLength()
    ******************************************************************************/
    uint32_t TSetup::getMaxConsecutiveCarLength()
    {
      uint32_t val = 0U;
      const LocoTypeAdap locoType = static_cast<LocoTypeAdap>(Config::instance().getLocoType());
      if (locoType != EMD)
      {
       // If locomotive type is NOT EMD, L is the train length 
        //from L_TRAIN received in the TrainSetup message from TCC.
        val = AbstractTSetup::getMaxConsecutiveCarLength();
      }
      else
      {
        //if locomotive type is EMD, L is the longest length of consecutive cars(not including any loco).
        val = maxConsecutiveCarsLength;
      }
      return val;
    }

    /******************************************************************************
    * removeTrainSetup
    ******************************************************************************/
    void TSetup::removeTrainSetup()
    {
      AbstractTSetup::removeTrainSetup();

      brakeAbilityBhp.invalidate();
    }

    /******************************************************************************
    * setBrakeData()
    ******************************************************************************/
    void TSetup::setBrakeData(
      const int32_t trainDynamicWeightLoaded,
      const int32_t trainDynamicWeightEmpty,
      const int32_t locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
      const int32_t locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems],
      const int32_t carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
      const int32_t carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems])
    {
      AbstractTSetup::setBrakeData(
        trainDynamicWeightLoaded,
        trainDynamicWeightEmpty,
        locomotiveBrakeWeightLoadedBrakeSystem,
        locomotiveBrakeWeightEmptyBrakeSystem,
        carsBrakeWeightLoadedBrakeSystem,
        carsBrakeWeightEmptyBrakeSystem);

      brakeAbilityBhp.setBrakeData(
        trainDynamicWeightLoaded,
        trainDynamicWeightEmpty,
        locomotiveBrakeWeightLoadedBrakeSystem,
        locomotiveBrakeWeightEmptyBrakeSystem,
        carsBrakeWeightLoadedBrakeSystem,
        carsBrakeWeightEmptyBrakeSystem);

      const LocoTypeAdap locoType = static_cast<LocoTypeAdap>(Config::instance().getLocoType());
      const BrakeSystemType brakeSystemInUse = brakeAbilityBhp.getBrakeSystemInUse();
      const BrakeSystemType newBrakeSystemType = TG::VehicleCom::instance().getBrakeSystem();

      if ((locoType == EMD) && (brakeSystemInUse != newBrakeSystemType))
      {
        const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
        // PB is only allowed to change to at standstill
        if ((newBrakeSystemType == BrakeSystemType2) || isStandStill)
        {
          updateBrakeSystemType(newBrakeSystemType);
        }
      }
    }

    /******************************************************************************
    * updateBrakeSystemType
    ******************************************************************************/
    void TSetup::updateBrakeSystemType(const BrakeSystemType newBrakeSystemType)
    {
      brakeAbilityBhp.updateBrakeSystemType(newBrakeSystemType);

      //prepare the dynamic text to be send while creating a log event
      char_t buffer[maxBrakeSystemTextSize];
      memset(&buffer[0], 0, sizeof(buffer));
      getBrakeSystemAsText(newBrakeSystemType, &buffer[0]);
      newBrakeSystemUpdated.setDynamicText(&buffer[0]);
      //Raise Log event  and send DMI text
      ATC::AbstractEventHandler::corePtr()->reportEvent(newBrakeSystemUpdated, __FILE__, __LINE__);
    }

    /******************************************************************************
    * handleBrakeBrakeSystemInUse
    ******************************************************************************/
    void TSetup::handleBrakeBrakeSystemInUse()
    {
      const LocoTypeAdap locoType = static_cast<LocoTypeAdap>(Config::instance().getLocoType());

      if (isTrainSetupValid())
      {
        // Check if we have an EMD loco, only EMD can change brake system type...
        if (locoType == EMD)
        {
          // EMD: Brake system type 1, if the stored brake system in use is Pneumatic
          //      Brake system type 2 if the stored brake system in use is ECPB
          // TMM: Brake system type 1
          // Hi - Rail: Brake system type 1

          // Check if the train is at standstill
          const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
          const BrakeSystemType brakeSystemInUse = brakeAbilityBhp.getBrakeSystemInUse();
          const BrakeSystemType newBrakeSystemType = TG::VehicleCom::instance().getBrakeSystem();

          // Check if we have a different reported brake system type
          if ((brakeSystemInUse == BrakeSystemType2) && (newBrakeSystemType == BrakeSystemType1))
          {
            if (isStandStill)
            {
              // Only at standstill AOS changes the current brake system in use to Pneumatic if LCS reports BrakeSystemInUse Pneumatic on interface
              updateBrakeSystemType(newBrakeSystemType);
            }
            else
            {
              // The AOS shall issue a Brake event if
              //  * The configured locomotive type is EMD AND
              //  * The current brake system in use is ECPB AND
              //  * The Brake system in use reported in Train Status Message on the LCS interface is Pneumatic.
              ATC::AbstractEventHandler::corePtr()->reportEvent(brakeHandlingEvent, __FILE__, __LINE__);
            }
          }
          else if (newBrakeSystemType != brakeSystemInUse)
          {
            updateBrakeSystemType(newBrakeSystemType);
          }
          else
          {
            // Do nothing, same brake system as in use...
          }

          if (newBrakeSystemType == BrakeSystemType2)
          {
            bool percentageValid = true;
            // If we don't have cars, there is no need to set the percentage of operative brakes.
            if (trainSetupStorage.data->vehicleCount > 1U)
            {
              const uint8_t percentageOfOperativeBrakesEcpb = TG::VehicleCom::instance().getPercentageOfOperativeBrakesEcpb();

              // The AOS shall issue a Brake event if:
              // * The configured locomotive type is EMD AND
              // * The current brake system in use is ECPB AND
              // * The Percentage of operative brakes, ECPB, from LCS is below the configurable Percentage of connected ECPB
              //   cars with operational brakes required for operation.
              percentageValid = brakeAbilityBhp.validatePercentageOfOperativeBrakes(percentageOfOperativeBrakesEcpb);

              if (percentageValid)
              {
                brakeAbilityBhp.setPercentageOfOperativeBrakes(percentageOfOperativeBrakesEcpb);
              }
              else if (!isStandStill)
              {
                //If train is not stand still raise SB
                ATC::AbstractEventHandler::corePtr()->reportEvent(ecpbReportedTooLowPercentageOfWorkingBrakes, __FILE__, __LINE__);
              }
              else
              {
                //Raise standstill 
                ATC::AbstractEventHandler::corePtr()->reportEvent(standStillTooLowPercentageOfWorkingBrakes, __FILE__, __LINE__);
              }
            }

            // Only check lambda if percentage is OK
            if ((!brakeAbilityBhp.isValidLambda()) && percentageValid)
            {
              // If calculated lambda (after receiving changed operating brakes percentage from LCS) is less then
              // configured minimum lambda
              if (!isStandStill)
              {
                // If train is not stand still raise SB
                ATC::AbstractEventHandler::corePtr()->reportEvent(sbDueToLowLambdaPercentage, __FILE__, __LINE__);
              }
              else
              {
                // Raise standstill 
                ATC::AbstractEventHandler::corePtr()->reportEvent(standStillDueToLowLambdaPercentage, __FILE__, __LINE__);
              }
            }

            // Fetch ECPB Operating Mode
            TG::EcpbOperatingModesType ecpbOperatingMode;
            bool ecpbOperatingModesValid = TG::VehicleCom::instance().getEcpbOperatingModes(ecpbOperatingMode);

            if (ecpbOperatingModesValid && (TG::EcpbOperatingRunMode != ecpbOperatingMode))
            {
              // The AOS shall issue a Standstill event as long as the following conditions exist :
              // * The configured locomotive type is EMD AND
              // * The current brake system in use is ECPB AND
              // * The ECPB mode reported in Train Status Message on the LCS interface is NOT Run mode.
              if (!isStandStill)
              {
                // If train is not stand still raise SB
                ATC::AbstractEventHandler::corePtr()->reportEvent(sbEcpbReportedChangedRunMode, __FILE__, __LINE__);
              }
              else
              {
                // Raise standstill 
                ATC::AbstractEventHandler::corePtr()->reportEvent(standstillEcpbReportedChangedRunMode, __FILE__, __LINE__);
              }
            }
          }
        }
      }
    }


    /******************************************************************************
    * getBrakeSystemAsText
    ******************************************************************************/
    void TSetup::getBrakeSystemAsText(const BrakeSystemType brakeSystem ,char_t* const buffer) const
    {
      switch (brakeSystem)
      {
      case BrakeSystemTypeUndefined:
        static_cast<void>(vfw_strlcpy(buffer, "Undefined", maxBrakeSystemTextSize));
        break;

      case BrakeSystemType1:
        static_cast<void>(vfw_strlcpy(buffer, "Pneumatic", maxBrakeSystemTextSize));
        break;

      case BrakeSystemType2:
        static_cast<void>(vfw_strlcpy(buffer, "ECPB", maxBrakeSystemTextSize));
        break;

      case BrakeSystemType3:
        static_cast<void>(vfw_strlcpy(buffer, "Not Used", maxBrakeSystemTextSize));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "Unknown", maxBrakeSystemTextSize));
        break;
      }
    }
  } // namespace DS
} // namespace ATP
