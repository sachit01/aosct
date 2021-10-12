/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of AbstractTSetup
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-16    lantback    Created
* 2016-04-26    lantback    Use ATC::ProcComponent, init to return bool, corePtr()
* 2016-08-13    saprasad    Implemented core functionality in Core part
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-19    arastogi    Added mode checks to access tsetup. Fixed bugs.
*                           Added Console call
* 2016-10-03    arastogi    Fixed bug in setBrakeability function.
* 2016-10-12    arastogi    Fixed the bug to do range checks for empty tsetup or
*                           tsetup from DMI
* 2016-10-18    arastogi    Added function getLocovsTrainOrientation
* 2017-03-29    skothiya    Modified function consoleCall to display train name on console
* 2017-04=25    skothiya    Modified temporary train setup to preliminary train setup
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_string.h>
#include <cstdio>
#include "abstract_tsetup.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_console.hpp"
#include "atc_util.hpp"
#include "abstract_config.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "brakeability.hpp"
#include "brakeability_brake_system.hpp"
#include "cross_compare_array.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_tsetup_event_ids.hpp"

#include <vfw_checkpoints.h>

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
    AbstractTSetup::AbstractTSetup() : ATC::ProcComponent(atpTSetupId, "TSetup", "TS"),
      // creating different set of objects for different type of events
      eventModConfFailAbstTSetup(ATC::Event::createSafetyHaltEvent(atpTSetupId, ATC::CoreContainer, eventIdReportEventModeConfFailed,
        ATC::NoEB, DMICom::tSetupAtpModFailure, "AbstractTSetup: Wrong mode or state!")),
      eventOutOfRangeAbstTSetup(ATC::Event::createSafetyHaltEvent(atpTSetupId, ATC::CoreContainer, eventIdReportEventOutOfRange,
        ATC::NoEB, DMICom::tSetupAtpVehicleIndexNotInRange, "AbstractTSetup: Parameter is out of range!")),
      eventStorgeNotValAbstTSetup(ATC::Event::createSafetyHaltEvent(atpTSetupId, ATC::CoreContainer, eventIdReportEventStorgeNotValid,
        ATC::NoEB, DMICom::tSetupAtpNotValid, "AbstractTSetup: Storage is not valid!")),
      initDone(false),
      oldChangeCounter(0U),
      oldSbResponseTime(0U),
      trainLoadedStatus(TrainIsLoaded),
      trainWeightLoaded(0),
      trainWeightEmpty(0)
    {
      if (coreTSetupInstancePtr != 0)
      {
        // Report error to event handler
        ATC::aosHalt(__FILE__, __LINE__, "Tsetup Constructor already instantiated");
      }
      // Setup single instance pointer for core access
      coreTSetupInstancePtr = this;

      //default orientation is B end facing cars
      locoOrientation = locoVsTrainADirection;
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractTSetup::init(void)
    {
      if (!initDone)
      {
        initDone = true;

        initOnce();
        initCrossCompare();
      }
      return initDone;
    }

    /******************************************************************************
    * VehicleSetupStorage::VehicleSetupStorage()
    ******************************************************************************/
    AbstractTSetup::VehicleSetupStorage::VehicleSetupStorage()
    {
      data = static_cast<VehicleSetup*>(NULL);
      dataValid = false;
    }

    /******************************************************************************
    * TrainSetupStorage::TrainSetupStorage()
    ******************************************************************************/
    AbstractTSetup::TrainSetupStorage::TrainSetupStorage()
    {
      data = static_cast<TrainSetup*>(NULL);
      dataValid = false;
      changed = false;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void AbstractTSetup::run(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "TS_run");

      const ATPMode modeType = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      if (ATPModeConfiguration == modeType)
      {
        Kernel::TrainConfigModeState configModeState =
          Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();

        switch (configModeState)
        {
        case Kernel::TrainConfigMode::trainConfigStart:
          trainSetupStorage.invalidate();
          trainPreliminarySetupStorage.invalidate();
          for (uint16_t i = 0U; i < maxVehicleCount; ++i)
          {
            vehicleSetupStorage[i].invalidate();
            vehiclePreliminarySetupStorage[i].invalidate();
          }
          trainSetupStorage.changed = false;
          trainSetupStorage.data->changeDetails.brakeAbility = false;
          trainSetupStorage.data->changeDetails.brakeResponseTime = false;
          break;

        case Kernel::TrainConfigMode::trainConfigTSetupAccepted:
          trainPreliminarySetupStorage.invalidate();
          for (uint16_t i = 0U; i < maxVehicleCount; ++i)
          {
            vehiclePreliminarySetupStorage[i].invalidate();
          }
          break;

        default:
          break;
        }
      }
      else
      {
        trainSetupStorage.changed = false;
        trainSetupStorage.data->changeDetails.brakeAbility = false;
        trainSetupStorage.data->changeDetails.brakeResponseTime = false;
      }

      //set the counter to the current value to reset if the brake ability has changed.
      oldChangeCounter = getBrakeabilityObject().getChangeCounter();

      //set the response time to the current value to reset if the brake response has changed.
      oldSbResponseTime = getServiceBrakeResponseTime();
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractTSetup* AbstractTSetup::corePtr(void)
    {
      return coreTSetupInstancePtr;
    }

    /******************************************************************************
    * getTrainSetup()
    ******************************************************************************/
    const TrainSetup* const AbstractTSetup::getTrainSetup() const
    {
      const TrainSetup* pTrainSetup = static_cast<const TrainSetup*>(NULL);

      if (trainSetupStorage.dataValid)
      {
        pTrainSetup = trainSetupStorage.data;
      }
      return pTrainSetup;
    }

    /******************************************************************************
    * setTrainSetup()
    ******************************************************************************/
    bool AbstractTSetup::setTrainSetup(const TrainSetup &trainSetup)
    {
      bool operationSucceed = false;
      ATPMode modeType = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      if ((ATPModeConfiguration == modeType) || (ATPModeShuntingRoute == modeType) || (ATPModeStaffResponsible == modeType) ||
        (ATPModeNormal == modeType))
      {
        // Limit maxSpeed to AbstractConfig::getMaxSpeed()
        TrainSetup newTrainSetup = trainSetup;

        if ((isAnyTSetUpFieldChange(newTrainSetup) && trainSetupStorage.dataValid) ||
          (!trainSetupStorage.dataValid))
        {
          TrainSetup *pTrainSetup = trainSetupStorage.data;

          if (pTrainSetup != NULL)
          {
            if (!isAnyFieldOutOfRange(newTrainSetup))
            {
              pTrainSetup->vehicleCount = newTrainSetup.vehicleCount;
              pTrainSetup->changeDetails = newTrainSetup.changeDetails;
              pTrainSetup->length = newTrainSetup.length;
              pTrainSetup->maxSpeed = newTrainSetup.maxSpeed;
              pTrainSetup->orientation = newTrainSetup.orientation;
              pTrainSetup->timsSupNeeded = newTrainSetup.timsSupNeeded;
              pTrainSetup->state = newTrainSetup.state;

              trainSetupStorage.dataValid = true;
              trainSetupStorage.changed = true;
              trainSetupStorage.data->changeDetails.brakeAbility = true;
              trainSetupStorage.data->changeDetails.brakeResponseTime = true;
              operationSucceed = true;

              // if the A end facing vehicle
              if ((trainLocoOrientation & pTrainSetup->orientation) > 0U)
              {
                // set the B end as front side
                locoOrientation = locoVsTrainBDirection;
              }
              else
              { // set the A end as front side
                locoOrientation = locoVsTrainADirection;
              }

              trace.write(1U, "Train-setup is changed\n");
            }
            else
            {
              // Report event out of range
              ATC::AbstractEventHandler::corePtr()->reportEvent(eventOutOfRangeAbstTSetup, __FILE__,
                __LINE__);
            }
          }
        }
        else
        {
          operationSucceed = true;
        }
      }
      else
      {
        // Report event ,not allowed to over-write
        ATC::AbstractEventHandler::corePtr()->reportEvent(eventModConfFailAbstTSetup, __FILE__,
          __LINE__);
      }
      return operationSucceed;
    }

    /******************************************************************************
    * getMaxConsecutiveCarLength
    ******************************************************************************/
    uint32_t AbstractTSetup::getMaxConsecutiveCarLength()
    {
      TrainSetup *pTrainSetup = trainSetupStorage.data;
      return pTrainSetup->length;
    }

    /******************************************************************************
    * isAnyTSetUpFieldChange
    ******************************************************************************/
    bool AbstractTSetup::isAnyTSetUpFieldChange(const TrainSetup& trainSetup) const
    {
      TrainSetup *pTrainSetup = trainSetupStorage.data;
      bool anySetupFieldChanged = false;
      if (pTrainSetup != NULL)
      {
        if ((pTrainSetup->state != trainSetup.state) || (
          (pTrainSetup->vehicleCount != trainSetup.vehicleCount)
          || (pTrainSetup->length != trainSetup.length)
          || (pTrainSetup->maxSpeed != trainSetup.maxSpeed)
          || (pTrainSetup->orientation != trainSetup.orientation)
          || (pTrainSetup->timsSupNeeded != trainSetup.timsSupNeeded)))
        {
          anySetupFieldChanged = true;
        }
      }
      return anySetupFieldChanged;
    }

    /******************************************************************************
    * isAnyFieldOutOfRange
    ******************************************************************************/
    bool AbstractTSetup::isAnyFieldOutOfRange(const TrainSetup& trainSetup) const
    {
      bool anyFieldOutOfRange = false;

      const uint16_t minTrainLength = AbstractConfig::corePtr()->getBalAntennaPosEnd() +
        AbstractConfig::corePtr()->getBalAntennaPosFront();

      if ((trainSetup.vehicleCount < minVehicleCount)
        || (trainSetup.vehicleCount > maxVehicleCount)
        || (trainSetup.length < minTrainLength)) // Only check min length, there is no max value here
      {
        anyFieldOutOfRange = true;
      }

      return anyFieldOutOfRange;
    }

    /******************************************************************************
    * getTrainName
    ******************************************************************************/
    bool AbstractTSetup::getTrainName(char_t* const trainName) const
    {
      bool operationSucceed = false;
      if (trainNameStorage.dataValid)
      {
        static_cast<void>(vfw_strlcpy(trainName, &trainNameStorage.trainName[0], trainNameMaxLength + 1U));
        operationSucceed = true;
      }
      return operationSucceed;
    }

    /******************************************************************************
    * setTrainName
    ******************************************************************************/
    bool AbstractTSetup::setTrainName(const char_t* const trainName)
    {
      bool operationSucceed = false;
      // trainName buffer should be trainNameMaxLength+1 
      const uint32_t trainNamelength = static_cast<uint32_t>(strnlen(trainName, trainNameMaxLength));

      if (trainNamelength < sizeof(trainNameStorage.trainName))
      {
        if ((strncmp(&trainNameStorage.trainName[0], trainName, trainNameMaxLength)) == 0)
        {
          AbstractTSetup::setTrainNameChanged(false);
        }
        else
        {
          AbstractTSetup::setTrainNameChanged(true);
        }

        static_cast<void>(vfw_strlcpy(&trainNameStorage.trainName[0], trainName, sizeof(trainNameStorage.trainName)));
        trainNameStorage.dataValid = true;
        operationSucceed = true;
        trace.write(1U, "Train name is changed\n");
      }
      return operationSucceed;
    }

    /*********************************************************************************
    *trainNameChanged
    **********************************************************************************/
    bool AbstractTSetup::trainNameChanged(void) const
    {
      return trainNameStorage.changed;
    }

    /*********************************************************************************
    *setTrainNameChanged
    **********************************************************************************/
    void AbstractTSetup::setTrainNameChanged(const bool statusChange)
    {
      trainNameStorage.changed = statusChange;
    }

    /*********************************************************************************
    *getTrainLoadStatus
    **********************************************************************************/
    TrainLoaded AbstractTSetup::getTrainLoadStatus(void) const
    {
      return trainLoadedStatus;
    }

    /*********************************************************************************
    *setTrainLoadStatus
    **********************************************************************************/
    void AbstractTSetup::setTrainLoadStatus(const TrainLoaded loadStatus)
    {
      trainLoadedStatus = loadStatus;

      getBrakeabilityObject().setBrakeDataTrainLoaded(loadStatus);
    }

    /*********************************************************************************
    * getTrainWeight
    **********************************************************************************/
    int32_t AbstractTSetup::getTrainWeight() const
    {
      int32_t trainWeight;

      if (trainLoadedStatus == TrainIsLoaded)
      {
        trainWeight = trainWeightLoaded;
      }
      else
      {
        trainWeight = trainWeightEmpty;
      }

      return trainWeight;
    }

    /******************************************************************************
    * getVehicleSetup
    ******************************************************************************/
    bool AbstractTSetup::getVehicleSetup(const uint16_t vehicleIndex, VehicleSetup &vehicleSetup) const
    {
      bool operationSucceed = false;
      if (vehicleIndex < maxVehicleCount)
      {
        VehicleSetup *pVehicleSetup = vehicleSetupStorage[vehicleIndex].data;
        if ((vehicleSetupStorage[vehicleIndex].dataValid) && (pVehicleSetup != NULL))
        {
          vehicleSetup = *pVehicleSetup;
          operationSucceed = true;
        }
      }
      else
      {
        // Report error to event handler
        ATC::AbstractEventHandler::corePtr()->reportEvent(eventStorgeNotValAbstTSetup, __FILE__,
          __LINE__);
      }
      return operationSucceed;
    }

    /******************************************************************************
    * setVehicleSetup
    ******************************************************************************/
    bool AbstractTSetup::setVehicleSetup(const uint16_t vehicleIndex, const VehicleSetup &vehicleSetup)
    {
      bool operationSucceed = false;
      ATPMode modeType = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      Kernel::TrainConfigModeState configModeState =
        Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();

      // Should only be allowed to write the vehicle setup when in config mode and TSetup is
      // confirmed by TCC.
      if (((ATPModeConfiguration == modeType) &&
        ((configModeState == Kernel::TrainConfigMode::trainConfigConfirmNewConfigFrmTCC) ||
        (configModeState == Kernel::TrainConfigMode::trainConfigWaitSetupFrmTCC) ||
          (configModeState == Kernel::TrainConfigMode::trainConfigConfirmReRegFrmDMI))) || (ATPModeStaffResponsible == modeType)
        || (ATPModeNormal == modeType) || (ATPModeShuntingRoute == modeType))
      {
        if (vehicleIndex < maxVehicleCount)
        {
          VehicleSetup *pVehicleSetup = vehicleSetupStorage[vehicleIndex].data;
          if (pVehicleSetup != NULL)
          {
            vehicleSetupStorage[vehicleIndex].dataValid = true;

            char_t *desPtr = pVehicleSetup->vehicleName;
            const char_t *srcPtr = vehicleSetup.vehicleName;
            static_cast<void>(vfw_strlcpy(desPtr, srcPtr, sizeof(pVehicleSetup->vehicleName)));
            pVehicleSetup->vehicleType = vehicleSetup.vehicleType;
            pVehicleSetup->nodeAdress = vehicleSetup.nodeAdress;
            operationSucceed = true;
            trace.write(1U, "Vehicle setup is changed for index\n", static_cast<uint32_t>(vehicleIndex));
          }
        }
        else
        {
          // Report error to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(eventStorgeNotValAbstTSetup, __FILE__,
            __LINE__);
        }
      }
      else
      {
        // Report event ,not allowed to over-write
        ATC::AbstractEventHandler::corePtr()->reportEvent(eventModConfFailAbstTSetup, __FILE__,
          __LINE__);
      }
      return operationSucceed;
    }

    /*************************************************/
    /* Interface functions for Temporary Train Setup */
    /*************************************************/
    /******************************************************************************
    * getPreliminaryTrainSetup
    ******************************************************************************/
    bool AbstractTSetup::getPreliminaryTrainSetup(TrainSetup &trainSetup) const
    {
      bool operationSucceed = false;
      ATPMode modeType = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      // Should only be allowed to read Preliminary vehicle storage when in config mode
      if (ATPModeConfiguration == modeType)
      {
        if (trainPreliminarySetupStorage.dataValid)
        {
          trainSetup = *(trainPreliminarySetupStorage.data);
          operationSucceed = true;
        }
      }

      return operationSucceed;
    }

    /******************************************************************************
    * setPreliminaryTrainSetup
    ******************************************************************************/
    bool AbstractTSetup::setPreliminaryTrainSetup(const TrainSetup &trainSetup)
    {
      bool isTsetupOk = false;

      ATPMode modeType = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      Kernel::TrainConfigModeState configModeState =
        Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();
      const bool isModeStateConfigWaitSetupFrmTCC = (configModeState == Kernel::TrainConfigMode::trainConfigWaitSetupFrmTCC);
      const bool isModeStateConfigWaitTIC = (configModeState == Kernel::TrainConfigMode::trainConfigWaitTIC);
      // Is train in config mode and waiting for TSetup
      if ((ATPModeConfiguration == modeType) && (isModeStateConfigWaitSetupFrmTCC || isModeStateConfigWaitTIC))
      {
        // The message handler sets the Qsetup reason after this call.
        isTsetupOk = true;
      }

      // If its in config and the train setup is from DMI
      else if ((ATPModeConfiguration == modeType) &&
        (configModeState == Kernel::TrainConfigMode::trainConfigWaitNewConfigDMI))
      {
        // check for the valid values.
        if ((trainSetup.vehicleCount < minVehicleCount)
          || (trainSetup.vehicleCount > maxVehicleCount))
        {
          isTsetupOk = false;
          // Report event out of range
          ATC::AbstractEventHandler::corePtr()->reportEvent(eventOutOfRangeAbstTSetup, __FILE__,
            __LINE__);
        }
        else
        {
          isTsetupOk = true;
        }
      }
      else
      {
        // Report event ,not allowed to over-write
        ATC::AbstractEventHandler::corePtr()->reportEvent(eventModConfFailAbstTSetup, __FILE__,
          __LINE__);
      }

      // if the train setup is OK, overwrite
      if (isTsetupOk)
      {
        trainPreliminarySetupStorage.dataValid = true;
        TrainSetup *pTrainSetup = trainPreliminarySetupStorage.data;
        pTrainSetup->vehicleCount = trainSetup.vehicleCount;
        pTrainSetup->changeDetails = trainSetup.changeDetails;
        pTrainSetup->length = trainSetup.length;
        pTrainSetup->maxSpeed = trainSetup.maxSpeed;
        pTrainSetup->orientation = trainSetup.orientation;
        pTrainSetup->state = trainSetup.state;
        pTrainSetup->timsSupNeeded = trainSetup.timsSupNeeded;

        trace.write(1U, "Preliminary vehicle setup is written\n");
      }

      return isTsetupOk;
    }

    /******************************************************************************
    * getPreliminaryVehicleSetup
    ******************************************************************************/
    bool AbstractTSetup::getPreliminaryVehicleSetup(const uint16_t vehicleIndex, VehicleSetup &vehicleSetup) const
    {
      bool operationSucceed = false;
      ATPMode modeType = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      // Should only be allowed to read Preliminary vehicle storage when in config mode
      if (ATPModeConfiguration == modeType)
      {
        if (vehicleIndex < maxVehicleCount)
        {
          if (vehiclePreliminarySetupStorage[vehicleIndex].dataValid)
          {
            const VehicleSetup* const pVehicleSetup = vehiclePreliminarySetupStorage[vehicleIndex].data;
            if (pVehicleSetup != NULL)
            {
              char_t* const desPtr = vehicleSetup.vehicleName;
              const char_t* const srcPtr = pVehicleSetup->vehicleName;
              static_cast<void>(vfw_strlcpy(desPtr, srcPtr, sizeof(vehicleSetup.vehicleName)));
              vehicleSetup.vehicleType = pVehicleSetup->vehicleType;
              vehicleSetup.nodeAdress = pVehicleSetup->nodeAdress;
              operationSucceed = true;
            }
            else
            {
              // Report error to event handler
              ATC::AbstractEventHandler::corePtr()->reportEvent(eventStorgeNotValAbstTSetup, __FILE__,
                __LINE__);
            }
          }
        }
        else
        {
          // Report error to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(eventOutOfRangeAbstTSetup, __FILE__,
            __LINE__);
        }
      }
      else
      {
        // Do nothing.. should return false.
      }
      return operationSucceed;
    }

    /******************************************************************************
    * setOrientationinPreliminaryTSetup
    ******************************************************************************/
    bool AbstractTSetup::setOrientationinPreliminaryTSetup(const uint8_t orientation)
    {
     
      bool operationSucceed = false;

      ATPMode modeType = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      if (ATPModeConfiguration == modeType)
      {
        // check if train set up is available
        if (trainPreliminarySetupStorage.dataValid)
        {
          trainPreliminarySetupStorage.data->orientation = orientation;
          operationSucceed = true;
        }
      }
      return operationSucceed;
    }


    /******************************************************************************
    * setPreliminaryVehicleSetup
    ******************************************************************************/
    bool AbstractTSetup::setPreliminaryVehicleSetup(const uint16_t vehicleIndex, const VehicleSetup &vehicleSetup)
    {
      bool operationSucceed = false;
      ATPMode modeType = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      Kernel::TrainConfigModeState configModeState =
        Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();

      // Set preliminary train setup only in configuration mode and
      // In case of registration when driver/TIC will send configuration data to TCC for confirmation or
      // In case of reregistration when TCC will send configuration data to AOS 

      if ((ATPModeConfiguration == modeType) &&
        ((configModeState == Kernel::TrainConfigMode::trainConfigWaitSetupFrmTCC) ||
        (configModeState == Kernel::TrainConfigMode::trainConfigWaitNewConfigDMI) ||
          (configModeState == Kernel::TrainConfigMode::trainConfigWaitTIC)))
      {
        if (vehicleIndex < maxVehicleCount)
        {
          VehicleSetup *pVehicleSetup = vehiclePreliminarySetupStorage[vehicleIndex].data;
          if (pVehicleSetup != NULL)
          {
            vehiclePreliminarySetupStorage[vehicleIndex].dataValid = true;

            char_t *desPtr = pVehicleSetup->vehicleName;
            const char_t *srcPtr = vehicleSetup.vehicleName;
            static_cast<void>(vfw_strlcpy(desPtr, srcPtr, sizeof(pVehicleSetup->vehicleName)));
            pVehicleSetup->vehicleType = vehicleSetup.vehicleType;
            pVehicleSetup->nodeAdress = vehicleSetup.nodeAdress;
            operationSucceed = true;
            trace.write(1U, "Preliminary vehicle setup is written for index\n", static_cast<uint32_t>(vehicleIndex));
          }
          else
          {
            // Report error to event handler
            ATC::AbstractEventHandler::corePtr()->reportEvent(eventStorgeNotValAbstTSetup, __FILE__,
              __LINE__);
          }
        }
        else
        {
          // Report error to event handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(eventOutOfRangeAbstTSetup, __FILE__,
            __LINE__);
        }
      }
      else
      {
        // Report event ,not allowed to over-write
        ATC::AbstractEventHandler::corePtr()->reportEvent(eventModConfFailAbstTSetup, __FILE__,
          __LINE__);
      }
      return operationSucceed;
    }


    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractTSetup::VehicleSetupStorage::initCrossCompare() const
    {
      if (data != static_cast<VehicleSetup*>(NULL))
      {
        data->initCrossCompare();
      }

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&dataValid));
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void AbstractTSetup::VehicleSetupStorage::invalidate()
    {
      dataValid = false;
      if (data != static_cast<VehicleSetup*>(NULL))
      {
        data->invalidate();
      }
    }

    /******************************************************************************
    * TrainNameStorage::invalidate
    ******************************************************************************/
    void AbstractTSetup::TrainNameStorage::invalidate()
    {
      dataValid = false;
      trainName[0] = '\0';
      changed = false;
    }

    /******************************************************************************
    * TrainSetupStorage::invalidate
    ******************************************************************************/
    void AbstractTSetup::TrainSetupStorage::invalidate()
    {
      dataValid = false;
      changed = false;
      if (data != static_cast<TrainSetup*>(NULL))
      {
        data->invalidate();
        data->timsSupNeeded = false;
      }
    }

    /******************************************************************************
    * getLocovsTrainOrientation()
    ******************************************************************************/
    LocoVsTrainDir AbstractTSetup::getLocovsTrainOrientation(void) const
    {
      return locoOrientation;
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool AbstractTSetup::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments searches for the "help", "trace" or any other Console
      component specific command calls and handles it. Returns true if completely handled
      else returns false. returning false will let other components handle the call. help always returns false.
      */

      bool retVal = false;
      char_t  buffer[512];
      const char_t* const preLimTrainText = "Preliminary ";
      char_t  emptyText[] = "";
      const char_t* trainText = static_cast<char_t*>(NULL);

      TrainSetupStorage* trainSetupPtr = static_cast<TrainSetupStorage*>(NULL);
      VehicleSetupStorage* vehicleSetupPtr = static_cast<VehicleSetupStorage*>(NULL);
      TrainNameStorage* trainNameptr = static_cast<TrainNameStorage*>(NULL);
      // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        const char_t* const toWrite = "TS            To print the train setup\n"
          "PrelTS        To print the preliminary train setup";

        ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);
        retVal = false;
      }
      // Setup proper references for ordinary setups
      else if (ATC::isTextMatch(&argv[0][0], "TS", sizeof("TS")) && (argc == 1U))
      {
        trainSetupPtr = &trainSetupStorage;
        vehicleSetupPtr = vehicleSetupStorage;
        trainText = emptyText;
        trainNameptr = &trainNameStorage;
      }

      // Setup proper references for Preliminary setups
      else if (ATC::isTextMatch(&argv[0][0], "PrelTS", sizeof("PrelTS")) && (argc == 1U))
      {
        trainSetupPtr = &trainPreliminarySetupStorage;
        vehicleSetupPtr = vehiclePreliminarySetupStorage;
        trainText = preLimTrainText;
        trainNameptr = &trainNameStorage;
      }
      else
      {
        // Do nothing
      }

      // If any of the console commands matched -> Print the ordinary or temporary data on console
      if ((trainSetupPtr != static_cast<TrainSetupStorage*>(NULL)) &&
        (vehicleSetupPtr != static_cast<VehicleSetupStorage*>(NULL)) &&
        (trainNameptr != static_cast<TrainNameStorage*>(NULL)))
      {
        if (trainSetupPtr->dataValid)
        {
          // // trainSetupState
          const char_t* trainSetupState = "Preliminary";
          if (trainSetupPtr->data->state == TrainSetupStatePermanent)
          {
            trainSetupState = "Permanent";
          }
          else if (TrainSetupStateTemporary == trainSetupPtr->data->state)
          {
            trainSetupState = "Temporary";
          }
          else
          {
            // do nothing
          }

          // TrainSetup values
          //lint -e{586} snprintf is needed here
          int32_t ret = snprintf(&buffer[0], sizeof(buffer),
            "%sTrain Setup\nState              : %s\nVehicle count      : %u\nLength             : %u\n"
            "Max Speed          : %u\nOrientation        : %u\nTimsSupNeeded      : %u\nTrain Name         : %s\n",
            trainText, trainSetupState, trainSetupPtr->data->vehicleCount, trainSetupPtr->data->length,
            trainSetupPtr->data->maxSpeed, trainSetupPtr->data->orientation,
            trainSetupPtr->data->timsSupNeeded, trainNameptr->trainName);

          if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);

            // All VehicleSetup values
            for (uint16_t i = 0U; i < trainSetupPtr->data->vehicleCount; ++i)
            {
              if (vehicleSetupPtr[i].dataValid)
              {
                //lint -e{586} snprintf is needed here
                ret = snprintf(&buffer[0], sizeof(buffer), "Vehicle Node Address: %u, Vehicle Type: %u, Vehicle Name: %s",
                  vehicleSetupPtr[i].data->nodeAdress,
                  static_cast<uint8_t>(vehicleSetupPtr[i].data->vehicleType),
                  vehicleSetupPtr[i].data->vehicleName);

                if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
                {
                  ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
                }
              }
            }
          }
        }
        else
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline("Setup is not valid");
        }
        retVal = true;
      }

      return retVal;
    }

    /******************************************************************************
    * removeTrainSetup
    ******************************************************************************/
    void AbstractTSetup::removeTrainSetup(void)
    {
      setTrainLoadStatus(TrainIsLoaded);
      trainSetupStorage.invalidate();
      trainPreliminarySetupStorage.invalidate();

      if (trainNameStorage.dataValid)
      {
        trainNameStorage.invalidate();
        setTrainNameChanged(true);
      }

      for (uint16_t i = 0U; i < maxVehicleCount; ++i)
      {
        vehicleSetupStorage[i].invalidate();
        vehiclePreliminarySetupStorage[i].invalidate();
      }
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractTSetup::initCrossCompare() const
    {
      for (uint16_t i = 0U; i < maxVehicleCount; ++i)
      {
        vehicleSetupStorage[i].initCrossCompare();
        vehiclePreliminarySetupStorage[i].initCrossCompare();
      }

      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&initDone));
      // trainNameStorage
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&trainNameStorage.dataValid));
      crossCompare->addCrossCompareData(new Support::CrossCompareArray<char_t>(&trainNameStorage.trainName[0], trainNameMaxLength + 1U));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&trainNameStorage.changed));

      // trainSetupStorage
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&trainSetupStorage.dataValid));
      trainSetupStorage.data->initCrossCompare();
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&trainSetupStorage.changed));

      // trainPreliminarySetupStorage
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&trainPreliminarySetupStorage.dataValid));
      trainPreliminarySetupStorage.data->initCrossCompare();
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&trainPreliminarySetupStorage.changed));

      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventModConfFailAbstTSetup));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventOutOfRangeAbstTSetup));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventStorgeNotValAbstTSetup));

      getBrakeabilityObject().initCrossCompare();
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&oldChangeCounter));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&oldSbResponseTime));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<TrainLoaded>(&trainLoadedStatus));
      crossCompare->addCrossCompareData(new Support::CrossCompareInt32(&trainWeightLoaded));
      crossCompare->addCrossCompareData(new Support::CrossCompareInt32(&trainWeightEmpty));
    }

    /******************************************************************************
    * initOnce
    ******************************************************************************/
    void AbstractTSetup::initOnce()
    {
      trainSetupStorage.invalidate();
      trainNameStorage.invalidate();
      trainPreliminarySetupStorage.invalidate();
      trainLoadedStatus = TrainIsLoaded;
    }

    /******************************************************************************
    * isTrainSetupValid
    ******************************************************************************/
    bool AbstractTSetup::isTrainSetupValid()const
    {
      bool validTrainSetup = false;
      if (trainSetupStorage.dataValid)
      {
        validTrainSetup = true;
      }
      return validTrainSetup;
    }

    /******************************************************************************
    * validateLambda
    ******************************************************************************/
    bool AbstractTSetup::validateLambda(
      const int32_t trainDynamicWeightLoaded,
      const int32_t trainDynamicWeightEmpty,
      const int32_t locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
      const int32_t locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems],
      const int32_t carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
      const int32_t carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems]) const
    {
      return getBrakeabilityObject().validateLambda(
        trainDynamicWeightLoaded,
        trainDynamicWeightEmpty,
        locomotiveBrakeWeightLoadedBrakeSystem,
        locomotiveBrakeWeightEmptyBrakeSystem,
        carsBrakeWeightLoadedBrakeSystem,
        carsBrakeWeightEmptyBrakeSystem);
    }

    /******************************************************************************
    * isBrakeDataEqualOrBetter()
    ******************************************************************************/
    bool AbstractTSetup::isBrakeDataEqualOrBetter(
      const int32_t    trainDynamicWeightLoaded,
      const int32_t    trainDynamicWeightEmpty,
      const int32_t    locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
      const int32_t    locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems],
      const int32_t    carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
      const int32_t    carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems]) const
    {
      return getBrakeabilityObject().isBrakeDataEqualOrBetter(
        trainDynamicWeightLoaded,
        trainDynamicWeightEmpty,
        locomotiveBrakeWeightLoadedBrakeSystem,
        locomotiveBrakeWeightEmptyBrakeSystem,
        carsBrakeWeightLoadedBrakeSystem,
        carsBrakeWeightEmptyBrakeSystem);
    }

    /******************************************************************************
    * setBrakeData()
    ******************************************************************************/
    void AbstractTSetup::setBrakeData(
      const int32_t trainDynamicWeightLoaded,
      const int32_t trainDynamicWeightEmpty,
      const int32_t /*locomotiveBrakeWeightLoadedBrakeSystem*/[maxBrakeSystems],
      const int32_t /*locomotiveBrakeWeightEmptyBrakeSystem*/[maxBrakeSystems],
      const int32_t /*carsBrakeWeightLoadedBrakeSystem*/[maxBrakeSystems],
      const int32_t /*carsBrakeWeightEmptyBrakeSystem*/[maxBrakeSystems])
    {
      trainWeightLoaded = trainDynamicWeightLoaded;
      trainWeightEmpty = trainDynamicWeightEmpty;
    }

    /******************************************************************************
    * getWorstBrakeabilityInRange()
    ******************************************************************************/
    int32_t AbstractTSetup::getWorstBrakeabilityInRange(const uint32_t v1, const uint32_t v2) const
    {
      return getBrakeabilityObject().getBrakeabilityBrakeSystemInUse()->getBrakeabilityInRange(v1, v2);
    }

    /******************************************************************************
    * getBrakeability()
    ******************************************************************************/
    int32_t AbstractTSetup::getBrakeability(const uint32_t v) const
    {
      return getBrakeabilityObject().getBrakeabilityBrakeSystemInUse()->getBrakeability(v);
    }

    /******************************************************************************
    * getServiceBrakeResponseTime()
    ******************************************************************************/
    uint32_t AbstractTSetup::getServiceBrakeResponseTime() const
    {
      return getBrakeabilityObject().getBrakeabilityBrakeSystemInUse()->getServiceBrakeResponseTime();
    }

    /******************************************************************************
    * getEmergencyBrakeResponseTime()
    ******************************************************************************/
    uint32_t AbstractTSetup::getEmergencyBrakeResponseTime() const
    {
      return getBrakeabilityObject().getBrakeabilityBrakeSystemInUse()->getEmergencyBrakeResponseTime();
    }

    /******************************************************************************
    * isBrakeAbilityChanged()
    ******************************************************************************/
    bool AbstractTSetup::isBrakeAbilityChanged() const
    {
      TrainSetupChangeDetails chDetails = trainSetupStorage.data->changeDetails;
      return ((getBrakeabilityObject().getChangeCounter() != oldChangeCounter) || (chDetails.brakeAbility));
    }

    /******************************************************************************
    * isBrakeResponseTimeChanged()
    ******************************************************************************/
    bool AbstractTSetup::isBrakeResponseTimeChanged() const
    {
      TrainSetupChangeDetails chDetails = trainSetupStorage.data->changeDetails;
      return ((getServiceBrakeResponseTime() != oldSbResponseTime) || (chDetails.brakeResponseTime));
    }

    /******************************************************************************
    * getBrakeSystemInUse()
    ******************************************************************************/
    BrakeSystemType AbstractTSetup::getBrakeSystemInUse() const
    {
      return getBrakeabilityObject().getBrakeabilityBrakeSystemInUse()->getBrakeSystemType();
    }

  }
}
