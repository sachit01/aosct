/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file implements the AbstractModeControl class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-01   arastogi      Created
* 2016-10-12   arastogi      Fixed update of previous mode in powerdown and unreg.
* 2016-10-17   arastogi      Added ATP reset variable access functions.
* 2017-01-12   saprasad      Added Registered Parameter(ATPMode) for Analyzer.
* 2017-04-01   skothiya      Updated for implementation of Cabin Handling and Authorization requirement
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include <vfw_string.h>
#include <vfw_checkpoints.h>

#include "abstract_brake.hpp"
#include "abstract_config.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_odometry.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_targets.hpp"
#include "abstract_dmi_handler.hpp"
#include "atc_math.hpp"
#include "normal_mode.hpp"
#include "safe_brake_to_stop_mode.hpp"
#include "yard_mode.hpp"
#include "power_down_mode.hpp"
#include "unregistered_mode.hpp"
#include "safety_halt_mode.hpp"
#include "shunting_mode.hpp"
#include "possession_mode.hpp"
#include "location_mode.hpp"
#include "staff_responsible_mode.hpp"
#include "sleeping_mode.hpp"
#include "abstract_analyzer_if.hpp"
#include "abstract_tsetup.hpp"
#include "split_mode.hpp"
#include "join_mode.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_radio_handler.hpp"
#include "sleeping_mode.hpp"
#include "abstract_mode_control_event_ids.hpp"

#ifndef __GNUG__
#include "time.h"
extern "C" void vfwGetTimeOfDay(struct timespec * const timespec_p, struct tm * const tm_p);
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#endif

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
namespace
{
  template <class Type>
  Type checkPointer(Type const pointer)
  {
    if (pointer == NULL)
    {
      ATC::aosHalt(__FILE__, __LINE__, "NULL pointer dereferenced");
    }
    return pointer;
  }
}

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * Allows AbstractAnalyzerIF to read the enum ATPMode from AbstractModeControl.
    */
    class ATPModeGetter : public ATC::AbstractAnalyzerIF::EnumValueGetter
    {
    public:
      /**
      * Constructs and initializes the object.
      */
      ATPModeGetter(const AbstractModeControl* const modeControl_)
        : modeControl(modeControl_)
      {
      }

      /**
      * Returns the value to be measured.
      * @return the value to be measured.
      */
      virtual uint32_t getValue() const
      {
        return static_cast<uint32_t>(modeControl->getCurrentMode());
      }

    private:
      /**
      * Dummy - must not be called.
      */
      ATPModeGetter();

      /**
      * Provides the "link" between AbstractAnalyzerIF and AbstractModeControl.
      */
      const AbstractModeControl* modeControl;
    };

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractModeControl::AbstractModeControl() :
      ATC::ProcComponent(atpModeControlId, "ModeControl", "MC"),
      bothCabinActiveSB(ATC::Event::createSBReqEvent(atpModeControlId, ATC::CoreContainer, eventIdSBinBothCabinActive,
        ATC::DriverSB, DMICom::sbInBothCabinActive, "Applying service break as both cabins are active")),
      noCabinActiveStandstill(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer, eventIdSBinNoCabinActive,
        ATC::DriverSB, 0x0U, "Applying standstill as no cabin is active and train is not in sleeping mode")),
      brakeTestIsMandatorySB(ATC::Event::createSBReqEvent(atpModeControlId, ATC::CoreContainer,
        eventIdTimeForMandatoryBrakeTestSB, ATC::NoSB, 0x0U, "Brake test is mandatory! Breaking")),
      brakeTestIsMandatorySS(ATC::Event::createStandstillEvent(atpModeControlId, ATC::CoreContainer,
        eventIdTimeForMandatoryBrakeTestSS, ATC::NoSB, 0x0U, "Brake test is mandatory! Standstill")),
      safetyHaltInSBTSInvalidPreviousMode(ATC::Event::createSafetyHaltEvent(atpModeControlId, ATC::CoreContainer,
        eventIdSafetyHaltInSBSInvalidPrevMod, ATC::NoEB, DMICom::modSBToStopInvalidPrevMod,
        "SafetyHalt from Safe Brake To Stop, previous mode is not valid")),
      atpNeedsResetEvent(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer,
        eventIdATPNeedsReset, DMICom::atpNeedsReset, "ATP Needs Reset!")),
      sleepingSigInactiveInSleeping(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer,
        eventIdSleepSigInactiveInSleeping, DMICom::sleepingSignalInActive, "Sleeping Signal Inactive in Sleeping Mode")),
      sleepingSigActiveIllegalMode(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer,
        eventIdSleepingSigActiveIllegalMode, DMICom::sleepingSignalActive, "Sleeping Signal Active in illegal mode")),
      exceedMaxAllowedRunTime(ATC::Event::createSBReqEvent(atpModeControlId, ATC::CoreContainer,
        eventIdMaxAllowedRunTime, ATC::DriverSB, DMICom::maxAllowedRunTime, "SB due to Allowed run time")),
      safetyHaltExceedMaxRunTime(ATC::Event::createSafetyHaltEvent(atpModeControlId, ATC::CoreContainer,
        eventIdSafetyHaltMaxAllowedRunTime, ATC::NoEB, DMICom::maxAllowedRunTime, "Safety Halt due to max run time")),
      notReadyToDriveWhileMoving(ATC::Event::createSBReqEvent(atpModeControlId, ATC::CoreContainer, eventIdNotReadyToDrive,
        ATC::DriverSB, DMICom::notReadyToDrive, "Applying service break as NotReadyToDrive is active")),
      modeChanged(ATC::Event::createLogEvent(atpModeControlId, ATC::CoreContainer, eventIdModeChanged, 0x0U,
        "ATP Mode changed to ", true)),
      coreInitDone(false),
      currentMode(ATPModeUndefined),
      previousMode(ATPModeUndefined),
      driverLoginSeq(static_cast<DriverLoginSeq*>(NULL)),
      emergencyAlertAlarmSeq(static_cast<EmergencyAlertSeq*>(NULL)),
      shuntSeq(static_cast<ShuntModeRequestSeq*>(NULL)),
      posSeq(static_cast<PosModeRequestSeq*>(NULL)),
      yardSeq(static_cast<YardModeRequestSeq*>(NULL)),
      configSeq(static_cast<ConfigModeRequestSeq*>(NULL)),
      cabActive(NoCabActive),
      prevActiveCab(NoCabActive),
      brakeTestRequested(false),
      brakeTestMandatory(false),
      brakeTestPossible(false),
      isTimeForBrakeTestNotification(false),
      remainingTimeToMandatoryBrakeTest(0U),
      currentMaxAllowedSpeedInLocation(0U)
    {
      if (coreModeControlInstancePtr != 0)
      {
        ATC::aosHalt(__FILE__, __LINE__, "Mode Control constructor already instantiated");
      }

      // Setup single instance pointer for core access
      coreModeControlInstancePtr = this;

      memset(&modeList[0], 0, sizeof(modeList));

      // Clear the common data
      commonData.clear();

      atpResetRequired = false;
      logSleepingSignalStatus = true;
      maxAllowableTimeAOSRunWithoutReset = 0U;
      logVersionsToTCCFlag = false;

    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractModeControl::init(void)
    {
      //If init has not been done successfully.
      if (!coreInitDone)
      {
        coreInitDone = true;

        //Iterate over all modes excluding the ATPUndefined mode.
        for (uint8_t i = static_cast<uint8_t>(ATPModePowerUp); i < static_cast<uint8_t>(ATPModesCount); ++i)
        {
          //Check if the mode object has not been created
          if (modeList[i] == static_cast<AbstractMode*>(NULL))
          {
            AbstractMode* modePtr = static_cast<AbstractMode*>(NULL);

            //Check if the mode object is created for this mode.
            //Will return false for unsupported/ not implemented modes.
            if (createModeObject(static_cast<ATPMode>(i), modePtr))
            {
              //If the mode object is NULL init has failed
              if (modePtr == static_cast<AbstractMode*>(NULL))
              {
                coreInitDone = false;
              }
              else
              {
                if (modePtr->init())
                {
                  //put the mode object pointer in the modelist.
                  modeList[i] = modePtr;

                }
                else
                {
                  coreInitDone = false;
                  break;
                }

              }
            }
          }
        }

        //If the driverLoginSeq object has not been created
        if (driverLoginSeq == static_cast<DriverLoginSeq*>(NULL))
        {
          driverLoginSeq = createDriverLoginSeqObj();
          //If the object was not created successfully
          if (driverLoginSeq == static_cast<DriverLoginSeq*>(NULL))
          {
            coreInitDone = false;
          }
        }

        //If the emergencyAlertAlarmSeq object has not been created
        if (emergencyAlertAlarmSeq == static_cast<EmergencyAlertSeq*>(NULL))
        {
          emergencyAlertAlarmSeq = createEmergencyAlertSeqObj();
          //If the object was not created successfully
          if (emergencyAlertAlarmSeq == static_cast<EmergencyAlertSeq*>(NULL))
          {
            coreInitDone = false;
          }
        }

        if (shuntSeq == static_cast<ShuntModeRequestSeq*>(NULL))
        {
          shuntSeq = createShuntButtonSeqObj();
          //If the object was not created successfully
          if (shuntSeq == static_cast<ShuntModeRequestSeq*>(NULL))
          {
            coreInitDone = false;
          }
        }


        if (yardSeq == static_cast<YardModeRequestSeq*>(NULL))
        {
          yardSeq = createYardButtonSeqObj();
          //If the object was not created successfully
          if (yardSeq == static_cast<YardModeRequestSeq*>(NULL))
          {
            coreInitDone = false;
          }
        }

        if (posSeq == static_cast<PosModeRequestSeq*>(NULL))
        {
          posSeq = createPosButtonSeqObj();
          //If the object was not created successfully
          if (posSeq == static_cast<PosModeRequestSeq*>(NULL))
          {
            coreInitDone = false;
          }
        }

        if (configSeq == static_cast<ConfigModeRequestSeq*>(NULL))
        {
          configSeq = createConfigButtonSeqObj();
          //If the object was not created successfully
          if (configSeq == static_cast<ConfigModeRequestSeq*>(NULL))
          {
            coreInitDone = false;
          }
        }

        //If all the objects have been created
        if (coreInitDone)
        {
          //Change the mode to PowerUp.
          currentMode = ATPModePowerUp;
          previousMode = ATPModeUndefined;

          //reset the new mode
          modeList[currentMode]->resetMode();

          cabActive = NoCabActive;
          prevActiveCab = NoCabActive;
        }

        //Register ATPMode for register to Analyzer component
        static ATPModeGetter atpModeGetter(this);
        bool resRegister = ATC::AbstractAnalyzerIF::corePtr()->registerMeasurement(
          "ATPMode", "ATPMode", "mode", 0U, 20U, &atpModeGetter);

        if (!resRegister)
        {
          writeToLog(ATC::BriefLog,"Registermeasurement failed for analyzerIF in mode control", __FILE__, __LINE__);
        }

        // Configurable time in secs, Max Allowable Time AOS run without reset
        maxAllowableTimeAOSRunWithoutReset = static_cast<uint32_t>(AbstractConfig::corePtr()->getMaxTimeAOSRun()) * 60U * 60U;

        initCrossCompare();
      }

      return coreInitDone;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void AbstractModeControl::run(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      commonData.buzzer = BuzzerTypeNone; //Reset buzzer in each cycle
      vfwVisitCheckPoint(&cp, "MC_run");

      // Manage logging of AOS Version Info to TCC
      manageVersionLoggingToTCC();

      // Manage the active cab indication
      manageCabActiveStatus();

      // Manage Sleeping Signal
      manageSleepingSignal();

      // Manage the need for Brake Test
      manageBrakeTest();

      // Manage the NotReadyToDrive signal
      manageNotReadyToDrive();

      // Run All Sequences
      runAllSeqs();

      // Handle events that is applicable for all modes (might also change the current mode).
      preProcessEventsForAllModes();

      // Execute the current mode
      modeList[currentMode]->handleMode(commonData);

      // Handle events that is applicable for all modes
      postProcessEventsForAllModes();

      // If next mode is updated.. update current and previous mode
      ATPMode nextMode = modeList[currentMode]->getNextModeId();
      if (nextMode != ATPModeUndefined)
      {
        previousMode = currentMode;
        currentMode = nextMode;

        traceMode();

        // Reset the new mode
        modeList[currentMode]->resetMode();
      }

      handleATPResetCondition();
    }

    /******************************************************************************
    * handleATPResetCondition
    ******************************************************************************/
    void AbstractModeControl::handleATPResetCondition()
    {
      // Time since reset in seconds
      int64_t timeSinceAOSRunningWithoutReset = vfwGetReferenceTime()/1000;

      // Issue a warning x hours before ATP has to be reset.
      const int64_t resetWarningTime = static_cast<int64_t>(maxAllowableTimeAOSRunWithoutReset) - 
        static_cast<int64_t>(getTimeDueToIntimidateTCCForATPReset());

      if ((resetWarningTime < 0) || (timeSinceAOSRunningWithoutReset > resetWarningTime))
      {
        if (!atpResetRequired)
        {
          atpResetRequired = true;
          ATC::AbstractEventHandler::corePtr()->reportEvent(atpNeedsResetEvent, __FILE__, __LINE__);
        }
        if (timeSinceAOSRunningWithoutReset >= static_cast<int64_t>(maxAllowableTimeAOSRunWithoutReset))
        {
          // Raise the service brake
          ATC::AbstractEventHandler::corePtr()->reportEvent(exceedMaxAllowedRunTime, __FILE__, __LINE__);
          
          // If standstill Raise the safety halt event
          if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(safetyHaltExceedMaxRunTime, __FILE__, __LINE__);
          }
        }

      }
      else
      {
        atpResetRequired = false;
      }
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractModeControl* AbstractModeControl::corePtr(void)
    {
      return coreModeControlInstancePtr;
    }

    /******************************************************************************
    * getCurrentMode
    ******************************************************************************/
    ATPMode AbstractModeControl::getCurrentMode() const
    {
      return currentMode;
    }

    /******************************************************************************
    * getPreviousMode
    ******************************************************************************/
    ATPMode AbstractModeControl::getPreviousMode() const
    {
      return previousMode;
    }

    /******************************************************************************
    * createPowerUpModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createPowerUpModeObj()
    {
      return new PowerUpMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createTrainConfigModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createTrainConfigModeObj()
    {
      return new TrainConfigMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createTrainRegistrationModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createTrainRegistrationModeObj()
    {
      return new TrainRegistrationMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createTrainRegistrationModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createBaliseSearchModeObj()
    {
      return new BaliseSearchMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createNormalModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createNormalModeObj()
    {
      return new NormalMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createYardModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createYardModeObj()
    {
      return new YardMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createSafeBrakeModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createSafeBrakeToStopModeObj()
    {
      return new SafeBrakeToStopMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createPowerDownModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createPowerDownModeObj()
    {
      return new PowerDownMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createUnregisteredModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createUnregisteredModeObj()
    {
      return new UnregisteredMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createSafetyHaltModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createSafetyHaltModeObj()
    {
      return new SafetyHaltMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createDriverLoginSeqObj
    ******************************************************************************/
    DriverLoginSeq* AbstractModeControl::createDriverLoginSeqObj()
    {
      return new DriverLoginSeq(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createEmergencyAlertSeqObj
    ******************************************************************************/
    EmergencyAlertSeq* AbstractModeControl::createEmergencyAlertSeqObj()
    {
      return new EmergencyAlertSeq(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createPosButtonSeqObj
    ******************************************************************************/
    ModeRequestSeq* AbstractModeControl::createPosButtonSeqObj()
    {
      return new PosModeRequestSeq(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createYardButtonSeqObj
    ******************************************************************************/
    ModeRequestSeq* AbstractModeControl::createYardButtonSeqObj()
    {
      return new YardModeRequestSeq(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createShuntButtonSeqObj
    ******************************************************************************/
    ModeRequestSeq* AbstractModeControl::createShuntButtonSeqObj()
    {
      return new ShuntModeRequestSeq(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    * createConfigButtonSeqObj
    ******************************************************************************/
    ModeRequestSeq* AbstractModeControl::createConfigButtonSeqObj()
    {
      return new ConfigModeRequestSeq(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    *  CreatePossessionModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createPossessionModeObj()
    {
      return new PossessionMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    *  createShuntingModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createShuntingModeObj()
    {
      return new ShuntingMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    *  createLocationModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createLocationModeObj()
    {
      return new LocationMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    *  createShuntingRouteModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createShuntingRouteModeObj()
    {
      return new ShuntingRouteMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    *  createStaffResponsibleModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createStaffResponsibleModeObj()
    {
      return new StaffResponsibleMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    *  createSleepingModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createSleepingModeObj()
    {
      return new SleepingMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    *  createSplitModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createSplitModeObj()
    {
      return new SplitMode(); //lint !e586 'new' is acceptable during initialization
    }

    /******************************************************************************
    *  createJoinModeObj
    ******************************************************************************/
    AbstractMode* AbstractModeControl::createJoinModeObj()
    {
      return new JoinMode(); //lint !e586 'new' is acceptable during initialization
    }


    /******************************************************************************
    * getModeState
    ******************************************************************************/
    ModeState AbstractModeControl::getModeState(const ATPMode atpMode) const
    {
      return modeList[atpMode]->getModeState();
    }

    /******************************************************************************
    * getcurrentATOMode
    ******************************************************************************/
    ATOMode AbstractModeControl::getATOMode() const
    {
      return ATOModeManual;
    }

    /******************************************************************************
    * getPreviousATOMode
    ******************************************************************************/
    ATOMode AbstractModeControl::getPreviousATOMode() const
    {
      // Will return Manual as ATO mode is not supported in this phase
      return ATOModeManual;
    }

    /******************************************************************************
    * getPowerUpModeState
    ******************************************************************************/
    PowerUpModeState AbstractModeControl::getPowerUpModeState() const
    {
      return modeList[ATPModePowerUp]->getModeState();
    }

    /******************************************************************************
    * getTrainConfigModeState
    ******************************************************************************/
    TrainConfigModeState AbstractModeControl::getTrainConfigModeState() const
    {
      return modeList[ATPModeConfiguration]->getModeState();
    }

    /******************************************************************************
    * getTrainRegistrationModeState
    ******************************************************************************/
    TrainRegistrationModeState AbstractModeControl::getTrainRegistrationModeState() const
    {
      return modeList[ATPModeRegistration]->getModeState();
    }

    /******************************************************************************
    * getTrainRegistrationModeState
    ******************************************************************************/
    BaliseSearchModeState AbstractModeControl::getBaliseSearchModeState() const
    {
      return modeList[ATPModeBaliseSearch]->getModeState();
    }

    /******************************************************************************
    * getStaffResponsibleModeState
    ******************************************************************************/
    StaffResponsibleModeState AbstractModeControl::getStaffResponsibleModeState() const
    {
      return modeList[ATPModeStaffResponsible]->getModeState();
    }

    /******************************************************************************
    * getJoinModeState
    ******************************************************************************/
    JoinModeState AbstractModeControl::getJoinModeState() const
    {
      return modeList[ATPModeJoin]->getModeState();
    }

    /******************************************************************************
    * getSplitModeState
    ******************************************************************************/
    SplitModeState AbstractModeControl::getSplitModeState() const
    {
      return modeList[ATPModeSplit]->getModeState();
    }

    /******************************************************************************
    * getShuntingModeState
    ******************************************************************************/
    ShuntingModeState AbstractModeControl::getShuntingModeState() const
    {
      return modeList[ATPModeShunting]->getModeState();
    }

    /******************************************************************************
    * getPosModeState
    ******************************************************************************/
    PossessionModeState AbstractModeControl::getPosModeState() const
    {
      return modeList[ATPModePossession]->getModeState();
    }

    /******************************************************************************
    * getYardModeState
    ******************************************************************************/
    YardModeState AbstractModeControl::getYardModeState() const
    {
      return modeList[ATPModeYard]->getModeState();
    }

    /******************************************************************************
    * getSleepingModeState
    ******************************************************************************/
    SleepingModeState AbstractModeControl::getSleepingModeState() const
    {
      return modeList[ATPModeSleeping]->getModeState();
    }


    /******************************************************************************
    * getLocationModeState
    ******************************************************************************/
    LocationModeState AbstractModeControl::getLocationModeState() const
    {
      return modeList[ATPModeLocation]->getModeState();
    }

    /******************************************************************************
    * getDriverLoginSeqState
    ******************************************************************************/
    DriverLoginState AbstractModeControl::getDriverLoginSeqState() const
    {
      return checkPointer(driverLoginSeq)->getState();
    }

    /******************************************************************************
    * getEmergencyAlertSeqState
    ******************************************************************************/
    EmergencyAlertState AbstractModeControl::getEmergencyAlertSeqState() const
    {
      return checkPointer(emergencyAlertAlarmSeq)->getState();
    }

    /******************************************************************************
    * getConfigModReqSeqState
    ******************************************************************************/
    ModeRequestSeqState AbstractModeControl::getConfigModReqSeqState() const
    {
      ModeRequestSeqState configModeRequestSequenceState = ConfigModeRequestSeq::configDmiButtonPressed;

      if (configSeq != static_cast<ConfigModeRequestSeq *>(NULL))
      {
        configModeRequestSequenceState = configSeq->getModeReqSeqState();
      }
      return configModeRequestSequenceState;
    }

    /******************************************************************************
    * getShuntModReqSeqState
    ******************************************************************************/
    ModeRequestSeqState AbstractModeControl::getShuntModReqSeqState() const
    {
      ModeRequestSeqState shuntModReqSeqState = ShuntModeRequestSeq::shuntIsDmiButtonPressed;

      if (shuntSeq != static_cast<ShuntModeRequestSeq *>(NULL))
      {
        shuntModReqSeqState = shuntSeq->getModeReqSeqState();
      }
      return shuntModReqSeqState;
    }

    /******************************************************************************
    * getYardModReqSeqState
    ******************************************************************************/
    ModeRequestSeqState AbstractModeControl::getYardModReqSeqState() const
    {
      ModeRequestSeqState yardModReqSeqState = YardModeRequestSeq::yardWaitDmiButtonPress;

      if (yardSeq != static_cast<YardModeRequestSeq *>(NULL))
      {
        yardModReqSeqState = yardSeq->getModeReqSeqState();
      }

      return yardModReqSeqState;
    }

    /******************************************************************************
    * getPosModReqSeqState
    ******************************************************************************/
    ModeRequestSeqState AbstractModeControl::getPosModReqSeqState() const
    {
      ModeRequestSeqState posModReqSeqState = PosModeRequestSeq::posIsDmiButtonPressed;

      if (posSeq != static_cast<PosModeRequestSeq *>(NULL))
      {
        posModReqSeqState = posSeq->getModeReqSeqState();
      }

      return posModReqSeqState;
    }

    /******************************************************************************
    * getBrakeTestRequested
    ******************************************************************************/
    bool AbstractModeControl::getBrakeTestRequested() const
    {
      return brakeTestRequested;
    }

    /******************************************************************************
    * isNewTrainConfiguration
    ******************************************************************************/
    bool AbstractModeControl::isNewTrainConfiguration() const
    {
      return commonData.isNewConfig;
    }

    /******************************************************************************
    * getIdleState
    ******************************************************************************/
    bool AbstractModeControl::getIdleState() const
    {
      return commonData.idling;
    }

    /******************************************************************************
    * getHandlingDone
    ******************************************************************************/
    bool AbstractModeControl::getHandlingDone() const
    {
      return commonData.handlingDone;
    }

    /******************************************************************************
    * getFreeRolling
    ******************************************************************************/
    bool AbstractModeControl::getFreeRolling() const
    {
      return commonData.freeRolling;
    }

    /******************************************************************************
    * getOdometerInvalid
    ******************************************************************************/
    bool AbstractModeControl::getOdometerInvalid() const
    {
      return commonData.odometerInvalid;
    }

    /******************************************************************************
    * getMATimeOut
    ******************************************************************************/
    bool AbstractModeControl::getMATimeOut(void) const
    {
      return commonData.maTimeOut;
    }

    /******************************************************************************
    * setCeilingSpeedInLoc
    ******************************************************************************/
    void AbstractModeControl::setMaxAllowedSpeedInLoc(const uint16_t speed)
    {
      //Set the ceiling speed
      AbstractMode *basePtr = modeList[ATPModeLocation];
      //Set the ceiling speed in location
      if (NULL != basePtr)
      {
        currentMaxAllowedSpeedInLocation = speed;
        basePtr->setMaxAllowedSpeedInLocation(speed);
      }
      else
      {
        ATC::aosHalt("Location mode is not available", __LINE__, __FILE__);
      }
    }

    /******************************************************************************
    * getCeilingSpeedInLoc
    ******************************************************************************/
    const uint32_t AbstractModeControl::geMaxAllowedSpeedInLoc() const
    {
      trace.write(ATC::briefTrace, "Current maximum allowed Speed in Location Mode:", currentMaxAllowedSpeedInLocation);
      return currentMaxAllowedSpeedInLocation;
    }

    /******************************************************************************
    * getActiveCab
    ******************************************************************************/
    CabActiveStatus AbstractModeControl::getActiveCab(void) const
    {
      return cabActive;
    }

    /******************************************************************************
    * getPrevActiveCab
    ******************************************************************************/
    CabActiveStatus AbstractModeControl::getPrevActiveCab(void) const
    {
      return prevActiveCab;
    }

    /******************************************************************************
    * manageCabActiveStatus
    ******************************************************************************/
    void AbstractModeControl::manageCabActiveStatus(void)
    {
      bool isCabAActive = false;
      bool isCabBActive = false;

      // At 'start up' AOS shall use the default values for Cabin selection: No cabin active
      if (getStartUpPassed())
      {
        //Get cab active inputs from LocoIO
        if (!(IO::AbstractLocoIO::corePtr()->getCoreDigitalInputValue(IO::AbstractLocoIO::Cab1, &isCabAActive)))
        {
          //error
          isCabAActive = false;
        }

        //If AOS is configured for only Cabin A, ignore Cabin B and treat as inactive
        if (AbstractConfig::corePtr()->getCabinConfiguration() == cabinAOnly)
        {
          trace.write(ATC::detailedLevel, "AOS is Configured for only Cabin A!");
          writeToLog(ATC::DetailedLog, "AOS is Configured for only Cabin A!", __FILE__, __LINE__);
          isCabBActive = false;
        }
        else
        {
          if (!(IO::AbstractLocoIO::corePtr()->getCoreDigitalInputValue(IO::AbstractLocoIO::Cab2, &isCabBActive)))
          {
            //error
            isCabBActive = false;
          }
        }
      }

      //if both cabin A & cabin B are active 
      if (isCabAActive && isCabBActive)
      {
        prevActiveCab = cabActive;
        cabActive = NoCabActive;
        //Raise safe break as both the cabins are active
        ATC::AbstractEventHandler::corePtr()->reportEvent(bothCabinActiveSB, __FILE__, __LINE__);
      }
      //if cab A is active and cab b is inactive
      else if (isCabAActive && (!isCabBActive))
      {
        prevActiveCab = cabActive;
        cabActive = CabAActive;
      }
      //if cab b is active and cab a is inactive
      else if (isCabBActive && (!isCabAActive))
      {
        prevActiveCab = cabActive;
        cabActive = CabBActive;
      }
      //if No cab is active
      else
      {
        prevActiveCab = cabActive;
        cabActive = NoCabActive;

        if (currentMode != ATPModeSleeping)
        {
          ///Raise standstill event if no cab active and current mode is not sleeping mode
          ATC::AbstractEventHandler::corePtr()->reportEvent(noCabinActiveStandstill, __FILE__, __LINE__);
          // AOS Ok should be set to State 'AOS is not operational' i.e.- atp ok Status is false
          commonData.atpOkStatus = false;
        }
      }

      if (((CabAActive == cabActive) || (CabBActive == cabActive)) &&
        ((currentMode != ATPModePoweringDown) && (currentMode != ATPModeSafetyHalt)))
      {
        // AOS Ok should be set to State 'AOS is operational' when a valid cabin selection is made in appropriate modes.
        // For the case both cabin are active. 
        commonData.atpOkStatus = true;
      }
      else
      {
        commonData.atpOkStatus = false;
      }

    }

    /******************************************************************************
    * manageSleepingSignal
    ******************************************************************************/
    void AbstractModeControl::manageSleepingSignal(void)
    {
      const bool sleepingSignal = IO::AbstractLocoIO::corePtr()->getSleepingSignal();
      if ((ATPModeSleeping == currentMode) && (!sleepingSignal))
      {
        if (logSleepingSignalStatus)
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(sleepingSigInactiveInSleeping, __FILE__, __LINE__);
          logSleepingSignalStatus = false; //Log once
        }
      }
      else if ((ATPModeSleeping == currentMode) && (sleepingSignal))
      {
        // Sleep Signal active in ATP Mode sleeping as expected.
        // Reset flag to log the next erroneous Sleeping Signal Status.
        logSleepingSignalStatus = true;
      }
      else if ((!((ATPModeJoin == currentMode) || (ATPModeYard == currentMode) ||
        (ATPModeShunting == currentMode) || (ATPModePowerUp == currentMode)))
        && sleepingSignal)
      {
        // Sleeping Signal active in illegal mode!
        if (logSleepingSignalStatus)
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(sleepingSigActiveIllegalMode, __FILE__, __LINE__);
          logSleepingSignalStatus = false; //Log once
        }
      }
      else
      {
        // Reset flag to log the next erroneous Sleeping Signal Status
        logSleepingSignalStatus = true;
      }

    }

    /******************************************************************************
    * evaluateBrakeTestPossible
    ******************************************************************************/
    void AbstractModeControl::evaluateBrakeTestPossible(void)
    {
      bool isDriverLoggedIn = false;
      bool lcsReady = false;
      bool modeOK = false;

      // Is Current Mode OK to perform brake Test?
      if ((ATPModeSafetyHalt != currentMode) && (ATPModePoweringDown != currentMode)
        && (ATPModeSleeping != currentMode) && (ATPModeSafeBrakeToStop != currentMode))
      {
        modeOK = true;
      }

      //Is ATO Mode = Manual?
      const bool atoModeManual = (getATOMode() == ATOModeManual);

      // Has Driver logged-in?
      DriverLoginState driverLoginState = getDriverLoginSeqState();
      if (driverLoginState == DriverLoginSeq::driverLoggedIn)
      {
        isDriverLoggedIn = true;
      }

      // Vehicle Ready?
      static_cast<void>(IO::AbstractLocoIO::corePtr()->getCoreDigitalInputValue(IO::AbstractLocoIO::LCSRdy, &lcsReady));

      // Is Vehicle at StandStill?
      const bool atStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

      brakeTestPossible = (modeOK && atoModeManual && isDriverLoggedIn && lcsReady && atStandStill);
    }

    /******************************************************************************
    * getBrakeTestMandatory
    ******************************************************************************/
    bool AbstractModeControl::getBrakeTestMandatory() const
    {
      return brakeTestMandatory;
    }

    /******************************************************************************
    * getBrakeTestPossible
    ******************************************************************************/
    bool AbstractModeControl::getBrakeTestPossible() const
    {
      return brakeTestPossible;
    }

    /******************************************************************************
    * getRemainingTimeToMandatoryBrakeTest
    ******************************************************************************/
    uint16_t AbstractModeControl::getRemainingTimeToMandatoryBrakeTest() const
    {
      return remainingTimeToMandatoryBrakeTest;
    }

    /******************************************************************************
    * getBrakeTestNotification
    ******************************************************************************/
    bool AbstractModeControl::getBrakeTestNotification() const
    {
      return isTimeForBrakeTestNotification;
    }

    /******************************************************************************
    * manageBrakeTest
    ******************************************************************************/
    void AbstractModeControl::manageBrakeTest(void)
    {
      brakeTestPossible = false;
      brakeTestMandatory = false;
      brakeTestRequested = false;
      isTimeForBrakeTestNotification = false;

      Supv::BrakeTestStatus brakeTestStatus = Supv::AbstractBrake::corePtr()->getBrakeTestStatus();
      //Has driver pressed the Brake Test Button?
      brakeTestRequested = DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIButtonStartBrakeTest;
      evaluateBrakeTestPossible();
      //If the previous Brake Test had failed, issue standstill until a successful Brake Test is performed
      if ((Supv::BrakeTestStatusFail == brakeTestStatus) || (Supv::BrakeTestStatusAborted == brakeTestStatus))
      {
        // Time for mandatory Brake Test
        ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestIsMandatorySS, __FILE__, __LINE__);
        brakeTestMandatory = true;
        remainingTimeToMandatoryBrakeTest = 0U;
      }
      else if (brakeTestStatus != Supv::BrakeTestStatusInProgress)
      {
        // Check if time for mandatory Brake test
        BrakeTestReasonType brakeTestReason = static_cast<BrakeTestReasonType>(AbstractConfig::corePtr()->getBrakeTestReason());
        struct timespec currentTimeSpec;
        vfwGetTimeOfDay(&currentTimeSpec, static_cast<tm *>(NULL));

        const uint32_t curTime = static_cast<uint32_t>(currentTimeSpec.tv_sec);
        const uint32_t lastBrakeTest = AbstractConfig::corePtr()->getLastBrakeTestTime();
        const uint32_t timeSinceLastSuccessfulTest = curTime - lastBrakeTest;
        const uint16_t mandatoryBrakeTestTime = AbstractConfig::corePtr()->getBrakeTestMandatoryTime();

        // Is current mode OK to trigg a mandatory brake test?
        const bool modeOK = ((ATPModeSafetyHalt != currentMode) && (ATPModePoweringDown != currentMode)
          && (ATPModeSleeping != currentMode) && (ATPModeSafeBrakeToStop != currentMode));

        //mandatoryBrakeTestTime obtained from Config is in 'minutes'
        if (modeOK && (timeSinceLastSuccessfulTest >= (60U * (static_cast<uint32_t>(mandatoryBrakeTestTime)))))
        {
          brakeTestReason = brakeTestReasonTimer;
          if (!AbstractConfig::corePtr()->setBrakeTestReason(brakeTestReason))
          {
            trace.write(ATC::briefTrace, "Failed to store the Brake Test Reason in NVS!");
            writeToLog(ATC::BriefLog, "Failed to store the Brake Test Reason in NVS!", __FILE__, __LINE__);
          }
        }

        if (braketestReasonNone != brakeTestReason)
        {
          if (modeOK)
          {
            if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestIsMandatorySS, __FILE__, __LINE__);
            }
            else
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(brakeTestIsMandatorySB, __FILE__, __LINE__);
            }

            brakeTestMandatory = true;
            remainingTimeToMandatoryBrakeTest = 0U;
          }
        }
        else
        {
          uint16_t timeSinceLastSuccessfulTestMin = static_cast<uint16_t>(ATC::ATCMath::instance().unsignDiv(
            timeSinceLastSuccessfulTest, 60U, __FILE__, __LINE__));
          remainingTimeToMandatoryBrakeTest = mandatoryBrakeTestTime - timeSinceLastSuccessfulTestMin;

          //Time to send a Brake test notification?
          if ((remainingTimeToMandatoryBrakeTest <= AbstractConfig::corePtr()->getBrakeTestNotifyTime()) && modeOK)
          {
            isTimeForBrakeTestNotification = true;
          }
        }
      }
      else
      {
        // Brake Test is in Progress.
        remainingTimeToMandatoryBrakeTest = 0U;
      }
    }

    /******************************************************************************
    * manageNotReadyToDrive
    ******************************************************************************/
    void AbstractModeControl::manageNotReadyToDrive() const
    {
      const bool isValidMode = (ATPModeSleeping != currentMode);
      const bool isNotReadyToDrive = getNotReadyToDrive();

      if (isValidMode && isNotReadyToDrive)
      {
        if (Pos::AbstractOdometry::corePtr()->isTrainStandStill())
        {
          //To avoid clearing the value of current ceiling speed, gradient, and direction in modes where 
          //targets are not available (e.g - Yard)
          if (!ATP::DS::AbstractTargets::corePtr()->isMATargetListEmpty())
          {
            // Clear any existing target data
            DS::AbstractTargets::corePtr()->removeAll();
          }
        }
        else
        {
          /// Raise brake-event if NotReadyToDrive is activated during driving
          ATC::AbstractEventHandler::corePtr()->reportEvent(notReadyToDriveWhileMoving, __FILE__, __LINE__);
        }
      }
    }

    /******************************************************************************
    * manageVersionLoggingToTCC
    ******************************************************************************/
    void AbstractModeControl::manageVersionLoggingToTCC()
    {
      // If any TCC is connected (with protocol version checked) AND Position Report is open for updates (acknowledged) ->
      // Log events to TCC with SW name and version, Configuration name and version, and HW name and version
      // (Log only once at startup, log only if startup is passed in order to make sure all versions are collected)

      if (getStartUpPassed()  &&  (!logVersionsToTCCFlag))
      {
        // Region 1
        const bool region1ValidProtocol = AbstractMessageHandler::corePtr()->isProtocolVersionMatching(RadioCom::radioChannelId2);
        const bool region1AckPositionReport = AbstractMessageHandler::corePtr()->getAckDefaultPositionReport(RadioCom::radioChannelId2);

        // Region 2
        const bool region2ValidProtocol = AbstractMessageHandler::corePtr()->isProtocolVersionMatching(RadioCom::radioChannelId3);
        const bool region2AckPositionReport = AbstractMessageHandler::corePtr()->getAckDefaultPositionReport(RadioCom::radioChannelId3);

        if ((region1ValidProtocol && region1AckPositionReport) || (region2ValidProtocol && region2AckPositionReport))
        {
          logVersionToTCC();
          logVersionsToTCCFlag = true;
        }
      }
    }

    /******************************************************************************
    * getModeString
    ******************************************************************************/
    void AbstractModeControl::getModeString(const ATPMode atpMode, char_t* const buffer) const
    {
      switch (atpMode)
      {
      case ATPModeUndefined:
        static_cast<void>(vfw_strlcpy(buffer, "Undefined", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModePowerUp:
        static_cast<void>(vfw_strlcpy(buffer, "Power Up", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeConfiguration:
        static_cast<void>(vfw_strlcpy(buffer, "Configuration", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeRegistration:
        static_cast<void>(vfw_strlcpy(buffer, "Registration", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeBaliseSearch:
        static_cast<void>(vfw_strlcpy(buffer, "Balise Search", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeNormal:
        static_cast<void>(vfw_strlcpy(buffer, "Normal", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeShunting:
        static_cast<void>(vfw_strlcpy(buffer, "Shunting", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeLocation:
        static_cast<void>(vfw_strlcpy(buffer, "Location", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeYard:
        static_cast<void>(vfw_strlcpy(buffer, "Yard", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeUnregistered:
        static_cast<void>(vfw_strlcpy(buffer, "Unregistered", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModePoweringDown:
        static_cast<void>(vfw_strlcpy(buffer, "Powering Down", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeSafetyHalt:
        static_cast<void>(vfw_strlcpy(buffer, "Safety Halt", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeSleeping:
        static_cast<void>(vfw_strlcpy(buffer, "Sleeping", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeStaffResponsible:
        static_cast<void>(vfw_strlcpy(buffer, "Staff Responsible", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeShuntingRoute:
        static_cast<void>(vfw_strlcpy(buffer, "Shunting Route", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModePossession:
        static_cast<void>(vfw_strlcpy(buffer, "Possession", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeSplit:
        static_cast<void>(vfw_strlcpy(buffer, "Split", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeJoin:
        static_cast<void>(vfw_strlcpy(buffer, "Join", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModeSafeBrakeToStop:
        static_cast<void>(vfw_strlcpy(buffer, "Safe Brake To Stop", AbstractMode::maxModeStateNameLength));
        break;

      case ATPModesCount:
      default:
        static_cast<void>(vfw_strlcpy(buffer, "Invalid", AbstractMode::maxModeStateNameLength));
        break;
      }
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool AbstractModeControl::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments searches for the "help", "trace" or any other Console
      component specific command calls and handles it. Returns true if completely handled
      else returns false. returning false will let other components handle the call. help always returns false.
      */

      bool retVal = false;
      char_t  buffer[512];

      // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        const char_t* const toWrite = "mode          To print out the current Mode and ModeState\n"
          "prvMode       To print out the previous Mode\n"
          "seq           To print out the state of all sequences\n"
          "MCVar         To print out variables in mode control\n"
          "cabStat       To print out current cabin state";

        ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);
        retVal = false;
      }
      else if (ATC::isTextMatch(&argv[0][0], "mode", sizeof("mode")) && (argc == 1U))
      {
        getModeString(currentMode, &buffer[0]);
        ATC::AbstractConsole::corePtr()->write("Current Mode : ");
        ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);

        if (modeList[currentMode]->getCurrentModeStateString(&buffer[0]))
        {
          ATC::AbstractConsole::corePtr()->write("Mode State : ");
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }
        retVal = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "prvMode", sizeof("prvMode")) && (argc == 1U))
      {
        getModeString(previousMode, &buffer[0]);
        ATC::AbstractConsole::corePtr()->write("Previous Mode : ");
        ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);

        retVal = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "seq", sizeof("seq")) && (argc == 1U))
      {
        int32_t ret = snprintf(&buffer[0], sizeof(buffer), "Driver Login Sequence State : %u", checkPointer(driverLoginSeq)->getState());
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Emergency Alert Sequence State : %u", checkPointer(emergencyAlertAlarmSeq)->getState());
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Yard Mode Req Sequence State : %u", checkPointer(yardSeq)->getModeReqSeqState());
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Possession Mode Req Sequence State : %u", checkPointer(posSeq)->getModeReqSeqState());
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Shunting Mode Req Sequence State : %u", checkPointer(shuntSeq)->getModeReqSeqState());
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Config Mode Req Sequence State : %u", checkPointer(configSeq)->getModeReqSeqState());
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        retVal = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "MCVar", sizeof("MCVar")) && (argc == 1U))
      {
        int32_t ret = snprintf(&buffer[0], sizeof(buffer), "Train Idling? : %s", commonData.idling ? "true" : "false");
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Ma TimeOut? : %s", commonData.maTimeOut ? "true" : "false");
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Stop Train? : %s", commonData.stopTrainActive ? "true" : "false");
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Handling Done? : %s", commonData.handlingDone ? "true" : "false");
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Odometer Invalid? : %s", commonData.odometerInvalid ? "true" : "false");
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Free Rolling? : %s", commonData.freeRolling ? "true" : "false");
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "ATP Lamp Status? : %s", commonData.atpLampStatus ? "true" : "false");
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "ATP OK Status? : %s", commonData.atpOkStatus ? "true" : "false");
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        retVal = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "cabStat", sizeof("cabStat")) && (argc == 1U))
      {
        int32_t ret = snprintf(&buffer[0], sizeof(buffer), "Current Cabin State : %u", cabActive);
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "Previous Cabin State : %u", prevActiveCab);
        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        retVal = true;
      }
      else
      {
        //Do Nothing
      }

      return retVal;
    }

    /******************************************************************************
    * createModeObject
    ******************************************************************************/
    bool AbstractModeControl::createModeObject(const ATPMode mode, AbstractMode* &modePtr)
    {
      bool ret = true;

      switch (mode)
      {
      case ATPModePowerUp:
        modePtr = createPowerUpModeObj();
        break;

      case ATPModeConfiguration:
        modePtr = createTrainConfigModeObj();
        break;

      case ATPModeRegistration:
        modePtr = createTrainRegistrationModeObj();
        break;

      case ATPModeBaliseSearch:
        modePtr = createBaliseSearchModeObj();
        break;

      case ATPModeNormal:
        modePtr = createNormalModeObj();
        break;

      case ATPModeSafeBrakeToStop:
        modePtr = createSafeBrakeToStopModeObj();
        break;

      case ATPModePoweringDown:
        modePtr = createPowerDownModeObj();
        break;

      case ATPModeUnregistered:
        modePtr = createUnregisteredModeObj();
        break;

      case ATPModeSafetyHalt:
        modePtr = createSafetyHaltModeObj();
        break;

      case ATPModeYard:
        modePtr = createYardModeObj();
        break;

      case ATPModeShunting:
        modePtr = createShuntingModeObj();
        break;

      case ATPModePossession:
        modePtr = createPossessionModeObj();
        break;
      case ATPModeStaffResponsible:
        modePtr = createStaffResponsibleModeObj();
        break;
      case ATPModeLocation:
        modePtr = createLocationModeObj();
        break;
      case ATPModeShuntingRoute:
        modePtr = createShuntingRouteModeObj();
        break;
      case ATPModeSleeping:
        modePtr = createSleepingModeObj();
        break;
      case ATPModeSplit:
        modePtr = createSplitModeObj();
        break;
      case ATPModeJoin:
        modePtr = createJoinModeObj();
        break;
      case ATPModeUndefined:
      case ATPModesCount:
      default:
        ret = false;
        ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        break;
      }

      return ret;
    }

    /******************************************************************************
    * traceMode
    ******************************************************************************/
    void AbstractModeControl::traceMode() const
    {
      char_t buffer[AbstractMode::maxModeStateNameLength] = { '\0' };
      getModeString(currentMode, &buffer[0]);

      modeChanged.setDynamicText(&buffer[0]);
      //Log event
      ATC::AbstractEventHandler::corePtr()->reportEvent(modeChanged, __FILE__, __LINE__);
      
    }

    /******************************************************************************
    * getATPReset
    ******************************************************************************/
    bool AbstractModeControl::getATPReset() const
    {
      return commonData.isATPReset;
    }

    /******************************************************************************
    * getATPNeedsReset
    ******************************************************************************/
    bool AbstractModeControl::getATPNeedsReset() const
    {
      return atpResetRequired;
    }


    /******************************************************************************
    * getStopTrainActive
    ******************************************************************************/
    bool AbstractModeControl::getStopTrainActive() const
    {
      return commonData.stopTrainActive;
    }

    /******************************************************************************
    * getATPOKStatus
    ******************************************************************************/
    bool AbstractModeControl::getATPOKStatus() const
    {
      return commonData.atpOkStatus;
    }

    /******************************************************************************
    * getLampStatus
    ******************************************************************************/
    bool AbstractModeControl::getLampStatus() const
    {
      return commonData.atpLampStatus;
    }

    /******************************************************************************
    * getShuntingRouteModeState
    ******************************************************************************/
    TrainShuntingRouteModeState AbstractModeControl::getShuntingRouteModeState() const
    {
      return modeList[ATPModeShuntingRoute]->getModeState();
    }

    /******************************************************************************
    * getOdoDir
    ******************************************************************************/
    OdoDir AbstractModeControl::getOdoDirInNewRegistration() const
    {
      return commonData.odoDirectionNewReg;
    }

    /******************************************************************************
    * getNotReadyToDrive
    ******************************************************************************/
    bool AbstractModeControl::getNotReadyToDrive() const
    {
      bool notReadyToDrive = false;

      // Emergency brake applied?
      const bool isEBApplied = Supv::AbstractBrake::corePtr()->getEbApplied();

      // Emergency brake activated by loco?
      const bool isEBActive = IO::AbstractLocoIO::corePtr()->getEmergencyStopActiveAlert();

      // Any Cabin active?
      const bool noActiveCabin = (Kernel::AbstractModeControl::corePtr()->getActiveCab() == NoCabActive);

      bool isLcsReady = false;

      // LCS Ready?
      static_cast<void> (IO::AbstractLocoIO::corePtr()->getCoreDigitalInputValue(IO::AbstractLocoIO::LCSRdy, &isLcsReady));

      // Driver Authorized?
      bool driverAuthorized = (Kernel::AbstractModeControl::corePtr()->getDriverLoginSeqState() == Kernel::DriverLoginSeq::driverLoggedIn);

      //  NotReadyToDrive?
      if (isEBApplied || isEBActive || noActiveCabin || !driverAuthorized || !isLcsReady)
      {
        notReadyToDrive = true;
      }

      return notReadyToDrive;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractModeControl::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&coreInitDone));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ATPMode>(&currentMode));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ATPMode>(&previousMode));
      // modeList[ATPModesCount] : Complex type
      checkPointer(driverLoginSeq)->initCrossCompare();
      checkPointer(emergencyAlertAlarmSeq)->initCrossCompare();
      checkPointer(shuntSeq)->initCrossCompare();
      checkPointer(posSeq)->initCrossCompare();
      checkPointer(yardSeq)->initCrossCompare();
      checkPointer(configSeq)->initCrossCompare();

      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<CabActiveStatus>(&cabActive));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<CabActiveStatus>(&prevActiveCab));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&bothCabinActiveSB));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&noCabinActiveStandstill));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&brakeTestIsMandatorySB));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&brakeTestIsMandatorySS));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&safetyHaltInSBTSInvalidPreviousMode));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&atpNeedsResetEvent));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&sleepingSigInactiveInSleeping));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&sleepingSigActiveIllegalMode));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&exceedMaxAllowedRunTime));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&safetyHaltExceedMaxRunTime));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&notReadyToDriveWhileMoving));

      // commonData
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.handlingDone));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.idling));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.maTimeOut));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.stopTrainActive));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.isATPReset));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.atpLampStatus));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.atpOkStatus));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.stopTrainActive));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.freeRolling));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.odometerInvalid));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.isNewConfig));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ATPMode>(&commonData.modeReqByDMI));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<BuzzerType>(&commonData.buzzer));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.isAbortSetupActive));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<OdoDir>(&commonData.odoDirectionNewReg));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.isAllowedToEnterLocationMode));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<TravelDir>(&commonData.currentDrivDirection));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.freeRollingButton.freeRollingConfirmToDMI));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.freeRollingButton.freeRollingDisplayToDMI));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&commonData.trainSetupAborted));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<AbortReason>(&commonData.abortSetupReason));

      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&brakeTestRequested));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&brakeTestMandatory));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&brakeTestPossible));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&isTimeForBrakeTestNotification));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&remainingTimeToMandatoryBrakeTest));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&atpResetRequired));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&logSleepingSignalStatus));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&maxAllowableTimeAOSRunWithoutReset));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&logVersionsToTCCFlag));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&currentMaxAllowedSpeedInLocation));
    }

    /******************************************************************************
    * isValidQRouteType
    ******************************************************************************/
    bool AbstractModeControl::isValidQRouteType(const RouteType routeType) const
    {
      bool retValue = false;
      if ((ATPModeUndefined < currentMode) &&
        (ATPModesCount > currentMode))
      {
        retValue = modeList[currentMode]->isValidQRouteType(routeType);
      }
      return retValue;
    }

    /******************************************************************************
    * isValidUncondShorteningMsg
    ******************************************************************************/
    //lint -esym(453,ATP::Kernel::AbstractModeControl::isValidUncondShorteningMsg) Lint is wrong, this is not designated pure
    bool AbstractModeControl::isValidUncondShorteningMsg() const
    {
      bool retValue = false;
      if ((ATPModeUndefined < currentMode) &&
        (ATPModesCount > currentMode))
      {
        retValue = modeList[currentMode]->isValidUncondShorteningMsg();
      }
      return retValue;
    }

    /******************************************************************************
    * checkRequestPowerdown
    ******************************************************************************/
    bool AbstractModeControl::checkRequestPowerdown(void) const
    {
      bool retVal = false;
      if (currentMode != ATPModePoweringDown)
      {
        PowerDownMode *pwrMode = ATC::dynamicCast<AbstractMode*, PowerDownMode*>(modeList[ATPModePoweringDown], __FILE__, __LINE__); 

        retVal = pwrMode->powerDownRequested();
      }
      return retVal;
    }

    /******************************************************************************
    * getPowerOffValue
    ******************************************************************************/
    bool AbstractModeControl::getPowerOffValue(void) const
    {
      PowerDownMode *pwrMode = ATC::dynamicCast<AbstractMode*, PowerDownMode*>(modeList[ATPModePoweringDown], __FILE__, __LINE__); 

      const bool retVal = pwrMode->powerOffSignalValue();

      return retVal;
    }

    /******************************************************************************
    * writePowerDownLogMessage
    ******************************************************************************/
    void AbstractModeControl::writePowerDownLogMessage(void) const
    {
      writeToLog(ATC::BriefLog, "PoweringDown sequence started", __FILE__, __LINE__);
    }

    /******************************************************************************
    * getPoweringDownPosMessageTimeout
    ******************************************************************************/
    uint32_t AbstractModeControl::getPoweringDownPosMessageTimeoutSec(void) const
    {
      return poweringDownPosMessageTimeoutSec;
    }
    /******************************************************************************
    * getPoweringDownBlinkDuration
    ******************************************************************************/
    uint32_t AbstractModeControl::getPoweringDownBlinkDutyMs(void) const
    {
      /* 0.25 Hz, 50% duty: 2 s toggle*/
      return poweringDownBlinkDutyMs;
    }

    /******************************************************************************
    * getPoweringDownNumPosMessages
    ******************************************************************************/
    uint32_t AbstractModeControl::getPoweringDownNumPosMessages(void) const
    {
      return poweringDownNumPosMessages;
    }


    /******************************************************************************
    * getStartUpPassed
    ******************************************************************************/
    bool AbstractModeControl::getStartUpPassed() const
    {
      return (modeList[ATPModePowerUp]->getModeState() >= PowerUpMode::powerUpActivation);
    }

    /******************************************************************************
    * isAllowedToEnterSleepMode
    ******************************************************************************/
    bool AbstractModeControl::isAllowedToEnterSleepMode() const
    {
      // Fetch all conditions needed for evaluation
      Kernel::DriverLoginState atpDriverLoginState = Kernel::AbstractModeControl::corePtr()->getDriverLoginSeqState();
      bool isDriverLoggedIn = false;
      // Driver logged in?
      if (atpDriverLoginState == Kernel::DriverLoginSeq::DriverLoginSeq::driverLoggedIn)
      {
        isDriverLoggedIn = true;
      }

      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      const bool sleeping = IO::AbstractLocoIO::corePtr()->getSleepingSignal();
      return (sleeping && isStandStill && isDriverLoggedIn);
    }

    /******************************************************************************
    * isAllowedToEnterConfigMode
    ******************************************************************************/
    bool  AbstractModeControl::isAllowedToEnterConfigMode() const
    {
      bool isAllowed = false;
      
      if (configSeq != static_cast<ConfigModeRequestSeq *>(NULL))
      {
        isAllowed = configSeq->isDMIButtonNeeded();
      }

      return isAllowed;
    }

    /******************************************************************************
    * isAllowedToEnterYardMode
    ******************************************************************************/
    bool  AbstractModeControl::isAllowedToEnterYardMode() const
    {
      bool isAllowed = false;
      
      if (yardSeq != static_cast<YardModeRequestSeq *>(NULL))
      {
        isAllowed = yardSeq->isDMIButtonNeeded();
      }

      return isAllowed;
    }

    /******************************************************************************
    * isAllowedToLogin
    ******************************************************************************/
    bool  AbstractModeControl::isAllowedToLogin() const
    {
      bool isAllowed = false;
      
      if (driverLoginSeq != static_cast<DriverLoginSeq *>(NULL))
      {
        isAllowed = driverLoginSeq->isAllowedToLogin();
      }

      return isAllowed;
    }

    /******************************************************************************
    * isAllowedToAbortSetup
    ******************************************************************************/
    bool  AbstractModeControl::isAllowedToAbortSetup() const
    {
      return commonData.isAbortSetupActive;
    }


    /******************************************************************************
    * isAllowedToEnterPosMode
    ******************************************************************************/
    bool AbstractModeControl::isAllowedToEnterPosMode() const
    {
      bool isAllowed = false;
      
      if (posSeq != static_cast<PosModeRequestSeq *>(NULL))
      {
        isAllowed = posSeq->isDMIButtonNeeded();
      }

      return isAllowed;
    }

    /******************************************************************************
    * isAllowedToEnterShuntMode
    ******************************************************************************/
    bool AbstractModeControl::isAllowedToEnterShuntMode() const
    {
      bool isAllowed = false;
      
      if (shuntSeq != static_cast<ShuntModeRequestSeq *>(NULL))
      {
        isAllowed = shuntSeq->isDMIButtonNeeded();
      }

      return isAllowed;
    }

    /******************************************************************************
    * isAllowedToDisplayHandlingDone
    ******************************************************************************/
    bool AbstractModeControl::isAllowedToDisplayHandlingDone() const
    {
      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      bool retFlag = false;
      if ((ATOModeManual == getATOMode()) && isStandStill && (ATPModeLocation == currentMode))
      {
        retFlag = true;
      }
      return retFlag;
    }

    /******************************************************************************
    * isAllowedToDisplayFreeRolling
    ******************************************************************************/
    bool AbstractModeControl::isAllowedToDisplayFreeRolling() const
    {
      return commonData.freeRollingButton.freeRollingDisplayToDMI;
    }

    /******************************************************************************
    * isAllowedToDisplayConfirmFreeRolling
    ******************************************************************************/
    bool AbstractModeControl::isAllowedToDisplayConfirmFreeRolling() const
    {
      return commonData.freeRollingButton.freeRollingConfirmToDMI;
    }

    /******************************************************************************
    * preProcessEventsForAllModes
    ******************************************************************************/
    void AbstractModeControl::preProcessEventsForAllModes()
    {
      // Manage power-down, once detected powering down sequence will be executed.
      const bool reqPowerdown = checkRequestPowerdown();
      const bool isSafetyHaltRequest = ATC::AbstractEventHandler::corePtr()->isModeChangeToSafetyHalt();
      //
      // Evaluating conditions for going to APTModes in priority order, highest priority first.
      //
      // ATPModePoweringDown can NOT be exited (without powering off the AOS system).
      // ATPModeSafetyHalt can ONLY be exited by requesting ATPModePoweringDown.
      if (ATPModeUndefined != commonData.modeReqByDMI)
      {
        // Update current and previous mode
        previousMode = currentMode;
        currentMode = commonData.modeReqByDMI;
        commonData.modeReqByDMI = ATPModeUndefined;
        //if previous mode is location
        if (ATPModeLocation == previousMode)
        {
          handleLocationData();
        }
        // Reset the new mode
        modeList[currentMode]->resetMode();
      }

      // Goto ATPModeSafetyHalt?
      else if ((!getInhibitAllBrakes()) && (isSafetyHaltRequest) &&
          (ATPModeSafetyHalt != currentMode) && (ATPModePoweringDown != currentMode))
      {
        gotoATPModeSafetyHalt();
      }

      // Goto ATPModeSafeBrakeToStop?
      else if ((ATC::AbstractEventHandler::corePtr()->isModeChangeToSafeBrakeToStop()) &&
        (ATPModeSafeBrakeToStop != currentMode) &&
        (ATPModeSafetyHalt != currentMode) &&
        (ATPModePoweringDown != currentMode) &&
        (ATPModeSleeping != currentMode) &&
        (ATPModeUnregistered != currentMode))
      {
        // Update current and previous mode
        previousMode = currentMode;
        currentMode = ATPModeSafeBrakeToStop;
        // Reset the new mode
        modeList[currentMode]->resetMode();

        // Raising Safety Halt Event if SafeBrakeToStop mode is entered from below modes
        if ((ATPModeShuntingRoute == previousMode)
          || (ATPModeSplit == previousMode))
        {
          // Raise Safety Halt Event
          ATC::AbstractEventHandler::corePtr()->reportEvent(safetyHaltInSBTSInvalidPreviousMode, __FILE__,
            __LINE__);
        }
      }

      // Goto ATPModePoweringDown?
      else if ((ATPModePoweringDown != currentMode) && reqPowerdown)
      {
        // Update current and previous mode
        previousMode = currentMode;
        currentMode = ATPModePoweringDown;
        traceMode();
        // Reset the new mode
        modeList[currentMode]->resetMode();

        // Removing train setup
        DS::AbstractTSetup::corePtr()->removeTrainSetup();
      }
      else
      {
        //do nothing
      }
    }

    /******************************************************************************
    * postProcessEventsForAllModes
    ******************************************************************************/
    void AbstractModeControl::postProcessEventsForAllModes()
    {
      // Process the deactivation of the ATP OK signal/status if a SHT event occurs.
      if (ATC::AbstractEventHandler::corePtr()->isModeChangeToSafetyHalt())
      {
        // Deactivate the AOS OK signal/status if a SHT event occurs (even if not allowed to enter SHT Mode).
        commonData.atpOkStatus = false;
      }
    }

    /******************************************************************************
    * handleLocationData
    ******************************************************************************/
    void AbstractModeControl::handleLocationData()
    {
      //do nothing
    }

    /******************************************************************************
    * gotoATPModeSafetyHalt
    ******************************************************************************/
    void AbstractModeControl::gotoATPModeSafetyHalt()
    {
      // Update current and previous mode
      previousMode = currentMode;
      currentMode = ATPModeSafetyHalt;

      // Reset the new mode
      modeList[currentMode]->resetMode();

      // Remove train setup 
      DS::AbstractTSetup::corePtr()->removeTrainSetup();
    }

    /******************************************************************************
    * timeToSendYardRequest
    ******************************************************************************/
    bool AbstractModeControl::timeToSendYardRequest() const
    {
      bool timeToSendReq;
      
      if (yardSeq != static_cast<YardModeRequestSeq *>(NULL))
      {
        timeToSendReq = (YardModeRequestSeq::yardDmiButtonSendReq == yardSeq->getModeReqSeqState());
      }
      else
      {
        timeToSendReq = false;
      }

      return timeToSendReq;
    }

    /******************************************************************************
    * timeToSendPossesionReqest
    ******************************************************************************/
    bool AbstractModeControl::timeToSendPossesionReqest() const
    {
      bool timeToSendReq = false;
      if (posSeq != static_cast<PosModeRequestSeq *>(NULL))
      {
        timeToSendReq = (PosModeRequestSeq::posDmiButtonSendReq == posSeq->getModeReqSeqState());
      }
      return timeToSendReq;
    }

    /******************************************************************************
    * timeToSendShuntRequest
    ******************************************************************************/
    bool AbstractModeControl::timeToSendShuntRequest() const
    {
      bool timeToSendReq = false;
      
      if (shuntSeq != static_cast<ShuntModeRequestSeq *>(NULL))
      {
        timeToSendReq = (ShuntModeRequestSeq::shuntDmiButtonSendReq == shuntSeq->getModeReqSeqState());
      }
      return timeToSendReq;
    }

    /******************************************************************************
    * runAllSeqs
    ******************************************************************************/
    void AbstractModeControl::runAllSeqs()
    {
      // Run all the sequences
      if (driverLoginSeq != static_cast<DriverLoginSeq*>(NULL))
      {
        driverLoginSeq->run();
      }
      //EA sequence
      if (emergencyAlertAlarmSeq != static_cast<EmergencyAlertSeq*>(NULL))
      {
        emergencyAlertAlarmSeq->run(commonData);
      }
      //Shunting sequence
      if (shuntSeq != static_cast<ShuntModeRequestSeq *>(NULL))
      {
        shuntSeq->run(commonData);
      }
      //Possession sequence
      if (posSeq != static_cast<PosModeRequestSeq*>(NULL))
      {
        posSeq->run(commonData);
      }
      //Yard sequence
      if (yardSeq != static_cast<YardModeRequestSeq*>(NULL))
      {
        yardSeq->run(commonData);
      }
      //config sequence
      if (configSeq != static_cast<ConfigModeRequestSeq *>(NULL))
      {
        configSeq->run(commonData);
      }
    }

    /******************************************************************************
    * getAlarm
    ******************************************************************************/
    BuzzerType AbstractModeControl::getBuzzerRequest(void) const
    {
      return commonData.buzzer;
    }

    /******************************************************************************
    * toggleFrequencyAOSStatus
    ******************************************************************************/
    uint8_t AbstractModeControl::getToggleFrequencyAOSStatus(void) const
    {
      return aosStatusToggleFrequency;
    }

    /******************************************************************************
    * inhibitAllBrakes
    ******************************************************************************/
    bool AbstractModeControl::getInhibitAllBrakes() const
    {
      const Kernel::SleepingModeState sleepingModeState = Kernel::AbstractModeControl::corePtr()->getSleepingModeState();
      bool value = true;
      if (Kernel::AbstractModeControl::corePtr()->getCurrentMode() == ATPModeSleeping)
      {
        if (Kernel::SleepingMode::sleepingWaitNotSleeping == sleepingModeState)
        {
          value = false;
        }
        else
        {
          value = true;
        }
      }
      else
      {
        value = false; //Activate Brakes in all other modes.
      }
      return value;
    }

    /******************************************************************************
    * getTimeDueToIntimidateTCCForATPReset
    ******************************************************************************/
    uint32_t AbstractModeControl::getTimeDueToIntimidateTCCForATPReset()
    {
      return 36000U; //Time in seconds(for 10 hours)
    }

    /******************************************************************************
    * getCurrentDrivDirection
    ******************************************************************************/
    TravelDir AbstractModeControl::getCurrentDrivingDirection() const
    {
      return commonData.currentDrivDirection;
    }

    /******************************************************************************
    * logVersionToTCC
    ******************************************************************************/
    void AbstractModeControl::logVersionToTCC()
    {
      trace.write(ATC::detailedLevel, "AOS Software, Configuration and Hardware version should be retrieved by Adaptation");
      writeToLog(ATC::DetailedLog, "AOS Software, Configuration and Hardware version should be retrieved by Adaptation", __FILE__, __LINE__);
    }

    /******************************************************************************
    * sendAbortSetupToTCC
    ******************************************************************************/
    bool AbstractModeControl::sendAbortSetupToTCC(AbortReason &abortReason) const
    {
      abortReason = commonData.abortSetupReason;
      return commonData.trainSetupAborted;
    }

    /******************************************************************************
    * getModeObj
    ******************************************************************************/
    const AbstractMode* AbstractModeControl::getModeObj(const ATPMode mode) const
    {
      return modeList[mode];
    }

  }
}

//lint +esym(586,snprintf)
