/********************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This is the core implementation of the supervise component
*  Supervise component, supervises the most restrictive speed. Supervise indicates
*  permitted speed, target speed and the remaining distance to the target point to
*  the driver. The component turns on/off an buzzer when certain zones are passed.
*  It also requests the brakes in the case of over speeding
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-09    Hidaji      Created
* 2016-10-05    Hidaji      Implementation of functions
* 2016-10-11    Hidaji      Added calcDeceleration(), adjustGradient() added some function calls, and to_do explanations
* 2016-10-12    arastogi    Fixed a null pointer problem in getFirstSupTarget
* 2016-10-14    arastogi    Convert brakeability to cm/s2 and brake response time to 0.1s
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "abstract_config.hpp"
#include "abstract_supervise.hpp"
#include "abstract_odometry.hpp"
#include "abstract_position.hpp"
#include "abstract_target_calculation.hpp"
#include "abstract_analyzer_if.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "abstract_tims.hpp"
#include "atc_math.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_supervise_event_ids.hpp"
#include "abstract_message_handler.hpp"

#include <vfw_checkpoints.h>
#include <vfw_string.h>

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
  namespace Supv
  {
    bool AbstractSupervise::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments searches for the "help", "trace" or any other Console
      component specific command calls and handles it. Returns true if completely handled
      else returns false. returning false will let other components handle the call. help always returns false.
      */

      bool completelyHandled = false;
      char_t  buffer[512];

      // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        const char_t* const helpText =
          "brakeab       Brakeability for current speed and brake system type in use\n"
          "brkDelay      Brake delay for SB and EB for the brake system type in use\n"
          "dist2BCA      Distance to Brake curve area (BCA)\n"
          "curmrt        Current most restrictive target position\n"
          "rap           Roll Away Protection information\n"
          "rmp           Reverse Movement Protection information\n";

        ATC::AbstractConsole::corePtr()->write(helpText);
        completelyHandled = false;
      }
      else if (ATC::isTextMatch(&argv[0][0], "brakeab", sizeof("brakeab")) && (argc == 1U))
      {
        char_t brakeType[30U];
        if (DS::AbstractTSetup::corePtr()->isTrainSetupValid())
        {
          if (DS::AbstractTSetup::corePtr()->getBrakeSystemInUse() == BrakeSystemType1)
          {
            static_cast<void>(vfw_strlcpy(&brakeType[0], "Pneumatic", sizeof(brakeType)));
          }
          else
          {
            static_cast<void>(vfw_strlcpy(&brakeType[0], "ECPB", sizeof(brakeType)));
          }
        }
        else
        {
          static_cast<void>(vfw_strlcpy(&brakeType[0], "default (Pneumatic)", sizeof(brakeType)));
        }

        const int32_t result = snprintf(
          &buffer[0],
          sizeof(buffer),
          "Brakeability with brake system %s \n"
          "and current speed: %d cm/s^2",
          brakeType,
          DS::AbstractTSetup::corePtr()->getBrakeability(Pos::AbstractOdometry::corePtr()->getSpeed()));

        if ((result > 0) && (static_cast<size_t>(result) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }
        completelyHandled = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "dist2BCA", sizeof("dist2BCA")) && (argc == 1U))
      {
        const int32_t result = snprintf(&buffer[0], sizeof(buffer), "Distance to BCA: %d cm", this->getDistanceToBCA());

        if ((result > 0) && (static_cast<size_t>(result) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }
        completelyHandled = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "curmrt", sizeof("curmrt")) && (argc == 1U))
      {
        const int32_t currLeadPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();
        DS::SupervisedTarget* mostRestTarget = AbstractTargetCalculation::corePtr()->getMostRestrictiveTarget();
        if (static_cast<DS::SupervisedTarget*>(NULL) != mostRestTarget)
        {
          int32_t result = snprintf(&buffer[0], sizeof(buffer), "Current MRT Position: %d", mostRestTarget->getOdometer());

          if ((result > 0) && (static_cast<size_t>(result) < sizeof(buffer)))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          }

          result = snprintf(&buffer[0], sizeof(buffer), "Current MRT Parent Position: %d", mostRestTarget->getParentTarget()->getOdometer());

          if ((result > 0) && (static_cast<size_t>(result) < sizeof(buffer)))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          }

          result = snprintf(&buffer[0], sizeof(buffer), "Current front position: %d", currLeadPos);

          if ((result > 0) && (static_cast<size_t>(result) < sizeof(buffer)))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          }
        }
        else
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline("Current MRT Position: No MRT found");
        }
        completelyHandled = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "brkDelay", sizeof("brkDelay")) && (argc == 1U))
      {
        const int32_t result = snprintf(&buffer[0], sizeof(buffer), "Brake delay (0.1s) EB: %d, SB: %d",
            DS::AbstractTSetup::corePtr()->getEmergencyBrakeResponseTime(), DS::AbstractTSetup::corePtr()->getServiceBrakeResponseTime());

        if ((result > 0) && (static_cast<size_t>(result) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }
        completelyHandled = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "rap", sizeof("rap")) && (argc == 1U))
      {
        OdoPosition currentOdoPos = Pos::AbstractOdometry::corePtr()->getOdoPositionWithoutOffset();
        OdoPosition rollAwayDistance = currentOdoPos - rollAwayStartPos;
        const int32_t result = snprintf(&buffer[0], sizeof(buffer), "Roll Away Protection distance: %d", rollAwayDistance);

        if ((result > 0) && (static_cast<size_t>(result) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }
        completelyHandled = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "rmp", sizeof("rmp")) && (argc == 1U))
      {
        const int32_t result = snprintf(&buffer[0], sizeof(buffer), "Reverse Movement Protection distance: %d", reverseDistance);

        if ((result > 0) && (static_cast<size_t>(result) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }
        completelyHandled = true;
      }
      else
      {
        //Do Nothing
      }

      return completelyHandled;
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractSupervise::AbstractSupervise() : ATC::ProcComponent(atpSuperviseId, "Supervise", "SUP"),
      // creating different set of objects for different type of events
      errorInTargetList(ATC::Event::createSafetyHaltEvent(atpSuperviseId, ATC::CoreContainer, eventIdTargetListError,
        ATC::NoEB, DMICom::supTargetListErr, "Target list error, No target found for supervision in non-empty target list!")),
      serviceBrakeCeilingSpeed(ATC::Event::createSBReqEvent(atpSuperviseId, ATC::CoreContainer, eventIdServiceBrakeCeilingSpeed,
        ATC::ATOSB, DMICom::sbOnCeilingSpeed, "Service brake on ceiling speed supervision")),
      standstillCeilingSpeedZero(ATC::Event::createStandstillEvent(atpSuperviseId, ATC::CoreContainer, eventIdStandstillCeilingSpeedZero,
        ATC::ATOSB, 0x0U, "Standstill is applied, ceiling speed is zero")),
      standstillNoTarget(ATC::Event::createStandstillEvent(atpSuperviseId, ATC::CoreContainer, eventIdStandstillNoTarget,
        ATC::ATOSB, 0x0U, "Standstill is applied, no target available")),
      serviceBrakeTargetSupervision(ATC::Event::createSBReqEvent(atpSuperviseId, ATC::CoreContainer, eventIdServiceBrakeTargetSupervision,
        ATC::ATOSB, DMICom::serviceBrakeTargetSupervision, "Service brake on target supervision!")),
      serviceBrakeStandstillSupervision(ATC::Event::createSBReqEvent(atpSuperviseId, ATC::CoreContainer, eventIdServiceBrakeStandstillSupervision,
        ATC::ATOSB, DMICom::serviceBrakeStandstillSupervision, "Service brake on standstill supervision!")),
      emergencyBrakeStandstillSupervision(ATC::Event::createEBReqEvent(atpSuperviseId, ATC::CoreContainer,
        eventIdEmergencyBrakeStandstillSupervision, ATC::DispatcherEB, DMICom::emergencyBrakeStandstillSupervision,
        "Emergency brake on standstill supervision!")),
      serviceBrakeTIMSBroken(ATC::Event::createSBReqEvent(atpSuperviseId, ATC::CoreContainer, eventIdServiceBrakeTIMSBroken,
        ATC::ATOSB, DMICom::timsIntegrityBrokenStillSupervised, "Service brake when TIMS Broken!")),
      emergencyBrakeCeilingSpeed(ATC::Event::createEBReqEvent(atpSuperviseId, ATC::CoreContainer, eventIdEmergencyBrakeCeilingSpeed,
        ATC::DispatcherEB, DMICom::emergencyBrakeCeilingSpeed, "Emergency brake on ceiling speed supervision!")),
      emergencyBrakeTargetSupervision(ATC::Event::createEBReqEvent(atpSuperviseId, ATC::CoreContainer, eventIdEmergencyBrakeTargetSupervision,
        ATC::DispatcherEB, DMICom::emergencyBrakeTargetSupervision, "Emergency brake on target supervision!")),
      firstWarningSpeedExceed(ATC::Event::createLogEvent(atpSuperviseId, ATC::CoreContainer, eventIdFirstWarnSpeedExceed,
        DMICom::firstWarningTargetSupervision, "Speed exceeds first warning curve")),
      secondWarningSpeedExceed(ATC::Event::createLogEvent(atpSuperviseId, ATC::CoreContainer, eventIdSecondWarnSpeedExceed,
        DMICom::secondWarningTargetSupervision, "Speed exceeds second warning curve")),
      speedWarningLimitExceed(ATC::Event::createLogEvent(atpSuperviseId, ATC::CoreContainer, eventIdWarningLimitExceed,
        DMICom::secondWarningCeilingSpeedSupervision, "Speed exceeds warning speed limit")),
      ceilingSpeedLimitExceed(ATC::Event::createLogEvent(atpSuperviseId, ATC::CoreContainer, eventIdCeilingSpeedExceed,
        DMICom::firstWarningCeilingSpeedSupervision, "Speed exceeds ceiling speed limit")),
      revSupvError(ATC::Event::createSBReqEvent(atpSuperviseId, ATC::CoreContainer, eventIdRevDistExceeded, ATC::NoSB,
        DMICom::revSupvError, "Service brake on reversing supervision!")),
      revEBMarginError(ATC::Event::createEBReqEvent(atpSuperviseId, ATC::CoreContainer, eventIdRevEBMarginExceeded,
        ATC::NoEB, DMICom::revEBMarginError, "Emergency brake on reversing supervision!")),
      rollAwaySupvError(ATC::Event::createSBReqEvent(atpSuperviseId, ATC::CoreContainer, eventIdRollAwayDistExceeded,
        ATC::NoSB, DMICom::rollAwaySupvError, "Service brake on roll-away supervision!")),
      rollAwayEBMarginError(ATC::Event::createEBReqEvent(atpSuperviseId, ATC::CoreContainer, eventIdRollAwayEBMarginExceeded,
        ATC::NoEB, DMICom::rollAwayEBMarginError, "Emergency brake on roll-away supervision!")),
      locationSupvError(ATC::Event::createSBReqEvent(atpSuperviseId, ATC::CoreContainer, eventIdLocationBorderExceeded,
        ATC::NoSB, DMICom::locationSupvError, "Service brake on location supervision!")),
      initDone(false),
      inReleaseSpeedArea(false),
      requestEmAlert(false),
      currentZone(NoZone),
      currentSpeedLimit(NoSpeedLimit),
      buzzer(BuzzerTypeNone),
      atpWarning(false),
      atpIntervention(false),
      inBCA(false),
      distanceToBCA(0),
      distanceToTarget(0),
      mrtOdo(0),
      speedAtTarget(0U),
      timeToIntervention(ATC::maxTimeValue),
      predDistToStandStill(0),
      firstWarningSpeed(0U),
      secondWarningSpeed(0U),
      sbSpeed(0U),
      ebSpeed(0U),
      prevTargetListEmpty(true),
      targetId(0U),
      standstillOdoPos(0),
      rollAwayStartPos(0),
      triggerStandstillSup(false),
      resetOfStandStillRefPosition(false),
      resetOfReverseRefPosition(false),
      warningLimitEventReportedToTCC(false),
      speedExceedSBSpeed(false),
      reverseDistance(0U)
    {
      if (coreSuperviseInstancePtr != NULL)
      {
        // Error handler
        ATC::aosHalt(__FILE__, __LINE__, "Supervise constructor already instantiated");
      }

      // Setup single instance pointer for core access
      coreSuperviseInstancePtr = this;
      requestEmAlert = false;
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractSupervise::init(void)
    {
      if (!initDone)
      {
        ATC::AbstractAnalyzerIF* const aif = ATC::AbstractAnalyzerIF::corePtr();
        const bool resFWSpeedAIF = aif->registerMeasurement("firstWarningSpeed", "first Warning Speed",
          "cm/s", 0U, ATC::uint32Max, &firstWarningSpeed);
        const bool resSWSpeedAIF = aif->registerMeasurement("secondWarningSpeed", "second Warning Speed", "cm/s",
          0U, ATC::uint32Max, &secondWarningSpeed);
        const bool resSBSpeedAIF = aif->registerMeasurement("sbSpeed", "SB Speed", "cm/s",
          0U, ATC::uint32Max, &sbSpeed);
        const bool resEBSpeedAIF = aif->registerMeasurement("ebSpeed", "EB Speed", "cm/s",
          0U, ATC::uint32Max, &ebSpeed);
        const bool resTimeToIntvAIF = aif->registerMeasurement("timeToIntervention", "Time To Intervention", "s",
          0U, ATC::uint32Max, &timeToIntervention);

        if ((!resFWSpeedAIF) || (!resSWSpeedAIF) || (!resSBSpeedAIF) || (!resEBSpeedAIF) || (!resTimeToIntvAIF))
        {
          writeToLog(ATC::BriefLog,"Register measurement failed for analyzer", __FILE__, __LINE__);
        }

        initCrossCompare();
        initDone = true;
      }
      return initDone;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void AbstractSupervise::run(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "SUP_run");

      bool superviseCeiling = true;
      bool superviseTarget = true;
      bool superviseStandstill = true;
      bool superviseReleaseSpeed = true;
      bool superviseLocation = false;
      const uint32_t currentSpeed = Pos::AbstractOdometry::corePtr()->getSpeed();
      const uint32_t calcCeilingSpeed = AbstractTargetCalculation::corePtr()->getCalcCeilingSpeed();

      // Making sure an buzzer with higher priority is not overwritten and onebeep is reset if it is set earlier.
      buzzer = (buzzer == BuzzerTypeOneBeep) ? BuzzerTypeNone : buzzer;

      handleModes(superviseCeiling, superviseTarget, superviseStandstill, superviseReleaseSpeed, superviseLocation);

      // Reset buzzer, atpWarning, and atpIntervention
      atpWarning = false;
      atpIntervention = false;

      // Reset all target data
      inBCA = false;
      distanceToBCA = 0;
      timeToIntervention = ATC::maxTimeValue;
      predDistToStandStill = 0;
      targetId = 0U;
      inReleaseSpeedArea = false;
      requestEmAlert = false;

      verifyBrakeabilityVsGradient();

      revSupervision();

      rollAwaySupervision();

      if (superviseReleaseSpeed && (calcCeilingSpeed > 0U)) // Mode Dependent
      {
        superviseCeilingSpeed(currentSpeed, ATC::ATCMath::maximum(calcCeilingSpeed,
          static_cast<uint32_t>(AbstractConfig::corePtr()->getReleaseSpeed())));
      }
      else if (superviseCeiling) // Mode Dependent
      {
        superviseCeilingSpeed(currentSpeed, calcCeilingSpeed);
      }
      else 
      {
        // For lint
      }

      const Pos::PosAccuracyState currentPosState = Pos::AbstractPosition::corePtr()->getAccuracyState();
      const bool validPosition = ((Pos::PosKnown == currentPosState)
        || (Pos::PosApprox == currentPosState));

      //supervision will be done in valid mode and know/approximate position
      if (superviseTarget && validPosition)
      {
        bool newTarget = false;

        const TravelDir tDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
        const int32_t gradient = DS::AbstractTargets::corePtr()->getCurGradient();
        const int32_t currLeadPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();

        // If target list is not empty
        if (!DS::AbstractTargets::corePtr()->isSupervisedTargetListEmpty())
        {
          const DS::SupervisedTarget* const supvTarg = getFirstSupTarget(tDir, newTarget);

          if (supvTarg != static_cast<const DS::BaseTarget* const>(NULL))
          {
            /*
            * Worst brakeability will be used for the different curve calculation
            * Worst brakeability will be calculated between:
            * EB ceiling speed (max speed) at current position and FW speed ((minimum speed) at next target.
            */
            int32_t brakeability = DS::AbstractTSetup::corePtr()->getWorstBrakeabilityInRange(
                                                                  supvTarg->getFirstWarningSpeed(), 
                                                                  DS::AbstractTargets::corePtr()->getCurrEbCeilingSpeed());

            //calculate deceleration on basis of brakeability and gradient
            int32_t deceleration = BrakeCalculations::instance().calcDeceleration(brakeability, gradient);

            superviseTargets(currentSpeed, currLeadPos, tDir, gradient, deceleration, calcCeilingSpeed, *supvTarg,
              newTarget, superviseReleaseSpeed);

            calcDMIDisplayData();

          }
          else
          {
            // No target is found, but the target list was not empty!
            ATC::AbstractEventHandler::corePtr()->reportEvent(errorInTargetList, __FILE__, __LINE__);
          }
        }
        else  // We did not find a target
        {
          // Apply Standstill if target list is empty
          ATC::AbstractEventHandler::corePtr()->reportEvent(standstillNoTarget, __FILE__, __LINE__);
          firstWarningSpeed = 0U;
          secondWarningSpeed = 0U;
          sbSpeed = 0U;
          ebSpeed = 0U;
          currentZone = NoZone;
          mrtOdo = 0;
          speedAtTarget = 0U;
          distanceToTarget = 0;
        }
      }
      else
      {
        //If the release speed is supervised and the current FW speed is not zero, take the max of release speed and CS.
        if ((DS::AbstractTargets::corePtr()->getCurrFwCeilingSpeed() > 0U) && (superviseReleaseSpeed))
        {
          firstWarningSpeed = ATC::ATCMath::maximum(calcCeilingSpeed, static_cast<uint32_t>(AbstractConfig::corePtr()->getReleaseSpeed()));
        }
        else
        {
          firstWarningSpeed = calcCeilingSpeed;
        }
      }

      if (superviseStandstill) // Mode Dependent
      {
        superviseTrainAtStandstill();
      }

      if (superviseLocation)
      {
        locationSupervision();
      }

      superviseTims();

      // set prevTargetListEmpty for next cycle
      prevTargetListEmpty = DS::AbstractTargets::corePtr()->isSupervisedTargetListEmpty();
    }

    /******************************************************************************
    * verifyBrakeabilityVsGradient
    ******************************************************************************/
    void AbstractSupervise::verifyBrakeabilityVsGradient()
    {
      if (Kernel::AbstractMessageHandler::corePtr()->isValidMAReceived())
      {
        int32_t minGradient = DS::AbstractTargets::corePtr()->getCurGradient();
        DS::SupervisedTargetIterator it = DS::AbstractTargets::corePtr()->getSupervisedTargetIter();

        while (it != DS::AbstractTargets::corePtr()->getSupervisedTargetIterEnd())
        {
          const int32_t gradient = (*it)->getGradient();

          if (minGradient > gradient)
          {
            minGradient = gradient;
          }

          ++it;
        }

        const DS::TrainSetup* const trainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

        if (trainSetup != static_cast<const DS::TrainSetup*>(NULL))
        {
          const int32_t brakeability = DS::AbstractTSetup::corePtr()->getWorstBrakeabilityInRange(
            0U, trainSetup->maxSpeed);

          const int32_t deceleration = BrakeCalculations::instance().calcDeceleration(
            brakeability, minGradient);

          if (deceleration <= 0)
          {
            requestEmAlert = true;
          }
        }
      }
    }

    /******************************************************************************
    * calcDMIDisplayData
    ******************************************************************************/
    void AbstractSupervise::calcDMIDisplayData(void)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_calcDMIDisplayData_begin");

      const uint32_t currentSpeed = Pos::AbstractOdometry::corePtr()->getSpeed();
      const int32_t currLeadPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();
      const int32_t gradient = DS::AbstractTargets::corePtr()->getCurGradient();
      const TravelDir tDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();

      DS::SupervisedTargetIterator mostRestTargetItr = DS::AbstractTargets::corePtr()->getSupervisedTargetIter();
      DS::SupervisedTargetIterator nextRestTargetItr = DS::AbstractTargets::corePtr()->getSupervisedTargetIter();

      findRestrictiveTarget(mostRestTargetItr, nextRestTargetItr);
      DS::SupervisedTarget* mostRestTarg = *mostRestTargetItr;

      //calculate the distance to BCA.
      calcDistanceToBCA(currLeadPos, nextRestTargetItr);

      uint32_t fwCeilSpeed = DS::AbstractTargets::corePtr()->getCurrFwCeilingSpeed();
      fwCeilSpeed = AbstractTargetCalculation::corePtr()->calcModeDependentCeilingSpeed(fwCeilSpeed);

      //check if train entered in BCA area and indicate to DMI.
      //train is in BCA if the FW speed is less than the current FW ceiling speed.
      // for case when CS is equal to release speed, check if train is in release speed area.
      if ((firstWarningSpeed < fwCeilSpeed) || inReleaseSpeedArea)
      {
        inBCA = true;
      }
      // for primary target train is in BCA if train is within MA margin
      else if(mostRestTarg->getParentTarget()->getTargetType() == DS::BaseTarget::PrimaryTarget)
      {
        // get maMargin from primary target (if any)
        const DS::PrimaryTarget* const pTarget =
          ATC::dynamicCast<DS::BaseTarget*, DS::PrimaryTarget*>(mostRestTarg->getParentTarget(), __FILE__, __LINE__);

        int32_t maMargin = static_cast<int32_t>(pTarget->getMAMargin());
        inBCA = (pTarget->getDirection() == DirForward)?(currLeadPos >= (pTarget->getOdometer() - maMargin))
                                                                      :(currLeadPos <= (pTarget->getOdometer() + maMargin));
      }
      else
      {
        //Do Nothing
      }

      if (inBCA)
      {
        uint32_t targetSpeed = mostRestTarg->getFirstWarningSpeed();
        int32_t brakeability = DS::AbstractTSetup::corePtr()->getWorstBrakeabilityInRange(targetSpeed, currentSpeed);
        int32_t deceleration = BrakeCalculations::instance().calcDeceleration(brakeability, gradient);
        const int16_t curAcceleration = Pos::AbstractOdometry::corePtr()->getAcceleration();

        //set the distance to target to position where speed is below release speed, when supervising a primary target
        if(mostRestTarg->getParentTarget()->getTargetType() == DS::BaseTarget::PrimaryTarget)
        {
          mrtOdo = AbstractTargetCalculation::corePtr()->getReleaseSpSBPos();
        }
        //for sm speed restriction or speed target, distance to target is same as to supervised target.
        else
        {
          mrtOdo = mostRestTarg->getOdometer();
        }

        speedAtTarget = mostRestTarg->getFirstWarningSpeed();

        distanceToTarget = (tDir == DirForward)?(mrtOdo - currLeadPos):(currLeadPos - mrtOdo);

        // Set predDistToStandStill
        predDistToStandStill = BrakeCalculations::instance().calcPredictedDistanceToStandStillLocation(currentSpeed, curAcceleration);

        // TODO: This is based on current gradient and current deceleration which is not accurate. We need to consider the SB target
        calcTimeToIntervention(mostRestTarg, currentSpeed, curAcceleration, currLeadPos, tDir, gradient, deceleration); // Sets timeToIntervention
      }
      else
      {
        mrtOdo = 0;
        speedAtTarget = 0U;
        distanceToTarget = 0;
      }

      vfwVisitCheckPoint(&endCp, "SUP_calcDMIDisplayData_end");
    }
    /******************************************************************************
    * superviseTrainAtStandstill
    ******************************************************************************/
    void AbstractSupervise::superviseTrainAtStandstill(void)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_superviseTrainAtStandstill_begin");

      bool isValidMode = (!Kernel::AbstractModeControl::corePtr()->getInhibitAllBrakes());
      //Only allow brakes if ATPMode is Sleeping and modeState is sleepingWaitNotSleeping or other ATPModes
      if (isValidMode)
      {
        if (ATC::AbstractEventHandler::corePtr()->isStandstillEventActive())
        {
          if (!triggerStandstillSup)
          {
            //For the first iteration when Standstill event is active. Save the position as standstill position to check for movement.
            standstillOdoPos = Pos::AbstractOdometry::corePtr()->getOdoPositionWithoutOffset();
            triggerStandstillSup = true;
            resetOfStandStillRefPosition = false;
          }
          else
          {
            const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

            // Reset reference position in case of brake event and coming to standstill.
            if (isStandStill && resetOfStandStillRefPosition)
            {
              standstillOdoPos = Pos::AbstractOdometry::corePtr()->getOdoPositionWithoutOffset();
              resetOfStandStillRefPosition = false;
            }

            // Check if train is moving more than configured limit (Standstill Supervision).
            OdoPosition currentOdoPos = Pos::AbstractOdometry::corePtr()->getOdoPositionWithoutOffset();
            OdoPosition movementAtStandstill = ATC::ATCMath::instance().absolute(currentOdoPos - standstillOdoPos, __FILE__, __LINE__);
            const uint32_t standStillMargin = AbstractConfig::corePtr()->getRollAwayMargin();

            //Calculating the standstill margin including eb margin
            const uint8_t percentageEBMargin = AbstractConfig::corePtr()->getEBMarginAdded();
            const uint32_t standStillEBMargin = ((standStillMargin * static_cast<uint32_t>(percentageEBMargin)) / 100U)
                                                    + standStillMargin;

            if (movementAtStandstill > static_cast<OdoPosition>(standStillEBMargin))
            {
              // If train has crossed the standstill eb margin apply emergency brake
              ATC::AbstractEventHandler::corePtr()->reportEvent(emergencyBrakeStandstillSupervision, __FILE__, __LINE__);

              resetOfStandStillRefPosition = true;
            }
            else if (movementAtStandstill > static_cast<OdoPosition>(standStillMargin))
            {
              // Apply SB to stop movement when standstill supervision is active.
              ATC::AbstractEventHandler::corePtr()->reportEvent(serviceBrakeStandstillSupervision, __FILE__, __LINE__);

              resetOfStandStillRefPosition = true;
            }
            else
            {
              // To avoid lint warning
            } 
          }
        }
        else
        {
          // Reset trigger to detect Standstill Event next time.
          triggerStandstillSup = false;
        }
      }
      else
      {
        //Reset trigger to detect Standstill Event next time.
        triggerStandstillSup = false;
      }

      vfwVisitCheckPoint(&endCp, "SUP_superviseTrainAtStandstill_end");
    }

    /******************************************************************************
    * superviseTims
    ******************************************************************************/
    void AbstractSupervise::superviseTims() const
    {
      const bool isTimsBroken = (TG::TIMSStatusBroken == TG::AbstractTIMS::corePtr()->getTimsStatus());
      const bool isTimsSupervised = (TG::TIMSSupervised == TG::AbstractTIMS::corePtr()->getTimsSupervision());

      if (isTimsSupervised && isTimsBroken)
      {
        //Apply SB to stop movement when TIMS is broken and Supervision is active.
        ATC::AbstractEventHandler::corePtr()->reportEvent(serviceBrakeTIMSBroken, __FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractSupervise* AbstractSupervise::corePtr()
    {
      return coreSuperviseInstancePtr;
    }

    /******************************************************************************
    * getFirstSupTarget
    ******************************************************************************/
    const DS::SupervisedTarget* AbstractSupervise::getFirstSupTarget(const TravelDir tDir, bool& newTarget)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_getFirstSupTarget_begin");

      const DS::SupervisedTarget* supTarg = static_cast<const DS::SupervisedTarget*>(NULL);
      const int32_t currLeadPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();

      // Find the closest target
      DS::SupervisedTargetIterator it = DS::AbstractTargets::corePtr()->getSupervisedTargetIter();
      while (it != DS::AbstractTargets::corePtr()->getSupervisedTargetIterEnd())
      {
        bool isTargetAhead = ((*it)->getDirection() == DirForward)?((*it)->getOdometer() > currLeadPos):((*it)->getOdometer() < currLeadPos);

        // If the travel direction matches target direction and target is ahead.
        if ((tDir == (*it)->getDirection()) && isTargetAhead)
        {
            supTarg = *it;
            break;  // target is found stop the loop

        }
        ++it;
      }

      if (supTarg != static_cast<const DS::SupervisedTarget*>(NULL))
      {
        // Check if it is a new target
        const uint32_t newTargetId = supTarg->getTargetId();

        if (targetId != newTargetId)
        {
          newTarget = true;
          targetId = newTargetId;
        }

        if (prevTargetListEmpty)  // target list was empty on previous cycle but now a new target is found
        {
          buzzer = (buzzer < BuzzerTypeOneBeep) ? BuzzerTypeOneBeep : buzzer;   // Making sure an buzzer with higher priority is not overwritten
        }
      }
      else
      {
        //Direction controller is in neutral in location mode
        if ((tDir == DirForward) || (tDir == DirReverse))
        {
          // No target is found, but the target list was not empty!
          ATC::AbstractEventHandler::corePtr()->reportEvent(errorInTargetList, __FILE__, __LINE__);
        }
      }

      vfwVisitCheckPoint(&endCp, "SUP_getFirstSupTarget_end");

      return supTarg;
    }

    /******************************************************************************
    * superviseCeilingSpeed
    ******************************************************************************/
    void AbstractSupervise::superviseCeilingSpeed(const uint32_t currentSpeed, const uint32_t calcCeilingSpeed)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_superviseCeilingSpeed_begin");

      uint32_t warningLimit = BrakeCalculations::instance().calcWarningLimitMargin(calcCeilingSpeed);
      uint32_t sbLimit = BrakeCalculations::instance().calcServiceBrakeLimitMargin(calcCeilingSpeed);
      uint32_t ebLimit = BrakeCalculations::instance().calcEmergencyBrakeLimitMargin(calcCeilingSpeed);

      if (0U != calcCeilingSpeed)
      {
        if (currentSpeed >= (calcCeilingSpeed + ebLimit)) // Check conditions for applying Emergency Brake
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(emergencyBrakeCeilingSpeed, __FILE__, __LINE__);
          atpIntervention = true;
          currentSpeedLimit = EBSpeedLimit;
        }
        else if ((currentSpeed >= (calcCeilingSpeed + sbLimit)) && (EBSpeedLimit != currentSpeedLimit)) // Check conditions for SB limit
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(serviceBrakeCeilingSpeed, __FILE__, __LINE__);
          atpIntervention = true;
          currentSpeedLimit = SBSpeedLimit;
        }
        else if (currentSpeed >= (calcCeilingSpeed + warningLimit)) // Check conditions for Warning handling
        {
          atpWarning = true;

          // Continuous beep till the speed is below the ceiling speed
          // Making sure an buzzer with higher priority is not overwritten
          buzzer = (buzzer < BuzzerTypeConstantBeep) ? BuzzerTypeConstantBeep : buzzer;

          if ((SBSpeedLimit != currentSpeedLimit) && (WarningSpeedLimit != currentSpeedLimit)
              && (EBSpeedLimit != currentSpeedLimit))
          {
            if (!warningLimitEventReportedToTCC)
            {
              //Raising log event and informing TCC
              ATC::AbstractEventHandler::corePtr()->reportEvent(speedWarningLimitExceed, __FILE__, __LINE__);
              warningLimitEventReportedToTCC = true;
            }
            currentSpeedLimit = WarningSpeedLimit;
          }
          else if (SBSpeedLimit == currentSpeedLimit)
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(serviceBrakeCeilingSpeed, __FILE__, __LINE__);
            atpIntervention = true;
          }
          else if (EBSpeedLimit == currentSpeedLimit)
          {
            atpIntervention = true;
            //No need to raise EB event as EB event cannot be released until standstill.
          }
          else
          {
            currentSpeedLimit = WarningSpeedLimit;
          }
        }
        else if (currentSpeed > calcCeilingSpeed)
        {
          atpWarning = true;
          if ((SBSpeedLimit != currentSpeedLimit) && (WarningSpeedLimit != currentSpeedLimit)
            && (CeilingSpeedLimit != currentSpeedLimit) && (EBSpeedLimit != currentSpeedLimit))
          {
            // Making sure an buzzer with higher priority is not overwritten
            buzzer = (buzzer < BuzzerTypeOneBeep) ? BuzzerTypeOneBeep : buzzer;
            //Raising log event
            ATC::AbstractEventHandler::corePtr()->reportEvent(ceilingSpeedLimitExceed, __FILE__, __LINE__);
            warningLimitEventReportedToTCC = false;
            currentSpeedLimit = CeilingSpeedLimit;
          }
          else if (SBSpeedLimit == currentSpeedLimit)
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(serviceBrakeCeilingSpeed, __FILE__, __LINE__);
            atpIntervention = true;
          }
          else if (EBSpeedLimit == currentSpeedLimit)
          {
            atpIntervention = true;
            //No need to raise EB event as EB event cannot be released until standstill.
          }
          else
          {
            currentSpeedLimit = CeilingSpeedLimit;
          }
        }
        else
        {
          //current speed is below ceiling speed
          warningLimitEventReportedToTCC = false;
          //to deactivate continue beep 
          buzzer = BuzzerTypeNone;
          currentSpeedLimit = NoSpeedLimit;
        }
      }
      else // ceilingSpeed is zero
      {
        //issue standstill event if the ceiling speed is zero
        ATC::AbstractEventHandler::corePtr()->reportEvent(standstillCeilingSpeedZero, __FILE__, __LINE__);
      }

      vfwVisitCheckPoint(&endCp, "SUP_superviseCeilingSpeed_end");
    }

    /******************************************************************************
    * handleModes
    ******************************************************************************/
    void AbstractSupervise::handleModes(
      bool& superviseCeiling, bool& superviseTarget, bool& superviseStandstill, bool& superviseReleaseSpeed, bool& superviseLocation) const
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_handleModes_begin");

      ATPMode atpMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      switch (atpMode)
      {
      case ATPModeYard:
      case ATPModePossession:
      case ATPModeShunting:
        // These modes do not have a target, only supervise ceiling seed
        superviseCeiling = true;
        superviseTarget = false;
        break;

      case ATPModeLocation:
        // Supervise ceiling speed and target in these modes but not release speed
        superviseCeiling = true;
        superviseTarget = true;
        superviseReleaseSpeed = false;
        superviseLocation = true;
        break;
      case ATPModeSplit:
      case ATPModeStaffResponsible:
      case ATPModeBaliseSearch:
      case ATPModeNormal:
      case ATPModeShuntingRoute:
      case ATPModeJoin:
        // Supervise ceiling speed and target in these modes
        superviseCeiling = true;
        superviseTarget = true;
        break;

      case ATPModeSleeping:
        //No Supervise Ceiling Speed and target in these modes
        superviseCeiling = false;
        superviseTarget = false;
        //Supervise Standstill when ATP is in Sleeping mode and when modeState is sleepingWaitNotSleeping
        if (Kernel::AbstractModeControl::corePtr()->getInhibitAllBrakes())
        {
            superviseStandstill = false;
        }
        else
        {
            superviseStandstill = true;
        }
        break;

      case ATPModeSafeBrakeToStop:
        superviseCeiling = true;    // The ATPModeSafeBrakeToStop carries previous modes ceiling speed
        superviseTarget = true;
        break;

        // atp modes on which, standstill event is triggered
      case ATPModePowerUp:
      case ATPModeConfiguration:
      case ATPModeRegistration:
      case ATPModeUnregistered:
      case ATPModePoweringDown:
        //No supervision needed as these events will trigger Standstill event which in turn will trigger Standstill supervision.
        superviseCeiling = false;
        superviseTarget = false;
        break;

        // atp modes on which, the train is not allowed to move:
      case ATPModeSafetyHalt:
        break;

        // invalid atp modes
      case ATPModeUndefined:
      case ATPModesCount:
      default:
        superviseCeiling = false;
        superviseTarget = false;
        ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        break;
      }

      vfwVisitCheckPoint(&endCp, "SUP_handleModes_end");
    }

    /******************************************************************************
    * calcBrakeCurveSpeedAtCurPosition
    ******************************************************************************/
    void AbstractSupervise::calcBrakeCurveSpeedAtCurPosition(const int32_t currLeadPos, const int32_t gradient, const int32_t deceleration,
      const TravelDir tDir, const uint32_t calcCeilingSpeed, const DS::SupervisedTarget &target, const bool superviseReleaseSpeed)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_calcBrakeCurveSpeedAtCurPosition_begin");

      uint32_t releaseSpeed = AbstractConfig::corePtr()->getReleaseSpeed();
      /***************************************************/
      /* Calculate brake curve speed at current position */
      /***************************************************/

      //First Warning Speed *****************************************************************************************************

      //if not passed first warning TP calculate speed based on current target
      uint32_t minCS = ATC::ATCMath::minimum(calcCeilingSpeed, DS::AbstractTargets::corePtr()->getCurrFwCeilingSpeed());
      firstWarningSpeed = BrakeCalculations::instance().calcFirstWarningCurveSpeed(currLeadPos, tDir, gradient, deceleration, minCS, target);

      //Second Warning Speed ****************************************************************************************************

      const uint32_t warningLimit = BrakeCalculations::instance().calcWarningLimitMargin(calcCeilingSpeed) + calcCeilingSpeed;
      minCS = ATC::ATCMath::minimum(warningLimit, DS::AbstractTargets::corePtr()->getCurrSwCeilingSpeed());
          //if passed second warning TP calculate speed based on next supervisable target
      secondWarningSpeed = BrakeCalculations::instance().calcSecondWarningCurveSpeed(currLeadPos, tDir, gradient,
        deceleration, minCS, target);

      //Service Brake Speed *****************************************************************************************************

      const uint32_t sbLimit = BrakeCalculations::instance().calcServiceBrakeLimitMargin(calcCeilingSpeed) + calcCeilingSpeed;
      minCS = ATC::ATCMath::minimum(sbLimit, DS::AbstractTargets::corePtr()->getCurrSbCeilingSpeed());
          //if passed service brake TP calculate speed based on next supervisable target
      sbSpeed = BrakeCalculations::instance().calcServiceBrakeCurveSpeed(currLeadPos, tDir, gradient,
        deceleration,  minCS, target);

      //Emergency Brake Speed ***************************************************************************************************

      const uint32_t ebLimit = BrakeCalculations::instance().calcEmergencyBrakeLimitMargin(calcCeilingSpeed) + calcCeilingSpeed;
      minCS = ATC::ATCMath::minimum(ebLimit, DS::AbstractTargets::corePtr()->getCurrEbCeilingSpeed());
      ebSpeed = BrakeCalculations::instance().calcEmergencyBrakeCurveSpeed(currLeadPos, tDir, gradient,
        deceleration,  minCS, target);


      if (superviseReleaseSpeed) 
      {
        if (firstWarningSpeed < releaseSpeed)
        {
          inReleaseSpeedArea = true;
        }

        firstWarningSpeed = ATC::ATCMath::maximum(firstWarningSpeed, releaseSpeed);

        const uint32_t marginWarn = BrakeCalculations::instance().calcWarningLimitMargin(releaseSpeed);
        secondWarningSpeed = ATC::ATCMath::maximum(secondWarningSpeed, (marginWarn + releaseSpeed));

        if (DS::AbstractTargets::corePtr()->getCurrSbCeilingSpeed() > 0U)
        {
          int32_t relSpPos = AbstractTargetCalculation::corePtr()->getReleaseSpSBPos();
          bool isCurrPosAhead = (tDir == DirForward)?(currLeadPos >= relSpPos):(currLeadPos <= relSpPos);

          if(isCurrPosAhead)
          {
            //set sb, sw and fw speed to 0
            sbSpeed = 0U;
            secondWarningSpeed = 0U;
            firstWarningSpeed = 0U;
          }
          else
          {
            bool isSupTargAhead = (tDir == DirForward)?(target.getOdometer() >= relSpPos):(target.getOdometer() <= relSpPos);
            const uint32_t marginSB = BrakeCalculations::instance().calcServiceBrakeLimitMargin(releaseSpeed);

            sbSpeed = (isSupTargAhead)? ATC::ATCMath::maximum(sbSpeed, releaseSpeed)
                                      : ATC::ATCMath::maximum(sbSpeed, (releaseSpeed + marginSB));
          }
        }

        if (DS::AbstractTargets::corePtr()->getCurrEbCeilingSpeed() > 0U)
        {
          int32_t relSpPos = AbstractTargetCalculation::corePtr()->getReleaseSpEBPos();
          bool isCurrPosAhead = (tDir == DirForward)?(currLeadPos >= relSpPos):(currLeadPos <= relSpPos);

          if(isCurrPosAhead)
          {
            //set eb, sb, sw and fw speed to 0
            ebSpeed = 0U;
            sbSpeed = 0U;
            secondWarningSpeed = 0U;
            firstWarningSpeed = 0U;
          }
          else
          {
            bool isSupTargAhead = (tDir == DirForward)?(target.getOdometer() >= relSpPos):(target.getOdometer() <= relSpPos);
            const uint32_t marginEB = BrakeCalculations::instance().calcEmergencyBrakeLimitMargin(releaseSpeed);

            ebSpeed = (isSupTargAhead)? ATC::ATCMath::maximum(ebSpeed, releaseSpeed):
                                        ATC::ATCMath::maximum(ebSpeed, (releaseSpeed + marginEB));
          }
        }
      }

      vfwVisitCheckPoint(&endCp, "SUP_calcBrakeCurveSpeedAtCurPosition_end");
    }

    /******************************************************************************
    * superviseTargets
    ******************************************************************************/
    void AbstractSupervise::superviseTargets(const uint32_t currentSpeed, const int32_t currfrontPos, const TravelDir tDir,
      const int32_t gradient, const int32_t deceleration, const uint32_t calcCeilingSpeed, const DS::SupervisedTarget &target,
      const bool newTarget, const bool superviseReleaseSpeed)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_superviseTargets_begin");

      calcBrakeCurveSpeedAtCurPosition(currfrontPos, gradient, deceleration, tDir, calcCeilingSpeed, target, superviseReleaseSpeed);

      /********************************************************
      * Zone condition checks
      ********************************************************/
      const uint32_t maxSpeed = Pos::AbstractOdometry::corePtr()->getFilteredMaxSpeed();

      //EB curve is supervised with the filtered maximum speed from COD.
      if (maxSpeed >= ebSpeed) /* Zone E entry condition */
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(emergencyBrakeTargetSupervision, __FILE__, __LINE__);
        atpIntervention = true;
        currentZone = ZoneE;
      }
      else if ((currentSpeed >= sbSpeed) && (ZoneE != currentZone))/* Zone D entry condition */
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(serviceBrakeTargetSupervision, __FILE__, __LINE__);
        atpIntervention = true;
        speedExceedSBSpeed = true;
        currentZone = ZoneD;
      }
      else if ((currentSpeed >= secondWarningSpeed) ||  /* Zone C entry condition */
         ((currentSpeed >= firstWarningSpeed) && (sbSpeed <= secondWarningSpeed)))
        //this can happen close to primary target when sb speed can go down till release speed while sw speed is limited to release speed + margin
      {
        if ((NoZone == currentZone) || (ZoneA == currentZone) || (ZoneB == currentZone))
        {
          // Making sure an buzzer with higher priority is not overwritten
          buzzer = (buzzer < BuzzerTypeConstantBeep) ? BuzzerTypeConstantBeep : buzzer;
          atpWarning = true;
          //Raising log event and informing TCC
          ATC::AbstractEventHandler::corePtr()->reportEvent(secondWarningSpeedExceed, __FILE__, __LINE__);
        }
        else if (speedExceedSBSpeed)
        {
          //service brake (caused by speed exceeded SB speed) should be raised till speed will be below first warning speed
          ATC::AbstractEventHandler::corePtr()->reportEvent(serviceBrakeTargetSupervision, __FILE__, __LINE__);
          atpIntervention = true;
        }
        else
        {
          atpWarning = true;
        }
        currentZone = ZoneC;
      }
      else if (currentSpeed >= firstWarningSpeed) /* Zone B entry condition  */
      {
        // If new target and the previous zone was noZone or ZoneA
        if (newTarget && ((ZoneA == currentZone) || (NoZone == currentZone)))
        {
          buzzer = (buzzer < BuzzerTypeOneBeep) ? BuzzerTypeOneBeep : buzzer;
          atpWarning = true;
          //Raising log events
          ATC::AbstractEventHandler::corePtr()->reportEvent(firstWarningSpeedExceed, __FILE__, __LINE__);
        }
        else if (speedExceedSBSpeed)
        {
          //Raising service brake till speed will be below first warning speed
          ATC::AbstractEventHandler::corePtr()->reportEvent(serviceBrakeTargetSupervision, __FILE__, __LINE__);
          atpIntervention = true;
        }
        else
        {
          atpWarning = true;
        }
        currentZone = ZoneB;
      }
      else if (currentSpeed < firstWarningSpeed) /* Zone A entry condition  */
      {
        currentZone = ZoneA;
        speedExceedSBSpeed = false;
        buzzer = BuzzerTypeNone;
      }
      else
      {
        speedExceedSBSpeed = false;
        buzzer = BuzzerTypeNone;
      }

      vfwVisitCheckPoint(&endCp, "SUP_superviseTargets_end");
    }

    /******************************************************************************
    * calcDistanceToBCA
    ******************************************************************************/
    void AbstractSupervise::calcDistanceToBCA(const int32_t currLeadPos, DS::SupervisedTargetIterator nextRestTargetIt)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_calcDistanceToBCA_begin");

      //if the iterator points to the end, set distance to bca as 0
      if (nextRestTargetIt == DS::AbstractTargets::corePtr()->getSupervisedTargetIterEnd())
      {
        distanceToBCA = 0;
      }
      else
      {
        //set the gradient as current gradient
        int32_t gradient = DS::AbstractTargets::corePtr()->getCurGradient();
        uint32_t ceilSpeed = DS::AbstractTargets::corePtr()->getCurrFwCeilingSpeed();
        uint32_t ebCeilingSpeed = DS::AbstractTargets::corePtr()->getCurrEbCeilingSpeed();

        const DS::SupervisedTarget* sTarget = *nextRestTargetIt;

        //if the next restrictive target is not the first target in list, find the previous target and get its gradient.
        if (nextRestTargetIt != DS::AbstractTargets::corePtr()->getSupervisedTargetIter())
        {
          DS::SupervisedTargetIterator prevTargetIt = --nextRestTargetIt;
          DS::SupervisedTarget* pTarget = *prevTargetIt;
          if (pTarget->getDirection() == sTarget->getDirection())
          {
            gradient = pTarget->getGradient();
            ceilSpeed = pTarget->getFwCeilingSpeed();
            ebCeilingSpeed = pTarget->getEbCeilingSpeed();
          }
        }

        //Adjust the ceilSpeed for atp mode and max train speed.
        ceilSpeed = AbstractTargetCalculation::corePtr()->calcModeDependentCeilingSpeed(ceilSpeed);

        /*
        * Worst brakeability will be used for the different curve calculation
        * Worst brakeability will be calculated between:
        * EB ceiling speed (max speed) at current position and FW speed ((minimum speed) at next target.
        */
        const int32_t brakeability = DS::AbstractTSetup::corePtr()->getWorstBrakeabilityInRange(sTarget->getFirstWarningSpeed(),ebCeilingSpeed);

        //calculate deceleration base on brakeability and gradient.

        const BrakeCalculations& brakeCalc = BrakeCalculations::instance();

        const int32_t deceleration = brakeCalc.calcDeceleration(brakeability, gradient);

        // Calculate the distance of first warning curve to target, if current speed is equal to ceiling speed
        int32_t abDist2Targ = brakeCalc.calcFirstWarningCurveDistance(ceilSpeed, gradient, deceleration, sTarget->getFirstWarningSpeed());
        int32_t distance2SB;
        
        if (sTarget->getDirection() == DirForward)
        {
          distance2SB = sTarget->getOdometer() - currLeadPos;
        }
        else
        {
          distance2SB = currLeadPos - sTarget->getOdometer();
        }

        distanceToBCA = distance2SB - abDist2Targ;

        if ((abDist2Targ < 0) || (distance2SB < 0) || (distanceToBCA < 0))
        {
          distanceToBCA = 0;
        }
      }

      vfwVisitCheckPoint(&endCp, "SUP_calcDistanceToBCA_end");
    }

    /******************************************************************************
    * calcTimeToIntervention
    ******************************************************************************/
    void AbstractSupervise::calcTimeToIntervention(DS::SupervisedTarget* const targetPtr, const uint32_t currSpeed, const int32_t currAcceleration,
      const int32_t startPointOdo, const TravelDir tDir, const int32_t gradient, const int32_t deceleration)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_calcTimeToIntervention_begin");

      //if the ATP has intervened, AOS reports 0. Brake Calculation will report max value.
      if (atpIntervention)
      {
        timeToIntervention = 0U;
      }
      else
      {
        int32_t relSpPos = AbstractTargetCalculation::corePtr()->getReleaseSpSBPos();
        int32_t targetOdo = targetPtr->getOdometer();
        uint32_t targetSp = targetPtr->getSBSpeed();
        uint32_t releaseSpeed = AbstractConfig::corePtr()->getReleaseSpeed();
        DS::BaseTarget::CoreTargetType targType = targetPtr->getParentTarget()->getTargetType();

        //if target position is ahead of the point where SB speed is below release speed
        //or if in release speed area for primary target
        //update target position to release speed SB position and target speed to 0.
        //and dont include SB margin in release speed.
        bool isTargPosAhead = (tDir == DirForward)?(targetOdo >= relSpPos):(targetOdo <= relSpPos);

        if((getInReleaseSpeedArea() && (targType == DS::BaseTarget::PrimaryTarget)) || (isTargPosAhead))
        {
          targetOdo = relSpPos;
          targetSp = 0U;
          targType = DS::BaseTarget::PrimaryTarget;
        }
        else
        {
          releaseSpeed += BrakeCalculations::instance().calcServiceBrakeLimitMargin(releaseSpeed);
        }

        timeToIntervention = BrakeCalculations::instance().calcTimeToIntervention(currSpeed, currAcceleration, startPointOdo,
            targetOdo, tDir, deceleration, targetSp, releaseSpeed, gradient, sbSpeed, targType);
      }

      vfwVisitCheckPoint(&endCp, "SUP_calcTimeToIntervention_end");
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractSupervise::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&initDone));
      // inReleaseSpeedArea       not cross-compared since reset every cycle
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<ZoneStatus>(&currentZone));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<SpeedLimitStatus>(&currentSpeedLimit));
      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<BuzzerType>(&buzzer));
      // atpWarning               not cross-compared since reset every cycle
      // atpIntervention          not cross-compared since reset every cycle
      // inBCA                    not cross-compared since reset every cycle
      // distanceToBCA            not cross-compared since reset every cycle
      crossCompare->addCrossCompareData(new Support::CrossCompareInt32(&distanceToTarget));
      crossCompare->addCrossCompareData(new Support::CrossCompareInt32(&mrtOdo));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&speedAtTarget));
      // timeToIntervention       not cross-compared since reset every cycle
      // predDistToStandStill     not cross-compared since reset every cycle
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&firstWarningSpeed));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&secondWarningSpeed));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&sbSpeed));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&ebSpeed));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&prevTargetListEmpty));
      // targetId                 not cross-compared since reset every cycle
      crossCompare->addCrossCompareData(new Support::CrossCompareInt32(&standstillOdoPos));
      crossCompare->addCrossCompareData(new Support::CrossCompareInt32(&rollAwayStartPos));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&triggerStandstillSup));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&resetOfStandStillRefPosition));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&resetOfReverseRefPosition));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&warningLimitEventReportedToTCC));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&speedExceedSBSpeed));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&reverseDistance));

      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorInTargetList));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&serviceBrakeCeilingSpeed));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&standstillCeilingSpeedZero));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&standstillNoTarget));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&serviceBrakeTargetSupervision));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&serviceBrakeStandstillSupervision));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&emergencyBrakeStandstillSupervision));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&serviceBrakeTIMSBroken));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&emergencyBrakeCeilingSpeed));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&emergencyBrakeTargetSupervision));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&firstWarningSpeedExceed));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&secondWarningSpeedExceed));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&speedWarningLimitExceed));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&ceilingSpeedLimitExceed));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&revSupvError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&revEBMarginError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&rollAwaySupvError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&rollAwayEBMarginError));  
   
      BrakeCalculations::instance().initCrossCompare();
    }


    /******************************************************************************
    * Data access functions
    ******************************************************************************/
    /******************************************************************************
    * getBuzzerRequest
    ******************************************************************************/
    BuzzerType AbstractSupervise::getBuzzerRequest(void) const
    {
      return buzzer;
    }

    /******************************************************************************
    * getAtpWarning
    ******************************************************************************/
    bool AbstractSupervise::getAtpWarning(void) const
    {
      return atpWarning;
    }

    /******************************************************************************
    * getAtpIntervention
    ******************************************************************************/
    bool AbstractSupervise::getAtpIntervention(void) const
    {
      return atpIntervention;
    }

    /******************************************************************************
    * getInBCA
    ******************************************************************************/
    bool AbstractSupervise::getInBCA(void) const
    {
      return inBCA;
    }

    /******************************************************************************
    * getInReleaseSpeedArea
    ******************************************************************************/
    bool AbstractSupervise::getInReleaseSpeedArea(void) const
    {
      return inReleaseSpeedArea;
    }

    /******************************************************************************
    * isEmAlertRequested
    ******************************************************************************/
    bool AbstractSupervise::isEmAlertRequested() const
    {
      return requestEmAlert;
    }

    /******************************************************************************
    * getDistanceToBCA
    ******************************************************************************/
    int32_t AbstractSupervise::getDistanceToBCA(void) const
    {
      return distanceToBCA;
    }

    /******************************************************************************
    * getDistanceToTarget
    ******************************************************************************/
    int32_t AbstractSupervise::getDistanceToTarget(void) const
    {
      return distanceToTarget;
    }

    /******************************************************************************
    * getSpeedAtTarget
    ******************************************************************************/
    uint32_t AbstractSupervise::getSpeedAtTarget(void) const
    {
      return speedAtTarget;
    }

    /******************************************************************************
    * getTimeToIntervention
    ******************************************************************************/
    uint32_t AbstractSupervise::getTimeToIntervention(void) const
    {
      return timeToIntervention;
    }

    /******************************************************************************
    * getPredDistToStandStill
    ******************************************************************************/
    int32_t AbstractSupervise::getPredDistToStandStill(void) const
    {
      return predDistToStandStill;
    }

    /******************************************************************************
    * getPermittedSpeed
    ******************************************************************************/
    uint32_t AbstractSupervise::getPermittedSpeed(void) const
    {
      return firstWarningSpeed;
    }

    /******************************************************************************
    * findRestrictiveTarget
    ******************************************************************************/
    void AbstractSupervise::findRestrictiveTarget(DS::SupervisedTargetIterator& mrt, DS::SupervisedTargetIterator& nextRestTarget) const
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "SUP_findRestrictiveTarget_begin");

      bool nextRestTargFound = false;
      bool mostRestTargFound = false;
      DS::SupervisedTarget* supvTarget = static_cast<DS::SupervisedTarget*>(NULL);
      TravelDir tDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
      const int32_t currLeadPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();

      DS::SupervisedTargetIterator it = DS::AbstractTargets::corePtr()->getSupervisedTargetIter();
      uint32_t currFWCeilingSpeed = DS::AbstractTargets::corePtr()->getCurrFwCeilingSpeed();

      while ((it != DS::AbstractTargets::corePtr()->getSupervisedTargetIterEnd()) && (!(nextRestTargFound  && mostRestTargFound)))
      {
        supvTarget = *it;
        const DS::SupervisedTarget::SupervisedTargetType superviseTargetType = supvTarget->getSupervisedTargetType();
        const uint32_t fwCeilingSpeed = supvTarget->getFwCeilingSpeed();
        const uint32_t ceilingSpeed = supvTarget->getCeilingSpeed();
        bool isTargetAhead = (supvTarget->getDirection() == DirForward)?(supvTarget->getOdometer() > currLeadPos):(supvTarget->getOdometer() < currLeadPos);

        // If the travel direction matches target direction and it is ahead of current position
        if ((tDir == supvTarget->getDirection()) && isTargetAhead)
        {
          //Restrictive target cannot be a speed increase target
          if ((superviseTargetType != DS::SupervisedTarget::SpeedIncreaseTarget))
          {
            // next restrictive target is the one with fw speed less than the fw ceiling speed.
            if ((supvTarget->getFirstWarningSpeed() < fwCeilingSpeed) && (!nextRestTargFound))
            {
              nextRestTarget = it;
              nextRestTargFound = true;
            }
            // target is restrictive when fw ceilingSpeed and fw permitted speed are the same and the fw ceiling speed is lower than the
            // current ceiling speed OR the ceiling speed is 0 (when the supervised targets end up at the same position as primary target),
            // which means that it is not restricted by any other target gradient target can never be MRT
            else if ((supvTarget->getFirstWarningSpeed() == supvTarget->getFwCeilingSpeed()) &&
                (superviseTargetType != DS::SupervisedTarget::GradientSupvTarget) &&
                ((fwCeilingSpeed < ceilingSpeed) || (ceilingSpeed == 0U)))
            {
              if (!nextRestTargFound)
              {
                nextRestTarget = it;
                nextRestTargFound = true;
              }
              mrt = it;
              mostRestTargFound = true;
            }
            else
            {
               // do nothing
            }
          }
          else
          {
            // next restrictive target is the speed increase if the FW CS speed is less than the previous FW CS.
            // This means another CS target ahead restricts this and the BCA should start from this target.
            if ((supvTarget->getFirstWarningSpeed() < currFWCeilingSpeed) && (!nextRestTargFound))
            {
              nextRestTarget = it;
              nextRestTargFound = true;
            }
          }
        }
        currFWCeilingSpeed = supvTarget->getFwCeilingSpeed();
        ++it;
      }

      vfwVisitCheckPoint(&endCp, "SUP_findRestrictiveTarget_end");
    }

    /******************************************************************************
    * revSupervision
    ******************************************************************************/
    void AbstractSupervise::revSupervision()
    {
      const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      // Get Direction of MA (the permitted one)
      const TravelDir maDirection = DS::AbstractTargets::corePtr()->getSupposedTravelDir();

      /** Measurement data from COD */
      const TravelDir odoDir = Pos::AbstractOdometry::corePtr()->getOdoDirection();

      const bool freeRolling = Kernel::AbstractModeControl::corePtr()->getFreeRolling();

      const bool isValidModeForReversSupv = freeRolling
        || (ATPModeNormal == currentMode)
        || (ATPModeStaffResponsible == currentMode)
        || (ATPModeShuntingRoute == currentMode)
        || (ATPModeSplit == currentMode)
        || (ATPModeJoin == currentMode)
        || (ATPModeBaliseSearch == currentMode);

      if (isValidModeForReversSupv)
      {
        const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

        // Reset reference position in case of brake event and coming to standstill.
        if (isStandStill && resetOfReverseRefPosition)
        {
          reverseDistance = 0U;
          resetOfReverseRefPosition = false;
        }

        // Reset reference position in case of MA from scratch.
        if (Kernel::AbstractMessageHandler::corePtr()->isMAFromScratch())
        {
          reverseDistance = 0U;
        }

        if (((maDirection == DirForward)  &&  (odoDir == DirReverse)) ||
            ((maDirection == DirReverse)  &&  (odoDir == DirForward)))
        {
          uint32_t maxReverseDistance = 0U;
          if (freeRolling)
          {
            //Fetch Max Reversing Supervision Margin for Free Rolling
            maxReverseDistance = AbstractConfig::corePtr()->getRevSupMargForFreeRolling();
          }
          else if (ATPModeShuntingRoute == currentMode)
          {
            //Fetch Max Reversing Supervision Margin for Shunting Route
            maxReverseDistance = AbstractConfig::corePtr()->getRevSupMargForShuntRoute();
          }
          else
          {
            //Fetch Max Reversing Supervision Margin for Normal, Staff Responsible, Split, Join and Balise Search
            maxReverseDistance = AbstractConfig::corePtr()->getRevSupMargin();
          }

          const int32_t dNom = Pos::AbstractOdometry::corePtr()->getOdoPositionWithoutOffset();
          const int32_t dNomOld = Pos::AbstractOdometry::corePtr()->getOldOdoPositionWithoutOffset();

          // Calculate the reverse distance
          reverseDistance = reverseDistance + static_cast<uint32_t>(ATC::ATCMath::instance().absolute(dNom - dNomOld, __FILE__, __LINE__));
         
          // Fetch maximum reverse emergency break percentage
          uint32_t maxReverseEBMargin = AbstractConfig::corePtr()->getEBMarginAdded();
          
          // Calculate the maximum reverse emergency break distance from EB margin percentage applied on max reverse distance
          const uint32_t maxReverseEBDistance = ((maxReverseEBMargin * maxReverseDistance) / 100U) + maxReverseDistance;
          if (reverseDistance > maxReverseEBDistance)
          {
            //Raising emergency break if train is moving reverse and exceeds EB margin
            ATC::AbstractEventHandler::corePtr()->reportEvent(revEBMarginError, __FILE__, __LINE__); 

            resetOfReverseRefPosition = true;
          }
          else if (reverseDistance > maxReverseDistance)
          {
            //Raising service brake if train is moving reverse and exceeds reverse supervision distance
            ATC::AbstractEventHandler::corePtr()->reportEvent(revSupvError, __FILE__, __LINE__);

            resetOfReverseRefPosition = true;
          }
          else
          {
            //no break intervention required
          }
        }
        else if (((maDirection == DirForward) && (odoDir == DirForward)) ||
                 ((maDirection == DirReverse) && (odoDir == DirReverse)))
        {
          // The AOS shall set the reference position for Reversing supervision to current position 
          // at standstill after movement in the direction of the MA. 
          resetOfReverseRefPosition = true;
        }
        else
        {
          // Do nothing
        }
      }
      else
      {
        reverseDistance = 0U;
      }
    }

    /******************************************************************************
    * rollAwaySupervision
    ******************************************************************************/
    void AbstractSupervise::rollAwaySupervision()
    {
      const TravelDir drivingDirection = Kernel::AbstractModeControl::corePtr()->getCurrentDrivingDirection();

      const bool isSleeping = Kernel::AbstractModeControl::corePtr()->getCurrentMode() == ATPModeSleeping;
      const bool isFreeRolling = Kernel::AbstractModeControl::corePtr()->getFreeRolling();
      const bool isStandstill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
      const bool isDirUndefined = (drivingDirection == DirUndefined);
      const bool isSupervisionActive = (!isSleeping) && (!isFreeRolling) && (!isStandstill) && (!isDirUndefined);

      if (isSupervisionActive)
      {
        TravelDir movementDirection;

        // Calculate the movement distance and direction
        OdoPosition currentOdoPos = Pos::AbstractOdometry::corePtr()->getOdoPositionWithoutOffset();
        OdoPosition movementDistance = currentOdoPos - rollAwayStartPos;

        if (movementDistance > 0)
        {
          movementDirection = DirForward;
        }
        else if (movementDistance < 0)
        {
          movementDirection = DirReverse;
        }
        else
        {
          movementDirection = DirNone;
        }

        if (movementDirection != drivingDirection)
        {
          OdoPosition rollAwayDistance = ATC::ATCMath::instance().absolute(movementDistance, __FILE__, __LINE__);

          // Calculate the intervention distances
          uint32_t maxDistanceUntilSB = AbstractConfig::corePtr()->getRollAwayMargin();
          uint32_t maxDistanceUntilEB = ((AbstractConfig::corePtr()->getEBMarginAdded() * maxDistanceUntilSB) / 100U) + maxDistanceUntilSB;

          if (rollAwayDistance > static_cast<OdoPosition>(maxDistanceUntilEB))
          {
            // Raise emergency brake
            ATC::AbstractEventHandler::corePtr()->reportEvent(rollAwayEBMarginError, __FILE__, __LINE__);
          }
          else if (rollAwayDistance > static_cast<OdoPosition>(maxDistanceUntilSB))
          {
            // Raise service brake
            ATC::AbstractEventHandler::corePtr()->reportEvent(rollAwaySupvError, __FILE__, __LINE__);
          }
          else
          {
            // No brake intervention required
          }
        }
        else
        {
          // Update rollAwayStartPos but only move it in the selected driving direction
          if ( ((drivingDirection == DirForward) && (currentOdoPos > rollAwayStartPos))
            || ((drivingDirection == DirReverse) && (currentOdoPos < rollAwayStartPos)) )
          {
            rollAwayStartPos = currentOdoPos;
          }
        }
      }
      else
      {
        // Update rollAwayStartPos until supervision starts
        rollAwayStartPos = Pos::AbstractOdometry::corePtr()->getOdoPositionWithoutOffset();
      }
    }

    /******************************************************************************
    * locationSupervision
    ******************************************************************************/
    void AbstractSupervise::locationSupervision() const
    {
      const DS::BaseTarget* const startTarget = DS::AbstractTargets::corePtr()->getLocationStartTarget();
      const DS::BaseTarget* const endTarget = DS::AbstractTargets::corePtr()->getLocationEndTarget();

      if ((startTarget != static_cast<const DS::BaseTarget*>(NULL)) && (endTarget != static_cast<const DS::BaseTarget*>(NULL)))
      {
        const OdoPosition rearPosition = TG::AbstractTIMS::corePtr()->getRearPositionOdo();
        OdoPosition largerLocationBorder;
        OdoPosition smallerLocationBorder;

        if (endTarget->getOdometer() > startTarget->getOdometer())
        {
          largerLocationBorder = endTarget->getOdometer();
          smallerLocationBorder = startTarget->getOdometer();
        }
        else
        {
          largerLocationBorder = startTarget->getOdometer();
          smallerLocationBorder = endTarget->getOdometer();
        }

        if ((rearPosition < smallerLocationBorder) || (rearPosition > largerLocationBorder))
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(locationSupvError, __FILE__, __LINE__);
        }
      }
    }
  }
}

//lint +esym(586,snprintf)
