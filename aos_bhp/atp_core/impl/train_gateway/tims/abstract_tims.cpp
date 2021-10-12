/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the AbstractTIMS class
* which contains the core functionality of the TIMS.
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-29    akushwah    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include <stdint.h>
#include <vfw_checkpoints.h>
#include "abstract_tims.hpp"
#include "abstract_tims_event_ids.hpp"
#include "atc_util.hpp"
#include "atc_math.hpp"
#include "abstract_config.hpp"
#include "abstract_console.hpp"
#include "abstract_tracks.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_targets.hpp"
#include "abstract_mode_control.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_message_handler.hpp"
#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime();
#else
#include <vfw_time.h>
#endif

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
    AbstractTIMS::AbstractTIMS() : ATC::ProcComponent(atpTimsId, "TIMS", "TM"),
      timsBrokenEvent(ATC::Event::createLogEvent(
        atpTimsId, ATC::CoreContainer, eventIdIntegrityBroken, 0x0U, "Train Integrity Status is now Broken.")),
      timsIntactEvent(ATC::Event::createLogEvent(
        atpTimsId, ATC::CoreContainer, eventIdIntegrityIntact, 0x0U, "Train Integrity Status is now Intact.")),
      timsAvailable(false),
      timsConfirmed(false),
      timsStatus(TIMSStatusNotAvailable),
      timsSupervision(TIMSNotSupervised),
      initDone(false),
      inhibitButtonPressed(false),
      resumeButtonPressed(false),
      isRouteTypeJoin(false),
      manualConfirmationStartTime(0),
      manualConfirmationReceived(false)
    {
      if (coreTIMSInstancePtr != 0)
      {
        ATC::aosHalt(__FILE__, __LINE__, "TIMS already instantiated");
      }

      // Setup single instance pointer for core access
      coreTIMSInstancePtr = this;

      positionReport.posKnown = false;
      positionReport.frontPos = 0;
      positionReport.rearPos = 0;
      lastCarPosition.odoValue = 0;
      lastCarPosition.valid = false;
      lastCarBaliseWindow = 0U;
      lastMovementDir = DirUndefined;

      clearPositionHistory();
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractTIMS::init()
    {
      if (!initDone)
      {
        initCrossCompare();

        timsAvailable = false;
        timsConfirmed = false;
        prevTIMSStatus = TIMSStatusNotAvailable;
        timsStatus = TIMSStatusNotAvailable;
        timsSupervision = TIMSNotSupervised;
        inhibitButtonPressed = false;
        resumeButtonPressed = false;
        isRouteTypeJoin = false;
        manualConfirmationStartTime = 0;
        manualConfirmationReceived = false;
        lastCarBaliseWindow = Pos::AbstractOdometry::corePtr()->getLastCarBaliseWindow();

        initDone = true;
      }

      return initDone;
    }

    /******************************************************************************
    * runIn
    ******************************************************************************/
    void AbstractTIMS::runIn()
    {
      // Placeholder for adaptation
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void AbstractTIMS::run()
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "AbstractTIMS_run");

      handleDmiActions();

      if (ATP::Kernel::AbstractModeControl::corePtr()->getCurrentMode() == ATPModeConfiguration)
      {
        clearPositionHistory();
      }
      else
      {
        updatePositionHistory();
      }

      updateTimsAvailable();
      updateTimsSupervision();
      updateTimsStatus();

      updateOrClearLastCarPosition();

      // Log event when train integrity changes to broken or intact
      if (timsStatus != prevTIMSStatus)
      {
        if (timsStatus == TIMSStatusBroken)
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(timsBrokenEvent, __FILE__, __LINE__);
        }
        else if (timsStatus == TIMSStatusIntact)
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(timsIntactEvent, __FILE__, __LINE__);
        }
        else
        {
          // Do nothing
        }

        // Save the TIMS integrity status for next cycle
        prevTIMSStatus = timsStatus;
      }

      inhibitButtonPressed = false;
      resumeButtonPressed = false;
      manualConfirmationReceived = false;
      if (timsSupervision != TIMSInhibited)
      {
        manualConfirmationStartTime = 0;
      }
    }

    /******************************************************************************
    * runOut
    ******************************************************************************/
    void AbstractTIMS::runOut()
    {
      // Placeholder for adaptation
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractTIMS* AbstractTIMS::corePtr()
    {
      return coreTIMSInstancePtr;
    }

    /******************************************************************************
    * getTimsAvailable
    ******************************************************************************/
    bool AbstractTIMS::getTimsAvailable() const
    {
      return timsAvailable;
    }

    /******************************************************************************
    * getTimsConfirmed
    ******************************************************************************/
    bool AbstractTIMS::getTimsConfirmed() const
    {
      return timsConfirmed;
    }

    /******************************************************************************
    * getTimsStatus
    ******************************************************************************/
    TIMSStatus AbstractTIMS::getTimsStatus() const
    {
      return timsStatus;
    }

    /******************************************************************************
    * getTimsSupervision
    ******************************************************************************/
    TIMSSupervision AbstractTIMS::getTimsSupervision() const
    {
      return timsSupervision;
    }

    /******************************************************************************
    * isManualConfirmationNeeded
    ******************************************************************************/
    bool AbstractTIMS::isManualConfirmationNeeded()
    {
      return manualConfirmationStartTime != 0;
    }

    /******************************************************************************
    * setPositionReport
    ******************************************************************************/
    void AbstractTIMS::setPositionReport(
      const Pos::PosAccuracyState posAccuracy,
      const TrackAndPos frontTrackAndPos,
      const TrackAndPos rearTrackAndPos)
    {
      bool ok1 = false;
      bool ok2 = false;

      if (posAccuracy == Pos::PosKnown)
      {
        ok1 = DS::AbstractTracks::corePtr()->getOdoPos(frontTrackAndPos, positionReport.frontPos);
        ok2 = DS::AbstractTracks::corePtr()->getOdoPos(rearTrackAndPos, positionReport.rearPos);
      }

      positionReport.posKnown = (ok1 && ok2);

      if (positionReport.posKnown)
      {
        char_t buf[100];

        int32_t ret = snprintf(&buf[0], sizeof(buf), "Position report updated: front track %u, pos %u",
          frontTrackAndPos.track, frontTrackAndPos.position);

        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buf)))
        {
          trace.write(ATC::detailedTrace, &buf[0]);
        }

        ret = snprintf(&buf[0], sizeof(buf), "Position report updated: rear  track %u, pos %u",
          rearTrackAndPos.track, rearTrackAndPos.position);

        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buf)))
        {
          trace.write(ATC::detailedTrace, &buf[0]);
        }
      }
    }

    /******************************************************************************
    * isRearPositionValid
    ******************************************************************************/
    bool AbstractTIMS::isRearPositionValid() const
    {
      bool rearPositionValid = false;

      const TravelDir supposedDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
      if (supposedDir == DirReverse)
      {
        rearPositionValid = true;
      }
      else
      {
        rearPositionValid = lastCarPosition.valid;
      }

      return rearPositionValid;
    }

    /******************************************************************************
    * getRearPosition
    ******************************************************************************/
    TrackAndPos AbstractTIMS::getRearPosition() const
    {
      TrackAndPos rearPosition = { 0U, 0U };

      if (isRearPositionValid())
      {
        rearPosition = DS::AbstractTracks::corePtr()->calculateTrackAndPos(getRearPositionOdo());
      }

      return rearPosition;
    }

    /******************************************************************************
    * getRearPositionOdo
    ******************************************************************************/
    OdoPosition AbstractTIMS::getRearPositionOdo() const
    {
      OdoPosition odoPosition = 0;

      const TravelDir supposedDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
      if (supposedDir == DirReverse)
      {
        odoPosition = Pos::AbstractPosition::corePtr()->getLocoEndPosOdo();
      }
      else
      {
        odoPosition = lastCarPosition.odoValue;
      }

      return odoPosition;
    }

    /******************************************************************************
    * getSafePositionToDeleteTrack
    ******************************************************************************/
    OdoPosition AbstractTIMS::getSafePositionToDeleteTrack() const
    {
      const OdoPosition frontPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();
      const OdoPosition rearPos = getRearPositionOdo();

      // Fetch delete track mrgin
      const OdoPosition distDtrackMargin = static_cast<OdoPosition>(AbstractConfig::corePtr()->getDeleteTrackMargin());

      // Calculate the margin
      const OdoPosition rearMargin = static_cast<OdoPosition>(DS::AbstractTargets::corePtr()->getSafetyMargin()) + distDtrackMargin;

      OdoPosition safePositionToDeleteTrack = 0;

      // Add/Subtract the margin
      if (frontPos > rearPos)
      {
        safePositionToDeleteTrack = rearPos - rearMargin;
      }
      else
      {
        safePositionToDeleteTrack = rearPos + rearMargin;
      }

      return safePositionToDeleteTrack;
    }

    /******************************************************************************
    * getFrontBaliseWindow
    ******************************************************************************/
    uint32_t AbstractTIMS::getFrontBaliseWindow() const
    {
      uint32_t frontBaliseWindow;

      const TravelDir supposedDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
      if (supposedDir != DirReverse)
      {
        frontBaliseWindow = Pos::AbstractOdometry::corePtr()->getLocoEndBaliseWindow();
      }
      else
      {
        frontBaliseWindow = lastCarBaliseWindow;
      }

      return frontBaliseWindow;
    }

    /******************************************************************************
    * getRearBaliseWindow
    ******************************************************************************/
    uint32_t AbstractTIMS::getRearBaliseWindow() const
    {
      uint32_t rearBaliseWindow;

      const TravelDir supposedDir = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
      if (supposedDir != DirReverse)
      {
        rearBaliseWindow = lastCarBaliseWindow;
      }
      else
      {
        rearBaliseWindow = Pos::AbstractOdometry::corePtr()->getLocoEndBaliseWindow();
      }

      return rearBaliseWindow;
    }

    /******************************************************************************
    * updateTimsSupervision
    ******************************************************************************/
    void AbstractTIMS::updateTimsSupervision()
    {
      bool isModeOKForSupervised = false;

      const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();
      bool timsSupNeeded = false;

      if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
      {
        timsSupNeeded = pTrainSetup->timsSupNeeded;
      }

      const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      if(currentMode !=ATPModeJoin)
      {
        //flag has to set to false, otherwise It remains set true and when mode will be set to Join again using Join command.
        //However, isModeOKForSupervised should be set to true, when Join MA is received only.
        isRouteTypeJoin = false;
      }

      MAHead maHead;
      if (Kernel::AbstractMessageHandler::corePtr()->getMAHead(maHead))
      {
       // when Join MA is received.
        if (RtJoin == maHead.routeType)
        {
          isRouteTypeJoin = true;
        }
      }

      const bool atpModeJoinWithMA = isRouteTypeJoin && (currentMode == ATPModeJoin);

      if ((currentMode == ATPModeNormal) || (currentMode == ATPModeStaffResponsible) ||
        (currentMode == ATPModeBaliseSearch) || (currentMode == ATPModeLocation) || (atpModeJoinWithMA))
      {
        isModeOKForSupervised = true;
      }

      const bool supervisedPossible = timsAvailable && timsSupNeeded && isModeOKForSupervised;

      // State transitions

      if (timsSupervision == TIMSNotSupervised)
      {
        if (supervisedPossible)
        {
          timsSupervision = TIMSSupervised;
        }
      }
      else // TIMSSupervised or TIMSInhibited
      {
        if (!supervisedPossible)
        {
          timsSupervision = TIMSNotSupervised;
        }
      }

      if (timsSupervision == TIMSSupervised)
      {
        if (inhibitButtonPressed)
        {
          timsSupervision = TIMSInhibited;
        }
      }
      else if (timsSupervision == TIMSInhibited)
      {
        if (resumeButtonPressed && timsConfirmed)
        {
          timsSupervision = TIMSSupervised;
        }
      }
      else
      {
        //Nothing
      }
    }

    /******************************************************************************
    * updateTimsAvailable
    ******************************************************************************/
    void AbstractTIMS::updateTimsAvailable()
    {
      // Treat TIMS equipment as NOT available if not overruled by the adaptation. Thus, return TIMS available as false;
      timsAvailable = false;
    }

    /******************************************************************************
    * updateTimsStatus
    ******************************************************************************/
    void AbstractTIMS::updateTimsStatus()
    {
      if (timsSupervision == TIMSNotSupervised) // also covers TIMS not available
      {
        setTimsStatus(TIMSStatusNotAvailable);
      }
      else if (timsSupervision == TIMSSupervised)
      {
        if (getTimsConfirmed())
        {
          setTimsStatus(TIMSStatusIntact);
        }
      }
      else
      {
        // Do nothing
      }
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool AbstractTIMS::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments searches for the "help", "trace" or any other Console
      component specific command calls and handles it. Returns true if completely handled
      else returns false. returning false will let other components handle the call. help always returns false.
      */

      bool retVal = false;

      // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        const char_t* const toWrite = "tims          To print TIMS status information";
        ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);
        retVal = false;
      }
      else if (ATC::isTextMatch(&argv[0][0], "tims", sizeof("tims")) && (argc == 1U))
      {
        char_t  buffer[512];

        memset(&buffer[0], 0, sizeof(buffer));

        int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%-16s%-18s%-16s%-16s",
          "TIMS Available", "TIMS Supervision", "TIMS Confirmed", "TIMS Status");

        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        const char_t* const buffTIMSAvailable = timsAvailable ? "True" : "False";
        const char_t* const buffTIMSConfirmed = timsConfirmed ? "True" : "False";
        const char_t* buffTIMSSupervision;
        const char_t* buffTIMSStatus;

        switch (timsSupervision)
        {
        case TIMSNotSupervised:
          buffTIMSSupervision = "NotSupervised";
          break;

        case TIMSSupervised:
          buffTIMSSupervision = "Supervised";
          break;

        case TIMSInhibited:
          buffTIMSSupervision = "Inhibited";
          break;

        default:
          buffTIMSSupervision = "Unknown";
          break;
        }

        switch (timsStatus)
        {
        case TIMSStatusNotAvailable:
          buffTIMSStatus = "NotAvailable";
          break;

        case TIMSStatusIntact:
          buffTIMSStatus = "Intact";
          break;

        case TIMSStatusBroken:
          buffTIMSStatus = "Broken";
          break;

        default:
          buffTIMSStatus = "Unknown";
          break;
        }

        ret = snprintf(&buffer[0], sizeof(buffer), "%-16s%-18s%-16s%-16s",
          buffTIMSAvailable, buffTIMSSupervision, buffTIMSConfirmed, buffTIMSStatus);
        
        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        retVal = true;
      }
      else
      {
        //Nothing
      }

      return retVal;
    }

    /******************************************************************************
    * setTimsAvailable
    ******************************************************************************/
    void AbstractTIMS::setTimsAvailable(const bool isTimsAvailable)
    {
      timsAvailable = isTimsAvailable;
    }

    /******************************************************************************
    * setTimsConfirmed
    ******************************************************************************/
    void AbstractTIMS::setTimsConfirmed(const bool isTimsConfirmed)
    {
      timsConfirmed = isTimsConfirmed;
    }

    /******************************************************************************
    * setTimsStatus
    ******************************************************************************/
    void AbstractTIMS::setTimsStatus(const TIMSStatus status)
    {
      timsStatus = status;

      if (status == TIMSStatusBroken)
      {
        timsConfirmed = false;
      }
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractTIMS::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&timsBrokenEvent));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&timsIntactEvent));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&timsAvailable));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&timsConfirmed));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TIMSStatus>(&timsStatus));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TIMSSupervision>(&timsSupervision));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&initDone));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&inhibitButtonPressed));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&resumeButtonPressed));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&isRouteTypeJoin));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&manualConfirmationStartTime));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&manualConfirmationReceived));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&lastCarPosition.valid));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&lastCarPosition.odoValue));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&lastCarBaliseWindow));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TravelDir>(&lastMovementDir));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&positionReport.posKnown));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&positionReport.frontPos));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&positionReport.rearPos));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TIMSStatus>(&prevTIMSStatus));
    }

    /******************************************************************************
    * getAutomatedReportTime
    ******************************************************************************/
    uint64_t AbstractTIMS::getAutomatedReportTime()
    {
      return 0U; // placeholder for adaptation
    }

    /******************************************************************************
    * updateOrClearLastCarPosition
    ******************************************************************************/
    void AbstractTIMS::updateOrClearLastCarPosition()
    {
      ATPMode atpMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      switch (atpMode)
      {
      case ATPModeConfiguration:
      case ATPModeRegistration:
      case ATPModeBaliseSearch:
      case ATPModeSplit:
      case ATPModeStaffResponsible:
      case ATPModeNormal:
      case ATPModeLocation:
      case ATPModeShuntingRoute:
      case ATPModeJoin:
      case ATPModeSafeBrakeToStop:
        updateLastCarPosition();
        break;

      // Position is Unknown in all these modes so reset the last car position
      case ATPModePoweringDown:
      case ATPModePowerUp:
      case ATPModeSleeping:
      case ATPModeUnregistered:
      case ATPModeSafetyHalt:
      case ATPModeYard:
      case ATPModeShunting:
      case ATPModePossession:
        lastCarPosition.odoValue = 0;
        lastCarPosition.valid = false;
        break;

      case ATPModeUndefined:
      case ATPModesCount:
      default:
        ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        break;
      }
    }

    /******************************************************************************
    * updateLastCarPosition
    ******************************************************************************/
    void AbstractTIMS::updateLastCarPosition()
    {
      bool updateIsHandled = false;
      const TravelDir travelDir = Pos::AbstractOdometry::corePtr()->getOdoDirection();

      if ((travelDir == DirForward) || (travelDir == DirReverse))
      {
        lastMovementDir = travelDir;
      }

      if (lastMovementDir != DirReverse)
      {
        if (timsSupervision == TIMSSupervised)
        {
          if (timsConfirmed)
          {
            uint64_t automatedReportTime = getAutomatedReportTime();
            if (automatedReportTime != 0U)
            {
              updateLastCarPositionSupervised(automatedReportTime);
            }
          }
          updateIsHandled = true;
        }
        else if (timsSupervision == TIMSInhibited)
        {
          if (manualConfirmationReceived)
          {
            updateLastCarPositionManual();
          }
          updateIsHandled = true;
        }
        else // TIMSNotSupervised
        {
          // Use "lastCarPosition = locoPosition - trainLength" (see below)
        }
      }

      if (!updateIsHandled)
      {
        if (Pos::AbstractPosition::corePtr()->getAccuracyState() != Pos::PosUnknown)
        {
          const DS::TrainSetup* trainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();
          if (trainSetup != NULL)
          {
            const OdoPosition currentLastCarPosition =
              Pos::AbstractPosition::corePtr()->getLocoEndPosOdo() - static_cast<OdoPosition>(trainSetup->length);

            bool hasLastCarPassedLastReportedPos = false;
            if ((travelDir == DirReverse) && positionReport.posKnown)
            {
              // The last car position is the smaller of 'front' and 'rear'
              if ((currentLastCarPosition < positionReport.frontPos) && (currentLastCarPosition < positionReport.rearPos))
              {
                hasLastCarPassedLastReportedPos = true;
              }
            }

            bool useCurrentLastCarPosition;
            if (timsSupervision == TIMSNotSupervised)
            {
              useCurrentLastCarPosition = true;
            }
            else if (hasLastCarPassedLastReportedPos)
            {
              useCurrentLastCarPosition = true;
            }
            else
            {
              useCurrentLastCarPosition = false;
            }

            if (useCurrentLastCarPosition)
            {
              lastCarPosition.odoValue = currentLastCarPosition;
              lastCarPosition.valid = true;
              lastCarBaliseWindow = Pos::AbstractOdometry::corePtr()->getLastCarBaliseWindow();

              trace.write(ATC::detailedTrace, "Last car position (now): ", lastCarPosition.odoValue);
              trace.write(ATC::detailedTrace, "Last car balise window:  ", lastCarBaliseWindow);
            }
          }
        }
      }

      MAHead maHead;
      if (Kernel::AbstractMessageHandler::corePtr()->getMAHead(maHead))
      {
        if (Kernel::AbstractMessageHandler::corePtr()->isMAFromScratch())
        {
          lastCarBaliseWindow = Pos::AbstractOdometry::corePtr()->getLastCarBaliseWindow();

          if (!lastCarPosition.valid)
          {
            const TravelDir maDir = Kernel::dirAndOrientation2TravelDir(maHead.trainDirection);

            if (maDir == DirForward)
            {
              lastCarPosition.valid = DS::AbstractTracks::corePtr()->getOdoPos(
                maHead.startOfMATrackAndPos, lastCarPosition.odoValue);
            }
          }
        }
      }
    }

    /******************************************************************************
    * updateLastCarPositionSupervised
    ******************************************************************************/
    void AbstractTIMS::updateLastCarPositionSupervised(const uint64_t tSensor)
    {
      const DS::TrainSetup* trainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();
      if (trainSetup != NULL)
      {
        uint32_t trainLength = trainSetup->length;

        // Times in milliseconds
        uint64_t tClock = 1000U * static_cast<uint64_t>(AbstractConfig::corePtr()->getTimsSensorTimeDiff());
        uint64_t tPropagation = ATC::ATCMath::instance().unsignDivRoundUp(1000U * trainLength,
          AbstractConfig::corePtr()->getEbPrPropagationSpeed(), __FILE__, __LINE__);
        uint64_t tConfirmed = (1000U * tSensor) - (tClock + tPropagation);

        PositionSample sample;
        if (getLastCarPositionAtTime(tConfirmed, sample))
        {
          if (sample.valid)
          {
            lastCarPosition.odoValue = sample.lastCarPosition;
            lastCarPosition.valid = true;
            lastCarBaliseWindow = sample.lastCarBaliseWindow;

            trace.write(ATC::detailedTrace, "Last car position (tims): ", lastCarPosition.odoValue);
            trace.write(ATC::detailedTrace, "Last car balise window:   ", lastCarBaliseWindow);
          }
        }
      }
    }

    /******************************************************************************
    * updateLastCarPositionManual
    ******************************************************************************/
    void AbstractTIMS::updateLastCarPositionManual()
    {
      const DS::TrainSetup* trainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();
      if (trainSetup != NULL)
      {
        // Times in milliseconds
        uint64_t millisNow = ATC::getUTCTimeInMs();
        uint64_t tDriv = 1000U * static_cast<uint64_t>(AbstractConfig::corePtr()->getTimsManualConfTime());
        uint64_t tDelay = 1000U;
        uint64_t tConfirmed = millisNow - (tDriv + tDelay);

        PositionSample sample;
        if (getLastCarPositionAtTime(tConfirmed, sample))
        {
          if (sample.valid)
          {
            lastCarPosition.odoValue = sample.lastCarPosition;
            lastCarPosition.valid = true;
            lastCarBaliseWindow = sample.lastCarBaliseWindow;

            trace.write(ATC::detailedTrace, "Last car position (manual): ", lastCarPosition.odoValue);
            trace.write(ATC::detailedTrace, "Last car balise window:     ", lastCarBaliseWindow);
          }
        }
      }
    }

    /******************************************************************************
    * handleDmiActions
    ******************************************************************************/
    void AbstractTIMS::handleDmiActions()
    {
      // Cancel the confirmation handshake if it times out
      const int64_t timeNow = vfwGetReferenceTime();
      if ((timeNow - manualConfirmationStartTime) > maxConfirmationTime)
      {
        manualConfirmationStartTime = 0;
      }

      switch (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus())
      {
      case DMICom::DMIButtonTIMSInhibit:
        inhibitButtonPressed = true;
        break;
      case DMICom::DMIButtonTIMSResume:
        resumeButtonPressed = true;
        break;
      case DMICom::DMIButtonManualTrainIntegrity:
        manualConfirmationStartTime = vfwGetReferenceTime();
        // This will cause the DMI to ask for confirmation
        break;
      case DMICom::DMIButtonConfirmManualTrainIntegrity:
        if (manualConfirmationStartTime != 0)
        {
          manualConfirmationStartTime = 0;
          manualConfirmationReceived = true;
        }
        break;
      case DMICom::DMIButtonCancelManualTrainIntegrity:
        manualConfirmationStartTime = 0;
        break;
      // To please lint:
      case DMICom::DMIButtonUndefined:
      case DMICom::DMIButtonBrakeRelease:
      case DMICom::DMIButtonTrainConfig:
      case DMICom::DMIButtonSpare5:
      case DMICom::DMIButtonHandlingDone:
      case DMICom::DMIButtonEnterYardMode:
      case DMICom::DMIButtonRetryConfig:
      case DMICom::DMIButtonAbortSetup:
      case DMICom::DMIButtonLogoutDriver:
      case DMICom::DMIButtonEnterPossessionMode:
      case DMICom::DMIButtonShuntingMode:
      case DMICom::DMIButtonSpare16:
      case DMICom::DMIButtonStartBrakeTest:
      case DMICom::DMIButtonAbortBrakeTest:
      case DMICom::DMIButtonStartBtmTest:
      case DMICom::DMIButtonAbortLastCarBrakeTest:
      case DMICom::DMIButtonConfirmYard:
      case DMICom::DMIButtonConfirmShuntingRoute:
      case DMICom::DMIButtonConfirmStaffResponsible:
      case DMICom::DMIButtonConfirmJoin:
      case DMICom::DMIButtonConfirmSleep:
      case DMICom::DMIButtonConfirmSplit:
      case DMICom::DMIButtonConfirmDeparture:
      case DMICom::DMIButtonAcceptAutomaticConfig:
      case DMICom::DMIButtonRequestFreeRolling:
      case DMICom::DMIButtonConfirmFreeRollingCleared:
      case DMICom::DMIButtonConfirmStaffResponsibleMA:
      case DMICom::DMIButtonConfirmShuntingRouteMA:
      case DMICom::DMIButtonConfirmJoinMA:
      case DMICom::DMIButtonCancelRegistrationArea:
      case DMICom::DMIButtonConfirmChangeOfTrainLoaded:
      case DMICom::DMIButtonCancelChangeOfTrainLoaded:
      case DMICom::DMIButtonConfirmAbortLastCarBrakeTest:
      case DMICom::DMIButtonCancelAbortLastCarBrakeTest:
      case DMICom::DMIButtonConfirmTachometer1Failure:
      case DMICom::DMIButtonConfirmTachometer2Failure:
      case DMICom::DMIButtonConfirmDopplerFailure:
      case DMICom::DMIButtonMaxCount:
        // Do nothing
        break;
      }
    }

    /******************************************************************************
    * clearPositionHistory
    ******************************************************************************/
    void AbstractTIMS::clearPositionHistory()
    {
      latestPositionIndex = 0U;
      historySize = 0U;
      memset(&positionHistory[0], 0, sizeof(positionHistory));
    }

    /******************************************************************************
    * updatePositionHistory
    ******************************************************************************/
    void AbstractTIMS::updatePositionHistory()
    {
      const uint64_t millisNow = ATC::getUTCTimeInMs();

      // Only update according to new odo-offset if an MA from scratch has caused a re-calculation of odo-values.
      if (Kernel::AbstractMessageHandler::corePtr()->isMAFromScratch())
      {
        const OdoPosition odoOffsetCorrection = Pos::AbstractPosition::corePtr()->getOdometerOffsetCorrection();

        if (0 != odoOffsetCorrection)
        {
          for (uint16_t i = 0U; i < maxHistorySize; i++)
          {
            positionHistory[i].lastCarPosition += odoOffsetCorrection;
          }

          lastCarPosition.odoValue += odoOffsetCorrection;
        }
      }

      // Store one sample per second (allowing for jitter in current time)
      if ((historySize == 0U) || ((millisNow - positionHistory[latestPositionIndex].millis) >= 950U))
      {
        // Find the oldest sample (the one to be replaced)
        if (historySize > 0U)
        {
          ++latestPositionIndex;
        }
        if (latestPositionIndex == maxHistorySize)
        {
          latestPositionIndex = 0U;
        }
        if (historySize < maxHistorySize)
        {
          ++historySize;
        }

        // Evaluate the conditions for this sample to be valid
        const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();
        const Pos::PosAccuracyState accuracyState = Pos::AbstractPosition::corePtr()->getAccuracyState();
        const bool isSlipping = Pos::AbstractOdometry::corePtr()->isSlipping();
        const bool isSliding = Pos::AbstractOdometry::corePtr()->isSliding();

        // Fill in the sample
        PositionSample& sample = positionHistory[latestPositionIndex];
        sample.millis = millisNow;

        if (isSlipping || isSliding || (accuracyState != Pos::PosKnown) || (pTrainSetup == NULL))
        {
          sample.valid = false;
        }
        else
        {
          sample.valid = true;
          sample.lastCarPosition = Pos::AbstractPosition::corePtr()->getLocoEndPosOdo() - static_cast<OdoPosition>(pTrainSetup->length);
          sample.lastCarBaliseWindow = Pos::AbstractOdometry::corePtr()->getLastCarBaliseWindow();
        }
      }
    }

    /******************************************************************************
    * getLocoPosAtTime
    ******************************************************************************/
    bool AbstractTIMS::getLastCarPositionAtTime(const uint64_t millis, PositionSample& sample) const
    {
      sample.lastCarPosition = ATC::int32Min;
      uint16_t index_ = latestPositionIndex;

      for (uint16_t i = 0U; i < historySize; i++)
      {
        if (millis >= positionHistory[index_].millis)
        {
          sample = positionHistory[index_];
          break;
        }
        else
        {
          // go further back in time
          index_ = (index_ == 0U) ? (maxHistorySize - 1U) : (index_ - 1U);
        }
      }

      return sample.lastCarPosition != ATC::int32Min;
    }
  }
}
