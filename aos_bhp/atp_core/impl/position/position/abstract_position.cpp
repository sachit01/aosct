/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file implements the abstract (core) position component class.
*
*******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-08    lantback    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-04-22    lantback    Added component type
* 2016-07-25    spandita    Added stub code for track of getCurrFrontPosOdo/getCurrFrontPos
*                           to testing/working
* 2016-08-10    spandita    Implemented/Added the functionality for position
* 2016-08-22    spandita    Updated the code with review comments
* 2016-08-25    spandita    Changed the name of log function to writeToLog
* 2016-08-31    spandita    updated the name change of readtrainsetup to gettrainsetup
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-19    arastogi    added the balise search mode functionality. Fixed bugs.
* 2016-09-27    spandita    Corrected the orientation check
* 2016-10-03    spandita    bug fix in getcurrentantennaodo variable and missed balise
* 2016-10-03    arastogi    Added functions to get safe front and rear position.
* 2016-10-12    arastogi    Fixed calling update current pos only when MA is available.
* 2016-10-14    arastogi    First missed balise id is saved to a variable to avoid null ptr.
*                           Missed balise check is performed from the last received balise
* 2016-10-03    arastogi    Fixed rear odo calculation.
* 2016-10-24    arastogi    Fixed checking of missed balises on movement and on detection when
*                           driving opposite to MA.
* 2017-03-21    spandita    Updated the position with the requirements provided in balise search section
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "abstract_position.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_cross_compare.hpp"
#include "abstract_tracks.hpp"
#include "abstract_mode_control.hpp" 
#include "abstract_tsetup.hpp"
#include "abstract_targets.hpp"
#include "abstract_analyzer_if.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_message_handler.hpp"
#include "atc_math.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_position_event_ids.hpp"
#include <vfw_checkpoints.h>

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
  /**
  * Allows AbstractAnalyzerIF to read the enum PosAccuracyState from AbstractPosition.
  */
  class AccuracyStateGetter : public ATC::AbstractAnalyzerIF::EnumValueGetter
  {
  public:
    AccuracyStateGetter(const ATP::Pos::AbstractPosition* const abstractPos)
      : abstractPosition(abstractPos)
    {
    }

    virtual uint32_t getValue() const
    {
      return static_cast<uint32_t>(abstractPosition->getAccuracyState());
    }

  private:
    AccuracyStateGetter();
    AccuracyStateGetter& operator=(const AccuracyStateGetter& other);
    const  ATP::Pos::AbstractPosition* abstractPosition;
  };

}


/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/

namespace ATP
{
  namespace Pos
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractPosition::AbstractPosition() : ATC::ProcComponent(atpPositionId, "Position", "POS"), initDone(false),
      // creating different set of objects for different type of events
      missedFirstBalise(ATC::Event::createLogEvent(atpPositionId, ATC::CoreContainer, eventIdFirstMissedBalise,
        0x0U, "Missed 1 Balise, Balise ID:", true)),
      missedBaliseFound(ATC::Event::createSafeBrakeSBEvent(atpPositionId, ATC::CoreContainer, eventIdMissedBalise,
        ATC::DriverSB, DMICom::posMissedBalErr, "More Missed Balise, Balise ID:", true)),
      yardOrShuntingModeNotAllowed(ATC::Event::createEBReqEvent(atpPositionId, ATC::CoreContainer, eventIdYardMode,
        ATC::NoEB, DMICom::posYardOrShuntingModeNotAllowed, "Danger for shunting balise in Yard/Shunting mode")),
      outSideBaliseFound(ATC::Event::createSafeBrakeSBEvent(atpPositionId, ATC::CoreContainer, eventIdOutSideBalise,
        ATC::DriverSB, DMICom::posOutsideBalWindow, "Outside of Balise window")),
      unKnownBaliseFound(ATC::Event::createSafeBrakeSBEvent(atpPositionId, ATC::CoreContainer, eventIdUnExpBalise,
        ATC::NoSB, DMICom::posUnknownBal, "Unknown Balise Found, Balise ID:", true)),
      outOfLimitBoundaries(ATC::Event::createSafetyHaltEvent(atpPositionId, ATC::CoreContainer, eventIdOutOfRange,
        ATC::NoEB, DMICom::posExceededRefRange, "Exceeded the Reference range")),
      rearOrFrontEndoutOfLimit(ATC::Event::createSafeBrakeSBEvent(atpPositionId, ATC::CoreContainer, eventIdFrontOrRearEndOutOfLimit,
        ATC::DriverSB, DMICom::posFrontOrRearEndOutOfBounderies, "Either Front or Rear end out of boundaries")),
      invalidBaliseInBalisesearch(ATC::Event::createSBReqEvent(atpPositionId, ATC::CoreContainer, eventIdInvalidBaliseInBalisesearch,
        ATC::NoSB, DMICom::invalidBaliseInBaliseSearch, "Invalid Balise detected in BS mode, Balise ID:", true)),
      identicalBaliseFoundError(ATC::Event::createSafeBrakeSBEvent(atpPositionId, ATC::CoreContainer, eventIdIdenticalBaliseFound,
        ATC::DriverSB, DMICom::posSecondAndFirstBalIdentical, "Second balise is same as first balise, Balise ID:", true)),
      secBaliseFoundBeforeMA(ATC::Event::createSBReqEvent(atpPositionId, ATC::CoreContainer, eventIdSecBaliseFoundBeforeMA,
        ATC::NoSB, DMICom::secBaliseFoundBeforeMA, "Second Balise Found Before MA, Balise ID:", true)),
      thirdBalFound(ATC::Event::createSafeBrakeSBEvent(atpPositionId, ATC::CoreContainer, eventIdThirdBalError,
        ATC::DriverSB, DMICom::posThirdBalError, "3rd balise found before 2nd Balise validation, Third Balise ID: ", true)),
      invalidBalInPossession(ATC::Event::createSBReqEvent(atpPositionId, ATC::CoreContainer, eventIdInvalBalInPos,
        ATC::NoSB, DMICom::invalidBalInPos, "Invalid Balise in Possession, Balise ID:", true)),
      baliseDetectedIrrelevantMode(ATC::Event::createLogEvent(atpPositionId, ATC::CoreContainer,
        eventIdBaliseDetectedIrrelevantMode, 0x0U, "Balise Detected in irrelevant Mode: Ignoring balise detection")),
      baliseDetected(ATC::Event::createLogEvent(atpPositionId, ATC::CoreContainer,
        eventIdBaliseDetected, 0x0U, "Detected Balise ID (at offset): ",true))
    {
      if (corePositionInstancePtr != 0)
      {
        ATC::aosHalt(__FILE__, __LINE__, "Position constructor already instantiated");
      }
      // Setup single instance pointer for core access
      corePositionInstancePtr = this;
      secBalFoundBeforeMA = false;
    }
    /******************************************************************************
    * run
    ******************************************************************************/
    void AbstractPosition::run(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "POS_run");

      //reset odometer offset every cycle.
      odoOffset = 0;
      isBaliseReceived = false;
      //save the current odometer reading
      prevOdoPos = currOdoPos;
      //get new odometer reading
      currOdoPos = AbstractOdometry::corePtr()->getOdoPosition();

      if (AbstractOdometry::corePtr()->isSafetyMarginCrossed() && (accuracyState == PosKnown))
      {
        accuracyState = PosDoubtfull;
      }

      //if an MA from scratch is accepted with first track which was not stored,
      //the odo at 0 is set to current antenna position. Offset needs to be adjusted to
      //position train correctly.
      const bool isMaScratch = Kernel::AbstractMessageHandler::corePtr()->isMAFromScratch();
      if(isMaScratch && (accuracyState != PosUnknown))
      {
        recalcOffset();
      }

      switch (Kernel::AbstractModeControl::corePtr()->getCurrentMode())
      {
        case ATPModePowerUp:
        case ATPModePoweringDown:
        case ATPModeSleeping:
        case ATPModeUnregistered:
        case ATPModeSafetyHalt:
        {//set the accuracy state to unknown
          accuracyState = PosUnknown;
          break;
        }
        case ATPModePossession:
          handlePossession();
          break;
        case ATPModeConfiguration:
        {
          //reset the values of the prev and next balise.
          isFirstBaliseFound = false;
          secBalFoundBeforeMA = false;

          //reset the detected balises if the position is not known.
          if(accuracyState != PosKnown)
          {
            prevBaliseInfo.nidBG = invalidBaliseId;
            prevBaliseInfo.odometerPos = 0;
            nextBaliseInfo.nidBG = invalidBaliseId;
            nextBaliseInfo.odometerPos = 0;
          }
          else
          {
            Kernel::TrainSetupReason tsetupReason;
            bool isTsetupReceived = Kernel::AbstractMessageHandler::corePtr()->getQSetup(tsetupReason);
            if (isTsetupReceived)
            {
              recalcOffset();
              updateCurrentPos();
            }
          }

          clearEncounteredBalises();

          break;
        }
        case ATPModeRegistration:
          handleRegistration();
          break;

        case ATPModeBaliseSearch:
          handleBaliseSearch();
          break;

        case ATPModeSafeBrakeToStop:
          handleSafeBrakeToStop();
          break;

        case ATPModeSplit:
        case ATPModeJoin:
        case ATPModeStaffResponsible:
        case ATPModeLocation:
        case ATPModeShuntingRoute:
        case ATPModeNormal:
          //update the current front and rear position

          updateCurrentPos();
          readBalises();
          checkMissedBaliseOnMovement();
          //Update the next expected balise
          updatePrevAndNextBaliseInfo();
          break;
        case ATPModeShunting:
        case ATPModeYard:
        {
          accuracyState = PosUnknown;
          //get status yard mode movement
          if (AbstractDecode::corePtr()->getDangerForShunting())
          {
            //emergency brake
            ATC::AbstractEventHandler::corePtr()->reportEvent(yardOrShuntingModeNotAllowed, __FILE__
              , __LINE__);
          }

          clearEncounteredBalises();
          break;
        }
        case ATPModesCount:
        case ATPModeUndefined:
        default:
        {
          //aos Halt
          ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        }
      }
    }

    /******************************************************************************
    * readBalises
    ******************************************************************************/
    void AbstractPosition::readBalises(void)
    {
      AbstractDecode::BaliseInfo detectedBalise = { 0U,0 };
      Kernel::BaliseSearchModeState bsModeState =
        Kernel::AbstractModeControl::corePtr()->getBaliseSearchModeState();
      //get the current status of slip/slide 
      bool isSlip = AbstractOdometry::corePtr()->isSlipping();
      bool isSlide = AbstractOdometry::corePtr()->isSliding();
      bool isSlipSlideFound = false;
      //condition for checking the slip for each cycle
      if (isSlip || isSlide)
      {
        isSlipSlideFound = true;
      }

      // Get the next detected balise
      while (AbstractDecode::corePtr()->getBaliseInformation(detectedBalise))
      {
        if ((Kernel::BaliseSearchMode::baliseSearchWaitBalise2 == bsModeState) &&
          (!secBalFoundBeforeMA))
        {
          if ((firstBaliseInfo.nidBG == detectedBalise.nidBG) && (isFirstBaliseFound))
          {
            //Raise the safe to stop
            //prepare the dynamic text to be send while reporting event.
            identicalBaliseFoundError.setDynamicText(static_cast<uint32_t>(firstBaliseInfo.nidBG));
            ATC::AbstractEventHandler::corePtr()->reportEvent(identicalBaliseFoundError, __FILE__, __LINE__);
          }
        }

        isBaliseReceived = true;

        // Check if the detected balise exists among the expected balises
        const DS::Balise* const expectedBalise = DS::AbstractTracks::corePtr()->getBalise(detectedBalise.nidBG);
        if (expectedBalise != static_cast<const DS::Balise* const>(NULL))
        {
          const OdoPosition offsetLocal = expectedBalise->getOdoPosition() - detectedBalise.odometerPos;
          updateCurrentOdoPos(offsetLocal);

          char_t logBuf[200];
          memset(&logBuf[0], 0, sizeof(logBuf));
          const int32_t res = snprintf(&logBuf[0], sizeof(logBuf),
            "Balise ID = %u, expected pos = %d, decoded pos = %d, offset = %d, window = +%u -%u",
            detectedBalise.nidBG, expectedBalise->getOdoPosition(), detectedBalise.odometerPos, offsetLocal,
            AbstractOdometry::corePtr()->getLocoEndBaliseWindow(), AbstractOdometry::corePtr()->getLastCarBaliseWindow());

          if ((res > 0) && (static_cast<size_t>(res) < sizeof(logBuf)))
          {
            writeToLog(ATC::BriefLog, &logBuf[0], __FILE__, __LINE__);
            trace.write(ATC::briefTrace, &logBuf[0]);
          }

          char_t buffer[20];
          memset(&buffer[0], 0, sizeof(buffer));
          const int32_t result = snprintf(&buffer[0], sizeof(buffer), "%u (%d)",
            detectedBalise.nidBG, offsetLocal);

          if ((result > 0) && (static_cast<size_t>(result) < sizeof(buffer)))
          {
            baliseDetected.setDynamicText(&buffer[0]);
            ATC::AbstractEventHandler::corePtr()->reportEvent(baliseDetected, __FILE__, __LINE__);
          }

          AbstractOdometry::corePtr()->logCODDetails(true);

          const uint32_t absoluteOffset = static_cast<uint32_t>(ATC::ATCMath::instance().absolute(offsetLocal, __FILE__, __LINE__));
          bool withinBaliseWindow = false;
          if (offsetLocal >= 0)
          {
            withinBaliseWindow = absoluteOffset <= AbstractOdometry::corePtr()->getLastCarBaliseWindow();
          }
          else
          {
            withinBaliseWindow = absoluteOffset <= AbstractOdometry::corePtr()->getLocoEndBaliseWindow();
          }

          switch (accuracyState)
          {
            case PosKnown:
              if (withinBaliseWindow)
              {
                odoOffset = offsetLocal;
              }
              else
              {
                if (isSlipSlideFound)
                {
                  odoOffset = offsetLocal;
                }
                else
                {
                  accuracyState = PosDoubtfull;
                  ATC::AbstractEventHandler::corePtr()->reportEvent(outSideBaliseFound, __FILE__, __LINE__);
                }
              }
              break;
            case PosApprox:
              if (isSlipSlideFound)
              {
                odoOffset = offsetLocal;
              }
              accuracyState = PosKnown;

              // Last, next an prev Balise are cleared to avoid missed balise, when last balise is found in stored balise list
              lastDetectedBaliseInfo.nidBG = invalidBaliseId;
              nextBaliseInfo.nidBG = invalidBaliseId;
              prevBaliseInfo.nidBG = invalidBaliseId;
              break;
            case PosDoubtfull:
              odoOffset = offsetLocal;
              break;
            case PosUnknown:
            default:
              // For Lint
              break;

          }

          //check for number of missed balises
          checkMissedBaliseOnDetection(detectedBalise);

          //Update the last detected balise
          lastDetectedBaliseInfo = detectedBalise;
          dirAtLastBalise = AbstractOdometry::corePtr()->getOdoDirection();

          //Update the prev balise info to the current detected balise.
          prevBaliseInfo = detectedBalise;
          dirAtPrevBalise = dirAtLastBalise;
          //find the next expected balise.
          findNextExpectedBalise();
        }
        else //detected balise is not in MA list
        {
          //update the recent balise received
          lastDetectedBaliseInfo = detectedBalise;
          if ((PosKnown == accuracyState) || (PosApprox == accuracyState))
          {
            accuracyState = PosDoubtfull;
            //safe brake 
            //prepare the dynamic text to be send while reporting event.
            unKnownBaliseFound.setDynamicText(static_cast<uint32_t>(detectedBalise.nidBG));
            ATC::AbstractEventHandler::corePtr()->reportEvent(unKnownBaliseFound, __FILE__, __LINE__);
          }
        }
      }
    }

    /******************************************************************************
    * updatePrevAndNextBaliseInfo
    ******************************************************************************/
    void AbstractPosition::updatePrevAndNextBaliseInfo()
    {
      TravelDir odoDir = AbstractOdometry::corePtr()->getOdoDirection();

      //if current odometer direction is defined
      if ((odoDir == DirForward) || (odoDir == DirReverse))
      {
        //if current direction is not the same as the direction at previous balise
        if ((odoDir != dirAtPrevBalise) && (dirAtPrevBalise != DirUndefined))
        {
          AbstractDecode::BaliseInfo  temp;
          temp = prevBaliseInfo;

          //Interchange the previous and next balise info as the travel direction has flipped.
          prevBaliseInfo = nextBaliseInfo;
          nextBaliseInfo = temp;
          dirAtPrevBalise = odoDir;

          lastDetectedBaliseInfo.nidBG = invalidBaliseId;
          isBaliseReceived = false;

          trace.write(ATC::briefTrace, "Next Expected Balise:", static_cast<int16_t>(nextBaliseInfo.nidBG));
          trace.write(ATC::briefTrace, "Prev Expected Balise:", static_cast<int16_t>(prevBaliseInfo.nidBG));
        }
      }

      //if the next balise id is 0, try to find the next balise.
      if (nextBaliseInfo.nidBG == invalidBaliseId)
      {
        findNextExpectedBalise();
      }
    }

    /******************************************************************************
    * findNextExpectedBalise
    ******************************************************************************/
    void AbstractPosition::findNextExpectedBalise()
    {
      const TravelDir currentOdoDir = AbstractOdometry::corePtr()->getOdoDirection();
      //balise start iterator
      DS::AbstractTracks::ConstBaliseListIteratorType baliseItr = DS::AbstractTracks::corePtr()->getBaliseIter();
      //balise end iterator
      DS::AbstractTracks::ConstBaliseListIteratorType baliseItrEnd = DS::AbstractTracks::corePtr()->getBaliseIterEnd();

      //proceed only if the odometer direction is known.
      if ((currentOdoDir == DirForward) || (currentOdoDir == DirReverse))
      {
        //find the iterator for previous balise.
        while (baliseItr != baliseItrEnd)
        {
          uint16_t bID = (*baliseItr)->getBaliseId();
          if (bID == prevBaliseInfo.nidBG)
          {
            break;
          }
          ++baliseItr;
        }

        //if the previous balise is found in balise list
        if (baliseItr != baliseItrEnd)
        {
          //if current odometer direction is same as the tracks list direction
          if (currentOdoDir == DS::AbstractTracks::corePtr()->getTravelDirection())
          {
            ++baliseItr;
            //if there is a next balise
            if (baliseItr != baliseItrEnd)
            {
              nextBaliseInfo.nidBG = (*baliseItr)->getBaliseId();
              nextBaliseInfo.odometerPos = (*baliseItr)->getOdoPosition();
              trace.write(ATC::briefTrace, "Next Expected Balise:", static_cast<int16_t>(nextBaliseInfo.nidBG));
            }
            else
            {
              nextBaliseInfo.nidBG = invalidBaliseId;
              nextBaliseInfo.odometerPos = 0;
            }
          }

          //if the current odometer direction is opposite to the tracks list direction
          else
          {
            //And the prev balise is not the first one in the balise list
            if (baliseItr != DS::AbstractTracks::corePtr()->getBaliseIter())
            {
              --baliseItr;
              nextBaliseInfo.nidBG = (*baliseItr)->getBaliseId();
              nextBaliseInfo.odometerPos = (*baliseItr)->getOdoPosition();
              trace.write(ATC::briefTrace, "Next Expected Balise:", static_cast<int16_t>(nextBaliseInfo.nidBG));
            }
            else
            {
              nextBaliseInfo.nidBG = invalidBaliseId;
              nextBaliseInfo.odometerPos = 0;
            }
          }
        }
        //if the previous balise is not found in the balise list
        else
        {
          //the next balise should be invalid
          if (nextBaliseInfo.nidBG == invalidBaliseId)
          {
            baliseItr = DS::AbstractTracks::corePtr()->getBaliseIter();

            //if balise list is not empty.
            if (baliseItr != DS::AbstractTracks::corePtr()->getBaliseIterEnd())
            {
              //if current odometer direction is same as the tracks list direction
              //next balise is the first balise in the list
              if (currentOdoDir == DS::AbstractTracks::corePtr()->getTravelDirection())
              {
                nextBaliseInfo.nidBG = (*baliseItr)->getBaliseId();
                nextBaliseInfo.odometerPos = (*baliseItr)->getOdoPosition();
                trace.write(ATC::briefTrace, "Next Expected Balise:", static_cast<int16_t>(nextBaliseInfo.nidBG));
              }
              //otherwise the next balise is still 0. But we should update the prev balise
              else
              {
                prevBaliseInfo.nidBG = (*baliseItr)->getBaliseId();
                prevBaliseInfo.odometerPos = (*baliseItr)->getOdoPosition();
                dirAtPrevBalise = currentOdoDir;
              }
            }
            else
            {
              //Do nothing.. cannot find the next balise as there are no balises in the list.
            }

          }
          else
          {
            //It should never come here..
            //This function is called when we detect a balise or miss a balise
            //or when the next balise id is invalid.
            //In the first 2 cases the previous balise is the detected/missed
            //balise and should be in the balise list.
            //TODO Should an event be raise here?
          }
        }
      }
      else
      {
        nextBaliseInfo.nidBG = invalidBaliseId;
        nextBaliseInfo.odometerPos = 0;
      }
    }

    /******************************************************************************
    * checkMissedBaliseOnMovement
    ******************************************************************************/
    void AbstractPosition::checkMissedBaliseOnMovement(void)
    {
      //if balise from decode is not available and the next balise id is valid
      if ((getAccuracyState() == PosKnown) && (!isBaliseReceived) && (nextBaliseInfo.nidBG != invalidBaliseId))
      {
        //only check for missed balise this cycle if there is movement
        if (prevOdoPos != currOdoPos)
        {
          bool missedBalise = false;

          //report latency in case of delay in reading 
          const uint16_t currentSpeed = AbstractOdometry::corePtr()->getSpeed();
          const TravelDir currentOdoDir = AbstractOdometry::corePtr()->getOdoDirection();
          const uint16_t baliseAirGap = getBaliseAirGap();

          uint32_t missedBaliseReportLatency = baliseAirGap +
            ((static_cast<uint32_t>(baliseReadTimeDelay) * static_cast<uint32_t>(currentSpeed)) / 1000U);

          if (DirForward == currentOdoDir)
          {
            const OdoPosition detectionThreshold = nextBaliseInfo.odometerPos
              + (static_cast<int32_t>(AbstractOdometry::corePtr()->getLastCarBaliseWindow())
                + static_cast<int32_t>(missedBaliseReportLatency));

            //if the current odometer reading is more than the threshold to detect balise
            // and previous odometer reading was within the threshold
            if ((currOdoPos > detectionThreshold) && (prevOdoPos <= detectionThreshold))
            {
              missedBalise = true;
            }
          }
          else if (DirReverse == currentOdoDir)
          {
            const OdoPosition detectionThreshold = nextBaliseInfo.odometerPos
              - (static_cast<int32_t>(AbstractOdometry::corePtr()->getLocoEndBaliseWindow())
                + static_cast<int32_t>(missedBaliseReportLatency));

            //if the current odometer reading is less than the threshold to detect balise
            // and previous odometer reading was within the threshold
            if ((currOdoPos < detectionThreshold) && (prevOdoPos >= detectionThreshold))
            {
              missedBalise = true;
            }
          }
          else
          {
            // No movement - do nothing
          }

          if (missedBalise)
          {
            trace.write(ATC::briefTrace, "Passed limit for balise ", static_cast<int16_t>(nextBaliseInfo.nidBG));
            numOfMissedBalise += 1U;
            reportMissedBalise(nextBaliseInfo.nidBG);

            //update the prev balise.
            prevBaliseInfo = nextBaliseInfo;
            dirAtPrevBalise = currentOdoDir;
            //find next expected balise
            findNextExpectedBalise();
          }
        }
      }
    }

    /******************************************************************************
    * Take the necessary actions for a missed balise
    ******************************************************************************/
    void AbstractPosition::reportMissedBalise(const uint16_t missedBaliseId)
    {
      ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      //when missed balise is greater than 2
      //or when missed balise is greater than 1 in balise search mode
      if ((numOfMissedBalise >= 2U) ||
        ((currentMode == ATPModeBaliseSearch) && (numOfMissedBalise >= 1U)))
      {
        if (accuracyState == PosKnown)
        {
          accuracyState = PosDoubtfull;
          //trigger the safebraketostop
          //prepare the dynamic text to be send while reporting event.
          missedBaliseFound.setDynamicText(static_cast<uint32_t>(missedBaliseId));
          ATC::AbstractEventHandler::corePtr()->reportEvent(missedBaliseFound, __FILE__, __LINE__);
        }        
      }
      else if (numOfMissedBalise == 1U)
      {
        if (accuracyState == PosKnown)
        {
          //issue log event
          //prepare the dynamic text to be send while reporting event.
          missedFirstBalise.setDynamicText(static_cast<uint32_t>(missedBaliseId));
          ATC::AbstractEventHandler::corePtr()->reportEvent(missedFirstBalise, __FILE__, __LINE__);
        }
      }
      else
      {
        //Do nothing
      }
    }

    /******************************************************************************
    * Update the Current Position
    ******************************************************************************/
    void AbstractPosition::updateCurrentPos(void)
    {
      //Check if front and rear position are within the track
      const TrackAndPos calSafeFrontPos = DS::AbstractTracks::corePtr()->calculateTrackAndPos(getLeadingPosOdo());
      const TrackAndPos calSafeRearPos = DS::AbstractTracks::corePtr()->calculateTrackAndPos(getTrailingPosOdo());

      if (((0U == calSafeFrontPos.track) &&
        (0U == calSafeRearPos.track)) &&
        (accuracyState != PosUnknown))
      {
        //Front and rear end train position are outside the stored track and position is not unknown
        accuracyState = PosDoubtfull;
        //Raising Safety Halt event
        ATC::AbstractEventHandler::corePtr()->reportEvent(outOfLimitBoundaries, __FILE__
          , __LINE__);
      }
      else if (((0U == calSafeFrontPos.track) ||
        (0U == calSafeRearPos.track)) &&
        ((PosKnown == accuracyState) || (PosApprox == accuracyState)))
      {
        //if either front or rear end of train are outside of stored tracks  and current position is known or approx
        accuracyState = PosDoubtfull;
        //safe brake to stop
        ATC::AbstractEventHandler::corePtr()->reportEvent(rearOrFrontEndoutOfLimit, __FILE__
          , __LINE__);
      }
      else
      {
        //check for current odo value present in track list
        TrackAndPos calPos = DS::AbstractTracks::corePtr()->calculateTrackAndPos(currOdoPos);
        const TrackAndPos calLeadingPos = DS::AbstractTracks::corePtr()->calculateTrackAndPos(getLeadingPosOdo());
        if (0U != calPos.track)
        {   //setting antenna position 
          currPosAntenna = calPos;
          //check for Current Front odo in MA track list 
          if (0U != calLeadingPos.track)
          {
            //set the leading position
            currentPosNominalLeading = calLeadingPos;
          }

        }
      }
    }//end of updateLastPos

    /******************************************************************************
    * Calculate number of missed balises
    ******************************************************************************/
    void AbstractPosition::checkMissedBaliseOnDetection(const AbstractDecode::BaliseInfo& newBaliseInfo)
    {
      //check if the last balise id is not zero and if the last detected balise is not same as the new balise
      if ((lastDetectedBaliseInfo.nidBG != invalidBaliseId) && (lastDetectedBaliseInfo.nidBG != newBaliseInfo.nidBG))
      {
        //get the balise object from the balise list
        if (DS::AbstractTracks::corePtr()->getBalise(lastDetectedBaliseInfo.nidBG) != static_cast<const DS::Balise* const>(NULL))
        {
          uint16_t missedBaliseId = invalidBaliseId;
          numOfMissedBalise = 0U;

          //get the balise iterator to start of balise list
          DS::AbstractTracks::ConstBaliseListIteratorType baliseItr = DS::AbstractTracks::corePtr()->getBaliseIter();
          DS::AbstractTracks::ConstBaliseListIteratorType baliseItrBegin = DS::AbstractTracks::corePtr()->getBaliseIter();
          DS::AbstractTracks::ConstBaliseListIteratorType baliseItrEnd = DS::AbstractTracks::corePtr()->getBaliseIterEnd();

          TravelDir currOdoDir = AbstractOdometry::corePtr()->getOdoDirection();

          //iterator over the balise list till last saved balise id
          while (baliseItr != baliseItrEnd)
          {
            if ((*(baliseItr))->getBaliseId() == lastDetectedBaliseInfo.nidBG)
            {
              break;
            }
            else
            {
              ++baliseItr;
            }
          }

          //iterate forward or backward in the balise list based on odo dir vs the balise list direction
          //If the odo direction is the same as the list direction.
          if (currOdoDir == DS::AbstractTracks::corePtr()->getTravelDirection())
          {
            if (baliseItr != baliseItrEnd)
            {
              // Increment the balise iterator
              ++baliseItr;

              //Iterate forward in the balise list
              while (baliseItr != baliseItrEnd)
              {
                uint16_t bId = (*baliseItr)->getBaliseId();
                //Exit loop if the current received balise is found.
                if (bId == newBaliseInfo.nidBG)
                {
                  break;
                }

                trace.write(ATC::briefTrace, "Missed Balise with ID ", static_cast<uint32_t>(bId));
                numOfMissedBalise += 1U;
                missedBaliseId = bId;

                ++baliseItr;
              }
            }
          }
          //If the odo direction is the opposite to the list direction.
          else
          {
            //If the list is not empty.
            if (baliseItr != baliseItrBegin)
            {
              //Iterate backward in the balise list
              do
              {
                --baliseItr;
                uint16_t bId = (*baliseItr)->getBaliseId();
                //Exit loop if the current received balise is found.
                if (bId == newBaliseInfo.nidBG)
                {
                  break;
                }

                trace.write(ATC::briefTrace, "Missed Balise with ID ", static_cast<uint32_t>(bId));
                numOfMissedBalise += 1U;
                missedBaliseId = bId;
              } while (baliseItr != baliseItrBegin);
            }
          }

          if (missedBaliseId != invalidBaliseId)
          {
            reportMissedBalise(missedBaliseId);
          }
        }
      }
      else if ((nextBaliseInfo.nidBG != invalidBaliseId) && (nextBaliseInfo.nidBG != newBaliseInfo.nidBG))
      {
        trace.write(ATC::briefTrace, "Missed Balise with ID ", static_cast<uint32_t>(nextBaliseInfo.nidBG));
        numOfMissedBalise += 1U;
        reportMissedBalise(nextBaliseInfo.nidBG);
      }
      else
      {
        //do nothing
      }
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractPosition::init(void)
    {
      if (!initDone)
      {
        initDone = true;
        //initialization of variables
        accuracyState = PosUnknown;
        currentPosNominalLeading.position = 0U;
        currentPosNominalLeading.track = 0U;
        currPosAntenna.position = 0U;
        currPosAntenna.track = 0U;
        lastDetectedBaliseInfo.odometerPos = 0;
        odoOffset = 0;
        lastDetectedBaliseInfo.nidBG = invalidBaliseId;
        numOfMissedBalise = 0U;
        isFirstBaliseFound = false;
        secBalFoundBeforeMA = false;
        currOdoPos = 0;
        prevOdoPos = 0;
        dirAtPrevBalise = DirUndefined;
        nextBaliseInfo.nidBG = invalidBaliseId;
        nextBaliseInfo.odometerPos = 0;
        prevBaliseInfo.nidBG = invalidBaliseId;
        prevBaliseInfo.odometerPos = 0;
        dirAtLastBalise = DirUndefined;

        // initCrossCompare
        initCrossCompare();

        static AccuracyStateGetter accuracyStateGetter(this);
        // Register measurement data to AIF component
        ATC::AbstractAnalyzerIF* const aif = ATC::AbstractAnalyzerIF::corePtr();
        const bool posTrack = aif->registerMeasurement("antennaTrack", "current antenna track", "id",
          0U, ATC::uint16Max, &currPosAntenna.track);
        const bool posPosition = aif->registerMeasurement("antennaPos", "current antenna pos", "cm",
          0U, ATC::uint32Max, &currPosAntenna.position);
        const bool posCurAccuracyState = aif->registerMeasurement("accuracyState", "current accuracyState", "state",
          0U, 3U, &accuracyStateGetter);

        if (!(posCurAccuracyState && posTrack && posPosition))
        {
          writeToLog(ATC::BriefLog, "Register measurement failed for analyzer", __FILE__, __LINE__);
        }
      }
      return initDone;
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractPosition* AbstractPosition::corePtr(void)
    {
      return corePositionInstancePtr;
    }

    /******************************************************************************
    * getLeadingPos
    ******************************************************************************/
    const TrackAndPos& AbstractPosition::getLeadingPos(void) const
    {
      return currentPosNominalLeading;
    }

    /******************************************************************************
    * getLeadingPosOdo
    ******************************************************************************/
    OdoPosition AbstractPosition::getLeadingPosOdo(void) const
    {
      TravelDir direction = DS::AbstractTargets::corePtr()->getSupposedTravelDir();

      return getLeadingPosForDirectionOdo(direction);
    }

    /******************************************************************************
    * getSafeLeadingPosOdo
    ******************************************************************************/
    OdoPosition AbstractPosition::getSafeLeadingPosOdo(void) const
    {
      OdoPosition safeFrontPos = getLeadingPosOdo();
      OdoPosition rearPos = getTrailingPosOdo();

      if (safeFrontPos > rearPos)
      {
        safeFrontPos += static_cast<OdoPosition>(AbstractOdometry::corePtr()->getLocoEndBaliseWindow());
      }
      else if (safeFrontPos < rearPos)
      {
        safeFrontPos -= static_cast<OdoPosition>(AbstractOdometry::corePtr()->getLastCarBaliseWindow());
      }
      else
      {
        trace.write(ATC::briefTrace, "Invalid front and rear positions");
        //should never come here
      }

      return safeFrontPos;
    }

    /******************************************************************************
    * getActualLeadingPosOdo
    ******************************************************************************/
    OdoPosition AbstractPosition::getActualLeadingPosOdo(void) const
    {
      TravelDir direction = Kernel::AbstractModeControl::corePtr()->getCurrentDrivingDirection();

      return getLeadingPosForDirectionOdo(direction);
    }

    /******************************************************************************
    * getTrailingPosOdo
    ******************************************************************************/
    OdoPosition AbstractPosition::getTrailingPosOdo(void) const
    {
      TravelDir direction = DS::AbstractTargets::corePtr()->getSupposedTravelDir();

      return getTrailingPosForDirectionOdo(direction);
    }

    /******************************************************************************
    * getSafeTrailingPosOdo
    ******************************************************************************/
    OdoPosition AbstractPosition::getSafeTrailingPosOdo(void) const
    {
      OdoPosition frontPos = getLeadingPosOdo();
      OdoPosition safeRearPos = getTrailingPosOdo();

      if (frontPos > safeRearPos)
      {
        safeRearPos -= static_cast<OdoPosition>(AbstractOdometry::corePtr()->getLastCarBaliseWindow());
      }
      else if (frontPos < safeRearPos)
      {
        safeRearPos += static_cast<OdoPosition>(AbstractOdometry::corePtr()->getLocoEndBaliseWindow());
      }
      else
      {
        trace.write(ATC::briefTrace, "Invalid front and rear positions");
        //should never come here
      }

      return safeRearPos;
    }

    /******************************************************************************
    * getActualTrailingPosOdo
    ******************************************************************************/
    OdoPosition AbstractPosition::getActualTrailingPosOdo(void) const
    {
      TravelDir direction = Kernel::AbstractModeControl::corePtr()->getCurrentDrivingDirection();

      return getTrailingPosForDirectionOdo(direction);
    }

    /******************************************************************************
    * getLeadingPosForDirectionOdo
    ******************************************************************************/
    OdoPosition AbstractPosition::getLeadingPosForDirectionOdo(TravelDir const direction) const
    {
      OdoPosition odoPosition = 0;
      const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

      if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
      {
        odoPosition = getLocoEndPosOdo();

        if (DirReverse == direction)
        {
          odoPosition -= static_cast<int32_t>(pTrainSetup->length);
        }
      }
      return odoPosition;
    }

    /******************************************************************************
    * getTrailingPosForDirectionOdo
    ******************************************************************************/
    OdoPosition AbstractPosition::getTrailingPosForDirectionOdo(TravelDir const direction) const
    {
      OdoPosition odoPosition = 0;
      const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

      if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
      {
        odoPosition = getLocoEndPosOdo();

        if (DirReverse != direction)
        {
          odoPosition -= static_cast<int32_t>(pTrainSetup->length);
        }
      }

      return odoPosition;
    }

    /******************************************************************************
    * getLocoEndPosOdo
    ******************************************************************************/
    OdoPosition AbstractPosition::getLocoEndPosOdo(void) const
    {
      OdoPosition odoPosition;

      const LocoVsTrainDir locoVsTrainDir = DS::AbstractTSetup::corePtr()->getLocovsTrainOrientation();
      if (locoVsTrainDir == locoVsTrainBDirection) // A end facing cars
      {
        odoPosition = getCurrAntennaPosOdo() +
          static_cast<OdoPosition>(AbstractConfig::corePtr()->getBalAntennaPosEnd());
      }
      else // B end facing cars
      {
        odoPosition = getCurrAntennaPosOdo() +
          static_cast<OdoPosition>(AbstractConfig::corePtr()->getBalAntennaPosFront());
      }

      return odoPosition;
    }

    /******************************************************************************
    * getCurrAntennaPos
    ******************************************************************************/
    const TrackAndPos& AbstractPosition::getCurrAntennaPos(void) const
    {
      return currPosAntenna;
    }
    /******************************************************************************
    * getcurrentodo value at antenna
    ******************************************************************************/
    OdoPosition AbstractPosition::getCurrAntennaPosOdo(void) const
    {
      return currOdoPos;
    }
    /******************************************************************************
    * getAccuracyState
    ******************************************************************************/
    PosAccuracyState AbstractPosition::getAccuracyState(void) const
    {
      return accuracyState;
    }

    /******************************************************************************
    * getOdometerOffsetCorrection
    ******************************************************************************/
    OdoPosition AbstractPosition::getOdometerOffsetCorrection(void) const
    {
      return odoOffset;
    }

    /******************************************************************************
    * isBaliseValid
    ******************************************************************************/
    bool AbstractPosition::isPassedBaliseExpected(const AbstractDecode::BaliseInfo& baliseInfo) const
    {
      return (this->prevBaliseInfo.nidBG == baliseInfo.nidBG);
    }

    /******************************************************************************
    * getBaliseInfo
    ******************************************************************************/
    bool AbstractPosition::getBaliseInfo(AbstractDecode::BaliseInfo& baliseInfo) const
    {
      bool retval = false;

      //check if a balise has been received this cycle
      if (isBaliseReceived)
      {
        baliseInfo = lastDetectedBaliseInfo;
        retval = true;
      }

      return retval;
    }

    /******************************************************************************
    * getFirstBaliseInfo
    ******************************************************************************/
    bool AbstractPosition::getFirstBaliseInfo(AbstractDecode::BaliseInfo& baliseInfo) const
    {
      bool retval = false;

      //check if the mode is balise search and first balise has been detected.
      if ((Kernel::AbstractModeControl::corePtr()->getCurrentMode() == ATPModeBaliseSearch) &&
        isFirstBaliseFound)
      {
        baliseInfo = firstBaliseInfo;
        retval = true;
      }

      return retval;
    }

    /******************************************************************************
    * clearEncounteredBalises
    ******************************************************************************/
    void AbstractPosition::clearEncounteredBalises(void) const
    {
      AbstractDecode::BaliseInfo baliseObj = { 0U,0 };

      while (AbstractDecode::corePtr()->getBaliseInformation(baliseObj))
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(baliseDetectedIrrelevantMode, __FILE__,
          __LINE__);
      }
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractPosition::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&initDone));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<PosAccuracyState>(&accuracyState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&currentPosNominalLeading.track));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&currentPosNominalLeading.position));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&currPosAntenna.track));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&currPosAntenna.position));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&currOdoPos));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&prevOdoPos));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&prevPosAntenna.track));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&prevPosAntenna.position));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&odoOffset));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&numOfMissedBalise));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&isFirstBaliseFound));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&firstBaliseInfo.nidBG));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&firstBaliseInfo.odometerPos));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&lastDetectedBaliseInfo.nidBG));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&lastDetectedBaliseInfo.odometerPos));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TravelDir>(&dirAtLastBalise));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&prevBaliseInfo.nidBG));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&prevBaliseInfo.odometerPos));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&nextBaliseInfo.nidBG));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&nextBaliseInfo.odometerPos));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TravelDir>(&dirAtPrevBalise));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&isBaliseReceived));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&secBalFoundBeforeMA));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&missedFirstBalise));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&missedBaliseFound));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&yardOrShuntingModeNotAllowed));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&outSideBaliseFound));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&unKnownBaliseFound));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&unDefATPMode));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&outOfLimitBoundaries));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&rearOrFrontEndoutOfLimit));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidBaliseInBalisesearch));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&identicalBaliseFoundError));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&secBaliseFoundBeforeMA));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&thirdBalFound));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidBalInPossession));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&baliseDetectedIrrelevantMode));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&baliseDetected));
    }

    /******************************************************************************
    * updateCurrentOdoPos
    ******************************************************************************/
    void AbstractPosition::updateCurrentOdoPos(const OdoPosition offset)
    {
      //Correction of Odo value in current cycle in order to update the front and rear position
      currOdoPos += offset;
    }

    /******************************************************************************
    * handleBaliseSearch
    ******************************************************************************/
    void AbstractPosition::handleBaliseSearch()
    {
      const Kernel::BaliseSearchModeState bsModeState =
        Kernel::AbstractModeControl::corePtr()->getBaliseSearchModeState();
      if ((bsModeState == Kernel::BaliseSearchMode::baliseSearchWaitBalise2) ||
        (bsModeState == Kernel::BaliseSearchMode::baliseSearchFinishOK))
      {
        //update the current front and rear position
        updateCurrentPos();
        readBalises();
        checkMissedBaliseOnMovement();
        updatePrevAndNextBaliseInfo();
      }
      else if (bsModeState == Kernel::BaliseSearchMode::baliseSearchStart) // Balise search start sub mode
      {
        //Setting Position to Unknown at starting of balise search start
        accuracyState = PosUnknown;
      }
      else
      {
        bool baliseFound = false;
        //check if a balise is received from decode
        AbstractDecode::BaliseInfo detectedBalise = { 0U, 0 };
        if (AbstractDecode::corePtr()->getBaliseInformation(detectedBalise))
        {
          //Check there is no other balise in decode
          AbstractDecode::BaliseInfo detectedBalise2 = { 0U, 0 };
          if (!AbstractDecode::corePtr()->getBaliseInformation(detectedBalise2))
          {
            baliseFound = true;
          }
          else
          {
            //More than 1 balise detected in Balise search mode. Raise an error.
            //prepare the dynamic text to be send while reporting event.
            invalidBaliseInBalisesearch.setDynamicText(static_cast<uint32_t>(detectedBalise2.nidBG));
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidBaliseInBalisesearch, __FILE__, __LINE__);
          }
        }

        //check if in balise search mode and waiting for first balise
        if ((bsModeState == Kernel::BaliseSearchMode::baliseSearchWaitForBaliseReg) && baliseFound)
        {
          //update values for prev and first balise.
          firstBaliseInfo = detectedBalise;
          lastDetectedBaliseInfo = detectedBalise;
          prevBaliseInfo = detectedBalise;

          dirAtLastBalise = AbstractOdometry::corePtr()->getOdoDirection();

          isBaliseReceived = true;
          isFirstBaliseFound = true;

          numOfMissedBalise = 0U;
        }
        else if ((bsModeState == Kernel::BaliseSearchMode::baliseSearchWaitForBaliseReReg) && baliseFound)
        {
          const DS::Balise* const storedBalise = DS::AbstractTracks::corePtr()->getBalise(detectedBalise.nidBG);
          if (storedBalise == static_cast<const DS::Balise*>(NULL))
          {
            unKnownBaliseFound.setDynamicText(static_cast<uint32_t>(detectedBalise.nidBG));
            ATC::AbstractEventHandler::corePtr()->reportEvent(unKnownBaliseFound, __FILE__, __LINE__);
          }
          else
          {
            //update values for prev and first balise.
            firstBaliseInfo = detectedBalise;
            lastDetectedBaliseInfo = detectedBalise;
            prevBaliseInfo = detectedBalise;
            dirAtLastBalise = AbstractOdometry::corePtr()->getOdoDirection();
            isBaliseReceived = true;
            isFirstBaliseFound = true;

            //calculation of odoOffset
            odoOffset = (storedBalise->getOdoPosition() - firstBaliseInfo.odometerPos);
            updateCurrentOdoPos(odoOffset);
            numOfMissedBalise = 0U;

            //set the position to valid
            accuracyState = PosKnown;
            //update the next expected balise as we already have MA
            updatePrevAndNextBaliseInfo();

          }
        }
        else if (bsModeState == Kernel::BaliseSearchMode::baliseSearchWaitMA)
        {
          MAHead mHead;
          bool isMAReceived = Kernel::AbstractMessageHandler::corePtr()->getMAHead(mHead);
          if (isMAReceived)
          {
            if (secBalFoundBeforeMA)
            {
              const DS::Balise* const foundSecondBalise = DS::AbstractTracks::corePtr()->getBalise(lastDetectedBaliseInfo.nidBG);
              if (foundSecondBalise != static_cast<const DS::Balise*>(NULL))
              {
                //calculation of odoOffset
                odoOffset = (foundSecondBalise->getOdoPosition() - lastDetectedBaliseInfo.odometerPos);
                updateCurrentOdoPos(odoOffset);
                secBalFoundBeforeMA = false;

                //Setting Position to Known
                accuracyState = PosKnown;
              }
              else
              {
                //prepare the dynamic text to be send while reporting event.
                unKnownBaliseFound.setDynamicText(static_cast<uint32_t>(lastDetectedBaliseInfo.nidBG));
                ATC::AbstractEventHandler::corePtr()->reportEvent(unKnownBaliseFound, __FILE__, __LINE__);
              }
              checkMissedBaliseOnMovement();
              updatePrevAndNextBaliseInfo();
            }
            else
            {
              const DS::Balise* const storedBalise = DS::AbstractTracks::corePtr()->getBalise(firstBaliseInfo.nidBG);
              if (storedBalise != static_cast<const DS::Balise*>(NULL))
              {
                //calculation of odoOffset
                odoOffset = (storedBalise->getOdoPosition() - firstBaliseInfo.odometerPos);
                updateCurrentOdoPos(odoOffset);

                //Setting Position to Known
                accuracyState = PosKnown;
              }
            }
          }
          else if (baliseFound)
          {
            if (!secBalFoundBeforeMA)
            {
              secBalFoundBeforeMA = true;
              isBaliseReceived = true;
              lastDetectedBaliseInfo = detectedBalise;
              prevBaliseInfo = detectedBalise;
              dirAtLastBalise = AbstractOdometry::corePtr()->getOdoDirection();
              //raise the brake
              //prepare the dynamic text to be send while reporting event.
              secBaliseFoundBeforeMA.setDynamicText(static_cast<uint32_t>(detectedBalise.nidBG));
              ATC::AbstractEventHandler::corePtr()->reportEvent(secBaliseFoundBeforeMA, __FILE__, __LINE__);

              // if the second balise is same as first
              if ((firstBaliseInfo.nidBG == lastDetectedBaliseInfo.nidBG) && (isFirstBaliseFound))
              {
                //Raise the safe to stop
                //prepare the dynamic text to be send while reporting event.
                identicalBaliseFoundError.setDynamicText(static_cast<uint32_t>(lastDetectedBaliseInfo.nidBG));
                ATC::AbstractEventHandler::corePtr()->reportEvent(identicalBaliseFoundError, __FILE__, __LINE__);
              }
            }
            else
            {
              //raise the safe brake to stop
              //prepare the dynamic text to be send while reporting event.
              thirdBalFound.setDynamicText(static_cast<uint32_t>(detectedBalise.nidBG));
              ATC::AbstractEventHandler::corePtr()->reportEvent(thirdBalFound, __FILE__, __LINE__);
            }
          } // (bsModeState == Kernel::BaliseSearchMode::baliseSearchWaitMA)
          else
          {
            // Do nothing, no balise found...
          }
        }
        else
        {
          // Do nothing 
        }
      }
    }

    /******************************************************************************
    * handleSafeBrakeToStop
    ******************************************************************************/
    void AbstractPosition::handleSafeBrakeToStop()
    {
      TrackAndPos fronTnp = { 0U,0U };
      const bool isFinalApproxPosMsgAccepted = Kernel::AbstractMessageHandler::corePtr()->getApproxFrontPos(fronTnp);
      //Checking if Approximate Position message accepted and Current Position is Unknown or Doubtful
      if (isFinalApproxPosMsgAccepted)
      {
        OdoPosition calOdo = 0;
        //check for front odo value present in track list
        if (DS::AbstractTracks::corePtr()->getOdoPos(fronTnp, calOdo))
        {
          odoOffset = (calOdo - getLeadingPosOdo());
          updateCurrentOdoPos(odoOffset);
        }
        updateCurrentPos();

        //Set current position to approx
        accuracyState = PosApprox;
      }
      else
      {
        //update the current front and rear position
        updateCurrentPos();
        readBalises();
        checkMissedBaliseOnMovement();
        //Update the next expected balis
        updatePrevAndNextBaliseInfo();
      }
    }

    /******************************************************************************
    * handleRegistration
    ******************************************************************************/
    void AbstractPosition::handleRegistration()
    {
      TrackAndPos fronTnp = { 0U,0U };

      const bool isFinalApproxPosMsgAccepted = Kernel::AbstractMessageHandler::corePtr()->getApproxFrontPos(fronTnp);
      //Checking if Approximate Position message accepted and Current Position is Unknown or Doubtful
      if (isFinalApproxPosMsgAccepted)
      {
        OdoPosition calOdo = 0;
        //check for front Odo value present in track list
        if (DS::AbstractTracks::corePtr()->getOdoPos(fronTnp, calOdo))
        {
          odoOffset = (calOdo - getLeadingPosOdo());
          updateCurrentOdoPos(odoOffset);
        }
        //Set current position to approx
        accuracyState = PosApprox;
        //Update the front and rear position
        updateCurrentPos();
      }
      else
      {
        //In the case of a re-registration the odo position of the train should be aligned with the start of MA.
        MAHead mHead;
        bool isMAReceived = Kernel::AbstractMessageHandler::corePtr()->getMAHead(mHead);
        if (isMAReceived && (mHead.routeType == RtReRegistration))
        {
          OdoPosition calOdo = 0;
          if (DS::AbstractTracks::corePtr()->getOdoPos(mHead.startOfMATrackAndPos, calOdo))
          {
            odoOffset = (calOdo - getLeadingPosOdo());
            updateCurrentOdoPos(odoOffset);
            updateCurrentPos();
          }
        }
      }

      clearEncounteredBalises();
    }

    /******************************************************************************
    * handlePossession
    ******************************************************************************/
    void AbstractPosition::handlePossession()
    {
      //set the accuracy state to unknown
      accuracyState = PosUnknown;
      AbstractDecode::BaliseInfo detectedBalise = { 0U,0 };
      while (AbstractDecode::corePtr()->getBaliseInformation(detectedBalise))
      {
        if (DS::AbstractTracks::corePtr()->isBaliseInPossessionList(detectedBalise.nidBG))
        {
          char_t logBuf[100];
          memset(&logBuf[0], 0, sizeof(logBuf));
          const int32_t res = snprintf(&logBuf[0], sizeof(logBuf), "Balise Id = %u detected in Possession", detectedBalise.nidBG);

          if ((res > 0) && (static_cast<size_t>(res) < sizeof(logBuf)))
          {
            writeToLog(ATC::BriefLog, &logBuf[0], __FILE__, __LINE__);
          }
        }
        else
        {
          //raise the sb brake
          //prepare the dynamic text to be send while reporting event.
          invalidBalInPossession.setDynamicText(static_cast<uint32_t>(detectedBalise.nidBG));
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidBalInPossession, __FILE__
            , __LINE__);
        }
      }
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool AbstractPosition::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments searches for the "help", "trace" or any other Console
      component specific command calls and handles it. Returns true if completely handled
      else returns false. returning false will let other components handle the call. help always returns false.
      */

      bool retVal = false;

      if (argc >= 1U)
      {
        char_t  buffer[512];
        // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
        if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
        {
          const char_t* const helpText =
            "expBal        Last and Next Expected balise\n"
            "pos           Prints odo position, offset, track, position and accuracy";

          ATC::AbstractConsole::corePtr()->writeWithNewline(helpText);
          retVal = false;
        }
        else if (ATC::isTextMatch(&argv[0][0], "expBal", sizeof("expBal")) && (argc == 1U))
        {
          int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%s%u", "Next Expected Balise: ", nextBaliseInfo.nidBG);

          if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          }

          ret = snprintf(&buffer[0], sizeof(buffer), "%s%u", "Previous Expected Balise: ", prevBaliseInfo.nidBG);

          if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          }
          retVal = true;
        }
        else if (ATC::isTextMatch(&argv[0][0], "pos", sizeof("pos")) && (argc == 1U))
        {
          const char_t* accuracyString;
          switch (accuracyState)
          {
          case PosUnknown:
            accuracyString = "unknown";
            break;
          case PosApprox:
            accuracyString = "approximate";
            break;
          case PosKnown:
            accuracyString = "known";
            break;
          case PosDoubtfull:
            accuracyString = "doubtful";
            break;
          default:
            accuracyString = "INVALID";
            break;
          }

          int32_t ret = snprintf(&buffer[0], sizeof(buffer),
            "odoPos %d, odoOffset %d, track %u, position %u (%s)",
            currOdoPos, odoOffset, currPosAntenna.track, currPosAntenna.position, accuracyString);

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
      }

      return retVal;
    }

    /******************************************************************************
    * recalcOffset
    ******************************************************************************/
    void AbstractPosition::recalcOffset()
    {
      OdoPosition calOdo = 0;
      //check for front Odo value present in track list
      if (DS::AbstractTracks::corePtr()->getOdoPos(currPosAntenna, calOdo))
      {
        odoOffset = (calOdo - currOdoPos);
        updateCurrentOdoPos(odoOffset);

        //adjust the positions of the balises if they are not invalid.
        if(prevBaliseInfo.nidBG != invalidBaliseId)
        {
          prevBaliseInfo.odometerPos += odoOffset;
        }

        if(nextBaliseInfo.nidBG != invalidBaliseId)
        {
          nextBaliseInfo.odometerPos += odoOffset;
        }
      }
    }
  }
}

//lint +esym(586,snprintf)
