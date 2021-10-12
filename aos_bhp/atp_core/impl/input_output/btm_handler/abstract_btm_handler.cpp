/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The BTM Handler component deals with the interface between the OPC and the AOS SW.
*  AbstractBTMHandler implements the core functionality of the component.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-29   arastogi    Created
* 2016-09-20   saprasad    Write the dummy getBTMTelegram function to fixed
*                          linking error in decode component
* 2016-09-23   adgupta     Implementation for BTM Handler
* 2016-10-06   spandita    Fixed the bug for equal seq number
* 2016-10-12   arastogi    The check that sequence number are in order is done
*                          everytime not only when the list is not empty.
* 2016-12-05   rquensel    Added call to Cross Compare
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_math.hpp"
#include "abstract_btm_handler.hpp"
#include "abstract_config.hpp"
#include "abstract_cross_compare.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_loco_io.hpp"
#include "abstract_log_handler.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_odometry.hpp"
#include "abstract_position.hpp"
#include "atc_util.hpp"
#include "cross_compare_complex.hpp"
#include "dmi_event_codes.hpp"
#include "spl_handler.hpp"
#include "abstract_btm_handler_event_ids.hpp"

#include <stdio.h>
#include <vfw_checkpoints.h>

#ifdef __GNUG__
#include <vfw_time.h>
#else
#include <time.h>
extern "C" int64_t vfwGetReferenceTime();
extern "C" void vfwGetTimeOfDay(struct timespec * const timespec_p, struct tm * const tm_p);
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
  namespace IO
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractBTMHandler::AbstractBTMHandler() : ATC::IOComponent(atpBTMHandlerId, "BTMHandler", "BH"),
      // Creating different set of objects for different type of events
      errorSequenceOrBaliseNumber(ATC::Event::createSafetyHaltEvent(atpBTMHandlerId, ATC::CoreContainer, eventIdErrorSequenceOrBaliseNumber,
        ATC::NoEB, DMICom::btmIncorrectBal, "Inconsistent Balise/Sequence number received.")),
      eventRoutineTestInProgress(ATC::Event::createStandstillEvent(atpBTMHandlerId, ATC::CoreContainer, eventIdErrorRoutineTestInProgress,
        ATC::DispatcherSB, DMICom::btmRoutineTestInProg, "Routine Test in progress.")),
      eventRoutineTestMandatory(ATC::Event::createSBReqEvent(atpBTMHandlerId, ATC::CoreContainer, eventIdErrorRoutineTestMandatory,
        ATC::DispatcherSB, DMICom::btmRoutineTestMandatory, "Routine Test is mandatory.")),
      eventRoutineTestSucceded(ATC::Event::createLogEvent(atpBTMHandlerId, ATC::CoreContainer, eventIdErrorRoutineTestSucceded,
        DMICom::btmRoutineTestSucceded, "Routine Test succeeded.")),
      eventRoutineTestFailed(ATC::Event::createLogEvent(atpBTMHandlerId, ATC::CoreContainer, eventIdErrorRoutineTestFailed,
        DMICom::btmRoutineTestFailed, "Routine Test failed.")),
      eventMandatoryRoutineTestFailed(ATC::Event::createSafetyHaltEvent(atpBTMHandlerId, ATC::CoreContainer, eventIdErrorMandatoryRoutineTestFailed,
        ATC::NoEB, DMICom::btmMandatoryRoutineTestFailed, "Routine Test failed.")),
      eventErrorBtmSupervisionFailed(ATC::Event::createSafetyHaltEvent(atpBTMHandlerId, ATC::CoreContainer, eventIdErrorBtmSupervisionFailed,
        ATC::NoEB,  DMICom::btmFailed, "BTM Supervision failed: ", true)),
      eventIncorrectBTMTelegramFormat(ATC::Event::createLogEvent(atpBTMHandlerId,ATC::CoreContainer, eventIdErrorTelegramInvalid,
        DMICom::btmFailed, "BTM Supervision failed, incorrect Telegram format: " ,true)),
      eventBalisePresentAtStandstill(ATC::Event::createLogEvent(atpBTMHandlerId, ATC::CoreContainer, eventIdErrorBrakeUntilStandstill,
        DMICom::btmBaliseDetectedAtStandstill, "Balise detected at Standstill.")),
      eventBtmCouldBeFaulty(ATC::Event::createLogEvent(atpBTMHandlerId, ATC::CoreContainer, eventIdErrorCouldBeFaulty,
        DMICom::btmReadingCouldBeFaulty, "BTM reading could be faulty.")),
      eventBtmAvailabilityIsGreen(ATC::Event::createLogEvent(atpBTMHandlerId, ATC::CoreContainer, eventIdErrorStausIsGreen,
        DMICom::btmAvailabilityIsGreen, "BTM availability is green.")),
      eventSporadicErrorOccurred(ATC::Event::createSafetyHaltEvent(atpBTMHandlerId, ATC::CoreContainer, eventIdErrorSporadicError,
        ATC::NoEB, DMICom::btmSporadicErrorOccurred, "BTM sporadic error occurred.")),
      informDriverFailedBtm(ATC::Event::createLogEvent(atpBTMHandlerId, ATC::CoreContainer, eventIdErrorFailedBtm,
        DMICom::btmMalfunctioning, "BTM Malfunctioning!", true)),
      antennaPowerState(BtmAntennaOff),
      routineTestState(RoutineTestOK),
      lastTgmSequenceNum(0U),
      initDone(false),
      eventBtmCouldBeFaultyReported(false),
      timeDirControllerInNeutral(0),
      timeTelePoweringChanged(0),
      timeBalisePresent(0),
      timeLastRoutineTestPassed(0U),
      isRoutineTestPossible(false),
      opcReferenceTimeOffset(ATC::int32Max),
      previousTimeStamp(ATC::uint32Max),
      baliseServiceState(Startup),
      remainingTimeToMandatoryRoutineTest(0U),
      balisePresentAtStandStill(false),
      balisePresentAtStandStillPos(0),
      forceStopAndTest(false),
      splhConnected(false),
      postponeTest(false),
      commandTelegramSentTimeout(0),
      baliseServiceAvailable(0U),
      bsaCounter(0U),
      sporadicErrorTimeout(0),
      routineTestTimeout(0),
      informDriverFailedBtmSent(false),
      isTelePoweringMismatching(false)
    {
      if (coreBTMHandlerInstancePtr != NULL)
      {
        // Error to event handler
        ATC::aosHalt(__FILE__, __LINE__, "BTM Handler Constructor already instantiated");
      }
      else
      {
        // Setup single instance pointer for core access
        coreBTMHandlerInstancePtr = this;
      }
    }

    /******************************************************************************
    * getMaxDistanceFromStandStill
    ******************************************************************************/
    OdoPosition AbstractBTMHandler::getMaxDistanceFromStandStill()
    {
      return maxDistanceFromStandStill;
    }

    /******************************************************************************
    * preInit
    ******************************************************************************/
    void AbstractBTMHandler::preInit()
    {
      SplHandler::instance().preInit();
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractBTMHandler::init()
    {
      if (!initDone)
      {
        initDone = true;
        // Initialize values for cross compare for BTM Handler
        initCrossCompare();

        // Run init on SPLH
        SplHandler::instance().init();

        // Set the time when the last runtime test was performed to the runtime parameter
        timeLastRoutineTestPassed = AbstractConfig::corePtr()->getLastBTMTestTime();
      }

      return initDone;
    }

    /******************************************************************************
    * runIn
    ******************************************************************************/
    void AbstractBTMHandler::runIn()
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "BH_runIn");

      if (!btmTgmList.empty())
      {
        setBaliseServiceFailed("btmTgmList not empty");
      }

      SplHandler& splh = SplHandler::instance();

      splh.runIn();

      if (splh.isConnected())
      {
        splhConnected = true;
      }
      else if (splhConnected)
      {
        setBaliseServiceFailed("SPL not connected");
      }
      else
      {
        // Do nothing...
      }
    }

    /******************************************************************************
    * runOut
    ******************************************************************************/
    void AbstractBTMHandler::runOut()
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "BH_runOut");

      SplHandler& splh = SplHandler::instance();

      if (splh.isConnected())
      {
        if (splh.opcIsSystemRunning())
        {
          // 1. Run the BTM antenna power state machine
          runAntennaPowerStateMachine();

          // 2. Run the BTM routine test state machine
          runRoutineTestStateMachine();

          // 3. Send the BTM command message
          populateAndSendBtmCommand();
        }

        // 4. Send the BTM Odo message
        populateAndSendOdoCommand();
      }

      // Run runOut in SPLH
      splh.runOut();
    }


    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractBTMHandler* AbstractBTMHandler::corePtr()
    {
      return coreBTMHandlerInstancePtr;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractBTMHandler::initCrossCompare() const
    {
      // Initializing of the Cross compare data variables stored more than one cycle.

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&errorSequenceOrBaliseNumber));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventRoutineTestInProgress));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventRoutineTestMandatory));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventRoutineTestSucceded));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventRoutineTestFailed));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventMandatoryRoutineTestFailed));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventErrorBtmSupervisionFailed));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventIncorrectBTMTelegramFormat));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventBalisePresentAtStandstill));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventBtmCouldBeFaulty));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventBtmAvailabilityIsGreen));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&eventSporadicErrorOccurred));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&informDriverFailedBtm));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<BtmAntennaPowerState>(&antennaPowerState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<BtmRoutineTestState>(&routineTestState));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&lastTgmSequenceNum));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&initDone));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&eventBtmCouldBeFaultyReported));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&timeDirControllerInNeutral));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&timeTelePoweringChanged));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&timeBalisePresent));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&timeLastRoutineTestPassed));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&isRoutineTestPossible));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&opcReferenceTimeOffset));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&previousTimeStamp));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<BtmServiceState>(&baliseServiceState));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&remainingTimeToMandatoryRoutineTest));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&balisePresentAtStandStill));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt32(&balisePresentAtStandStillPos));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&forceStopAndTest));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&splhConnected));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&postponeTest));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&commandTelegramSentTimeout));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&baliseServiceAvailable));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&bsaCounter));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&sporadicErrorTimeout));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&routineTestTimeout));

      btmStatusMsg.initCrossCompare();
      btmCmdMsg.initCrossCompare();
    }

    
    /******************************************************************************
    * setBaliseServiceFailed
    ******************************************************************************/
    void AbstractBTMHandler::setBaliseServiceFailed(const char_t* const errorMessage)
    {
      if (baliseServiceState != ServiceFailed)
      {
        baliseServiceState = ServiceFailed;
        eventErrorBtmSupervisionFailed.setDynamicText(errorMessage);
        ATC::AbstractEventHandler::corePtr()->reportEvent(eventErrorBtmSupervisionFailed, __FILE__, __LINE__);
      }
    }


    /******************************************************************************
    * startRoutineTest
    ******************************************************************************/
    void AbstractBTMHandler::startRoutineTest()
    {
      routineTestState = RoutineTestStarted;
      remainingTimeToMandatoryRoutineTest = 0U;
      routineTestTimeout = vfwGetReferenceTime() + timeoutValueRoutineTest;
      btmCmdMsg.setPerformRoutineTest();
    }


    /******************************************************************************
    * routineTestFailed
    ******************************************************************************/
    void AbstractBTMHandler::routineTestFailed(const uint32_t timeSinceLastPassTest)
    {
      // Test failed!
      const uint32_t mandatoryBtmTestTimeValue = 60U * static_cast<uint32_t>(AbstractConfig::corePtr()->getBTMTestMandatory());
      routineTestTimeout = 0;

      // Check if it was a mandatory test
      if (timeSinceLastPassTest > mandatoryBtmTestTimeValue)
      {
        routineTestState = RoutineTestMandatory;
        postponeTest = false;
        // Send the safety halt event about the failed routine test...
        ATC::AbstractEventHandler::corePtr()->reportEvent(eventMandatoryRoutineTestFailed, __FILE__, __LINE__);
      }
      else
      {
        // If the BTM routine test fails a log event shall be issued AND the BTM routine test shall be postponed until
        // next time when the conditions are fulfilled.
        routineTestState = RoutineTestNeeded;
        postponeTest = true;
        ATC::AbstractEventHandler::corePtr()->reportEvent(eventRoutineTestFailed, __FILE__, __LINE__);
      }

    }

    /******************************************************************************
    * processTelegramMessage
    ******************************************************************************/
    void AbstractBTMHandler::processTelegramMessage(VFW_Buffer& vfwParseBuffer)
    {
      // Msg is still valid till here? Go for unpacking
      const uint8_t prevTgmSeqNumber = lastTgmSequenceNum;
      // Unpack 5 ports data, update data variables, populate btmPacket
      bool telegramValid = false;

      if (lastReceivedBaliseTelegram.unpack(vfwParseBuffer, lastTgmSequenceNum, telegramValid))
      {
        if (telegramValid)
        {
          if (lastReceivedBaliseTelegram.getFinalReport() && (prevTgmSeqNumber != lastTgmSequenceNum))
          {
            if (!btmTgmList.full())
            {
              btmTgmList.pushBack(lastReceivedBaliseTelegram);
            }
            else
            {
              // Error!
              setBaliseServiceFailed("btmTgmList full");
            }
          }
        }
        else
        {
          if (antennaPowerState != BtmAntennaOff)
          {
            // But only go to safety halt if telepowering is on
            setBaliseServiceFailed("Antenna not off");
          }
          else if (!informDriverFailedBtmSent)
          {
            informDriverFailedBtmSent = true;
            ATC::AbstractEventHandler::corePtr()->reportEvent(informDriverFailedBtm, __FILE__, __LINE__);
          }
          else
          {
            // Do nothing...
          }
        }
      }
      else
      {
        // Report safety halt event
        ATC::AbstractEventHandler::corePtr()->reportEvent(errorSequenceOrBaliseNumber, __FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * processStatusMessage
    ******************************************************************************/
    void AbstractBTMHandler::processStatusMessage(VFW_Buffer& vfwParseBuffer)
    {
      const uint8_t nidBtm = vfwGetU8(&vfwParseBuffer);  // NID_BTM
      const uint8_t qStatus = vfwGetU8(&vfwParseBuffer); // Q_ITEM_STATUS

      if (qStatus == itemSetBySource)
      {
        if (nidBtm == 1U) // We only have one BTM
        {
          GP_10ByteVitalSinkDataA statusData;

          // Reset the data to 0
          memset(&statusData, 0, sizeof(statusData));

          // See chapter 4.5.24.2 GP_10ByteVitalSinkDataA in 3NSS000250S0021 
          // safteyData[10]  uint8
          // telegramValid   boolA (enum boolA { trueA = 1, falseA = 0 })
          // telegramStatus  tios_FailureInformationA
          // safetyTimeStamp uint32

          vfwCpyToRawBuffer(&(statusData.safetyData[0]), &vfwParseBuffer, sizeof(statusData.safetyData));

          const bool telegramValid = (vfwGetU8(&vfwParseBuffer) == 1U);
          const uint8_t telegramStatus = vfwGetU8(&vfwParseBuffer);

          const uint32_t safetyTimeStamp = vfwGetU32(&vfwParseBuffer);

          if (telegramValid)
          {
            btmStatusMsg.unpack(statusData);
            trace.write(ATC::briefTrace, "BTM Status message safety timestamp", safetyTimeStamp);
            handleBtmStatusMessage();
          }
          else
          {
            // Always log
            writeToLog(ATC::BriefLog, "TelegramValid field is false, status: ", static_cast<int32_t>(telegramStatus), __FILE__, __LINE__);

            if (antennaPowerState != BtmAntennaOff)
            {
              // But only go to safety halt if telepowering is on
              setBaliseServiceFailed("Antenna not off");
            }
            else if (!informDriverFailedBtmSent)
            {
              informDriverFailedBtmSent = true;
              ATC::AbstractEventHandler::corePtr()->reportEvent(informDriverFailedBtm, __FILE__, __LINE__);
            }
            else
            {
              // Do nothing...
            }
          }

          if (vfwGetValidSize(&vfwParseBuffer) != 0U)
          {
            writeToLog(ATC::BriefLog, "Invalid length of Status message received", __FILE__, __LINE__);
            trace.write(ATC::briefTrace, "Invalid length of status message received");
          }
        }
        else if (nidBtm == 2U)
        {
          // nidBtm 2 is OK, but not used
        }
        else
        {
          writeToLog(ATC::BriefLog, "Invalid BTM message. id ", static_cast<uint32_t>(nidBtm), __FILE__, __LINE__);
          writeToLog(ATC::BriefLog, "Q_STATUS ", static_cast<uint32_t>(qStatus), __FILE__, __LINE__);
        }
      }
      else
      {
        writeToLog(ATC::BriefLog, "BTM status message not valid", __FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * handleBtmStatusMessage
    ******************************************************************************/
    void AbstractBTMHandler::handleBtmStatusMessage()
    {
      const uint8_t newBsaCounter = btmStatusMsg.getBsaCounter();
      const uint8_t newBaliseServiceAvailable = btmStatusMsg.getBaliseServiceAvailable();
      const int64_t currentTime = vfwGetReferenceTime();

      if (btmStatusMsg.isBalisePresent())
      {
        timeBalisePresent = currentTime;
      }

      if ((baliseServiceState == ServiceOK) && (antennaPowerState == BtmAntennaOn) &&
          ((currentTime - timeTelePoweringChanged) > timeoutValueCommandSupervision))
      {
        // BSA Counter shall be incremented by one each time a change of status of Balise Service Available is reported.
        const uint8_t numberOfChangedSteps = newBsaCounter - bsaCounter;

        if (((numberOfChangedSteps == 0U) || (numberOfChangedSteps == 2U) || (numberOfChangedSteps == 4U))  &&
          (newBaliseServiceAvailable == baliseServiceAvailable))
        {
          // OK, even number of changes, we keep the value
        }
        else if (((numberOfChangedSteps == 1U) || (numberOfChangedSteps == 3U))  &&
          (newBaliseServiceAvailable != baliseServiceAvailable))
        {
          // OK, odd number of changes, value must change
        }
        else
        {
          baliseServiceState = ServiceFailed;

          char_t  buff[100];
          //lint -e{586} snprintf is needed here
          const int32_t ret = snprintf(&buff[0], sizeof(buff), " (%d %d %d)",
            static_cast<int32_t>(newBaliseServiceAvailable), static_cast<int32_t>(bsaCounter), static_cast<int32_t>(newBsaCounter));

          if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buff)))
          {
            eventErrorBtmSupervisionFailed.setDynamicText(&buff[0]);
          }
          ATC::AbstractEventHandler::corePtr()->reportEvent(eventErrorBtmSupervisionFailed, __FILE__, __LINE__);
        }
      }

      if (baliseServiceAvailable != newBaliseServiceAvailable)
      {
        trace.write(ATC::briefMessageTrace, "BSA change from: ", static_cast<int32_t>(baliseServiceAvailable));
        trace.write(ATC::briefMessageTrace, "BSA change to: ", static_cast<int32_t>(newBaliseServiceAvailable));
        baliseServiceAvailable = newBaliseServiceAvailable;
      }

      if (bsaCounter != newBsaCounter)
      {
        trace.write(ATC::briefMessageTrace, "BSA counter from: ", static_cast<int32_t>(bsaCounter));
        trace.write(ATC::briefMessageTrace, "BSA counter to: ", static_cast<int32_t>(newBsaCounter));
        bsaCounter = newBsaCounter;
      }
    }

    /******************************************************************************
    * setOpcReferenceTimeOffset
    ******************************************************************************/
    void AbstractBTMHandler::setOpcReferenceTimeOffset(const int32_t offset)
    {
      opcReferenceTimeOffset = offset;
    }


    /******************************************************************************
    * getOpcReferenceTimeOffset
    ******************************************************************************/
    int32_t AbstractBTMHandler::getOpcReferenceTimeOffset() const
    {
      return opcReferenceTimeOffset;
    }

    /******************************************************************************
    * getIsRoutineTestMandatory
    ******************************************************************************/
    bool AbstractBTMHandler::getIsRoutineTestMandatory() const
    {
      return (routineTestState == RoutineTestMandatory);
    }

    /******************************************************************************
    * getIsRoutineTestNeeded
    ******************************************************************************/
    bool AbstractBTMHandler::getIsRoutineTestNeeded() const
    {
      return  (routineTestState == RoutineTestNeeded);
    }

    /******************************************************************************
    * getIsRoutineTestPossible
    ******************************************************************************/
    bool AbstractBTMHandler::getIsRoutineTestPossible() const
    {
      return isRoutineTestPossible;
    }
    
    /******************************************************************************
    * getRemainingTimeToMandatoryRoutineTest
    ******************************************************************************/
    uint16_t AbstractBTMHandler::getRemainingTimeToMandatoryRoutineTest() const
    {
      return remainingTimeToMandatoryRoutineTest;
    }

    /******************************************************************************
    * updateBtmCalendarTime
    ******************************************************************************/
    void AbstractBTMHandler::updateBtmCalendarTime(const uint32_t calendarTime) const
    {
      SplHandler::instance().packAtpServiceCalendarTime(calendarTime);
    }
    
    /******************************************************************************
    * getOpcVersionString
    ******************************************************************************/
    const OpcVersionString& AbstractBTMHandler::getOpcVersionString() const
    {
      return SplHandler::instance().getOpcVersionString();
    }

    /******************************************************************************
    * validateOpcVersion
    ******************************************************************************/
    bool AbstractBTMHandler::validateOpcVersion(const OpcVersionString& opcVersion) const
    {
      uint32_t majorVersion = 0U;
      uint32_t middleVersion = 0U;
      uint32_t minorVersion = 0U;

      const uint32_t ignoreMinor = 255U;

      uint32_t expectedMajorVersion = AbstractConfig::corePtr()->getExpectedOpcMajorVersion();
      uint32_t expectedMiddleVersion = AbstractConfig::corePtr()->getExpectedOpcMiddleVersion();
      uint32_t expectedMinorVersion = AbstractConfig::corePtr()->getExpectedOpcMinorVersion();

      const int32_t numScanned = sscanf(&opcVersion.versionString[0], "%u.%u.%u", &majorVersion, &middleVersion, &minorVersion);

      bool result = false;

      if (numScanned == 3)
      {
        result = (majorVersion == expectedMajorVersion) &&
          (middleVersion == expectedMiddleVersion) &&
          ((minorVersion == expectedMinorVersion) || (expectedMinorVersion == ignoreMinor));
      }

      if (!result)
      {
        char_t buffer[100];
        //lint -e{586} snprintf is needed here
        const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "OPC version incorrect: %s, accepted version is: %d.%d.%d",
          opcVersion.versionString, expectedMajorVersion, expectedMiddleVersion, expectedMinorVersion);

        if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          writeToLog(ATC::BriefLog, &buffer[0], __FILE__, __LINE__);
        }
      }

      return result;
    }

    /******************************************************************************
    * runAntennaPowerStateMachine
    ******************************************************************************/
    void AbstractBTMHandler::runAntennaPowerStateMachine()
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "BH_runBTMAntennaPowerStateMachine");

      // Get all the parameters to determine the State Machine

      // Get mode from Mode control
      const ATPMode mode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      // Get cabin active from Mode control      
      const CabActiveStatus cabStatus = Kernel::AbstractModeControl::corePtr()->getActiveCab();
      const bool cabinActive = (CabAActive == cabStatus) || (CabBActive == cabStatus);
      const bool isDirectionControllerNeutral = (AbstractLocoIO::corePtr()->getLocoDirection() == DirNone);
      const bool overheatSignal = btmStatusMsg.getBTMOverheatingWarningStatus();
      const int64_t currentTime = vfwGetReferenceTime();

      const uint8_t telePoweringStatusBits = btmStatusMsg.getTelepoweringStatus();
      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

      const bool activeRoutineTest = (routineTestState == RoutineTestInProgress) || (routineTestState == RoutineTestStarted);

      const bool readBalisesInThisMode = !(
          (mode == ATPModePowerUp)      ||
          (mode == ATPModePoweringDown) ||
          (mode == ATPModeSleeping)     ||
          (mode == ATPModeSafetyHalt)
        );

      const bool telePoweringStatus = (telePoweringStatusBits == telePoweringOn);

      if (telePoweringStatusBits > telePoweringOn)
      {
        // Tele powering invalid
        ATC::AbstractEventHandler::corePtr()->reportEvent(informDriverFailedBtm, __FILE__, __LINE__);
      }

      if (isDirectionControllerNeutral  &&  isStandStill)
      {
        // Set the time when we first are in neutral
        if (timeDirControllerInNeutral == 0)
        {
          timeDirControllerInNeutral = currentTime + timeoutValueDirectionController;
        }
      }
      else
      {
        timeDirControllerInNeutral = 0;
      }

      // Has the timeout started, and has it expired?
      if ((commandTelegramSentTimeout != 0) && (currentTime > commandTelegramSentTimeout))
      {
        // Supervise the options from the BTM status message, they should after the timeout be the same as the ones sent in the command message.
        const uint8_t btmOptions = btmStatusMsg.getBtmOptions();
        if (btmOptions != btmStatusMessageOptions)
        {
          writeToLog(ATC::BriefLog, "BTM options incorrect: ", static_cast<int32_t>(btmOptions));

          if (antennaPowerState != BtmAntennaOff)
          {
            setBaliseServiceFailed("Antenna not off");
          }
          else
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(informDriverFailedBtm, __FILE__, __LINE__);
          }
        }

        const uint8_t enabledTelegramFormat = btmStatusMsg.getEnabledTelegramFormat();
        if (enabledTelegramFormat != telegramFormat)
        {
          if (antennaPowerState == BtmAntennaOff)
          {
            eventIncorrectBTMTelegramFormat.setDynamicText(static_cast<uint32_t>(enabledTelegramFormat));
            ATC::AbstractEventHandler::corePtr()->reportEvent(eventIncorrectBTMTelegramFormat, __FILE__, __LINE__);
          }
          else
          {
            setBaliseServiceFailed("Antenna not off");
          }
        }
      }

      if (baliseServiceState == ServiceOK)
      {
        switch (antennaPowerState)
        {
        case BtmAntennaOn:
        {
          // Check Tele Powering status
          if ((currentTime - timeTelePoweringChanged) > timeoutValueCommandSupervision)
          {
            if (!telePoweringStatus)
            {
              if (!isTelePoweringMismatching)
              {
                // Tele powering not matching 
                isTelePoweringMismatching = true;
                informDriverFailedBtm.setDynamicText(" Tele powering status is off.");
                ATC::AbstractEventHandler::corePtr()->reportEvent(informDriverFailedBtm, __FILE__, __LINE__);
              }
            }

            // Check the preliminary availability flag
            const uint8_t preliminaryAvailabilityTestStatus = btmStatusMsg.getPreliminaryAvailabilityTestStatus();

            if (preliminaryAvailabilityTestStatus == btmPreliminaryBtmStatusTestInProgress)
            {
              // Do nothing
            }
            else if (preliminaryAvailabilityTestStatus != btmPreliminaryBtmStatusGreen)
            {
              if (!eventBtmCouldBeFaultyReported)
              {
                ATC::AbstractEventHandler::corePtr()->reportEvent(eventBtmCouldBeFaulty, __FILE__, __LINE__);
                eventBtmCouldBeFaultyReported = true;
              }
            }
            else if (eventBtmCouldBeFaultyReported)
            {
              eventBtmCouldBeFaultyReported = false;
              ATC::AbstractEventHandler::corePtr()->reportEvent(eventBtmAvailabilityIsGreen, __FILE__, __LINE__);
            }
            else
            {
              // Do nothing...
            }
          }
          else
          {
            isTelePoweringMismatching = false;
          }

          const bool isBalisePresentTimeout = ((currentTime - timeBalisePresent) > timeoutValueBalisePresent);
          const bool isDirectionControllerNeutralTimeout = (currentTime > timeDirControllerInNeutral);

          if ((!readBalisesInThisMode) || overheatSignal || activeRoutineTest ||
            (isBalisePresentTimeout && isStandStill &&
              isDirectionControllerNeutral && isDirectionControllerNeutralTimeout))
          {
            // Change to Antenna Off State
            antennaPowerState = BtmAntennaOff;
            timeTelePoweringChanged = currentTime;
          }

          if (baliseServiceAvailable == bsaBaliseServiceAvailable) // (TRUE), everything is OK
          {
            // Everything is OK
            sporadicErrorTimeout = 0;
          }
          else if (baliseServiceAvailable == bsaBtmStartUpOrRoutineTestInProgress)
          {
            if (currentTime > routineTestTimeout)
            {
              // Error!
              setBaliseServiceFailed("BTM test timed out");
            }
          }
          else if (baliseServiceAvailable == bsaSporadicFailure) // (false), sporadic error)
          {
            if (sporadicErrorTimeout == 0)
            {
              sporadicErrorTimeout = currentTime + timeoutValueCommandSupervision;
            }
            else if (currentTime > sporadicErrorTimeout)
            {
              // Also called temporary error in requirements, but BTM IFS says sporadic.
              ATC::AbstractEventHandler::corePtr()->reportEvent(eventSporadicErrorOccurred, __FILE__, __LINE__);
            }
            else
            {
              // Just wait for the timeout...
            }
          }
          else
          {
            // Error!
            setBaliseServiceFailed("Wrong service state");
          }
          break;
        }

        case BtmAntennaOff:
          sporadicErrorTimeout = 0;
          if ((currentTime - timeTelePoweringChanged) > timeoutValueCommandSupervision)
          {
            if (telePoweringStatus)
            {
              if (!isTelePoweringMismatching)
              {
                isTelePoweringMismatching = true;
                // Telepowering not matching
                informDriverFailedBtm.setDynamicText(" Tele powering status is On.");
                ATC::AbstractEventHandler::corePtr()->reportEvent(informDriverFailedBtm, __FILE__, __LINE__);
              }
            }
          }
          else
          {
            isTelePoweringMismatching = false;
          }

          if (readBalisesInThisMode && (!overheatSignal) &&
            (!activeRoutineTest) &&
            ((!isStandStill) || (cabinActive && (!isDirectionControllerNeutral))))
          {
            // Change to Antenna On State
            antennaPowerState = BtmAntennaOn;
            timeTelePoweringChanged = currentTime;
          }

          if (baliseServiceAvailable == bsaBaliseServiceAvailable)
          {
            if ((currentTime - timeTelePoweringChanged) > timeoutValueCommandSupervision)
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(informDriverFailedBtm, __FILE__, __LINE__);
            }
          }
          else if (baliseServiceAvailable == bsaBtmStartUpOrRoutineTestInProgress)
          {
            if (currentTime > routineTestTimeout)
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(informDriverFailedBtm, __FILE__, __LINE__);
            }
          }
          else if (baliseServiceAvailable == bsaSporadicFailure) // (false), sporadic error)
          {
            // This is the default state if no tele power
          }
          else
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(informDriverFailedBtm, __FILE__, __LINE__);
          }
          break;

        default:
          // Should never occur.
          antennaPowerState = BtmAntennaOff;
          setBaliseServiceFailed("Wrong power state");
          break;
        }// End of switch(antennaPowerState)
      } // baliseServiceState == ServiceOK
    }


    /******************************************************************************
    * runRoutineTestStateMachine
    ******************************************************************************/
    void AbstractBTMHandler::runRoutineTestStateMachine()
    {
      // Get all the parameters to determine the Routine Test State Machine

      // Get the current time
      struct timespec currentTimeSpec;
      vfwGetTimeOfDay(&currentTimeSpec, static_cast<tm *>(NULL));

      const uint32_t curTime = static_cast<uint32_t>(currentTimeSpec.tv_sec);

      // Time since last test passed (in seconds)
      uint32_t timeSinceLastPassTest = curTime - timeLastRoutineTestPassed;

      // Is a balise present?
      const bool isBalisePresent = btmStatusMsg.isBalisePresent();

      // Any Cabin active?
      const CabActiveStatus currentCabinStatus = Kernel::AbstractModeControl::corePtr()->getActiveCab();
      const CabActiveStatus previousCabinStatus = Kernel::AbstractModeControl::corePtr()->getPrevActiveCab();
      const bool cabinActive = (currentCabinStatus != NoCabActive);

      const ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();

      const Kernel::LocationModeState locationModeState = Kernel::AbstractModeControl::corePtr()->getLocationModeState();
      const bool freeRollingInUnloadLocation = Kernel::AbstractModeControl::corePtr()->getFreeRolling()
        && (currentMode == ATPModeLocation) && (locationModeState == Kernel::LocationMode::locationUnloadLocation);

      // At cabin activation in all ATP modes except for Safety Halt, Powering Down, Sleeping and Location mode type unload with Free Rolling status set
      const bool cabinActivation = (currentCabinStatus != previousCabinStatus) && cabinActive && (
        (currentMode != ATPModePoweringDown) && (currentMode != ATPModeSafetyHalt) && (currentMode != ATPModeSleeping) && (!freeRollingInUnloadLocation));

      const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

      if (btmStatusMsg.isInitialized()  &&  (baliseServiceState == Startup))
      {
        if (baliseServiceAvailable != bsaBtmStartUpOrRoutineTestInProgress)
        {
          const OpcVersionString& opcVersion = SplHandler::instance().getOpcVersionString();
          if (validateOpcVersion(opcVersion))
          {
            baliseServiceState = ServiceOK;
          }
          else
          {
            setBaliseServiceFailed("Wrong OPC version");
          }
        }
      }

      switch (routineTestState)
      {
        case RoutineTestOK:
          if (timeSinceLastPassTest > (60U * (static_cast<uint32_t>(AbstractConfig::corePtr()->getBTMTestNotify()))))
          {
            routineTestState = RoutineTestNeeded;
          }
          break;

        case RoutineTestNeeded:
        {
          // Check if State can be moved to in progress. Else, check if should be moved to test mandatory state          
          const bool trainStateIdle = Kernel::AbstractModeControl::corePtr()->getIdleState();

          // First check if routine test is possible, set the routine test possible flag
          isRoutineTestPossible = cabinActive && (!isBalisePresent) && isStandStill && (baliseServiceState == ServiceOK);

          // ATP mode Normal, Staff Responsible OR Shunting Route 
          const bool performAutomaticTest = cabinActivation || (
            trainStateIdle && ((currentMode == ATPModeNormal) || (currentMode == ATPModeStaffResponsible) || (currentMode == ATPModeShuntingRoute)));

          if (!performAutomaticTest)
          {
            postponeTest = false;
          }

          // Get the DMI Button Status...
          const DMICom::DMIButtonStatus dmiButtonStatus = DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus();
          // Check if driver pressed the routine button
          const bool routineTestRequested = (dmiButtonStatus == DMICom::DMIButtonStartBtmTest);

          if (((performAutomaticTest && (!postponeTest)) || routineTestRequested) && isRoutineTestPossible)
          {
            // Routine test will be started if Routine test state moves to In progress and it should remain there till the test is in progress.
            // The routine state should be updated when the test completes and status returns the result.
            isRoutineTestPossible = false;
            startRoutineTest();
          }
          else
          {
            const uint32_t mandatoryBtmTestTimeValue = 60U * static_cast<uint32_t>(AbstractConfig::corePtr()->getBTMTestMandatory());

            if (timeSinceLastPassTest > mandatoryBtmTestTimeValue)
            {
              // Leave the state for Routine test mandatory
              isRoutineTestPossible = false;
              remainingTimeToMandatoryRoutineTest = 0U;
              routineTestState = RoutineTestMandatory;
            }
            else
            {
              // No Change, remain in Test Needed. Update the remaining time to mandatory test...
              remainingTimeToMandatoryRoutineTest = static_cast<uint16_t>(mandatoryBtmTestTimeValue - timeSinceLastPassTest) / 60U;
            }
          }
          break;
        }

        case RoutineTestStarted:
          // Wait until the BTM reports that the routine test is running.
          if (baliseServiceAvailable == bsaBtmStartUpOrRoutineTestInProgress)
          {
            routineTestState = RoutineTestInProgress;
          }
          else if (vfwGetReferenceTime() > routineTestTimeout)
          {
            routineTestFailed(timeSinceLastPassTest);
          }
          else
          {
            // Just wait for the test to finish (or timeout)
          }

          // Raise Standstill event as movement is not allowed.
          ATC::AbstractEventHandler::corePtr()->reportEvent(eventRoutineTestInProgress, __FILE__, __LINE__);
          break;

        case RoutineTestInProgress:
          if (baliseServiceAvailable == bsaBtmStartUpOrRoutineTestInProgress)
          {
            if (vfwGetReferenceTime() > routineTestTimeout)
            {
              routineTestFailed(timeSinceLastPassTest);
            }
            else
            {
              // Still the test is in progress. No change in state.
              // Raise Standstill event as movement is not allowed.
              ATC::AbstractEventHandler::corePtr()->reportEvent(eventRoutineTestInProgress, __FILE__, __LINE__);
            }
          }
          else
          {
            // Move the State to Test OK if Test passes successfully.
            if (btmStatusMsg.isBtmTestOk())
            {
              routineTestState = RoutineTestOK;
              timeLastRoutineTestPassed = curTime; // Update the last passed time
              postponeTest = false;

              // Send the event...
              ATC::AbstractEventHandler::corePtr()->reportEvent(eventRoutineTestSucceded, __FILE__, __LINE__);
              
              // Write the BTM Routine Test time to NVSH
              if (!AbstractConfig::corePtr()->setLastBTMTestTime(curTime))
              {
                trace.write(ATC::briefMessageTrace, "Last BTM Test value not set properly");
                writeToLog(ATC::BriefLog, "Last BTM Test value not set properly", __FILE__, __LINE__);
              }
              else
              {
                trace.write(ATC::briefMessageTrace, "BTM Routine Test OK!");
                writeToLog(ATC::BriefLog, "BTM Routine Test OK!", __FILE__, __LINE__);
              }
            }
            else
            {
              routineTestFailed(timeSinceLastPassTest);
            }
          }
          break;

        case RoutineTestMandatory:
          if ((currentMode != ATPModeSafetyHalt)  &&
              (currentMode != ATPModePoweringDown) &&
              (currentMode != ATPModeSleeping)        )
          {
            if (!isStandStill)
            {
              if (balisePresentAtStandStill)
              {
                const OdoPosition maxDistFromStandStill = getMaxDistanceFromStandStill();
                const OdoPosition distanceFromStandStill = ATC::ATCMath::instance().absolute(
                  Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo() - balisePresentAtStandStillPos,
                  __FILE__, __LINE__);

                if (distanceFromStandStill > maxDistFromStandStill)
                {
                  forceStopAndTest = true;
                }
              }

              // Should we brake?
              if (balisePresentAtStandStill &&
                  isBalisePresent &&
                  (!forceStopAndTest))
              {
                // We allow the driver to move the train until a balise is not present
              }
              else
              {
                balisePresentAtStandStill = false;
                // Apply SB
                ATC::AbstractEventHandler::corePtr()->reportEvent(eventRoutineTestMandatory, __FILE__, __LINE__);
              }
            }
            else if ((!isBalisePresent) || forceStopAndTest)
            {
              balisePresentAtStandStill = false;
              forceStopAndTest = false;
              if (cabinActive && (baliseServiceState == ServiceOK))
              {
                startRoutineTest();
              }
            }
            else if (!balisePresentAtStandStill)
            {
              // We are at standstill, and a balise is present...
              // Inform driver to move the train...
              ATC::AbstractEventHandler::corePtr()->reportEvent(eventBalisePresentAtStandstill, __FILE__, __LINE__);
              balisePresentAtStandStill = true;
              balisePresentAtStandStillPos = Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo();
            }
            else
            {
              // Driver already informed, just do nothing...
            }
          }
          break;

        default:
          // This should never occur
          routineTestState = RoutineTestNeeded;
          break;
      }// End of switch (routineTestState)

    }


    /******************************************************************************
    * isBTMServiceAvailable
    ******************************************************************************/
    bool AbstractBTMHandler::isBTMServiceAvailable() const
    {
      return (baliseServiceState == ServiceOK);
    }


    /******************************************************************************
    * getBTMTelegram
    ******************************************************************************/
    bool AbstractBTMHandler::getBTMTelegram(BtmTelegram& telegram)
    {
      bool retVal = false;

      if (!btmTgmList.empty())
      {
        telegram = btmTgmList.front();
        btmTgmList.popFront();
        retVal = true;
      }

      return retVal;
    }

    /******************************************************************************
    * populateAndSendBtmCommand
    ******************************************************************************/
    void AbstractBTMHandler::populateAndSendBtmCommand()
    {
      bool    state;

      // Populate antenna Power state

      switch (antennaPowerState)
      {
        case BtmAntennaOff:
        {
          state = false;
          break;
        }

        case BtmAntennaOn:
        {
          state = true;
          break;
        }

        default:
        {
          // This should never occur
          state = false;
          break;
        }
      }

      // Have we sent the telegram enable before?
      if (commandTelegramSentTimeout == 0)
      {
        // Use this for supervision of the reply in the status message
        commandTelegramSentTimeout = vfwGetReferenceTime() + timeoutValueCommandSupervision;
      }
      btmCmdMsg.setTelePoweringCmd(state);
      btmCmdMsg.setBtmOptions(btmCommandOptions);

      // /All needed variables set. Pack now!
      GP_10ByteVitalSourceDataA commandData;
      memset(&commandData, 0, sizeof(commandData));

      //Check is there any change in the BTM Command message or not
      if (btmCmdMsg.pack(commandData))
      {
        SplHandler::instance().packBtmhToBtmPacket(commandData);
      }
    }

    /******************************************************************************
    * populateAndSendOdoCommand
    ******************************************************************************/
    void AbstractBTMHandler::populateAndSendOdoCommand()
    {
      const Pos::AbstractOdometry::OdoData& odoData = Pos::AbstractOdometry::corePtr()->getOdometerData();
      const uint16_t maxAccelComp = Pos::AbstractOdometry::corePtr()->getMaxAccelComp();

      // Only start sending after receiving the offset time value...
      if ((opcReferenceTimeOffset != ATC::int32Max)  &&
          (previousTimeStamp != odoData.timestamp))
      {
        previousTimeStamp = odoData.timestamp;

        GP_32ByteVitalSourceDataA odoH2OpcData;
        memset(&odoH2OpcData, 0, sizeof(odoH2OpcData));

        VFW_Buffer odoH2OpcBuffer;
        vfwInitBuffer(&odoH2OpcBuffer, &odoH2OpcData.safetyData[0], sizeof(odoH2OpcData.safetyData));

        const uint32_t opcReferenceTime = odoData.timestamp + static_cast<uint32_t>(opcReferenceTimeOffset);
        const uint32_t opcProdTime = odoData.tProdTime + static_cast<uint32_t>(opcReferenceTimeOffset);

        vfwPutU32(&odoH2OpcBuffer, opcReferenceTime); // T_DV_TRAIN 4 Timestamp for current measurement
        vfwPutU32(&odoH2OpcBuffer, opcProdTime); // PROD_TIME 4 Production time of odometer data, see 11.19

        vfwPutI32(&odoH2OpcBuffer, odoData.dMax); // D_MAX 4 Maximum value of distance
        vfwPutI32(&odoH2OpcBuffer, odoData.dNom); // D_NOM 4 Nominal value of distance
        vfwPutI32(&odoH2OpcBuffer, odoData.dMin); // D_MIN 4 Minimum value of distance

        vfwPutU16(&odoH2OpcBuffer, maxAccelComp); // A_MAX_COMP 2 Maximum expected acceleration value compensated for current gradient.
        vfwPutI16(&odoH2OpcBuffer, odoData.acceleration); // A_TRAIN 2 Signed value for train acceleration in the traveling direction of the train.

        vfwPutI16(&odoH2OpcBuffer, odoData.vMax); // V_MAX 2 Maximum value of velocity
        vfwPutI16(&odoH2OpcBuffer, odoData.vNom); // V_NOM 2 Nominal value of velocity
        vfwPutI16(&odoH2OpcBuffer, odoData.vMin); // V_MIN 2 Minimum value of velocity

        vfwPutU8(&odoH2OpcBuffer, odoData.dirError ? 1U : 0U);  // Q_DIR_ERR 1 Indicates that the direction of movement is unknown
        vfwPutU8(&odoH2OpcBuffer, odoData.isOdoSafe ? 1U : 0U); // Q_ODOSAFE 1 Level of safety

        SplHandler::instance().packOdohToOpcPacket(odoH2OpcData);
      }
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool AbstractBTMHandler::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments searches for the "help" or any other DMI Channel
      component specific command calls and handles it. Returns true if completely handled
      else returns false. returning false will let other components handle the call. help always returns false.
      */

      bool retVal = false;
      char_t  buff[100];

      // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        const char_t* const toWrite = 
          "btmTel        Prints the last received BTM balise telegram\n"
          "btmSta        Prints the last received BTM status telegram\n"
          "btmCmd        Prints the last sent BTM command\n"
#ifdef WIN32
          "btmDebug      Changes Routine test state to the next one (only enabled in PC Simulator)\n"
#endif
          "btmInfo       Prints BTM state information\n";

        ATC::AbstractConsole::corePtr()->write(toWrite);
      }
      else if (ATC::isTextMatch(&argv[0][0], "btmTel", sizeof("btmTel")))
      {
        switch (argc)
        {
          case 1:
          {
            const char_t* const toWrite =
              "SeqNum.  BaliseNum.  BegMVBTime  EndMVBTime  BegVFWTime  EndVFWTime  FinalReport\n"
              "-------------------------------------------------------------------------------------";
            ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);

            //lint -e{586} snprintf is needed here
            const int32_t ret = snprintf(&buff[0], sizeof(buff), "%-9u%-12u%-12u%-12u%-12lld%-12lld%d",
              lastTgmSequenceNum,
              lastReceivedBaliseTelegram.getBaliseNumber(),
              lastReceivedBaliseTelegram.getBeginMvbTime(),
              lastReceivedBaliseTelegram.getEndMvbTime(),
              lastReceivedBaliseTelegram.getBeginTimeStamp(),
              lastReceivedBaliseTelegram.getEndTimeStamp(),
              lastReceivedBaliseTelegram.getFinalReport());

            if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buff)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buff[0]);
            }

            break;
          }
          default:
          {
            const char_t* const toWrite = "Illegal Argument: btmTel takes 0 argument";
            ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);

            break;
          }
        }

        retVal = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "btmSta", sizeof("btmSta")))
      {
        switch (argc)
        {
          case 1:
          {
            const char_t* const bsaStr = (isBTMServiceAvailable()) ? "Av" : "NA";
            const char_t* const testStr = (btmStatusMsg.isBtmTestOk()) ? "OK" : "NOK";
            const char_t* const statusStr = (btmStatusMsg.getTelepoweringStatus() == 1U) ? "ON" : "OFF";
            const char_t* const heatStr = (btmStatusMsg.getBTMOverheatingWarningStatus()) ? "YES" : "NO";
            const char_t* const antStr = (btmStatusMsg.isBalisePresent()) ? "YES" : "NO";
            const char_t* const runStr = (routineTestState == RoutineTestInProgress) ? "Running" : "Not running";
            const char_t* const toWrite =
              "BSA  BTMTestOK  Telepowering  Overheating  AntOverBal  RoutTest  RoutTestRunn\n"
              "-----------------------------------------------------------------------------";
            ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);

            //lint -e{586} snprintf is needed here
            int32_t ret = snprintf(&buff[0], sizeof(buff), "%-5s%-11s%-14s%-13s%-10s%-12s BSA = %d Counter = %d",
              bsaStr, testStr, statusStr, heatStr, antStr, runStr, baliseServiceAvailable, bsaCounter);

            if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buff)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buff[0]);
            }

            const uint8_t preliminaryAvailabilityTestStatus = btmStatusMsg.getPreliminaryAvailabilityTestStatus();
            const uint8_t version = btmStatusMsg.getVersion();

            //lint -e{586} snprintf is needed here
            ret = snprintf(&buff[0], sizeof(buff), "Version = %d ,Tmp Availabilty = %d", 
              version, preliminaryAvailabilityTestStatus);

            if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buff)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buff[0]);
            }

            break;
          }
          default:
          {
            const char_t* const toWrite = "Illegal Argument: btmSta takes 0 argument";
            ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);

            break;
          }
        }

        retVal = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "btmCmd", sizeof("btmCmd")))
      {
        switch (argc)
        {
          case 1U:
          {
            const char_t* const powStr = (btmCmdMsg.getTelePoweringCmd() == 0U) ? "OFF" : "ON";

            const char_t* const toWrite =
              "BTMId  Telepow  TelegrmFormat(Hex)  RoutineTstNum  Version  BTMOptions(Hex)\n"
              "---------------------------------------------------------------------------";
            ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);

            //lint -e{586} snprintf is needed here
            const int32_t ret = snprintf(&buff[0], sizeof(buff), "%-7d%-9s%-20d%-15d%-9d%-15X", 1, powStr, telegramFormat, btmCmdMsg.getRoutineTest(),
              btmProtocolVersion, btmCmdMsg.getBtmOptions());

            if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buff)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buff[0]);
            }

            break;
          }
          default:
          {
            const char_t* const toWrite = "Illegal Argument: btmCmd takes 0 argument";
            ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);

            break;
          }
        }
        retVal = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "btmInfo", sizeof("btmInfo")))
      {
        if (argc == 1U)
        {
          //lint -e{586} snprintf is needed here
          const int32_t ret = snprintf(&buff[0], sizeof(buff), "State = %d, routine state = %d", static_cast<uint32_t>(baliseServiceState),
            static_cast<uint32_t>(routineTestState));

          if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buff)))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buff[0]);
          }
        }
        else
        {
          const char_t* const toWrite = "Illegal Argument: btmInfo takes 0 argument";
          ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);
        }

        retVal = true;
      }
#ifdef WIN32
      else if (ATC::isTextMatch(&argv[0][0], "btmDebug", sizeof("btmDebug")))
      {
        if (routineTestState == RoutineTestOK)
        {
          routineTestState = RoutineTestNeeded;
        }
        else if (routineTestState == RoutineTestNeeded)
        {
          routineTestState = RoutineTestMandatory;
        }
        else
        {
          // Do nothing...
        }

        retVal = true;
      }
#endif
      else
      {
        // Do nothing
      }

      return retVal;
    }
  }
}
